;;; Portable implementation of syntax-case
;;; Extracted from Chez Scheme Version 6.3
;;; Authors: R. Kent Dybvig, Oscar Waddell, Bob Hieb, Carl Bruggeman

;;; Copyright (c) 1992-2000 Cadence Research Systems
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright notice in full.  This software
;;; is provided AS IS, with NO WARRANTY, EITHER EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO IMPLIED WARRANTIES OF MERCHANTABILITY
;;; OR FITNESS FOR ANY PARTICULAR PURPOSE.  IN NO EVENT SHALL THE
;;; AUTHORS BE LIABLE FOR CONSEQUENTIAL OR INCIDENTAL DAMAGES OF ANY
;;; NATURE WHATSOEVER.

;;; Before attempting to port this code to a new implementation of
;;; Scheme, please read the notes below carefully.

;;; This file defines the syntax-case expander, sc-expand, and a set
;;; of associated syntactic forms and procedures.  Of these, the
;;; following are documented in The Scheme Programming Language,
;;; Second Edition (R. Kent Dybvig, Prentice Hall, 1996), which can be
;;; found online at http://www.scheme.com.  Most are also documented
;;; in the R4RS and draft R5RS.
;;;
;;;   bound-identifier=?
;;;   datum->syntax-object
;;;   define-syntax
;;;   fluid-let-syntax
;;;   free-identifier=?
;;;   generate-temporaries
;;;   identifier?
;;;   identifier-syntax
;;;   let-syntax
;;;   letrec-syntax
;;;   syntax
;;;   syntax-case
;;;   syntax-object->datum
;;;   syntax-rules
;;;   with-syntax
;;;
;;; All standard Scheme syntactic forms are supported by the expander
;;; or syntactic abstractions defined in this file.  Only the R4RS
;;; delay is omitted, since its expansion is implementation-dependent.

;;; Also defined are three forms that support modules: module, import,
;;; and import-only.  These are documented in the Chez Scheme User's
;;; Guide (R. Kent Dybvig, Cadence Research Systems, 1998), which can
;;; also be found online at http://www.scheme.com.  They are described
;;; briefly here as well.
;;;
;;; Both are definitions and may appear where and only where other
;;; definitions may appear.  modules may be named:
;;;
;;;   (module id (ex ...) defn ... init ...)
;;;
;;; or anonymous:
;;;
;;;   (module (ex ...) defn ... init ...)
;;;
;;; The latter form is semantically equivalent to:
;;;
;;;   (module T (ex ...) defn ... init ...)
;;;   (import T)
;;;
;;; where T is a fresh identifier.
;;;
;;; In either form, each of the exports in (ex ...) is either an
;;; identifier or of the form (id ex ...).  In the former case, the
;;; single identifier ex is exported.  In the latter, the identifier
;;; id is exported and the exports ex ... are "implicitly" exported.
;;; This listing of implicit exports is useful only when id is a
;;; keyword bound to a transformer that expands into references to
;;; the listed implicit exports.  In the present implementation,
;;; listing of implicit exports is necessary only for top-level
;;; modules and allows the implementation to avoid placing all
;;; identifiers into the top-level environment where subsequent passes
;;; of the compiler will be unable to deal effectively with them.
;;;
;;; Named modules may be referenced in import statements, which
;;; always take one of the forms:
;;;
;;;   (import id)
;;;   (import-only id)
;;;
;;; id must name a module.  Each exported identifier becomes visible
;;; within the scope of the import form.  In the case of import-only,
;;; all other identifiers become invisible in the scope of the
;;; import-only form, except for those established by definitions
;;; that appear textually after the import-only form.

;;; The remaining exports are listed below.  sc-expand, eval-when, and
;;; syntax-error are described in the Chez Scheme User's Guide.
;;;
;;;   (sc-expand datum)
;;;      if datum represents a valid expression, sc-expand returns an
;;;      expanded version of datum in a core language that includes no
;;;      syntactic abstractions.  The core language includes begin,
;;;      define, if, lambda, letrec, quote, and set!.
;;;   (eval-when situations expr ...)
;;;      conditionally evaluates expr ... at compile-time or run-time
;;;      depending upon situations
;;;   (syntax-error object message)
;;;      used to report errors found during expansion
;;;   ($syntax-dispatch e p)
;;;      used by expanded code to handle syntax-case matching
;;;   ($sc-put-cte symbol val)
;;;      used to establish top-level compile-time (expand-time) bindings.

;;; The following nonstandard procedures must be provided by the
;;; implementation for this code to run.
;;;
;;; (void)
;;; returns the implementation's cannonical "unspecified value".  The
;;; following usually works:
;;;
;;; (define void (lambda () (if #f #f))).
;;;
;;; (andmap proc list1 list2 ...)
;;; returns true if proc returns true when applied to each element of list1
;;; along with the corresponding elements of list2 ....  The following
;;; definition works but does no error checking:
;;;
;;; (define andmap
;;;   (lambda (f first . rest)
;;;     (or (null? first)
;;;         (if (null? rest)
;;;             (let andmap ((first first))
;;;               (let ((x (car first)) (first (cdr first)))
;;;                 (if (null? first)
;;;                     (f x)
;;;                     (and (f x) (andmap first)))))
;;;             (let andmap ((first first) (rest rest))
;;;               (let ((x (car first))
;;;                     (xr (map car rest))
;;;                     (first (cdr first))
;;;                     (rest (map cdr rest)))
;;;                 (if (null? first)
;;;                     (apply f (cons x xr))
;;;                     (and (apply f (cons x xr)) (andmap first rest)))))))))
;;;
;;; (ormap proc list1)
;;; returns the first non-false return result of proc applied to
;;; the elements of list1 or false if none.  The following definition
;;; works but does no error checking:
;;;
;;; (define ormap
;;;   (lambda (proc list1)
;;;     (and (not (null? list1))
;;;          (or (proc (car list1)) (ormap proc (cdr list1))))))
;;;
;;; The following nonstandard procedures must also be provided by the
;;; implementation for this code to run using the standard portable
;;; hooks and output constructors.  They are not used by expanded code,
;;; and so need be present only at expansion time.
;;;
;;; (eval x)
;;; where x is always in the form ("noexpand" expr).
;;; returns the value of expr.  the "noexpand" flag is used to tell the
;;; evaluator/expander that no expansion is necessary, since expr has
;;; already been fully expanded to core forms.
;;;
;;; eval will not be invoked during the loading of psyntax.pp.  After
;;; psyntax.pp has been loaded, the expansion of any macro definition,
;;; whether local or global, results in a call to eval.  If, however,
;;; sc-expand has already been registered as the expander to be used
;;; by eval, and eval accepts one argument, nothing special must be done
;;; to support the "noexpand" flag, since it is handled by sc-expand.
;;;
;;; (error who format-string why what)
;;; where who is either a symbol or #f, format-string is always "~a ~s",
;;; why is always a string, and what may be any object.  error should
;;; signal an error with a message something like
;;;
;;;    "error in <who>: <why> <what>"
;;;
;;; (gensym)
;;; returns a unique symbol each time it's called.  In Chez Scheme, gensym
;;; returns a symbol with a "globally" unique name so that gensyms that
;;; end up in the object code of separately compiled files cannot conflict.
;;; This is necessary only if you intend to support compiled files.
;;;
;;; (putprop symbol key value)
;;; (getprop symbol key)
;;; (remprop symbol key)
;;; key is always a symbol; value may be any object.  putprop should
;;; associate the given value with the given symbol and key in some way
;;; that it can be retrieved later with getprop.  getprop should return
;;; #f if no value is associated with the given symbol and key.  remprop
;;; should remove the association between the given symbol and key.

;;; When porting to a new Scheme implementation, you should define the
;;; procedures listed above, load the expanded version of psyntax.ss
;;; (psyntax.pp, which should be available whereever you found
;;; psyntax.ss), and register sc-expand as the current expander (how
;;; you do this depends upon your implementation of Scheme).  You may
;;; change the hooks and constructors defined toward the beginning of
;;; the code below, but to avoid bootstrapping problems, do so only
;;; after you have a working version of the expander.

;;; Chez Scheme allows the syntactic form (syntax <template>) to be
;;; abbreviated to #'<template>, just as (quote <datum>) may be
;;; abbreviated to '<datum>.  The #' syntax makes programs written
;;; using syntax-case shorter and more readable and draws out the
;;; intuitive connection between syntax and quote.  If you have access
;;; to the source code of your Scheme system's reader, you might want
;;; to implement this extension.

;;; If you find that this code loads or runs slowly, consider
;;; switching to faster hardware or a faster implementation of
;;; Scheme.  In Chez Scheme on a 200Mhz Pentium Pro, expanding,
;;; compiling (with full optimization), and loading this file takes
;;; between one and two seconds.

;;; In the expander implementation, we sometimes use syntactic abstractions
;;; when procedural abstractions would suffice.  For example, we define
;;; top-wrap and top-marked? as
;;;   (define-syntax top-wrap (identifier-syntax '((top))))
;;;   (define-syntax top-marked?
;;;     (syntax-rules ()
;;;       ((_ w) (memq 'top (wrap-marks w)))))
;;; rather than
;;;   (define top-wrap '((top)))
;;;   (define top-marked?
;;;     (lambda (w) (memq 'top (wrap-marks w))))
;;; On ther other hand, we don't do this consistently; we define make-wrap,
;;; wrap-marks, and wrap-subst simply as
;;;   (define make-wrap cons)
;;;   (define wrap-marks car)
;;;   (define wrap-subst cdr)
;;; In Chez Scheme, the syntactic and procedural forms of these
;;; abstractions are equivalent, since the optimizer consistently
;;; integrates constants and small procedures.  Some Scheme
;;; implementations, however, may benefit from more consistent use 
;;; of one form or the other.


;;; Implementation notes:

;;; "begin" is treated as a splicing construct at top level and at
;;; the beginning of bodies.  Any sequence of expressions that would
;;; be allowed where the "begin" occurs is allowed.

;;; "let-syntax" and "letrec-syntax" are also treated as splicing
;;; constructs, in violation of the R5RS.  A consequence is that let-syntax
;;; and letrec-syntax do not create local contours, as do let and letrec.
;;; Although the functionality is greater as it is presently implemented,
;;; we will probably change it to conform to the R5RS.  modules provide
;;; similar functionality to nonsplicing letrec-syntax when the latter is
;;; used as a definition.

;;; Objects with no standard print syntax, including objects containing
;;; cycles and syntax objects, are allowed in quoted data as long as they
;;; are contained within a syntax form or produced by datum->syntax-object.
;;; Such objects are never copied.

;;; When the expander encounters a reference to an identifier that has
;;; no global or lexical binding, it treats it as a global-variable
;;; reference.  This allows one to write mutually recursive top-level
;;; definitions, e.g.:
;;;   
;;;   (define f (lambda (x) (g x)))
;;;   (define g (lambda (x) (f x)))
;;;
;;; but may not always yield the intended when the variable in question
;;; is later defined as a keyword.

;;; Top-level variable definitions of syntax keywords are permitted.
;;; In order to make this work, top-level define not only produces a
;;; top-level definition in the core language, but also modifies the
;;; compile-time environment (using $sc-put-cte) to record the fact
;;; that the identifier is a variable.

;;; Top-level definitions of macro-introduced identifiers are visible
;;; only in code produced by the macro.  That is, a binding for a
;;; hidden (generated) identifier is created instead, and subsequent
;;; references within the macro output are renamed accordingly.  For
;;; example:
;;;
;;; (define-syntax a
;;;   (syntax-rules ()
;;;     ((_ var exp)
;;;      (begin
;;;        (define secret exp)
;;;        (define var
;;;          (lambda ()
;;;            (set! secret (+ secret 17))
;;;            secret))))))
;;; (a x 0)
;;; (x) => 17
;;; (x) => 34
;;; secret => Error: variable secret is not bound
;;;
;;; The definition above would fail if the definition for secret
;;; were placed after the definition for var, since the expander would
;;; encounter the references to secret before the definition that
;;; establishes the compile-time map from the identifier secret to
;;; the generated identifier.

;;; Identifiers and syntax objects are implemented as vectors for
;;; portability.  As a result, it is possible to "forge" syntax
;;; objects.

;;; The input to sc-expand may contain "annotations" describing, e.g., the
;;; source file and character position from where each object was read if
;;; it was read from a file.  These annotations are handled properly by
;;; sc-expand only if the annotation? hook (see hooks below) is implemented
;;; properly and the operators make-annotation, annotation-expression,
;;; annotation-source, annotation-stripped, and set-annotation-stripped!
;;; are supplied.  If annotations are supplied, the proper annotation
;;; source is passed to the various output constructors, allowing
;;; implementations to accurately correlate source and expanded code.
;;; Contact one of the authors for details if you wish to make use of
;;; this feature.

;;; Implementation of modules:
;;;
;;; The implementation of modules requires that implicit top-level exports
;;; be listed with the exported macro at some level where both are visible,
;;; e.g.,
;;;
;;;   (module M (alpha (beta b))
;;;     (module ((alpha a) b)
;;;       (define-syntax alpha (identifier-syntax a))
;;;       (define a 'a)
;;;       (define b 'b))
;;;     (define-syntax beta (identifier-syntax b)))
;;;
;;; Listing of implicit imports is not needed for macros that do not make
;;; it out to top level, including all macros that are local to a "body".
;;; (They may be listed in this case, however.)  We need this information
;;; for top-level modules since a top-level module expands into a letrec
;;; for non-top-level variables and top-level definitions (assignments) for
;;; top-level variables.  Because of the general nature of macro
;;; transformers, we cannot determine the set of implicit exports from the
;;; transformer code, so without the user's help, we'd have to put all
;;; variables at top level.
;;; 
;;; Each such top-level identifier is given a generated name (gensym).
;;; When a top-level module is imported at top level, a compile-time
;;; alias is established from the top-level name to the generated name.
;;; The expander follows these aliases transparently.  When any module is
;;; imported anywhere other than at top level, the id-var-name of the
;;; import identifier is set to the id-var-name of the export identifier.
;;; Since we can't determine the actual labels for identifiers defined in
;;; top-level modules until we determine which are placed in the letrec
;;; and which make it to top level, we give each an "indirect" label---a
;;; pair whose car will eventually contain the actual label.  Import does
;;; not follow the indirect, but id-var-name does.
;;;
;;; All identifiers defined within a local module are folded into the
;;; letrec created for the enclosing body.  Visibility is controlled in
;;; this case and for nested top-level modules by introducing a new wrap
;;; for each module.


;;; Bootstrapping:

;;; When changing syntax-object representations, it is necessary to support
;;; both old and new syntax-object representations in id-var-name.  It
;;; should be sufficient to recognize old representations and treat
;;; them as not lexically bound.


(let ()

(define-syntax when
  (syntax-rules ()
    ((_ test e1 e2 ...) (if test (begin e1 e2 ...)))))
(define-syntax unless
  (syntax-rules ()
    ((_ test e1 e2 ...) (when (not test) (begin e1 e2 ...)))))
(define-syntax define-structure
  (lambda (x)
    (define construct-name
      (lambda (template-identifier . args)
        (datum->syntax-object
          template-identifier
          (string->symbol
            (apply string-append
                   (map (lambda (x)
                          (if (string? x)
                              x
                              (symbol->string (syntax-object->datum x))))
                        args))))))
    (syntax-case x ()
      ((_ (name id1 ...))
       (andmap identifier? (syntax (name id1 ...)))
       (with-syntax
         ((constructor (construct-name (syntax name) "make-" (syntax name)))
          (predicate (construct-name (syntax name) (syntax name) "?"))
          ((access ...)
           (map (lambda (x) (construct-name x (syntax name) "-" x))
                (syntax (id1 ...))))
          ((assign ...)
           (map (lambda (x)
                  (construct-name x "set-" (syntax name) "-" x "!"))
                (syntax (id1 ...))))
          (structure-length
           (+ (length (syntax (id1 ...))) 1))
          ((index ...)
           (let f ((i 1) (ids (syntax (id1 ...))))
              (if (null? ids)
                  '()
                  (cons i (f (+ i 1) (cdr ids)))))))
         (syntax (begin
                   (define constructor
                     (lambda (id1 ...)
                       (vector 'name id1 ... )))
                   (define predicate
                     (lambda (x)
                       (and (vector? x)
                            (= (vector-length x) structure-length)
                            (eq? (vector-ref x 0) 'name))))
                   (define access
                     (lambda (x)
                       (vector-ref x index)))
                   ...
                   (define assign
                     (lambda (x update)
                       (vector-set! x index update)))
                   ...)))))))

(define noexpand "noexpand")

;;; hooks to nonportable run-time helpers
(begin
(define-syntax fx+ (identifier-syntax +))
(define-syntax fx- (identifier-syntax -))
(define-syntax fx= (identifier-syntax =))
(define-syntax fx< (identifier-syntax <))

(define annotation? (lambda (x) #f))

(define top-level-eval-hook
  (lambda (x)
    (eval `(,noexpand ,x))))

(define local-eval-hook
  (lambda (x)
    (eval `(,noexpand ,x))))

(define error-hook
  (lambda (who why what)
    (error who "~a ~s" why what)))

(define-syntax gensym-hook
  (syntax-rules ()
    ((_) (gensym))))

(define put-global-definition-hook
  (lambda (symbol val)
    ($sc-put-cte symbol val)))

(define get-global-definition-hook
  (lambda (symbol)
    (getprop symbol '*sc-expander*)))

(define get-import-binding
  (lambda (symbol token)
    (getprop symbol token)))

(define generate-id
  (let ((b (- 127 32 2)))
   ; session-key should generate a unique integer for each system run
   ; to support separate compilation
    (define session-key (lambda () 0))
    (define make-digit (lambda (x) (integer->char (fx+ x 33))))
    (define fmt
      (lambda (n)
        (let fmt ((n n) (a '()))
          (if (< n b)
              (list->string (cons (make-digit n) a))
              (let ((r (modulo n b)) (rest (quotient n b)))
                (fmt rest (cons (make-digit r) a)))))))
    (let ((prefix (fmt (session-key))) (n -1))
      (lambda (name)
        (set! n (+ n 1))
        (let ((newsym (string->symbol (string-append "#" prefix (fmt n)))))
          newsym)))))
)


;;; output constructors
(begin
(define-syntax build-application
  (syntax-rules ()
    ((_ source fun-exp arg-exps)
     `(,fun-exp . ,arg-exps))))

(define-syntax build-conditional
  (syntax-rules ()
    ((_ source test-exp then-exp else-exp)
     `(if ,test-exp ,then-exp ,else-exp))))

(define-syntax build-lexical-reference
  (syntax-rules ()
    ((_ type source var)
     var)))

(define-syntax build-lexical-assignment
  (syntax-rules ()
    ((_ source var exp)
     `(set! ,var ,exp))))

(define-syntax build-global-reference
  (syntax-rules ()
    ((_ source var)
     var)))

(define-syntax build-global-assignment
  (syntax-rules ()
    ((_ source var exp)
     `(set! ,var ,exp))))

(define-syntax build-global-definition
  (syntax-rules ()
    ((_ source var exp)
     `(define ,var ,exp))))

(define-syntax build-module-definition
 ; should have the effect of a global definition but may not appear at top level
  (identifier-syntax build-global-assignment))

(define-syntax build-cte-install
 ; should build a call that has the same effect as calling the
 ; global definition hook
  (syntax-rules ()
    ((_ sym exp) `($sc-put-cte ',sym ,exp))))
 
(define-syntax build-lambda
  (syntax-rules ()
    ((_ src vars exp)
     `(lambda ,vars ,exp))))

(define-syntax build-primref
  (syntax-rules ()
    ((_ src name) name)
    ((_ src level name) name)))

(define-syntax build-data
  (syntax-rules ()
    ((_ src exp) `',exp)))

(define build-sequence
  (lambda (src exps)
    (if (null? (cdr exps))
        (car exps)
        `(begin ,@exps))))

(define build-letrec
  (lambda (src vars val-exps body-exp)
    (if (null? vars)
        body-exp
        `(letrec ,(map list vars val-exps) ,body-exp))))

(define-syntax build-lexical-var
  (syntax-rules ()
    ((_ src id) (gensym))))

(define-syntax self-evaluating?
  (syntax-rules ()
    ((_ e)
     (let ((x e))
       (or (boolean? x) (number? x) (string? x) (char? x) (null? x))))))
)

(define-structure (syntax-object expression wrap))

(define-syntax unannotate
  (syntax-rules ()
    ((_ x)
     (let ((e x))
       (if (annotation? e)
           (annotation-expression e)
           e)))))

(define-syntax no-source (identifier-syntax #f))

(define source-annotation
  (lambda (x)
     (cond
       ((annotation? x) (annotation-source x))
       ((syntax-object? x) (source-annotation (syntax-object-expression x)))
       (else no-source))))

(define-syntax arg-check
  (syntax-rules ()
    ((_ pred? e who)
     (let ((x e))
       (if (not (pred? x)) (error-hook who "invalid argument" x))))))

;;; compile-time environments

;;; wrap and environment comprise two level mapping.
;;;   wrap : id --> label
;;;   env : label --> <element>

;;; environments are represented in two parts: a lexical part and a global
;;; part.  The lexical part is a simple list of associations from labels
;;; to bindings.  The global part is implemented by
;;; {put,get}-global-definition-hook and associates symbols with
;;; bindings.

;;; global (assumed global variable) and displaced-lexical (see below)
;;; do not show up in any environment; instead, they are fabricated by
;;; lookup when it finds no other bindings.

;;; <environment>              ::= ((<label> . <binding>)*)

;;; identifier bindings include a type and a value

;;; <binding> ::= (macro . <procedure>)           macros
;;;               (deferred . <expanded code>)    lazy-evaluation of transformers
;;;               (core . <procedure>)            core forms
;;;               (begin)                         begin
;;;               (define)                        define
;;;               (define-syntax)                 define-syntax
;;;               (local-syntax . rec?)           let-syntax/letrec-syntax
;;;               (eval-when)                     eval-when
;;;               (syntax . (<var> . <level>))    pattern variables
;;;               (global . <symbol>)             assumed global variable
;;;               (lexical . <var>)               lexical variables
;;;               (displaced-lexical . #f)        id-var-name not found in store
;;; <level>   ::= <nonnegative integer>
;;; <var>     ::= variable returned by build-lexical-var

;;; a macro is a user-defined syntactic-form.  a core is a system-defined
;;; syntactic form.  begin, define, define-syntax, and eval-when are
;;; treated specially since they are sensitive to whether the form is
;;; at top-level and (except for eval-when) can denote valid internal
;;; definitions.

;;; a pattern variable is a variable introduced by syntax-case and can
;;; be referenced only within a syntax form.

;;; any identifier for which no top-level syntax definition or local
;;; binding of any kind has been seen is assumed to be a global
;;; variable.

;;; a lexical variable is a lambda- or letrec-bound variable.

;;; a displaced-lexical identifier is a lexical identifier removed from
;;; it's scope by the return of a syntax object containing the identifier.
;;; a displaced lexical can also appear when a letrec-syntax-bound
;;; keyword is referenced on the rhs of one of the letrec-syntax clauses.
;;; a displaced lexical should never occur with properly written macros.

(define make-binding (lambda (x y) (cons x y)))
(define binding-type car)
(define binding-value cdr)
(define set-binding-type! set-car!)
(define set-binding-value! set-cdr!)
(define binding? (lambda (x) (and (pair? x) (symbol? (car x)))))

(define-syntax null-env (identifier-syntax '()))

(define extend-env
  (lambda (label binding r)
    (cons (cons label binding) r)))

(define extend-env*
  (lambda (labels bindings r) 
    (if (null? labels)
        r
        (extend-env* (cdr labels) (cdr bindings)
          (extend-env (car labels) (car bindings) r)))))

(define extend-var-env*
  ; variant of extend-env* that forms "lexical" binding
  (lambda (labels vars r)
    (if (null? labels)
        r
        (extend-var-env* (cdr labels) (cdr vars)
          (extend-env (car labels) (make-binding 'lexical (car vars)) r)))))

;;; we use a "macros only" environment in expansion of local macro
;;; definitions so that their definitions can use local macros without
;;; attempting to use other lexical identifiers.
;;;
;;; - can make this null-env if we don't want to allow macros to use other
;;;   macros in defining their transformers
;;; - can add a cache here if it pays off
(define transformer-env
  (lambda (r)
    (if (null? r)
        '()
        (let ((a (car r)))
          (if (eq? (cadr a) 'lexical)       ; only strip out lexical so that (transformer x) works
              (transformer-env (cdr r))
              (cons a (transformer-env (cdr r))))))))

(define displaced-lexical-error
  (lambda (id)
    (syntax-error id
      (if (id-var-name id empty-wrap)
          "identifier out of context"
          "identifier not visible"))))

(define lookup*
  ; x may be a label or a symbol
  ; although symbols are usually global, we check the environment first
  ; anyway because a temporary binding may have been established by
  ; fluid-let-syntax
  (lambda (x r)
    (cond
      ((assq x r) => cdr)
      ((symbol? x)
       (or (get-global-definition-hook x) (make-binding 'global x)))
      (else (make-binding 'displaced-lexical #f)))))

(define sanitize-binding
  (lambda (b)
    (cond
      ((procedure? b) (make-binding 'macro b))
      ((binding? b)
       (case (binding-type b)
         ((core macro macro!) (and (procedure? (binding-value b)) b))
         ((module) (and (interface? (binding-value b)) b))
         (else b)))
      (else #f))))

(define lookup
  (lambda (x r)
    (define whack-binding!
      (lambda (b *b)
        (set-binding-type! b (binding-type *b))
        (set-binding-value! b (binding-value *b))))
    (let ((b (lookup* x r)))
      (case (binding-type b)
;        ((*alias) (lookup (id-var-name (binding-value b) empty-wrap) r))
        ((deferred)
         (whack-binding! b
           (let ((*b (local-eval-hook (binding-value b))))
             (or (sanitize-binding *b)
                 (syntax-error *b "invalid transformer"))))
         (case (binding-type b)
;           ((*alias) (lookup (id-var-name (binding-value b) empty-wrap) r))
           (else b)))
        (else b)))))

(define global-extend
  (lambda (type sym val)
    (put-global-definition-hook sym (make-binding type val))))


;;; Conceptually, identifiers are always syntax objects.  Internally,
;;; however, the wrap is sometimes maintained separately (a source of
;;; efficiency and confusion), so that symbols are also considered
;;; identifiers by id?.  Externally, they are always wrapped.

(define nonsymbol-id?
  (lambda (x)
    (and (syntax-object? x)
         (symbol? (unannotate (syntax-object-expression x))))))

(define id?
  (lambda (x)
    (cond
      ((symbol? x) #t)
      ((syntax-object? x) (symbol? (unannotate (syntax-object-expression x))))
      ((annotation? x) (symbol? (annotation-expression x)))
      (else #f))))

(define-syntax id-sym-name
  (syntax-rules ()
    ((_ e)
     (let ((x e))
       (unannotate (if (syntax-object? x) (syntax-object-expression x) x))))))

(define id-sym-name&marks
  (lambda (x w)
    (if (syntax-object? x)
        (values
          (unannotate (syntax-object-expression x))
          (join-marks (wrap-marks w) (wrap-marks (syntax-object-wrap x))))
        (values (unannotate x) (wrap-marks w)))))

;;; syntax object wraps

;;;         <wrap> ::= ((<mark> ...) . (<subst> ...))
;;;        <subst> ::= <ribcage> | <shift>
;;;      <ribcage> ::= #((<ex-symname> ...) (<mark> ...) (<label> ...)) ; extensible, for chi-internal/external
;;;                  | #(#(<symname> ...) #(<mark> ...) #(<label> ...)) ; nonextensible
;;;   <ex-symname> ::= <symname> | <import token> | <barrier>
;;;        <shift> ::= shift
;;;      <barrier> ::= #f                                               ; inserted by import-only
;;; <import token> ::= #<"import-token" <token>>
;;;        <token> ::= <generated id>

(define make-wrap cons)
(define wrap-marks car)
(define wrap-subst cdr)

(define-syntax subst-rename? (identifier-syntax vector?))
(define-syntax rename-old (syntax-rules () ((_ x) (vector-ref x 0))))
(define-syntax rename-new (syntax-rules () ((_ x) (vector-ref x 1))))
(define-syntax rename-marks (syntax-rules () ((_ x) (vector-ref x 2))))
(define-syntax make-rename
  (syntax-rules ()
    ((_ old new marks) (vector old new marks))))

;;; labels

;;; simple labels must be comparable with "eq?" and distinct from symbols
;;; and pairs.

;;; indirect labels, which are implemented as pairs, are used to support
;;; import aliasing for identifiers exported (explictly or implicitly) from
;;; top-level modules.  chi-external creates an indirect label for each
;;; defined identifier, import causes the pair to be shared aliases it
;;; establishes, and chi-top-module whacks the pair to hold the top-level
;;; identifier name (symbol) if the id is to be placed at top level, before
;;; expanding the right-hand sides of the definitions in the module.

(define gen-label
  (lambda () (string #\i)))
(define label?
  (lambda (x)
    (or (string? x) ; normal lexical labels
        (symbol? x) ; global labels (symbolic names)
        (indirect-label? x))))

(define gen-labels
  (lambda (ls)
    (if (null? ls)
        '()
        (cons (gen-label) (gen-labels (cdr ls))))))

(define gen-indirect-label
  (lambda () (list (gen-label))))

(define indirect-label? pair?)
(define get-indirect-label car)
(define set-indirect-label! set-car!)

(define-structure (ribcage symnames marks labels))
(define-syntax empty-wrap (identifier-syntax '(())))

(define-syntax top-wrap (identifier-syntax '((top))))

(define-syntax top-marked?
  (syntax-rules ()
    ((_ w) (memq 'top (wrap-marks w)))))

(define-syntax only-top-marked?
  (syntax-rules ()
    ((_ id) (same-marks? (wrap-marks (syntax-object-wrap id)) (wrap-marks top-wrap)))))

;;; Marks must be comparable with "eq?" and distinct from pairs and
;;; the symbol top.  We do not use integers so that marks will remain
;;; unique even across file compiles.

(define-syntax the-anti-mark (identifier-syntax #f))

(define anti-mark
  (lambda (w)
    (make-wrap (cons the-anti-mark (wrap-marks w))
               (cons 'shift (wrap-subst w)))))

(define-syntax new-mark
  (syntax-rules ()
    ((_) (string #\m))))

(define barrier-marker #f)
(module (make-import-token import-token? import-token-key)
  (define tag 'import-token)
  (define make-import-token (lambda (x) (cons tag x)))
  (define import-token? (lambda (x) (and (pair? x) (eq? (car x) tag))))
  (define import-token-key cdr))

;;; make-empty-ribcage and extend-ribcage maintain list-based ribcages for
;;; internal definitions, in which the ribcages are built incrementally
(define-syntax make-empty-ribcage
  (syntax-rules ()
    ((_) (make-ribcage '() '() '()))))

(define extend-ribcage!
 ; must receive ids with complete wraps
 ; ribcage guaranteed to be list-based
  (lambda (ribcage id label)
    (set-ribcage-symnames! ribcage
      (cons (unannotate (syntax-object-expression id))
            (ribcage-symnames ribcage)))
    (set-ribcage-marks! ribcage
      (cons (wrap-marks (syntax-object-wrap id))
            (ribcage-marks ribcage)))
    (set-ribcage-labels! ribcage
      (cons label (ribcage-labels ribcage)))))

(define extend-ribcage-barrier!
 ; must receive ids with complete wraps
 ; ribcage guaranteed to be list-based
  (lambda (ribcage killer-id)
    (extend-ribcage-barrier-help! ribcage (syntax-object-wrap killer-id))))

(define extend-ribcage-barrier-help!
  (lambda (ribcage wrap)
    (set-ribcage-symnames! ribcage
      (cons barrier-marker (ribcage-symnames ribcage)))
    (set-ribcage-marks! ribcage
      (cons (wrap-marks wrap) (ribcage-marks ribcage)))))

(define extend-ribcage-subst!
 ; ribcage guaranteed to be list-based
  (lambda (ribcage token)
    (set-ribcage-symnames! ribcage
      (cons (make-import-token token) (ribcage-symnames ribcage)))))

(define lookup-import-binding-name
  (lambda (sym key marks)
    (let ((new (get-import-binding sym key)))
      (and new
           (let f ((new new))
             (cond
               ((pair? new) (or (f (car new)) (f (cdr new))))
               ((same-marks? marks (wrap-marks (syntax-object-wrap new))) new)
               (else #f)))))))

;;; make-binding-wrap creates vector-based ribcages
(define make-binding-wrap
  (lambda (ids labels w)
    (if (null? ids)
        w
        (make-wrap
          (wrap-marks w)
          (cons
            (let ((labelvec (list->vector labels)))
              (let ((n (vector-length labelvec)))
                (let ((symnamevec (make-vector n)) (marksvec (make-vector n)))
                  (let f ((ids ids) (i 0))
                    (if (not (null? ids))
                        (call-with-values
                          (lambda () (id-sym-name&marks (car ids) w))
                          (lambda (symname marks)
                            (vector-set! symnamevec i symname)
                            (vector-set! marksvec i marks)
                            (f (cdr ids) (fx+ i 1))))))
                  (make-ribcage symnamevec marksvec labelvec))))
            (wrap-subst w))))))

;;; make-trimmed-syntax-object is used by make-resolved-interface to support
;;; creation of module export lists whose constituent ids do not contain
;;; unnecessary substitutions or marks.
(define make-trimmed-syntax-object
  (lambda (id)
    (call-with-values
      (lambda () (id-var-name&marks id empty-wrap))
      (lambda (tosym marks)
        (unless tosym
          (syntax-error id "identifier not visible for export"))
        (let ((fromsym (id-sym-name id)))
          (make-syntax-object fromsym
            (make-wrap marks
              (list (make-ribcage (vector fromsym) (vector marks) (vector tosym))))))))))

;;; Scheme's append should not copy the first argument if the second is
;;; nil, but it does, so we define a smart version here.
(define smart-append
  (lambda (m1 m2)
    (if (null? m2)
        m1
        (append m1 m2))))

(define join-wraps
  (lambda (w1 w2)
    (let ((m1 (wrap-marks w1)) (s1 (wrap-subst w1)))
      (if (null? m1)
          (if (null? s1)
              w2
              (make-wrap
                (wrap-marks w2)
                (smart-append s1 (wrap-subst w2))))
          (make-wrap
            (smart-append m1 (wrap-marks w2))
            (smart-append s1 (wrap-subst w2)))))))

(define join-marks
  (lambda (m1 m2)
    (smart-append m1 m2)))

(define same-marks?
  (lambda (x y)
    (or (eq? x y)
        (and (not (null? x))
             (not (null? y))
             (eq? (car x) (car y))
             (same-marks? (cdr x) (cdr y))))))

(define id-var-name-loc&marks
  (lambda (id w)
    (define search
      (lambda (sym subst marks)
        (if (null? subst)
            (values sym marks)
            (let ((fst (car subst)))
              (if (eq? fst 'shift)
                  (search sym (cdr subst) (cdr marks))
                  (let ((symnames (ribcage-symnames fst)))
                    (if (vector? symnames)
                        (search-vector-rib sym subst marks symnames fst)
                        (search-list-rib sym subst marks symnames fst))))))))
    (define search-list-rib
      (lambda (sym subst marks symnames ribcage)
        (let f ((symnames symnames) (i 0))
          (cond
            ((null? symnames) (search sym (cdr subst) marks))
            ((and (eq? (car symnames) sym)
                  (same-marks? marks (list-ref (ribcage-marks ribcage) i)))
             (values (list-ref (ribcage-labels ribcage) i) marks))
            ((import-token? (car symnames))
             (cond
               ((lookup-import-binding-name sym (import-token-key (car symnames)) marks) =>
                (lambda (id)
                  (if (symbol? id)
                      (values id marks)
                      (id-var-name&marks id empty-wrap))))   ; could be more efficient:  new is a resolved id
               (else (f (cdr symnames) i))))
            ((and (eq? (car symnames) barrier-marker)
                  (same-marks? marks (list-ref (ribcage-marks ribcage) i)))
             (values #f marks))
            (else (f (cdr symnames) (fx+ i 1)))))))
    (define search-vector-rib
      (lambda (sym subst marks symnames ribcage)
        (let ((n (vector-length symnames)))
          (let f ((i 0))
            (cond
              ((fx= i n) (search sym (cdr subst) marks))
              ((and (eq? (vector-ref symnames i) sym)
                    (same-marks? marks (vector-ref (ribcage-marks ribcage) i)))
               (values (vector-ref (ribcage-labels ribcage) i) marks))
              (else (f (fx+ i 1))))))))
    (cond
      ((symbol? id) (search id (wrap-subst w) (wrap-marks w)))
      ((syntax-object? id)
       (let ((sym (unannotate (syntax-object-expression id)))
             (w1 (syntax-object-wrap id)))
         (let ((marks (join-marks (wrap-marks w) (wrap-marks w1))))
           (call-with-values (lambda () (search sym (wrap-subst w) marks))
             (lambda (new-id marks)
               (if (eq? new-id sym)
                   (search sym (wrap-subst w1) marks)
                   (values new-id marks)))))))
      ((annotation? id) (search (unannotate id) (wrap-subst w) (wrap-marks w)))
      (else (error-hook 'id-var-name "invalid id" id)))))

(define id-var-name&marks
 ; this version follows indirect labels
  (lambda (id w)
    (call-with-values
      (lambda () (id-var-name-loc&marks id w))
      (lambda (label marks)
        (values (if (indirect-label? label) (get-indirect-label label) label) marks)))))

(define id-var-name-loc
 ; this version doesn't follow indirect labels
  (lambda (id w)
    (call-with-values
      (lambda () (id-var-name-loc&marks id w))
      (lambda (label marks) label))))

(define id-var-name
 ; this version follows indirect labels
  (lambda (id w)
    (call-with-values
      (lambda () (id-var-name-loc&marks id w))
      (lambda (label marks)
        (if (indirect-label? label) (get-indirect-label label) label)))))

;;; free-id=? must be passed fully wrapped ids since (free-id=? x y)
;;; may be true even if (free-id=? (wrap x w) (wrap y w)) is not.

(define free-id=?
  (lambda (i j)
    (and (eq? (id-sym-name i) (id-sym-name j)) ; accelerator
         (eq? (id-var-name i empty-wrap) (id-var-name j empty-wrap)))))

(define-syntax literal-id=? (identifier-syntax free-id=?))

;;; bound-id=? may be passed unwrapped (or partially wrapped) ids as
;;; long as the missing portion of the wrap is common to both of the ids
;;; since (bound-id=? x y) iff (bound-id=? (wrap x w) (wrap y w))

(define bound-id=?
  (lambda (i j)
    (if (and (syntax-object? i) (syntax-object? j))
        (and (eq? (unannotate (syntax-object-expression i))
                  (unannotate (syntax-object-expression j)))
             (same-marks? (wrap-marks (syntax-object-wrap i))
                  (wrap-marks (syntax-object-wrap j))))
        (eq? (unannotate i) (unannotate j)))))

;;; "valid-bound-ids?" returns #t if it receives a list of distinct ids.
;;; valid-bound-ids? may be passed unwrapped (or partially wrapped) ids
;;; as long as the missing portion of the wrap is common to all of the
;;; ids.

(define valid-bound-ids?
  (lambda (ids)
     (and (let all-ids? ((ids ids))
            (or (null? ids)
                (and (id? (car ids))
                     (all-ids? (cdr ids)))))
          (distinct-bound-ids? ids))))

;;; distinct-bound-ids? expects a list of ids and returns #t if there are
;;; no duplicates.  It is quadratic on the length of the id list; long
;;; lists could be sorted to make it more efficient.  distinct-bound-ids?
;;; may be passed unwrapped (or partially wrapped) ids as long as the
;;; missing portion of the wrap is common to all of the ids.

(define distinct-bound-ids?
  (lambda (ids)
    (let distinct? ((ids ids))
      (or (null? ids)
          (and (not (bound-id-member? (car ids) (cdr ids)))
               (distinct? (cdr ids)))))))

(define invalid-ids-error
 ; find first bad one and complain about it
  (lambda (ids exp class)
    (let find ((ids ids) (gooduns '()))
      (if (null? ids)
          (syntax-error exp) ; shouldn't happen
          (if (id? (car ids))
              (if (bound-id-member? (car ids) gooduns)
                  (syntax-error (car ids) "duplicate " class)
                  (find (cdr ids) (cons (car ids) gooduns)))
              (syntax-error (car ids) "invalid " class))))))

(define bound-id-member?
   (lambda (x list)
      (and (not (null? list))
           (or (bound-id=? x (car list))
               (bound-id-member? x (cdr list))))))

;;; wrapping expressions and identifiers

(define wrap
  (lambda (x w)
    (cond
      ((and (null? (wrap-marks w)) (null? (wrap-subst w))) x)
      ((syntax-object? x)
       (make-syntax-object
         (syntax-object-expression x)
         (join-wraps w (syntax-object-wrap x))))
      ((null? x) x)
      (else (make-syntax-object x w)))))

(define source-wrap
  (lambda (x w s)
    (wrap (if s (make-annotation x s #f) x) w)))

;;; expanding

(define chi-sequence
  (lambda (body r w s)
    (build-sequence s
      (let dobody ((body body) (r r) (w w))
        (if (null? body)
            '()
            (let ((first (chi (car body) r w)))
              (cons first (dobody (cdr body) r w))))))))

(define chi-top-sequence
  (lambda (body r w s m esew ribcage)
    (build-sequence s
      (let dobody ((body body) (r r) (w w) (m m) (esew esew))
        (if (null? body)
            '()
            (let ((first (chi-top (car body) r w m esew ribcage)))
              (cons first (dobody (cdr body) r w m esew))))))))

(define chi-when-list
  (lambda (e when-list w)
    ; when-list is syntax'd version of list of situations
    (let f ((when-list when-list) (situations '()))
      (if (null? when-list)
          situations
          (f (cdr when-list)
             (cons (let ((x (car when-list)))
                     (cond
                       ((literal-id=? x (syntax compile)) 'compile)
                       ((literal-id=? x (syntax load)) 'load)
                       ((literal-id=? x (syntax eval)) 'eval)
                       (else (syntax-error (wrap x w)
                               "invalid eval-when situation"))))
                   situations))))))

;;; syntax-type returns five values: type, value, e, w, and s.  The first
;;; two are described in the table below.
;;;
;;;    type                   value         explanation
;;;    -------------------------------------------------------------------
;;;    begin                  none          begin keyword
;;;    begin-form             none          begin expression
;;;    call                   none          any other call
;;;    constant               none          self-evaluating datum
;;;    core                   procedure     core form (including singleton)
;;;    define                 none          define keyword
;;;    define-form            none          variable definition
;;;    define-syntax          none          define-syntax keyword
;;;    define-syntax-form     none          syntax definition
;;;    displaced-lexical      none          displaced lexical identifier
;;;    eval-when              none          eval-when keyword
;;;    eval-when-form         none          eval-when form
;;;    global                 name          global variable reference
;;;    import                 none          import keyword
;;;    import-form            none          import form
;;;    lexical                name          lexical variable reference
;;;    lexical-call           name          call to lexical variable
;;;    local-syntax           rec?          letrec-syntax/let-syntax keyword
;;;    local-syntax-form      rec?          syntax definition
;;;    module                 none          module keyword
;;;    module-form            none          module definition
;;;    other                  none          anything else
;;;    syntax                 level         pattern variable
;;;
;;; For all forms, e is the form, w is the wrap for e. and s is the source.
;;;
;;; syntax-type expands macros and unwraps as necessary to get to
;;; one of the forms above.

(define syntax-type
  (lambda (e r w s rib)
    (cond
      ((symbol? e)
       (let* ((n (id-var-name e w))
              (b (lookup n r))
              (type (binding-type b)))
         (case type
           ((lexical) (values type (binding-value b) e w s))
          ((global) (values type (binding-value b) e w s))
           ((macro macro!) (syntax-type (chi-macro (binding-value b) e r w s rib) r empty-wrap #f rib))
           (else (values type (binding-value b) e w s)))))
      ((pair? e)
       (let ((first (car e)))
         (if (id? first)
             (let* ((n (id-var-name first w))
                    (b (lookup n r))
                    (type (binding-type b)))
               (case type
                 ((lexical) (values 'lexical-call (binding-value b) e w s))
                 ((macro macro!)
                  (syntax-type (chi-macro (binding-value b) e r w s rib)
                    r empty-wrap #f rib))
                 ((core) (values type (binding-value b) e w s))
                 ((local-syntax)
                  (values 'local-syntax-form (binding-value b) e w s))
                 ((begin) (values 'begin-form #f e w s))
                 ((eval-when) (values 'eval-when-form #f e w s))
                 ((define) (values 'define-form #f e w s))
                 ((define-syntax) (values 'define-syntax-form #f e w s))
                 ((module-key) (values 'module-form #f e w s))
                 ((import) (values 'import-form (and (binding-value b) (wrap first w)) e w s))
                 ((set!) (chi-set! e r w s rib))
                 (else (values 'call #f e w s))))
             (values 'call #f e w s))))
      ((syntax-object? e)
       ;; s can't be valid source if we've unwrapped
       (syntax-type (syntax-object-expression e)
                    r
                    (join-wraps w (syntax-object-wrap e))
                    no-source rib))
      ((annotation? e)
       (syntax-type (annotation-expression e) r w (annotation-source e) rib))
      ((self-evaluating? e) (values 'constant #f e w s))
      (else (values 'other #f e w s)))))

(define chi-top-expr
  (lambda (e r w top-ribcage)
    (call-with-values
      (lambda () (syntax-type e r w no-source top-ribcage))
      (lambda (type value e w s)
        (chi-expr type value e r w s)))))

(define chi-top
  (lambda (e r w m esew top-ribcage)
    (define-syntax eval-if-c&e
      (syntax-rules ()
        ((_ m e)
         (let ((x e))
           (if (eq? m 'c&e) (top-level-eval-hook x))
           x))))
    (call-with-values
      (lambda () (syntax-type e r w no-source top-ribcage))
      (lambda (type value e w s)
        (case type
          ((begin-form)
           (syntax-case e ()
             ((_) (chi-void))
             ((_ e1 e2 ...)
              (chi-top-sequence (syntax (e1 e2 ...)) r w s m esew top-ribcage))))
          ((local-syntax-form)
           (chi-local-syntax value e r w s
             (lambda (body r w s)
               (chi-top-sequence body r w s m esew top-ribcage))))
          ((eval-when-form)
           (syntax-case e ()
             ((_ (x ...) e1 e2 ...)
              (let ((when-list (chi-when-list e (syntax (x ...)) w))
                    (body (syntax (e1 e2 ...))))
                (cond
                  ((eq? m 'e)
                   (if (memq 'eval when-list)
                       (chi-top-sequence body r w s 'e '(eval) top-ribcage)
                       (chi-void)))
                  ((memq 'load when-list)
                   (if (or (memq 'compile when-list)
                           (and (eq? m 'c&e) (memq 'eval when-list)))
                       (chi-top-sequence body r w s 'c&e '(compile load) top-ribcage)
                       (if (memq m '(c c&e))
                           (chi-top-sequence body r w s 'c '(load) top-ribcage)
                           (chi-void))))
                  ((or (memq 'compile when-list)
                       (and (eq? m 'c&e) (memq 'eval when-list)))
                   (top-level-eval-hook
                     (chi-top-sequence body r w s 'e '(eval) top-ribcage))
                   (chi-void))
                  (else (chi-void)))))))
          ((define-syntax-form)
           (parse-define-syntax e w s
             (lambda (id rhs w)
               (let ((id (wrap id w)))
                 (let ((n (id-var-name id empty-wrap)))
                   (let ((b (lookup n r)))
                     (case (binding-type b)
                       ((displaced-lexical) (displaced-lexical-error id)))))
                 (ct-eval/residualize m esew
                   (lambda ()
                     (build-cte-install
                       (let ((sym (id-sym-name id)))
                         (if (only-top-marked? id)
                             sym
                             (let ((marks (wrap-marks (syntax-object-wrap id))))
                               (make-syntax-object sym
                                 (make-wrap marks
                                   (list (make-ribcage (vector sym)
                                           (vector marks) (vector (generate-id sym)))))))))
                       (chi rhs (transformer-env r) w))))))))
          ((define-form)
           (parse-define e w s
             (lambda (id rhs w)
               (let ((id (wrap id w)))
                 (let ((n (id-var-name id empty-wrap)))
                   (let ((b (lookup n r)))
                     (case (binding-type b)
                       ((displaced-lexical) (displaced-lexical-error id)))))
                 (let ((sym (id-sym-name id)))
                   (let ((valsym (if (only-top-marked? id) sym (generate-id sym))))
                     (build-sequence no-source
                       (list
                         (ct-eval/residualize m esew
                           (lambda ()
                             (build-cte-install
                               (if (eq? sym valsym)
                                   sym
                                   (let ((marks (wrap-marks (syntax-object-wrap id))))
                                     (make-syntax-object sym
                                       (make-wrap marks
                                         (list (make-ribcage (vector sym)
                                                 (vector marks) (vector valsym)))))))
                               (build-data no-source (make-binding 'global valsym)))))
                         (eval-if-c&e m (build-global-definition s valsym (chi rhs r w))))))
                  )))))
          ((module-form)
           (let ((r (cons '("top-level module placeholder" . (placeholder)) r))
                 (ribcage (make-empty-ribcage)))
             (parse-module e w s (make-wrap (wrap-marks w) (cons ribcage (wrap-subst w)))
               (lambda (id exports forms)
                 (if id
                     (begin
                       (let ((n (id-var-name id empty-wrap)))
                         (let ((b (lookup n r)))
                           (case (binding-type b)
                             ((displaced-lexical) (displaced-lexical-error (wrap id w))))))
                       (chi-top-module e r ribcage w s m esew id exports forms))
                     (chi-top-module e r ribcage w s m esew #f exports forms))))))
          ((import-form)
           (parse-import e w s
             (lambda (mid)
               (ct-eval/residualize m esew
                 (lambda ()
                   (when value (syntax-error (source-wrap e w s) "not valid at top-level"))
                   (let ((binding (lookup (id-var-name mid empty-wrap) null-env)))
                     (case (binding-type binding)
                       ((module) (do-top-import mid (interface-token (binding-value binding))))
                       ((displaced-lexical) (displaced-lexical-error mid))
                       (else (syntax-error mid "import from unknown module")))))))))
          (else (eval-if-c&e m (chi-expr type value e r w s))))))))

(define flatten-exports
  (lambda (exports)
    (let loop ((exports exports) (ls '()))
      (if (null? exports)
          ls
          (loop (cdr exports)
                (if (pair? (car exports))
                    (loop (car exports) ls)
                    (cons (car exports) ls)))))))


(define-structure (interface exports token))

(define make-trimmed-interface
 ; trim out implicit exports
  (lambda (exports)
    (make-interface
      (list->vector (map (lambda (x) (if (pair? x) (car x) x)) exports))
      #f)))

(define make-resolved-interface
 ; trim out implicit exports & resolve others to actual top-level symbol
  (lambda (exports import-token)
    (make-interface
      (list->vector (map (lambda (x) (make-trimmed-syntax-object (if (pair? x) (car x) x))) exports))
      import-token)))

(define-structure (module-binding type id label imps val))

(define chi-top-module
  (lambda (e r ribcage w s m esew id exports forms)
    (let ((fexports (flatten-exports exports)))
      (chi-external ribcage (source-wrap e w s)
        (map (lambda (d) (cons r d)) forms) r exports fexports m esew
        (lambda (bindings inits)
         ; dvs & des: "defined" (letrec-bound) vars & rhs expressions
         ; svs & ses: "set!" (top-level) vars & rhs expressions
          (let partition ((fexports fexports) (bs bindings) (svs '()) (ses '()) (ctdefs '()))
            (if (null? fexports)
               ; remaining bindings are either local vars or local macros/modules
                (let partition ((bs bs) (dvs '()) (des '()))
                  (if (null? bs)
                      (let ((ses (map (lambda (x) (chi (cdr x) (car x) empty-wrap)) ses))
                            (des (map (lambda (x) (chi (cdr x) (car x) empty-wrap)) des))
                            (inits (map (lambda (x) (chi (cdr x) (car x) empty-wrap)) inits)))
                       ; we wait to do this here so that expansion of des & ses use
                       ; local versions, which in particular, allows us to use macros
                       ; locally even if esew tells us not to eval them
                        (for-each (lambda (x)
                                    (apply (lambda (t label sym val)
                                             (when label (set-indirect-label! label sym)))
                                           x))
                                  ctdefs)
                        (build-sequence no-source
                          (list (ct-eval/residualize m esew
                                  (lambda ()
                                    (if (null? ctdefs)
                                        (chi-void)
                                        (build-sequence no-source
                                          (map (lambda (x)
                                                 (apply (lambda (t label sym val)
                                                          (build-cte-install sym
                                                            (if (eq? t 'define-syntax-form)
                                                                val
                                                                (build-data no-source
                                                                  (make-binding 'module
                                                                    (make-resolved-interface val sym))))))
                                                        x))
                                               ctdefs)))))
                                (ct-eval/residualize m esew
                                  (lambda ()
                                    (let ((n (if id (id-sym-name id) #f)))
                                      (let* ((token (generate-id n))
                                             (b (build-data no-source
                                                  (make-binding 'module
                                                    (make-resolved-interface exports token)))))
                                        (if n
                                            (build-cte-install
                                              (if (only-top-marked? id)
                                                  n
                                                  (let ((marks (wrap-marks (syntax-object-wrap id))))
                                                    (make-syntax-object n
                                                      (make-wrap marks
                                                        (list (make-ribcage (vector n)
                                                                (vector marks) (vector (generate-id n))))))))
                                              b)
                                            (let ((n (generate-id 'tmp)))
                                              (build-sequence no-source
                                                (list (build-cte-install n b)
                                                      (do-top-import n token)))))))))
                              ; Some systems complain when undefined variables are assigned.
                               (build-sequence no-source
                                 (map (lambda (v) (build-global-definition no-source v (chi-void))) svs))
                                (build-letrec no-source
                                  dvs
                                  des
                                  (build-sequence no-source
                                    (list 
                                      (if (null? svs)
                                          (chi-void)
                                          (build-sequence no-source
                                            (map (lambda (v e)
                                                   (build-module-definition no-source v e))
                                                 svs
                                                 ses)))
                                      (if (null? inits)
                                          (chi-void)
                                          (build-sequence no-source inits)))))
                                (chi-void))))
                      (let ((b (car bs)))
                        (case (module-binding-type b)
                          ((define-form)
                           (let ((var (gen-var (module-binding-id b))))
                             (extend-store! r
                               (get-indirect-label (module-binding-label b))
                               (make-binding 'lexical var))
                             (partition (cdr bs) (cons var dvs)
                               (cons (module-binding-val b) des))))
                          ((define-syntax-form module-form) (partition (cdr bs) dvs des))
                          (else (error 'sc-expand-internal "unexpected module binding type"))))))
                (let ((id (car fexports)) (fexports (cdr fexports)))
                  (define pluck-binding
                    (lambda (id bs succ fail)
                      (let loop ((bs bs) (new-bs '()))
                        (if (null? bs)
                            (fail)
                            (if (bound-id=? (module-binding-id (car bs)) id)
                                (succ (car bs) (smart-append (reverse new-bs) (cdr bs)))
                                (loop (cdr bs) (cons (car bs) new-bs)))))))
                  (pluck-binding id bs
                    (lambda (b bs)
                      (let ((t (module-binding-type b))
                            (label (module-binding-label b))
                            (imps (module-binding-imps b)))
                        (let ((fexports (append imps fexports))
                              (sym (generate-id (id-sym-name id))))
                          (case t
                            ((define-form)
                             (set-indirect-label! label sym)
                             (partition fexports bs (cons sym svs)
                               (cons (module-binding-val b) ses)
                               ctdefs))
                            ((define-syntax-form)
                             (partition fexports bs svs ses
                               (cons (list t label sym (module-binding-val b)) ctdefs)))
                            ((module-form)
                             (let ((exports (module-binding-val b)))
                               (partition (append (flatten-exports exports) fexports) bs
                                 svs ses
                                 (cons (list t label sym exports) ctdefs))))
                            (else (error 'sc-expand-internal "unexpected module binding type"))))))
                    (lambda () (partition fexports bs svs ses ctdefs)))))))))))

(define id-set-diff
  (lambda (exports defs)
    (cond
      ((null? exports) '())
      ((bound-id-member? (car exports) defs) (id-set-diff (cdr exports) defs))
      (else (cons (car exports) (id-set-diff (cdr exports) defs))))))

(define extend-store!
  (lambda (r label binding)
    (set-cdr! r (extend-env label binding (cdr r)))))

(define check-module-exports
  ; After processing the definitions of a module this is called to verify that the
  ; module has defined or imported each exported identifier.  Because ids in fexports are
  ; wrapped with the given ribcage, they will contain substitutions for anything defined
  ; or imported here.  These subsitutions can be used by do-import! and do-import-top! to
  ; provide access to reexported bindings, for example.
  (lambda (source-exp fexports ids)
    (define defined?
      (lambda (e ids)
        (ormap (lambda (x)
                 (if (interface? x)
                     (let ((token (interface-token x)))
                       (if token
                           (lookup-import-binding-name (id-sym-name e) token (wrap-marks (syntax-object-wrap e)))
                           (let ((v (interface-exports x)))
                             (let lp ((i (fx- (vector-length v) 1)))
                               (and (fx>= i 0)
                                    (or (bound-id=? e (vector-ref v i))
                                        (lp (fx- i 1))))))))
                     (bound-id=? e x)))
               ids)))
    (let loop ((fexports fexports) (missing '()))
      (if (null? fexports)
          (unless (null? missing) (syntax-error missing "missing definition for export(s)"))
          (let ((e (car fexports)) (fexports (cdr fexports)))
            (if (defined? e ids) 
                (loop fexports missing)
                (loop fexports (cons e missing))))))))

(define check-defined-ids
  (lambda (source-exp ls)
    (define b-i=?
      ; cope with fat-fingered top-level
      (lambda (x y)
        (if (symbol? x)
            (if (symbol? y)
                (eq? x y)
                (and (eq? x (id-sym-name y))
                     (same-marks? (wrap-marks (syntax-object-wrap y)) (wrap-marks top-wrap))))
            (if (symbol? y)
                (and (eq? y (id-sym-name x))
                     (same-marks? (wrap-marks (syntax-object-wrap x)) (wrap-marks top-wrap)))
                (bound-id=? x y)))))
    (define vfold
      (lambda (v p cls)
        (let ((len (vector-length v)))
          (let lp ((i 0) (cls cls))
            (if (fx= i len)
                cls
                (lp (fx+ i 1) (p (vector-ref v i) cls)))))))
    (define conflicts
      (lambda (x y cls)
        (if (interface? x)
            (if (interface? y)
                (call-with-values
                  (lambda ()
                    (let ((xe (interface-exports x)) (ye (interface-exports y)))
                      (if (fx> (vector-length xe) (vector-length ye))
                          (values x ye)
                          (values y xe))))
                  (lambda (iface exports)
                    (vfold exports (lambda (id cls) (id-iface-conflicts id iface cls)) cls)))
                (id-iface-conflicts y x cls))
            (if (interface? y)
                (id-iface-conflicts x y cls)
                (if (b-i=? x y) (cons x cls) cls)))))
     (define id-iface-conflicts
       (lambda (id iface cls)
         (let ((token (interface-token iface)))
           (if token
               (if (lookup-import-binding-name (id-sym-name id) token
                     (if (symbol? id)
                         (wrap-marks top-wrap)
                         (wrap-marks (syntax-object-wrap id))))
                   (cons id cls)
                   cls)
               (vfold (interface-exports iface)
                      (lambda (*id cls) (if (b-i=? *id id) (cons *id cls) cls))
                      cls)))))
     (unless (null? ls)
       (let lp ((x (car ls)) (ls (cdr ls)) (cls '()))
         (if (null? ls)
             (unless (null? cls)
               (let ((cls (syntax-object->datum cls)))
                 (syntax-error source-exp "duplicate definition for "
                  (symbol->string (car cls))
                   " in")))
             (let lp2 ((ls2 ls) (cls cls))
               (if (null? ls2)
                   (lp (car ls) (cdr ls) cls)
                   (lp2 (cdr ls2) (conflicts x (car ls2) cls)))))))))

(define chi-external
  (lambda (ribcage source-exp body r exports fexports m esew k)
    (define return
      (lambda (bindings ids inits)
        (check-defined-ids source-exp ids)
        (check-module-exports source-exp fexports ids)
        (k bindings inits)))
    (define get-implicit-exports
      (lambda (id)
        (let f ((exports exports))
          (if (null? exports)
              '()
              (if (and (pair? (car exports)) (bound-id=? id (caar exports)))
                  (flatten-exports (cdar exports))
                  (f (cdr exports)))))))
    (define update-imp-exports
      (lambda (bindings exports)
        (let ((exports (map (lambda (x) (if (pair? x) (car x) x)) exports)))
          (map (lambda (b)
                 (let ((id (module-binding-id b)))
                   (if (not (bound-id-member? id exports))
                       b
                       (make-module-binding
                         (module-binding-type b)
                         id
                         (module-binding-label b)
                         (append (get-implicit-exports id) (module-binding-imps b))
                         (module-binding-val b)))))
               bindings))))
    (let parse ((body body) (ids '()) (bindings '()) (inits '()))
      (if (null? body)
          (return bindings ids inits)
          (let ((e (cdar body)) (er (caar body)))
            (call-with-values
              (lambda () (syntax-type e er empty-wrap no-source ribcage))
              (lambda (type value e w s)
                (case type
                  ((define-form)
                   (parse-define e w s
                     (lambda (id rhs w)
                       (let* ((id (wrap id w))
                              (label (gen-indirect-label))
                              (imps (get-implicit-exports id)))
                         (extend-ribcage! ribcage id label)
                         (parse
                           (cdr body)
                           (cons id ids)
                           (cons (make-module-binding type id label
                                   imps (cons er (wrap rhs w)))
                                 bindings)
                           inits)))))
                  ((define-syntax-form)
                   (parse-define-syntax e w s
                     (lambda (id rhs w)
                       (let* ((id (wrap id w))
                              (label (gen-indirect-label))
                              (imps (get-implicit-exports id))
                              (exp (chi rhs (transformer-env er) w)))
                         ; arrange to evaluate the transformer lazily
                         (extend-store! r (get-indirect-label label) (cons 'deferred exp))
                         (extend-ribcage! ribcage id label)
                         (parse
                           (cdr body)
                           (cons id ids)
                           (cons (make-module-binding type id label imps exp)
                                 bindings)
                           inits)))))
                  ((module-form)
                   (let* ((*ribcage (make-empty-ribcage))
                          (*w (make-wrap (wrap-marks w) (cons *ribcage (wrap-subst w)))))
                     (parse-module e w s *w
                       (lambda (id *exports forms)
                         (chi-external *ribcage (source-wrap e w s)
                                     (map (lambda (d) (cons er d)) forms)
                                     r *exports (flatten-exports *exports) m esew
                           (lambda (*bindings *inits)
                             (let* ((iface (make-trimmed-interface *exports))
                                    (bindings (append (if id *bindings (update-imp-exports *bindings *exports)) bindings))
                                    (inits (append inits *inits)))
                               (if id
                                   (let ((label (gen-indirect-label))
                                         (imps (get-implicit-exports id)))
                                     (extend-store! r (get-indirect-label label)
                                       (make-binding 'module iface))
                                     (extend-ribcage! ribcage id label)
                                     (parse
                                       (cdr body)
                                       (cons id ids)
                                       (cons (make-module-binding type id label imps *exports) bindings)
                                       inits))
                                   (let ()
                                     (do-import! iface ribcage)
                                     (parse (cdr body) (cons iface ids) bindings inits))))))))))
                 ((import-form)
                  (parse-import e w s
                    (lambda (mid)
                      (let ((mlabel (id-var-name mid empty-wrap)))
                        (let ((binding (lookup mlabel r)))
                          (case (binding-type binding)
                            ((module)
                             (let ((iface (binding-value binding)))
                               (when value (extend-ribcage-barrier! ribcage value))
                               (do-import! iface ribcage)
                               (parse
                                 (cdr body)
                                 (cons iface ids)
                                 (update-imp-exports bindings (vector->list (interface-exports iface)))
                                 inits)))
                            ((displaced-lexical) (displaced-lexical-error mid))
                            (else (syntax-error mid "import from unknown module"))))))))
                  ((begin-form)
                   (syntax-case e ()
                     ((_ e1 ...)
                      (parse (let f ((forms (syntax (e1 ...))))
                               (if (null? forms)
                                   (cdr body)
                                   (cons (cons er (wrap (car forms) w))
                                         (f (cdr forms)))))
                        ids bindings inits))))
                  ((local-syntax-form)
                   (chi-local-syntax value e er w s
                     (lambda (forms er w s)
                       (parse (let f ((forms forms))
                                (if (null? forms)
                                    (cdr body)
                                    (cons (cons er (wrap (car forms) w))
                                          (f (cdr forms)))))
                         ids bindings inits))))
                  (else ; found an init expression
                   (return bindings ids
                     (append inits (cons (cons er (source-wrap e w s)) (cdr body)))))))))))))

(define vmap
  (lambda (fn v)
    (do ((i (fx- (vector-length v) 1) (fx- i 1))
         (ls '() (cons (fn (vector-ref v i)) ls)))
        ((fx< i 0) ls))))

(define vfor-each
  (lambda (fn v)
    (let ((len (vector-length v)))
      (do ((i 0 (fx+ i 1)))
          ((fx= i len))
        (fn (vector-ref v i))))))

(define do-top-import
  (lambda (mid token)
    (build-cte-install mid
      (build-data no-source
        (make-binding 'do-import token)))))

(define ct-eval/residualize
  (lambda (m esew thunk)
    (case m
      ((c) (if (memq 'compile esew)
               (let ((e (thunk)))
                 (top-level-eval-hook e)
                 (if (memq 'load esew) e (chi-void)))
               (if (memq 'load esew) (thunk) (chi-void))))
      ((c&e) (let ((e (thunk))) (top-level-eval-hook e) e))
      (else (if (memq 'eval esew) (top-level-eval-hook (thunk))) (chi-void)))))

(define chi
  (lambda (e r w)
    (call-with-values
      (lambda () (syntax-type e r w no-source #f))
      (lambda (type value e w s)
        (chi-expr type value e r w s)))))

(define chi-expr
  (lambda (type value e r w s)
    (case type
      ((lexical)
       (build-lexical-reference 'value s value))
      ((core) (value e r w s))
      ((lexical-call)
       (chi-application
         (build-lexical-reference 'fun (source-annotation (car e)) value)
         e r w s))
      ((constant) (build-data s (strip (source-wrap e w s) empty-wrap)))
      ((global) (build-global-reference s value))
      ((call) (chi-application (chi (car e) r w) e r w s))
      ((begin-form)
       (syntax-case e ()
         ((_ e1 e2 ...) (chi-sequence (syntax (e1 e2 ...)) r w s))))
      ((local-syntax-form)
       (chi-local-syntax value e r w s chi-sequence))
      ((eval-when-form)
       (syntax-case e ()
         ((_ (x ...) e1 e2 ...)
          (let ((when-list (chi-when-list e (syntax (x ...)) w)))
            (if (memq 'eval when-list)
                (chi-sequence (syntax (e1 e2 ...)) r w s)
                (chi-void))))))
      ((define-form define-syntax-form module-form import-form)
       (syntax-error (source-wrap e w s) "invalid context for definition"))
      ((syntax)
       (syntax-error (source-wrap e w s)
         "reference to pattern variable outside syntax form"))
      ((displaced-lexical) (displaced-lexical-error (source-wrap e w s)))
      (else (syntax-error (source-wrap e w s))))))

(define chi-application
  (lambda (x e r w s)
    (syntax-case e ()
      ((e0 e1 ...)
       (build-application s x
         (map (lambda (e) (chi e r w)) (syntax (e1 ...)))))
      (_ (syntax-error (source-wrap e w s))))))

(define chi-set!
  (lambda (e r w s rib)
    (syntax-case e ()
      ((_ id val)
       (id? (syntax id))
       (let ((n (id-var-name (syntax id) w)))
         (let ((b (lookup n r)))
           (case (binding-type b)
             ((macro!)
              (let ((id (wrap (syntax id) w)) (val (wrap (syntax val) w)))
                (syntax-type (chi-macro (binding-value b)
                               `(,(syntax set!) ,id ,val)
                               r empty-wrap s rib) r empty-wrap s rib)))
             (else
              (values 'core
                (lambda (e r w s)
                 ; repeat lookup in case we were first expression (init) in
                 ; module or lambda body.  we repeat id-var-name as well,
                 ; although this is only necessary if we allow inits to
                 ; preced definitions
                  (let ((val (chi (syntax val) r w))
                        (n (id-var-name (syntax id) w)))
                    (let ((b (lookup n r)))
                      (case (binding-type b)
                        ((lexical) (build-lexical-assignment s (binding-value b) val))
                        ((global) (build-global-assignment s (binding-value b) val))
                        ((displaced-lexical)
                         (syntax-error (wrap (syntax id) w) "identifier out of context"))
                        (else (syntax-error (source-wrap e w s)))))))
                e w s))))))
      (_ (syntax-error (source-wrap e w s))))))

(define chi-macro
  (lambda (p e r w s rib)
    (define rebuild-macro-output
      (lambda (x m)
        (cond ((pair? x)
               (cons (rebuild-macro-output (car x) m)
                     (rebuild-macro-output (cdr x) m)))
              ((syntax-object? x)
               (let ((w (syntax-object-wrap x)))
                 (let ((ms (wrap-marks w)) (s (wrap-subst w)))
                   (make-syntax-object (syntax-object-expression x)
                     (if (and (pair? ms) (eq? (car ms) the-anti-mark))
                         (make-wrap (cdr ms)
                           (if rib (cons rib (cdr s)) (cdr s)))
                         (make-wrap (cons m ms)
                           (if rib
                               (cons rib (cons 'shift s))
                               (cons 'shift s))))))))
              ((vector? x)
               (let* ((n (vector-length x)) (v (make-vector n)))
                 (do ((i 0 (fx+ i 1)))
                     ((fx= i n) v)
                     (vector-set! v i
                       (rebuild-macro-output (vector-ref x i) m)))))
              ((symbol? x)
               (syntax-error (source-wrap e w s)
                 "encountered raw symbol "
                 (format "~s" x)
                 " in output of macro"))
              (else x))))
    (rebuild-macro-output
      (let ((out (p (source-wrap e (anti-mark w) s))))
        (if (procedure? out)
            (out (lambda (id)
                   (unless (identifier? id)
                     (syntax-error id
                       "environment argument is not an identifier"))
                   (lookup (id-var-name id empty-wrap) r)))
            out))
      (new-mark))))

(define chi-body
  ;; Here we create the empty wrap and new environment with placeholder
  ;; as required by chi-internal.  On return we extend the environment
  ;; to recognize the var-labels as lexical variables and build a letrec
  ;; binding them to the var-vals which we expand here.
  (lambda (body outer-form r w)
    (let* ((r (cons '("placeholder" . (placeholder)) r))
           (ribcage (make-empty-ribcage))
           (w (make-wrap (wrap-marks w) (cons ribcage (wrap-subst w))))
           (body (map (lambda (x) (cons r (wrap x w))) body)))
      (chi-internal ribcage outer-form body r
        (lambda (exprs ids vars vals inits)
          (when (null? exprs) (syntax-error outer-form "no expressions in body"))
          (build-letrec no-source
            vars
            (map (lambda (x) (chi (cdr x) (car x) empty-wrap)) vals)
            (build-sequence no-source
              (map (lambda (x) (chi (cdr x) (car x) empty-wrap)) (append inits exprs)))))))))

(define chi-internal
  ;; In processing the forms of the body, we create a new, empty wrap.
  ;; This wrap is augmented (destructively) each time we discover that
  ;; the next form is a definition.  This is done:
  ;;
  ;;   (1) to allow the first nondefinition form to be a call to
  ;;       one of the defined ids even if the id previously denoted a
  ;;       definition keyword or keyword for a macro expanding into a
  ;;       definition;
  ;;   (2) to prevent subsequent definition forms (but unfortunately
  ;;       not earlier ones) and the first nondefinition form from
  ;;       confusing one of the bound identifiers for an auxiliary
  ;;       keyword; and
  ;;   (3) so that we do not need to restart the expansion of the
  ;;       first nondefinition form, which is problematic anyway
  ;;       since it might be the first element of a begin that we
  ;;       have just spliced into the body (meaning if we restarted,
  ;;       we'd really need to restart with the begin or the macro
  ;;       call that expanded into the begin, and we'd have to give
  ;;       up allowing (begin <defn>+ <expr>+), which is itself
  ;;       problematic since we don't know if a begin contains only
  ;;       definitions until we've expanded it).
  ;;
  ;; Before processing the body, we also create a new environment
  ;; containing a placeholder for the bindings we will add later and
  ;; associate this environment with each form.  In processing a
  ;; let-syntax or letrec-syntax, the associated environment may be
  ;; augmented with local keyword bindings, so the environment may
  ;; be different for different forms in the body.  Once we have
  ;; gathered up all of the definitions, we evaluate the transformer
  ;; expressions and splice into r at the placeholder the new variable
  ;; and keyword bindings.  This allows let-syntax or letrec-syntax
  ;; forms local to a portion or all of the body to shadow the
  ;; definition bindings.
  ;;
  ;; Subforms of a begin, let-syntax, or letrec-syntax are spliced
  ;; into the body.
  ;;
  ;; outer-form is fully wrapped w/source
  (lambda (ribcage source-exp body r k)
    (define return
      (lambda (exprs ids vars vals inits)
        (check-defined-ids source-exp ids)
        (k exprs ids vars vals inits)))
    (let parse ((body body) (ids '()) (vars '()) (vals '()) (inits '()))
      (if (null? body)
          (return body ids vars vals inits)
          (let ((e (cdar body)) (er (caar body)))
            (call-with-values
              (lambda () (syntax-type e er empty-wrap no-source ribcage))
              (lambda (type value e w s)
                (case type
                  ((define-form)
                   (parse-define e w s
                     (lambda (id rhs w)
                       (let ((id (wrap id w)) (label (gen-label)))
                         (let ((var (gen-var id)))
                           (extend-ribcage! ribcage id label)
                           (extend-store! r label (make-binding 'lexical var))
                           (parse
                             (cdr body)
                             (cons id ids)
                             (cons var vars)
                             (cons (cons er (wrap rhs w)) vals)
                             inits))))))
                  ((define-syntax-form)
                   (parse-define-syntax e w s
                     (lambda (id rhs w)
                       (let ((id (wrap id w))
                             (label (gen-label))
                             (exp (chi rhs (transformer-env er) w)))
                         (extend-ribcage! ribcage id label)
                         (extend-store! r label (make-binding 'deferred exp))
                         (parse (cdr body) (cons id ids) vars vals inits)))))
                  ((module-form)
                   (let* ((*ribcage (make-empty-ribcage))
                          (*w (make-wrap (wrap-marks w) (cons *ribcage (wrap-subst w)))))
                     (parse-module e w s *w
                       (lambda (id exports forms)
                         (chi-internal *ribcage (source-wrap e w s)
                           (map (lambda (d) (cons er d)) forms) r
                           (lambda (*body *ids *vars *vals *inits)
                             ; valid bound ids checked already by chi-internal
                             (check-module-exports source-exp (flatten-exports exports) *ids)
                             (let ((iface (make-trimmed-interface exports))
                                   (vars (append *vars vars))
                                   (vals (append *vals vals))
                                   (inits (append inits *inits *body)))
                               (if id
                                   (let ((label (gen-label)))
                                     (extend-ribcage! ribcage id label)
                                     (extend-store! r label (make-binding 'module iface))
                                     (parse (cdr body) (cons id ids) vars vals inits))
                                   (let ()
                                     (do-import! iface ribcage)
                                     (parse (cdr body) (cons iface ids) vars vals inits))))))))))
                 ((import-form)
                  (parse-import e w s
                    (lambda (mid)
                      (let ((mlabel (id-var-name mid empty-wrap)))
                        (let ((binding (lookup mlabel r)))
                          (case (car binding)
                            ((module)
                             (let ((iface (cdr binding)))
                               (when value (extend-ribcage-barrier! ribcage value))
                               (do-import! iface ribcage)
                               (parse (cdr body) (cons iface ids) vars vals inits)))
                            ((displaced-lexical) (displaced-lexical-error mid))
                            (else (syntax-error mid "import from unknown module"))))))))
                  ((begin-form)
                   (syntax-case e ()
                     ((_ e1 ...)
                      (parse (let f ((forms (syntax (e1 ...))))
                               (if (null? forms)
                                   (cdr body)
                                   (cons (cons er (wrap (car forms) w))
                                         (f (cdr forms)))))
                        ids vars vals inits))))
                  ((local-syntax-form)
                   (chi-local-syntax value e er w s
                     (lambda (forms er w s)
                       (parse (let f ((forms forms))
                                (if (null? forms)
                                    (cdr body)
                                    (cons (cons er (wrap (car forms) w))
                                          (f (cdr forms)))))
                         ids vars vals inits))))
                  (else ; found a non-definition
                   (return (cons (cons er (source-wrap e w s)) (cdr body))
                           ids vars vals inits))))))))))

(define do-import!
  (lambda (interface ribcage)
    (let ((token (interface-token interface)))
      (if token
          (extend-ribcage-subst! ribcage token)
          (vfor-each
            (lambda (id)
              (let ((label1 (id-var-name-loc id empty-wrap)))
                (unless label1
                  (syntax-error id "exported identifier not visible"))
                (extend-ribcage! ribcage id label1)))
            (interface-exports interface))))))

(define parse-module
  (lambda (e w s *w k)
    (define listify
      (lambda (exports)
        (if (null? exports)
            '()
            (cons (syntax-case (car exports) ()
                    ((ex ...) (listify (syntax (ex ...))))
                    (x (if (id? (syntax x))
                           (wrap (syntax x) *w)
                           (syntax-error (source-wrap e w s)
                             "invalid exports list in"))))
                  (listify (cdr exports))))))
    (define return
      (lambda (id exports forms)
        (k id (listify exports) (map (lambda (x) (wrap x *w)) forms))))
    (syntax-case e ()
      ((_ (ex ...) form ...)
       (return #f (syntax (ex ...)) (syntax (form ...))))
      ((_ mid (ex ...) form ...)
       (id? (syntax mid))
      ; id receives old wrap so it won't be confused with id of same name
      ; defined within the module
       (return (wrap (syntax mid) w) (syntax (ex ...)) (syntax (form ...))))
      (_ (syntax-error (source-wrap e w s))))))

(define parse-import
  (lambda (e w s k)
    (syntax-case e ()
      ((_ mid)
       (id? (syntax mid))
       (k (wrap (syntax mid) w)))
      (_ (syntax-error (source-wrap e w s))))))

(define parse-define
  (lambda (e w s k)
    (syntax-case e ()
      ((_ name val)
       (id? (syntax name))
       (k (syntax name) (syntax val) w))
      ((_ (name . args) e1 e2 ...)
       (and (id? (syntax name))
            (valid-bound-ids? (lambda-var-list (syntax args))))
       (k (wrap (syntax name) w)
          (cons (syntax lambda) (wrap (syntax (args e1 e2 ...)) w))
          empty-wrap))
      ((_ name)
       (id? (syntax name))
       (k (wrap (syntax name) w) (syntax (void)) empty-wrap))
      (_ (syntax-error (source-wrap e w s))))))

(define parse-define-syntax
  (lambda (e w s k)
    (syntax-case e ()
      ((_ name val)
       (id? (syntax name))
       (k (syntax name) (syntax val) w))
      (_ (syntax-error (source-wrap e w s))))))

(define chi-lambda-clause
  (lambda (e c r w k)
    (syntax-case c ()
      (((id ...) e1 e2 ...)
       (let ((ids (syntax (id ...))))
         (if (not (valid-bound-ids? ids))
             (syntax-error e "invalid parameter list in")
             (let ((labels (gen-labels ids))
                   (new-vars (map gen-var ids)))
               (k new-vars
                  (chi-body (syntax (e1 e2 ...))
                            e
                            (extend-var-env* labels new-vars r)
                            (make-binding-wrap ids labels w)))))))
      ((ids e1 e2 ...)
       (let ((old-ids (lambda-var-list (syntax ids))))
         (if (not (valid-bound-ids? old-ids))
             (syntax-error e "invalid parameter list in")
             (let ((labels (gen-labels old-ids))
                   (new-vars (map gen-var old-ids)))
               (k (let f ((ls1 (cdr new-vars)) (ls2 (car new-vars)))
                    (if (null? ls1)
                        ls2
                        (f (cdr ls1) (cons (car ls1) ls2))))
                  (chi-body (syntax (e1 e2 ...))
                            e
                            (extend-var-env* labels new-vars r)
                            (make-binding-wrap old-ids labels w)))))))
      (_ (syntax-error e)))))

(define chi-local-syntax
  (lambda (rec? e r w s k)
    (syntax-case e ()
      ((_ ((id val) ...) e1 e2 ...)
       (let ((ids (syntax (id ...))))
         (if (not (valid-bound-ids? ids))
             (invalid-ids-error (map (lambda (x) (wrap x w)) ids)
               (source-wrap e w s)
               "keyword")
             (let ((labels (gen-labels ids)))
               (let ((new-w (make-binding-wrap ids labels w)))
                 (k (syntax (e1 e2 ...))
                    (extend-env*
                      labels
                      (let ((w (if rec? new-w w))
                            (trans-r (transformer-env r)))
                        (map (lambda (x) (make-binding 'deferred (chi x trans-r w))) (syntax (val ...))))
                      r)
                    new-w
                    s))))))
      (_ (syntax-error (source-wrap e w s))))))

(define chi-void
  (lambda ()
    (build-application no-source (build-primref no-source 'void) '())))

(define ellipsis?
  (lambda (x)
    (and (nonsymbol-id? x)
         (literal-id=? x (syntax (... ...))))))

;;; data

;;; strips all annotations from potentially circular reader output

(define strip-annotation
  (lambda (x parent)
    (cond
      ((pair? x)
       (let ((new (cons #f #f)))
         (when parent (set-annotation-stripped! parent new))
         (set-car! new (strip-annotation (car x) #f))
         (set-cdr! new (strip-annotation (cdr x) #f))
         new))
      ((annotation? x)
       (or (annotation-stripped x)
           (strip-annotation (annotation-expression x) x)))
      ((vector? x)
       (let ((new (make-vector (vector-length x))))
         (when parent (set-annotation-stripped! parent new))
         (let loop ((i (- (vector-length x) 1)))
           (unless (fx< i 0)
             (vector-set! new i (strip-annotation (vector-ref x i) #f))
             (loop (fx- i 1))))
         new))
      (else x))))

;;; strips syntax-objects down to top-wrap; if top-wrap is layered directly
;;; on an annotation, strips the annotation as well.
;;; since only the head of a list is annotated by the reader, not each pair
;;; in the spine, we also check for pairs whose cars are annotated in case
;;; we've been passed the cdr of an annotated list

(define strip*
  (lambda (x w fn)
    (if (top-marked? w)
        (fn x)
        (let f ((x x))
          (cond
            ((syntax-object? x)
             (strip* (syntax-object-expression x) (syntax-object-wrap x) fn))
            ((pair? x)
             (let ((a (f (car x))) (d (f (cdr x))))
               (if (and (eq? a (car x)) (eq? d (cdr x)))
                   x
                   (cons a d))))
            ((vector? x)
             (let ((old (vector->list x)))
                (let ((new (map f old)))
                   (if (andmap eq? old new) x (list->vector new)))))
            (else x))))))

(define strip
  (lambda (x w)
    (strip* x w
      (lambda (x)
        (if (or (annotation? x) (and (pair? x) (annotation? (car x))))
            (strip-annotation x #f)
            x)))))

;;; lexical variables

(define gen-var
  (lambda (id)
    (let ((id (if (syntax-object? id) (syntax-object-expression id) id)))
      (if (annotation? id)
          (build-lexical-var (annotation-source id) (annotation-expression id))
          (build-lexical-var no-source id)))))

(define lambda-var-list
  (lambda (vars)
    (let lvl ((vars vars) (ls '()) (w empty-wrap))
       (cond
         ((pair? vars) (lvl (cdr vars) (cons (wrap (car vars) w) ls) w))
         ((id? vars) (cons (wrap vars w) ls))
         ((null? vars) ls)
         ((syntax-object? vars)
          (lvl (syntax-object-expression vars)
               ls
               (join-wraps w (syntax-object-wrap vars))))
         ((annotation? vars)
          (lvl (annotation-expression vars) ls w))
       ; include anything else to be caught by subsequent error
       ; checking
         (else (cons vars ls))))))


; must precede global-extends

(set! $sc-put-cte
  (lambda (id b)
    (define put-token
      (lambda (id token)
        (define cons-id
          (lambda (id x)
            (if (not x) id (cons id x))))
        (define weed
          (lambda (id x)
            (if (pair? x)
                (if (bound-id=? (car x) id) ; could just check same-marks
                    (weed id (cdr x))
                    (cons-id (car x) (weed id (cdr x))))
                (if (or (not x) (bound-id=? x id))
                    #f
                    x))))
        (let ((sym (id-sym-name id)))
          (let ((x (weed id (getprop sym token))))
            (if (and (not x) (symbol? id))
               ; don't pollute property list when all we have is a plain
               ; top-level binding, since that's what's assumed anyway
                (remprop sym token)
                (putprop sym token (cons-id id x)))))))
    (define sc-put-module
      (lambda (exports token)
        (vfor-each
          (lambda (id) (put-token id token))
          exports)))
    (define (put-cte id binding)
     ;; making assumption here that all macros should be visible to the user and that system
     ;; globals don't come through here (primvars.ss sets up their properties)
      (let ((sym (if (symbol? id) id (id-var-name id empty-wrap))))
        (putprop sym '*sc-expander* binding)))
    (let ((binding (or (sanitize-binding b) (error 'define-syntax "invalid transformer ~s" b))))
      (case (binding-type binding)
        ((module)
         (let ((iface (binding-value binding)))
           (sc-put-module (interface-exports iface) (interface-token iface)))
         (put-cte id binding))
        ((do-import) ; fake binding: id is module id, binding-value is import token
         (let ((token (binding-value b)))
           (let ((b (lookup (id-var-name id empty-wrap) null-env)))
             (case (binding-type b)
               ((module)
                (let ((iface (binding-value b)))
                  (unless (eq? (interface-token iface) token)
                    (syntax-error id "import mismatch for module"))
                  (sc-put-module (interface-exports iface) '*top*)))
               (else (syntax-error id "import from unknown module"))))))
        (else (put-cte id binding))))))


;;; core transformers

(global-extend 'local-syntax 'letrec-syntax #t)
(global-extend 'local-syntax 'let-syntax #f)


(global-extend 'core 'fluid-let-syntax
  (lambda (e r w s)
    (syntax-case e ()
      ((_ ((var val) ...) e1 e2 ...)
       (valid-bound-ids? (syntax (var ...)))
       (let ((names (map (lambda (x) (id-var-name x w)) (syntax (var ...)))))
         (for-each
           (lambda (id n)
             (case (binding-type (lookup n r))
               ((displaced-lexical) (displaced-lexical-error (wrap id w)))))
           (syntax (var ...))
           names)
         (chi-body
           (syntax (e1 e2 ...))
           (source-wrap e w s)
           (extend-env*
             names
             (let ((trans-r (transformer-env r)))
               (map (lambda (x) (make-binding 'deferred (chi x trans-r w))) (syntax (val ...))))
             r)
           w)))
      (_ (syntax-error (source-wrap e w s))))))

(global-extend 'core 'quote
   (lambda (e r w s)
      (syntax-case e ()
         ((_ e) (build-data s (strip (syntax e) w)))
         (_ (syntax-error (source-wrap e w s))))))

(global-extend 'core 'syntax
  (let ()
    (define gen-syntax
      (lambda (src e r maps ellipsis?)
        (if (id? e)
            (let ((label (id-var-name e empty-wrap)))
              (let ((b (lookup label r)))
                (if (eq? (binding-type b) 'syntax)
                    (call-with-values
                      (lambda ()
                        (let ((var.lev (binding-value b)))
                          (gen-ref src (car var.lev) (cdr var.lev) maps)))
                      (lambda (var maps) (values `(ref ,var) maps)))
                    (if (ellipsis? e)
                        (syntax-error src "misplaced ellipsis in syntax form")
                        (values `(quote ,e) maps)))))
            (syntax-case e ()
              ((dots e)
               (ellipsis? (syntax dots))
               (gen-syntax src (syntax e) r maps (lambda (x) #f)))
              ((x dots . y)
               ; this could be about a dozen lines of code, except that we
               ; choose to handle (syntax (x ... ...)) forms
               (ellipsis? (syntax dots))
               (let f ((y (syntax y))
                       (k (lambda (maps)
                            (call-with-values
                              (lambda ()
                                (gen-syntax src (syntax x) r
                                  (cons '() maps) ellipsis?))
                              (lambda (x maps)
                                (if (null? (car maps))
                                    (syntax-error src
                                      "extra ellipsis in syntax form")
                                    (values (gen-map x (car maps))
                                            (cdr maps))))))))
                 (syntax-case y ()
                   ((dots . y)
                    (ellipsis? (syntax dots))
                    (f (syntax y)
                       (lambda (maps)
                         (call-with-values
                           (lambda () (k (cons '() maps)))
                           (lambda (x maps)
                             (if (null? (car maps))
                                 (syntax-error src
                                   "extra ellipsis in syntax form")
                                 (values (gen-mappend x (car maps))
                                         (cdr maps))))))))
                   (_ (call-with-values
                        (lambda () (gen-syntax src y r maps ellipsis?))
                        (lambda (y maps)
                          (call-with-values
                            (lambda () (k maps))
                            (lambda (x maps)
                              (values (gen-append x y) maps)))))))))
              ((x . y)
               (call-with-values
                 (lambda () (gen-syntax src (syntax x) r maps ellipsis?))
                 (lambda (x maps)
                   (call-with-values
                     (lambda () (gen-syntax src (syntax y) r maps ellipsis?))
                     (lambda (y maps) (values (gen-cons x y) maps))))))
              (#(e1 e2 ...)
               (call-with-values
                 (lambda ()
                   (gen-syntax src (syntax (e1 e2 ...)) r maps ellipsis?))
                 (lambda (e maps) (values (gen-vector e) maps))))
              (_ (values `(quote ,e) maps))))))

    (define gen-ref
      (lambda (src var level maps)
        (if (fx= level 0)
            (values var maps)
            (if (null? maps)
                (syntax-error src "missing ellipsis in syntax form")
                (call-with-values
                  (lambda () (gen-ref src var (fx- level 1) (cdr maps)))
                  (lambda (outer-var outer-maps)
                    (let ((b (assq outer-var (car maps))))
                      (if b
                          (values (cdr b) maps)
                          (let ((inner-var (gen-var 'tmp)))
                            (values inner-var
                                    (cons (cons (cons outer-var inner-var)
                                                (car maps))
                                          outer-maps)))))))))))

    (define gen-mappend
      (lambda (e map-env)
        `(apply (primitive append) ,(gen-map e map-env))))

    (define gen-map
      (lambda (e map-env)
        (let ((formals (map cdr map-env))
              (actuals (map (lambda (x) `(ref ,(car x))) map-env)))
          (cond
            ((eq? (car e) 'ref)
             ; identity map equivalence:
             ; (map (lambda (x) x) y) == y
             (car actuals))
            ((andmap
                (lambda (x) (and (eq? (car x) 'ref) (memq (cadr x) formals)))
                (cdr e))
             ; eta map equivalence:
             ; (map (lambda (x ...) (f x ...)) y ...) == (map f y ...)
             `(map (primitive ,(car e))
                   ,@(map (let ((r (map cons formals actuals)))
                            (lambda (x) (cdr (assq (cadr x) r))))
                          (cdr e))))
            (else `(map (lambda ,formals ,e) ,@actuals))))))

    (define gen-cons
      (lambda (x y)
        (case (car y)
          ((quote)
           (if (eq? (car x) 'quote)
               `(quote (,(cadr x) . ,(cadr y)))
               (if (eq? (cadr y) '())
                   `(list ,x)
                   `(cons ,x ,y))))
          ((list) `(list ,x ,@(cdr y)))
          (else `(cons ,x ,y)))))

    (define gen-append
      (lambda (x y)
        (if (equal? y '(quote ()))
            x
            `(append ,x ,y))))

    (define gen-vector
      (lambda (x)
        (cond
          ((eq? (car x) 'list) `(vector ,@(cdr x)))
          ((eq? (car x) 'quote) `(quote #(,@(cadr x))))
          (else `(list->vector ,x)))))


    (define regen
      (lambda (x)
        (case (car x)
          ((ref) (build-lexical-reference 'value no-source (cadr x)))
          ((primitive) (build-primref no-source (cadr x)))
          ((quote) (build-data no-source (cadr x)))
          ((lambda) (build-lambda no-source (cadr x) (regen (caddr x))))
          ((map) (let ((ls (map regen (cdr x))))
                   (build-application no-source
                     (if (fx= (length ls) 2)
                         (build-primref no-source 'map)
                        ; really need to do our own checking here
                         (build-primref no-source 2 'map)) ; require error check
                     ls)))
          (else (build-application no-source
                  (build-primref no-source (car x))
                  (map regen (cdr x)))))))

    (lambda (e r w s)
      (let ((e (source-wrap e w s)))
        (syntax-case e ()
          ((_ x)
           (call-with-values
             (lambda () (gen-syntax e (syntax x) r '() ellipsis?))
             (lambda (e maps) (regen e))))
          (_ (syntax-error e)))))))


(global-extend 'core 'lambda
   (lambda (e r w s)
      (syntax-case e ()
         ((_ . c)
          (chi-lambda-clause (source-wrap e w s) (syntax c) r w
            (lambda (vars body) (build-lambda s vars body)))))))


(global-extend 'core 'letrec
  (lambda (e r w s)
    (syntax-case e ()
      ((_ ((id val) ...) e1 e2 ...)
       (let ((ids (syntax (id ...))))
         (if (not (valid-bound-ids? ids))
             (invalid-ids-error (map (lambda (x) (wrap x w)) ids)
               (source-wrap e w s) "bound variable")
             (let ((labels (gen-labels ids))
                   (new-vars (map gen-var ids)))
               (let ((w (make-binding-wrap ids labels w))
                    (r (extend-var-env* labels new-vars r)))
                 (build-letrec s
                   new-vars
                   (map (lambda (x) (chi x r w)) (syntax (val ...)))
                   (chi-body (syntax (e1 e2 ...)) (source-wrap e w s) r w)))))))
      (_ (syntax-error (source-wrap e w s))))))

(global-extend 'core 'if
   (lambda (e r w s)
      (syntax-case e ()
         ((_ test then)
          (build-conditional s
             (chi (syntax test) r w)
             (chi (syntax then) r w)
             (chi-void)))
         ((_ test then else)
          (build-conditional s
             (chi (syntax test) r w)
             (chi (syntax then) r w)
             (chi (syntax else) r w)))
         (_ (syntax-error (source-wrap e w s))))))



(global-extend 'set! 'set! '())

(global-extend 'begin 'begin '())

(global-extend 'module-key 'module '())
(global-extend 'import 'import #f)
(global-extend 'import 'import-only #t)

(global-extend 'define 'define '())

(global-extend 'define-syntax 'define-syntax '())

(global-extend 'eval-when 'eval-when '())

(global-extend 'core 'syntax-case
  (let ()
    (define convert-pattern
      ; accepts pattern & keys
      ; returns syntax-dispatch pattern & ids
      (lambda (pattern keys)
        (let cvt ((p pattern) (n 0) (ids '()))
          (if (id? p)
              (if (bound-id-member? p keys)
                  (values (vector 'free-id p) ids)
                  (values 'any (cons (cons p n) ids)))
              (syntax-case p ()
                ((x dots)
                 (ellipsis? (syntax dots))
                 (call-with-values
                   (lambda () (cvt (syntax x) (fx+ n 1) ids))
                   (lambda (p ids)
                     (values (if (eq? p 'any) 'each-any (vector 'each p))
                             ids))))
                ((x . y)
                 (call-with-values
                   (lambda () (cvt (syntax y) n ids))
                   (lambda (y ids)
                     (call-with-values
                       (lambda () (cvt (syntax x) n ids))
                       (lambda (x ids)
                         (values (cons x y) ids))))))
                (() (values '() ids))
                (#(x ...)
                 (call-with-values
                   (lambda () (cvt (syntax (x ...)) n ids))
                   (lambda (p ids) (values (vector 'vector p) ids))))
                (x (values (vector 'atom (strip p empty-wrap)) ids)))))))

    (define build-dispatch-call
      (lambda (pvars exp y r)
        (let ((ids (map car pvars)) (levels (map cdr pvars)))
          (let ((labels (gen-labels ids)) (new-vars (map gen-var ids)))
            (build-application no-source
              (build-primref no-source 'apply)
              (list (build-lambda no-source new-vars
                      (chi exp
                         (extend-env*
                             labels
                             (map (lambda (var level)
                                    (make-binding 'syntax `(,var . ,level)))
                                  new-vars
                                  (map cdr pvars))
                             r)
                           (make-binding-wrap ids labels empty-wrap)))
                    y))))))

    (define gen-clause
      (lambda (x keys clauses r pat fender exp)
        (call-with-values
          (lambda () (convert-pattern pat keys))
          (lambda (p pvars)
            (cond
              ((not (distinct-bound-ids? (map car pvars)))
               (invalid-ids-error (map car pvars) pat "pattern variable"))
              ((not (andmap (lambda (x) (not (ellipsis? (car x)))) pvars))
               (syntax-error pat
                 "misplaced ellipsis in syntax-case pattern"))
              (else
               (let ((y (gen-var 'tmp)))
                 ; fat finger binding and references to temp variable y
                 (build-application no-source
                   (build-lambda no-source (list y)
                     (let-syntax ((y (identifier-syntax
                                       (build-lexical-reference 'value no-source y))))
                       (build-conditional no-source
                         (syntax-case fender ()
                           (#t y)
                           (_ (build-conditional no-source
                                y
                                (build-dispatch-call pvars fender y r)
                                (build-data no-source #f))))
                         (build-dispatch-call pvars exp y r)
                         (gen-syntax-case x keys clauses r))))
                   (list (if (eq? p 'any)
                             (build-application no-source
                               (build-primref no-source 'list)
                               (list (build-lexical-reference no-source 'value x)))
                             (build-application no-source
                               (build-primref no-source '$syntax-dispatch)
                               (list (build-lexical-reference no-source 'value x)
                                     (build-data no-source p)))))))))))))

    (define gen-syntax-case
      (lambda (x keys clauses r)
        (if (null? clauses)
            (build-application no-source
              (build-primref no-source 'syntax-error)
              (list (build-lexical-reference 'value no-source x)))
            (syntax-case (car clauses) ()
              ((pat exp)
               (if (and (id? (syntax pat))
                        (not (bound-id-member? (syntax pat) keys))
                        (not (ellipsis? (syntax pat))))
                   (let ((label (gen-label))
                         (var (gen-var (syntax pat))))
                     (build-application no-source
                       (build-lambda no-source (list var)
                         (chi (syntax exp)
                              (extend-env label (make-binding 'syntax `(,var . 0)) r)
                              (make-binding-wrap (syntax (pat))
                                (list label) empty-wrap)))
                       (list (build-lexical-reference 'value no-source x))))
                   (gen-clause x keys (cdr clauses) r
                     (syntax pat) #t (syntax exp))))
              ((pat fender exp)
               (gen-clause x keys (cdr clauses) r
                 (syntax pat) (syntax fender) (syntax exp)))
              (_ (syntax-error (car clauses) "invalid syntax-case clause"))))))

    (lambda (e r w s)
      (let ((e (source-wrap e w s)))
        (syntax-case e ()
          ((_ val (key ...) m ...)
           (if (andmap (lambda (x) (and (id? x) (not (ellipsis? x))))
                       (syntax (key ...)))
               (let ((x (gen-var 'tmp)))
                 ; fat finger binding and references to temp variable x
                 (build-application s
                   (build-lambda no-source (list x)
                     (gen-syntax-case x
                       (syntax (key ...)) (syntax (m ...))
                       r))
                   (list (chi (syntax val) r empty-wrap))))
               (syntax-error e "invalid literals list in"))))))))

;;; The portable sc-expand seeds chi-top's mode m with 'e (for
;;; evaluating) and esew (which stands for "eval syntax expanders
;;; when") with '(eval).  In Chez Scheme, m is set to 'c instead of e
;;; if we are compiling a file, and esew is set to
;;; (eval-syntactic-expanders-when), which defaults to the list
;;; '(compile load eval).  This means that, by default, top-level
;;; syntactic definitions are evaluated immediately after they are
;;; expanded, and the expanded definitions are also residualized into
;;; the object file if we are compiling a file.
(set! sc-expand
  (let ((m 'e) (esew '(eval))
        (user-ribcage
         (let ((ribcage (make-empty-ribcage)))
           (extend-ribcage-subst! ribcage '*top*)
           ribcage)))
    (let ((user-top-wrap
           (make-wrap (wrap-marks top-wrap)
             (cons user-ribcage (wrap-subst top-wrap)))))
      (lambda (x)
        (if (and (pair? x) (equal? (car x) noexpand))
            (cadr x)
            (chi-top x null-env user-top-wrap m esew user-ribcage))))))

(set! identifier?
  (lambda (x)
    (nonsymbol-id? x)))

(set! datum->syntax-object
  (lambda (id datum)
    (arg-check nonsymbol-id? id 'datum->syntax-object)
    (make-syntax-object datum (syntax-object-wrap id))))

(set! syntax-object->datum
  ; accepts any object, since syntax objects may consist partially
  ; or entirely of unwrapped, nonsymbolic data
  (lambda (x)
    (strip x empty-wrap)))

(set! generate-temporaries
  (lambda (ls)
    (arg-check list? ls 'generate-temporaries)
    (map (lambda (x) (wrap (gensym-hook) top-wrap)) ls)))

(set! free-identifier=?
   (lambda (x y)
      (arg-check nonsymbol-id? x 'free-identifier=?)
      (arg-check nonsymbol-id? y 'free-identifier=?)
      (free-id=? x y)))

(set! bound-identifier=?
   (lambda (x y)
      (arg-check nonsymbol-id? x 'bound-identifier=?)
      (arg-check nonsymbol-id? y 'bound-identifier=?)
      (bound-id=? x y)))


(set! syntax-error
  (lambda (object . messages)
    (for-each (lambda (x) (arg-check string? x 'syntax-error)) messages)
    (let ((message (if (null? messages)
                       "invalid syntax"
                       (apply string-append messages))))
      (error-hook #f message (strip object empty-wrap)))))

;;; syntax-dispatch expects an expression and a pattern.  If the expression
;;; matches the pattern a list of the matching expressions for each
;;; "any" is returned.  Otherwise, #f is returned.  (This use of #f will
;;; not work on r4rs implementations that violate the ieee requirement
;;; that #f and () be distinct.)

;;; The expression is matched with the pattern as follows:

;;; pattern:                           matches:
;;;   ()                                 empty list
;;;   any                                anything
;;;   (<pattern>1 . <pattern>2)          (<pattern>1 . <pattern>2)
;;;   each-any                           (any*)
;;;   #(free-id <key>)                   <key> with free-identifier=?
;;;   #(each <pattern>)                  (<pattern>*)
;;;   #(vector <pattern>)                (list->vector <pattern>)
;;;   #(atom <object>)                   <object> with "equal?"

;;; Vector cops out to pair under assumption that vectors are rare.  If
;;; not, should convert to:
;;;   #(vector <pattern>*)               #(<pattern>*)

(let ()

(define match-each
  (lambda (e p w)
    (cond
      ((annotation? e)
       (match-each (annotation-expression e) p w))
      ((pair? e)
       (let ((first (match (car e) p w '())))
         (and first
              (let ((rest (match-each (cdr e) p w)))
                 (and rest (cons first rest))))))
      ((null? e) '())
      ((syntax-object? e)
       (match-each (syntax-object-expression e)
                   p
                   (join-wraps w (syntax-object-wrap e))))
      (else #f))))

(define match-each-any
  (lambda (e w)
    (cond
      ((annotation? e)
       (match-each-any (annotation-expression e) w))
      ((pair? e)
       (let ((l (match-each-any (cdr e) w)))
         (and l (cons (wrap (car e) w) l))))
      ((null? e) '())
      ((syntax-object? e)
       (match-each-any (syntax-object-expression e)
                       (join-wraps w (syntax-object-wrap e))))
      (else #f))))

(define match-empty
  (lambda (p r)
    (cond
      ((null? p) r)
      ((eq? p 'any) (cons '() r))
      ((pair? p) (match-empty (car p) (match-empty (cdr p) r)))
      ((eq? p 'each-any) (cons '() r))
      (else
       (case (vector-ref p 0)
         ((each) (match-empty (vector-ref p 1) r))
         ((free-id atom) r)
         ((vector) (match-empty (vector-ref p 1) r)))))))

(define match*
  (lambda (e p w r)
    (cond
      ((null? p) (and (null? e) r))
      ((pair? p)
       (and (pair? e) (match (car e) (car p) w
                        (match (cdr e) (cdr p) w r))))
      ((eq? p 'each-any)
       (let ((l (match-each-any e w))) (and l (cons l r))))
      (else
       (case (vector-ref p 0)
         ((each)
          (if (null? e)
              (match-empty (vector-ref p 1) r)
              (let ((l (match-each e (vector-ref p 1) w)))
                (and l
                     (let collect ((l l))
                       (if (null? (car l))
                           r
                           (cons (map car l) (collect (map cdr l)))))))))
         ((free-id) (and (id? e) (literal-id=? (wrap e w) (vector-ref p 1)) r))
         ((atom) (and (equal? (vector-ref p 1) (strip e w)) r))
         ((vector)
          (and (vector? e)
               (match (vector->list e) (vector-ref p 1) w r))))))))

(define match
  (lambda (e p w r)
    (cond
      ((not r) #f)
      ((eq? p 'any) (cons (wrap e w) r))
      ((syntax-object? e)
       (match*
         (unannotate (syntax-object-expression e))
         p
         (join-wraps w (syntax-object-wrap e))
         r))
      (else (match* (unannotate e) p w r)))))

(set! $syntax-dispatch
  (lambda (e p)
    (cond
      ((eq? p 'any) (list e))
      ((syntax-object? e)
       (match* (unannotate (syntax-object-expression e))
         p (syntax-object-wrap e) '()))
      (else (match* (unannotate e) p empty-wrap '())))))
))


(define-syntax with-syntax
   (lambda (x)
      (syntax-case x ()
         ((_ () e1 e2 ...)
          (syntax (begin e1 e2 ...)))
         ((_ ((out in)) e1 e2 ...)
          (syntax (syntax-case in () (out (begin e1 e2 ...)))))
         ((_ ((out in) ...) e1 e2 ...)
          (syntax (syntax-case (list in ...) ()
                     ((out ...) (begin e1 e2 ...))))))))
 
(define-syntax syntax-rules
  (lambda (x)
    (syntax-case x ()
      ((_ (k ...) ((keyword . pattern) template) ...)
       (syntax (lambda (x)
                (syntax-case x (k ...)
                  ((dummy . pattern) (syntax template))
                  ...)))))))

(define-syntax or
   (lambda (x)
      (syntax-case x ()
         ((_) (syntax #f))
         ((_ e) (syntax e))
         ((_ e1 e2 e3 ...)
          (syntax (let ((t e1)) (if t t (or e2 e3 ...))))))))

(define-syntax and
   (lambda (x)
      (syntax-case x ()
         ((_ e1 e2 e3 ...) (syntax (if e1 (and e2 e3 ...) #f)))
         ((_ e) (syntax e))
         ((_) (syntax #t)))))

(define-syntax let
   (lambda (x)
      (syntax-case x ()
         ((_ ((x v) ...) e1 e2 ...)
          (andmap identifier? (syntax (x ...)))
          (syntax ((lambda (x ...) e1 e2 ...) v ...)))
         ((_ f ((x v) ...) e1 e2 ...)
          (andmap identifier? (syntax (f x ...)))
          (syntax ((letrec ((f (lambda (x ...) e1 e2 ...))) f)
                    v ...))))))

(define-syntax let*
  (lambda (x)
    (syntax-case x ()
      ((let* ((x v) ...) e1 e2 ...)
       (andmap identifier? (syntax (x ...)))
       (let f ((bindings (syntax ((x v)  ...))))
         (if (null? bindings)
             (syntax (let () e1 e2 ...))
             (with-syntax ((body (f (cdr bindings)))
                           (binding (car bindings)))
               (syntax (let (binding) body)))))))))

(define-syntax cond
  (lambda (x)
    (syntax-case x ()
      ((_ m1 m2 ...)
       (let f ((clause (syntax m1)) (clauses (syntax (m2 ...))))
         (if (null? clauses)
             (syntax-case clause (else =>)
               ((else e1 e2 ...) (syntax (begin e1 e2 ...)))
               ((e0) (syntax (let ((t e0)) (if t t))))
               ((e0 => e1) (syntax (let ((t e0)) (if t (e1 t)))))
               ((e0 e1 e2 ...) (syntax (if e0 (begin e1 e2 ...))))
               (_ (syntax-error x)))
             (with-syntax ((rest (f (car clauses) (cdr clauses))))
               (syntax-case clause (else =>)
                 ((e0) (syntax (let ((t e0)) (if t t rest))))
                 ((e0 => e1) (syntax (let ((t e0)) (if t (e1 t) rest))))
                 ((e0 e1 e2 ...) (syntax (if e0 (begin e1 e2 ...) rest)))
                 (_ (syntax-error x))))))))))

(define-syntax do
   (lambda (orig-x)
      (syntax-case orig-x ()
         ((_ ((var init . step) ...) (e0 e1 ...) c ...)
          (with-syntax (((step ...)
                         (map (lambda (v s)
                                 (syntax-case s ()
                                    (() v)
                                    ((e) (syntax e))
                                    (_ (syntax-error orig-x))))
                              (syntax (var ...))
                              (syntax (step ...)))))
             (syntax-case (syntax (e1 ...)) ()
                (() (syntax (let doloop ((var init) ...)
                               (if (not e0)
                                   (begin c ... (doloop step ...))))))
                ((e1 e2 ...)
                 (syntax (let doloop ((var init) ...)
                            (if e0
                                (begin e1 e2 ...)
                                (begin c ... (doloop step ...))))))))))))

(define-syntax quasiquote
   (letrec
     ; these are here because syntax-case uses literal-identifier=?,
     ; and we want the more precise free-identifier=?
      ((isquote? (lambda (x)
                   (and (identifier? x)
                        (free-identifier=? x (syntax quote)))))
       (islist? (lambda (x)
                  (and (identifier? x)
                       (free-identifier=? x (syntax list)))))
       (iscons? (lambda (x)
                  (and (identifier? x)
                       (free-identifier=? x (syntax cons)))))
       (quote-nil? (lambda (x)
                    (syntax-case x ()
                      ((quote? ()) (isquote? (syntax quote?)))
                      (_ #f))))
       (quasilist*
        (lambda (x y)
          (let f ((x x))
            (if (null? x)
                y
                (quasicons (car x) (f (cdr x)))))))
       (quasicons
        (lambda (x y)
          (with-syntax ((x x) (y y))
            (syntax-case (syntax y) ()
              ((quote? dy)
               (isquote? (syntax quote?))
               (syntax-case (syntax x) ()
                 ((quote? dx)
                  (isquote? (syntax quote?))
                  (syntax (quote (dx . dy))))
                 (_ (if (null? (syntax dy))
                        (syntax (list x))
                        (syntax (cons x y))))))
              ((listp . stuff)
               (islist? (syntax listp))
               (syntax (list x . stuff)))
              (else (syntax (cons x y)))))))
       (quasiappend
        (lambda (x y)
          (let ((ls (let f ((x x))
                      (if (null? x)
                          (if (quote-nil? y)
                              '()
                              (list y))
                          (if (quote-nil? (car x))
                              (f (cdr x))
                              (cons (car x) (f (cdr x))))))))
            (cond
              ((null? ls) (syntax (quote ())))
              ((null? (cdr ls)) (car ls))
              (else (with-syntax (((p ...) ls))
                      (syntax (append p ...))))))))
       (quasivector
        (lambda (x)
          (with-syntax ((pat-x x))
            (syntax-case (syntax pat-x) ()
              ((quote? (x ...))
               (isquote? (syntax quote?))
               (syntax (quote #(x ...))))
              (_ (let f ((x x) (k (lambda (ls) `(,(syntax vector) ,@ls))))
                   (syntax-case x ()
                     ((quote? (x ...))
                      (isquote? (syntax quote?))
                      (k (syntax ((quote x) ...))))
                     ((listp x ...)
                      (islist? (syntax listp))
                      (k (syntax (x ...))))
                     ((cons? x y)
                      (iscons? (syntax cons?))
                      (f (syntax y) (lambda (ls) (k (cons (syntax x) ls)))))
                     (else
                      (syntax (list->vector pat-x))))))))))
       (quasi
        (lambda (p lev)
           (syntax-case p (unquote unquote-splicing quasiquote)
              ((unquote p)
               (if (= lev 0)
                   (syntax p)
                   (quasicons (syntax (quote unquote))
                              (quasi (syntax (p)) (- lev 1)))))
              (((unquote p ...) . q)
               (if (= lev 0)
                   (quasilist* (syntax (p ...)) (quasi (syntax q) lev))
                   (quasicons (quasicons (syntax (quote unquote))
                                         (quasi (syntax (p ...)) (- lev 1)))
                              (quasi (syntax q) lev))))
              (((unquote-splicing p ...) . q)
               (if (= lev 0)
                   (quasiappend (syntax (p ...)) (quasi (syntax q) lev))
                   (quasicons (quasicons (syntax (quote unquote-splicing))
                                         (quasi (syntax (p ...)) (- lev 1)))
                              (quasi (syntax q) lev))))
              ((quasiquote p)
               (quasicons (syntax (quote quasiquote))
                          (quasi (syntax (p)) (+ lev 1))))
              ((p . q)
               (quasicons (quasi (syntax p) lev) (quasi (syntax q) lev)))
              (#(x ...) (quasivector (quasi (syntax (x ...)) lev)))
              (p (syntax (quote p)))))))
    (lambda (x)
       (syntax-case x ()
          ((_ e) (quasi (syntax e) 0))))))

(define-syntax include
  (lambda (x)
    (define read-file
      (lambda (fn k)
        (let ((p (open-input-file fn)))
          (let f ()
            (let ((x (read p)))
              (if (eof-object? x)
                  (begin (close-input-port p) '())
                  (cons (datum->syntax-object k x) (f))))))))
    (syntax-case x ()
      ((k filename)
       (let ((fn (syntax-object->datum (syntax filename))))
         (with-syntax (((exp ...) (read-file fn (syntax k))))
           (syntax (begin exp ...))))))))

(define-syntax unquote
  (lambda (x)
    (syntax-case x ()
      ((_ e ...)
       (syntax-error x
         "expression not valid outside of quasiquote")))))

(define-syntax unquote-splicing
  (lambda (x)
    (syntax-case x ()
      ((_ e ...)
       (syntax-error x
         "expression not valid outside of quasiquote")))))

(define-syntax case
  (lambda (x)
    (syntax-case x ()
      ((_ e m1 m2 ...)
       (with-syntax
         ((body (let f ((clause (syntax m1)) (clauses (syntax (m2 ...))))
                  (if (null? clauses)
                      (syntax-case clause (else)
                        ((else e1 e2 ...) (syntax (begin e1 e2 ...)))
                        (((k ...) e1 e2 ...)
                         (syntax (if (memv t '(k ...)) (begin e1 e2 ...))))
                        (_ (syntax-error x)))
                      (with-syntax ((rest (f (car clauses) (cdr clauses))))
                        (syntax-case clause (else)
                          (((k ...) e1 e2 ...)
                           (syntax (if (memv t '(k ...))
                                       (begin e1 e2 ...)
                                       rest)))
                          (_ (syntax-error x))))))))
         (syntax (let ((t e)) body)))))))

(define-syntax identifier-syntax
  (lambda (x)
    (syntax-case x (set!)
      ((_ e)
       (syntax
         (lambda (x)
           (syntax-case x ()
             (id
              (identifier? (syntax id))
              (syntax e))
             ((_ x (... ...))
              (syntax (e x (... ...))))))))
      ((_ (id exp1) ((set! var val) exp2))
       (and (identifier? (syntax id)) (identifier? (syntax var)))
       (syntax
         (cons 'macro!
           (lambda (x)
             (syntax-case x (set!)
               ((set! var val) (syntax exp2))
               ((id x (... ...)) (syntax (exp1 x (... ...))))
               (id (identifier? (syntax id)) (syntax exp1))))))))))

