;;;; -*-scheme-*-
;;;;
;;;; 	Copyright (C) 2001, 2003, 2006, 2009 Free Software Foundation, Inc.
;;;; 
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;;;; 


;;; Portable implementation of syntax-case
;;; Extracted from Chez Scheme Version 5.9f
;;; Authors: R. Kent Dybvig, Oscar Waddell, Bob Hieb, Carl Bruggeman

;;; Modified by Andy Wingo <wingo@pobox.com> according to the Git
;;; revision control logs corresponding to this file: 2009.

;;; Modified by Mikael Djurfeldt <djurfeldt@nada.kth.se> according
;;; to the ChangeLog distributed in the same directory as this file:
;;; 1997-08-19, 1997-09-03, 1997-09-10, 2000-08-13, 2000-08-24,
;;; 2000-09-12, 2001-03-08

;;; Copyright (c) 1992-1997 Cadence Research Systems
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
;;; Second Edition (R. Kent Dybvig, Prentice Hall, 1996).  Most are
;;; also documented in the R4RS and draft R5RS.
;;;
;;;   bound-identifier=?
;;;   datum->syntax
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
;;;   syntax->datum
;;;   syntax-rules
;;;   with-syntax
;;;
;;; All standard Scheme syntactic forms are supported by the expander
;;; or syntactic abstractions defined in this file.  Only the R4RS
;;; delay is omitted, since its expansion is implementation-dependent.

;;; The remaining exports are listed below:
;;;
;;;   (sc-expand datum)
;;;      if datum represents a valid expression, sc-expand returns an
;;;      expanded version of datum in a core language that includes no
;;;      syntactic abstractions.  The core language includes begin,
;;;      define, if, lambda, letrec, quote, and set!.
;;;   (eval-when situations expr ...)
;;;      conditionally evaluates expr ... at compile-time or run-time
;;;      depending upon situations (see the Chez Scheme System Manual,
;;;      Revision 3, for a complete description)
;;;   (syntax-violation who message form [subform])
;;;      used to report errors found during expansion
;;;   ($sc-dispatch e p)
;;;      used by expanded code to handle syntax-case matching

;;; The following nonstandard procedures must be provided by the
;;; implementation for this code to run using the standard portable
;;; hooks and output constructors. They are not used by expanded code,
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
;;; whether local or global, will result in a call to eval.  If, however,
;;; sc-expand has already been registered as the expander to be used
;;; by eval, and eval accepts one argument, nothing special must be done
;;; to support the "noexpand" flag, since it is handled by sc-expand.
;;;
;;; (gensym)
;;; returns a unique symbol each time it's called

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
;;; intuitive connection between syntax and quote.

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


;;; implementation information:

;;; "begin" is treated as a splicing construct at top level and at
;;; the beginning of bodies.  Any sequence of expressions that would
;;; be allowed where the "begin" occurs is allowed.

;;; "let-syntax" and "letrec-syntax" are also treated as splicing
;;; constructs, in violation of the R4RS appendix and probably the R5RS
;;; when it comes out.  A consequence, let-syntax and letrec-syntax do
;;; not create local contours, as do let and letrec.  Although the
;;; functionality is greater as it is presently implemented, we will
;;; probably change it to conform to the R4RS/expected R5RS.

;;; Objects with no standard print syntax, including objects containing
;;; cycles and syntax object, are allowed in quoted data as long as they
;;; are contained within a syntax form or produced by datum->syntax.
;;; Such objects are never copied.

;;; All identifiers that don't have macro definitions and are not bound
;;; lexically are assumed to be global variables

;;; Top-level definitions of macro-introduced identifiers are allowed.
;;; This may not be appropriate for implementations in which the
;;; model is that bindings are created by definitions, as opposed to
;;; one in which initial values are assigned by definitions.

;;; Top-level variable definitions of syntax keywords is not permitted.
;;; Any solution allowing this would be kludgey and would yield
;;; surprising results in some cases.  We can provide an undefine-syntax
;;; form.  The questions is, should define be an implicit undefine-syntax?
;;; We've decided no for now.

;;; Identifiers and syntax objects are implemented as vectors for
;;; portability.  As a result, it is possible to "forge" syntax
;;; objects.

;;; The implementation of generate-temporaries assumes that it is possible
;;; to generate globally unique symbols (gensyms).


;;; Bootstrapping:

;;; When changing syntax-object representations, it is necessary to support
;;; both old and new syntax-object representations in id-var-name.  It
;;; should be sufficient to recognize old representations and treat
;;; them as not lexically bound.



(eval-when (compile)
  (set-current-module (resolve-module '(guile))))

(let ()
;;; Private version of and-map that handles multiple lists.
(define and-map*
  (lambda (f first . rest)
    (or (null? first)
        (if (null? rest)
            (let andmap ((first first))
              (let ((x (car first)) (first (cdr first)))
                (if (null? first)
                    (f x)
                    (and (f x) (andmap first)))))
            (let andmap ((first first) (rest rest))
              (let ((x (car first))
                    (xr (map car rest))
                    (first (cdr first))
                    (rest (map cdr rest)))
                (if (null? first)
                    (apply f (cons x xr))
                    (and (apply f (cons x xr)) (andmap first rest)))))))))

(define-syntax define-structure
  (lambda (x)
    (define construct-name
      (lambda (template-identifier . args)
        (datum->syntax
          template-identifier
          (string->symbol
            (apply string-append
                   (map (lambda (x)
                          (if (string? x)
                              x
                              (symbol->string (syntax->datum x))))
                        args))))))
    (syntax-case x ()
      ((_ (name id1 ...))
       (and-map identifier? (syntax (name id1 ...)))
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

(let ()
(define noexpand "noexpand")
(define *mode* (make-fluid))

;;; hooks to nonportable run-time helpers
(begin
(define fx+ +)
(define fx- -)
(define fx= =)
(define fx< <)

(define top-level-eval-hook
  (lambda (x mod)
    (primitive-eval
     `(,noexpand
       ,(case (fluid-ref *mode*)
          ((c) ((@ (language tree-il) tree-il->scheme) x))
          (else x))))))

(define local-eval-hook
  (lambda (x mod)
    (primitive-eval
     `(,noexpand
       ,(case (fluid-ref *mode*)
          ((c) ((@ (language tree-il) tree-il->scheme) x))
          (else x))))))

(define-syntax gensym-hook
  (syntax-rules ()
    ((_) (gensym))))

(define put-global-definition-hook
  (lambda (symbol type val)
    (let ((existing (let ((v (module-variable (current-module) symbol)))
                      (and v (variable-bound? v)
                           (let ((val (variable-ref v)))
                             (and (macro? val)
                                  (not (syncase-macro-type val))
                                  val))))))
      (module-define! (current-module)
                      symbol
                      (if existing
                          (make-extended-syncase-macro existing type val)
                          (make-syncase-macro type val))))))

(define get-global-definition-hook
  (lambda (symbol module)
    (if (and (not module) (current-module))
        (warn "module system is booted, we should have a module" symbol))
    (let ((v (module-variable (if module
                                  (resolve-module (cdr module))
                                  (current-module))
                              symbol)))
      (and v (variable-bound? v)
           (let ((val (variable-ref v)))
             (and (macro? val) (syncase-macro-type val)
                  (cons (syncase-macro-type val)
                        (syncase-macro-binding val))))))))

)


(define (decorate-source e s)
  (if (and (pair? e) s)
      (set-source-properties! e s))
  e)

;;; output constructors
(define build-void
  (lambda (source)
    (case (fluid-ref *mode*)
      ((c) ((@ (language tree-il) make-void) source))
      (else (decorate-source '(if #f #f) source)))))

(define build-application
  (lambda (source fun-exp arg-exps)
    (case (fluid-ref *mode*)
      ((c) ((@ (language tree-il) make-application) source fun-exp arg-exps))
      (else (decorate-source `(,fun-exp . ,arg-exps) source)))))

(define build-conditional
  (lambda (source test-exp then-exp else-exp)
    (case (fluid-ref *mode*)
      ((c) ((@ (language tree-il) make-conditional)
            source test-exp then-exp else-exp))
      (else (decorate-source
             (if (equal? else-exp '(if #f #f))
                 `(if ,test-exp ,then-exp)
                 `(if ,test-exp ,then-exp ,else-exp))
             source)))))

(define build-lexical-reference
  (lambda (type source name var)
    (case (fluid-ref *mode*)
      ((c) ((@ (language tree-il) make-lexical-ref) source name var))
      (else (decorate-source var source)))))

(define build-lexical-assignment
  (lambda (source name var exp)
    (case (fluid-ref *mode*)
      ((c) ((@ (language tree-il) make-lexical-set) source name var exp))
      (else (decorate-source `(set! ,var ,exp) source)))))

;; Before modules are booted, we can't expand into data structures from
;; (language tree-il) -- we need to give the evaluator the
;; s-expressions that it understands natively. Actually the real truth
;; of the matter is that the evaluator doesn't understand tree-il
;; structures at all. So until we fix the evaluator, if ever, the
;; conflation that we should use tree-il iff we are compiling
;; holds true.
;;
(define (analyze-variable mod var modref-cont bare-cont)
  (if (not mod)
      (bare-cont var)
      (let ((kind (car mod))
            (mod (cdr mod)))
        (case kind
          ((public) (modref-cont mod var #t))
          ((private) (if (not (equal? mod (module-name (current-module))))
                         (modref-cont mod var #f)
                         (bare-cont var)))
          ((bare) (bare-cont var))
          ((hygiene) (if (and (not (equal? mod (module-name (current-module))))
                              (module-variable (resolve-module mod) var))
                         (modref-cont mod var #f)
                         (bare-cont var)))
          (else (syntax-violation #f "bad module kind" var mod))))))

(define build-global-reference
  (lambda (source var mod)
    (analyze-variable
     mod var
     (lambda (mod var public?) 
       (case (fluid-ref *mode*)
         ((c) ((@ (language tree-il) make-module-ref) source mod var public?))
         (else (decorate-source (list (if public? '@ '@@) mod var) source))))
     (lambda (var)
       (case (fluid-ref *mode*)
         ((c) ((@ (language tree-il) make-toplevel-ref) source var))
         (else (decorate-source var source)))))))

(define build-global-assignment
  (lambda (source var exp mod)
    (analyze-variable
     mod var
     (lambda (mod var public?) 
       (case (fluid-ref *mode*)
         ((c) ((@ (language tree-il) make-module-set) source mod var public? exp))
         (else (decorate-source `(set! ,(list (if public? '@ '@@) mod var) ,exp) source))))
     (lambda (var)
       (case (fluid-ref *mode*)
         ((c) ((@ (language tree-il) make-toplevel-set) source var exp))
         (else (decorate-source `(set! ,var ,exp) source)))))))

;; FIXME: there is a bug that prevents (set! ((@ (foo) bar) baz) quz)
;; from working. Hack around it.
(define (maybe-name-value! name val)
  (cond
   (((@ (language tree-il) lambda?) val)
    (let ((meta ((@ (language tree-il) lambda-meta) val)))
      (if (not (assq 'name meta))
          ((setter (@ (language tree-il) lambda-meta))
           val
           (acons 'name name meta)))))))

(define build-global-definition
  (lambda (source var exp)
    (case (fluid-ref *mode*)
      ((c)
       (maybe-name-value! var exp)
       ((@ (language tree-il) make-toplevel-define) source var exp))
      (else (decorate-source `(define ,var ,exp) source)))))

;; Ideally we would have all lambdas be case lambdas, but that would
;; need special support in the interpreter for the full capabilities of
;; case-lambda, with optional and keyword args, predicates, and else
;; clauses. This will come with the new interpreter, but for now we
;; separate the cases.
(define build-simple-lambda
  (lambda (src req rest vars docstring exp)
    (case (fluid-ref *mode*)
      ((c) ((@ (language tree-il) make-lambda) src
            (if docstring `((documentation . ,docstring)) '())
            ;; hah, a case in which kwargs would be nice.
            ((@ (language tree-il) make-lambda-case)
             ;; src req opt rest kw vars predicate body else
             src req #f rest #f vars #f exp #f)))
      (else (decorate-source
             `(lambda ,(if rest (apply cons* vars) vars)
                ,@(if docstring (list docstring) '())
                ,exp)
             src)))))

(define build-case-lambda
  (lambda (src docstring body)
    (case (fluid-ref *mode*)
      ((c) ((@ (language tree-il) make-lambda) src
            (if docstring `((documentation . ,docstring)) '())
            body))
      (else (decorate-source
             ;; really gross hack
             `(lambda %%args 
                ,@(if docstring (list docstring) '())
                (cond ,@body))
             src)))))

(define build-lambda-case
  ;; req := (name ...)
  ;; opt := ((name init) ...) | #f
  ;; rest := name | #f
  ;; kw: (allow-other-keys? (keyword name var [init]) ...) | #f
  ;; vars: (sym ...)
  ;; vars map to named arguments in the following order:
  ;;  required, optional (positional), rest, keyword.
  ;; predicate: something you can stuff in a (lambda ,vars ,pred), already expanded
  ;; the body of a lambda: anything, already expanded
  ;; else: lambda-case | #f
  (lambda (src req opt rest kw vars predicate body else-case)
    (case (fluid-ref *mode*)
      ((c)
       ((@ (language tree-il) make-lambda-case)
        src req opt rest kw vars predicate body else-case))
      (else
       ;; Very much like the logic of (language tree-il compile-glil).
       (let* ((nreq (length req))
              (nopt (if opt (length opt) 0))
              (rest-idx (and rest (+ nreq nopt)))
              (opt-inits (map (lambda (x) `(lambda ,vars ,(cdr x)))
                              (or opt '())))
              (allow-other-keys? (if kw (car kw) #f))
              (kw-indices (map (lambda (x)
                                 ;; (,key ,name ,var . _)
                                 (cons (car x) (list-index vars (caddr x))))
                               (if kw (cdr kw) '())))
              (kw-inits (sort
                         (filter
                          identity
                          (map (lambda (x)
                                 (if (pair? (cdddr x))
                                     ;; (,key ,name ,var ,init)
                                     (let ((i (list-index vars (caddr x))))
                                       (if (> (+ nreq nopt) i)
                                           (error "kw init for rest arg" x)
                                           (if (and rest (= (+ nreq nopt) i))
                                               (error "kw init for positional arg" x)
                                               `(lambda ,vars ,(cadddr x)))))
                                     ;; (,key ,name ,var)
                                     (let ((i (list-index vars (caddr x))))
                                       (if (< (+ nreq nopt) i)
                                           #f
                                           (error "missing init for kw arg" x)))))
                               (if kw (cdr kw) '())))
                         (lambda (x y) (< (cdr x) (cdr y)))))
              (nargs (apply max (pk (+ nreq nopt (if rest 1 0)))
                            (map cdr kw-indices))))
         (or (= nargs
                (length vars)
                (+ nreq (length opt-inits) (if rest 1 0) (length kw-inits)))
             (error "something went wrong"
                    req opt rest kw vars nreq nopt kw-indices kw-inits nargs))
         (decorate-source
          `((((@@ (ice-9 optargs) parse-lambda-case)
              '(,nreq ,nopt ,rest-idx ,nargs ,allow-other-keys? ,kw-indices)
              (list ,@opt-inits ,@kw-inits)
              ,(if predicate `(lambda ,vars ,predicate) #f)
              %%args)
             => (lambda ,vars ,body))
            ,@(or else-case
                  `((%%args (error "wrong number of arguments" %%args)))))
          src))))))

(define build-primref
  (lambda (src name)
    (if (equal? (module-name (current-module)) '(guile))
        (case (fluid-ref *mode*)
          ((c) ((@ (language tree-il) make-toplevel-ref) src name))
          (else (decorate-source name src)))
        (case (fluid-ref *mode*)
          ((c) ((@ (language tree-il) make-module-ref) src '(guile) name #f))
          (else (decorate-source `(@@ (guile) ,name) src))))))

(define (build-data src exp)
  (case (fluid-ref *mode*)
    ((c) ((@ (language tree-il) make-const) src exp))
    (else (decorate-source
           (if (and (self-evaluating? exp) (not (vector? exp)))
               exp
               (list 'quote exp))
           src))))

(define build-sequence
  (lambda (src exps)
    (if (null? (cdr exps))
        (car exps)
        (case (fluid-ref *mode*)
          ((c) ((@ (language tree-il) make-sequence) src exps))
          (else (decorate-source `(begin ,@exps) src))))))

(define build-let
  (lambda (src ids vars val-exps body-exp)
    (if (null? vars)
	body-exp
        (case (fluid-ref *mode*)
          ((c)
           (for-each maybe-name-value! ids val-exps)
           ((@ (language tree-il) make-let) src ids vars val-exps body-exp))
          (else (decorate-source
                 `(let ,(map list vars val-exps) ,body-exp)
                 src))))))

(define build-named-let
  (lambda (src ids vars val-exps body-exp)
    (let ((f (car vars))
          (f-name (car ids))
          (vars (cdr vars))
          (ids (cdr ids)))
      (case (fluid-ref *mode*)
        ((c)
         (let ((proc (build-simple-lambda src ids #f vars #f body-exp)))
           (maybe-name-value! f-name proc)
           (for-each maybe-name-value! ids val-exps)
           ((@ (language tree-il) make-letrec) src
            (list f-name) (list f) (list proc)
            (build-application src (build-lexical-reference 'fun src f-name f)
                               val-exps))))
        (else (decorate-source
               `(let ,f ,(map list vars val-exps) ,body-exp)
               src))))))

(define build-letrec
  (lambda (src ids vars val-exps body-exp)
    (if (null? vars)
        body-exp
        (case (fluid-ref *mode*)
          ((c)
           (for-each maybe-name-value! ids val-exps)
           ((@ (language tree-il) make-letrec) src ids vars val-exps body-exp))
          (else (decorate-source
                 `(letrec ,(map list vars val-exps) ,body-exp)
                 src))))))

;; FIXME: use a faster gensym
(define-syntax build-lexical-var
  (syntax-rules ()
    ((_ src id) (gensym (string-append (symbol->string id) " ")))))

(define-structure (syntax-object expression wrap module))

(define-syntax no-source (identifier-syntax #f))

(define source-annotation
  (lambda (x)
     (cond
      ((syntax-object? x)
       (source-annotation (syntax-object-expression x)))
      ((pair? x) (let ((props (source-properties x)))
                   (if (pair? props)
                       props
                       #f)))
      (else #f))))

(define-syntax arg-check
  (syntax-rules ()
    ((_ pred? e who)
     (let ((x e))
       (if (not (pred? x)) (syntax-violation who "invalid argument" x))))))

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
;;;               (core . <procedure>)            core forms
;;;               (module-ref . <procedure>)      @ or @@
;;;               (begin)                         begin
;;;               (define)                        define
;;;               (define-syntax)                 define-syntax
;;;               (local-syntax . rec?)           let-syntax/letrec-syntax
;;;               (eval-when)                     eval-when
;;;               (syntax . (<var> . <level>))    pattern variables
;;;               (global)                        assumed global variable
;;;               (lexical . <var>)               lexical variables
;;;               (displaced-lexical)             displaced lexicals
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

(define-syntax make-binding
  (syntax-rules (quote)
    ((_ type value) (cons type value))
    ((_ 'type) '(type))
    ((_ type) (cons type '()))))
(define binding-type car)
(define binding-value cdr)

(define-syntax null-env (identifier-syntax '()))

(define extend-env
  (lambda (labels bindings r) 
    (if (null? labels)
        r
        (extend-env (cdr labels) (cdr bindings)
          (cons (cons (car labels) (car bindings)) r)))))

(define extend-var-env
  ; variant of extend-env that forms "lexical" binding
  (lambda (labels vars r)
    (if (null? labels)
        r
        (extend-var-env (cdr labels) (cdr vars)
          (cons (cons (car labels) (make-binding 'lexical (car vars))) r)))))

;;; we use a "macros only" environment in expansion of local macro
;;; definitions so that their definitions can use local macros without
;;; attempting to use other lexical identifiers.
(define macros-only-env
  (lambda (r)
    (if (null? r)
        '()
        (let ((a (car r)))
          (if (eq? (cadr a) 'macro)
              (cons a (macros-only-env (cdr r)))
              (macros-only-env (cdr r)))))))

(define lookup
  ; x may be a label or a symbol
  ; although symbols are usually global, we check the environment first
  ; anyway because a temporary binding may have been established by
  ; fluid-let-syntax
  (lambda (x r mod)
    (cond
      ((assq x r) => cdr)
      ((symbol? x)
       (or (get-global-definition-hook x mod) (make-binding 'global)))
      (else (make-binding 'displaced-lexical)))))

(define global-extend
  (lambda (type sym val)
    (put-global-definition-hook sym type val)))


;;; Conceptually, identifiers are always syntax objects.  Internally,
;;; however, the wrap is sometimes maintained separately (a source of
;;; efficiency and confusion), so that symbols are also considered
;;; identifiers by id?.  Externally, they are always wrapped.

(define nonsymbol-id?
  (lambda (x)
    (and (syntax-object? x)
         (symbol? (syntax-object-expression x)))))

(define id?
  (lambda (x)
    (cond
      ((symbol? x) #t)
      ((syntax-object? x) (symbol? (syntax-object-expression x)))
      (else #f))))

(define-syntax id-sym-name
  (syntax-rules ()
    ((_ e)
     (let ((x e))
       (if (syntax-object? x)
           (syntax-object-expression x)
           x)))))

(define id-sym-name&marks
  (lambda (x w)
    (if (syntax-object? x)
        (values
         (syntax-object-expression x)
         (join-marks (wrap-marks w) (wrap-marks (syntax-object-wrap x))))
        (values x (wrap-marks w)))))

;;; syntax object wraps

;;;         <wrap> ::= ((<mark> ...) . (<subst> ...))
;;;        <subst> ::= <shift> | <subs>
;;;         <subs> ::= #(<old name> <label> (<mark> ...))
;;;        <shift> ::= positive fixnum

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

;;; labels must be comparable with "eq?" and distinct from symbols.
(define gen-label
  (lambda () (string #\i)))

(define gen-labels
  (lambda (ls)
    (if (null? ls)
        '()
        (cons (gen-label) (gen-labels (cdr ls))))))

(define-structure (ribcage symnames marks labels))

(define-syntax empty-wrap (identifier-syntax '(())))

(define-syntax top-wrap (identifier-syntax '((top))))

(define-syntax top-marked?
  (syntax-rules ()
    ((_ w) (memq 'top (wrap-marks w)))))

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

;;; make-empty-ribcage and extend-ribcage maintain list-based ribcages for
;;; internal definitions, in which the ribcages are built incrementally
(define-syntax make-empty-ribcage
  (syntax-rules ()
    ((_) (make-ribcage '() '() '()))))

(define extend-ribcage!
  ; must receive ids with complete wraps
  (lambda (ribcage id label)
    (set-ribcage-symnames! ribcage
      (cons (syntax-object-expression id)
            (ribcage-symnames ribcage)))
    (set-ribcage-marks! ribcage
      (cons (wrap-marks (syntax-object-wrap id))
            (ribcage-marks ribcage)))
    (set-ribcage-labels! ribcage
      (cons label (ribcage-labels ribcage)))))

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

(define id-var-name
  (lambda (id w)
    (define-syntax first
      (syntax-rules ()
        ((_ e) (call-with-values (lambda () e) (lambda (x . ignore) x)))))
    (define search
      (lambda (sym subst marks)
        (if (null? subst)
            (values #f marks)
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
      ((symbol? id)
       (or (first (search id (wrap-subst w) (wrap-marks w))) id))
      ((syntax-object? id)
        (let ((id (syntax-object-expression id))
              (w1 (syntax-object-wrap id)))
          (let ((marks (join-marks (wrap-marks w) (wrap-marks w1))))
            (call-with-values (lambda () (search id (wrap-subst w) marks))
              (lambda (new-id marks)
                (or new-id
                    (first (search id (wrap-subst w1) marks))
                    id))))))
      (else (syntax-violation 'id-var-name "invalid id" id)))))

;;; free-id=? must be passed fully wrapped ids since (free-id=? x y)
;;; may be true even if (free-id=? (wrap x w) (wrap y w)) is not.

(define free-id=?
  (lambda (i j)
    (and (eq? (id-sym-name i) (id-sym-name j)) ; accelerator
         (eq? (id-var-name i empty-wrap) (id-var-name j empty-wrap)))))

;;; bound-id=? may be passed unwrapped (or partially wrapped) ids as
;;; long as the missing portion of the wrap is common to both of the ids
;;; since (bound-id=? x y) iff (bound-id=? (wrap x w) (wrap y w))

(define bound-id=?
  (lambda (i j)
    (if (and (syntax-object? i) (syntax-object? j))
        (and (eq? (syntax-object-expression i)
                  (syntax-object-expression j))
             (same-marks? (wrap-marks (syntax-object-wrap i))
                  (wrap-marks (syntax-object-wrap j))))
        (eq? i j))))

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

(define bound-id-member?
   (lambda (x list)
      (and (not (null? list))
           (or (bound-id=? x (car list))
               (bound-id-member? x (cdr list))))))

;;; wrapping expressions and identifiers

(define wrap
  (lambda (x w defmod)
    (cond
      ((and (null? (wrap-marks w)) (null? (wrap-subst w))) x)
      ((syntax-object? x)
       (make-syntax-object
         (syntax-object-expression x)
         (join-wraps w (syntax-object-wrap x))
         (syntax-object-module x)))
      ((null? x) x)
      (else (make-syntax-object x w defmod)))))

(define source-wrap
  (lambda (x w s defmod)
    (wrap (decorate-source x s) w defmod)))

;;; expanding

(define chi-sequence
  (lambda (body r w s mod)
    (build-sequence s
      (let dobody ((body body) (r r) (w w) (mod mod))
        (if (null? body)
            '()
            (let ((first (chi (car body) r w mod)))
              (cons first (dobody (cdr body) r w mod))))))))

(define chi-top-sequence
  (lambda (body r w s m esew mod)
    (build-sequence s
      (let dobody ((body body) (r r) (w w) (m m) (esew esew) (mod mod))
        (if (null? body)
            '()
            (let ((first (chi-top (car body) r w m esew mod)))
              (cons first (dobody (cdr body) r w m esew mod))))))))

(define chi-install-global
  (lambda (name e)
    (build-global-definition
     no-source
     name
     ;; FIXME: seems nasty to call current-module here
     (if (let ((v (module-variable (current-module) name)))
           ;; FIXME use primitive-macro?
           (and v (variable-bound? v) (macro? (variable-ref v))
                (not (eq? (macro-type (variable-ref v)) 'syncase-macro))))
         (build-application
          no-source
          (build-primref no-source 'make-extended-syncase-macro)
          (list (build-application
                 no-source
                 (build-primref no-source 'module-ref)
                 (list (build-application 
                        no-source
                        (build-primref no-source 'current-module)
                        '())
                       (build-data no-source name)))
                (build-data no-source 'macro)
                e))
         (build-application
          no-source
          (build-primref no-source 'make-syncase-macro)
          (list (build-data no-source 'macro) e))))))

(define chi-when-list
  (lambda (e when-list w)
    ; when-list is syntax'd version of list of situations
    (let f ((when-list when-list) (situations '()))
      (if (null? when-list)
          situations
          (f (cdr when-list)
             (cons (let ((x (car when-list)))
                     (cond
                       ((free-id=? x (syntax compile)) 'compile)
                       ((free-id=? x (syntax load)) 'load)
                       ((free-id=? x (syntax eval)) 'eval)
                       (else (syntax-violation 'eval-when
                                               "invalid situation"
                                               e (wrap x w #f)))))
                   situations))))))

;;; syntax-type returns six values: type, value, e, w, s, and mod. The
;;; first two are described in the table below.
;;;
;;;    type                   value         explanation
;;;    -------------------------------------------------------------------
;;;    core                   procedure     core singleton
;;;    core-form              procedure     core form
;;;    module-ref             procedure     @ or @@ singleton
;;;    lexical                name          lexical variable reference
;;;    global                 name          global variable reference
;;;    begin                  none          begin keyword
;;;    define                 none          define keyword
;;;    define-syntax          none          define-syntax keyword
;;;    local-syntax           rec?          letrec-syntax/let-syntax keyword
;;;    eval-when              none          eval-when keyword
;;;    syntax                 level         pattern variable
;;;    displaced-lexical      none          displaced lexical identifier
;;;    lexical-call           name          call to lexical variable
;;;    global-call            name          call to global variable
;;;    call                   none          any other call
;;;    begin-form             none          begin expression
;;;    define-form            id            variable definition
;;;    define-syntax-form     id            syntax definition
;;;    local-syntax-form      rec?          syntax definition
;;;    eval-when-form         none          eval-when form
;;;    constant               none          self-evaluating datum
;;;    other                  none          anything else
;;;
;;; For define-form and define-syntax-form, e is the rhs expression.
;;; For all others, e is the entire form.  w is the wrap for e.
;;; s is the source for the entire form. mod is the module for e.
;;;
;;; syntax-type expands macros and unwraps as necessary to get to
;;; one of the forms above.  It also parses define and define-syntax
;;; forms, although perhaps this should be done by the consumer.

(define syntax-type
  (lambda (e r w s rib mod for-car?)
    (cond
      ((symbol? e)
       (let* ((n (id-var-name e w))
              (b (lookup n r mod))
              (type (binding-type b)))
         (case type
           ((lexical) (values type (binding-value b) e w s mod))
           ((global) (values type n e w s mod))
           ((macro)
            (if for-car?
                (values type (binding-value b) e w s mod)
                (syntax-type (chi-macro (binding-value b) e r w rib mod)
                             r empty-wrap s rib mod #f)))
           (else (values type (binding-value b) e w s mod)))))
      ((pair? e)
       (let ((first (car e)))
         (call-with-values
             (lambda () (syntax-type first r w s rib mod #t))
           (lambda (ftype fval fe fw fs fmod)
             (case ftype
               ((lexical)
                (values 'lexical-call fval e w s mod))
               ((global)
                ;; If we got here via an (@@ ...) expansion, we need to
                ;; make sure the fmod information is propagated back
                ;; correctly -- hence this consing.
                (values 'global-call (make-syntax-object fval w fmod)
                        e w s mod))
               ((macro)
                (syntax-type (chi-macro fval e r w rib mod)
                             r empty-wrap s rib mod for-car?))
               ((module-ref)
                (call-with-values (lambda () (fval e))
                  (lambda (sym mod)
                    (syntax-type sym r w s rib mod for-car?))))
               ((core)
                (values 'core-form fval e w s mod))
               ((local-syntax)
                (values 'local-syntax-form fval e w s mod))
               ((begin)
                (values 'begin-form #f e w s mod))
               ((eval-when)
                (values 'eval-when-form #f e w s mod))
               ((define)
                (syntax-case e ()
                  ((_ name val)
                   (id? (syntax name))
                   (values 'define-form (syntax name) (syntax val) w s mod))
                  ((_ (name . args) e1 e2 ...)
                   (and (id? (syntax name))
                        (valid-bound-ids? (lambda-var-list (syntax args))))
                                        ; need lambda here...
                   (values 'define-form (wrap (syntax name) w mod)
                           (decorate-source
                            (cons (syntax lambda) (wrap (syntax (args e1 e2 ...)) w mod))
                            s)
                           empty-wrap s mod))
                  ((_ name)
                   (id? (syntax name))
                   (values 'define-form (wrap (syntax name) w mod)
                           (syntax (if #f #f))
                           empty-wrap s mod))))
               ((define-syntax)
                (syntax-case e ()
                  ((_ name val)
                   (id? (syntax name))
                   (values 'define-syntax-form (syntax name)
                           (syntax val) w s mod))))
               (else
                (values 'call #f e w s mod)))))))
      ((syntax-object? e)
       (syntax-type (syntax-object-expression e)
                    r
                    (join-wraps w (syntax-object-wrap e))
                    s rib (or (syntax-object-module e) mod) for-car?))
      ((self-evaluating? e) (values 'constant #f e w s mod))
      (else (values 'other #f e w s mod)))))

(define chi-top
  (lambda (e r w m esew mod)
    (define-syntax eval-if-c&e
      (syntax-rules ()
        ((_ m e mod)
         (let ((x e))
           (if (eq? m 'c&e) (top-level-eval-hook x mod))
           x))))
    (call-with-values
      (lambda () (syntax-type e r w (source-annotation e) #f mod #f))
      (lambda (type value e w s mod)
        (case type
          ((begin-form)
           (syntax-case e ()
             ((_) (chi-void))
             ((_ e1 e2 ...)
              (chi-top-sequence (syntax (e1 e2 ...)) r w s m esew mod))))
          ((local-syntax-form)
           (chi-local-syntax value e r w s mod
             (lambda (body r w s mod)
               (chi-top-sequence body r w s m esew mod))))
          ((eval-when-form)
           (syntax-case e ()
             ((_ (x ...) e1 e2 ...)
              (let ((when-list (chi-when-list e (syntax (x ...)) w))
                    (body (syntax (e1 e2 ...))))
                (cond
                  ((eq? m 'e)
                   (if (memq 'eval when-list)
                       (chi-top-sequence body r w s 'e '(eval) mod)
                       (chi-void)))
                  ((memq 'load when-list)
                   (if (or (memq 'compile when-list)
                           (and (eq? m 'c&e) (memq 'eval when-list)))
                       (chi-top-sequence body r w s 'c&e '(compile load) mod)
                       (if (memq m '(c c&e))
                           (chi-top-sequence body r w s 'c '(load) mod)
                           (chi-void))))
                  ((or (memq 'compile when-list)
                       (and (eq? m 'c&e) (memq 'eval when-list)))
                   (top-level-eval-hook
                     (chi-top-sequence body r w s 'e '(eval) mod)
                     mod)
                   (chi-void))
                  (else (chi-void)))))))
          ((define-syntax-form)
           (let ((n (id-var-name value w)) (r (macros-only-env r)))
             (case m
               ((c)
                (if (memq 'compile esew)
                    (let ((e (chi-install-global n (chi e r w mod))))
                      (top-level-eval-hook e mod)
                      (if (memq 'load esew) e (chi-void)))
                    (if (memq 'load esew)
                        (chi-install-global n (chi e r w mod))
                        (chi-void))))
               ((c&e)
                (let ((e (chi-install-global n (chi e r w mod))))
                  (top-level-eval-hook e mod)
                  e))
               (else
                (if (memq 'eval esew)
                    (top-level-eval-hook
                      (chi-install-global n (chi e r w mod))
                      mod))
                (chi-void)))))
          ((define-form)
           (let* ((n (id-var-name value w))
		  (type (binding-type (lookup n r mod))))
             (case type
               ((global core macro module-ref)
                ;; affect compile-time environment (once we have booted)
                (if (and (not (module-local-variable (current-module) n))
                         (current-module))
                    (let ((old (module-variable (current-module) n)))
                      ;; use value of the same-named imported variable, if
                      ;; any
                      (module-define! (current-module) n
                                      (if (variable? old)
                                          (variable-ref old)
                                          #f))))
                (eval-if-c&e m
                  (build-global-definition s n (chi e r w mod))
                  mod))
               ((displaced-lexical)
                (syntax-violation #f "identifier out of context"
                                  e (wrap value w mod)))
               (else
                (syntax-violation #f "cannot define keyword at top level"
                                  e (wrap value w mod))))))
          (else (eval-if-c&e m (chi-expr type value e r w s mod) mod)))))))

(define chi
  (lambda (e r w mod)
    (call-with-values
      (lambda () (syntax-type e r w (source-annotation e) #f mod #f))
      (lambda (type value e w s mod)
        (chi-expr type value e r w s mod)))))

(define chi-expr
  (lambda (type value e r w s mod)
    (case type
      ((lexical)
       (build-lexical-reference 'value s e value))
      ((core core-form)
       ;; apply transformer
       (value e r w s mod))
      ((module-ref)
       (call-with-values (lambda () (value e))
         ;; we could add a public? arg here
         (lambda (id mod) (build-global-reference s id mod))))
      ((lexical-call)
       (chi-application
         (build-lexical-reference 'fun (source-annotation (car e))
                                  (car e) value)
         e r w s mod))
      ((global-call)
       (chi-application
         (build-global-reference (source-annotation (car e))
                                 (if (syntax-object? value)
                                     (syntax-object-expression value)
                                     value)
                                 (if (syntax-object? value)
                                     (syntax-object-module value)
                                     mod))
         e r w s mod))
      ((constant) (build-data s (strip (source-wrap e w s mod) empty-wrap)))
      ((global) (build-global-reference s value mod))
      ((call) (chi-application (chi (car e) r w mod) e r w s mod))
      ((begin-form)
       (syntax-case e ()
         ((_ e1 e2 ...) (chi-sequence (syntax (e1 e2 ...)) r w s mod))))
      ((local-syntax-form)
       (chi-local-syntax value e r w s mod chi-sequence))
      ((eval-when-form)
       (syntax-case e ()
         ((_ (x ...) e1 e2 ...)
          (let ((when-list (chi-when-list e (syntax (x ...)) w)))
            (if (memq 'eval when-list)
                (chi-sequence (syntax (e1 e2 ...)) r w s mod)
                (chi-void))))))
      ((define-form define-syntax-form)
       (syntax-violation #f "definition in expression context"
                         e (wrap value w mod)))
      ((syntax)
       (syntax-violation #f "reference to pattern variable outside syntax form"
                         (source-wrap e w s mod)))
      ((displaced-lexical)
       (syntax-violation #f "reference to identifier outside its scope"
                          (source-wrap e w s mod)))
      (else (syntax-violation #f "unexpected syntax"
                              (source-wrap e w s mod))))))

(define chi-application
  (lambda (x e r w s mod)
    (syntax-case e ()
      ((e0 e1 ...)
       (build-application s x
         (map (lambda (e) (chi e r w mod)) (syntax (e1 ...))))))))

(define chi-macro
  (lambda (p e r w rib mod)
    (define rebuild-macro-output
      (lambda (x m)
        (cond ((pair? x)
               (cons (rebuild-macro-output (car x) m)
                     (rebuild-macro-output (cdr x) m)))
              ((syntax-object? x)
               (let ((w (syntax-object-wrap x)))
                 (let ((ms (wrap-marks w)) (s (wrap-subst w)))
                   (if (and (pair? ms) (eq? (car ms) the-anti-mark))
                       ;; output is from original text
                       (make-syntax-object
                        (syntax-object-expression x)
                        (make-wrap (cdr ms) (if rib (cons rib (cdr s)) (cdr s)))
                        (syntax-object-module x))
                       ;; output introduced by macro
                       (make-syntax-object
                        (syntax-object-expression x)
                        (make-wrap (cons m ms)
                                   (if rib
                                       (cons rib (cons 'shift s))
                                       (cons 'shift s)))
                        (let ((pmod (procedure-module p)))
                          (if pmod
                              ;; hither the hygiene
                              (cons 'hygiene (module-name pmod))
                              ;; but it's possible for the proc to have
                              ;; no mod, if it was made before modules
                              ;; were booted
                              '(hygiene guile))))))))
              ((vector? x)
               (let* ((n (vector-length x)) (v (make-vector n)))
                 (do ((i 0 (fx+ i 1)))
                     ((fx= i n) v)
                     (vector-set! v i
                       (rebuild-macro-output (vector-ref x i) m)))))
              ((symbol? x)
               (syntax-violation #f "encountered raw symbol in macro output"
                                 (source-wrap e w (wrap-subst w) mod) x))
              (else x))))
    (rebuild-macro-output (p (wrap e (anti-mark w) mod)) (new-mark))))

(define chi-body
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
  (lambda (body outer-form r w mod)
    (let* ((r (cons '("placeholder" . (placeholder)) r))
           (ribcage (make-empty-ribcage))
           (w (make-wrap (wrap-marks w) (cons ribcage (wrap-subst w)))))
      (let parse ((body (map (lambda (x) (cons r (wrap x w mod))) body))
                  (ids '()) (labels '())
                  (var-ids '()) (vars '()) (vals '()) (bindings '()))
        (if (null? body)
            (syntax-violation #f "no expressions in body" outer-form)
            (let ((e (cdar body)) (er (caar body)))
              (call-with-values
                (lambda () (syntax-type e er empty-wrap (source-annotation er) ribcage mod #f))
                (lambda (type value e w s mod)
                  (case type
                    ((define-form)
                     (let ((id (wrap value w mod)) (label (gen-label)))
                       (let ((var (gen-var id)))
                         (extend-ribcage! ribcage id label)
                         (parse (cdr body)
                           (cons id ids) (cons label labels)
                           (cons id var-ids)
                           (cons var vars) (cons (cons er (wrap e w mod)) vals)
                           (cons (make-binding 'lexical var) bindings)))))
                    ((define-syntax-form)
                     (let ((id (wrap value w mod)) (label (gen-label)))
                       (extend-ribcage! ribcage id label)
                       (parse (cdr body)
                         (cons id ids) (cons label labels)
                         var-ids vars vals
                         (cons (make-binding 'macro (cons er (wrap e w mod)))
                               bindings))))
                    ((begin-form)
                     (syntax-case e ()
                       ((_ e1 ...)
                        (parse (let f ((forms (syntax (e1 ...))))
                                 (if (null? forms)
                                     (cdr body)
                                     (cons (cons er (wrap (car forms) w mod))
                                           (f (cdr forms)))))
                          ids labels var-ids vars vals bindings))))
                    ((local-syntax-form)
                     (chi-local-syntax value e er w s mod
                       (lambda (forms er w s mod)
                         (parse (let f ((forms forms))
                                  (if (null? forms)
                                      (cdr body)
                                      (cons (cons er (wrap (car forms) w mod))
                                            (f (cdr forms)))))
                           ids labels var-ids vars vals bindings))))
                    (else ; found a non-definition
                     (if (null? ids)
                         (build-sequence no-source
                           (map (lambda (x)
                                  (chi (cdr x) (car x) empty-wrap mod))
                                (cons (cons er (source-wrap e w s mod))
                                      (cdr body))))
                         (begin
                           (if (not (valid-bound-ids? ids))
                               (syntax-violation
                                #f "invalid or duplicate identifier in definition"
                                outer-form))
                           (let loop ((bs bindings) (er-cache #f) (r-cache #f))
                             (if (not (null? bs))
                                 (let* ((b (car bs)))
                                   (if (eq? (car b) 'macro)
                                       (let* ((er (cadr b))
                                              (r-cache
                                                (if (eq? er er-cache)
                                                    r-cache
                                                    (macros-only-env er))))
                                         (set-cdr! b
                                           (eval-local-transformer
                                             (chi (cddr b) r-cache empty-wrap mod)
                                             mod))
                                         (loop (cdr bs) er r-cache))
                                       (loop (cdr bs) er-cache r-cache)))))
                           (set-cdr! r (extend-env labels bindings (cdr r)))
                           (build-letrec no-source
                             (map syntax->datum var-ids)
                             vars
                             (map (lambda (x)
                                    (chi (cdr x) (car x) empty-wrap mod))
                                  vals)
                             (build-sequence no-source
                               (map (lambda (x)
                                      (chi (cdr x) (car x) empty-wrap mod))
                                    (cons (cons er (source-wrap e w s mod))
                                          (cdr body)))))))))))))))))

(define chi-local-syntax
  (lambda (rec? e r w s mod k)
    (syntax-case e ()
      ((_ ((id val) ...) e1 e2 ...)
       (let ((ids (syntax (id ...))))
         (if (not (valid-bound-ids? ids))
             (syntax-violation #f "duplicate bound keyword" e)
             (let ((labels (gen-labels ids)))
               (let ((new-w (make-binding-wrap ids labels w)))
                 (k (syntax (e1 e2 ...))
                    (extend-env
                      labels
                      (let ((w (if rec? new-w w))
                            (trans-r (macros-only-env r)))
                        (map (lambda (x)
                               (make-binding 'macro
                                 (eval-local-transformer
                                  (chi x trans-r w mod)
                                  mod)))
                             (syntax (val ...))))
                      r)
                    new-w
                    s
                    mod))))))
      (_ (syntax-violation #f "bad local syntax definition"
                           (source-wrap e w s mod))))))

(define eval-local-transformer
  (lambda (expanded mod)
    (let ((p (local-eval-hook expanded mod)))
      (if (procedure? p)
          p
          (syntax-violation #f "nonprocedure transformer" p)))))

(define chi-void
  (lambda ()
    (build-void no-source)))

(define ellipsis?
  (lambda (x)
    (and (nonsymbol-id? x)
         (free-id=? x (syntax (... ...))))))

;;; data

;;; strips syntax-objects down to top-wrap
;;;
;;; since only the head of a list is annotated by the reader, not each pair
;;; in the spine, we also check for pairs whose cars are annotated in case
;;; we've been passed the cdr of an annotated list

(define strip
  (lambda (x w)
    (if (top-marked? w)
        x
        (let f ((x x))
          (cond
           ((syntax-object? x)
            (strip (syntax-object-expression x) (syntax-object-wrap x)))
           ((pair? x)
            (let ((a (f (car x))) (d (f (cdr x))))
              (if (and (eq? a (car x)) (eq? d (cdr x)))
                  x
                  (cons a d))))
           ((vector? x)
            (let ((old (vector->list x)))
              (let ((new (map f old)))
                (if (and-map* eq? old new) x (list->vector new)))))
           (else x))))))

;;; lexical variables

(define gen-var
  (lambda (id)
    (let ((id (if (syntax-object? id) (syntax-object-expression id) id)))
      (build-lexical-var no-source id))))

;; appears to return a reversed list
(define lambda-var-list
  (lambda (vars)
    (let lvl ((vars vars) (ls '()) (w empty-wrap))
       (cond
         ((pair? vars) (lvl (cdr vars) (cons (wrap (car vars) w #f) ls) w))
         ((id? vars) (cons (wrap vars w #f) ls))
         ((null? vars) ls)
         ((syntax-object? vars)
          (lvl (syntax-object-expression vars)
               ls
               (join-wraps w (syntax-object-wrap vars))))
       ; include anything else to be caught by subsequent error
       ; checking
         (else (cons vars ls))))))

;;; core transformers

(global-extend 'local-syntax 'letrec-syntax #t)
(global-extend 'local-syntax 'let-syntax #f)

(global-extend 'core 'fluid-let-syntax
  (lambda (e r w s mod)
    (syntax-case e ()
      ((_ ((var val) ...) e1 e2 ...)
       (valid-bound-ids? (syntax (var ...)))
       (let ((names (map (lambda (x) (id-var-name x w)) (syntax (var ...)))))
         (for-each
           (lambda (id n)
             (case (binding-type (lookup n r mod))
               ((displaced-lexical)
                (syntax-violation 'fluid-let-syntax
                                  "identifier out of context"
                                  e
                                  (source-wrap id w s mod)))))
           (syntax (var ...))
           names)
         (chi-body
           (syntax (e1 e2 ...))
           (source-wrap e w s mod)
           (extend-env
             names
             (let ((trans-r (macros-only-env r)))
               (map (lambda (x)
                      (make-binding 'macro
                        (eval-local-transformer (chi x trans-r w mod)
                                                mod)))
                    (syntax (val ...))))
             r)
           w
           mod)))
      (_ (syntax-violation 'fluid-let-syntax "bad syntax"
                           (source-wrap e w s mod))))))

(global-extend 'core 'quote
   (lambda (e r w s mod)
      (syntax-case e ()
         ((_ e) (build-data s (strip (syntax e) w)))
         (_ (syntax-violation 'quote "bad syntax"
                              (source-wrap e w s mod))))))

(global-extend 'core 'syntax
  (let ()
    (define gen-syntax
      (lambda (src e r maps ellipsis? mod)
        (if (id? e)
            (let ((label (id-var-name e empty-wrap)))
              (let ((b (lookup label r mod)))
                (if (eq? (binding-type b) 'syntax)
                    (call-with-values
                      (lambda ()
                        (let ((var.lev (binding-value b)))
                          (gen-ref src (car var.lev) (cdr var.lev) maps)))
                      (lambda (var maps) (values `(ref ,var) maps)))
                    (if (ellipsis? e)
                        (syntax-violation 'syntax "misplaced ellipsis" src)
                        (values `(quote ,e) maps)))))
            (syntax-case e ()
              ((dots e)
               (ellipsis? (syntax dots))
               (gen-syntax src (syntax e) r maps (lambda (x) #f) mod))
              ((x dots . y)
               ; this could be about a dozen lines of code, except that we
               ; choose to handle (syntax (x ... ...)) forms
               (ellipsis? (syntax dots))
               (let f ((y (syntax y))
                       (k (lambda (maps)
                            (call-with-values
                              (lambda ()
                                (gen-syntax src (syntax x) r
                                  (cons '() maps) ellipsis? mod))
                              (lambda (x maps)
                                (if (null? (car maps))
                                    (syntax-violation 'syntax "extra ellipsis"
                                                      src)
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
                                 (syntax-violation 'syntax "extra ellipsis" src)
                                 (values (gen-mappend x (car maps))
                                         (cdr maps))))))))
                   (_ (call-with-values
                        (lambda () (gen-syntax src y r maps ellipsis? mod))
                        (lambda (y maps)
                          (call-with-values
                            (lambda () (k maps))
                            (lambda (x maps)
                              (values (gen-append x y) maps)))))))))
              ((x . y)
               (call-with-values
                 (lambda () (gen-syntax src (syntax x) r maps ellipsis? mod))
                 (lambda (x maps)
                   (call-with-values
                     (lambda () (gen-syntax src (syntax y) r maps ellipsis? mod))
                     (lambda (y maps) (values (gen-cons x y) maps))))))
              (#(e1 e2 ...)
               (call-with-values
                 (lambda ()
                   (gen-syntax src (syntax (e1 e2 ...)) r maps ellipsis? mod))
                 (lambda (e maps) (values (gen-vector e) maps))))
              (_ (values `(quote ,e) maps))))))

    (define gen-ref
      (lambda (src var level maps)
        (if (fx= level 0)
            (values var maps)
            (if (null? maps)
                (syntax-violation 'syntax "missing ellipsis" src)
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
            ((and-map
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
          ((ref) (build-lexical-reference 'value no-source (cadr x) (cadr x)))
          ((primitive) (build-primref no-source (cadr x)))
          ((quote) (build-data no-source (cadr x)))
          ((lambda)
           (if (list? (cadr x))
               (build-simple-lambda no-source (cadr x) #f (cadr x) #f (regen (caddr x)))
               (error "how did we get here" x)))
          (else (build-application no-source
                  (build-primref no-source (car x))
                  (map regen (cdr x)))))))

    (lambda (e r w s mod)
      (let ((e (source-wrap e w s mod)))
        (syntax-case e ()
          ((_ x)
           (call-with-values
             (lambda () (gen-syntax e (syntax x) r '() ellipsis? mod))
             (lambda (e maps) (regen e))))
          (_ (syntax-violation 'syntax "bad `syntax' form" e)))))))


(global-extend 'core 'lambda
   (lambda (e r w s mod)
     (define (docstring&body ids vars labels c)
       (syntax-case c ()
         ((docstring e1 e2 ...)
          (string? (syntax->datum (syntax docstring)))
          (values (syntax->datum (syntax docstring))
                  (chi-body (syntax (e1 e2 ...))
                            (source-wrap e w s mod)
                            (extend-var-env labels vars r)
                            (make-binding-wrap ids labels w)
                            mod)))
         ((e1 e2 ...)
          (values #f
                  (chi-body (syntax (e1 e2 ...))
                            (source-wrap e w s mod)
                            (extend-var-env labels vars r)
                            (make-binding-wrap ids labels w)
                            mod)))))
     (syntax-case e ()
       ((_ (id ...) e1 e2 ...)
        (let ((ids (syntax (id ...))))
          (if (not (valid-bound-ids? ids))
              (syntax-violation 'lambda "invalid parameter list" e)
              (let ((vars (map gen-var ids))
                    (labels (gen-labels ids)))
                (call-with-values
                    (lambda ()
                      (docstring&body ids vars labels
                                      (syntax (e1 e2 ...))))
                  (lambda (docstring body)
                    (build-simple-lambda s (map syntax->datum ids) #f
                                         vars docstring body)))))))
       ((_ ids e1 e2 ...)
        (let ((rids (lambda-var-list (syntax ids))))
          (if (not (valid-bound-ids? rids))
              (syntax-violation 'lambda "invalid parameter list" e)
              (let* ((req (reverse (cdr rids)))
                     (rest (car rids))
                     (rrids (reverse rids))
                     (vars (map gen-var rrids))
                     (labels (gen-labels rrids)))
                (call-with-values
                    (lambda ()
                      (docstring&body rrids vars labels
                                      (syntax (e1 e2 ...))))
                  (lambda (docstring body)
                    (build-simple-lambda s (map syntax->datum req)
                                         (syntax->datum rest)
                                         vars docstring body)))))))
       (_ (syntax-violation 'lambda "bad lambda" e)))))


(global-extend 'core 'let
  (let ()
    (define (chi-let e r w s mod constructor ids vals exps)
      (if (not (valid-bound-ids? ids))
	  (syntax-violation 'let "duplicate bound variable" e)
	  (let ((labels (gen-labels ids))
		(new-vars (map gen-var ids)))
	    (let ((nw (make-binding-wrap ids labels w))
		  (nr (extend-var-env labels new-vars r)))
	      (constructor s
                           (map syntax->datum ids)
			   new-vars
			   (map (lambda (x) (chi x r w mod)) vals)
			   (chi-body exps (source-wrap e nw s mod)
                                     nr nw mod))))))
    (lambda (e r w s mod)
      (syntax-case e ()
	((_ ((id val) ...) e1 e2 ...)
         (and-map id? (syntax (id ...)))
	 (chi-let e r w s mod
		  build-let
		  (syntax (id ...))
		  (syntax (val ...))
		  (syntax (e1 e2 ...))))
	((_ f ((id val) ...) e1 e2 ...)
	 (and (id? (syntax f)) (and-map id? (syntax (id ...))))
	 (chi-let e r w s mod
		  build-named-let
		  (syntax (f id ...))
		  (syntax (val ...))
		  (syntax (e1 e2 ...))))
	(_ (syntax-violation 'let "bad let" (source-wrap e w s mod)))))))


(global-extend 'core 'letrec
  (lambda (e r w s mod)
    (syntax-case e ()
      ((_ ((id val) ...) e1 e2 ...)
       (and-map id? (syntax (id ...)))
       (let ((ids (syntax (id ...))))
         (if (not (valid-bound-ids? ids))
             (syntax-violation 'letrec "duplicate bound variable" e)
             (let ((labels (gen-labels ids))
                   (new-vars (map gen-var ids)))
               (let ((w (make-binding-wrap ids labels w))
                    (r (extend-var-env labels new-vars r)))
                 (build-letrec s
                   (map syntax->datum ids)
                   new-vars
                   (map (lambda (x) (chi x r w mod)) (syntax (val ...)))
                   (chi-body (syntax (e1 e2 ...)) 
                             (source-wrap e w s mod) r w mod)))))))
      (_ (syntax-violation 'letrec "bad letrec" (source-wrap e w s mod))))))


(global-extend 'core 'set!
  (lambda (e r w s mod)
    (syntax-case e ()
      ((_ id val)
       (id? (syntax id))
       (let ((val (chi (syntax val) r w mod))
             (n (id-var-name (syntax id) w)))
         (let ((b (lookup n r mod)))
           (case (binding-type b)
             ((lexical)
              (build-lexical-assignment s
                                        (syntax->datum (syntax id))
                                        (binding-value b)
                                        val))
             ((global) (build-global-assignment s n val mod))
             ((displaced-lexical)
              (syntax-violation 'set! "identifier out of context"
                                (wrap (syntax id) w mod)))
             (else (syntax-violation 'set! "bad set!"
                                     (source-wrap e w s mod)))))))
      ((_ (head tail ...) val)
       (call-with-values
           (lambda () (syntax-type (syntax head) r empty-wrap no-source #f mod #t))
         (lambda (type value ee ww ss modmod)
           (case type
             ((module-ref)
              (let ((val (chi (syntax val) r w mod)))
                (call-with-values (lambda () (value (syntax (head tail ...))))
                  (lambda (id mod)
                    (build-global-assignment s id val mod)))))
             (else
              (build-application s
                                 (chi (syntax (setter head)) r w mod)
                                 (map (lambda (e) (chi e r w mod))
                                      (syntax (tail ... val)))))))))
      (_ (syntax-violation 'set! "bad set!" (source-wrap e w s mod))))))

(global-extend 'module-ref '@
   (lambda (e)
     (syntax-case e ()
        ((_ (mod ...) id)
         (and (and-map id? (syntax (mod ...))) (id? (syntax id)))
         (values (syntax->datum (syntax id))
                 (syntax->datum
                  (syntax (public mod ...))))))))

(global-extend 'module-ref '@@
   (lambda (e)
     (syntax-case e ()
        ((_ (mod ...) id)
         (and (and-map id? (syntax (mod ...))) (id? (syntax id)))
         (values (syntax->datum (syntax id))
                 (syntax->datum
                  (syntax (private mod ...))))))))

(global-extend 'core 'if
  (lambda (e r w s mod)
    (syntax-case e ()
      ((_ test then)
       (build-conditional
        s
        (chi (syntax test) r w mod)
        (chi (syntax then) r w mod)
        (build-void no-source)))
      ((_ test then else)
       (build-conditional
        s
        (chi (syntax test) r w mod)
        (chi (syntax then) r w mod)
        (chi (syntax else) r w mod))))))

(global-extend 'begin 'begin '())

(global-extend 'define 'define '())

(global-extend 'define-syntax 'define-syntax '())

(global-extend 'eval-when 'eval-when '())

(global-extend 'core 'syntax-case
  (let ()
    (define convert-pattern
      ; accepts pattern & keys
      ; returns $sc-dispatch pattern & ids
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
      (lambda (pvars exp y r mod)
        (let ((ids (map car pvars)) (levels (map cdr pvars)))
          (let ((labels (gen-labels ids)) (new-vars (map gen-var ids)))
            (build-application no-source
              (build-primref no-source 'apply)
              (list (build-simple-lambda no-source (map syntax->datum ids) #f new-vars #f
                      (chi exp
                           (extend-env
                            labels
                            (map (lambda (var level)
                                   (make-binding 'syntax `(,var . ,level)))
                                 new-vars
                                 (map cdr pvars))
                            r)
                           (make-binding-wrap ids labels empty-wrap)
                           mod))
                    y))))))

    (define gen-clause
      (lambda (x keys clauses r pat fender exp mod)
        (call-with-values
          (lambda () (convert-pattern pat keys))
          (lambda (p pvars)
            (cond
              ((not (distinct-bound-ids? (map car pvars)))
               (syntax-violation 'syntax-case "duplicate pattern variable" pat))
              ((not (and-map (lambda (x) (not (ellipsis? (car x)))) pvars))
               (syntax-violation 'syntax-case "misplaced ellipsis" pat))
              (else
               (let ((y (gen-var 'tmp)))
                 ; fat finger binding and references to temp variable y
                 (build-application no-source
                   (build-simple-lambda no-source (list 'tmp) #f (list y) #f
                     (let ((y (build-lexical-reference 'value no-source
                                                       'tmp y)))
                       (build-conditional no-source
                         (syntax-case fender ()
                           (#t y)
                           (_ (build-conditional no-source
                                y
                                (build-dispatch-call pvars fender y r mod)
                                (build-data no-source #f))))
                         (build-dispatch-call pvars exp y r mod)
                         (gen-syntax-case x keys clauses r mod))))
                   (list (if (eq? p 'any)
                             (build-application no-source
                               (build-primref no-source 'list)
                               (list x))
                             (build-application no-source
                               (build-primref no-source '$sc-dispatch)
                               (list x (build-data no-source p)))))))))))))

    (define gen-syntax-case
      (lambda (x keys clauses r mod)
        (if (null? clauses)
            (build-application no-source
              (build-primref no-source 'syntax-violation)
              (list (build-data no-source #f)
                    (build-data no-source
                                "source expression failed to match any pattern")
                    x))
            (syntax-case (car clauses) ()
              ((pat exp)
               (if (and (id? (syntax pat))
                        (and-map (lambda (x) (not (free-id=? (syntax pat) x)))
                                 (cons (syntax (... ...)) keys)))
                   (let ((labels (list (gen-label)))
                         (var (gen-var (syntax pat))))
                     (build-application no-source
                       (build-simple-lambda
                        no-source (list (syntax->datum (syntax pat))) #f (list var)
                        #f
                        (chi (syntax exp)
                             (extend-env labels
                                         (list (make-binding 'syntax `(,var . 0)))
                                         r)
                             (make-binding-wrap (syntax (pat))
                                                labels empty-wrap)
                             mod))
                       (list x)))
                   (gen-clause x keys (cdr clauses) r
                     (syntax pat) #t (syntax exp) mod)))
              ((pat fender exp)
               (gen-clause x keys (cdr clauses) r
                 (syntax pat) (syntax fender) (syntax exp) mod))
              (_ (syntax-violation 'syntax-case "invalid clause"
                                   (car clauses)))))))

    (lambda (e r w s mod)
      (let ((e (source-wrap e w s mod)))
        (syntax-case e ()
          ((_ val (key ...) m ...)
           (if (and-map (lambda (x) (and (id? x) (not (ellipsis? x))))
                        (syntax (key ...)))
               (let ((x (gen-var 'tmp)))
                 ; fat finger binding and references to temp variable x
                 (build-application s
                   (build-simple-lambda no-source (list 'tmp) #f (list x) #f
                     (gen-syntax-case (build-lexical-reference 'value no-source
                                                               'tmp x)
                       (syntax (key ...)) (syntax (m ...))
                       r
                       mod))
                   (list (chi (syntax val) r empty-wrap mod))))
               (syntax-violation 'syntax-case "invalid literals list" e))))))))

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
      (lambda (x . rest)
        (if (and (pair? x) (equal? (car x) noexpand))
            (cadr x)
            (let ((m (if (null? rest) 'e (car rest)))
                  (esew (if (or (null? rest) (null? (cdr rest)))
                            '(eval)
                            (cadr rest))))
              (with-fluid* *mode* m
                (lambda ()
                  (chi-top x null-env top-wrap m esew
                           (cons 'hygiene (module-name (current-module))))))))))

(set! identifier?
  (lambda (x)
    (nonsymbol-id? x)))

(set! datum->syntax
  (lambda (id datum)
    (make-syntax-object datum (syntax-object-wrap id) #f)))

(set! syntax->datum
  ; accepts any object, since syntax objects may consist partially
  ; or entirely of unwrapped, nonsymbolic data
  (lambda (x)
    (strip x empty-wrap)))

(set! generate-temporaries
  (lambda (ls)
    (arg-check list? ls 'generate-temporaries)
    (map (lambda (x) (wrap (gensym-hook) top-wrap #f)) ls)))

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

(set! syntax-violation
  (lambda (who message form . subform)
    (arg-check (lambda (x) (or (not x) (string? x) (symbol? x)))
               who 'syntax-violation)
    (arg-check string? message 'syntax-violation)
    (scm-error 'syntax-error 'sc-expand
               (string-append
                (if who "~a: " "")
                "~a "
                (if (null? subform) "in ~a" "in subform `~s' of `~s'"))
               (let ((tail (cons message
                                 (map (lambda (x) (strip x empty-wrap))
                                      (append subform (list form))))))
                 (if who (cons who tail) tail))
               #f)))

;;; $sc-dispatch expects an expression and a pattern.  If the expression
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
  (lambda (e p w mod)
    (cond
     ((pair? e)
      (let ((first (match (car e) p w '() mod)))
        (and first
             (let ((rest (match-each (cdr e) p w mod)))
               (and rest (cons first rest))))))
     ((null? e) '())
     ((syntax-object? e)
      (match-each (syntax-object-expression e)
                  p
                  (join-wraps w (syntax-object-wrap e))
                  (syntax-object-module e)))
     (else #f))))

(define match-each-any
  (lambda (e w mod)
    (cond
     ((pair? e)
      (let ((l (match-each-any (cdr e) w mod)))
        (and l (cons (wrap (car e) w mod) l))))
     ((null? e) '())
     ((syntax-object? e)
      (match-each-any (syntax-object-expression e)
                      (join-wraps w (syntax-object-wrap e))
                      mod))
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
  (lambda (e p w r mod)
    (cond
      ((null? p) (and (null? e) r))
      ((pair? p)
       (and (pair? e) (match (car e) (car p) w
                        (match (cdr e) (cdr p) w r mod)
                        mod)))
      ((eq? p 'each-any)
       (let ((l (match-each-any e w mod))) (and l (cons l r))))
      (else
       (case (vector-ref p 0)
         ((each)
          (if (null? e)
              (match-empty (vector-ref p 1) r)
              (let ((l (match-each e (vector-ref p 1) w mod)))
                (and l
                     (let collect ((l l))
                       (if (null? (car l))
                           r
                           (cons (map car l) (collect (map cdr l)))))))))
         ((free-id) (and (id? e) (free-id=? (wrap e w mod) (vector-ref p 1)) r))
         ((atom) (and (equal? (vector-ref p 1) (strip e w)) r))
         ((vector)
          (and (vector? e)
               (match (vector->list e) (vector-ref p 1) w r mod))))))))

(define match
  (lambda (e p w r mod)
    (cond
      ((not r) #f)
      ((eq? p 'any) (cons (wrap e w mod) r))
      ((syntax-object? e)
       (match*
        (syntax-object-expression e)
        p
        (join-wraps w (syntax-object-wrap e))
        r
        (syntax-object-module e)))
      (else (match* e p w r mod)))))

(set! $sc-dispatch
  (lambda (e p)
    (cond
      ((eq? p 'any) (list e))
      ((syntax-object? e)
       (match* (syntax-object-expression e)
               p (syntax-object-wrap e) '() (syntax-object-module e)))
      (else (match* e p empty-wrap '() #f)))))

))
)

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

(define-syntax let*
  (lambda (x)
    (syntax-case x ()
      ((let* ((x v) ...) e1 e2 ...)
       (and-map identifier? (syntax (x ...)))
       (let f ((bindings (syntax ((x v)  ...))))
         (if (null? bindings)
             (syntax (let () e1 e2 ...))
             (with-syntax ((body (f (cdr bindings)))
                           (binding (car bindings)))
               (syntax (let (binding) body)))))))))

(define-syntax do
   (lambda (orig-x)
      (syntax-case orig-x ()
         ((_ ((var init . step) ...) (e0 e1 ...) c ...)
          (with-syntax (((step ...)
                         (map (lambda (v s)
                                 (syntax-case s ()
                                    (() v)
                                    ((e) (syntax e))
                                    (_ (syntax-violation
                                        'do "bad step expression" 
                                        orig-x s))))
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
      ((quasicons
        (lambda (x y)
          (with-syntax ((x x) (y y))
            (syntax-case (syntax y) (quote list)
              ((quote dy)
               (syntax-case (syntax x) (quote)
                 ((quote dx) (syntax (quote (dx . dy))))
                 (_ (if (null? (syntax dy))
                        (syntax (list x))
                        (syntax (cons x y))))))
              ((list . stuff) (syntax (list x . stuff)))
              (else (syntax (cons x y)))))))
       (quasiappend
        (lambda (x y)
          (with-syntax ((x x) (y y))
            (syntax-case (syntax y) (quote)
              ((quote ()) (syntax x))
              (_ (syntax (append x y)))))))
       (quasivector
        (lambda (x)
          (with-syntax ((x x))
            (syntax-case (syntax x) (quote list)
              ((quote (x ...)) (syntax (quote #(x ...))))
              ((list x ...) (syntax (vector x ...)))
              (_ (syntax (list->vector x)))))))
       (quasi
        (lambda (p lev)
           (syntax-case p (unquote unquote-splicing quasiquote)
              ((unquote p)
               (if (= lev 0)
                   (syntax p)
                   (quasicons (syntax (quote unquote))
                              (quasi (syntax (p)) (- lev 1)))))
              ((unquote . args)
               (= lev 0)
               (syntax-violation 'unquote
                                 "unquote takes exactly one argument"
                                 p (syntax (unquote . args))))
              (((unquote-splicing p) . q)
               (if (= lev 0)
                   (quasiappend (syntax p) (quasi (syntax q) lev))
                   (quasicons (quasicons (syntax (quote unquote-splicing))
                                         (quasi (syntax (p)) (- lev 1)))
                              (quasi (syntax q) lev))))
              (((unquote-splicing . args) . q)
               (= lev 0)
               (syntax-violation 'unquote-splicing
                                 "unquote-splicing takes exactly one argument"
                                 p (syntax (unquote-splicing . args))))
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
          (let f ((x (read p)))
            (if (eof-object? x)
                (begin (close-input-port p) '())
                (cons (datum->syntax k x)
                      (f (read p))))))))
    (syntax-case x ()
      ((k filename)
       (let ((fn (syntax->datum (syntax filename))))
         (with-syntax (((exp ...) (read-file fn (syntax k))))
           (syntax (begin exp ...))))))))

(define-syntax unquote
  (lambda (x)
    (syntax-case x ()
      ((_ e)
       (syntax-violation 'unquote
                         "expression not valid outside of quasiquote"
                         x)))))

(define-syntax unquote-splicing
  (lambda (x)
    (syntax-case x ()
      ((_ e)
       (syntax-violation 'unquote-splicing
                         "expression not valid outside of quasiquote"
                         x)))))

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
                        (_ (syntax-violation 'case "bad clause" x clause)))
                      (with-syntax ((rest (f (car clauses) (cdr clauses))))
                        (syntax-case clause (else)
                          (((k ...) e1 e2 ...)
                           (syntax (if (memv t '(k ...))
                                       (begin e1 e2 ...)
                                       rest)))
                          (_ (syntax-violation 'case "bad clause" x
                                               clause))))))))
         (syntax (let ((t e)) body)))))))

(define-syntax identifier-syntax
  (lambda (x)
    (syntax-case x ()
      ((_ e)
       (syntax
         (lambda (x)
           (syntax-case x ()
             (id
              (identifier? (syntax id))
              (syntax e))
             ((_ x (... ...))
              (syntax (e x (... ...)))))))))))
