;;; Guile Emacs Lisp

;; Copyright (C) 2009, 2010 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(define-module (language elisp compile-tree-il)
  #:use-module (language elisp bindings)
  #:use-module (language tree-il)
  #:use-module (system base pmatch)
  #:use-module (system base compile)
  #:use-module (srfi srfi-1)
  #:export (compile-tree-il))

; Certain common parameters (like the bindings data structure or compiler
; options) are not always passed around but accessed using fluids to simulate
; dynamic binding (hey, this is about elisp).

; The bindings data structure to keep track of symbol binding related data.

(define bindings-data (make-fluid))

; Store for which symbols (or all/none) void checks are disabled.

(define disable-void-check (make-fluid))

; Store which symbols (or all/none) should always be bound lexically, even
; with ordinary let and as lambda arguments.

(define always-lexical (make-fluid))

; Find the source properties of some parsed expression if there are any
; associated with it.

(define (location x)
  (and (pair? x)
       (let ((props (source-properties x)))
         (and (not (null? props))
              props))))

; Values to use for Elisp's nil and t.

(define (nil-value loc) (make-const loc (@ (language elisp runtime) nil-value)))

(define (t-value loc) (make-const loc (@ (language elisp runtime) t-value)))

; Modules that contain the value and function slot bindings.

(define runtime '(language elisp runtime))

(define macro-slot '(language elisp runtime macro-slot))

(define value-slot (@ (language elisp runtime) value-slot-module))

(define function-slot (@ (language elisp runtime) function-slot-module))

; The backquoting works the same as quasiquotes in Scheme, but the forms are
; named differently; to make easy adaptions, we define these predicates checking
; for a symbol being the car of an unquote/unquote-splicing/backquote form.

(define (backquote? sym)
  (and (symbol? sym) (eq? sym '\`)))

(define (unquote? sym)
  (and (symbol? sym) (eq? sym '\,)))

(define (unquote-splicing? sym)
  (and (symbol? sym) (eq? sym '\,@)))

; Build a call to a primitive procedure nicely.

(define (call-primitive loc sym . args)
  (make-application loc (make-primitive-ref loc sym) args))

; Error reporting routine for syntax/compilation problems or build code for
; a runtime-error output.

(define (report-error loc . args)
  (apply error args))

(define (runtime-error loc msg . args)
  (make-application loc (make-primitive-ref loc 'error)
    (cons (make-const loc msg) args)))

; Generate code to ensure a global symbol is there for further use of a given
; symbol.  In general during the compilation, those needed are only tracked with
; the bindings data structure.  Afterwards, however, for all those needed
; symbols the globals are really generated with this routine.

(define (generate-ensure-global loc sym module)
  (make-application loc (make-module-ref loc runtime 'ensure-fluid! #t)
    (list (make-const loc module)
          (make-const loc sym))))

; See if we should do a void-check for a given variable.  That means, check
; that this check is not disabled via the compiler options for this symbol.
; Disabling of void check is only done for the value-slot module!

(define (want-void-check? sym module)
  (let ((disabled (fluid-ref disable-void-check)))
    (or (not (equal? module value-slot))
        (and (not (eq? disabled 'all))
             (not (memq sym disabled))))))

; Build a construct that establishes dynamic bindings for certain variables.
; We may want to choose between binding with fluids and with-fluids* and
; using just ordinary module symbols and setting/reverting their values with
; a dynamic-wind.

(define (let-dynamic loc syms module vals body)
  (call-primitive loc 'with-fluids*
    (make-application loc (make-primitive-ref loc 'list)
      (map (lambda (sym)
             (make-module-ref loc module sym #t))
           syms))
    (make-application loc (make-primitive-ref loc 'list) vals)
    (make-lambda loc '()
                 (make-lambda-case #f '() #f #f #f '() '() body #f))))

; Handle access to a variable (reference/setting) correctly depending on
; whether it is currently lexically or dynamically bound.
; lexical access is done only for references to the value-slot module!

(define (access-variable loc sym module handle-lexical handle-dynamic)
  (let ((lexical (get-lexical-binding (fluid-ref bindings-data) sym)))
    (if (and lexical (equal? module value-slot))
      (handle-lexical lexical)
      (handle-dynamic))))

; Generate code to reference a variable.
; For references in the value-slot module, we may want to generate a lexical
; reference instead if the variable has a lexical binding.

(define (reference-variable loc sym module)
  (access-variable loc sym module
                   (lambda (lexical)
                     (make-lexical-ref loc lexical lexical))
                   (lambda ()
                     (mark-global-needed! (fluid-ref bindings-data) sym module)
                     (call-primitive loc 'fluid-ref
                                     (make-module-ref loc module sym #t)))))

; Reference a variable and error if the value is void.

(define (reference-with-check loc sym module)
  (if (want-void-check? sym module)
    (let ((var (gensym)))
      (make-let loc '(value) `(,var) `(,(reference-variable loc sym module))
        (make-conditional loc
          (call-primitive loc 'eq?
                          (make-module-ref loc runtime 'void #t)
                          (make-lexical-ref loc 'value var))
          (runtime-error loc "variable is void:" (make-const loc sym))
          (make-lexical-ref loc 'value var))))
    (reference-variable loc sym module)))

; Generate code to set a variable.
; Just as with reference-variable, in case of a reference to value-slot,
; we want to generate a lexical set when the variable has a lexical binding.

(define (set-variable! loc sym module value)
  (access-variable loc sym module
                   (lambda (lexical)
                     (make-lexical-set loc lexical lexical value))
                   (lambda ()
                     (mark-global-needed! (fluid-ref bindings-data) sym module)
                     (call-primitive loc 'fluid-set!
                                     (make-module-ref loc module sym #t)
                                     value))))

; Process the bindings part of a let or let* expression; that is, check for
; correctness and bring it to the form ((sym1 . val1) (sym2 . val2) ...).

(define (process-let-bindings loc bindings)
  (map (lambda (b)
         (if (symbol? b)
           (cons b 'nil)
           (if (or (not (list? b))
                   (not (= (length b) 2)))
             (report-error loc "expected symbol or list of 2 elements in let")
             (if (not (symbol? (car b)))
               (report-error loc "expected symbol in let")
               (cons (car b) (cadr b))))))
       bindings))

; Split the let bindings into a list to be done lexically and one dynamically.
; A symbol will be bound lexically if and only if:
; We're processing a lexical-let (i.e. module is 'lexical), OR
; we're processing a value-slot binding AND
;   the symbol is already lexically bound or it is always lexical.

(define (bind-lexically? sym module)
  (or (eq? module 'lexical)
      (and (equal? module value-slot)
           (let ((always (fluid-ref always-lexical)))
             (or (eq? always 'all)
                 (memq sym always)
                 (get-lexical-binding (fluid-ref bindings-data) sym))))))

(define (split-let-bindings bindings module)
  (let iterate ((tail bindings)
                (lexical '())
                (dynamic '()))
    (if (null? tail)
      (values (reverse lexical) (reverse dynamic))
      (if (bind-lexically? (caar tail) module)
        (iterate (cdr tail) (cons (car tail) lexical) dynamic)
        (iterate (cdr tail) lexical (cons (car tail) dynamic))))))

; Compile let and let* expressions.  The code here is used both for let/let*
; and flet/flet*, just with a different bindings module.
;
; A special module value 'lexical means that we're doing a lexical-let instead
; and the bindings should not be saved to globals at all but be done with the
; lexical framework instead.

; Let is done with a single call to let-dynamic binding them locally to new
; values all "at once".  If there is at least one variable to bind lexically
; among the bindings, we first do a let for all of them to evaluate all
; values before any bindings take place, and then call let-dynamic for the
; variables to bind dynamically.

(define (generate-let loc module bindings body)
  (let ((bind (process-let-bindings loc bindings)))
    (call-with-values
      (lambda ()
        (split-let-bindings bind module))
      (lambda (lexical dynamic)
        (for-each (lambda (sym)
                    (mark-global-needed! (fluid-ref bindings-data) sym module))
                  (map car dynamic))
        (let ((make-values (lambda (for)
                             (map (lambda (el)
                                    (compile-expr (cdr el)))
                                  for)))
              (make-body (lambda ()
                           (make-sequence loc (map compile-expr body)))))
          (if (null? lexical)
            (let-dynamic loc (map car dynamic) module
                         (make-values dynamic) (make-body))
            (let* ((lexical-syms (map (lambda (el) (gensym)) lexical))
                   (dynamic-syms (map (lambda (el) (gensym)) dynamic))
                   (all-syms (append lexical-syms dynamic-syms))
                   (vals (append (make-values lexical) (make-values dynamic))))
              (make-let loc all-syms all-syms vals
                (with-lexical-bindings (fluid-ref bindings-data)
                                       (map car lexical) lexical-syms
                  (lambda ()
                    (if (null? dynamic)
                      (make-body)
                      (let-dynamic loc (map car dynamic) module
                                   (map (lambda (sym)
                                          (make-lexical-ref loc sym sym))
                                        dynamic-syms)
                                   (make-body)))))))))))))

; Let* is compiled to a cascaded set of "small lets" for each binding in turn
; so that each one already sees the preceding bindings.

(define (generate-let* loc module bindings body)
  (let ((bind (process-let-bindings loc bindings)))
    (begin
      (for-each (lambda (sym)
                  (if (not (bind-lexically? sym module))
                    (mark-global-needed! (fluid-ref bindings-data) sym module)))
                (map car bind))
      (let iterate ((tail bind))
        (if (null? tail)
          (make-sequence loc (map compile-expr body))
          (let ((sym (caar tail))
                (value (compile-expr (cdar tail))))
            (if (bind-lexically? sym module)
              (let ((target (gensym)))
                (make-let loc `(,target) `(,target) `(,value)
                  (with-lexical-bindings (fluid-ref bindings-data)
                                         `(,sym) `(,target)
                    (lambda ()
                      (iterate (cdr tail))))))
              (let-dynamic loc
                           `(,(caar tail)) module `(,value)
                           (iterate (cdr tail))))))))))

; Split the argument list of a lambda expression into required, optional and
; rest arguments and also check it is actually valid.
; Additionally, we create a list of all "local variables" (that is, required,
; optional and rest arguments together) and also this one split into those to
; be bound lexically and dynamically.
; Returned is as multiple values: required optional rest lexical dynamic

(define (bind-arg-lexical? arg)
  (let ((always (fluid-ref always-lexical)))
    (or (eq? always 'all)
        (memq arg always))))

(define (split-lambda-arguments loc args)
  (let iterate ((tail args)
                (mode 'required)
                (required '())
                (optional '())
                (lexical '())
                (dynamic '()))
    (cond
      ((null? tail)
       (let ((final-required (reverse required))
             (final-optional (reverse optional))
             (final-lexical (reverse lexical))
             (final-dynamic (reverse dynamic)))
         (values final-required final-optional #f
                 final-lexical final-dynamic)))
      ((and (eq? mode 'required)
            (eq? (car tail) '&optional))
       (iterate (cdr tail) 'optional required optional lexical dynamic))
      ((eq? (car tail) '&rest)
       (if (or (null? (cdr tail))
               (not (null? (cddr tail))))
         (report-error loc "expected exactly one symbol after &rest")
         (let* ((rest (cadr tail))
                (rest-lexical (bind-arg-lexical? rest))
                (final-required (reverse required))
                (final-optional (reverse optional))
                (final-lexical (reverse (if rest-lexical
                                          (cons rest lexical)
                                          lexical)))
                (final-dynamic (reverse (if rest-lexical
                                          dynamic
                                          (cons rest dynamic)))))
           (values final-required final-optional rest
                   final-lexical final-dynamic))))
      (else
        (if (not (symbol? (car tail)))
          (report-error loc "expected symbol in argument list, got" (car tail))
          (let* ((arg (car tail))
                 (bind-lexical (bind-arg-lexical? arg))
                 (new-lexical (if bind-lexical
                                (cons arg lexical)
                                lexical))
                 (new-dynamic (if bind-lexical
                                dynamic
                                (cons arg dynamic))))
            (case mode
              ((required) (iterate (cdr tail) mode
                                   (cons arg required) optional
                                   new-lexical new-dynamic))
              ((optional) (iterate (cdr tail) mode
                                   required (cons arg optional)
                                   new-lexical new-dynamic))
              (else
                (error "invalid mode in split-lambda-arguments" mode)))))))))

; Compile a lambda expression.  Things get a little complicated because TreeIL
; does not allow optional arguments but only one rest argument, and also the
; rest argument should be nil instead of '() for no values given.  Because of
; this, we have to do a little preprocessing to get everything done before the
; real body is called.
;
; (lambda (a &optional b &rest c) body) should become:
; (lambda (a_ . rest_)
;   (with-fluids* (list a b c) (list a_ nil nil)
;     (lambda ()
;       (if (not (null? rest_))
;         (begin
;           (fluid-set! b (car rest_))
;           (set! rest_ (cdr rest_))
;           (if (not (null? rest_))
;             (fluid-set! c rest_))))
;       body)))
;
; This is formulated very imperatively, but I think in this case that is quite
; clear and better than creating a lot of nested let's.
;
; Another thing we have to be aware of is that lambda arguments are usually
; dynamically bound, even when a lexical binding is in tact for a symbol.
; For symbols that are marked as 'always lexical' however, we bind them here
; lexically, too -- and thus we get them out of the let-dynamic call and
; register a lexical binding for them (the lexical target variable is already
; there, namely the real lambda argument from TreeIL).
; For optional arguments that are lexically bound we need to create the lexical
; bindings though with an additional let, as those arguments are not part of the
; ordinary argument list.

(define (compile-lambda loc args body)
  (if (not (list? args))
    (report-error loc "expected list for argument-list" args))
  (if (null? body)
    (report-error loc "function body might not be empty"))
  (call-with-values
    (lambda ()
      (split-lambda-arguments loc args))
    (lambda (required optional rest lexical dynamic)
      (let* ((make-sym (lambda (sym) (gensym)))
             (required-sym (map make-sym required))
             (required-pairs (map cons required required-sym))
             (have-real-rest (or rest (not (null? optional))))
             (rest-sym (if have-real-rest (gensym) '()))
             (rest-name (if rest rest rest-sym))
             (rest-lexical (and rest (memq rest lexical)))
             (rest-dynamic (and rest (not rest-lexical)))
             (real-args (append required-sym rest-sym))
             (arg-names (append required rest-name))
             (lex-optionals (lset-intersection eq? optional lexical))
             (dyn-optionals (lset-intersection eq? optional dynamic))
             (optional-sym (map make-sym lex-optionals))
             (optional-lex-pairs (map cons lex-optionals optional-sym))
             (find-required-pairs (lambda (filter)
                                    (lset-intersection (lambda (name-sym el)
                                                         (eq? (car name-sym)
                                                              el))
                                                       required-pairs filter)))
             (required-lex-pairs (find-required-pairs lexical))
             (rest-pair (if rest-lexical `((,rest . ,rest-sym)) '()))
             (all-lex-pairs (append required-lex-pairs optional-lex-pairs
                                    rest-pair)))
        (for-each (lambda (sym)
                    (mark-global-needed! (fluid-ref bindings-data)
                                         sym value-slot))
                  dynamic)
        (with-dynamic-bindings (fluid-ref bindings-data) dynamic
          (lambda ()
            (with-lexical-bindings (fluid-ref bindings-data)
                                   (map car all-lex-pairs)
                                   (map cdr all-lex-pairs)
              (lambda ()
                (make-lambda loc '()
                 (make-lambda-case
                  #f required #f
                  (if have-real-rest rest-name #f)
                  #f '()
                  (if have-real-rest
                    (append required-sym (list rest-sym))
                    required-sym)
                  (let* ((init-req (map (lambda (name-sym)
                                          (make-lexical-ref loc (car name-sym)
                                                                (cdr name-sym)))
                                        (find-required-pairs dynamic)))
                         (init-nils (map (lambda (sym) (nil-value loc))
                                         (if rest-dynamic
                                           `(,@dyn-optionals ,rest-sym)
                                           dyn-optionals)))
                         (init (append init-req init-nils))
                         (func-body (make-sequence loc
                                      `(,(process-optionals loc optional
                                                            rest-name rest-sym)
                                        ,(process-rest loc rest
                                                       rest-name rest-sym)
                                        ,@(map compile-expr body))))
                         (dynlet (let-dynamic loc dynamic value-slot
                                              init func-body))
                         (full-body (if (null? dynamic) func-body dynlet)))
                  (if (null? optional-sym)
                    full-body
                    (make-let loc
                              optional-sym optional-sym
                              (map (lambda (sym) (nil-value loc)) optional-sym)
                      full-body)))
                  #f))))))))))

; Build the code to handle setting of optional arguments that are present
; and updating the rest list.

(define (process-optionals loc optional rest-name rest-sym)
  (let iterate ((tail optional))
    (if (null? tail)
      (make-void loc)
      (make-conditional loc
        (call-primitive loc 'null? (make-lexical-ref loc rest-name rest-sym))
        (make-void loc)
        (make-sequence loc
          (list (set-variable! loc (car tail) value-slot
                  (call-primitive loc 'car
                                  (make-lexical-ref loc rest-name rest-sym)))
                (make-lexical-set loc rest-name rest-sym
                  (call-primitive loc 'cdr
                                  (make-lexical-ref loc rest-name rest-sym)))
                (iterate (cdr tail))))))))

; This builds the code to set the rest variable to nil if it is empty.

(define (process-rest loc rest rest-name rest-sym)
  (let ((rest-empty (call-primitive loc 'null?
                                    (make-lexical-ref loc rest-name rest-sym))))
    (cond
      (rest
       (make-conditional loc rest-empty
         (make-void loc)
         (set-variable! loc rest value-slot
                        (make-lexical-ref loc rest-name rest-sym))))
      ((not (null? rest-sym))
       (make-conditional loc rest-empty
         (make-void loc)
         (runtime-error loc "too many arguments and no rest argument")))
      (else (make-void loc)))))

; Handle the common part of defconst and defvar, that is, checking for a correct
; doc string and arguments as well as maybe in the future handling the docstring
; somehow.

(define (handle-var-def loc sym doc)
  (cond
    ((not (symbol? sym)) (report-error loc "expected symbol, got" sym))
    ((> (length doc) 1) (report-error loc "too many arguments to defvar"))
    ((and (not (null? doc)) (not (string? (car doc))))
     (report-error loc "expected string as third argument of defvar, got"
                   (car doc)))
    ; TODO: Handle doc string if present.
    (else #t)))

; Handle macro bindings.

(define (is-macro? sym)
  (module-defined? (resolve-interface macro-slot) sym))

(define (define-macro! loc sym definition)
  (let ((resolved (resolve-module macro-slot)))
    (if (is-macro? sym)
      (report-error loc "macro is already defined" sym)
      (begin
        (module-define! resolved sym definition)
        (module-export! resolved (list sym))))))

(define (get-macro sym)
  (module-ref (resolve-module macro-slot) sym))

; See if a (backquoted) expression contains any unquotes.

(define (contains-unquotes? expr)
  (if (pair? expr)
    (if (or (unquote? (car expr)) (unquote-splicing? (car expr)))
      #t
      (or (contains-unquotes? (car expr))
          (contains-unquotes? (cdr expr))))
    #f))

; Process a backquoted expression by building up the needed cons/append calls.
; For splicing, it is assumed that the expression spliced in evaluates to a
; list.  The emacs manual does not really state either it has to or what to do
; if it does not, but Scheme explicitly forbids it and this seems reasonable
; also for elisp.

(define (unquote-cell? expr)
  (and (list? expr) (= (length expr) 2) (unquote? (car expr))))

(define (unquote-splicing-cell? expr)
  (and (list? expr) (= (length expr) 2) (unquote-splicing? (car expr))))

(define (process-backquote loc expr)
  (if (contains-unquotes? expr)
    (if (pair? expr)
      (if (or (unquote-cell? expr) (unquote-splicing-cell? expr))
        (compile-expr (cadr expr))
        (let* ((head (car expr))
               (processed-tail (process-backquote loc (cdr expr)))
               (head-is-list-2 (and (list? head) (= (length head) 2)))
               (head-unquote (and head-is-list-2 (unquote? (car head))))
               (head-unquote-splicing (and head-is-list-2
                                           (unquote-splicing? (car head)))))
          (if head-unquote-splicing
            (call-primitive loc 'append
              (compile-expr (cadr head)) processed-tail)
            (call-primitive loc 'cons
              (if head-unquote
                (compile-expr (cadr head))
                (process-backquote loc head))
              processed-tail))))
      (report-error loc "non-pair expression contains unquotes" expr))
    (make-const loc expr)))

; Temporarily update a list of symbols that are handled specially (disabled
; void check or always lexical) for compiling body.
; We need to handle special cases for already all / set to all and the like.

(define (with-added-symbols loc fluid syms body)
  (if (null? body)
    (report-error loc "symbol-list construct has empty body"))
  (if (not (or (eq? syms 'all)
               (and (list? syms) (and-map symbol? syms))))
    (report-error loc "invalid symbol list" syms))
  (let ((old (fluid-ref fluid))
        (make-body (lambda ()
                     (make-sequence loc (map compile-expr body)))))
    (if (eq? old 'all)
      (make-body)
      (let ((new (if (eq? syms 'all)
                   'all
                   (append syms old))))
        (with-fluids ((fluid new))
          (make-body))))))

; Compile a symbol expression.  This is a variable reference or maybe some
; special value like nil.

(define (compile-symbol loc sym)
  (case sym
    ((nil) (nil-value loc))
    ((t) (t-value loc))
    (else (reference-with-check loc sym value-slot))))

; Compile a pair-expression (that is, any structure-like construct).

(define (compile-pair loc expr)
  (pmatch expr
    ((progn . ,forms)
     (make-sequence loc (map compile-expr forms)))

    ((if ,condition ,ifclause)
     (make-conditional loc (compile-expr condition)
                           (compile-expr ifclause)
                           (nil-value loc)))

    ((if ,condition ,ifclause ,elseclause)
     (make-conditional loc (compile-expr condition)
                           (compile-expr ifclause)
                           (compile-expr elseclause)))

    ((if ,condition ,ifclause . ,elses)
     (make-conditional loc (compile-expr condition)
                           (compile-expr ifclause)
                           (make-sequence loc (map compile-expr elses))))

    ; defconst and defvar are kept here in the compiler (rather than doing them
    ; as macros) for if we may want to handle the docstring somehow.

    ((defconst ,sym ,value . ,doc)
     (if (handle-var-def loc sym doc)
       (make-sequence loc
         (list (set-variable! loc sym value-slot (compile-expr value))
               (make-const loc sym)))))

    ((defvar ,sym) (make-const loc sym))

    ((defvar ,sym ,value . ,doc)
     (if (handle-var-def loc sym doc)
       (make-sequence loc
         (list (make-conditional loc
                 (call-primitive loc 'eq?
                                 (make-module-ref loc runtime 'void #t)
                                 (reference-variable loc sym value-slot))
                 (set-variable! loc sym value-slot
                                (compile-expr value))
                 (make-void loc))
               (make-const loc sym)))))

    ; Build a set form for possibly multiple values.  The code is not formulated
    ; tail recursive because it is clearer this way and large lists of symbol
    ; expression pairs are very unlikely.

    ((setq . ,args) (guard (not (null? args)))
     (make-sequence loc
       (let iterate ((tail args))
         (let ((sym (car tail))
               (tailtail (cdr tail)))
           (if (not (symbol? sym))
             (report-error loc "expected symbol in setq")
             (if (null? tailtail)
               (report-error loc "missing value for symbol in setq" sym)
               (let* ((val (compile-expr (car tailtail)))
                      (op (set-variable! loc sym value-slot val)))
                 (if (null? (cdr tailtail))
                   (let* ((temp (gensym))
                          (ref (make-lexical-ref loc temp temp)))
                     (list (make-let loc `(,temp) `(,temp) `(,val)
                             (make-sequence loc
                               (list (set-variable! loc sym value-slot ref)
                                     ref)))))
                   (cons (set-variable! loc sym value-slot val)
                         (iterate (cdr tailtail)))))))))))

    ; All lets (let, flet, lexical-let and let* forms) are done using the
    ; generate-let/generate-let* methods.

    ((let ,bindings . ,body) (guard (and (list? bindings)
                                         (not (null? bindings))
                                         (not (null? body))))
     (generate-let loc value-slot bindings body))

    ((lexical-let ,bindings . ,body) (guard (and (list? bindings)
                                                 (not (null? bindings))
                                                 (not (null? body))))
     (generate-let loc 'lexical bindings body))

    ((flet ,bindings . ,body) (guard (and (list? bindings)
                                          (not (null? bindings))
                                          (not (null? body))))
     (generate-let loc function-slot bindings body))

    ((let* ,bindings . ,body) (guard (and (list? bindings)
                                          (not (null? bindings))
                                          (not (null? body))))
     (generate-let* loc value-slot bindings body))

    ((lexical-let* ,bindings . ,body) (guard (and (list? bindings)
                                                  (not (null? bindings))
                                                  (not (null? body))))
     (generate-let* loc 'lexical bindings body))

    ((flet* ,bindings . ,body) (guard (and (list? bindings)
                                           (not (null? bindings))
                                           (not (null? body))))
     (generate-let* loc function-slot bindings body))

    ; Temporarily disable void checks or set symbols as always lexical only
    ; for the lexical scope of a construct.

    ((without-void-checks ,syms . ,body)
     (with-added-symbols loc disable-void-check syms body))

    ((with-always-lexical ,syms . ,body)
     (with-added-symbols loc always-lexical syms body))

    ; guile-ref allows building TreeIL's module references from within
    ; elisp as a way to access data within
    ; the Guile universe.  The module and symbol referenced are static values,
    ; just like (@ module symbol) does!

    ((guile-ref ,module ,sym) (guard (and (list? module) (symbol? sym)))
     (make-module-ref loc module sym #t))

    ; guile-primitive allows to create primitive references, which are still
    ; a little faster.

    ((guile-primitive ,sym) (guard (symbol? sym))
     (make-primitive-ref loc sym))

    ; A while construct is transformed into a tail-recursive loop like this:
    ; (letrec ((iterate (lambda ()
    ;                     (if condition
    ;                       (begin body
    ;                              (iterate))
    ;                       #nil))))
    ;   (iterate))
    ;
    ; As letrec is not directly accessible from elisp, while is implemented here
    ; instead of with a macro.

    ((while ,condition . ,body)
     (let* ((itersym (gensym))
            (compiled-body (map compile-expr body))
            (iter-call (make-application loc
                         (make-lexical-ref loc 'iterate itersym)
                         (list)))
            (full-body (make-sequence loc
                         `(,@compiled-body ,iter-call)))
            (lambda-body (make-conditional loc
                           (compile-expr condition)
                           full-body
                           (nil-value loc)))
            (iter-thunk (make-lambda loc '()
                          (make-lambda-case #f '() #f #f #f '() '()
                                            lambda-body #f))))
       (make-letrec loc #f '(iterate) (list itersym) (list iter-thunk)
         iter-call)))

    ; Either (lambda ...) or (function (lambda ...)) denotes a lambda-expression
    ; that should be compiled.

    ((lambda ,args . ,body)
     (compile-lambda loc args body))

    ((function (lambda ,args . ,body))
     (compile-lambda loc args body))

    ; Build a lambda and also assign it to the function cell of some symbol.
    ; This is no macro as we might want to honour the docstring at some time;
    ; just as with defvar/defconst.

    ((defun ,name ,args . ,body)
     (if (not (symbol? name))
       (report-error loc "expected symbol as function name" name)
       (make-sequence loc
         (list (set-variable! loc name function-slot
                              (compile-lambda loc args body))
               (make-const loc name)))))

    ; Define a macro (this is done directly at compile-time!).
    ; FIXME: Recursive macros don't work!

    ((defmacro ,name ,args . ,body)
     (if (not (symbol? name))
       (report-error loc "expected symbol as macro name" name)
       (let* ((tree-il (with-fluids ((bindings-data (make-bindings)))
                         (compile-lambda loc args body)))
              (object (compile tree-il #:from 'tree-il #:to 'value)))
         (define-macro! loc name object)
         (make-const loc name))))

    ; XXX: Maybe we could implement backquotes in macros, too.

    ((,backq ,val) (guard (backquote? backq))
     (process-backquote loc val))

    ; XXX: Why do we need 'quote here instead of quote?

    (('quote ,val)
     (make-const loc val))

    ; Macro calls are simply expanded and recursively compiled.

    ((,macro . ,args) (guard (and (symbol? macro) (is-macro? macro)))
     (let ((expander (get-macro macro)))
       (compile-expr (apply expander args))))

    ; Function calls using (function args) standard notation; here, we have to
    ; take the function value of a symbol if it is one.  It seems that functions
    ; in form of uncompiled lists are not supported in this syntax, so we don't
    ; have to care for them.

    ((,func . ,args)
     (make-application loc
       (if (symbol? func)
         (reference-with-check loc func function-slot)
         (compile-expr func))
       (map compile-expr args)))

    (else
      (report-error loc "unrecognized elisp" expr))))

; Compile a single expression to TreeIL.

(define (compile-expr expr)
  (let ((loc (location expr)))
    (cond
      ((symbol? expr)
       (compile-symbol loc expr))
      ((pair? expr)
       (compile-pair loc expr))
      (else (make-const loc expr)))))

; Process the compiler options.
; FIXME: Why is '(()) passed as options by the REPL?

(define (valid-symbol-list-arg? value)
  (or (eq? value 'all)
      (and (list? value) (and-map symbol? value))))

(define (process-options! opt)
  (if (and (not (null? opt))
           (not (equal? opt '(()))))
    (if (null? (cdr opt))
      (report-error #f "Invalid compiler options" opt)
      (let ((key (car opt))
            (value (cadr opt)))
        (case key
          ((#:disable-void-check)
           (if (valid-symbol-list-arg? value)
             (fluid-set! disable-void-check value)
             (report-error #f "Invalid value for #:disable-void-check" value)))
          ((#:always-lexical)
           (if (valid-symbol-list-arg? value)
             (fluid-set! always-lexical value)
             (report-error #f "Invalid value for #:always-lexical" value)))
          (else (report-error #f "Invalid compiler option" key)))))))

; Entry point for compilation to TreeIL.
; This creates the bindings data structure, and after compiling the main
; expression we need to make sure all globals for symbols used during the
; compilation are created using the generate-ensure-global function.

(define (compile-tree-il expr env opts)
  (values
    (with-fluids ((bindings-data (make-bindings))
                  (disable-void-check '())
                  (always-lexical '()))
      (process-options! opts)
      (let ((loc (location expr))
            (compiled (compile-expr expr)))
        (make-sequence loc
          `(,@(map-globals-needed (fluid-ref bindings-data)
                                  (lambda (mod sym)
                                    (generate-ensure-global loc sym mod)))
            ,compiled))))
    env
    env))
