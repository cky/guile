;;; Guile Emac Lisp

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
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
  #:export (compile-tree-il))


; Certain common parameters (like the bindings data structure or compiler
; options) are not always passed around but accessed using fluids.

; The bindings data structure to keep track of symbol binding related data.
(define bindings-data (make-fluid))

; Store for which symbols (or all/none) void checks are disabled.
(define disabled-void-check (make-fluid))


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

; FIXME: Remove the quasiquote/unquote/unquote-splicing symbols when real elisp
; reader is there.

(define (backquote? sym)
  (and (symbol? sym) (or (eq? sym 'quasiquote)
                         (eq? sym '\`))))

(define (unquote? sym)
  (and (symbol? sym) (or (eq? sym 'unquote)
                         (eq? sym '\,))))

(define (unquote-splicing? sym)
  (and (symbol? sym) (or (eq? sym 'unquote-splicing)
                         (eq? sym '\,@))))


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


; Generate code to ensure a fluid is there for further use of a given symbol.
; In general during the compilation, fluids needed are only tracked with the
; bindings data structure.  Afterwards, however, for all those needed symbols
; the fluids are really generated with this routine.

(define (generate-ensure-fluid loc sym module)
  (make-application loc (make-module-ref loc runtime 'ensure-fluid! #t)
    (list (make-const loc module)
          (make-const loc sym))))


; Generate code to reference a fluid saved variable.

(define (reference-variable loc sym module)
  (mark-fluid-needed! (fluid-ref bindings-data) sym module)
  (call-primitive loc 'fluid-ref
                  (make-module-ref loc module sym #t)))


; Reference a variable and error if the value is void.

(define (reference-with-check loc sym module)
  (let ((var (gensym)))
    (make-let loc '(value) `(,var) `(,(reference-variable loc sym module))
      (make-conditional loc
        (call-primitive loc 'eq?
                        (make-module-ref loc runtime 'void #t)
                        (make-lexical-ref loc 'value var))
        (runtime-error loc "variable is void:" (make-const loc sym))
        (make-lexical-ref loc 'value var)))))


; Generate code to set a fluid saved variable.

(define (set-variable! loc sym module value)
  (mark-fluid-needed! (fluid-ref bindings-data) sym module)
  (call-primitive loc 'fluid-set!
                  (make-module-ref loc module sym #t) value))


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


; Split the argument list of a lambda expression into required, optional and
; rest arguments and also check it is actually valid.

(define (split-lambda-arguments loc args)
  (let iterate ((tail args)
                (mode 'required)
                (required '())
                (optional '()))
    (cond

      ((null? tail)
       (values (reverse required) (reverse optional) #f))

      ((and (eq? mode 'required)
            (eq? (car tail) '&optional))
       (iterate (cdr tail) 'optional required optional))

      ((eq? (car tail) '&rest)
       (if (or (null? (cdr tail))
               (not (null? (cddr tail))))
         (report-error loc "expected exactly one symbol after &rest")
         (values (reverse required) (reverse optional) (cadr tail))))

      (else
        (if (not (symbol? (car tail)))
          (report-error loc "expected symbol in argument list, got" (car tail))
          (case mode
            ((required) (iterate (cdr tail) mode
                                 (cons (car tail) required) optional))
            ((optional) (iterate (cdr tail) mode
                                 required (cons (car tail) optional)))
            ((else) (error "invalid mode in split-lambda-arguments" mode))))))))


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
; This is formulated quite imperatively, but I think in this case that is quite
; clear and better than creating a lot of nested let's.

(define (compile-lambda loc args body)
  (if (not (list? args))
    (error "expected list for argument-list" args))
  (if (null? body)
    (error "function body might not be empty"))
  (call-with-values
    (lambda ()
      (split-lambda-arguments loc args))
    (lambda (required optional rest)
      (let ((required-sym (map (lambda (sym) (gensym)) required))
            (rest-sym (if (or rest (not (null? optional))) (gensym) '())))
        (let ((real-args (append required-sym rest-sym))
              (locals `(,@required ,@optional ,@(if rest (list rest) '()))))
          (make-lambda loc
            real-args real-args '()
            (begin
              (for-each (lambda (sym)
                          (mark-fluid-needed! (fluid-ref bindings-data)
                                              sym value-slot))
                        locals)
              (call-primitive loc 'with-fluids*
                (make-application loc (make-primitive-ref loc 'list)
                  (map (lambda (sym) (make-module-ref loc value-slot sym #t))
                       locals))
                (make-application loc (make-primitive-ref loc 'list)
                  (append (map (lambda (sym) (make-lexical-ref loc sym sym))
                               required-sym)
                          (map (lambda (sym) (nil-value loc))
                               (if rest
                                 `(,@optional ,rest-sym)
                                 optional))))
                (make-lambda loc '() '() '()
                  (make-sequence loc
                    `(,(process-optionals loc optional rest-sym)
                      ,(process-rest loc rest rest-sym)
                      ,@(map compile-expr body))))))))))))

; Build the code to handle setting of optional arguments that are present
; and updating the rest list.
(define (process-optionals loc optional rest-sym)
  (let iterate ((tail optional))
    (if (null? tail)
      (make-void loc)
      (make-conditional loc
        (call-primitive loc 'null? (make-lexical-ref loc rest-sym rest-sym))
        (make-void loc)
        (make-sequence loc
          (list (set-variable! loc (car tail) value-slot
                  (call-primitive loc 'car
                                  (make-lexical-ref loc rest-sym rest-sym)))
                (make-lexical-set loc rest-sym rest-sym
                  (call-primitive loc 'cdr
                                  (make-lexical-ref loc rest-sym rest-sym)))
                (iterate (cdr tail))))))))

; This builds the code to set the rest variable to nil if it is empty.
(define (process-rest loc rest rest-sym)
  (let ((rest-empty (call-primitive loc 'null?
                                    (make-lexical-ref loc rest-sym rest-sym))))
    (cond
      (rest
       (make-conditional loc rest-empty
         (make-void loc)
         (set-variable! loc rest value-slot
                        (make-lexical-ref loc rest-sym rest-sym))))
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
      (error "non-pair expression contains unquotes" expr))
    (make-const loc expr)))


; Compile a dolist construct.
; This is compiled to something along:
; (with-fluid* iter-var %nil
;   (lambda ()
;     (let iterate ((tail list))
;       (if (null? tail)
;         result
;         (begin
;           (fluid-set! iter-var (car tail))
;           body
;           (iterate (cdr tail)))))))

(define (compile-dolist loc var iter-list result body)
  (let* ((tailvar (gensym))
         (iterate (gensym))
         (tailref (make-lexical-ref loc tailvar tailvar))
         (iterate-func (make-lambda loc `(,tailvar) `(,tailvar) '()
                         (make-conditional loc
                           (call-primitive loc 'null? tailref)
                           (compile-expr result)
                           (make-sequence loc
                             `(,(set-variable! loc var value-slot
                                  (call-primitive loc 'car tailref))
                               ,@(map compile-expr body)
                               ,(make-application loc
                                  (make-lexical-ref loc iterate iterate)
                                  (list (call-primitive loc 'cdr
                                          tailref)))))))))
    (mark-fluid-needed! (fluid-ref bindings-data) var value-slot)
    (call-primitive loc 'with-fluid*
      (make-module-ref loc value-slot var #t)
      (nil-value loc)
      (make-lambda loc '() '() '()
        (make-letrec loc `(,iterate) `(,iterate) `(,iterate-func)
          (make-application loc
            (make-lexical-ref loc iterate iterate)
            (list (compile-expr iter-list))))))))


; Compile let and let* expressions.  The code here is used both for let/let*
; and flet/flet*, just with a different bindings module.

; Let is done with a single call to with-fluids* binding them locally to new
; values all "at once".
(define (generate-let loc module bindings body)
 (let ((bind (process-let-bindings loc bindings)))
   (begin
     (for-each (lambda (sym)
                 (mark-fluid-needed! (fluid-ref bindings-data) sym module))
               (map car bind))
     (call-primitive loc 'with-fluids*
       (make-application loc (make-primitive-ref loc 'list)
         (map (lambda (el)
                (make-module-ref loc module (car el) #t))
              bind))
       (make-application loc (make-primitive-ref loc 'list)
         (map (lambda (el)
                (compile-expr (cdr el)))
              bind))
       (make-lambda loc '() '() '() 
         (make-sequence loc (map compile-expr body)))))))

; Let* is compiled to a cascaded set of with-fluid* for each binding in turn
; so that each one already sees the preceding bindings.
(define (generate-let* loc module bindings body)
 (let ((bind (process-let-bindings loc bindings)))
   (begin
     (for-each (lambda (sym)
                 (mark-fluid-needed! (fluid-ref bindings-data) sym module))
               (map car bind))
     (let iterate ((tail bind))
       (if (null? tail)
         (make-sequence loc (map compile-expr body))
         (call-primitive loc 'with-fluid*
           (make-module-ref loc module (caar tail) #t)
           (compile-expr (cdar tail))
           (make-lambda loc '() '() '() (iterate (cdr tail)))))))))


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

    ; I chose to implement prog1 directly (not with macros) so that the
    ; temporary variable used can be a lexical one that is not backed by a fluid
    ; for better performance.
    ((prog1 ,form1 . ,forms)
     (let ((temp (gensym)))
       (make-let loc `(,temp) `(,temp) `(,(compile-expr form1))
         (make-sequence loc
           (append (map compile-expr forms)
                   (list (make-lexical-ref loc temp temp)))))))

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

    ; For (cond ...) forms, a special case is a (condition) clause without
    ; body.  In this case, the value of condition itself should be returned,
    ; and thus is saved in a local variable for testing and returning, if it
    ; is found true.
    ((cond . ,clauses) (guard (and-map (lambda (el)
                                         (and (list? el) (not (null? el))))
                                       clauses))
     (let iterate ((tail clauses))
       (if (null? tail)
         (nil-value loc)
         (let ((cur (car tail)))
           (if (null? (cdr cur))
             (let ((var (gensym)))
               (make-let loc
                 '(condition) `(,var) `(,(compile-expr (car cur)))
                 (make-conditional loc
                   (make-lexical-ref loc 'condition var)
                   (make-lexical-ref loc 'condition var)
                   (iterate (cdr tail)))))
             (make-conditional loc
               (compile-expr (car cur))
               (make-sequence loc (map compile-expr (cdr cur)))
               (iterate (cdr tail))))))))

    ((and) (t-value loc))
    ((and . ,expressions)
     (let iterate ((tail expressions))
       (if (null? (cdr tail))
         (compile-expr (car tail))
         (make-conditional loc
           (compile-expr (car tail))
           (iterate (cdr tail))
           (nil-value loc)))))

    ((or . ,expressions)
     (let iterate ((tail expressions))
       (if (null? tail)
         (nil-value loc)
         (let ((var (gensym)))
           (make-let loc
             '(condition) `(,var) `(,(compile-expr (car tail)))
             (make-conditional loc
               (make-lexical-ref loc 'condition var)
               (make-lexical-ref loc 'condition var)
               (iterate (cdr tail))))))))

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

    ; let/let* and flet/flet* are done using the generate-let/generate-let*
    ; methods.

    ((let ,bindings . ,body) (guard (and (list? bindings)
                                         (not (null? bindings))
                                         (not (null? body))))
     (generate-let loc value-slot bindings body))
    ((flet ,bindings . ,body) (guard (and (list? bindings)
                                          (not (null? bindings))
                                          (not (null? body))))
     (generate-let loc function-slot bindings body))

    ((let* ,bindings . ,body) (guard (and (list? bindings)
                                          (not (null? bindings))
                                          (not (null? body))))
     (generate-let* loc value-slot bindings body))
    ((flet* ,bindings . ,body) (guard (and (list? bindings)
                                           (not (null? bindings))
                                           (not (null? body))))
     (generate-let* loc function-slot bindings body))

    ; guile-ref allows building TreeIL's module references from within
    ; elisp as a way to access data (and primitives, for instance) within
    ; the Guile universe.  The module and symbol referenced are static values,
    ; just like (@ module symbol) does!
    ((guile-ref ,module ,sym) (guard (and (list? module) (symbol? sym)))
     (make-module-ref loc module sym #t))

    ; A while construct is transformed into a tail-recursive loop like this:
    ; (letrec ((iterate (lambda ()
    ;                     (if condition
    ;                       (begin body
    ;                              (iterate))
    ;                       %nil))))
    ;   (iterate))
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
            (iter-thunk (make-lambda loc '() '() '() lambda-body)))
       (make-letrec loc '(iterate) (list itersym) (list iter-thunk)
         iter-call)))

    ; dolist is treated here rather than as macro because it can take advantage
    ; of a non-fluid-based variable.
    ((dolist (,var ,iter-list) . ,body) (guard (symbol? var))
     (compile-dolist loc var iter-list 'nil body))
    ((dolist (,var ,iter-list ,result) . ,body) (guard (symbol? var))
     (compile-dolist loc var iter-list result body))

    ; catch and throw can mainly be implemented directly using Guile's
    ; primitives for exceptions, the only difficulty is that the keys used
    ; within Guile must be symbols, while elisp allows any value and checks
    ; for matches using eq (eq?).  We handle this by using always #t as key
    ; for the Guile primitives and check for matches inside the handler; if
    ; the elisp keys are not eq?, we rethrow the exception.
    ;
    ; throw is implemented as built-in function.

    ((catch ,tag . ,body) (guard (not (null? body)))
     (let* ((tag-value (gensym))
            (tag-ref (make-lexical-ref loc tag-value tag-value)))
       (make-let loc `(,tag-value) `(,tag-value) `(,(compile-expr tag))
         (call-primitive loc 'catch
           (make-const loc #t)
           (make-lambda loc '() '() '()
             (make-sequence loc (map compile-expr body)))
           (let* ((dummy-key (gensym))
                  (dummy-ref (make-lexical-ref loc dummy-key dummy-key))
                  (elisp-key (gensym))
                  (key-ref (make-lexical-ref loc elisp-key elisp-key))
                  (value (gensym))
                  (value-ref (make-lexical-ref loc value value))
                  (arglist `(,dummy-key ,elisp-key ,value)))
             (make-lambda loc arglist arglist '()
               (make-conditional loc
                 (call-primitive loc 'eq? key-ref tag-ref)
                 value-ref
                 (call-primitive loc 'throw
                                 dummy-ref key-ref value-ref))))))))

    ; unwind-protect is just some weaker construct as dynamic-wind, so 
    ; straight-forward to implement.
    ((unwind-protect ,body . ,clean-ups) (guard (not (null? clean-ups)))
     (call-primitive loc 'dynamic-wind
                     (make-lambda loc '() '() '() (make-void loc))
                     (make-lambda loc '() '() '()
                       (compile-expr body))
                     (make-lambda loc '() '() '()
                       (make-sequence loc
                         (map compile-expr clean-ups)))))

    ; Either (lambda ...) or (function (lambda ...)) denotes a lambda-expression
    ; that should be compiled.
    ((lambda ,args . ,body)
     (compile-lambda loc args body))
    ((function (lambda ,args . ,body))
     (compile-lambda loc args body))

    ; Build a lambda and also assign it to the function cell of some symbol.
    ((defun ,name ,args . ,body)
     (if (not (symbol? name))
       (error "expected symbol as function name" name)
       (make-sequence loc
         (list (set-variable! loc name function-slot
                              (compile-lambda loc args body))
               (make-const loc name)))))

    ; Define a macro (this is done directly at compile-time!).
    ; FIXME: Recursive macros don't work!
    ((defmacro ,name ,args . ,body)
     (if (not (symbol? name))
       (error "expected symbol as macro name" name)
       (let* ((tree-il (with-fluid* bindings-data (make-bindings)
                         (lambda ()
                           (compile-lambda loc args body))))
              (object (compile tree-il #:from 'tree-il #:to 'value)))
         (define-macro! loc name object)
         (make-const loc name))))

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


; Entry point for compilation to TreeIL.
; This creates the bindings data structure, and after compiling the main
; expression we need to make sure all fluids for symbols used during the
; compilation are created using the generate-ensure-fluid function.

(define (compile-tree-il expr env opts)
  (values
    (with-fluid* bindings-data (make-bindings)
      (lambda ()
        (let ((loc (location expr))
              (compiled (compile-expr expr)))
          (make-sequence loc
            `(,@(map-fluids-needed (fluid-ref bindings-data)
                                   (lambda (mod sym)
                                     (generate-ensure-fluid loc sym mod)))
              ,compiled)))))
    env
    env))
