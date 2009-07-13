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
  #:use-module (language tree-il)
  #:use-module (system base pmatch)
  #:export (compile-tree-il))


; Find the source properties of some parsed expression if there are any
; associated with it.

(define (location x)
  (and (pair? x)
       (let ((props (source-properties x)))
         (and (not (null? props))
              props))))


; Value to use for Elisp's nil and t.

; FIXME: Use real nil.
(define (nil-value loc) (make-const loc #f))
(define (t-value loc) (make-const loc #t))


; Modules that contain the value and function slot bindings.

(define runtime '(language elisp runtime))
(define value-slot '(language elisp runtime value-slot))
(define function-slot '(language elisp runtime function-slot))


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
; ensure-fluids-for does the same for a list of symbols and builds a sequence
; that executes the fluid-insurances first, followed by all body commands; this
; is a routine for convenience (needed with let, let*, lambda).

(define (ensure-fluid! loc sym module)
  (let ((resolved-module (call-primitive loc 'resolve-module
                                         (make-const loc module)))
        (resolved-intf (call-primitive loc 'resolve-interface
                                       (make-const loc module))))
    (make-conditional loc
      (call-primitive loc 'module-defined? resolved-intf (make-const loc sym))
      (make-void loc)
      (make-sequence loc
        (list (call-primitive loc 'module-define!
                resolved-module (make-const loc sym)
                (call-primitive loc 'make-fluid))
              (call-primitive loc 'module-export!
                resolved-module
                (call-primitive loc 'list (make-const loc sym)))
              (call-primitive loc 'fluid-set!
                (make-module-ref loc module sym #t)
                (make-module-ref loc runtime 'void #t)))))))


(define (ensure-fluids-for loc syms module . body)
  (make-sequence loc
    `(,@(map (lambda (sym) (ensure-fluid! loc sym module)) syms)
      ,@body)))


; Generate code to reference a fluid saved variable.

(define (reference-variable loc sym module)
  (make-sequence loc
    (list (ensure-fluid! loc sym module)
          (call-primitive loc 'fluid-ref
                          (make-module-ref loc module sym #t)))))


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
  (make-sequence loc
    (list (ensure-fluid! loc sym module)
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
            (ensure-fluids-for loc locals value-slot
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

    ; Build a set form for possibly multiple values.  The code is not formulated
    ; tail recursive because it is clearer this way and large lists of symbol
    ; expression pairs are very unlikely.
    ((setq . ,args)
     (make-sequence loc
       (let iterate ((tail args))
         (if (null? tail)
           (list (make-void loc))
           (let ((sym (car tail))
                 (tailtail (cdr tail)))
             (if (not (symbol? sym))
               (report-error loc "expected symbol in setq")
               (if (null? tailtail)
                 (report-error loc "missing value for symbol in setq" sym)
                 (let* ((val (compile-expr (car tailtail)))
                        (op (set-variable! loc sym value-slot val)))
                   (cons op (iterate (cdr tailtail)))))))))))

    ; Let is done with a single call to with-fluids* binding them locally to new
    ; values.
    ((let ,bindings . ,body) (guard (and (list? bindings)
                                         (list? body)
                                         (not (null? bindings))
                                         (not (null? body))))
     (let ((bind (process-let-bindings loc bindings)))
       (ensure-fluids-for loc (map car bind) value-slot
         (call-primitive loc 'with-fluids*
           (make-application loc (make-primitive-ref loc 'list)
             (map (lambda (el)
                 (make-module-ref loc value-slot (car el) #t))
               bind))
           (make-application loc (make-primitive-ref loc 'list)
             (map (lambda (el)
                    (compile-expr (cdr el)))
                  bind))
           (make-lambda loc '() '() '() 
             (make-sequence loc (map compile-expr body)))))))

    ; Let* is compiled to a cascaded set of with-fluid* for each binding in turn
    ; so that each one already sees the preceding bindings.
    ((let* ,bindings . ,body) (guard (and (list? bindings)
                                          (list? body)
                                          (not (null? bindings))
                                          (not (null? body))))
     (let ((bind (process-let-bindings loc bindings)))
       (ensure-fluids-for loc (map car bind) value-slot
         (let iterate ((tail bind))
           (if (null? tail)
             (make-sequence loc (map compile-expr body))
             (call-primitive loc 'with-fluid*
               (make-module-ref loc value-slot (caar tail) #t)
               (compile-expr (cdar tail))
               (make-lambda loc '() '() '() (iterate (cdr tail)))))))))

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

    ; Either (lambda ...) or (function (lambda ...)) denotes a lambda-expression
    ; that should be compiled.
    ((lambda ,args . ,body) (guard (not (null? body)))
     (compile-lambda loc args body))
    ((function (lambda ,args . ,body)) (guard (not (null? body)))
     (compile-lambda loc args body))

    (('quote ,val)
     (make-const loc val))

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

(define (compile-tree-il expr env opts)
  (values
    (compile-expr expr)
    env
    env))
