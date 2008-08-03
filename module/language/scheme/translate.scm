;;; Guile Scheme specification

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

(define-module (language scheme translate)
  :use-module (system base pmatch)
  :use-module (system base language)
  :use-module (system il ghil)
  :use-module (system il inline)
  :use-module (ice-9 receive)
  :use-module (srfi srfi-39)
  :use-module ((system base compile) :select (syntax-error))
  :export (translate))


(define (translate x e)
  (call-with-ghil-environment (make-ghil-mod e) '()
    (lambda (env vars)
      (make-ghil-lambda env #f vars #f (trans env (location x) x)))))


;;;
;;; Translator
;;;

(define %forbidden-primitives
  ;; Guile's `procedure->macro' family is evil because it crosses the
  ;; compilation boundary.  One solution might be to evaluate calls to
  ;; `procedure->memoizing-macro' at compilation time, but it may be more
  ;; compicated than that.
  '(procedure->syntax procedure->macro procedure->memoizing-macro))

(define (lookup-transformer e head retrans)
  (let* ((mod (ghil-mod-module (ghil-env-mod e)))
         (val (and (symbol? head)
                   (and=> (module-variable mod head) 
                          (lambda (var)
                            ;; unbound vars can happen if the module
                            ;; definition forward-declared them
                            (and (variable-bound? var) (variable-ref var)))))))
    (cond
     ((or (primitive-macro? val) (eq? val eval-case))
      (or (assq-ref primitive-syntax-table head)
          (syntax-error #f "unhandled primitive macro" head)))

     ((defmacro? val)
      (lambda (env loc exp)
        (retrans (apply (defmacro-transformer val) (cdr exp)))))

     ((and (macro? val) (eq? (macro-name val) 'sc-macro))
      ;; syncase!
      (let* ((the-syncase-module (resolve-module '(ice-9 syncase)))
             (eec (module-ref the-syncase-module 'expansion-eval-closure))
             (sc-expand3 (module-ref the-syncase-module 'sc-expand3)))
        (lambda (env loc exp)
          (retrans
           (with-fluids ((eec (module-eval-closure mod)))
             (sc-expand3 exp 'c '(compile load eval)))))))

     ((macro? val)
      (syntax-error #f "unknown kind of macro" head))

     (else #f))))

(define (trans e l x)
  (define (retrans x) (trans e (location x) x))
  (cond ((pair? x)
         (let ((head (car x)) (tail (cdr x)))
           (cond
            ((lookup-transformer e head retrans)
             => (lambda (t) (t e l x)))

            ;; FIXME: lexical/module overrides of forbidden primitives
            ((memq head %forbidden-primitives)
   	     (syntax-error l (format #f "`~a' is forbidden" head)
   			   (cons head tail)))

            (else
             (let ((tail (map retrans tail)))
               (or (and (symbol? head)
                        (try-inline-with-env e l (cons head tail)))
                   (make-ghil-call e l (retrans head) tail)))))))

	((symbol? x)
         (make-ghil-ref e l (ghil-lookup e x)))

        ;; fixme: non-self-quoting objects like #<foo>
	(else
         (make-ghil-quote e l #:obj x))))

(define (valid-bindings? bindings . it-is-for-do)
  (define (valid-binding? b)
    (pmatch b 
      ((,sym ,var) (guard (symbol? sym)) #t)
      ((,sym ,var ,update) (guard (pair? it-is-for-do) (symbol? sym)) #t)
      (else #f)))
  (and (list? bindings) (and-map valid-binding? bindings)))

(define-macro (make-pmatch-transformers env loc retranslate . body)
  (define exp (gensym))
  (define (make1 clause)
    (let ((sym (car clause))
          (clauses (cdr clause)))
      `(cons ',sym
             (lambda (,env ,loc ,exp)
               (define (,retranslate x) (trans ,env (location x) x))
               (pmatch (cdr ,exp)
                ,@clauses
                (else (syntax-error ,loc (format #f "bad ~A" ',sym) ,exp)))))))
  `(list ,@(map make1 body)))

(define *the-compile-toplevel-symbol* 'compile-toplevel)

(define primitive-syntax-table
  (make-pmatch-transformers
   e l retrans
   (quote
    ;; (quote OBJ)
    ((,obj) (make-ghil-quote e l obj)))
    
   (quasiquote
    ;; (quasiquote OBJ)
    ((,obj) (make-ghil-quasiquote e l (trans-quasiquote e l obj))))

   (define
    ;; (define NAME VAL)
    ((,name ,val) (guard (symbol? name) (ghil-env-toplevel? e))
     (make-ghil-define e l (ghil-define (ghil-env-parent e) name)
                       (retrans val)))
    ;; (define (NAME FORMALS...) BODY...)
    (((,name . ,formals) . ,body) (guard (symbol? name))
     ;; -> (define NAME (lambda FORMALS BODY...))
     (retrans `(define ,name (lambda ,formals ,@body)))))

   (set!
    ;; (set! NAME VAL)
    ((,name ,val) (guard (symbol? name))
     (make-ghil-set e l (ghil-lookup e name) (retrans val)))

    ;; (set! (NAME ARGS...) VAL)
    (((,name . ,args) ,val) (guard (symbol? name))
     ;; -> ((setter NAME) ARGS... VAL)
     (retrans `((setter ,name) . (,@args ,val)))))

   (if
    ;; (if TEST THEN [ELSE])
    ((,test ,then)
     (make-ghil-if e l (retrans test) (retrans then) (retrans '(begin))))
    ((,test ,then ,else)
     (make-ghil-if e l (retrans test) (retrans then) (retrans else))))

   (and
    ;; (and EXPS...)
    (,tail (make-ghil-and e l (map retrans tail))))

   (or
    ;; (or EXPS...)
    (,tail (make-ghil-or e l (map retrans tail))))

   (begin
     ;; (begin EXPS...)
     (,tail (make-ghil-begin e l (map retrans tail))))

   (let
    ;; (let NAME ((SYM VAL) ...) BODY...)
    ((,name ,bindings . ,body) (guard (symbol? name)
                                      (valid-bindings? bindings))
     ;; -> (letrec ((NAME (lambda (SYM...) BODY...))) (NAME VAL...))
     (retrans `(letrec ((,name (lambda ,(map car bindings) ,@body)))
                 (,name ,@(map cadr bindings)))))

    ;; (let () BODY...)
    ((() . ,body)
     ;; Note: this differs from `begin'
     (make-ghil-begin e l (list (trans-body e l body))))
    
    ;; (let ((SYM VAL) ...) BODY...)
    ((,bindings . ,body) (guard (valid-bindings? bindings))
     (let ((vals (map retrans (map cadr bindings))))
       (call-with-ghil-bindings e (map car bindings)
         (lambda (vars)
           (make-ghil-bind e l vars vals (trans-body e l body)))))))

   (let*
    ;; (let* ((SYM VAL) ...) BODY...)
    ((() . ,body)
     (retrans `(let () ,@body)))
    ((((,sym ,val) . ,rest) . ,body) (guard (symbol? sym))
     (retrans `(let ((,sym ,val)) (let* ,rest ,@body)))))

   (letrec
    ;; (letrec ((SYM VAL) ...) BODY...)
    ((,bindings . ,body) (guard (valid-bindings? bindings))
     (call-with-ghil-bindings e (map car bindings)
        (lambda (vars)
          (let ((vals (map retrans (map cadr bindings))))
            (make-ghil-bind e l vars vals (trans-body e l body)))))))

   (cond
    ;; (cond (CLAUSE BODY...) ...)
    (() (retrans '(begin)))
    (((else . ,body)) (retrans `(begin ,@body)))
    (((,test) . ,rest) (retrans `(or ,test (cond ,@rest))))
    (((,test => ,proc) . ,rest)
     ;; FIXME hygiene!
     (retrans `(let ((_t ,test)) (if _t (,proc _t) (cond ,@rest)))))
    (((,test . ,body) . ,rest)
     (retrans `(if ,test (begin ,@body) (cond ,@rest)))))

   (case
    ;; (case EXP ((KEY...) BODY...) ...)
    ((,exp . ,clauses)
     (retrans
      ;; FIXME hygiene!
      `(let ((_t ,exp))
         ,(let loop ((ls clauses))
            (cond ((null? ls) '(begin))
                  ((eq? (caar ls) 'else) `(begin ,@(cdar ls)))
                  (else `(if (memv _t ',(caar ls))
                             (begin ,@(cdar ls))
                             ,(loop (cdr ls))))))))))

    (do
     ;; (do ((SYM VAL [UPDATE]) ...) (TEST RESULT...) BODY...)
     ((,bindings (,test . ,result) . ,body)
      (let ((sym (map car bindings))
            (val (map cadr bindings))
            (update (map cddr bindings)))
        (define (next s x) (if (pair? x) (car x) s))
        (retrans
         ;; FIXME hygiene!
         `(letrec ((_l (lambda ,sym
                         (if ,test
                             (begin ,@result)
                             (begin ,@body
                                    (_l ,@(map next sym update)))))))
            (_l ,@val))))))

    (lambda
     ;; (lambda FORMALS BODY...)
     ((,formals . ,body)
      (receive (syms rest) (parse-formals formals)
        (call-with-ghil-environment e syms
       (lambda (env vars)
            (make-ghil-lambda env l vars rest (trans-body env l body)))))))

    (eval-case
     (,clauses
      (retrans
       `(begin
          ,@(let ((toplevel? (ghil-env-toplevel? e)))
              (let loop ((seen '()) (in clauses) (runtime '()))
                (cond
                 ((null? in) runtime)
                 (else
                  (pmatch (car in)
                   ((else . ,body)
                    (if (and toplevel? (not (memq *the-compile-toplevel-symbol* seen)))
                        (primitive-eval `(begin ,@body)))
                    (if (memq (if toplevel? *the-compile-toplevel-symbol* 'evaluate) seen)
                        runtime
                        body))
                   ((,keys . ,body) (guard (list? keys) (and-map symbol? keys))
                    (for-each (lambda (k)
                                (if (memq k seen)
                                    (syntax-error l "eval-case condition seen twice" k)))
                              keys)
                    (if (and toplevel? (memq *the-compile-toplevel-symbol* keys))
                        (primitive-eval `(begin ,@body)))
                    (loop (append keys seen)
                          (cdr in)
                          (if (memq (if toplevel? 'load-toplevel 'evaluate) keys)
                              (append runtime body)
                              runtime)))
                   (else (syntax-error l "bad eval-case clause" (car in))))))))))))))

(define (trans-quasiquote e l x)
  (cond ((not (pair? x)) x)
	((memq (car x) '(unquote unquote-splicing))
	 (let ((l (location x)))
	   (pmatch (cdr x)
	     ((,obj)
	      (if (eq? (car x) 'unquote)
		  (make-ghil-unquote e l (trans e l obj))
		  (make-ghil-unquote-splicing e l (trans e l obj))))
	     (else (syntax-error l (format #f "bad ~A" (car x)) x)))))
	(else (cons (trans-quasiquote e l (car x))
		    (trans-quasiquote e l (cdr x))))))

(define (trans-body e l body)
  (define (define->binding df)
    (pmatch (cdr df)
      ((,name ,val) (guard (symbol? name)) (list name val))
      (((,name . ,formals) . ,body) (guard (symbol? name))
       (list name `(lambda ,formals ,@body)))
      (else (syntax-error (location df) "bad define" df))))
  ;; main
  (let loop ((ls body) (ds '()))
    (pmatch ls
      (() (syntax-error l "bad body" body))
      (((define . _) . _)
       (loop (cdr ls) (cons (car ls) ds)))
      (else
       (if (null? ds)
           (trans e l `(begin ,@ls))
           (trans e l `(letrec ,(map define->binding ds) ,@ls)))))))

(define (parse-formals formals)
  (cond
   ;; (lambda x ...)
   ((symbol? formals) (values (list formals) #t))
   ;; (lambda (x y z) ...)
   ((list? formals) (values formals #f))
   ;; (lambda (x y . z) ...)
   ((pair? formals)
    (let loop ((l formals) (v '()))
      (if (pair? l)
	  (loop (cdr l) (cons (car l) v))
	  (values (reverse! (cons l v)) #t))))
   (else (syntax-error (location formals) "bad formals" formals))))

(define (location x)
  (and (pair? x)
       (let ((props (source-properties x)))
	 (and (not (null? props))
	      (vector (assq-ref props 'line)
                      (assq-ref props 'column)
                      (assq-ref props 'filename))))))
