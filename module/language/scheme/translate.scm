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
  :use-module (system base language)
  :use-module (system il ghil)
  :use-module (ice-9 match)
  :use-module (ice-9 receive)
  :export (translate))

(define (translate x e)
  (call-with-ghil-environment (make-ghil-mod e) '()
    (lambda (env vars)
      (<ghil-lambda> env #f vars #f (trans env #f x)))))


;;;
;;; Translator
;;;

(define scheme-primitives
  '(not null? eq? eqv? equal? pair? list? cons car cdr set-car! set-cdr!))

(define (trans e l x)
  (cond ((pair? x)
	 (let ((y (macroexpand x)))
	   (if (eq? x y)
	     (trans-pair e (or (location x) l) (car x) (cdr x))
	     (trans e l y))))
	((symbol? x)
	 (let ((y (symbol-expand x)))
	   (if (symbol? y)
	     (<ghil-ref> e l (ghil-lookup e y))
	     (trans e l y))))
	(else (<ghil-quote> e l x))))

(define (symbol-expand x)
  (let loop ((s (symbol->string x)))
    (let ((i (string-rindex s #\.)))
      (if i
	(let ((sym (string->symbol (substring s (1+ i)))))
	  `(slot ,(loop (substring s 0 i)) (quote ,sym)))
	(string->symbol s)))))

(define (trans-pair e l head tail)
  (define (trans:x x) (trans e l x))
  (define (trans:pair x) (trans-pair e l (car x) (cdr x)))
  (define (trans:body body) (trans-body e l body))
  (define (make:void) (<ghil-void> e l))
  (define (bad-syntax)
    (syntax-error l (format #f "bad ~A" head) (cons head tail)))
  (case head
    ;; (void)
    ((void)
     (match tail
       (() (make:void))
       (else (bad-syntax))))

    ;; (quote OBJ)
    ((quote)
     (match tail
       ((obj) (<ghil-quote> e l obj))
       (else (bad-syntax))))

    ;; (quasiquote OBJ)
    ((quasiquote)
     (match tail
       ((obj) (<ghil-quasiquote> e l (trans-quasiquote e l obj)))
       (else (bad-syntax))))

    ((define define-private)
     (match tail
       ;; (define NAME VAL)
       (((? symbol? name) val)
	(<ghil-define> e l (ghil-lookup e name) (trans:x val)))

       ;; (define (NAME FORMALS...) BODY...)
       ((((? symbol? name) . formals) . body)
	;; -> (define NAME (lambda FORMALS BODY...))
	(let ((val (trans:x `(lambda ,formals ,@body))))
	  (<ghil-define> e l (ghil-lookup e name) val)))

       (else (bad-syntax))))

    ((set!)
     (match tail
       ;; (set! NAME VAL)
       (((? symbol? name) val)
	(<ghil-set> e l (ghil-lookup e name) (trans:x val)))

       ;; (set! (NAME ARGS...) VAL)
       ((((? symbol? name) . args) val)
	;; -> ((setter NAME) ARGS... VAL)
	(trans:pair `((setter ,name) (,@args ,val))))

       (else (bad-syntax))))

    ;; (if TEST THEN [ELSE])
    ((if)
     (match tail
       ((test then)
	(<ghil-if> e l (trans:x test) (trans:x then) (make:void)))
       ((test then else)
	(<ghil-if> e l (trans:x test) (trans:x then) (trans:x else)))
       (else (bad-syntax))))

    ;; (and EXPS...)
    ((and)
     (<ghil-and> e l (map trans:x tail)))

    ;; (or EXPS...)
    ((or)
     (<ghil-or> e l (map trans:x tail)))

    ;; (begin EXPS...)
    ((begin)
     (<ghil-begin> e l (map trans:x tail)))

    ((let)
     (match tail
       ;; (let NAME ((SYM VAL) ...) BODY...)
       (((? symbol? name) (((? symbol? sym) val) ...) body ...)
	;; -> (letrec ((NAME (lambda (SYM...) BODY...))) (NAME VAL...))
	(trans:pair `(letrec ((,name (lambda ,sym ,@body))) (,name ,@val))))

       ;; (let () BODY...)
       ((() body ...)
	;; NOTE: This differs from `begin'
	(<ghil-begin> e l (list (trans:body body))))

       ;; (let ((SYM VAL) ...) BODY...)
       (((((? symbol? sym) val) ...) body ...)
	(let ((vals (map trans:x val)))
	  (call-with-ghil-bindings e sym
	    (lambda (vars)
	      (<ghil-bind> e l vars vals (trans:body body))))))

       (else (bad-syntax))))

    ;; (let* ((SYM VAL) ...) BODY...)
    ((let*)
     (match tail
       (((def ...) body ...)
	(if (null? def)
	    (trans:pair `(let () ,@body))
	    (trans:pair `(let (,(car def)) (let* ,(cdr def) ,@body)))))
       (else (bad-syntax))))

    ;; (letrec ((SYM VAL) ...) BODY...)
    ((letrec)
     (match tail
       (((((? symbol? sym) val) ...) body ...)
	(call-with-ghil-bindings e sym
	  (lambda (vars)
	    (let ((vals (map trans:x val)))
	      (<ghil-bind> e l vars vals (trans:body body))))))
       (else (bad-syntax))))

    ;; (cond (CLAUSE BODY...) ...)
    ((cond)
     (match tail
       (() (make:void))
       ((('else . body)) (trans:body body))
       (((test) . rest) (trans:pair `(or ,test (cond ,@rest))))
       (((test '=> proc) . rest)
	(trans:pair `(let ((_t ,test)) (if _t (,proc _t) (cond ,@rest)))))
       (((test . body) . rest)
	(trans:pair `(if ,test (begin ,@body) (cond ,@rest))))
       (else (bad-syntax))))

    ;; (case EXP ((KEY...) BODY...) ...)
    ((case)
     (match tail
       ((exp . clauses)
	(trans:pair
	 `(let ((_t ,exp))
	    ,(let loop ((ls clauses))
	       (cond ((null? ls) '(void))
		     ((eq? (caar ls) 'else) `(begin ,@(cdar ls)))
		     (else `(if (memv _t ',(caar ls))
				(begin ,@(cdar ls))
				,(loop (cdr ls)))))))))
       (else (bad-syntax))))

    ;; (do ((SYM VAL [UPDATE]) ...) (TEST RESULT...) BODY...)
    ((do)
     (let ()
       (define (next s x) (if (pair? x) (car x) s))
       (match tail
	 ((((sym init . update) ...) (test . result) body ...)
	  (trans:pair
	   `(letrec ((_l (lambda ,sym
			   (if ,test
			       (let () (void) ,@result)
			       (let () (void) ,@body
				    (_l ,@(map next sym update)))))))
	      (_l ,@init))))
	 (else (bad-syntax)))))

    ;; (lambda FORMALS BODY...)
    ((lambda)
     (match tail
       ((formals body ...)
	(receive (syms rest) (parse-formals formals)
	  (call-with-ghil-environment e syms
	    (lambda (env vars)
	      (<ghil-lambda> env l vars rest (trans-body env l body))))))
       (else (bad-syntax))))

    ((eval-case)
     (let loop ((x tail))
       (match x
	 (() (make:void))
	 ((('else . body)) (trans:pair `(begin ,@body)))
	 (((((? symbol? key) ...) body ...) rest ...)
	  (if (memq 'load-toplevel key)
	      (begin
		(primitive-eval `(begin ,@(copy-tree body)))
		(trans:pair `(begin ,@body)))
	      (loop rest)))
	 (else (bad-syntax)))))

    (else
     (if (memq head scheme-primitives)
       (<ghil-inline> e l head (map trans:x tail))
       (<ghil-call> e l (trans:x head) (map trans:x tail))))))

(define (trans-quasiquote e l x)
  (cond ((not (pair? x)) x)
	((memq (car x) '(unquote unquote-splicing))
	 (let ((l (location x)))
	   (match (cdr x)
	     ((obj)
	      (if (eq? (car x) 'unquote)
		  (<ghil-unquote> e l (trans e l obj))
		  (<ghil-unquote-splicing> e l (trans e l obj))))
	     (else (syntax-error l (format #f "bad ~A" (car x)) x)))))
	(else (cons (trans-quasiquote e l (car x))
		    (trans-quasiquote e l (cdr x))))))

(define (trans-body e l body)
  (define (define->binding df)
    (match (cdr df)
      (((? symbol? name) val) (list name val))
      ((((? symbol? name) . formals) . body)
       (list name `(lambda ,formals ,@body)))
      (else (syntax-error (location df) "bad define" df))))
  ;; main
  (let loop ((ls body) (ds '()))
    (cond ((null? ls) (syntax-error l "bad body" body))
	  ((and (pair? (car ls)) (eq? (caar ls) 'define))
	   (loop (cdr ls) (cons (car ls) ds)))
	  (else
	   (if (null? ds)
	       (trans-pair e l 'begin ls)
	       (trans-pair e l 'letrec (cons (map define->binding ds) ls)))))))

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
	      (cons (assq-ref props 'line) (assq-ref props 'column))))))
