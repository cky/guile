;;; Guile High Intermediate Language

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

(define-module (system il ghil)
  :use-syntax (system base syntax)
  :use-module (ice-9 match)
  :use-module (ice-9 regex)
  :export
  (
   <ghil-void> <ghil-void>? <ghil-void>-1 <ghil-void>-2
   <ghil-quote> <ghil-quote>? <ghil-quote>-1 <ghil-quote>-2 <ghil-quote>-3
   <ghil-quasiquote> <ghil-quasiquote>?
   <ghil-quasiquote>-1 <ghil-quasiquote>-2 <ghil-quasiquote>-3
   <ghil-unquote> <ghil-unquote>?
   <ghil-unquote>-1 <ghil-unquote>-2 <ghil-unquote>-3
   <ghil-unquote-splicing> <ghil-unquote-splicing>?
   <ghil-unquote-splicing>-1 <ghil-unquote-splicing>-2
   <ghil-unquote-splicing>-3

   <ghil-ref> <ghil-ref>? <ghil-ref>-1 <ghil-ref>-2 <ghil-ref>-3
   <ghil-set> <ghil-set>? <ghil-set>-1 <ghil-set>-2 <ghil-set>-3 <ghil-set>-4
   <ghil-define> <ghil-define>?
   <ghil-define>-1 <ghil-define>-2 <ghil-define>-3 <ghil-define>-4

   <ghil-if> <ghil-if>?
   <ghil-if>-1 <ghil-if>-2 <ghil-if>-3 <ghil-if>-4 <ghil-if>-5
   <ghil-and> <ghil-and>? <ghil-and>-1 <ghil-and>-2 <ghil-and>-3
   <ghil-or> <ghil-or>? <ghil-or>-1 <ghil-or>-2 <ghil-or>-3
   <ghil-begin> <ghil-begin>? <ghil-begin>-1 <ghil-begin>-2 <ghil-begin>-3
   <ghil-bind> <ghil-bind>?
   <ghil-bind>-1 <ghil-bind>-2 <ghil-bind>-3 <ghil-bind>-4 <ghil-bind>-5
   <ghil-lambda> <ghil-lambda>? <ghil-lambda>-1 <ghil-lambda>-2
   <ghil-lambda>-3 <ghil-lambda>-4 <ghil-lambda>-5
   <ghil-inline> <ghil-inline>?
   <ghil-inline>-1 <ghil-inline>-2 <ghil-inline>-3 <ghil-inline>-4
   <ghil-call> <ghil-call>?
   <ghil-call>-1 <ghil-call>-2 <ghil-call>-3 <ghil-call>-4
   ))


;;;
;;; Parse tree
;;;

(define-type <ghil>
  (|
   ;; Objects
   (<ghil-void> env loc)
   (<ghil-quote> env loc obj)
   (<ghil-quasiquote> env loc exp)
   (<ghil-unquote> env loc exp)
   (<ghil-unquote-splicing> env loc exp)
   ;; Variables
   (<ghil-ref> env loc var)
   (<ghil-set> env loc var val)
   (<ghil-define> env loc var val)
   ;; Controls
   (<ghil-if> env loc test then else)
   (<ghil-and> env loc exps)
   (<ghil-or> env loc exps)
   (<ghil-begin> env loc exps)
   (<ghil-bind> env loc vars vals body)
   (<ghil-lambda> env loc vars rest body)
   (<ghil-call> env loc proc args)
   (<ghil-inline> env loc inline args)))

(define-public ghil-env %slot-1)
(define-public ghil-loc %slot-2)


;;;
;;; Procedures
;;;

(define *core-primitives*
  '(@void @quote @define @set! @if @begin @let @letrec @lambda))

(define *macro-module* (resolve-module '(system il macros)))

(define-public (ghil-primitive-macro? x)
  (and (module-defined? *macro-module* x)
       (procedure? (module-ref *macro-module* x))))

(define (ghil-macro-expander x)
  (module-ref *macro-module* x))

(define (ghil-primitive? x)
  (or (memq x *core-primitives*)
      (ghil-primitive-macro? x)))


;;;
;;; Variables
;;;

(define-record (<ghil-var> env name kind (type #f) (value #f) (index #f)))

(define-public (make-ghil-var env name kind)
  (<ghil-var> :env env :name name :kind kind))


;;;
;;; Modules
;;;

(define-record (<ghil-mod> module (table '()) (imports '())))

(define-public (make-ghil-mod module)
  (<ghil-mod> :module module))


;;;
;;; Environments
;;;

(define-record (<ghil-env> mod parent (table '()) (variables '())))

(define-public (make-ghil-env e)
  (match e
    (($ <ghil-mod>) (<ghil-env> :mod e :parent e))
    (($ <ghil-env> m) (<ghil-env> :mod m :parent e))))

(define (ghil-env-toplevel? e)
  (eq? e.mod e.parent))

(define (ghil-env-ref env sym)
  (assq-ref env.table sym))

(define-public (ghil-env-add! env var)
  (set! env.table (acons var.name var env.table))
  (set! env.variables (cons var env.variables)))

(define (ghil-env-remove! env var)
  (set! env.table (assq-remove! env.table var.name)))


;;;
;;; Public interface
;;;

(define-public (ghil-lookup env sym)
  (or (ghil-env-ref env sym)
      (let loop ((e env.parent))
	(cond ((<ghil-mod>? e)
	       (or (assq-ref e.table sym)
		   (let ((var (make-ghil-var #f sym 'module)))
		     (set! e.table (acons sym var e.table))
		     var)))
	      ((ghil-env-ref e sym) =>
	       (lambda (var) (set! var.kind 'external) var))
	      (else (loop e.parent))))))

(define-public (call-with-ghil-environment e syms func)
  (let* ((e (make-ghil-env e))
	 (vars (map (lambda (s)
		      (let ((v (make-ghil-var e s 'argument)))
			(ghil-env-add! e v) v))
		    syms)))
    (func e vars)))

(define-public (call-with-ghil-bindings e syms func)
  (let* ((vars (map (lambda (s)
		      (let ((v (make-ghil-var e s 'local)))
			(ghil-env-add! e v) v))
		    syms))
	 (ret (func vars)))
    (for-each (lambda (v) (ghil-env-remove! e v)) vars)
    ret))


;;;
;;; Parser
;;;

;;; (define-public (parse-ghil x e)
;;;   (parse `(@lambda () ,x) (make-ghil-mod e)))
;;; 
;;; (define (parse x e)
;;;   (cond ((pair? x) (parse-pair x e))
;;; 	((symbol? x)
;;; 	 (let ((str (symbol->string x)))
;;; 	   (case (string-ref str 0)
;;; 	     ((#\@) (error "Invalid use of IL primitive" x))
;;; 	     ((#\:) (let ((sym (string->symbol (substring str 1))))
;;; 		      (<ghil-quote> (symbol->keyword sym))))
;;; 	     (else (<ghil-ref> e (ghil-lookup e x))))))
;;; 	(else (<ghil-quote> x))))
;;; 
;;; (define (map-parse x e)
;;;   (map (lambda (x) (parse x e)) x))
;;; 
;;; (define (parse-pair x e)
;;;   (let ((head (car x)) (tail (cdr x)))
;;;     (if (and (symbol? head) (eq? (string-ref (symbol->string head) 0) #\@))
;;; 	(if (ghil-primitive-macro? head)
;;; 	    (parse (apply (ghil-macro-expander head) tail) e)
;;; 	    (parse-primitive head tail e))
;;; 	(<ghil-call> e (parse head e) (map-parse tail e)))))
;;; 
;;; (define (parse-primitive prim args e)
;;;   (case prim
;;;     ;; (@ IDENTIFIER)
;;;     ((@)
;;;      (match args
;;;        (()
;;; 	(<ghil-ref> e (make-ghil-var '@ '@ 'module)))
;;;        ((identifier)
;;; 	(receive (module name) (identifier-split identifier)
;;; 	  (<ghil-ref> e (make-ghil-var module name 'module))))))
;;; 
;;;     ;; (@@ OP ARGS...)
;;;     ((@@)
;;;      (match args
;;;        ((op . args)
;;; 	(<ghil-inline> op (map-parse args e)))))
;;; 
;;;     ;; (@void)
;;;     ((@void)
;;;      (match args
;;;        (() (<ghil-void>))))
;;; 
;;;     ;; (@quote OBJ)
;;;     ((@quote)
;;;      (match args
;;;        ((obj)
;;; 	(<ghil-quote> obj))))
;;; 
;;;     ;; (@define NAME VAL)
;;;     ((@define)
;;;      (match args
;;;        ((name val)
;;; 	(let ((v (ghil-lookup e name)))
;;; 	  (<ghil-set> e v (parse val e))))))
;;; 
;;;     ;; (@set! NAME VAL)
;;;     ((@set!)
;;;      (match args
;;;        ((name val)
;;; 	(let ((v (ghil-lookup e name)))
;;; 	  (<ghil-set> e v (parse val e))))))
;;; 
;;;     ;; (@if TEST THEN [ELSE])
;;;     ((@if)
;;;      (match args
;;;        ((test then)
;;; 	(<ghil-if> (parse test e) (parse then e) (<ghil-void>)))
;;;        ((test then else)
;;; 	(<ghil-if> (parse test e) (parse then e) (parse else e)))))
;;; 
;;;     ;; (@begin BODY...)
;;;     ((@begin)
;;;      (parse-body args e))
;;; 
;;;     ;; (@let ((SYM INIT)...) BODY...)
;;;     ((@let)
;;;      (match args
;;;        ((((sym init) ...) body ...)
;;; 	(let* ((vals (map-parse init e))
;;; 	       (vars (map (lambda (s)
;;; 			    (let ((v (make-ghil-var e s 'local)))
;;; 			      (ghil-env-add! e v) v))
;;; 			  sym))
;;; 	       (body (parse-body body e)))
;;; 	  (for-each (lambda (v) (ghil-env-remove! e v)) vars)
;;; 	  (<ghil-bind> e vars vals body)))))
;;; 
;;;     ;; (@letrec ((SYM INIT)...) BODY...)
;;;     ((@letrec)
;;;      (match args
;;;        ((((sym init) ...) body ...)
;;; 	(let* ((vars (map (lambda (s)
;;; 			    (let ((v (make-ghil-var e s 'local)))
;;; 			      (ghil-env-add! e v) v))
;;; 			  sym))
;;; 	       (vals (map-parse init e))
;;; 	       (body (parse-body body e)))
;;; 	  (for-each (lambda (v) (ghil-env-remove! e v)) vars)
;;; 	  (<ghil-bind> e vars vals body)))))
;;; 
;;;     ;; (@lambda FORMALS BODY...)
;;;     ((@lambda)
;;;      (match args
;;;        ((formals . body)
;;; 	(receive (syms rest) (parse-formals formals)
;;; 	  (let* ((e (make-ghil-env e))
;;; 		 (vars (map (lambda (s)
;;; 			      (let ((v (make-ghil-var e s 'argument)))
;;; 				(ghil-env-add! e v) v))
;;; 			    syms)))
;;; 	    (<ghil-lambda> e vars rest (parse-body body e)))))))
;;; 
;;;     ;; (@eval-case CLAUSE...)
;;;     ((@eval-case)
;;;      (let loop ((clauses args))
;;;        (cond ((null? clauses) (<ghil-void>))
;;; 	     ((or (eq? (caar clauses) '@else)
;;; 		  (and (memq 'load-toplevel (caar clauses))
;;; 		       (ghil-env-toplevel? e)))
;;; 	      (parse-body (cdar clauses) e))
;;; 	     (else
;;; 	      (loop (cdr clauses))))))
;;; 
;;;     (else (error "Unknown primitive:" prim))))
;;; 
;;; (define (parse-body x e)
;;;   (<ghil-begin> (map-parse x e)))
;;; 
;;; (define (parse-formals formals)
;;;   (cond
;;;    ;; (@lambda x ...)
;;;    ((symbol? formals) (values (list formals) #t))
;;;    ;; (@lambda (x y z) ...)
;;;    ((list? formals) (values formals #f))
;;;    ;; (@lambda (x y . z) ...)
;;;    ((pair? formals)
;;;     (let loop ((l formals) (v '()))
;;;       (if (pair? l)
;;; 	  (loop (cdr l) (cons (car l) v))
;;; 	  (values (reverse! (cons l v)) #t))))
;;;    (else (error "Invalid formals:" formals))))
;;; 
;;; (define (identifier-split identifier)
;;;   (let ((m (string-match "::([^:]*)$" (symbol->string identifier))))
;;;     (if m
;;; 	(values (string->symbol (match:prefix m))
;;; 		(string->symbol (match:substring m 1)))
;;; 	(values #f identifier))))
