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
  :use-module (oop goops)
  :use-syntax (system base syntax)
  :use-module (system base module)
  :use-module (ice-9 match)
  :use-module (ice-9 regex)
  :export
  (parse-ghil
   make-<ghil-void> <ghil-void>?
   make-<ghil-quote> <ghil-quote>? <ghil-quote>-1
   make-<ghil-ref> <ghil-ref>? <ghil-ref>-1 <ghil-ref>-2
   make-<ghil-set> <ghil-set>? <ghil-set>-1 <ghil-set>-2 <ghil-set>-3
   make-<ghil-if> <ghil-if>? <ghil-if>-1 <ghil-if>-2 <ghil-if>-3
   make-<ghil-begin> <ghil-begin>? <ghil-begin>-1
   make-<ghil-bind> <ghil-bind>?
   <ghil-bind>-1 <ghil-bind>-2 <ghil-bind>-3 <ghil-bind>-4
   make-<ghil-lambda> <ghil-lambda>?
   <ghil-lambda>-1 <ghil-lambda>-2 <ghil-lambda>-3 <ghil-lambda>-4
   make-<ghil-call> <ghil-call>? <ghil-call>-1 <ghil-call>-2
   make-<ghil-inst> <ghil-inst>? <ghil-inst>-1 <ghil-inst>-2
   ))


;;;
;;; Parse tree
;;;

(define-structure (<ghil-void>))
(define-structure (<ghil-quote> obj))
(define-structure (<ghil-ref> env var))
(define-structure (<ghil-set> env var val))
(define-structure (<ghil-if> test then else))
(define-structure (<ghil-begin> exps))
(define-structure (<ghil-bind> env vars vals body))
(define-structure (<ghil-lambda> env args rest body))
(define-structure (<ghil-call> proc args))
(define-structure (<ghil-inst> inst args))


;;;
;;; Variables
;;;

(define-vm-class <ghil-var> ()
  env name kind type value index)

(define (make-ghil-var env name kind)
  (make <ghil-var> :env env :name name :kind kind))


;;;
;;; Modules
;;;

(define-vm-class <ghil-mod> ()
  (module)
  (table '())
  (imports '()))

(define (make-ghil-mod module)
  (make <ghil-mod> :module module))

(define-method (ghil-lookup (mod <ghil-mod>) (sym <symbol>))
  (or (assq-ref mod.table sym)
      (let ((var (make-ghil-var (env-identifier mod.module) sym 'module)))
	(set! mod.table (acons sym var mod.table))
	var)))


;;;
;;; Environments
;;;

(define-vm-class <ghil-env> ()
  (mod)
  (parent #f)
  (table '())
  (variables '()))

(define-method (make-ghil-env (m <ghil-mod>))
  (make <ghil-env> :mod m :parent m))

(define-method (make-ghil-env (e <ghil-env>))
  (make <ghil-env> :mod e.mod :parent e))

(define-method (ghil-env-ref (env <ghil-env>) (sym <symbol>))
  (assq-ref env.table sym))

(define-method (ghil-env-add! (env <ghil-env>) (sym <symbol>) kind)
  (let ((var (make-ghil-var env sym kind)))
    (set! env.table (acons sym var env.table))
    (set! env.variables (cons var env.variables))
    var))

(define-method (ghil-env-remove! (env <ghil-env>) (sym <symbol>))
  (set! env.table (assq-remove! env.table sym)))

(define-method (ghil-lookup (env <ghil-env>) (sym <symbol>))
  (or (ghil-env-ref env sym)
      (let loop ((e env.parent))
	(cond ((is-a? e <ghil-mod>) (ghil-lookup e sym))
	      ((ghil-env-ref e sym) =>
	       (lambda (var) (set! var.kind 'external) var))
	      (else (loop e.parent))))))


;;;
;;; Parser
;;;

(define (parse-ghil x e)
  (parse `(@lambda () ,x) (make-ghil-mod e)))

(define (parse x e)
  (cond ((pair? x) (parse-pair x e))
	((symbol? x)
	 (let ((str (symbol->string x)))
	   (case (string-ref str 0)
	     ((#\@) (error "Invalid use of IL primitive" x))
	     ((#\:) (let ((sym (string->symbol (substring str 1))))
		      (make-<ghil-quote> (symbol->keyword sym))))
	     (else (make-<ghil-ref> e (ghil-lookup e x))))))
	(else (make-<ghil-quote> x))))

(define (map-parse x e)
  (map (lambda (x) (parse x e)) x))

(define *macros* (resolve-module '(system il macros)))

(define (parse-pair x e)
  (let ((head (car x)) (tail (cdr x)))
    (if (and (symbol? head) (eq? (string-ref (symbol->string head) 0) #\@))
	(if (module-defined? *macros* head)
	    (parse (apply (module-ref *macros* head) tail) e)
	    (parse-primitive head tail e))
	(make-<ghil-call> (parse head e) (map-parse tail e)))))

(define (parse-primitive prim args e)
  (case prim
    ;; (@ IDENTIFIER)
    ((@)
     (match args
       (()
	(make-<ghil-ref> e (make-ghil-var '@ '@ 'module)))
       ((identifier)
	(receive (module name) (identifier-split identifier)
	  (make-<ghil-ref> e (make-ghil-var module name 'module))))))

    ;; (@@ INST ARGS...)
    ((@@)
     (match args
       ((inst . args)
	(make-<ghil-inst> inst (map-parse args e)))))

    ;; (@void)
    ((@void)
     (match args
       (() (make-<ghil-void>))))

    ;; (@quote OBJ)
    ((@quote)
     (match args
       ((obj)
	(make-<ghil-quote> obj))))

    ;; (@define NAME VAL)
    ((@define)
     (match args
       ((name val)
	(let ((v (ghil-lookup e name)))
	  (make-<ghil-set> e v (parse val e))))))

    ;; (@set! NAME VAL)
    ((@set!)
     (match args
       ((name val)
	(let ((v (ghil-lookup e name)))
	  (make-<ghil-set> e v (parse val e))))))

    ;; (@if TEST THEN [ELSE])
    ((@if)
     (match args
       ((test then)
	(make-<ghil-if> (parse test e) (parse then e) (make-<ghil-void>)))
       ((test then else)
	(make-<ghil-if> (parse test e) (parse then e) (parse else e)))))

    ;; (@begin BODY...)
    ((@begin)
     (parse-body args e))

    ;; (@let ((SYM INIT)...) BODY...)
    ((@let)
     (match args
       ((((sym init) ...) body ...)
	(let* ((vals (map-parse init e))
	       (vars (map (lambda (s) (ghil-env-add! e s 'local)) sym))
	       (body (parse-body body e)))
	  (for-each (lambda (s) (ghil-env-remove! e s)) sym)
	  (make-<ghil-bind> e vars vals body)))))

    ;; (@letrec ((SYM INIT)...) BODY...)
    ((@letrec)
     (match args
       ((((sym init) ...) body ...)
	(let* ((vars (map (lambda (s) (ghil-env-add! e s 'local)) sym))
	       (vals (map-parse init e))
	       (body (parse-body body e)))
	  (for-each (lambda (s) (ghil-env-remove! e s)) sym)
	  (make-<ghil-bind> e vars vals body)))))

    ;; (@lambda FORMALS BODY...)
    ((@lambda)
     (match args
       ((formals . body)
	(receive (syms rest) (parse-formals formals)
	  (let* ((e (make-ghil-env e))
		 (args (map (lambda (s) (ghil-env-add! e s 'argument)) syms)))
	    (make-<ghil-lambda> e args rest (parse-body body e)))))))

    (else (error "Unknown primitive:" prim))))

(define (parse-body x e)
  (make-<ghil-begin> (map-parse x e)))

(define (parse-formals formals)
  (cond
   ;; (@lambda x ...)
   ((symbol? formals) (values (list formals) #t))
   ;; (@lambda (x y z) ...)
   ((list? formals) (values formals #f))
   ;; (@lambda (x y . z) ...)
   ((pair? formals)
    (let loop ((l formals) (v '()))
      (if (pair? l)
	  (loop (cdr l) (cons (car l) v))
	  (values (reverse! (cons l v)) #t))))
   (else (error "Invalid formals:" formals))))

(define (identifier-split identifier)
  (let ((m (string-match "::([^:]*)$" (symbol->string identifier))))
    (if m
	(values (string->symbol (match:prefix m))
		(string->symbol (match:substring m 1)))
	(values #f identifier))))
