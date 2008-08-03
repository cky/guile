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
  :use-module (ice-9 regex)
  :export
  (<ghil-void> make-ghil-void ghil-void?
   ghil-void-env ghil-void-loc

   <ghil-quote> make-ghil-quote ghil-quote?
   ghil-quote-env ghil-quote-loc ghil-quote-obj

   <ghil-quasiquote> make-ghil-quasiquote ghil-quasiquote?
   ghil-quasiquote-env ghil-quasiquote-loc ghil-quasiquote-exp

   <ghil-unquote> make-ghil-unquote ghil-unquote?
   ghil-unquote-env ghil-unquote-loc ghil-unquote-exp

   <ghil-unquote-splicing> make-ghil-unquote-splicing ghil-unquote-splicing?
   ghil-unquote-env ghil-unquote-loc ghil-unquote-exp

   <ghil-ref> make-ghil-ref ghil-ref?
   ghil-ref-env ghil-ref-loc ghil-ref-var

   <ghil-set> make-ghil-set ghil-set?
   ghil-set-env ghil-set-loc ghil-set-var ghil-set-val

   <ghil-define> make-ghil-define ghil-define?
   ghil-define-env ghil-define-loc ghil-define-var ghil-define-val

   <ghil-if> make-ghil-if ghil-if?
   ghil-if-env ghil-if-loc ghil-if-test ghil-if-then ghil-if-else

   <ghil-and> make-ghil-and ghil-and?
   ghil-and-env ghil-and-loc ghil-and-exps

   <ghil-or> make-ghil-or ghil-or?
   ghil-or-env ghil-or-loc ghil-or-exps

   <ghil-begin> make-ghil-begin ghil-begin?
   ghil-begin-env ghil-begin-loc ghil-begin-exps

   <ghil-bind> make-ghil-bind ghil-bind?
   ghil-bind-env ghil-bind-loc ghil-bind-vars ghil-bind-vals ghil-bind-body

   <ghil-lambda> make-ghil-lambda ghil-lambda?
   ghil-lambda-env ghil-lambda-loc ghil-lambda-vars ghil-lambda-rest
   ghil-lambda-meta ghil-lambda-body

   <ghil-inline> make-ghil-inline ghil-inline?
   ghil-inline-env ghil-inline-loc ghil-inline-inline ghil-inline-args

   <ghil-call> make-ghil-call ghil-call?
   ghil-call-env ghil-call-loc ghil-call-proc ghil-call-args

   <ghil-var> make-ghil-var ghil-var?
   ghil-var-env ghil-var-name ghil-var-kind ghil-var-type ghil-var-value
   ghil-var-index

   <ghil-mod> make-ghil-mod ghil-mod?
   ghil-mod-module ghil-mod-table ghil-mod-imports

   <ghil-env> make-ghil-env ghil-env?
   ghil-env-mod ghil-env-parent ghil-env-table ghil-env-variables

   ghil-env-add! ghil-lookup ghil-define
   ghil-env-toplevel?
   call-with-ghil-environment call-with-ghil-bindings))


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
   (<ghil-lambda> env loc vars rest meta body)
   (<ghil-call> env loc proc args)
   (<ghil-inline> env loc inline args)))


;;;
;;; Variables
;;;

(define-record (<ghil-var> env name kind (type #f) (value #f) (index #f)))


;;;
;;; Modules
;;;

(define-record (<ghil-mod> module (table '()) (imports '())))


;;;
;;; Environments
;;;

(define-record (<ghil-env> mod parent (table '()) (variables '())))

(define %make-ghil-env make-ghil-env)
(define (make-ghil-env e)
  (record-case e
    ((<ghil-mod>) (%make-ghil-env :mod e :parent e))
    ((<ghil-env> mod) (%make-ghil-env :mod mod :parent e))))

(define (ghil-env-toplevel? e)
  (eq? (ghil-env-mod e) (ghil-env-parent e)))

(define (ghil-env-ref env sym)
  (assq-ref (ghil-env-table env) sym))

(define-macro (push! item loc)
  `(set! ,loc (cons ,item ,loc)))
(define-macro (apush! k v loc)
  `(set! ,loc (acons ,k ,v ,loc)))
(define-macro (apopq! k loc)
  `(set! ,loc (assq-remove! ,loc ,k)))

(define (ghil-env-add! env var)
  (apush! (ghil-var-name var) var (ghil-env-table env))
  (push! var (ghil-env-variables env)))

(define (ghil-env-remove! env var)
  (apopq! (ghil-var-name var) (ghil-env-table env)))


;;;
;;; Public interface
;;;

(define (fix-ghil-mod! mod for-sym)
  ;;; So, these warnings happen for all instances of define-module.
  ;;; Rather than fixing the problem, I'm going to suppress the common
  ;;; warnings.
  (if (not (eq? for-sym 'process-define-module))
      (warn "during lookup of" for-sym ":"
            (ghil-mod-module mod) "!= current" (current-module)))
  (if (not (null? (ghil-mod-table mod)))
      (warn "throwing away old variable table"
            (ghil-mod-module) (ghil-mod-table mod)))
  (set! (ghil-mod-module mod) (current-module))
  (set! (ghil-mod-table mod) '())
  (set! (ghil-mod-imports mod) '()))

;; looking up a var has side effects?
(define (ghil-lookup env sym)
  (or (ghil-env-ref env sym)
      (let loop ((e (ghil-env-parent env)))
        (record-case e
          ((<ghil-mod> module table imports)
           (cond ((not (eq? module (current-module)))
                  ;; FIXME: the primitive-eval in eval-case and/or macro
                  ;; expansion can have side effects on the compilation
                  ;; environment, for example changing the current
                  ;; module. We probably need to add a special case in
                  ;; compilation to handle define-module.
                  (fix-ghil-mod! e sym)
                  (loop e))
                 ((assq-ref table sym)) ;; when does this hit?
                 (else
                  ;; although we could bind the variable here, in
                  ;; practice further toplevel definitions in this
                  ;; compilation unit could change how we would resolve
                  ;; this binding, so punt and memoize the lookup at
                  ;; runtime always.
                  (let ((var (make-ghil-var (make-ghil-env e) sym 'module)))
                    (apush! sym var table)
                    var))))
          ((<ghil-env> mod parent table variables)
           (let ((found (assq-ref table sym)))
             (if found
                 (begin (set! (ghil-var-kind found) 'external) found)
                 (loop parent))))))))

(define (ghil-define mod sym)
  (if (not (eq? (ghil-mod-module mod) (current-module)))
      (fix-ghil-mod! mod sym))
  (or (assq-ref (ghil-mod-table mod) sym)
      (let ((var (make-ghil-var (make-ghil-env mod) sym 'module)))
        (apush! sym var (ghil-mod-table mod))
        var)))
          
(define (call-with-ghil-environment e syms func)
  (let* ((e (make-ghil-env e))
	 (vars (map (lambda (s)
		      (let ((v (make-ghil-var e s 'argument)))
			(ghil-env-add! e v) v))
		    syms)))
    (func e vars)))

(define (call-with-ghil-bindings e syms func)
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
