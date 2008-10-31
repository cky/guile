;;;; 	Copyright (C) 1999, 2001, 2006 Free Software Foundation, Inc.
;;;; 
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2.1 of the License, or (at your option) any later version.
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


(define-module (oop goops compile)
  :use-module (oop goops)
  :use-module (oop goops util)
  :export (compute-cmethod compute-entry-with-cmethod
	   compile-method cmethod-code cmethod-environment)
  :no-backtrace
  )

;;;
;;; Method entries
;;;

(define code-table-lookup
  (letrec ((check-entry (lambda (entry types)
			  (if (null? types)
			      (and (not (struct? (car entry)))
				   entry)
			      (and (eq? (car entry) (car types))
				   (check-entry (cdr entry) (cdr types)))))))
    (lambda (code-table types)
      (cond ((null? code-table) #f)
	    ((check-entry (car code-table) types)
	     => (lambda (cmethod)
		  (cons (car code-table) cmethod)))
	    (else (code-table-lookup (cdr code-table) types))))))

(define (compute-entry-with-cmethod methods types)
  (or (code-table-lookup (slot-ref (car methods) 'code-table) types)
      (let* ((method (car methods))
             (cmethod (compile-method methods types))
	     (entry (append types cmethod)))
	(slot-set! method 'code-table
		   (cons entry (slot-ref method 'code-table)))
	(cons entry cmethod))))

(define (compute-cmethod methods types)
  (cdr (compute-entry-with-cmethod methods types)))

;;;
;;; Next methods
;;;

;;; Temporary solution---return #f if x doesn't refer to `next-method'.
(define (next-method? x)
  (and (pair? x)
       (or (eq? (car x) 'next-method)
	   (next-method? (car x))
	   (next-method? (cdr x)))))

(define (make-final-make-next-method method)
  (lambda default-args
    (lambda args
      (@apply method (if (null? args) default-args args)))))	  

(define (make-final-make-no-next-method gf)
  (lambda default-args
    (lambda args
      (no-next-method gf (if (null? args) default-args args)))))

;;;
;;; Method compilation
;;;

;;; So, for the reader: there basic idea is that, given that the
;;; semantics of `next-method' depend on the concrete types being
;;; dispatched, why not compile a specific procedure to handle each type
;;; combination that we see at runtime. There are two compilation
;;; strategies implemented: one for the memoizer, and one for the VM
;;; compiler.
;;;
;;; In theory we can do much better than a bytecode compilation, because
;;; we know the *exact* types of the arguments. It's ideal for native
;;; compilation. A task for the future.
;;;
;;; I think this whole generic application mess would benefit from a
;;; strict MOP.

(define (compile-method methods types)
  (if (slot-ref (car methods) 'compile-env)
      (compile-method/vm methods types)
      (compile-method/memoizer methods types)))

(define (make-next-method gf methods types)
  (if (null? methods)
      (lambda args (no-next-method gf args))
      (let ((cmethod (compute-cmethod methods types)))
        (if (pair? cmethod)
            ;; if it's a pair, the next-method is interpreted
            (local-eval (cons 'lambda (cmethod-code cmethod))
                        (cmethod-environment cmethod))
            ;; otherwise a normal procedure
            cmethod))))

(define (compile-method/vm methods types)
  (let* ((program-external (@ (system vm program) program-external))
         (formals (slot-ref (car methods) 'formals))
	 (body (slot-ref (car methods) 'body)))
    (cond
     ((not (next-method? body))
      ;; just one method to call -- in the future we could compile this
      ;; based on the types that we see, but for now just return the
      ;; method procedure (which is vm-compiled already)
      (method-procedure (car methods)))

     ;; (and-map (lambda (m) (null? (slot-ref m 'compile-env))) methods)
     ;; many methods, but with no lexical bindings: can inline, in theory.
     ;;
     ;; modules complicate this though, the different method bodies only
     ;; make sense in the contexts of their modules. so while we could
     ;; expand this to a big letrec, there wouldn't be real inlining.

     (else
      (let* ((next-method-sym (gensym " next-method"))
             (method (car methods))
             (cmethod (compile
                       `(let ((,next-method-sym #f))
                          (lambda ,formals
                            (let ((next-method
                                   (lambda args
                                     (if (null? args)
                                         ,(if (list? formals)
                                              `(,next-method-sym ,@formals)
                                              `(apply
                                                ,next-method-sym
                                                ,@(improper->proper formals)))
                                         (apply ,next-method-sym args)))))
                              ,@body)))
                       (slot-ref method 'compile-env))))
        (list-set! (program-external cmethod) 0
                   (make-next-method (method-generic-function method)
                                     (cdr methods)
                                     types))
        cmethod)))))

;;;
;;; Compiling methods for the memoizer
;;;

(define source-formals cadr)
(define source-body cddr)

(define cmethod-code cdr)
(define cmethod-environment car)

(define %tag-body
  (nested-ref the-root-module '(app modules oop goops %tag-body)))

;;; An exegetical note: the strategy here seems to be to (a) only put in
;;; next-method if it's referenced in the code; (b) memoize the lookup
;;; lazily, when `next-method' is first called.

(define (make-make-next-method/memoizer vcell gf methods types)
  (lambda default-args
    (lambda args
      (if (null? methods)
	  (begin
	    (set-cdr! vcell (make-final-make-no-next-method gf))
	    (no-next-method gf (if (null? args) default-args args)))
	  (let* ((cmethod (compute-cmethod methods types))
		 (method
                  (if (pair? cmethod)
                      (local-eval (cons 'lambda (cmethod-code cmethod))
                                  (cmethod-environment cmethod))
                      cmethod)))
	    (set-cdr! vcell (make-final-make-next-method method))
	    (@apply method (if (null? args) default-args args)))))))

(define (compile-method/memoizer methods types)
  (let* ((proc (method-procedure (car methods)))
	 ;; XXX - procedure-source can not be guaranteed to be
	 ;;       reliable or efficient
	 (src (procedure-source proc))
	 (formals (source-formals src))
	 (body (source-body src)))
    (if (next-method? body)
	(let ((vcell (cons 'goops:make-next-method #f)))
	  (set-cdr! vcell
		    (make-make-next-method/memoizer
		     vcell
		     (method-generic-function (car methods))
		     (cdr methods) types))
	  ;;*fixme*
	  `(,(cons vcell (procedure-environment proc))
	    ,formals
	    ;;*fixme* Only do this on source where next-method can't be inlined
	    (let ((next-method ,(if (list? formals)
				    `(goops:make-next-method ,@formals)
				    `(apply goops:make-next-method
					    ,@(improper->proper formals)))))
	      ,@body)))
	(cons (procedure-environment proc)
	      (cons formals
		    (%tag-body body)))
	)))
