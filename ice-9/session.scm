;;;; 	Copyright (C) 1997 Free Software Foundation, Inc.
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;; 


(define-module (ice-9 session)
  :no-backtrace)



;;; {Apropos}
;;;
;;; Author: Roland Orre <orre@nada.kth.se>
;;;

(define (id x) x)

(define-public (apropos rgx . options)
  "Search for bindings: apropos regexp {options= 'full 'shadow 'value}"
  (if (zero? (string-length rgx))
      "Empty string not allowed"
      (let* ((match (make-regexp rgx))
	     (modules (cons (current-module)
			    (module-uses (current-module))))
	     (separator #\tab)
	     (shadow (member 'shadow options))
	     (value (member 'value options)))
	(cond ((member 'full options)
	       (set! shadow #t)
	       (set! value #t)))
	(for-each
	 (lambda (module)
	   (let* ((builtin (or (eq? module the-scm-module)
			       (eq? module the-root-module)))
		  (name (module-name module))
		  (obarrays (if builtin
				(list (builtin-weak-bindings)
				      (builtin-bindings))
				(list (module-obarray module))))
		  (get-refs (if builtin
				(list id id)
				(list variable-ref)))
		  )
	     (for-each
	      (lambda (obarray get-ref)
		(array-for-each
		 (lambda (oblist)
		   (for-each
		    (lambda (x)
		      (cond ((regexp-exec match (car x))
			     (display name)
			     (display ": ")
			     (display (car x))
			     (cond ((procedure? (get-ref (cdr x)))
				    (display separator)
				    (display (get-ref (cdr x))))
				   (value
				    (display separator)
				    (display (get-ref (cdr x)))))
			     (if (and shadow
				      (not (eq? (module-ref module
							    (car x))
						(module-ref (current-module)
							    (car x)))))
				 (display " shadowed"))
			     (newline)
			     )))
		    oblist))
		 obarray))
	      obarrays get-refs)))
	 modules))))

(define-public (apropos-internal rgx)
  "Return a list of accessible variable names."
  (letrec ((match (make-regexp rgx))
	   (recorded (make-vector 61 '()))
	   (obarray-names
	    (lambda (obarray names)
	      (hash-fold obarray
			 (lambda (name var vars)
			   (if (and (regexp-exec match name)
				    (not (hashq-get-handle recorded name)))
			       (begin
				 (hashq-set! recorded name #t)
				 (cons name vars))
			       vars))
			 names))))
    (do ((modules (cons (current-module) (module-uses (current-module)))
		  (cdr modules))
	 (names '()
		(if (or (eq? (car modules) the-scm-module)
			(eq? (car modules) the-root-module))
		    (obarray-names (builtin-weak-bindings)
				   (obarray-names (builtin-bindings)
						  names))
		    (obarray-names (module-obarray (car modules))
				   names))))
	((null? modules) names))))

(define-public (name obj)
  (cond ((procedure? obj) (procedure-name obj))
	((macro? obj) (macro-name obj))
	(else #f)))

(define-public (source obj)
  (cond ((procedure? obj) (procedure-source obj))
	((macro? obj) (procedure-source (macro-transformer obj)))
	(else #f)))

(define-public (arity obj)
  (let ((arity (procedure-property obj 'arity)))
    (display (car arity))
    (cond ((caddr arity)
	   (display " or more"))
	  ((not (zero? (cadr arity)))
	   (display " required and ")
	   (display (cadr arity))
	   (display " optional")))
    (if (and (not (caddr arity))
	     (= (car arity) 1)
	     (<= (cadr arity) 1))
	(display " argument")
	(display " arguments"))
    (if (closure? obj)
	(let ((formals (cadr (procedure-source obj))))
	  (if (pair? formals)
	      (begin
		(display ": `")
		(display (car formals))
		(let loop ((ls (cdr formals)))
		  (cond ((null? ls)
			 (display #\'))
			((not (pair? ls))
			 (display "', the rest in `")
			 (display ls)
			 (display #\'))
			(else
			 (if (pair? (cdr ls))
			     (display "', `")
			     (display "' and `"))
			 (display (car ls))
			 (loop (cdr ls))))))
	      (begin
		(display " in `")
		(display formals)
		(display #\')))))
    (display ".\n")))
