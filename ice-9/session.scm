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


(define-module (ice-9 session))



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
  (let ((match (make-regexp rgx))
	(modules (cons (current-module)
		       (module-uses (current-module))))
	(recorded (make-vector 61 '()))
	(vars (cons '() '())))
    (let ((last vars))
      (for-each
       (lambda (module)
	 (for-each
	  (lambda (obarray)
	    (array-for-each
	     (lambda (oblist)
	       (for-each
		(lambda (x)
		  (if (and (regexp-exec match (car x))
			   (not (hashq-get-handle recorded (car x))))
		      (begin
			(set-cdr! last (cons (car x) '()))
			(set! last (cdr last))
			(hashq-set! recorded (car x) #t))))
		oblist))
	     obarray))
	  (if (or (eq? module the-scm-module)
		  (eq? module the-root-module))
	      (list (builtin-weak-bindings)
		    (builtin-bindings))
	      (list (module-obarray module)))))
       modules))
    (cdr vars)))
