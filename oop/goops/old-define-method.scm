;;; installed-scm-file

;;;; 	Copyright (C) 2001 Free Software Foundation, Inc.
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
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;; 


(define-module (oop goops old-define-method)
  :use-module (oop goops)
  :no-backtrace
  )

(export define-method)

(define define-method
  (procedure->memoizing-macro
    (lambda (exp env)
      (let ((name (cadr exp)))
	(if (and (pair? name)
		 (eq? (car name) 'setter)
		 (pair? (cdr name))
		 (symbol? (cadr name))
		 (null? (cddr name)))
	    (let ((name (cadr name)))
	      (cond ((not (symbol? name))
		     (goops-error "bad method name: ~S" name))
		    ((defined? name env)
		     `(begin
			;; *fixme* Temporary hack for the current module system
			(if (not ,name)
			    (define-accessor ,name))
			(add-method! (setter ,name) (method ,@(cddr exp)))))
		    (else
		     `(begin
			(define-accessor ,name)
			(add-method! (setter ,name) (method ,@(cddr exp)))))))
	    (cond ((not (symbol? name))
		   (goops-error "bad method name: ~S" name))
		  ((defined? name env)
		   `(begin
		      ;; *fixme* Temporary hack for the current module system
		      (if (not ,name)
			  (define-generic ,name))
		      (add-method! ,name (method ,@(cddr exp)))))
		  (else
		   `(begin
		      (define-generic ,name)
		      (add-method! ,name (method ,@(cddr exp)))))))))))
