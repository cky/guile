;;; translate.scm --- Scheme to Guile IL translator

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; This file is part of Guile VM.

;; Guile VM is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; Guile VM is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with Guile VM; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(define-module (language r5rs translate)
  :export (translate))

(define (translate x) (trans x))

(define (trans x) (if (pair? x) (trans-pair x) x))

(define *primitive-procedure-list*
  '(void car cdr cons + - * / < >))

(define (trans-pair x)
  (let ((name (car x)) (args (cdr x)))
    (let ((il (case name
		((quote) (cons '@quote args))
		((define set! if and or begin)
		 (cons (symbol-append '@ name) (map trans args)))
		((let let* letrec)
		 (cons* (symbol-append '@ name)
			(map (lambda (b)
			       (cons (car b) (map trans (cdr b))))
			     (car args))
			(map trans (cdr args))))
		((lambda)
		 (cons* '@lambda (trans-formals (car args))
			(map trans (cdr args))))
		(else
		 (if (memq name *primitive-procedure-list*)
		     ;; FIXME: Temporary hack for direct optimization
		     (cons (symbol-append '@ name) (map trans args))
		     (cons (trans name) (map trans args))))))
	  (props (source-properties x)))
      (if (not (null? props))
	  (set-source-properties! il props))
      il)))

(define (trans-formals formals)
  (cond ((symbol? formals) `(:rest ,formals))
	((or (null? formals) (null? (cdr (last-pair formals)))) formals)
	(else
	 (let* ((list (list-copy formals))
		(last (last-pair list)))
	   (set-cdr! last `(:rest ,(cdr last)))
	   list))))

;;; translate.scm ends here
