;;;; 	Copyright (C) 1997, 2000 Free Software Foundation, Inc.
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


(define-module (language r5rs expand)
  :export (expand void
	   identifier? free-identifier=? bound-identifier=?
	   generate-temporaries datum->syntax-object syntax-object->datum))

(define sc-expand #f)
(define $sc-put-cte #f)
(define $syntax-dispatch #f)
(define syntax-rules #f)
(define syntax-error #f)
(define identifier? #f)
(define free-identifier=? #f)
(define bound-identifier=? #f)
(define generate-temporaries #f)
(define datum->syntax-object #f)
(define syntax-object->datum #f)

(define void (lambda () (if #f #f)))

(define andmap
  (lambda (f first . rest)
    (or (null? first)
	(if (null? rest)
	    (let andmap ((first first))
	      (let ((x (car first)) (first (cdr first)))
		(if (null? first)
		    (f x)
		    (and (f x) (andmap first)))))
	    (let andmap ((first first) (rest rest))
	      (let ((x (car first))
		    (xr (map car rest))
		    (first (cdr first))
		    (rest (map cdr rest)))
		(if (null? first)
		    (apply f (cons x xr))
		    (and (apply f (cons x xr)) (andmap first rest)))))))))

(define ormap
  (lambda (proc list1)
    (and (not (null? list1))
	 (or (proc (car list1)) (ormap proc (cdr list1))))))

(define putprop set-symbol-property!)
(define getprop symbol-property)
(define remprop symbol-property-remove!)

(define syncase-module (current-module))
(define (sc-eval x) (eval x syncase-module))

(load "psyntax.scm")

(define expand sc-expand)

(define (rebuild)
  (call-with-input-file "psyntax.ss"
    (lambda (in)
      (call-with-output-file "psyntax.scm"
	(lambda (out)
	  (do ((obj (read in) (read in)))
	      ((eof-object? obj))
	    (write (sc-expand obj 'c '(eval load compile)) out)))))))

;(rebuild)
