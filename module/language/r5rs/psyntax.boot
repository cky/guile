;;; psyntax.ss -> psyntax.scm

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

(define sc-expand #f)
(define $sc-put-cte #f)
(define bound-identifier=? #f)
(define datum->syntax-object #f)
(define free-identifier=? #f)
(define generate-temporaries #f)
(define identifier? #f)
(define syntax-object->datum #f)
(define syntax-rules #f)
(define syntax-error #f)
(define $syntax-dispatch #f)

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

(define core-eval eval)
(define (eval x) (core-eval (cadr x) (interaction-environment)))

(load "psyntax.pp")

(call-with-input-file "psyntax.ss"
  (lambda (in)
    (call-with-output-file "psyntax.scm"
      (lambda (out)
	(do ((obj (read in) (read in)))
	    ((eof-object? obj))
	  (write (sc-expand obj) out))))))
