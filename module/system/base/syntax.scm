;;; Guile VM specific syntaxes and utilities

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

(define-module (system base syntax)
  :use-module (oop goops)
  :use-module (ice-9 match)
  :use-module (ice-9 receive)
  :use-module (ice-9 and-let-star)
  :export (match syntax-error and-let* receive))


;;;
;;; Keywords by `:KEYWORD'
;;;

(read-set! keywords 'prefix)


;;;
;;; Dot expansion
;;;

;; FOO.BAR -> (slot FOO 'BAR)

(define (expand-dot! x)
  (cond ((and (symbol? x) (not (eq? x '...))) (expand-symbol x))
	((pair? x)
	 (cond ((memq (car x) '(quote quasiquote)) x)
	       (else (set-car! x (expand-dot! (car x)))
		     (set-cdr! x (expand-dot! (cdr x)))
		     x)))
	(else x)))

(define (expand-symbol x)
  (let loop ((s (symbol->string x)))
    (let ((i (string-rindex s #\.)))
      (if i
	  `(slot ,(loop (substring s 0 i))
		 (quote ,(string->symbol (substring s (1+ i)))))
	  (string->symbol s)))))

(define syntax expand-dot!)
(export-syntax syntax)

;; slot accessor
(define slot (make-procedure-with-setter slot-ref slot-set!))
(export slot)


;;;
;;; Simplified define-class
;;;

;; (define-vm-class <foo> () (x 1) (y 2)) =>
;; 
;;   (define-class <foo> ()
;;     (a :init-keyword :a :init-form 1)
;;     (b :init-keyword :b :init-form 2))

(define-macro (define-vm-class name supers . rest)
  `(define-class ,name ,supers
     ,@(map (lambda (def)
	      (if (not (pair? def)) (set! def (list def)))
	      (let ((name (car def)) (rest (cdr def)))
		(cons* name :init-keyword (symbol->keyword name)
		       (if (or (null? rest) (keyword? (car rest)))
			   rest
			   (cons :init-form rest)))))
	    rest)))

(export-syntax define-vm-class)

;;;
;;; Other utilities
;;;

(define-public (list-fold f d l)
  (if (null? l)
      d
      (list-fold f (f (car l) d) (cdr l))))
