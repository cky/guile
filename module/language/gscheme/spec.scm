;;; Guile Scheme specification

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

(define-module (language gscheme spec)
  :use-module (system base language)
  :use-module (system il ghil)
  :use-module (language r5rs expand)
  :use-module (ice-9 match)
  :use-module (ice-9 and-let-star)
  :export (gscheme))


;;;
;;; Macro expander
;;;

(define expand-syntax expand)

(define (expand-macro x m)
  (if (pair? x)
      (let* ((s (car x))
	     (v (and (symbol? s) (module-defined? m s) (module-ref m s))))
	(if (defmacro? v)
	    (expand-macro (apply (defmacro-transformer v) (cdr x)) m)
	    (cons (expand-macro (car x) m) (expand-macro (cdr x) m))))
      x))

(define (expand x)
  (expand-syntax (expand-macro x (current-module))))


;;;
;;; Translator
;;;

(define (translate x) (if (pair? x) (translate-pair x) x))

(define (translate-pair x)
  (let ((head (car x)) (rest (cdr x)))
    (case head
      ((quote) (cons '@quote rest))
      ((define set! if and or begin)
       (cons (symbol-append '@ head) (map translate rest)))
      ((let let* letrec)
       (match x
	 (('let (? symbol? f) ((s v) ...) body ...)
	  `(@letrec ((,f (@lambda ,s ,@(map translate body))))
		    (,f ,@(map translate v))))
	 (else
	  (cons* (symbol-append '@ head)
		 (map (lambda (b) (cons (car b) (map translate (cdr b))))
		      (car rest))
		 (map translate (cdr rest))))))
      ((lambda)
       (cons* '@lambda (car rest) (map translate (cdr rest))))
      (else
       (let ((prim (and (symbol? head) (symbol-append '@ head))))
	 (if (and prim (ghil-primitive? prim))
	     (cons prim (map translate rest))
	     (cons (translate head) (map translate rest))))))))


;;;
;;; Language definition
;;;

(define-language gscheme
  :title	"Guile Scheme"
  :version	"0.3"
  :reader	read
  :expander	expand
  :translator	translate
  :printer	write
  )
