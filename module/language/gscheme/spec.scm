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
  :use-module (ice-9 match)
  :export (gscheme))


;;;
;;; Macro expander
;;;

(define (expand x)
  (if (pair? x)
      (let* ((s (car x))
	     (m (current-module))
	     (v (and (symbol? s) (module-defined? m s) (module-ref m s))))
	(if (defmacro? v)
	    (expand (apply (defmacro-transformer v) (cdr x)))
	    (cons (expand (car x)) (expand (cdr x)))))
      x))


;;;
;;; Translator
;;;

(define *primitive-procedure-list*
  '(void car cdr cons + - < >))

(define (translate x) (if (pair? x) (translate-pair x) x))

(define (translate-pair x)
  (let ((name (car x)) (args (cdr x)))
    (case name
      ((quote) (cons '@quote args))
      ((set! if and or begin)
       (cons (symbol-append '@ name) (map translate args)))
      ((define)
       (if (pair? (car args))
	   `(@define ,(caar args)
		     (@lambda ,(cdar args) ,@(map translate (cdr args))))
	   `(@define ,(car args) ,@(map translate (cdr args)))))
      ((let let* letrec)
       (match x
	 (('let (? symbol? f) ((s v) ...) body ...)
	  `(@letrec ((,f (@lambda ,s ,@(map translate body))))
		    (,f ,@(map translate v))))
	 (else
	  (cons* (symbol-append '@ name)
		 (map (lambda (b) (cons (car b) (map translate (cdr b))))
		      (car args))
		 (map translate (cdr args))))))
      ((lambda)
       (cons* '@lambda (car args) (map translate (cdr args))))
      (else
       (if (memq name *primitive-procedure-list*)
	   (cons (symbol-append '@ name) (map translate args))
	   (cons (translate name) (map translate args)))))))


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
