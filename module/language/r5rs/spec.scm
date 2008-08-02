;;; Guile R5RS

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

(define-module (language r5rs spec)
  :use-module (system base language)
  :use-module (language r5rs expand)
  :use-module (language r5rs translate)
  :export (r5rs))


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
       (cons* (symbol-append '@ head)
	      (map (lambda (b) (cons (car b) (map translate (cdr b))))
		   (car rest))
	      (map translate (cdr rest))))
      ((lambda)
       (cons* '@lambda (car rest) (map translate (cdr rest))))
      (else
       (cons (translate head) (map translate rest))))))


;;;
;;; Language definition
;;;

(define-language r5rs
  :title	"Standard Scheme (R5RS + syntax-case)"
  :version	"0.3"
  :reader	read
  :expander	expand
  :translator	translate
  :printer	write
;;  :environment	(global-ref 'Language::R5RS::core)
  )
