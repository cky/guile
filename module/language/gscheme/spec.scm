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
  :use-module (ice-9 match)
  :use-module (ice-9 and-let-star)
  :export (gscheme))


;;;
;;; Translator
;;;

(define (translate x) (if (pair? x) (translate-pair x) x))

(define (translate-pair x)
  (let ((head (car x)) (rest (cdr x)))
    (case head
      ((quote) `(@quote ,@rest))
      ((set! if and or begin)
       (cons (symbol-append '@ head) (map translate rest)))
      ((define)
       (match rest
	 ((((? symbol? name) . args) . body)
	  `(@define ,name (@lambda ,args ,@(map translate body))))
	 (((? symbol? name) val)
	  `(@define ,name ,(translate val)))
	 (else (error "Syntax error:" x))))
      ((lambda)
       `(@lambda ,(car rest) ,@(map translate (cdr rest))))
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
      ((cond)
       (let loop ((x rest))
	 (match x
	   (() '(@void))
	   ((('else . body)) `(@begin ,@(map translate body)))
	   (((test) . rest) `(@or ,(translate test) ,(loop rest)))
	   (((test '=> proc) . rest)
	    `(@let ((_t ,(translate test)))
		   (@if _t (,(translate proc) _t) ,(loop rest))))
	   (((test . body) . rest)
	    `(@if ,(translate test)
		  (@begin ,@(map translate body))
		  ,(loop rest)))
	   (else (error "bad cond" x)))))
      ((case)
       `(@let ((_t ,(translate (car rest))))
	      ,(let loop ((x (cdr rest)))
		 (match x
		   (() '(@void))
		   ((('else . body)) `(@begin ,@(map translate body)))
		   ((((keys ...) . body) . rest)
		    `(@if (@memv _t (@quote ,keys))
			  (@begin ,@(map translate body))
			  ,(loop rest)))
		   (else (error "bad cond" x))))))
      ((do)
       (match rest
	 ((((sym init . update) ...) (test . result) body ...)
	  (define (translate-update s x)
	    (if (pair? x) (translate (car x)) s))
	  `(@letrec ((_loop (@lambda
			     ,sym
			     (@if ,(translate test)
				  (@begin ,@(map translate result))
				  (@begin ,@(map translate body)
					  (_loop ,@(map translate-update
							sym update)))))))
		    (_loop ,@(map translate init))))))

      ((eval-case)
       `(@eval-case
	 ,@(let loop ((x rest))
	     (match x
	       (() '(()))
	       ((('else . body)) `((@else ,@(map translate body))))
	       (((keys . body) . rest)
		`((,keys ,@(map translate body)) ,@(loop rest)))
	       (else (error "bad eval-case" x))))))

      (else
       (let ((e (expand x)))
	 (if (eq? e x)
	     (let ((prim (and (symbol? head) (symbol-append '@ head))))
	       (if (and prim (ghil-primitive? prim))
		   (cons prim (map translate rest))
		   (cons (translate head) (map translate rest))))
	     (translate e)))))))

(define (expand x)
  (if (and (symbol? (car x))
	   (module-defined? (current-module) (car x)))
      (let ((v (module-ref (current-module) (car x))))
	(if (defmacro? v)
	    (apply (defmacro-transformer v) (cdr x))
	    x))
      x))


;;;
;;; Language definition
;;;

(define-language gscheme
  :title	"Guile Scheme"
  :version	"0.4"
  :reader	read
  :translator	translate
  :printer	write
  )
