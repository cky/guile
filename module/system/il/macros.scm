;;; GHIL macros

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

(define-module (system il macros)
  :use-module (ice-9 match))

(define (make-label) (gensym ":L"))
(define (make-sym) (gensym "_"))


;;;
;;; Syntax
;;;

;; (@and X Y...) =>
;; 
;; (@if X (@and Y...) #f)
(define @and
  (match-lambda*
    (() #t)
    ((x) x)
    ((x . rest) `(@if ,x (@and ,@rest) #f))))

;; (@or X Y...) =>
;; 
;; (@let ((@_ X)) (@if @_ @_ (@or Y...)))
(define @or
  (match-lambda*
    (() #f)
    ((x) x)
    ((x . rest)
     (let ((sym (make-sym)))
       `(@let ((,sym ,x)) (@if ,sym ,sym (@or ,@rest)))))))

;; (@while TEST BODY...) =>
;; 
;;     (@goto L1)
;; L0: BODY...
;; L1: (@if TEST (@goto L0) (@void))
;;; non-R5RS
(define (@while test . body)
  (let ((L0 (make-label)) (L1 (make-label)))
    `(@begin
       (@goto ,L1)
       (@label ,L0) ,@body
       (@label ,L1) (@if ,test (@goto ,L0) (@void)))))

;; (@cond (TEST BODY...) ...) =>
;;
;;   (@if TEST
;;        (@begin BODY...)
;;        (@cond ...))
(define (@cond . clauses)
  (cond ((null? clauses) (error "missing clauses"))
	((pair? (car clauses))
	 (let ((c (car clauses)) (l (cdr clauses)))
	   (let ((rest (if (null? l) '(@void) `(@cond ,@l))))
	     (cond ((eq? (car c) '@else) `(@begin (@void) ,@(cdr c)))
		   ((null? (cdr c)) `(@or ,(car c) ,rest))
		   (else `(@if ,(car c) (@begin ,@(cdr c)) ,rest))))))
	(else (error "bad clause:" (car clauses)))))

(define (@let* binds . body)
  (if (null? binds)
      `(@begin ,@body)
      `(@let (,(car binds)) (@let* ,(cdr binds) ,@body))))


;;;
;;; R5RS Procedures
;;;

;; 6. Standard procedures

;;; 6.1 Equivalence predicates

(define (@eq? x y)     `(@@ eq? ,x ,y))
(define (@eqv? x y)    `(@@ eqv? ,x ,y))
(define (@equal? x y)  `(@@ equal? ,x ,y))

;;; 6.2 Numbers

(define (@number? x)   `(@@ number? ,x))
(define (@complex? x)  `(@@ complex? ,x))
(define (@real? x)     `(@@ real? ,x))
(define (@rational? x) `(@@ rational? ,x))
(define (@integer? x)  `(@@ integer? ,x))

(define (@exact? x)    `(@@ exact? ,x))
(define (@inexact? x)  `(@@ inexact? ,x))

(define (@= x y)       `(@@ ee? ,x ,y))
(define (@< x y)       `(@@ lt? ,x ,y))
(define (@> x y)       `(@@ gt? ,x ,y))
(define (@<= x y)      `(@@ le? ,x ,y))
(define (@>= x y)      `(@@ ge? ,x ,y))

(define (@zero? x)     `(@= ,x 0))
(define (@positive? x) `(@> ,x 0))
(define (@negative? x) `(@< ,x 0))
(define (@odd? x)      `(@= (@modulo ,x 2) 1))
(define (@even? x)     `(@= (@modulo ,x 2) 0))

(define (@max . args)  `(@@ max ,@args))
(define (@min . args)  `(@@ min ,@args))

(define @+
  (match-lambda*
    (() 0)
    ((x) x)
    ((x y) `(@@ add ,x ,y))
    ((x y . rest) `(@@ add ,x (@+ ,y ,@rest)))))

(define @*
  (match-lambda*
    (() 1)
    ((x) x)
    ((x y) `(@@ mul ,x ,y))
    ((x y . rest) `(@@ mul ,x (@* ,y ,@rest)))))

(define @-
  (match-lambda*
    ((x) `(@@ neg ,x))
    ((x y) `(@@ sub ,x ,y))
    ((x y . rest) `(@@ sub ,x (@+ ,y ,@rest)))))

(define @/
  (match-lambda*
    ((x) `(@@ rec ,x))
    ((x y) `(@@ div ,x ,y))
    ((x y . rest) `(@@ div ,x (@* ,y ,@rest)))))

(define (@abs x) `(@if (@< x 0) (@- x) x))

(define (@quotient x y) `(@@ quotient ,x ,y))
(define (@remainder x y) `(@@ remainder ,x ,y))
(define (@modulo x y) `(@@ modulo ,x ,y))

;;; gcd
;;; lcm
;;; 
;;; numerator
;;; denominator
;;; 
;;; floor
;;; ceiling
;;; truncate
;;; round
;;; 
;;; rationalize
;;; 
;;; exp
;;; log
;;; sin
;;; cos
;;; tan
;;; asin
;;; acos
;;; atan
;;; 
;;; sqrt
;;; expt
;;; 
;;; make-rectangular
;;; make-polar
;;; real-part
;;; imag-part
;;; magnitude
;;; angle
;;; 
;;; exact->inexact
;;; inexact->exact
;;; 
;;; number->string
;;; string->number

;;; 6.3 Other data types

;;;; 6.3.1 Booleans

(define (@not x) `(@@ not ,x))
(define (@boolean? x) `(@@ boolean? ,x))

;;;; 6.3.2 Pairs and lists

(define (@pair? x) `(@@ pair? ,x))
(define (@cons x y) `(@@ cons ,x ,y))

(define (@car x) `(@@ car ,x))
(define (@cdr x) `(@@ cdr ,x))
(define (@set-car! x) `(@@ set-car! ,x))
(define (@set-cdr! x) `(@@ set-cdr! ,x))

(define (@caar x) `(@@ car (@@ car ,x)))
(define (@cadr x) `(@@ car (@@ cdr ,x)))
(define (@cdar x) `(@@ cdr (@@ car ,x)))
(define (@cddr x) `(@@ cdr (@@ cdr ,x)))
(define (@caaar x) `(@@ car (@@ car (@@ car ,x))))
(define (@caadr x) `(@@ car (@@ car (@@ cdr ,x))))
(define (@cadar x) `(@@ car (@@ cdr (@@ car ,x))))
(define (@caddr x) `(@@ car (@@ cdr (@@ cdr ,x))))
(define (@cdaar x) `(@@ cdr (@@ car (@@ car ,x))))
(define (@cdadr x) `(@@ cdr (@@ car (@@ cdr ,x))))
(define (@cddar x) `(@@ cdr (@@ cdr (@@ car ,x))))
(define (@cdddr x) `(@@ cdr (@@ cdr (@@ cdr ,x))))
(define (@caaaar x) `(@@ car (@@ car (@@ car (@@ car ,x)))))
(define (@caaadr x) `(@@ car (@@ car (@@ car (@@ cdr ,x)))))
(define (@caadar x) `(@@ car (@@ car (@@ cdr (@@ car ,x)))))
(define (@caaddr x) `(@@ car (@@ car (@@ cdr (@@ cdr ,x)))))
(define (@cadaar x) `(@@ car (@@ cdr (@@ car (@@ car ,x)))))
(define (@cadadr x) `(@@ car (@@ cdr (@@ car (@@ cdr ,x)))))
(define (@caddar x) `(@@ car (@@ cdr (@@ cdr (@@ car ,x)))))
(define (@cadddr x) `(@@ car (@@ cdr (@@ cdr (@@ cdr ,x)))))
(define (@cdaaar x) `(@@ cdr (@@ car (@@ car (@@ car ,x)))))
(define (@cdaadr x) `(@@ cdr (@@ car (@@ car (@@ cdr ,x)))))
(define (@cdadar x) `(@@ cdr (@@ car (@@ cdr (@@ car ,x)))))
(define (@cdaddr x) `(@@ cdr (@@ car (@@ cdr (@@ cdr ,x)))))
(define (@cddaar x) `(@@ cdr (@@ cdr (@@ car (@@ car ,x)))))
(define (@cddadr x) `(@@ cdr (@@ cdr (@@ car (@@ cdr ,x)))))
(define (@cdddar x) `(@@ cdr (@@ cdr (@@ cdr (@@ car ,x)))))
(define (@cddddr x) `(@@ cdr (@@ cdr (@@ cdr (@@ cdr ,x)))))

(define (@null? x) `(@@ null? ,x))
(define (@list? x) `(@@ list? ,x))
(define (@list . args) `(@@ list ,@args))

;;; length
;;; append
;;; reverse
;;; list-tail
;;; list-ref
;;; 
;;; memq
;;; memv
;;; member
;;; 
;;; assq
;;; assv
;;; assoc

;;;; 6.3.3 Symbols

;;; symbol?
;;; symbol->string
;;; string->symbol

;;;; 6.3.4 Characters

;;; char?
;;; char=?
;;; char<?
;;; char>?
;;; char<=?
;;; char>=?
;;; char-ci=?
;;; char-ci<?
;;; char-ci>?
;;; char-ci<=?
;;; char-ci>=?
;;; char-alphabetic?
;;; char-numeric?
;;; char-whitespace?
;;; char-upper-case?
;;; char-lower-case?
;;; char->integer
;;; integer->char
;;; char-upcase
;;; char-downcase

;;;; 6.3.5 Strings

;;; string?
;;; make-string
;;; string
;;; string-length
;;; string-ref
;;; string-set!
;;; 
;;; string=?
;;; string-ci=?
;;; string<?
;;; string>?
;;; string<=?
;;; string>=?
;;; string-ci<?
;;; string-ci>?
;;; string-ci<=?
;;; string-ci>=?
;;; 
;;; substring
;;; string-append
;;; string->list
;;; list->string
;;; string-copy
;;; string-fill!

;;;; 6.3.6 Vectors

;;; vector?
;;; make-vector
;;; vector
;;; vector-length
;;; vector-ref
;;; vector-set!
;;; vector->list
;;; list->vector
;;; vector-fill!

;;;; 6.4 Control features

(define (@procedure? x) `(@@ procedure? x))

;; (define (@apply proc . args) ...)

;;; map
;;; for-each

;;; (define (@force promise) `(@@ force promise))

;;; (define (@call-with-current-continuation proc) `(@@ call/cc proc))

;;; (define @call/cc @call-with-current-continuation)

;;; values
;;; call-with-values
;;; dynamic-wind

;;; 6.5 Eval

;;; eval
;;; scheme-report-environment
;;; null-environment
;;; interaction-environment

;;; 6.6 Input and output

;;;; 6.6.1 Ports

;;; call-with-input-file
;;; call-with-output-file
;;; 
;;; input-port?
;;; output-port?
;;; current-input-port
;;; current-output-port
;;; 
;;; with-input-from-file
;;; with-output-to-file
;;; 
;;; open-input-file
;;; open-output-file
;;; close-input-port
;;; close-output-port

;;;; 6.6.2 Input

;;; read
;;; read-char
;;; peek-char
;;; eof-object?
;;; char-ready?

;;;; 6.6.3 Output

;;; write
;;; display
;;; newline
;;; write-char

;;;; 6.6.4 System interface

;;; load
;;; transcript-on
;;; transcript-off


;;;
;;; Non-R5RS Procedures
;;;

(define @cons*
  (match-lambda*
    ((x) x)
    ((x y) `(@cons ,x ,y))
    ((x y . rest) `(@cons ,x (@cons* ,y ,@rest)))))

(define (@error . args) `(@@ display ,@args))

(define (@current-module)
  `((@ System::Base::module::current-module)))
