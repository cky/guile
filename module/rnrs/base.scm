;;; base.scm --- The R6RS base library

;;      Copyright (C) 2010 Free Software Foundation, Inc.
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA


(library (rnrs base (6))
  (export boolean? symbol? char? vector? null? pair? number? string? procedure?
	 
	  define define-syntax syntax-rules lambda let let* let-values 
	  let*-values letrec letrec* begin 

	  quote lambda if set! cond case 
	 
	  or and not
	 
	  eqv? equal? eq?
	 
	  + - * / max min abs numerator denominator gcd lcm floor ceiling
	  truncate round rationalize real-part imag-part make-rectangular angle
	  div mod div-and-mod div0 mod0 div0-and-mod0
	  
	  expt exact-integer-sqrt sqrt exp log sin cos tan asin acos atan 
	  make-polar magnitude angle
	 
	  complex? real? rational? integer? exact? inexact? real-valued?
	  rational-valued? integer-values? zero? positive? negative? odd? even?
	  nan? finite? infinite?

	  exact inexact = < > <= >= 

	  number->string string->number

	  cons car cdr caar cadr cdar cddr caaar caadr cadar cdaar caddr cdadr 
	  cddar cdddr caaaar caaadr caadar cadaar cdaaar cddaar cdadar cdaadr 
	  cadadr caaddr caddar cadddr cdaddr cddadr cdddar cddddr

	  list? list length append reverse list-tail list-ref map for-each

	  symbol->string string->symbol symbol=?

	  char->integer integer->char char=? char<? char>? char<=? char>=?

	  make-string string string-length string-ref string=? string<? string>?
	  string<=? string>=? substring string-append string->list list->string
	  string-for-each string-copy

	  vector? make-vector vector vector-length vector-ref vector-set! 
	  vector->list list->vector vector-fill! vector-map vector-for-each

	  error assertion-violation assert

	  call-with-current-continuation call/cc call-with-values dynamic-wind
	  values apply

	  quasiquote unquote unquote-splicing

	  let-syntax letrec-syntax

	  syntax-rules identifier-syntax)
 (import (rename (guile) (quotient div) (modulo mod))
	 (srfi srfi-11))

 (define (vector-for-each proc . vecs)
   (apply for-each (cons proc (map vector->list vecs))))
 (define (vector-map proc . vecs)
   (list->vector (apply map (cons proc (map vector->list vecs)))))

 (define (div-and-mod x y) (let ((q (div x y)) (r (mod x y))) (values q r)))

 (define (div0 x y)
   (call-with-values (lambda () (div0-and-mod0 x y)) (lambda (q r) q)))

 (define (mod0 x y)
   (call-with-values (lambda () (div0-and-mod0 x y)) (lambda (q r) r)))

 (define (div0-and-mod0 x y)
   (call-with-values (lambda () (div-and-mod x y))
     (lambda (q r)
       (cond ((< r (abs (/ y 2))) (values q r))
	     ((negative? y) (values (- q 1) (+ r y)))
	     (else (values (+ q 1) (+ r y)))))))

)
