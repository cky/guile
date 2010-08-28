;;; fixnums.scm --- The R6RS fixnums arithmetic library

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


(library (rnrs arithmetic fixnums (6))
  (export fixnum?
	  
	  fixnum-width
	  least-fixnum
	  greatest-fixnum

	  fx=?
	  fx>?
	  fx<?
	  fx>=?
	  fx<=?

	  fxzero?
	  fxpositive?
	  fxnegative?
	  fxodd?
	  fxeven?

	  fxmax
	  fxmin
	  
	  fx+
	  fx*
	  fx-

	  fxdiv-and-mod
	  fxdiv
	  fxmod
	  fxdiv0-and-mod0
	  fxdiv0
	  fxmod0

	  fx+/carry
	  fx-/carry
	  fx*/carry

	  fxnot
	  fxand
	  fxior
	  fxxor
	  fxif

	  fxbit-count
	  fxlength
	  fxfirst-bit-set
	  fxbit-set?
	  fxcopy-bit
	  fxbit-field
	  fxcopy-bit-field

	  fxarithmetic-shift
	  fxarithmetic-shift-left
	  fxarithmetic-shift-right

	  fxrotate-bit-field
	  fxreverse-bit-field)
  (import (only (guile) ash
		        cons*
			inexact->exact
			logand
			logbit?
			logcount
			logior
			lognot
			logxor
			most-positive-fixnum 
			most-negative-fixnum)
	  (ice-9 optargs)
	  (rnrs base (6))
	  (rnrs arithmetic bitwise (6))
	  (rnrs conditions (6))
	  (rnrs exceptions (6))
	  (rnrs lists (6)))

  (define fixnum-width 
    (let ((w (inexact->exact (round (/ (log (+ most-positive-fixnum 1)) (log 2))))))
      (lambda () w)))

  (define (greatest-fixnum) most-positive-fixnum)
  (define (least-fixnum) most-negative-fixnum)
  
  (define (fixnum? obj) 
    (and (integer? obj) 
	 (exact? obj) 
	 (>= obj most-negative-fixnum) 
	 (<= obj most-positive-fixnum)))

  (define (assert-fixnum . args)
    (or (for-all fixnum? args) (raise (make-assertion-violation))))

  (define (fx=? fx1 fx2 . rst)
    (let ((args (cons* fx1 fx2 rst)))
      (apply assert-fixnum args) 
      (apply = args)))

  (define (fx>? fx1 fx2 . rst)
    (let ((args (cons* fx1 fx2 rst))) 
      (apply assert-fixnum args) 
      (apply > args)))

  (define (fx<? fx1 fx2 . rst)
    (let ((args (cons* fx1 fx2 rst)))
      (apply assert-fixnum rst)
      (apply < args)))

  (define (fx>=? fx1 fx2 . rst)
    (let ((args (cons* fx1 fx2 rst)))
      (apply assert-fixnum rst)
      (apply >= args)))

  (define (fx<=? fx1 fx2 . rst)
    (let ((args (cons* fx1 fx2 rst)))
      (apply assert-fixnum rst)
      (apply <= args)))
  
  (define (fxzero? fx) (assert-fixnum fx) (zero? fx))
  (define (fxpositive? fx) (assert-fixnum fx) (positive? fx))
  (define (fxnegative? fx) (assert-fixnum fx) (negative? fx))
  (define (fxodd? fx) (assert-fixnum fx) (odd? fx))
  (define (fxeven? fx) (assert-fixnum fx) (even? fx))

  (define (fxmax fx1 fx2 . rst)
    (let ((args (cons* fx1 fx2 rst)))
      (apply assert-fixnum args)
      (apply max args)))

  (define (fxmin fx1 fx2 . rst)
    (let ((args (cons* fx1 fx2 rst)))
      (apply assert-fixnum args)
      (apply min args)))
 
  (define (fx+ fx1 fx2)
    (assert-fixnum fx1 fx2) 
    (let ((r (+ fx1 fx2))) 
      (or (fixnum? r) (raise (make-implementation-restriction-violation)))
      r))

  (define (fx* fx1 fx2)
    (assert-fixnum fx1 fx2) 
    (let ((r (* fx1 fx2))) 
      (or (fixnum? r) (raise (make-implementation-restriction-violation)))
      r))

  (define* (fx- fx1 #:optional fx2)
    (assert-fixnum fx1)
    (if fx2 
	(begin 
	  (assert-fixnum fx2) 
	  (let ((r (- fx1 fx2))) 
	    (or (fixnum? r) (raise (make-assertion-violation)))
	    r))
	(let ((r (- fx1))) 
	  (or (fixnum? r) (raise (make-assertion-violation)))
	  r)))

  (define (fxdiv fx1 fx2)
    (assert-fixnum fx1 fx2)
    (if (zero? fx2) (raise (make-assertion-violation)))
    (let ((r (div fx1 fx2))) r))

  (define (fxmod fx1 fx2)
    (assert-fixnum fx1 fx2)
    (if (zero? fx2) (raise (make-assertion-violation)))
    (let ((r (mod fx1 fx2))) r))

  (define (fxdiv-and-mod fx1 fx2)
    (assert-fixnum fx1 fx2)
    (if (zero? fx2) (raise (make-assertion-violation)))
    (div-and-mod fx1 fx2))

  (define (fxdiv0 fx1 fx2)
    (assert-fixnum fx1 fx2)
    (if (zero? fx2) (raise (make-assertion-violation)))
    (let ((r (div0 fx1 fx2))) r))
  
  (define (fxmod0 fx1 fx2)
    (assert-fixnum fx1 fx2)
    (if (zero? fx2) (raise (make-assertion-violation)))
    (let ((r (mod0 fx1 fx2))) r))    

  (define (fxdiv0-and-mod0 fx1 fx2)
    (assert-fixnum fx1 fx2)
    (if (zero? fx2) (raise (make-assertion-violation)))
    (call-with-values (lambda () (div0-and-mod0 fx1 fx2))
      (lambda (q r) (values q r))))

  (define (fx+/carry fx1 fx2 fx3)
    (assert-fixnum fx1 fx2 fx3)
    (let* ((s (+ fx1 fx2 fx3))
	   (s0 (mod0 s (inexact->exact (expt 2 (fixnum-width)))))
	   (s1 (div0 s (inexact->exact (expt 2 (fixnum-width))))))
      (values s0 s1)))

  (define (fx-/carry fx1 fx2 fx3)
    (assert-fixnum fx1 fx2 fx3)
    (let* ((d (- fx1 fx2 fx3))
	   (d0 (mod0 d (expt 2 (fixnum-width))))
	   (d1 (div0 d (expt 2 (fixnum-width)))))
      (values d0 d1)))

  (define (fx*/carry fx1 fx2 fx3)
    (assert-fixnum fx1 fx2 fx3)
    (let* ((s (+ (* fx1 fx2) fx3))
	   (s0 (mod0 s (expt 2 (fixnum-width))))
	   (s1 (div0 s (expt 2 (fixnum-width)))))
      (values s0 s1)))

  (define (fxnot fx) (assert-fixnum fx) (lognot fx))
  (define (fxand . args) (apply assert-fixnum args) (apply logand args))
  (define (fxior . args) (apply assert-fixnum args) (apply logior args))
  (define (fxxor . args) (apply assert-fixnum args) (apply logxor args))

  (define (fxif fx1 fx2 fx3) 
    (assert-fixnum fx1 fx2 fx3) 
    (bitwise-if fx1 fx2 fx3))

  (define (fxbit-count fx) (assert-fixnum fx) (logcount fx))
  (define (fxlength fx) (assert-fixnum fx) (bitwise-length fx))
  (define (fxfirst-bit-set fx) (assert-fixnum fx) (bitwise-first-bit-set fx))
  (define (fxbit-set? fx1 fx2) (assert-fixnum fx1 fx2) (logbit? fx2 fx1))

  (define (fxcopy-bit fx1 fx2 fx3) 
    (assert-fixnum fx1 fx2 fx3) 
    (bitwise-copy-bit fx1 fx2 fx3))

  (define (fxbit-field fx1 fx2 fx3)
    (assert-fixnum fx1 fx2 fx3)
    (bitwise-bit-field fx1 fx2 fx3))

  (define (fxcopy-bit-field fx1 fx2 fx3 fx4)
    (assert-fixnum fx1 fx2 fx3 fx4)
    (bitwise-copy-bit-field fx1 fx2 fx3 fx4))

  (define (fxarithmetic-shift fx1 fx2) (assert-fixnum fx1 fx2) (ash fx1 fx2))
  (define fxarithmetic-shift-left fxarithmetic-shift)

  (define (fxarithmetic-shift-right fx1 fx2)
    (assert-fixnum fx1 fx2) (ash fx1 (- fx2)))

  (define (fxrotate-bit-field fx1 fx2 fx3 fx4)
    (assert-fixnum fx1 fx2 fx3 fx4)
    (bitwise-rotate-bit-field fx1 fx2 fx3 fx4))
  
  (define (fxreverse-bit-field fx1 fx2 fx3)
    (assert-fixnum fx1 fx2 fx3)
    (bitwise-reverse-bit-field fx1 fx2 fx3))

)
