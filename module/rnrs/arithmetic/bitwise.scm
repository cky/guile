;;; bitwise.scm --- The R6RS bitwise arithmetic operations library

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


(library (rnrs arithmetic bitwise (6))
  (export bitwise-not
	  
	  bitwise-and
	  bitwise-ior
	  bitwise-xor
	  
	  bitwise-if
	  bitwise-bit-count
	  bitwise-length

	  bitwise-first-bit-set
	  bitwise-bit-set?
	  bitwise-copy-bit
	  bitwise-bit-field
	  bitwise-copy-bit-field

	  bitwise-arithmetic-shift
	  bitwise-arithmetic-shift-left
	  bitwise-arithmetic-shift-right
	  bitwise-rotate-bit-field
	  bitwise-reverse-bit-field)
  (import (rnrs base (6))
	  (rnrs control (6))
	  (rename (only (guile) lognot 
			        logand 
				logior
				logxor 
				logcount 
				logbit?
				modulo
				ash)
		  (lognot bitwise-not)
		  (logand bitwise-and) 
		  (logior bitwise-ior) 
		  (logxor bitwise-xor)
		  (ash bitwise-arithmetic-shift)))

  (define (bitwise-bit-count ei)
    (if (negative? ei)
        (bitwise-not (logcount ei))
        (logcount ei)))

  (define (bitwise-if ei1 ei2 ei3)
    (bitwise-ior (bitwise-and ei1 ei2) (bitwise-and (bitwise-not ei1) ei3)))
  
  (define (bitwise-length ei)
    (do ((result 0 (+ result 1))
	 (bits (if (negative? ei) (bitwise-not ei) ei)
	       (bitwise-arithmetic-shift bits -1)))
	((zero? bits)
	 result)))

  (define (bitwise-first-bit-set ei)
    (define (bitwise-first-bit-set-inner bits count)
      (cond ((zero? bits) -1)
	    ((logbit? 0 bits) count)
	    (else (bitwise-first-bit-set-inner 
		   (bitwise-arithmetic-shift bits -1) (+ count 1)))))
    (bitwise-first-bit-set-inner ei 0))

  (define (bitwise-bit-set? ei1 ei2) (logbit? ei2 ei1))

  (define (bitwise-copy-bit ei1 ei2 ei3)
    (bitwise-if (bitwise-arithmetic-shift-left 1 ei2) 
		(bitwise-arithmetic-shift-left ei3 ei2)
		ei1))

  (define (bitwise-bit-field ei1 ei2 ei3)
    (bitwise-arithmetic-shift-right 
     (bitwise-and ei1 (bitwise-not (bitwise-arithmetic-shift-left -1 ei3)))
     ei2))

  (define (bitwise-copy-bit-field ei1 ei2 ei3 ei4)
    (bitwise-if (bitwise-and (bitwise-arithmetic-shift-left -1 ei2)
			     (bitwise-not 
			      (bitwise-arithmetic-shift-left -1 ei3)))
		(bitwise-arithmetic-shift-left ei4 ei2)
		ei1))

  (define bitwise-arithmetic-shift-left bitwise-arithmetic-shift)
  (define (bitwise-arithmetic-shift-right ei1 ei2)
    (bitwise-arithmetic-shift ei1 (- ei2)))
  
  (define (bitwise-rotate-bit-field ei1 ei2 ei3 ei4)
    (let ((width (- ei3 ei2)))
      (if (positive? width)
	  (let ((field (bitwise-bit-field ei1 ei2 ei3))
		(count (modulo ei4 width)))
	    (bitwise-copy-bit-field 
	     ei1 ei2 ei3 
	     (bitwise-ior (bitwise-arithmetic-shift-left field count)
			  (bitwise-arithmetic-shift-right 
			   field (- width count)))))
	  ei1)))

  (define (bitwise-reverse-bit-field ei1 ei2 ei3)
    (define (reverse-bit-field-recursive n1 n2 len)
      (if (> len 0)
	  (reverse-bit-field-recursive
	   (bitwise-arithmetic-shift-right n1 1) 
	   (bitwise-copy-bit (bitwise-arithmetic-shift-left n2 1) 0 n1)
	   (- len 1))
	  n2))
    (let ((width (- ei3 ei2)))
      (if (positive? width)
	  (let ((field (bitwise-bit-field ei1 ei2 ei3)))
	    (bitwise-copy-bit-field
	     ei1 ei2 ei3 (reverse-bit-field-recursive field 0 width)))
	  ei1))))
