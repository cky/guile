;;;"fft.scm" Fast Fourier Transform
;Copyright (C) 1999 Aubrey Jaffer
;
;Permission to copy this software, to redistribute it, and to use it
;for any purpose is granted, subject to the following restrictions and
;understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;;;; See:
;;; Introduction to Algorithms (MIT Electrical
;;;    Engineering and Computer Science Series)
;;; by Thomas H. Cormen, Charles E. Leiserson (Contributor),
;;;    Ronald L. Rivest (Contributor)
;;; MIT Press; ISBN: 0-262-03141-8 (July 1990)

;;; http://www.astro.virginia.edu/~eww6n/math/DiscreteFourierTransform.html
;;; differs in the direction of rotation of the complex unit vectors.

(require 'array)

(define (fft:shuffled&scaled ara n scale)
  (define lgn (integer-length (+ -1 n)))
  (define new (apply make-array 0 (array-dimensions ara)))
  (define bit-reverse (lambda (width in)
			(if (zero? width) 0
			    (+ (bit-reverse (+ -1 width) (quotient in 2))
			       (ash (modulo in 2) (+ -1 width))))))
  (if (not (eqv? n (expt 2 lgn)))
      (slib:error 'fft "array length not power of 2" n))
  (do ((k 0 (+ 1 k)))
      ((>= k n) new)
    (array-set! new (* (array-ref ara k) scale) (bit-reverse lgn k))))

(define (dft! ara n dir)
  (define lgn (integer-length (+ -1 n)))
  (define pi2i (* 0+8i (atan 1)))
  (do ((s 1 (+ 1 s)))
      ((> s lgn) ara)
    (let* ((m (expt 2 s))
	   (w_m (exp (* dir (/ pi2i m))))
	   (m/2-1 (+ (quotient m 2) -1)))
      (do ((j 0 (+ 1 j))
	   (w 1 (* w w_m)))
	  ((> j m/2-1))
	(do ((k j (+ m k)))
	    ((>= k n))
	  (let* ((k+m/2 (+ k m/2-1 1))
		 (t (* w (array-ref ara k+m/2)))
		 (u (array-ref ara k)))
	    (array-set! ara (+ u t) k)
	    (array-set! ara (- u t) k+m/2)))))))

(define (fft ara)
  (define n (car (array-dimensions ara)))
  (dft! (fft:shuffled&scaled ara n 1) n 1))

(define (fft-1 ara)
  (define n (car (array-dimensions ara)))
  (dft! (fft:shuffled&scaled ara n (/ n)) n -1))
