;;;; "modular.scm", modular fixnum arithmetic for Scheme
;;; Copyright (C) 1991, 1993, 1995 Aubrey Jaffer.
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

(define (symmetric:modulus n)
  (cond ((or (not (number? n)) (not (positive? n)) (even? n))
	 (slib:error 'symmetric:modulus n))
	(else (quotient (+ -1 n) -2))))

(define (modulus->integer m)
  (cond ((negative? m) (- 1 m m))
	((zero? m) #f)
	(else m)))

(define (modular:normalize m k)
  (cond ((positive? m) (modulo k m))
	((zero? m) k)
	((<= m k (- m)) k)
	((or (provided? 'bignum)
	     (<= m (quotient (+ -1 most-positive-fixnum) 2)))
	 (let* ((pm (+ 1 (* -2 m)))
		(s (modulo k pm)))
	   (if (<= s (- m)) s (- s pm))))
	((positive? k) (+ (+ (+ k -1) m) m))
	(else  (- (- (+ k 1) m) m))))

;;;; NOTE: The rest of these functions assume normalized arguments!

(require 'logical)

(define (modular:extended-euclid x y)
  (define q 0)
  (do ((r0 x r1) (r1 y (remainder r0 r1))
       (u0 1 u1) (u1 0 (- u0 (* q u1)))
       (v0 0 v1) (v1 1 (- v0 (* q v1))))
      ;; (assert (= r0 (+ (* u0 x) (* v0 y))))
      ;; (assert (= r1 (+ (* u1 x) (* v1 y))))
      ((zero? r1) (list r0 u0 v0))
    (set! q (quotient r0 r1))))

(define (modular:invertable? m a)
  (eqv? 1 (gcd (or (modulus->integer m) 0) a)))

(define (modular:invert m a)
  (cond ((eqv? 1 (abs a)) a)		; unit
	(else
	 (let ((pm (modulus->integer m)))
	   (cond
	    (pm
	     (let ((d (modular:extended-euclid (modular:normalize pm a) pm)))
	       (if (= 1 (car d))
		   (modular:normalize m (cadr d))
		   (slib:error 'modular:invert "can't invert" m a))))
	    (else (slib:error 'modular:invert "can't invert" m a)))))))

(define (modular:negate m a)
  (if (zero? a) 0
      (if (negative? m) (- a)
	  (- m a))))

;;; Being careful about overflow here
(define (modular:+ m a b)
  (cond ((positive? m)
	 (modulo (+ (- a m) b) m))
	((zero? m) (+ a b))
	((negative? a)
	 (if (negative? b)
	     (let ((s (+ (- a m) b)))
	       (if (negative? s)
		   (- s -1 m)
		   (+ s m)))
	     (+ a b)))
	((negative? b) (+ a b))
	(else (let ((s (+ (+ a m) b)))
		(if (positive? s)
		    (+ s -1 m)
		    (- s m))))))

(define (modular:- m a b)
  (cond ((positive? m) (modulo (- a b) m))
	((zero? m) (- a b))
	(else (modular:+ m a (- b)))))

;;; See: L'Ecuyer, P. and Cote, S. "Implementing a Random Number Package
;;; with Splitting Facilities." ACM Transactions on Mathematical
;;; Software, 17:98-111 (1991)

;;; modular:r = 2**((nb-2)/2) where nb = number of bits in a word.
(define modular:r
  (ash 1 (quotient (integer-length most-positive-fixnum) 2)))
(define modular:*
  (if (provided? 'bignum)
      (lambda (m a b)
	(cond ((zero? m) (* a b))
	      ((positive? m) (modulo (* a b) m))
	      (else (modular:normalize m (* a b)))))
      (lambda (m a b)
	(let ((a0 a)
	      (p 0))
	  (cond
	   ((zero? m) (* a b))
	   ((negative? m)
	    "This doesn't work for the full range of modulus M;"
	    "Someone please create or convert the following"
	    "algorighm to work with symmetric representation"
	    (modular:normalize m (* a b)))
	   (else
	    (cond
	     ((< a modular:r))
	     ((< b modular:r) (set! a b) (set! b a0) (set! a0 a))
	     (else
	      (set! a0 (modulo a modular:r))
	      (let ((a1 (quotient a modular:r))
		    (qh (quotient m modular:r))
		    (rh (modulo m modular:r)))
		(cond ((>= a1 modular:r)
		       (set! a1 (- a1 modular:r))
		       (set! p (modulo (- (* modular:r (modulo b qh))
					  (* (quotient b qh) rh)) m))))
		(cond ((not (zero? a1))
		       (let ((q (quotient m a1)))
			 (set! p (- p (* (quotient b q) (modulo m a1))))
			 (set! p (modulo (+ (if (positive? p) (- p m) p)
					    (* a1 (modulo b q))) m)))))
		(set! p (modulo (- (* modular:r (modulo p qh))
				   (* (quotient p qh) rh)) m)))))
	    (if (zero? a0)
		p
		(let ((q (quotient m a0)))
		  (set! p (- p (* (quotient b q) (modulo m a0))))
		  (modulo (+ (if (positive? p) (- p m) p)
			     (* a0 (modulo b q))) m)))))))))

(define (modular:expt m a b)
  (cond ((= a 1) 1)
	((= a (- m 1)) (if (odd? b) a 1))
	((zero? a) 0)
	((zero? m) (integer-expt a b))
	(else
	 (logical:ipow-by-squaring a b 1
				   (lambda (c d) (modular:* m c d))))))

(define extended-euclid modular:extended-euclid)
