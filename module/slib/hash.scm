; "hash.scm", hashing functions for Scheme.
; Copyright (c) 1992, 1993, 1995 Aubrey Jaffer
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

(define (hash:hash-char-ci char n)
  (modulo (char->integer (char-downcase char)) n))

(define hash:hash-char hash:hash-char-ci)

(define (hash:hash-symbol sym n)
  (hash:hash-string (symbol->string sym) n))

;;; This can overflow on implemenatations where inexacts have a larger
;;; range than exact integers.
(define hash:hash-number
  (if (provided? 'inexact)
      (lambda (num n)
	(if (integer? num)
	    (modulo (if (exact? num) num (inexact->exact num)) n)
	    (hash:hash-string-ci
	     (number->string (if (exact? num) (exact->inexact num) num))
	     n)))
      (lambda (num n)
	(if (integer? num)
	    (modulo num n)
	    (hash:hash-string-ci (number->string num) n)))))

(define (hash:hash-string-ci str n)
  (let ((len (string-length str)))
    (if (> len 5)
	(let loop ((h (modulo 264 n)) (i 5))
	  (if (positive? i)
	      (loop (modulo (+ (* h 256)
			       (char->integer
				(char-downcase
				 (string-ref str (modulo h len)))))
			    n)
		    (- i 1))
	      h))
	(let loop ((h 0) (i (- len 1)))
	  (if (>= i 0)
	      (loop (modulo (+ (* h 256)
			       (char->integer
				(char-downcase (string-ref str i))))
			    n)
		    (- i 1))
	      h)))))

(define hash:hash-string hash:hash-string-ci)

(define (hash:hash obj n)
  (let hs ((d 10) (obj obj))
    (cond
     ((number? obj)      (hash:hash-number obj n))
     ((char? obj)        (modulo (char->integer (char-downcase obj)) n))
     ((symbol? obj)      (hash:hash-symbol obj n))
     ((string? obj)      (hash:hash-string obj n))
     ((vector? obj)
      (let ((len (vector-length obj)))
	(if (> len 5)
	    (let lp ((h 1) (i (quotient d 2)))
	      (if (positive? i)
		  (lp (modulo (+ (* h 256)
				 (hs 2 (vector-ref obj (modulo h len))))
			      n)
		      (- i 1))
		  h))
	    (let loop ((h (- n 1)) (i (- len 1)))
	      (if (>= i 0)
		  (loop (modulo (+ (* h 256) (hs (quotient d len)
						 (vector-ref obj i)))
				n)
			(- i 1))
		  h)))))
     ((pair? obj)
      (if (positive? d) (modulo (+ (hs (quotient d 2) (car obj))
				   (hs (quotient d 2) (cdr obj)))
				n)
	  1))
     (else
      (modulo
       (cond
	((null? obj)        256)
	((boolean? obj)     (if obj 257 258))
	((eof-object? obj)  259)
	((input-port? obj)  260)
	((output-port? obj) 261)
	((procedure? obj)   262)
	((and (provided? 'RECORD) (record? obj))
	 (let* ((rtd (record-type-descriptor obj))
		(fns (record-type-field-names rtd))
		(len (length fns)))
	   (if (> len 5)
	       (let lp ((h (modulo 266 n)) (i (quotient d 2)))
		 (if (positive? i)
		     (lp (modulo
			  (+ (* h 256)
			     (hs 2 ((record-accessor
				     rtd (list-ref fns (modulo h len)))
				    obj)))
			  n)
			 (- i 1))
		     h))
	       (let loop ((h (- n 1)) (i (- len 1)))
		 (if (>= i 0)
		     (loop (modulo
			    (+ (* h 256)
			       (hs (quotient d len)
				   ((record-accessor
				     rtd (list-ref fns (modulo h len)))
				    obj)))
			    n)
			   (- i 1))
		     h)))))
	(else               263))
       n)))))

(define hash hash:hash)
(define hashv hash:hash)

;;; Object-hash is somewhat expensive on copying GC systems (like
;;; PC-Scheme and MITScheme).  We use it only on strings, pairs,
;;; vectors, and records.  This also allows us to use it for both
;;; hashq and hashv.

(if (provided? 'object-hash)
    (set! hashv
	  (if (provided? 'record)
	      (lambda (obj k)
		(if (or (string? obj) (pair? obj) (vector? obj) (record? obj))
		    (modulo (object-hash obj) k)
		    (hash:hash obj k)))
	      (lambda (obj k)
		(if (or (string? obj) (pair? obj) (vector? obj))
		    (modulo (object-hash obj) k)
		    (hash:hash obj k))))))

(define hashq hashv)
