;;;; "makcrc.scm" Compute Cyclic Checksums
;;; Copyright (C) 1995, 1996, 1997, 2001 Aubrey Jaffer.
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

(require 'byte)
(require 'logical)

(define (make-port-crc . margs)
  (define (make-mask hibit)
    (+ (ash (+ -1 (ash 1 (+ 1 (- hibit 2)))) 1) 1))
  (define chunk-bits (integer-length (+ -1 char-code-limit)))
  (define accum-bits #f)
  (define generator #f)
  (case (length margs)
    ((0) #t)
    ((1) (if (< (car margs) 128)
	     (set! accum-bits (car margs))
	     (set! generator (car margs))))
    ((2)
     (set! accum-bits (car margs))
     (set! generator (cadr margs)))
    (else  (slib:error 'make-port-crc 'args margs)))
  (cond ((not generator)
	 (case accum-bits
	   ((#f 32) (set! accum-bits 32)
	    (set! generator #b00000100110000010001110110110111)) ; CRC-32
	   ((16) (set! generator #b0001000000001011)) ; CRC-16
	   ;;((16) (set! generator #b0001000000100001)) ; CRC-CCIT
	   ;;((08) (set! generator #b101011))
	   (else (slib:error 'make-port-crc "no default polynomial for"
			     accum-bits "bits"))))
	((not accum-bits)
	 (set! accum-bits (+ -1 (integer-length generator)))))
  (set! generator (logand generator (lognot (ash 1 accum-bits))))
  (cond ((>= (integer-length generator) accum-bits)
	 (slib:error 'make-port-crc
		     "generator longer than" accum-bits "bits")))
  (let* ((chunk-mask (make-mask chunk-bits))
	 (crctab (make-vector (+ 1 chunk-mask))))
    (define (accum src)
      `(set!
	crc
	(logxor (ash (logand ,(make-mask (- accum-bits chunk-bits)) crc)
		     ,chunk-bits)
		(vector-ref crctab
			    (logand ,chunk-mask
				    (logxor
				     (ash crc ,(- chunk-bits accum-bits))
				     ,src))))))
    (define (make-crc-table)
      (letrec ((r (make-vector chunk-bits))
	       (remd (lambda (m)
		       (define rem 0)
		       (do ((i 0 (+ 1 i)))
			   ((>= i chunk-bits) rem)
			 (if (logbit? i m)
			     (set! rem (logxor rem (vector-ref r i))))))))
	(vector-set! r 0 generator)
	(do ((i 1 (+ 1 i)))
	    ((>= i chunk-bits))
	  (let ((r-1 (vector-ref r (+ -1 i)))
		(m-1 (make-mask (+ -1 accum-bits))))
	    (vector-set! r i (if (logbit? (+ -1 accum-bits) r-1)
				 (logxor (ash (logand m-1 r-1) 1) generator)
				 (ash (logand m-1 r-1) 1)))))
	(do ((i 0 (+ 1 i)))
	    ((> i chunk-mask))
	  (vector-set! crctab i (remd i)))))
    (make-crc-table)
    `(lambda (port)
       (define crc 0)
       (define byte-count 0)
       (define crctab ',crctab)
       (do ((ci (read-byte port) (read-byte port)))
	   ((eof-object? ci))
	 ,(accum 'ci)
	 (set! byte-count (+ 1 byte-count)))
       (do ((byte-count byte-count (ash byte-count ,(- chunk-bits))))
	   ((zero? byte-count))
	 ,(accum 'byte-count))
       (logxor ,(make-mask accum-bits) crc))))
