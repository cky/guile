;;;; "arraymap.scm", applicative routines for arrays in Scheme.
;;; Copyright (c) 1993 Aubrey Jaffer
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

(require 'array)

(define (array-map! ra0 proc . ras)
  (define (ramap rshape inds)
    (if (null? (cdr rshape))
	(do ((i (cadar rshape) (+ -1 i))
	     (is (cons (cadar rshape) inds)
		 (cons (+ -1 i) inds)))
	    ((< i (caar rshape)))
	  (apply array-set! ra0
		 (apply proc (map (lambda (ra) (apply array-ref ra is))
				  ras))
		 is))
	(let ((crshape (cdr rshape))
	      (ll (caar rshape)))
	  (do ((i (cadar rshape) (+ -1 i)))
	      ((< i ll))
	    (ramap crshape (cons i inds))))))
  (ramap (reverse (array-shape ra0)) '()))

(define (array-for-each proc . ras)
  (define (rafe rshape inds)
    (if (null? (cdr rshape))
	(do ((i (caar rshape) (+ 1 i)))
	    ((> i (cadar rshape)))
	  (apply proc
		 (map (lambda (ra)
			(apply array-ref ra (reverse (cons i inds)))) ras)))
	(let ((crshape (cdr rshape))
	      (ll (cadar rshape)))
	  (do ((i (caar rshape) (+ 1 i)))
	      ((> i ll))
	    (rafe crshape (cons i inds))))))
  (rafe (array-shape (car ras)) '()))

(define (array-index-map! ra fun)
  (define (ramap rshape inds)
    (if (null? (cdr rshape))
	(do ((i (cadar rshape) (+ -1 i))
	     (is (cons (cadar rshape) inds)
		 (cons (+ -1 i) inds)))
	    ((< i (caar rshape)))
	  (apply array-set! ra (apply fun is) is))
	(let ((crshape (cdr rshape))
	      (ll (caar rshape)))
	  (do ((i (cadar rshape) (+ -1 i)))
	      ((< i ll))
	    (ramap crshape (cons i inds))))))
  (if (zero? (array-rank ra))
      (array-set! ra (fun))
      (ramap (reverse (array-shape ra)) '())))

(define (array-indexes ra)
  (let ((ra0 (apply make-array '() (array-shape ra))))
    (array-index-map! ra0 list)
    ra0))

(define (array-copy! source dest)
  (array-map! dest identity source))
