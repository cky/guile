;"plottest.scm" test charplot.scm
;Copyright (C) 1992 Aubrey Jaffer
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

(require 'charplot)
(require 'random)

(define strophoid
  (let ((l '()))
    (do ((x -1.0 (+ x 0.05)))
	((> x 4.0))
      (let* ((a (/ (- 2 x) (+ 2 x))))
	(if (>= a 0.0)
	    (let* ((y (* x (sqrt a))))
	      (set! l (cons (cons x y) l))
	      (set! l (cons (cons x (- y)) l))))))
    l))

(plot! strophoid "x" "y") (newline)

(define unif
  (let* ((l 6)
	 (v (make-vector l)))
    (do ((i (- l 1) (- i 1)))
	((negative? i))
      (vector-set! v i (cons i 0)))
    (do ((i 24 (- i 1))
	 (r (random l) (random l)))
	((zero? i) (vector->list v))
      (set-cdr! (vector-ref v r) (+ 1 (cdr (vector-ref v r)))))))

(plot! unif "n" "occur")
