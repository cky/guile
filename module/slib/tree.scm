;;"tree.scm" Implementation of COMMON LISP tree functions for Scheme
; Copyright 1993, 1994 David Love (d.love@dl.ac.uk)
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

;; Deep copy of the tree -- new one has all new pairs.  (Called
;; tree-copy in Dybvig.)
(define (tree:copy-tree tree)
  (if (pair? tree)
      (cons (tree:copy-tree (car tree))
	    (tree:copy-tree (cdr tree)))
      tree))

;; Substitute occurrences of old equal? to new in tree.
;; Similar to tree walks in SICP without the internal define.
(define (tree:subst new old tree)
  (let walk ((tree tree))
    (cond ((equal? old tree)
	   new)
	  ((pair? tree)
	   (cons (walk (car tree))
		 (walk (cdr tree))))
	  (else tree))))

;; The next 2 aren't in CL.  (Names from Dybvig)

(define (tree:substq new old tree)
  (let walk ((tree tree))
    (cond ((eq? old tree)
	   new)
	  ((pair? tree)
	   (cons (walk (car tree))
		 (walk (cdr tree))))
	  (else tree))))

(define (tree:substv new old tree)
  (let walk ((tree tree))
    (cond ((eqv? old tree)
	   new)
	  ((pair? tree)
	   (cons (walk (car tree))
		 (walk (cdr tree))))
	  (else tree))))

(define copy-tree tree:copy-tree)
(define subst tree:subst)
(define substq tree:substq)
(define substv tree:substv)
