; "mulapply.scm" Redefine APPLY take more than 2 arguments.
;Copyright (C) 1991 Aubrey Jaffer
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

(define two-arg:apply apply)
(define apply
  (lambda args
    (two-arg:apply (car args) (apply:append-to-last (cdr args)))))

(define (apply:append-to-last lst)
  (if (null? (cdr lst))
      (car lst)
      (cons (car lst) (apply:append-to-last (cdr lst)))))
