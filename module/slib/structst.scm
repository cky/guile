;"structst.scm" test "struct.scm"
;Copyright (C) 1993 Aubrey Jaffer
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

(require 'struct)

(define-record foo (a b c))
(define-record goo (xx yy))

(define a-foo (make-foo 1 2 3))
(define a-goo (make-goo 4 5))

(define (struct:test)
  (define (t1 x)
    (variant-case x
      (foo (a b c) (list a b c))
      (goo (xx yy) (list xx yy))
      (else (list 7 8))))
  (write (append (t1 a-foo) (t1 a-goo) (t1 9)))
  (newline))

(struct:test)
