; Currently, map is a C function, so this is a way of testing that the
; VM is reentrant.

(begin

  (define (square x)
    (* x x))

  (map (lambda (x) (square x))
       '(1 2 3)))
