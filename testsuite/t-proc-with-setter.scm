(define the-struct (vector 1 2))

(define get/set
  (make-procedure-with-setter
   (lambda (struct name)
     (case name
       ((first)  (vector-ref struct 0))
       ((second) (vector-ref struct 1))
       (else     #f)))
   (lambda (struct name val)
     (case name
       ((first)  (vector-set! struct 0 val))
       ((second) (vector-set! struct 1 val))
       (else     #f)))))

(and (eq? (vector-ref the-struct 0) (get/set the-struct 'first))
     (eq? (vector-ref the-struct 1) (get/set the-struct 'second))
     (begin
       (set! (get/set the-struct 'second) 77)
       (eq? (vector-ref the-struct 1) (get/set the-struct 'second))))
