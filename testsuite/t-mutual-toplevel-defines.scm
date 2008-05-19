(define (even? x)
  (or (zero? x)
      (not (odd? (1- x)))))

(define (odd? x)
  (not (even? (1- x))))

(even? 20)
