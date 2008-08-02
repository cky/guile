(use-modules (ice-9 receive))

(define (do-stuff x y)
  (values x y))

(call-with-values (lambda ()    (values 1 2))
		  (lambda (x y) (cons x y)))

