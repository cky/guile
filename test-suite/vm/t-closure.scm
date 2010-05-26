(define func
  (let ((x 2))
    (lambda ()
      (let ((x++ (+ 1 x)))
	(set! x x++)
	x++))))

(list (func) (func) (func))
