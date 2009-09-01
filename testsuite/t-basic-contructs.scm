;;; Basic RnRS constructs.

(and (eq? 2 (begin (+ 2 4) 5 2))
     ((lambda (x y)
	(and (eq? x 1) (eq? y 2)
	     (begin
	       (set! x 11) (set! y 22)
	       (and (eq? x 11) (eq? y 22)))))
      1 2)
     (let ((x 1) (y 3))
       (and (eq? x 1) (eq? y 3)))
     (let loop ((x #t))
       (if (not x)
	   #t
	   (loop #f))))

