
(define (uid)
  (let* ((x 2)
	 (do-uid (lambda ()
		   (let ((x++ (+ 1 x)))
		     (set! x x++)
		     x++))))
    (do-uid)))

(list (uid) (uid) (uid))
