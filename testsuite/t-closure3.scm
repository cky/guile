(define (stuff)
  (let* ((x 2)
	 (chbouib (lambda (z)
		    (+ 7 z x))))
    (chbouib 77)))

(stuff)
