;;; "mularg.scm" Redefine - and / to take more than 2 arguments.

(define / /)
(define - -)
(let ((maker
       (lambda (op)
	 (lambda (d1 . ds)
	   (cond ((null? ds) (op d1))
		 ((null? (cdr ds)) (op d1 (car ds)))
		 (else (for-each (lambda (d) (set! d1 (op d1 d))) ds) d1))))))
  (set! / (maker /))
  (set! - (maker -)))
