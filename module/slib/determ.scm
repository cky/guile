;"determ.scm" Determinant

(define (determinant m)
  (define (butnth n lst)
    (if (zero? n) (cdr lst) (cons (car lst) (butnth (+ -1 n) (cdr lst)))))
  (define (minor m i j)
    (map (lambda (x) (butnth j x)) (butnth i m)))
  (define (cofactor m i j)
    (* (if (odd? (+ i j)) -1 1) (determinant (minor m i j))))
  (define n (length m))
  (if (eqv? 1 n) (caar m)
      (do ((j (+ -1 n) (+ -1 j))
	   (ans 0 (+ ans (* (list-ref (car m) j) (cofactor m 0 j)))))
	  ((negative? j) ans))))
