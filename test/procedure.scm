(define length
  (lambda (ls)
    (if (null? ls)
	0
	(+ (length (cdr ls)) 1))))

(test (length '()) 0)
(test (length '(a)) 1)
(test (length '(a b)) 2)

(define remv
  (lambda (x ls)
    (cond
     ((null? ls) '())
     ((eqv? (car ls) x) (remv x (cdr ls)))
     (else (cons (car ls) (remv x (cdr ls)))))))

(test (remv 'a '(a b b d)) '(b b d))
(test (remv 'b '(a b b d)) '(a d))
(test (remv 'c '(a b b d)) '(a b b d))
(test (remv 'd '(a b b d)) '(a b b))

(define tree-copy
  (lambda (tr)
    (if (not (pair? tr))
	tr
	(cons (tree-copy (car tr))
	      (tree-copy (cdr tr))))))

(test (tree-copy '((a . b) . c)) '((a . b) . c))

(define quadratic-formula
  (lambda (a b c)
    (let ((root1 0) (root2 0) (minusb 0) (radical 0) (divisor 0))
      (set! minusb (- 0 b))
      (set! radical (sqrt (- (* b b) (* 4 (* a c)))))
      (set! divisor (* 2 a))
      (set! root1 (/ (+ minusb radical) divisor))
      (set! root2 (/ (- minusb radical) divisor))
      (cons root1 root2))))

(test (quadratic-formula 2 -4 -6) '(3.0 . -1.0))

(define count
  (let ((n 0))
    (lambda ()
      (set! n (1+ n))
      n)))

(test (count) 1)
(test (count) 2)

(define (fibonacci i)
  (cond ((= i 0) 0)
	((= i 1) 1)
	(else (+ (fibonacci (- i 1)) (fibonacci (- i 2))))))

(test (fibonacci 0) 0)
(test (fibonacci 5) 5)
(test (fibonacci 10) 55)
