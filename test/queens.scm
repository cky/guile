(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define empty-board '())

(define (rest bs k rest-of-queens)
  (map (lambda (new-row)
	 (adjoin-position new-row k rest-of-queens))
       (enumerate-interval 1 bs)))

(define (queen-cols board-size k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? k positions))
		(flatmap (lambda (r) (rest board-size k r))
			 (queen-cols board-size (- k 1))))))

(define (queens board-size)
  (queen-cols board-size board-size))

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list new-row)))

(define (safe? k positions)
  (let ((new  (car (last-pair positions)))
	(bottom (car positions)))
    (cond ((= k 1) #t)
	  ((= new bottom) #f)
	  ((or (= new (- bottom (- k 1))) (= new (+ bottom (- k 1)))) #f)
	  (else (safe? (- k 1) (cdr positions))))))

(test (queens 4) '((2 4 1 3) (3 1 4 2)))
