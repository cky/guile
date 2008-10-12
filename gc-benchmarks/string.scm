;;; From from http://www.ccs.neu.edu/home/will/Twobit/KVW/string.txt .
; string test
; (try 100000)

(define s "abcdef")

(define (grow)
	(set! s (string-append "123" s "456" s "789"))
	(set! s (string-append
		(substring s (quotient (string-length s) 2) (string-length s))
		(substring s 0 (+ 1 (quotient (string-length s) 2)))))
	s)

(define (trial n)
	(do ((i 0 (+ i 1)))
		((> (string-length s) n) (string-length s))
		(grow)))

(define (try n)
	(do ((i 0 (+ i 1)))
		((>= i 10) (string-length s))
		(set! s "abcdef")
		(trial n)))

(try 50000000)