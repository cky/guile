;; -*- Scheme -*-
;;
;; A library of dumb functions that may be used to benchmark Guile-VM.


(define (fibo x)
  (if (= 1 x)
      1
      (+ x
	 (fibo (1- x)))))

(define (g-c-d x y)
  (if (= x y)
      x
      (if (< x y)
	  (g-c-d x (- y x))
	  (g-c-d (- x y) y))))

(define (loop n)
  ;; This one shows that procedure calls are no faster than within the
  ;; interpreter: the VM yields no performance improvement.
  (if (= 0 n)
      0
      (loop (1- n))))

;; Disassembly of `loop'
;;
; Disassembly of #<objcode 302360b0>:

; nlocs = 0  nexts = 0

;    0    (make-int8 64)                  ;; 64
;    2    (link "=")
;    5    (link "loop")
;   11    (link "1-")
;   15    (vector 3)
;   17    (make-int8:0)                   ;; 0
;   18    (load-symbol "n")               ;; n
;   28    (make-false)                    ;; #f
;   29    (make-int8:0)                   ;; 0
;   30    (list 3)
;   32    (list 2)
;   34    (list 1)
;   36    (make-int8 8)                   ;; 8
;   38    (make-int8 2)                   ;; 2
;   40    (make-int8 6)                   ;; 6
;   42    (cons)
;   43    (cons)
;   44    (make-int8 23)                  ;; 23
;   46    (make-int8 4)                   ;; 4
;   48    (make-int8 12)                  ;; 12
;   50    (cons)
;   51    (cons)
;   52    (make-int8 25)                  ;; 25
;   54    (make-int8 4)                   ;; 4
;   56    (make-int8 6)                   ;; 6
;   42    (cons)
;   43    (cons)
;   44    (make-int8 23)                  ;; 23
;   46    (make-int8 4)                   ;; 4
;   48    (make-int8 12)                  ;; 12
;   50    (cons)
;   51    (cons)
;   52    (make-int8 25)                  ;; 25
;   54    (make-int8 4)                   ;; 4
;   56    (make-int8 6)                   ;; 6
;   58    (cons)
;   59    (cons)
;   60    (list 4)
;   62    load-program ##{201}#
;   89    (link "loop")
;   95    (variable-set)
;   96    (void)
;   97    (return)

; Bytecode ##{201}#:

;    0    (object-ref 0)
;    2    (variable-ref)
;    3    (make-int8:0)                   ;; 0
;    4    (local-ref 0)
;    6    (call 2)
;    8    (br-if-not 0 2)                 ;; -> 13
;   11    (make-int8:0)                   ;; 0
;   12    (return)
;   13    (object-ref 1)
;   15    (variable-ref)
;   16    (object-ref 2)
;   18    (variable-ref)
;   19    (local-ref 0)
;   21    (call 1)
;   23    (tail-call 1)


(define (loopi n)
  ;; Same as `loop'.
  (let loopi ((n n))
    (if (= 0 n)
	0
	(loopi (1- n)))))

(define (do-loop n)
  ;; Same as `loop' using `do'.
  (do ((i n (1- i)))
      ((= 0 i))
    ;; do nothing
    ))


(define (do-cons x)
  ;; This one shows that the built-in `cons' instruction yields a significant
  ;; improvement (speedup: 1.5).
  (let loop ((x x)
	     (result '()))
    (if (<= x 0)
	result
	(loop (1- x) (cons x result)))))

(define big-list (iota 500000))

(define (copy-list lst)
  ;; Speedup: 5.9.
  (let loop ((lst lst)
	     (result '()))
    (if (null? lst)
	result
	(loop (cdr lst)
	      (cons (car lst) result)))))
