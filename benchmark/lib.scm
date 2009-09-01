;; -*- Scheme -*-
;;
;; A library of dumb functions that may be used to benchmark Guile-VM.


;; The comments are from Ludovic, a while ago. The speedups now are much
;; more significant (all over 2x, sometimes 8x).

(define (fibo x)
  (if (or (= x 1) (= x 2))
      1
      (+ (fibo (- x 1))
	 (fibo (- x 2)))))

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
;; Disassembly of #<objcode b79bdf28>:

;; nlocs = 0  nexts = 0

;;    0    (make-int8 64)                  ;; 64
;;    2    (load-symbol "guile-user")      ;; guile-user
;;   14    (list 0 1)                      ;; 1 element
;;   17    (load-symbol "loop")            ;; loop
;;   23    (link-later)
;;   24    (vector 0 1)                    ;; 1 element
;;   27    (make-int8 0)                   ;; 0
;;   29    (load-symbol "n")               ;; n
;;   32    (make-false)                    ;; #f
;;   33    (make-int8 0)                   ;; 0
;;   35    (list 0 3)                      ;; 3 elements
;;   38    (list 0 2)                      ;; 2 elements
;;   41    (list 0 1)                      ;; 1 element
;;   44    (make-int8 5)                   ;; 5
;;   46    (make-false)                    ;; #f
;;   47    (cons)
;;   48    (make-int8 18)                  ;; 18
;;   50    (make-false)                    ;; #f
;;   51    (cons)
;;   52    (make-int8 20)                  ;; 20
;;   54    (make-false)                    ;; #f
;;   55    (cons)
;;   56    (list 0 4)                      ;; 4 elements
;;   59    (load-program ##{66}#)
;;   81    (define "loop")
;;   87    (variable-set)
;;   88    (void)
;;   89    (return)

;; Bytecode ##{66}#:

;;    0    (make-int8 0)                   ;; 0
;;    2    (local-ref 0)
;;    4    (ee?)
;;    5    (br-if-not 0 3)                 ;; -> 11
;;    8    (make-int8 0)                   ;; 0
;;   10    (return)
;;   11    (toplevel-ref 0)
;;   13    (local-ref 0)
;;   15    (make-int8 1)                   ;; 1
;;   17    (sub)
;;   18    (tail-call 1)

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
