;; Are macros well-expanded at compilation-time?

(defmacro minus-binary (a b)
  `(- ,a ,b))

(define-macro (plus . args)
  `(let ((res (+ ,@args)))
     ;;(format #t "plus -> ~a~%" res)
     res))


(plus (let* ((x (minus-binary 12 7)) ;; 5
	     (y (minus-binary x 1))) ;; 4
	(plus x y 5)) ;; 14
      12              ;; 26
      (expt 2 3))     ;; => 34

