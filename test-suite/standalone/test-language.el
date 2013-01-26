;; Sample Elisp code for `test-language'.

(defun fib (n)
  "Anything but a fib."
  (if (<= n 1)
      n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(or (= 13 (fib 7))
    (error "Something's wrong!"))
