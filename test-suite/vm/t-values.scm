(list (call-with-values
          (lambda () (values 1 2))
        (lambda (x y) (cons x y)))
      
      ;; the start-stack forces a bounce through the interpreter
      (call-with-values
          (lambda () (start-stack 'foo (values 1 2)))
        list)

      (call-with-values
          (lambda () (apply values '(1)))
        list))

