;; Test that nonlocal exits of the VM work.

(begin
  (define (foo thunk)
    (catch #t thunk (lambda args args)))
  (foo
   (lambda ()
     (let ((a 'one))
       (1+ a)))))
       
