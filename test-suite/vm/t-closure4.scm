(define (extract-symbols exp)
  (define (process x out cont)
    (cond ((pair? x)
           (process (car x)
                    out
                    (lambda (car-x out)
                      ;; used to have a bug here whereby `x' was
                      ;; modified in the self-tail-recursion to (process
                      ;; (cdr x) ...), because we didn't allocate fresh
                      ;; externals when doing self-tail-recursion.
                      (process (cdr x)
                               out
                               (lambda (cdr-x out)
                                 (cont (cons car-x cdr-x)
                                       out))))))
          ((symbol? x)
           (cont x (cons x out)))
          (else
           (cont x out))))
  (process exp '() (lambda (x out) out)))

(extract-symbols '(a b . c))
