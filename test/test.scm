
(set! %load-path (cons ".." %load-path))
(use-modules (vm vm))

(define (test a b)
  (if (equal? a b)
      (display "OK\n")
      (display "failed\n")))

(let ((file (cadr (command-line))))
  (format #t "Testing ~S...\n" file)
  (load file))
