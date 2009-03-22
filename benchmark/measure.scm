#!/bin/sh
# aside from this initial boilerplate, this is actually -*- scheme -*- code
main='(module-ref (resolve-module '\''(measure)) '\'main')'
exec ${GUILE-guile} -l $0 -c "(apply $main (cdr (command-line)))" "$@"
!#

;; A simple interpreter vs. VM performance comparison tool
;;

(define-module (measure)
  :export (measure)
  :use-module (system vm vm)
  :use-module (system base compile)
  :use-module (system base language))


(define (time-for-eval sexp eval)
  (let ((before (tms:utime (times))))
    (eval sexp)
    (let ((elapsed (- (tms:utime (times)) before)))
      (format #t "elapsed time: ~a~%" elapsed)
      elapsed)))

(define *scheme* (lookup-language 'scheme))


(define (measure . args)
  (if (< (length args) 2)
      (begin
	(format #t "Usage: measure SEXP FILE-TO-LOAD...~%")
	(format #t "~%")
	(format #t "Example: measure '(loop 23424)' lib.scm~%~%")
	(exit 1)))
  (for-each load (cdr args))
  (let* ((sexp (with-input-from-string (car args)
		 (lambda ()
		   (read))))
	 (eval-here (lambda (sexp) (eval sexp (current-module))))
	 (proc-name (car sexp))
	 (proc-source (procedure-source (eval proc-name (current-module))))
	 (% (format #t "proc: ~a~%source: ~a~%" proc-name proc-source))
	 (time-interpreted (time-for-eval sexp eval-here))
	 (& (if (defined? proc-name)
		(eval `(set! ,proc-name #f) (current-module))
		(format #t "unbound~%")))
	 (the-program (compile proc-source))

	 (time-compiled (time-for-eval `(,proc-name ,@(cdr sexp))
				       (lambda (sexp)
					 (eval `(begin
						  (define ,proc-name
						    ,the-program)
						  ,sexp)
					       (current-module))))))

    (format #t "proc:        ~a => ~a~%"
	    proc-name (eval proc-name (current-module)))
    (format #t "interpreted: ~a~%" time-interpreted)
    (format #t "compiled:    ~a~%" time-compiled)
    (format #t "speedup:     ~a~%"
	    (exact->inexact (/ time-interpreted time-compiled)))
    0))

(define main measure)
