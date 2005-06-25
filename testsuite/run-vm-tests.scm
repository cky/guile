;;; A simple test-running script.

(use-modules (system vm core)
	     (system vm disasm)
	     (system base compile)
	     (system base language)

	     (srfi srfi-1))


(define *scheme* (lookup-language 'scheme))

(define (fetch-sexp-from-file file)
  (with-input-from-file file
    (lambda ()
      (let loop ((sexp (read))
		 (result '()))
	(if (eof-object? sexp)
	    (cons 'begin (reverse result))
	    (loop (read) (cons sexp result)))))))

(define (compile-to-objcode sexp)
  "Compile the expression @var{sexp} into a VM program and return it."
  (compile-in sexp (current-module) *scheme*))

(define (run-vm-program objcode)
  "Run VM program contained into @var{objcode}."
  (vm-load (the-vm) objcode))

(define (run-test-from-file file)
  "Run test from source file @var{file} and return a value indicating whether
it succeeded."
  (run-vm-program (compile-to-objcode (fetch-sexp-from-file file))))


(define-macro (watch-proc proc-name str)
  `(let ((orig-proc ,proc-name))
     (set! ,proc-name
	   (lambda args
	     (format #t (string-append ,str "...  "))
	     (apply orig-proc args)))))

(watch-proc fetch-sexp-from-file  "reading")
(watch-proc compile-to-objcode    "compiling")
(watch-proc run-vm-program        "running")


;; The program.

(define (run-vm-tests files)
  (let* ((res (map (lambda (file)
		     (format #t "running `~a'...  " file)
		     (if (catch #t
				(lambda ()
				  (run-test-from-file file))
				(lambda (key . args)
				  (format #t "[~a/~a] " key args)
				  #f))
			 (format #t "ok~%")
			 (begin (format #t "FAILED~%") #f)))
		   files))
	 (total (length files))
	 (failed (length (filter not res))))

    (if (= 0 failed)
	(begin
	  (format #t "~%All ~a tests passed~%" total)
	  (exit 0))
	(begin
	  (format #t "~%~a tests failed out of ~a~%"
		  failed total)
	  (exit failed)))))

