(define-module (readline-activator))

(define-public (activate-readline)
  (if (not (provided? 'readline))
      (scm-error 'misc-error
		 'activate-readline
		 "readline is not provided in this Guile installation"
		 '()
		 '()))
  (save-module-excursion
   (lambda ()
     (define-module (guile))
     (dynamic-call "scm_init_readline" (dynamic-link "libguilereadline.so"))
     (if (isatty? (current-input-port))
	 (begin
	   (define-module (guile) :use-module (ice-9 readline))
	   (define-module (guile-user) :use-module (ice-9 readline)))))))
