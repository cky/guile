(define-module (lang elisp primitives fns)
  #:use-module (lang elisp internals fset))

(fset 'fset fset)
(fset 'defalias fset)

(fset 'apply elisp-apply)

(fset 'funcall
      (lambda (function . args)
	(elisp-apply function args)))

(fset 'interactive-p
      (lambda ()
	#f))

(fset 'commandp
      (lambda (sym)
	(if (interactive-spec (fref sym)) #t #f)))

(fset 'fboundp
      (lambda (sym)
	(variable? (symbol-fref sym))))

(fset 'symbol-function fref/error-if-void)

(fset 'macroexpand macroexpand)

(fset 'subrp
      (lambda (obj)
	(not (not-subr? obj))))

(fset 'byte-code-function-p
      (lambda (object)
	#f))

(fset 'run-hooks
      (lambda (hooks)
	(cond ((null hooks))
	      ((list? hooks)
	       (for-each (lambda (hook)
			   (elisp-apply hook '()))
			 hooks))
	      (else
	       (elisp-apply hooks '())))))
