(define-module (lang elisp primitives symprop)
  #:use-module (lang elisp internals set)
  #:use-module (lang elisp internals fset)
  #:use-module (lang elisp internals evaluation)
  #:use-module (ice-9 optargs))

;;; {Elisp Exports}

(fset 'put set-symbol-property!)

(fset 'get symbol-property)

(fset 'set set)

(fset 'set-default 'set)

(fset 'boundp
      (lambda (sym)
	(module-defined? the-elisp-module sym)))

(fset 'default-boundp 'boundp)

(fset 'symbol-value
      (lambda (sym)
	(if (module-defined? the-elisp-module sym)
	    (module-ref the-elisp-module sym)
	    (error "Symbol's value as variable is void:" sym))))

(fset 'default-value 'symbol-value)

(fset 'symbolp
      (lambda (object)
	(or (symbol? object)
	    (keyword? object))))

(fset 'local-variable-if-set-p
      (lambda* (variable #:optional buffer)
	#f))

(fset 'symbol-name symbol->string)
