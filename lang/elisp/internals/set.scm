(define-module (lang elisp internals set)
  #:use-module (lang elisp internals evaluation)
  #:use-module (lang elisp internals signal)
  #:export (set))

;; Set SYM's variable value to VAL, and return VAL.
(define (set sym val)
  (module-define! the-elisp-module sym val)
  val)
