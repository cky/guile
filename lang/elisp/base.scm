(define-module (lang elisp base)

  ;; Be pure.  Nothing in this module requires most of the standard
  ;; Guile builtins, and it creates a problem if this module has
  ;; access to them, as @bind can dynamically change their values.
  #:pure

  ;; But we do need a few builtins - import them here.
  #:use-module ((guile) #:select (@fop @bind nil-cond))

  ;; {Elisp Primitives}
  ;;
  ;; In other words, Scheme definitions of elisp primitives.  This
  ;; should (ultimately) include everything that Emacs defines in C.
  #:use-module (lang elisp primitives buffers)
  #:use-module (lang elisp primitives features)
  #:use-module (lang elisp primitives format)
  #:use-module (lang elisp primitives fns)
  #:use-module (lang elisp primitives guile)
  #:use-module (lang elisp primitives keymaps)
  #:use-module (lang elisp primitives lists)
  #:use-module (lang elisp primitives load)
  #:use-module (lang elisp primitives match)
  #:use-module (lang elisp primitives numbers)
  #:use-module (lang elisp primitives pure)
  #:use-module (lang elisp primitives read)
  #:use-module (lang elisp primitives signal)
  #:use-module (lang elisp primitives strings)
  #:use-module (lang elisp primitives symprop)
  #:use-module (lang elisp primitives syntax)
  #:use-module (lang elisp primitives system)
  #:use-module (lang elisp primitives time)

  ;; Now switch into Emacs Lisp syntax.
  #:use-syntax (lang elisp transform))

;(use-modules (lang elisp transform))
;(read-set! keywords 'prefix)
;(set-module-transformer! (current-module) transformer)

;;; Everything below here is written in Elisp.

(defun load-emacs ()
  (message "Calling loadup.el to clothe the bare Emacs...")
  (load "loadup.el")
  (message "Guile Emacs now fully clothed"))
