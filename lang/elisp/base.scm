(define-module (lang elisp base))

;;; {Elisp Primitives}
;;;
;;; In other words, Scheme definitions of elisp primitives.  This
;;; should (ultimately) include everything that Emacs defines in C.

(use-modules (lang elisp primitives buffers)
	     (lang elisp primitives features)
	     (lang elisp primitives format)
	     (lang elisp primitives fns)
	     (lang elisp primitives guile)
	     (lang elisp primitives keymaps)
	     (lang elisp primitives lists)
	     (lang elisp primitives load)
	     (lang elisp primitives match)
	     (lang elisp primitives numbers)
	     (lang elisp primitives pure)
	     (lang elisp primitives read)
	     (lang elisp primitives signal)
	     (lang elisp primitives strings)
	     (lang elisp primitives symprop)
	     (lang elisp primitives system)
	     (lang elisp primitives time))

;;; Now switch into Emacs Lisp syntax.

(use-modules (lang elisp transform))
(read-set! keywords 'prefix)
(read-set! language 'elisp)
(set-module-transformer! (current-module) transformer)

;;; Everything below here is written in Elisp.

(defun load-emacs ()
  (message "Calling loadup.el to clothe the bare Emacs...")
  (load "loadup.el")
  (message "Guile Emacs now fully clothed"))
