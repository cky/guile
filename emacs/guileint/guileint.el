;;; NAME:	 guileint.el
;;; SYNOPSIS:	 A Guile/Emacs interface prototype
;;; VERSION:	 1.5
;;; LAST CHANGE: 2002-10-19
;;; CREATED:	 1997-07-17
;;; AUTHOR:	 Mikael Djurfeldt <djurfeldt@nada.kth.se>
;;; COPYRIGHT:	 (C) 1997, 2002 Mikael Djurfeldt
;;;
;;;  Verbatim copies of this file may be freely redistributed.
;;;
;;;  Modified versions of this file may be redistributed provided that this
;;;  notice remains unchanged, the file contains prominent notice of
;;;  author and time of modifications, and redistribution of the file
;;;  is not further restricted in any way.
;;;
;;;  This file is distributed `as is', without warranties of any kind.
;;;
;;; REQUIREMENTS:
;;;
;;; USAGE:
;;;
;;; BUGS:
;;;
;;;
;;; Setup load-path

(if (featurep 'guileint)
    nil
  
(require 'cl-19 "cl")

(defconst guileint-init-file "guileint")

(defvar guileint-emacs-dir nil)
(let ((pathlist (getenv "EMACSSITELOAD")))
  (if (and pathlist
	   (string-match (concat "\\(\\(/[^:/]+\\)*\\)/?"
				 guileint-init-file
				 "\\(\.elc?\\)?\\(:\\|\\'\\)")
			 pathlist))
      (setq guileint-emacs-dir (match-string 1 pathlist))))

(defvar guileint-default-load-path load-path)
(setq load-path
      (append (list
	       guileint-emacs-dir
	       )
	      guileint-default-load-path
	      '(
		)))

(setq scheme-program-name
      (let ((v (getenv "SCHEME_PROGRAM_NAME")))
	(or v
	    (concat "guile"
		    (and window-system " --emacs")))))

;;; Select buffers to pop up as separate windows
(if window-system
    (progn
      (defvar default-special-display-buffer-names
	special-display-buffer-names)
      (setq special-display-buffer-names
	    (union default-special-display-buffer-names '("*scheme*")))

      (setq same-window-buffer-names
	    (delete "*scheme*" same-window-buffer-names))

      (setq special-display-frame-alist
	    '((height . 24) (width . 80) (unsplittable . t)))
      ))

;;; Do things to support lisp-hacking better
(if (equal (substring emacs-version 0 2) "19")
    ;; Emacs version 19 specific initializations
    (progn
      (copy-face 'default 'paren)
      (condition-case err
	  (make-face-bold 'paren)
	(error))
      (setq show-paren-face 'paren)
      (require 'paren)
      ;; The old parenthesis matcher has the advantage of displaying
      ;; non-visible matching parenthesis in the minibuffer.
      ;; Since paren.el adds (setq blink-paren-function nil) to the
      ;; window-setup-hook it's necessary to put this setq there
      ;; also.
      (add-hook 'window-setup-hook (function restore-blink-paren) t)
      (setq blink-matching-delay 0.5)
      ))

(defun restore-blink-paren ()
  (interactive)
  (setq blink-matching-paren-on-screen t)
  (set-face-underline-p 'paren t))

;;; Menus
;;;

(require 'defmenu)

;(setq menu-bar-final-items
;      '(completion inout signals scheme help-menu))
(setq menu-bar-final-items
      '(interpret scheme help-menu))

;; The global menu
;;
(define-menu global-map 'interpret "Interpret"
  '(("Guile" run-scheme (not (comint-check-proc "*scheme*")))
    ("Switch to *scheme*" guile-switch-to-scheme
     (comint-check-proc "*scheme*"))))

(load "inda-scheme")

(provide 'guileint)
)
