;;; NAME:	 inda-scheme.el
;;; SYNOPSIS:	 Customizations of the scheme modes for
;;;              the INDA course at NADA/KTH
;;; VERSION:	 1.0
;;; LAST CHANGE: 950827
;;; CREATED:	 950827
;;; AUTHOR:	 Mikael Djurfeldt <djurfeldt@nada.kth.se>
;;; COPYRIGHT:	 (C) Mikael Djurfeldt 1995
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

(require 'guile-init)

;;; Customizations of the scheme modes

(defun inda-scheme-mode-initializations ()
  (define-key scheme-mode-map "\r" 'newline-and-indent)
  ;(define-key scheme-mode-map "\C-c\C-e" 'scheme-send-definition-and-go)
  (define-key scheme-mode-map [S-mouse-2] 'guile-frame-eval-at-click)
  (define-key scheme-mode-map [triple-mouse-1] 'inda-mark-sexp) ;*fixme*
  (define-key scheme-mode-map "\C-c\C-b" 'scheme-send-buffer)
  (define-key scheme-mode-map "(" 'scheme-electric-open-paren)
  (define-key scheme-mode-map "[" 'scheme-electric-open-paren)
  (define-key scheme-mode-map ")" 'scheme-close-paren)
  (define-key scheme-mode-map "]" 'scheme-close-paren)
  (define-key scheme-mode-map "\M-?" 'guile-list-completions)
  (define-key scheme-mode-map "\C-cd" 'guile-describe-variable)
  (define-key scheme-mode-map "\M-\t" 'guile-complete-symbol)
  (put 'procedure->macro 'scheme-indent-function 0)
  (put 'procedure->memoizing-macro 'scheme-indent-function 0)
  (put 'bind 'scheme-indent-function 1)
  (put 'letrec* 'scheme-indent-function 1)
  (put 'syntax-rules 'scheme-indent-function 1)
  (put 'syntax-case 'scheme-indent-function 2)
  (put 'define-syntax 'scheme-indent-function 1)
  (put 'with-syntax 'scheme-indent-function 1))

(add-hook 'scheme-mode-hook (function inda-scheme-mode-initializations))

(defun scheme-electric-open-paren ()
  (interactive)
  (insert last-input-char)
  (let ((old-point (point)))
    (indent-for-tab-command)
    (if (not (eq (char-after (1- (point))) last-input-char))
	(goto-char old-point))))

(defun scheme-close-paren ()
  (interactive)
  (insert last-input-char)
  (if (guile-enhancedp)
      (guile-repair-overlays))
  (if blink-paren-function
      (funcall blink-paren-function)))

(defun inda-send-definition (click)
  "Position point and send definition to the inferior Scheme process."
  (interactive "e")
  (mouse-set-point click)
  (sit-for 0)
  (scheme-send-definition))

(defun inda-mark-sexp ()
  (interactive)
  (beginning-of-defun)
  (mark-sexp))

(defvar inda-read-only-overlay nil)

(defun inda-inferior-initializations ()
  (setq guile-kill-buffer-on-death t)
  ;; The following seems already to be done in comint-mode...
  ;;(add-hook 'pre-command-hook (function comint-preinput-scroll-to-bottom))
  (setq comint-scroll-to-bottom-on-input 'this)
  (setq comint-scroll-to-bottom-on-output nil)

  ;; Some key bindings.
  (define-key inferior-scheme-mode-map "\C-a" 'comint-bol)
  (define-key inferior-scheme-mode-map [C-a] 'comint-bol)
  (define-key inferior-scheme-mode-map "\C-c\C-a" 'beginning-of-line)
  (define-key inferior-scheme-mode-map [C-c C-a] 'beginning-of-line)
  (define-key inferior-scheme-mode-map "\r" 'guile-send-input)
  (define-key inferior-scheme-mode-map "\t" 'guile-indent-or-complete)
  (define-key inferior-scheme-mode-map "\M-?" 'guile-list-completions)
  (define-key inferior-scheme-mode-map "\C-cd" 'guile-describe-variable)
  (define-key inferior-scheme-mode-map [C-c d] 'guile-describe-variable)

  ;; Create the read-only overlay.
  (make-local-variable 'inda-read-only-overlay)
  (cond ((not (overlayp inda-read-only-overlay))
	 (setq inda-read-only-overlay (make-overlay 1 (point)))
	 (overlay-put inda-read-only-overlay 'modification-hooks
		      '(inda-barf-at-modifications))))

  ;; Disable font-lock
  (make-local-variable 'font-lock-fontify-region-function)
  (setq font-lock-fontify-region-function 'ignore)

  ;; We don't want all comint modes to have these values
  (add-hook 'comint-input-filter-functions
	    (function inda-make-input-memory) 'append 'local)
  (add-hook 'comint-input-filter-functions
	    (function inda-extend-read-only-overlay) 'append 'local)
  (add-hook 'comint-output-filter-functions
	    (function inda-extend-read-only-overlay) 'append 'local)
  (add-hook 'comint-output-filter-functions
	    (function inda-reset-guile-last-output) 'append 'local)
  ;; This is a bit kludgy...
  (add-hook 'scheme-enter-input-wait-hook (function inda-boldify-previous-character))
)

;; No message about reason when process dies

(setq guile-insert-reason nil)

(add-hook 'inferior-scheme-mode-hook
	  (function inda-inferior-initializations)
	  'append)

(require 'defmenu)

;; Scheme mode menu
;;
(fset 'scheme-advanced-menu
  (make-menu
   "Advanced"
   '(
     ("Sync with scheme" guile-sync-with-scheme
      (and (> guile-n-enhanced-buffers 0)
	   (not (guile-synchronizedp))
	   scheme-ready-p))
     ("Re-eval buffer" guile-reread-buffer (and (guile-attachedp)
						scheme-ready-p))
     ()
     ("Enhanced edit" guile-enhanced-edit (not (guile-enhancedp)))
     ("Normal edit" guile-normal-edit (and (guile-enhancedp)
					   (not (guile-attachedp))))
     ()
     ("Eval definition" scheme-send-definition (comint-check-proc "*scheme*"))
     ("Eval region" scheme-send-region (comint-check-proc "*scheme*"))
     ("Eval buffer" scheme-send-buffer (comint-check-proc "*scheme*"))
     )))

(define-menu scheme-mode-map 'scheme "Scheme"
  '(
    ("Eval definition" scheme-send-definition (comint-check-proc "*scheme*"))
    ("Eval region" scheme-send-region (comint-check-proc "*scheme*"))
    ("Eval buffer" scheme-send-buffer (comint-check-proc "*scheme*"))
    ("Eval all changes" guile-send-changes (comint-check-proc "*scheme*"))
    ()
    ("Indent buffer" indent-buffer)
    ("Indent region" indent-region)
    ("Indent definition" indent-defun)
    ()
    ("Enhanced edit" guile-enhanced-edit (not (guile-enhancedp)))
    ("Normal edit" guile-normal-edit (and (guile-enhancedp)
					  (not (guile-attachedp))))
    ()
    ("Attach buffer" guile-attach-buffer (and (comint-check-proc "*scheme*")
					      scheme-ready-p
					      (not (guile-attachedp))))
    ("Detach buffer" guile-detach-buffer (guile-attachedp))
    ()
    ("Re-init buffer" guile-reread-buffer (and (guile-attachedp)
					       scheme-ready-p))
    ("Find bad expressions" guile-check-all (> guile-n-enhanced-buffers 0))
    ))

;(define-key scheme-mode-map [menu-bar interpret] 'undefined)

;; Inferior scheme menu
;;
(define-menu inferior-scheme-mode-map 'scheme "Scheme"
  '(("Start scheme" run-scheme (not (comint-check-proc "*scheme*")))
    ("Restart scheme" guile-restart-scheme (comint-check-proc "*scheme*"))
    ("Exit scheme" guile-exit-scheme (comint-check-proc "*scheme*"))
    ()
    ("Load file..." guile-load-file
     (and (comint-check-proc "*scheme*")
	  scheme-ready-p))
    ("Eval all changes" guile-send-changes (comint-check-proc "*scheme*"))
    ("Find bad expressions" guile-check-all (comint-check-proc "*scheme*"))
    ()
    ("Clear transcript" guile-clear-transcript (comint-check-proc "*scheme*"))))

(define-key inferior-scheme-mode-map [menu-bar interpret] 'undefined)
