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

(defun inda-barf-at-modifications (&rest args)
  (or inhibit-read-only
      (error "Attempt to modify read-only text")))

(defun inda-boldify-previous-character ()
  ;; Must check this so that we don't point outside buffer...
  (if (> (point) (point-min))
      (let ((inhibit-read-only t))
	(put-text-property (1- (point)) (point) 'face 'bold))))

(defun inda-make-input-memory (string)
  ;; If input consists of many lines, the read-only overlay will
  ;; cover the previous line, so we have to disable the protection.
  (let ((inhibit-read-only t))
      ;(setq n (1+ n)
      ;      l (append l (list (list n 'input-filter string))))
    (if (marker-position guile-last-output-end)
	(add-text-properties guile-last-output-end (1- (point))
			     '(input-memory t rear-nonsticky t mouse-face highlight)))))

(defun inda-reset-guile-last-output (string)
  ;(setq n (1+ n)
  ;      l (append l (list (list n 'output-filter string))))
  (if (not scheme-ready-p)
      (set-marker guile-last-output-end nil)))

;; Should rather be implemented with advice.
(defun inda-mouse-yank-at-click (click arg)
  "Insert the last stretch of killed text at the position clicked on.
Also move point to one end of the text thus inserted (normally the end).
Prefix arguments are interpreted as with \\[yank].
If `mouse-yank-at-point' is non-nil, insert at point
regardless of where you click."
  (interactive "e\nP")
  (if (get-char-property (posn-point (event-start click)) 'input-memory)
      (if (memq 'shift (event-modifiers (car click)))
	  (inda-insert-input-memory click)
	(inda-insert-input-memory-and-send click))
    ;; Give temporary modes such as isearch a chance to turn off.
    (run-hooks 'mouse-leave-buffer-hook)
    (or mouse-yank-at-point (mouse-set-point click))
    (setq this-command 'yank)
    (yank arg)))

(defun inda-insert-input-memory (event)
  (interactive "e")
  (let* ((pos (posn-point (event-start event)))
	 (beg (previous-single-property-change (1+ pos) 'mouse-face))
	 (end (next-single-property-change pos 'mouse-face)))
    (goto-char (point-max))
    (let ((input-start (point)))
      (comint-kill-input)
      (insert (buffer-substring beg end))
      (add-text-properties input-start (point)
			   '(mouse-face nil
					rear-nonsticky nil
					input-memory nil)))))

(defun inda-insert-input-memory-and-send (event)
  (interactive "e")
  (inda-insert-input-memory event)
  (guile-send-input))

(defun inda-extend-read-only-overlay (string)
  (if guile-input-sent-p
      (let ((inhibit-read-only t))
	(move-overlay inda-read-only-overlay (point-min) (point)))))

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

  ;; Mouse bindings.
  (define-key inferior-scheme-mode-map [mouse-2] 'inda-mouse-yank-at-click)
  (define-key inferior-scheme-mode-map [S-mouse-2] 'inda-mouse-yank-at-click)

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

(defun scheme-send-buffer ()
  "Send the current buffer to the inferior Scheme process."
  (interactive)
   (let (begin end)
     (save-excursion
       (goto-char (point-max))
       (setq end (point))
       (goto-char (point-min))
       (setq begin (point)))
     (scheme-send-region begin end)))

(defun indent-buffer ()
  "Indent entire buffer."
  (interactive)
  (save-excursion
   (end-of-buffer)
   (let ((end (point)))
     (beginning-of-buffer)
     (indent-region (point) end nil))))

(defun indent-defun ()
  "Indent lisp definition."
  (interactive)
  (save-excursion
   (end-of-defun)
   (let ((end (point)))
     (beginning-of-defun)
     (indent-region (point) end nil))))

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
