;;; @(#) guile.el -- A GNU Emacs interface to Guile
;;; @(#) $Keywords: guile, comint, scheme-mode $

;; Copyright (C) 1995, 2002 Mikael Djurfeldt

;; LCD Archive Entry:
;; guile|djurfeldt@nada.kth.se|
;; A GNU Emacs extension which |
;; $Date: 2003-08-20 19:00:44 $|$Revision: 1.1 $|~/misc/guile.el.Z|

;; Author:  Mikael Djurfeldt <djurfeldt@nada.kth.se>
;; Version: 1.5.2

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; Requirements:
;;
;; Usage:
;;
;; Bugs:
;;
;;
;;; *************************************************************************
;;; * This is code is currently under development                           *
;;; * Mail any problems to djurfeldt@nada.kth.se                            *
;;; *************************************************************************

(require 'cl)
(require 'fcreate)

(defvar guile-auto-attach nil)

(defvar guile-load-hook nil
  "*Hook run when file is loaded")

;;(require 'cmuscheme)
(load "comint")      ; `comint' and `cmuscheme' are already loaded.
(load "cmuscheme")   ; We need to replace them.

;; Faces are set in the cond expression below.

(defvar guile-error-face nil
  "Face used to highlight erroneous scheme forms.")

(defvar guile-backtrace-mouse-face nil
  "Face used when the mouse is over a backtrace frame.")

(defvar guile-modified-face nil
  "Face for modified top-level forms in scheme-mode buffers.")

(defvar guile-broken-face nil
  "Face for broken top-level forms in scheme-mode buffers.")

;; These faces are used during debugging of the list parsing code.

(defvar guile-unmodified-face-1 nil)
(defvar guile-unmodified-face-2 nil)
(defvar guile-modified-face-1 nil)
(defvar guile-modified-face-2 nil)
(defvar guile-broken-face-1 nil)
(defvar guile-broken-face-2 nil)

;;; Customization
;;;

(defvar guile-backtrace-in-source-window t
  "*If non-nil, let backtrace windows appear in bottom of source window.
This only occurs if the erring expression can be located.")

(defvar guile-show-runlight-in-scheme-mode nil
  "*If non-nil, show process status also in attached scheme-mode buffers.
Otherwise the mode-line shows if the buffer is attached or not.")

(defvar guile-default-enhanced-edit t
  "If non-nil, automatically enter enhanced edit mode for scheme buffers.")

(defvar guile-popup-restart-on-death t)

(defvar guile-popup-restart-on-stop t)

(defvar guile-insert-reason t)

(defvar guile-kill-buffer-on-death nil)

(defvar guile-process-timeout 500
  "Milliseconds")

(defconst guile-backtrace-buffer-name "*Scheme Backtrace*")

(defconst guile-error-buffer-name "*Scheme Error*")

(defconst guile-backtrace-min-height 10)
(defconst guile-backtrace-max-height 30)
(defconst guile-backtrace-min-width 30)
(defconst guile-backtrace-max-width 90)

(cond ((not window-system)
       ;; Faces for text terminals
       (setq guile-error-face 'modeline)
       (setq guile-backtrace-mouse-face 'highlight)
       (setq guile-modified-face nil) ; no special face
       (setq guile-broken-face nil)
       (setq guile-unmodified-face-1 nil)
       (setq guile-unmodified-face-2 'modeline)
       (setq guile-modified-face-1 'bold)
       (setq guile-modified-face-2 guile-error-face)
       (setq guile-broken-face-1 nil)
       (setq guile-broken-face-2 nil))
      ((x-display-color-p)
       ;; Faces for color screens
       (setq guile-error-face (lookup-face-create 'black/red-bold))
       (setq guile-backtrace-mouse-face 'highlight)
       (setq guile-modified-face nil) ; no special face
       (setq guile-broken-face 'bold)
       (setq guile-unmodified-face-1 (lookup-face-create 'black/lightblue))
       (setq guile-unmodified-face-2 'secondary-selection)
       (setq guile-modified-face-1 'highlight)
       (setq guile-modified-face-2 (lookup-face-create 'black/pink))
       (setq guile-broken-face-1
	     (let ((face (make-face 'broken-form-1)))
	       (copy-face guile-modified-face-1 face)
	       (set-face-underline-p face t)
	       face))
       (setq guile-broken-face-2
	     (let ((face (make-face 'broken-form-2)))
	       (copy-face guile-modified-face-2 face)
	       (set-face-underline-p face t)
	       face)))
      (t
       ;; Faces for monochrome screens
       (setq guile-error-face (lookup-face-create 'white/black-bold))
       (setq guile-backtrace-mouse-face 'highlight)
       (setq guile-modified-face nil) ; no special face
       (setq guile-broken-face 'bold)
       (setq guile-unmodified-face-1 nil)
       (setq guile-unmodified-face-2 'modeline)
       (setq guile-modified-face-1 'bold)
       (setq guile-modified-face-2 guile-error-face)
       (setq guile-broken-face-1
	     (let ((face (make-face 'broken-form-1)))
	       (copy-face guile-modified-face-1 face)
	       (set-face-underline-p face t)
	       face))
       (setq guile-broken-face-2
	     (let ((face (make-face 'broken-form-2)))
	       (copy-face guile-modified-face-2 face)
	       (set-face-underline-p face t)
	       face))))

(if (not (fboundp 'lisp-mode-auto-fill))
    (defun lisp-mode-auto-fill ()
      (if (> (current-column) (current-fill-column))
	  (if (save-excursion
		(nth 4 (parse-partial-sexp (save-excursion
					     (beginning-of-defun)
					     (point))
					   (point))))
	      (do-auto-fill)
	    (let ((comment-start nil) (comment-start-skip nil))
	      (do-auto-fill))))))

(defconst guile-symclash-obarray-size 521)

(defconst guile-big-integer 33333333)

;;; Mode initializers
;;;

(defvar guile-inferior-scheme-frame nil)

;; Inferior Scheme Mode
;;
(defun guile-inferior-initialize ()
  ;; Buffer local variables
  (make-local-variable 'guile-eval-result)
  (make-local-variable 'guile-eval-output)
  (make-local-variable 'guile-last-output-end)
  (make-local-variable 'guile-last-prompt-end)
  (make-local-variable 'guile-define-name-marker)
  (make-local-variable 'guile-unallowed-output)
  (make-local-variable 'guile-define-startcol)
  (make-local-variable 'guile-define-filler)
  (make-local-variable 'guile-define-fillcol)
  (set-process-sentinel (scheme-proc) (function guile-sentinel))
  (setq comint-dispatch-alist guile-dispatch-alist)
  (add-hook 'comint-input-filter-functions
	    (function guile-sync-on-input) nil 'local)
  (add-hook 'comint-unallowed-output-filter-functions
	    (function guile-remember-unallowed-output) nil 'local)
  (setq comint-dynamic-complete-functions '(guile-complete-symbol))
  (make-local-hook 'scheme-enter-input-wait-hook)
  ;; Some initializations
  (setq scheme-ready-p nil)
  (setq scheme-load-p nil)
  (setq guile-no-stack-p nil)
  (setq guile-no-source-p nil)
  (setq guile-last-output-end (make-marker))
  (setq guile-last-prompt-end (make-marker))
  (setq guile-input-sent-p t)
  (setq guile-define-name-marker (make-marker))
  (setq guile-error-p nil)
  (setq guile-sexp-overlay nil)
  (setq guile-frame-overlay nil)
  (let ((enhanced (guile-get-enhanced-buffers)))
    (and scheme-buffer (guile-detach-all))
    (for-each (function guile-normal-edit) enhanced)
    (guile-kill-overlays)
    (for-each (function (lambda (buffer)
			  (save-excursion
			    (set-buffer buffer)
			    (guile-enhanced-edit
			     buffer
			     (not scheme-buffer-modified-p)))))
	      enhanced))
  (setq guile-synchronizedp t)
  (setq comint-allow-output-p t)
  (setq guile-unallowed-output nil)
  )

(defvar default-handle-switch-frame-binding
  (lookup-key global-map [switch-frame]))
(define-key global-map [switch-frame] 'guile-handle-switch-frame)

(defun guile-handle-switch-frame (event)
  (interactive "e")
  (let ((frame (nth 1 event)))
    (if (eq frame guile-inferior-scheme-frame)
	(guile-sync-with-scheme))
    (funcall default-handle-switch-frame-binding frame)))

(defun guile-sync-on-input (string)
  (if scheme-load-p
      (progn
	nil))
  (setq guile-error-p nil) ;; What is this??? *fixme*
  (guile-sync-with-scheme)
  (if guile-error-p
      (progn
	;; The read-only-overlay extends during transfer of error and
	;; backtrace information.  Check why! *fixme*
	(let ((inhibit-read-only t))
	  (comint-kill-input))
	;; By generating an error we interrupt the execution
	;; of the comint-input-filter-functions hook.
	(error "Bad expression!  Please correct."))))

(defvar guile-unallowed-output nil)

(defun guile-remember-unallowed-output (string)
  (if guile-unallowed-output
      (setq guile-unallowed-output
	    (concat guile-unallowed-output string))))

(add-hook 'inferior-scheme-mode-hook (function guile-inferior-initialize))

;; Scheme Mode
;;
(defvar scheme-buffer-overlays ()
  "The overlays containing top-level sexps when in enhanced edit mode.
A nil value indicates that the buffer is not in enhanced edit mode.")

(defvar scheme-buffer-last-overlay nil
  "When in enhanced edit mode, this variable contains the lowermost
overlay.")

(defvar scheme-buffer-modified-p nil
  "Non-nil if any overlay has been modified since last synchronization.")

(defvar scheme-buffer-overlays-modified-p nil)

(defvar scheme-associated-process-buffer nil
  "The buffer of the scheme process to which this buffer is associated.
A value of nil means that this buffer is detached.")

(defvar scheme-overlay-repair-function nil)

(make-variable-buffer-local 'scheme-overlay-repair-function)

(defvar scheme-overlay-repair-idle-timer nil)

(defun guile-scheme-mode-initialize ()
  "Initialize a scheme mode buffer."
  (make-local-variable 'scheme-buffer-overlays)
  (make-local-variable 'scheme-buffer-modified-p)
  (make-local-variable 'scheme-buffer-last-overlay)
  (make-local-variable 'scheme-buffer-overlays-modified-p)
  (make-local-variable 'scheme-associated-process-buffer)
  (make-local-variable 'guile-last-broken)
  (make-local-variable 'guile-repair-limit)
  (make-local-hook 'first-change-hook)
  (add-hook 'first-change-hook (function guile-scheme-buffer-modified) nil t)
  (make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook (function guile-scheme-mode-cleanup) nil t)
  (if guile-default-enhanced-edit
      (guile-enhanced-edit (current-buffer)
			   ;; If buffer not modified, take a chance...
			   (and (not scheme-buffer-modified-p)
				(not (buffer-modified-p (current-buffer))))
			   ))
  )

(add-hook 'scheme-mode-hook (function guile-scheme-mode-initialize))

(defun guile-scheme-buffer-modified ()
  (setq scheme-buffer-modified-p t))

(defun guile-scheme-mode-cleanup ()
  (if (guile-attachedp (current-buffer))
      (progn
	(guile-sync-buffer (current-buffer))
	(guile-detach-buffer (current-buffer))))
  (if (guile-enhancedp (current-buffer))
      (guile-normal-edit (current-buffer))))

;;; User interface support
;;;

(defun guile-clear-transcript ()
  "Delete all text before the last prompt in the scheme process buffer."
  (interactive)
  (if (or (not (buffer-name))
	  (not (string= (buffer-name) scheme-buffer)))
      (error "This command must be issued in the scheme process buffer!"))
  (save-excursion
    (goto-char (or (marker-position guile-last-prompt-end)
		   (point-max)))
    (if (re-search-backward comint-prompt-regexp nil t)
	(goto-char (match-beginning 0))
      (beginning-of-line))
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point)))))

(defun guile-switch-to-scheme ()
  "Switch to the scheme process buffer and places cursor at the end.
Also update the scheme process with all changes made in attached buffers."
  (interactive)
  (guile-sync-with-scheme)
  ;(if (not guile-error-p)
  ;    (switch-to-scheme t))
  (switch-to-scheme t))

;;; Process control
;;;
;(defvar scheme-running-p nil
;  "This variable, if nil, indicates that the process is waiting for input.")

(defvar scheme-ready-p nil
  "If non-nil, the process is waiting for input at the top-level repl.")

(defvar scheme-load-p nil)

(defvar guile-no-stack-p nil)

(defvar guile-no-source-p nil)

(defun guile-inferior-dialog (contents)
  (let ((window (display-buffer "*scheme*")))
    (x-popup-dialog window contents)))

(defun guile-sentinel (process reason)
  (let ((status (process-status process)))
    (if guile-insert-reason
	(let ((old-buffer (current-buffer)))
	  (unwind-protect
	      (progn
		(set-buffer (process-buffer process))
		(goto-char (point-max))
		(insert reason)
		(goto-char (point-max))
		(sit-for 0))
	    (set-buffer old-buffer))))
    (cond ((eq status 'run)
	   (scheme-set-runlight scheme-last-runlight))
	  ((eq status 'stop)
	   (scheme-set-runlight 'stopped)
	   (if guile-popup-restart-on-stop
	       (if (guile-inferior-dialog '("The scheme process has been stopped.
Do you want to restart it?" ("Yes" . t) nil ("No" . nil)))
		   (continue-process process))))
	  (t
	   (guile-inferior-death-cleanup)
	   (if guile-popup-restart-on-death
	       (if (guile-inferior-dialog '("The scheme process has died.
Do you want to restart it?" ("Yes" . t) nil ("No" . nil)))
		   (run-scheme scheme-program-name)
		 (or guile-kill-buffer-on-death
		     (kill-buffer "*scheme*")))
	     (or guile-kill-buffer-on-death
		 (kill-buffer "*scheme*")))))))

(defun guile-inferior-death-cleanup ()
  (scheme-set-runlight nil)
  (setq scheme-ready-p nil)
  (setq scheme-virtual-file-list nil)
  (guile-detach-all))

;; It would be too late to set this variable in the inferior-scheme-mode-hook:
;;(setq comint-output-filter-function (function comint-dispatch-output-filter))
;; *fixme* This should rather be done with advice.

(defun run-scheme (cmd)
  "Run an inferior Scheme process, input and output via buffer *scheme*.
If there is a process already running in *scheme*, just switch to that buffer.
With argument, allows you to edit the command line (default is value
of scheme-program-name).  Runs the hooks from inferior-scheme-mode-hook
\(after the comint-mode-hook is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  (interactive (list (if current-prefix-arg
			 (read-string "Run Scheme: " scheme-program-name)
			 scheme-program-name)))
  (if (not (comint-check-proc "*scheme*"))
      (let ((cmdlist (scheme-args-to-list cmd))
	    (comint-output-filter-function
	     (function comint-dispatch-output-filter)))
	(set-buffer (apply 'make-comint "scheme" (car cmdlist)
			   nil (cdr cmdlist)))
	(inferior-scheme-mode)))
  (setq scheme-program-name cmd)
  (setq scheme-buffer "*scheme*")
  (pop-to-buffer "*scheme*")
  ;; *fixme* Ugly to specialize `run-scheme' in this way...
  (setq guile-inferior-scheme-frame (selected-frame)))

(defun guile-restart-scheme ()
  (interactive)
  (let ((old-buffer (current-buffer)))
    (unwind-protect
	(progn
	  (set-buffer scheme-buffer)
	  (let ((attached-buffers inferior-scheme-associated-buffers))
	    (guile-shutdown)
	    (let ((inhibit-read-only t))
	      (erase-buffer))
	    (setq comint-allow-output-p t)
	    (run-scheme scheme-program-name)
	    ;(sit-for 0 200)
	    (for-each (function (lambda (buffer)
				  (if (buffer-name buffer)
				      (guile-attach-buffer buffer))))
		      (reverse attached-buffers))))
      (set-buffer old-buffer))))

(defun guile-shutdown ()
  (interactive)
  (let ((guile-popup-restart-on-death nil)
	(old-buffer (current-buffer)))
    (unwind-protect
	(progn
	  (set-buffer scheme-buffer)
	  (setq comint-allow-output-p nil) ; Hide output
	  (setq guile-unallowed-output nil)
	  (if scheme-ready-p
	      (let ((inhibit-read-only t))
		(comint-kill-input)
		(comint-send-string (scheme-proc) "(quit)\n")
		(let ((countdown 5))
		  (while (and scheme-ready-p (> countdown 0))
		    (sit-for 0 300)
		    (setq countdown (1- countdown))))))
	  (sit-for 0 100)
	  (if (comint-check-proc "*scheme*")
	      (progn
		(kill-process (scheme-proc))
		(while (comint-check-proc "*scheme*")
		  (sit-for 0 300))))
	  (sit-for 0 100))
      (set-buffer old-buffer))))

(defun guile-exit-scheme ()
  "Stop the running scheme process and kill the corresponding window"
  (interactive)
  (guile-shutdown)
  (if (not (comint-check-proc "*scheme*"))
      (kill-buffer "*scheme*")))

;;; Basic process protocol

(setq guile-dispatch-alist
      '((?f scheme-exit-input-wait   scheme:simple-action)
	(?l scheme-load-acknowledge  scheme:simple-action)
	(?r scheme-enter-read        scheme:simple-action)
	(?s scheme-enter-input-wait  scheme:simple-action)
	(?B guile-receive-backtrace  scheme:buffer-action)
	(?F guile-receive-error      scheme:buffer-action)
	(?x guile-receive-result     scheme:string-action)
	(?S guile-no-stack	     scheme:simple-action)
	(?R guile-no-source	     scheme:simple-action)
      ))

(defun scheme:simple-action (action)
  (setq comint-dispatch-state 'idle)
  (funcall action))

(defun scheme:string-action (action)
  (setq comint-string-receiver action)
  (setq comint-string-accumulator "")
  (setq comint-dispatch-state 'reading-string))

(defun scheme:buffer-action (action)
  (setq comint-buffer-receiver action)
  (setq comint-receiving-buffer (generate-new-buffer "*receiving-buffer*"))
  (setq comint-dispatch-state 'reading-to-buffer))

;;; Guile protocol

(defun guile-no-stack ()
  (setq guile-no-stack-p t))

(defun guile-no-source ()
  (setq guile-no-source-p t))

(defvar guile-eval-result nil)
(defvar guile-eval-output nil)

(defun guile-receive-result (string)
  (setq comint-allow-output-p nil)
  (setq guile-eval-result string)
  (setq guile-eval-output guile-unallowed-output)
  (setq guile-unallowed-output nil))

(defun guile-eval (sexp &optional stringp)
  (let ((process (scheme-proc)) ;*fixme*
	(comint-input-filter-functions '())
	(comint-output-filter-functions '()))
    (if (not scheme-ready-p)
	(error "Scheme process not ready to receive commands."))
    (setq guile-eval-result nil)
    (comint-send-string process
			(format "(%%%%emacs-eval-request '%S)\n" sexp))
    (while (not guile-eval-result)
      (accept-process-output process))
    (while (not scheme-ready-p)
      (accept-process-output process))
    (if stringp
	guile-eval-result
      (car (read-from-string guile-eval-result)))))

(defun scheme-set-runlight (runlight)
  (setq inferior-scheme-mode-line-process
	(or runlight "no process"))
  (setq scheme-last-runlight runlight)
  (if guile-show-runlight-in-scheme-mode
      (let ((old-buffer (current-buffer))
	    (buffers inferior-scheme-associated-buffers))
	(unwind-protect
	    (while buffers
	      (set-buffer (car buffers))
	      (setq scheme-mode-line-process runlight)
	      (setq buffers (cdr buffers)))
	  (set-buffer old-buffer))))
  (force-mode-line-update t))

(defconst scheme-runlight:running "eval"
  "The character displayed when the Scheme process is running.")

(defconst scheme-runlight:input "ready"
  "The character displayed when the Scheme process is waiting for input.")

(defconst scheme-runlight:read "input"
  "The character displayed when the Scheme process is waiting for input.")

(defconst scheme-runlight:load "loading"
  "The character displayed when the Scheme process is loading forms.")

(defvar guile-last-output-end)

(setq count 0)
(defun scheme-enter-input-wait ()
  (scheme-set-runlight scheme-runlight:input)
  (setq scheme-running-p nil)
  (setq scheme-ready-p t)
  (setq count (1+ count))
  ;(insert-before-markers (format "#%d\n" count))
  ;(setq n (1+ n)
  ;	 l (append l (list (list n 'enter-input-wait))))
  (if comint-allow-output-p
      (progn
	(set-marker guile-last-output-end (point))
	(if (and guile-input-sent-p
		 ;; This code can be invoked multiple times
		 (or (not (marker-position guile-last-prompt-end))
		     (/= (marker-position guile-last-prompt-end)
			 (point))))
	    (progn
	      (setq guile-input-sent-p nil)
	      (set-marker guile-last-prompt-end (point))))))
  (setq comint-allow-output-p t)
  (run-hooks 'scheme-enter-input-wait-hook))

(defun guile-on-error ()
  (setq guile-input-sent-p t) ;*fixme*
  (if comint-allow-output-p
      (progn
	(goto-char (point-max))
	(if (not (zerop (current-column)))
	    (insert "\n"))
	(set-marker (process-mark (get-buffer-process scheme-buffer))
		    (point)))))

(defun scheme-exit-input-wait ()
  (scheme-set-runlight scheme-runlight:running)
  (setq scheme-ready-p nil)
  (setq scheme-running-p t))

(defun scheme-enter-read ()
  (scheme-set-runlight scheme-runlight:read)
  (setq scheme-ready-p nil)
  (setq scheme-running-p nil))

(defun scheme-enter-load ()
  (scheme-set-runlight scheme-runlight:load)
  (setq scheme-ready-p nil)
  (setq scheme-load-p t))

(defun scheme-load-acknowledge ()
  (setq scheme-load-p nil))

;;; Error reporting and backtrace
;;;
(defvar guile-error-p nil)

(defvar guile-last-displayed-position nil)

(defvar guile-positional-reliability nil)

(defvar guile-last-erring-overlay nil)

(defvar guile-sexp-overlay nil)

(defvar guile-frame-overlay nil)

;(defconst guile-position-regexp
;  " at line \\([0-9]+\\), column \\([0-9]+\\) in file \\(.+\\):$")
(defconst guile-position-regexp
  "^\\(.+\\):\\([0-9]+\\):\\([0-9]+\\): ")

(defconst guile-position-regexp-line 2)
(defconst guile-position-regexp-column 3)
(defconst guile-position-regexp-filename 1)

(defvar guile-error-width 0)
(defvar guile-backtrace-length nil)
(defvar guile-backtrace-width 0)

(defvar guile-error-map nil)
(if guile-error-map
    nil
  (setq guile-error-map ;(copy-keymap global-map) copies menus too...
	(cons 'keymap (copy-sequence (nth 1 global-map))))
  (suppress-keymap guile-error-map)
  (define-key guile-error-map "\e" 'guile-exit-debug)
  (define-key guile-error-map "e" 'guile-frame-eval)
  (define-key guile-error-map "q" 'guile-exit-debug)
  ;; The following line is included since `local-map' doesn't seem to work.
  (define-key guile-error-map [mouse-2] 'guile-select-stackframe)
  (define-key guile-error-map [S-mouse-2] 'guile-frame-eval-at-click)
  )

(defvar guile-stack-frame-map nil)
(if guile-stack-frame-map
    nil
  (setq guile-stack-frame-map (copy-list guile-error-map))
  (fset 'guile-stack-frame-map guile-stack-frame-map) ;*fixme*
  (define-key guile-stack-frame-map [mouse-2] 'guile-select-stackframe)
  )

(setplist 'guile-backtrace-button
	  (list 'mouse-face guile-backtrace-mouse-face
		'local-map 'guile-stack-frame-map))

(defun guile-exit-debug ()
  (interactive)
  (if (eq (selected-frame) guile-error-frame)
      (iconify-frame)
    (if guile-sexp-overlay
	(delete-overlay guile-sexp-overlay))
    (delete-other-windows (frame-first-window)))
  (guile-unselect-stackframe))

(setq guile-backtrace-received-p nil) ;*fixme*

(defun guile-receive-backtrace (buffer)
  (let ((backtrace (get-buffer-create guile-backtrace-buffer-name)))
    (save-excursion
      (set-buffer backtrace)
      (toggle-read-only 0)
      (erase-buffer)
      (insert-buffer-substring buffer)
      (kill-buffer buffer)
      (use-local-map guile-error-map)
      (toggle-read-only 1)
      (setq truncate-lines t)
      (setq guile-backtrace-received-p t)))) ;*fixme*

(defun guile-prep-backtrace ()
  (guile-unselect-stackframe)
  (let ((buffer (get-buffer-create guile-backtrace-buffer-name)))
    (and guile-got-backtrace-p ;*fixme*
	 (save-excursion
	   (set-buffer buffer)
	   (set-syntax-table scheme-mode-syntax-table)
	   (toggle-read-only 0)
	   (goto-char (point-max))
	   (delete-backward-char 1)
	   (goto-char (point-min))
	   ;; Parse
	   (save-match-data
	     (if (not (looking-at "\\(.\\|\n\\)*Backtrace:\n"))
		 nil
	       (replace-match "")
	       (let ((beg (point))
		     (width 0)
		     (len 0))
		 (while (not (eobp))
		   (forward-line 1)
		   (let ((o (make-overlay beg (point)))) ;(1- (point))
		     (overlay-put o 'category 'guile-backtrace-button)
		     (overlay-put o 'frame-number-pos beg))
		   (setq width (- (point) beg 1))
		   (if (> width guile-backtrace-width)
		       (setq guile-backtrace-width width))
		   (setq beg (point))
		   (setq len (1+ len)))
		 (setq guile-backtrace-length len))))
	   (toggle-read-only 1)))
    buffer))

(defvar guile-selected-frame nil)

(defun guile-select-stackframe (click)
  (interactive "e")
  (setq guile-no-stack-p nil)
  (setq guile-no-source-p nil)
  (let* ((frame (save-excursion
		  (mouse-set-point click)
		  (goto-char (get-char-property (point) 'frame-number-pos))
		  (guile-place-frame-overlay)
		  (let ((start (point)))
		    (skip-chars-forward " ")
		    (skip-chars-forward "0-9")
		    (if (= (char-after) ?:)
			;; new backtrace format
			(progn
			  (forward-char)
			  (skip-chars-forward " ")
			  (setq start (point))
			  (skip-chars-forward "0-9")))
		    (string-to-number (buffer-substring-no-properties start (point))))))
	 (oldpos (save-excursion
		   (set-buffer scheme-buffer)
		   (guile-eval `(%%emacs-select-frame ,frame))))
	 (pos (and oldpos (list (nth 0 oldpos)
				(1+ (nth 1 oldpos)) ;Increment line number
				(nth 2 oldpos)))))
    (setq guile-selected-frame frame)
    (cond (pos (if guile-source-window	;This is just insane *fixme*
		   (apply 'guile-display-scheme-sexp
			  (append pos (list guile-source-window t)))
		 (guile-display-error (get-buffer guile-error-buffer-name)
				      (get-buffer guile-backtrace-buffer-name)
				      pos)))
	  (guile-no-stack-p (message "No stack."))
	  (guile-no-source-p (message "No source.")))))

(defun guile-unselect-stackframe ()
  (guile-turn-off-frame-overlay)
  (setq guile-selected-frame nil))

(defun guile-frame-eval (string)
  (interactive "sEval: ")
  (if (not guile-selected-frame)
      (message "No frame selected.")
  (setq guile-no-stack-p nil)
  (setq guile-no-source-p nil)
    (let ((res (save-excursion
		 (set-buffer scheme-buffer)
		 (guile-eval `(%%emacs-frame-eval ,guile-selected-frame
						  ,string)))))
      (cond (guile-no-stack-p (message "No stack."))
	    (guile-no-source-p (message "No source."))
	    ((eq (car res) 'result) (message "%s = %s" string (cadr res)))
	    (t (message "%s" (cadr res)))))))

(defun guile-frame-eval-at-click (click)
  (interactive "e")
  (save-excursion
    (mouse-set-point click)
    (forward-sexp)
    (let ((end (point)))
      (backward-sexp)
      (guile-frame-eval (buffer-substring-no-properties (point) end)))))

(defun guile-receive-error (buffer)
  (guile-on-error)
  (setq guile-got-backtrace-p guile-backtrace-received-p)
  (setq guile-backtrace-received-p nil) ;*fixme*
  (setq guile-error-p t)
  (let ((errbuf (get-buffer-create guile-error-buffer-name)))
    (save-excursion
      (set-buffer errbuf)
      (toggle-read-only 0)
      (erase-buffer)
      (insert-buffer-substring buffer)
      (kill-buffer buffer)
      (use-local-map guile-error-map)
      (toggle-read-only 1)
      (setq guile-error-width 0)
      (goto-char (point-min))
      (let ((beg (point))
	    (width 0))
	(while (not (eobp))
	  (forward-line 1)
	  (setq width (- (point) beg 1))
	  (if (> width guile-error-width)
	      (setq guile-error-width width))
	  (setq beg (point))))
      (setq guile-backtrace-width guile-error-width)
      (guile-display-error errbuf (guile-prep-backtrace)))))

(defvar guile-source-window nil)

(defun guile-display-error (errbuf backbuf &optional pos)
  (set-buffer errbuf)
  (setq guile-source-window nil)
  (let* ((errbuf-len (progn
		       (goto-char (point-max))
		       (1- (guile-current-line))))
	 (selected-window (selected-window))
	 (mini-window nil)
	 (window
	  (if pos
	      (apply 'guile-display-scheme-sexp pos)
	    (and (progn
		   (goto-char (point-min))
		   (re-search-forward guile-position-regexp nil t))
		 (save-match-data
		   (guile-display-scheme-sexp
		    (car (read-from-string
			  (concat "\""
				  (match-string guile-position-regexp-filename)
				  "\"")))
		    (string-to-number (match-string guile-position-regexp-line))
		    (1- (string-to-number (match-string guile-position-regexp-column))))))))
	 (errbuf-lines
	  (min (+ errbuf-len
		  (* 2 (/ guile-error-width
			  (if window
			      (window-width window)
			    guile-backtrace-max-width))))
	       ;;In case we get big error messages
	       (/ guile-backtrace-max-height 2)))
	 (total-height
	  (if guile-got-backtrace-p
	      (min (max (+ guile-backtrace-length errbuf-lines 2)
			guile-backtrace-min-height)
		   guile-backtrace-max-height)
	    (+ errbuf-lines 1))))
    (if (and window guile-backtrace-in-source-window)
	(progn
	  (set-buffer errbuf) ;*fixme* This is awkward...
	  (or pos
	      (let ((inhibit-read-only t))
		(replace-match "")
		(re-search-forward guile-position-regexp nil t)
		(replace-match "")))
	  (setq guile-source-window window) ;*fixme*
	  (and (frame-live-p guile-error-frame)
	       (make-frame-invisible guile-error-frame))
	  (let* ((window-min-height 2)
		 (size (max (- (window-height window) total-height)
			    (/ (window-height window) 2)))
		 (new-window (split-window window size)))
	    (set-buffer (window-buffer window))
	    (goto-char guile-last-displayed-position)
	    (guile-safe-forward-sexp)
	    (recenter (/ size 2))
	    (setq x errbuf-lines)
	    (guile-display-buffers errbuf (1+ errbuf-lines) backbuf new-window
				   pos)))
      (setq guile-source-window nil)
      (guile-display-buffers
       errbuf (1+ errbuf-lines) backbuf
       (setq mini-window
	     (guile-get-create-error-window
	      total-height
	      (+ (min (max guile-backtrace-width
			   guile-backtrace-min-width)
		      guile-backtrace-max-width)
		 2)))
       pos))
    (cond ((window-live-p selected-window)
	   (select-window selected-window))
	  ((window-live-p window)
	   (select-window window))
	  ((window-live-p mini-window)
	   (select-window mini-window)))
    ;; Warn if unreliable position
    (if (and window (not guile-positional-reliability))
	(message "Warning: Couldn't reliably locate erring expression."))
    ))

(defun guile-display-buffers (buffer1 split buffer2 window no-ding)
  "Display BUFFER1 and BUFFER2 in WINDOW and raise the containing frame.
Display BUFFER1 and BUFFER2 in two windows obtained by splitting WINDOW
and ring the bell.  Make sure that the whole contents of BUFFER1 and the
lower part of BUFFER2 will be visible.  Also delete all other windows
displaying the buffers."
  ;; Delete other windows displaying the buffers
  (or (not window-system) (delete-windows-on buffer1)) ; *fixme*
  (delete-windows-on buffer2)
  ;; Split the window
  (let ((lower-window
	 (and guile-got-backtrace-p
	      (let ((window-min-height 2) ;; Parameter to split-window
		    )
		(split-window window split)))))
    ;; Contents
    (set-window-buffer window buffer1)
    (and guile-got-backtrace-p
	 (set-window-buffer lower-window buffer2))
    ;; Look
    (set-window-start window 1)
    (if guile-got-backtrace-p
	(progn
	  (let ((pos (save-excursion
		       (set-buffer buffer2)
		       (goto-char (point-max))
		       (forward-line -1)
		       (point))))
	    (set-window-point lower-window pos))
	  (select-window lower-window)
	  (recenter -1)))
    ;; Raise frame
    (make-frame-visible (window-frame window))
    (raise-frame (window-frame window))
    ;; Beep
    (or no-ding (ding))
    ))

(defvar guile-error-frame nil)

(defun guile-get-create-error-window (height width)
  (if window-system
      (progn
	(if (frame-live-p guile-error-frame)
	    (set-frame-size guile-error-frame width height)
	  (setq guile-error-frame (make-frame (list (cons 'height height)
						    (cons 'width width)
						    '(minibuffer . nil)
						    '(menu-bar-lines . 0)))))
	(let ((window (frame-first-window guile-error-frame)))
	  (delete-other-windows window)
	  window))
    (let ((window (get-buffer-window (pop-to-buffer guile-error-buffer-name))))
      (sit-for 0) ; necessary because of an Emacs bug
      window)))

(defun guile-display-scheme-sexp (filename line column &optional swindow no-error-p)
  (let ((finfo (scheme-virtual-file-list-find filename)))
    (if finfo
	(guile-display-sexp finfo line column swindow no-error-p)
      (if (stringp filename)
	  (let ((buffer (guile-get-file-buffer filename)))
	    (if buffer
		(if (and (guile-attachedp buffer)
			 (not guile-known-by-scheme))
		    (progn
		      ;(ding)		; We shouldn't generate errors inside a filter.
		      ;(message "Internal data structures corrupt: guile-display-scheme-sexp")
		      (error "Internal data structures corrupt: guile-display-scheme-sexp"))
		  (if (and (not scheme-buffer-modified-p)
			   (not (buffer-modified-p buffer)))
		      ;; Take a chance and let's hope the file looks
		      ;; like it did when scheme saw it...
		      (progn
			(if guile-auto-attach
			    (guile-attach-buffer buffer t)
			  ;*fixme*
			  (guile-dont-attach-buffer buffer t))
			(guile-display-scheme-sexp
			 (guile-buffer-file-name buffer) line column swindow no-error-p))
		    nil			; Can't trust this one...
		    ))
	      (if (guile-file-readable-p filename)
		  (let ((guile-known-by-scheme t))
		    (let ((buffer (guile-find-file-noselect filename)))
		      (if guile-auto-attach
			  (guile-attach-buffer buffer t)
			;*fixme*
			(guile-dont-attach-buffer buffer t))
		      (guile-display-scheme-sexp
		       (guile-buffer-file-name buffer)
		       line column swindow no-error-p)))
		(ding)
		(message "Couldn't find the erring file.")
		nil)))))))

(defun guile-file-readable-p (filename)
  (save-excursion
    (set-buffer scheme-buffer)
    (file-readable-p filename)))

(defun guile-find-file-noselect (filename)
  (save-excursion
    (set-buffer scheme-buffer)
    (find-file-noselect filename)))

(defun guile-display-sexp (finfo line column &optional swindow no-error-p)
  ;; Returns the window containing the displayed sexp
  (let ((overlay-list (cdr finfo))
	(overlay nil))
    ;; Select an overlay candidate
    (while overlay-list
      (if (not (overlay-get (car overlay-list) 'original-line))
	  (setq overlay-list (cdr overlay-list))
	(if (>= line (overlay-get (car overlay-list) 'original-line))
	    (progn
	      (setq overlay (car overlay-list))
	      (setq overlay-list nil))
	  (setq overlay-list (cdr overlay-list)))))
    (let ((buffer (and overlay (overlay-buffer overlay))))
      (if buffer
	  (progn
	    (set-buffer buffer)
	    (guile-goto-position line column overlay)
	    (if (< (point) (overlay-end overlay))
		(progn
		  (setq guile-positional-reliability
			(not (overlay-get overlay 'modifiedp)))
		  (if (not (eq (char-syntax (following-char)) ?\())
		      (progn
			(setq guile-positional-reliability nil)
			(goto-char (overlay-start overlay))))
		  (setq guile-last-erring-overlay overlay)
		  (guile-display-sexp-at-point swindow no-error-p))))))))

(defun guile-display-sexp-at-point (&optional swindow no-error-p)
  "Move sexp overlay to sexp at point and display window.
Returns the displayed window."
  (let ((start (point))
	(end nil))
    (save-excursion
      (setq end
	    (if (guile-safe-forward-sexp)
		(point)
	      (goto-char (1+ start))
	      (if (re-search-forward "^\\((\\|$\\)" nil t)
		  (1- (match-beginning 0))
		(point-max)))))
    (if (overlayp guile-sexp-overlay)
	(move-overlay guile-sexp-overlay start end (current-buffer))
      (setq guile-sexp-overlay (make-overlay start end))
      (overlay-put guile-sexp-overlay 'category 'guile-error-sexp))
    (if (window-live-p swindow)
	(set-window-buffer swindow (current-buffer)))
    (guile-display-position start nil swindow no-error-p)))

(setplist 'guile-error-sexp
	  (list 'face guile-error-face
		'evaporate t
		'modification-hooks '(guile-turn-off-sexp-overlay)
		'insert-behind-hooks '(guile-turn-off-sexp-overlay)))

(setplist 'guile-stack-frame
	  (list 'face guile-error-face
		'mouse-face guile-error-face
		'evaporate t
		'modification-hooks '(guile-turn-off-frame-overlay)
		'insert-behind-hooks '(guile-turn-off-frame-overlay)))

(defun guile-place-frame-overlay ()
  (let ((end (save-excursion (forward-line) (point))))
    (if (and guile-frame-overlay (overlayp guile-frame-overlay))
	(move-overlay guile-frame-overlay (point) end)
      (setq guile-frame-overlay (make-overlay (point) end)))
    (overlay-put guile-frame-overlay 'category 'guile-stack-frame)))

(defun guile-turn-off-sexp-overlay (&rest args)
  (cond (guile-sexp-overlay (delete-overlay guile-sexp-overlay))
	;; For stability.
	((overlayp (car args)) (delete-overlay (car args)))))

(defun guile-turn-off-frame-overlay (&rest args)
  (cond (guile-frame-overlay (delete-overlay guile-frame-overlay))
	;; For stability.
	((overlayp (car args)) (delete-overlay (car args)))))

(defun guile-display-position (pos &optional buffer swindow no-delete-p)
  "Display position POS in BUFFER.
If BUFFER is omitted, the current buffer is used.
Returns the displaying window."
  (let ((buffer (or buffer (current-buffer))))
    (set-buffer buffer)
    (let ((window (or (and (window-live-p swindow) swindow)
		      (get-buffer-window buffer t)
		      (if (frame-live-p guile-error-frame)
			  (delete-frame guile-error-frame))
		      (display-buffer buffer))))
      (or no-delete-p
	  (delete-other-windows window))
      (select-window window)
      (goto-char pos)
      (setq guile-last-displayed-position pos)
      window)))

(defun guile-goto-position (line column overlay)
  (goto-char (overlay-start overlay))
  (forward-line (- line (overlay-get overlay 'original-line)))
  (move-to-column column))


;;; Scheme process associated buffers
;;;

;; This function must be fixed to handle rel/absol filenames
(defun guile-get-file-buffer (filename)
  (get-file-buffer filename))

(defun guile-attachedp (&optional buffer)
  (if buffer
      (save-excursion
	(set-buffer buffer)
	scheme-associated-process-buffer)
    scheme-associated-process-buffer))

(defun guile-attach-buffer (buffer &optional known-by-scheme)
  "Put the buffer in enhanced editing mode and attach it to the scheme
process: load it into scheme, and make sure to send any changes to it
hereafter to scheme at synchronization points."
  (interactive (list (current-buffer)))
  (if (memq buffer inferior-scheme-associated-buffers)
      (error "Scheme buffer already attached!"))
  (if (not (guile-enhancedp buffer))
      (guile-enhanced-edit buffer known-by-scheme))
  (save-excursion
    (set-buffer scheme-buffer)
    (setq inferior-scheme-associated-buffers
	  (cons buffer
		inferior-scheme-associated-buffers))
    (set-buffer buffer)
    (setq scheme-associated-process-buffer scheme-buffer)
    (if (not guile-show-runlight-in-scheme-mode)
	(setq scheme-mode-line-process "attached"))
    ;; Now link it to the scheme process
    (if (and (guile-buffer-file-name)
	     (not (guile-virtually-linked-p (guile-buffer-file-name))))
	(guile-virtual-link (guile-buffer-file-name) scheme-buffer-overlays))
    ;; And sync.
    (if (not known-by-scheme)
	(progn
	  (for-each (function (lambda (overlay)
				(overlay-put overlay 'modifiedp t)))
		    scheme-buffer-overlays)
	  (setq scheme-buffer-modified-p t)
	  (setq guile-synchronizedp nil)
	  (guile-sync-with-scheme))))
  ;; Rebuild menus...
  (force-mode-line-update))

;;*fixme*
(defun guile-dont-attach-buffer (buffer &optional known-by-scheme)
  "Put the buffer in enhanced editing mode and attach it to the scheme
process: load it into scheme, and make sure to send any changes to it
hereafter to scheme at synchronization points."
  (interactive (list (current-buffer)))
  (if (memq buffer inferior-scheme-associated-buffers)
      (error "Scheme buffer already attached!"))
  (if (not (guile-enhancedp buffer))
      (guile-enhanced-edit buffer known-by-scheme))
  (save-excursion
;    (set-buffer scheme-buffer)
;    (setq inferior-scheme-associated-buffers
;	  (cons buffer
;		inferior-scheme-associated-buffers))
    (set-buffer buffer)
;    (setq scheme-associated-process-buffer scheme-buffer) == attach
;    (if (not guile-show-runlight-in-scheme-mode)
;	(setq scheme-mode-line-process "attached"))
    ;; Now link it to the scheme process
    (if (guile-buffer-file-name)
	(guile-virtual-link (guile-buffer-file-name) scheme-buffer-overlays))
    ;; And sync.
    (if (not known-by-scheme)
	(progn
	  (for-each (function (lambda (overlay)
				(overlay-put overlay 'modifiedp t)))
		    scheme-buffer-overlays)
	  (setq scheme-buffer-modified-p t)
	  (setq guile-synchronizedp nil)
	  ;(guile-sync-with-scheme)
	  )))
  ;; Rebuild menus...
  (force-mode-line-update))

(defun guile-detach-buffer (buffer)
  "Disconnect the buffer from the scheme process."
  (interactive (list (current-buffer)))
  (save-excursion
    (set-buffer buffer)
    ;; Unlink any virtual overlay files associated with the buffer...
    ;(let ((overlays scheme-buffer-overlays))
    ;  (while overlays
    ;    (if (guile-virtual-p (car overlays))
    ;        (scheme-virtual-unlink (overlay-get (car overlays) 'id)))
    ;    (setq overlays (cdr overlays))))
    (setq scheme-associated-process-buffer nil)
    (if (not guile-show-runlight-in-scheme-mode)
	(setq scheme-mode-line-process nil))
    (set-buffer scheme-buffer)
    (setq inferior-scheme-associated-buffers
	  (delq buffer
		inferior-scheme-associated-buffers))
    ;(scheme-virtual-unlink (guile-buffer-file-name buffer))
    )
  (force-mode-line-update))

(defun guile-detach-all ()
  "Disconnect all buffers from the scheme process."
  (interactive)
  (save-excursion
    (set-buffer scheme-buffer)
    (while inferior-scheme-associated-buffers
      ;; Is it alive?
      (if (buffer-name (car inferior-scheme-associated-buffers))
	  (save-excursion
	    (set-buffer (car inferior-scheme-associated-buffers))
	    (setq scheme-associated-process-buffer nil)
	    (if (not guile-show-runlight-in-scheme-mode)
		(setq scheme-mode-line-process nil))))
      (setq inferior-scheme-associated-buffers
	    (cdr inferior-scheme-associated-buffers)))))

;;; Linkage of files to scheme space
;;;
(defvar scheme-virtual-file-list '())

(defun scheme-virtual-file-list-find (name)
  (let ((name (file-truename name)))
    (assoc name scheme-virtual-file-list)))

(defun guile-buffer-file-name (&optional buffer)
  (let ((name (buffer-file-name buffer)))
    (and name
	 (file-truename name))))

(defvar guile-synchronizedp t)

(defvar guile-last-virtual-id 0)

(defun guile-synchronizedp ()
  guile-synchronizedp)

;;*fixme*
(defun guile-alloc-virtual-id (overlay)
  (let ((n (setq guile-last-virtual-id (1+ guile-last-virtual-id))))
    (let* ((buffer (overlay-buffer overlay))
	   (name (or (guile-buffer-file-name buffer)
		     (buffer-name buffer))))
      (format "%s(%d)" name n))))

(defun guile-virtual-p (overlay)
  (overlay-get overlay 'virtualp))

(defun guile-virtually-linked-p (name)
  (scheme-virtual-file-list-find name))

(defun guile-virtual-link (name overlay-list)
  (let ((finfo (scheme-virtual-file-list-find name)))
    (if finfo
	(progn
	  (guile-kill-overlays (cdr finfo))
	  (setcdr finfo (copy-sequence overlay-list)))
      (setq scheme-virtual-file-list
	    (cons (cons name
			(copy-sequence overlay-list))
		  scheme-virtual-file-list)))))

(defun scheme-virtual-unlink (name)
  (let ((finfo (scheme-virtual-file-list-find name)))
    (if finfo
	(setq scheme-virtual-file-list
	      (delq finfo scheme-virtual-file-list)))))

(defun guile-load-file (filename)
  "Load a Scheme file into the inferior Scheme process."
  (interactive (comint-get-source "Load Scheme file: " scheme-prev-l/c-dir/file
				  scheme-source-modes t)) ; T because LOAD
                                                          ; needs an exact name
  (if (not scheme-ready-p)
      (error "Scheme not ready."))
  (comint-check-source filename) ; Check to see if buffer needs to be saved.
  (setq scheme-prev-l/c-dir/file (cons (file-name-directory    filename)
				       (file-name-nondirectory filename)))
  (let ((old-buffer (current-buffer)))
    (set-buffer scheme-buffer)
    (setq comint-allow-output-p nil)
    (setq guile-unallowed-output nil)
    (set-buffer old-buffer))
  (scheme-set-runlight scheme-runlight:load)
  (setq scheme-ready-p nil)
  (comint-send-string (scheme-proc) (concat "(load \""
					    filename
					    "\"\)\n"))
  ;; Syncronize...
  (while (not scheme-ready-p)
    (accept-process-output (scheme-proc) 0 guile-process-timeout))
  )

(defun guile-reread-buffer (buffer)
  "Make the scheme interpreter read the buffer contents again."
  (interactive (list (current-buffer)))
  (if (not scheme-ready-p)
      (error "Scheme not ready."))
  (save-excursion
    (set-buffer buffer)
    (for-each (function (lambda (overlay)
			  (overlay-put overlay 'modifiedp t)))
	      scheme-buffer-overlays)
    (setq scheme-buffer-modified-p t))
  (setq guile-synchronizedp nil)
  (guile-sync-with-scheme))

(defun guile-get-associated-buffers ()
  (save-excursion
    (set-buffer scheme-buffer)
    inferior-scheme-associated-buffers))

(defvar guile-symclash-obarray (make-vector guile-symclash-obarray-size 0))

(defun guile-reset-symclash-obarray ()
  (mapatoms (function makunbound) guile-symclash-obarray))

(defvar guile-displayed-erring-buffers nil)
(defvar guile-quiet t)

(defun guile-check-all ()
  (interactive)
  (setq guile-quiet t)
  (guile-check-all-1))

(defun guile-check-all-1 ()
  (guile-show-check-error
   (catch 'erroneous-overlay
     (guile-reset-symclash-obarray)
     (if (not (and guile-last-displayed-erring-overlay
		   (eq (overlay-buffer guile-last-displayed-erring-overlay)
		       (current-buffer))))
	 (progn
	   (setq guile-last-displayed-erring-overlay nil)
	   (setq guile-displayed-erring-buffers nil)))
     (for-each (function (lambda (buffer)
			   (guile-check-buffer-1 buffer)
			   (setq guile-displayed-erring-buffers
				 (cons buffer
				       guile-displayed-erring-buffers))))
	       (let ((ls (guile-get-enhanced-buffers))
		     (rem guile-displayed-erring-buffers))
		 (while rem
		   (setq ls (delq (car rem) ls))
		   (setq rem (cdr rem)))
		 ls))
     nil)))

(defun guile-check-buffer (buffer)
  (interactive (list (current-buffer)))
  (guile-show-check-error
   (catch 'erroneous-overlay
     (save-excursion
       (guile-reset-symclash-obarray)
       (guile-check-buffer-1 buffer)
       ;(set-buffer old-buffer)
       nil))))

(defun guile-show-check-error (oinfo)
  (if (not oinfo)
      (progn
	(if guile-last-displayed-erring-overlay
	    (message "No more errors found among buffers in enhanced editing mode!")
	  (message "No errors found among buffers in enhanced editing mode!"))
	(setq guile-last-displayed-erring-overlay nil)
	(setq guile-displayed-erring-buffers nil)
	t)
    (setq guile-last-displayed-erring-overlay (car oinfo))
    (set-buffer (overlay-buffer (car oinfo)))
    (goto-char (overlay-start (car oinfo)))
    (if (not guile-quiet)
	(ding))
    (guile-display-sexp-at-point)
    (recenter)
    (message "%s" (cdr oinfo))
    nil))

(defvar guile-last-displayed-erring-overlay nil)

(defun guile-check-buffer-1 (buffer)
  (set-buffer buffer)
  (save-excursion
    (for-each (function guile-check-overlay)
	      (let* ((ls (reverse scheme-buffer-overlays))
		     (tail (memq guile-last-displayed-erring-overlay ls)))
		(if tail
		    (cdr tail)
		  ls)))))

(defconst guile-defexpr "(\\(define\\|defmacro\\)[^ \t\n()]*[ \t\n]+(*\\([^ \t\n()]+\\)")
(defconst guile-defexpr-name 2)

(defun guile-check-overlay (overlay)
  (if (overlay-get overlay 'brokenp)
      (throw 'erroneous-overlay
	     (cons overlay "Bad expression."))
    (goto-char (overlay-start overlay))
    (if (looking-at guile-defexpr)
	(let ((sym (intern (match-string guile-defexpr-name)
			   guile-symclash-obarray)))
	  (if (boundp sym)
	      (let* ((overlay1 (symbol-value sym))
		     (buffer (overlay-buffer overlay1))
		     (line (save-excursion
			     (set-buffer buffer)
			     (save-excursion
			       (goto-char (overlay-start overlay1))
			       (guile-current-line)))))
		(throw 'erroneous-overlay
		       (cons overlay
			     (format "Symbol \"%s\" already defined in %s, line %d."
				     sym
				     (file-name-nondirectory
				      (or (guile-buffer-file-name buffer)
					  (buffer-name buffer)))
				     line))))
	    (set sym overlay))))))

(defun guile-sync-with-scheme ()
  (interactive)
  (if (and (not guile-synchronizedp)
	   scheme-ready-p)
      (progn
	(setq guile-error-p nil)
	(setq guile-last-erring-overlay nil)
	(catch 'exit
	  (for-each (function guile-sync-buffer-1)
		    (guile-get-associated-buffers))
	  (setq guile-synchronizedp t))
	(if guile-last-erring-overlay
	    (progn
	      (overlay-put guile-last-erring-overlay 'brokenp t)
	      (overlay-put guile-last-erring-overlay
			   'face guile-broken-face)
	      (if guile-show-overlays-p
		  (save-excursion
		    (set-buffer (overlay-buffer guile-last-erring-overlay))
		    (guile-show-overlays))))))))

(defun guile-sync-buffer (buffer)
  (interactive (list (current-buffer)))
  (catch 'exit
    (guile-sync-buffer-1 buffer)))

(defun guile-sync-buffer-1 (buffer)
  (save-excursion
    (set-buffer buffer)
    (if scheme-buffer-modified-p
	(progn
	  ;; Can we do it by loading the file again?
	  (if (and (not (buffer-modified-p buffer))
		   (file-readable-p (guile-buffer-file-name))
		   (not (let ((overlays scheme-buffer-overlays))
			  (while (and overlays
				      (not (overlay-get (car overlays) 'brokenp)))
			    (goto-char (overlay-start (car overlays)))
			    (overlay-put (car overlays) 'original-line
					 (guile-current-line)) ; non-optimal *fixme*
			    (setq overlays (cdr overlays)))
			  overlays)))
	      (progn
		(guile-load-file (guile-buffer-file-name))
		(if guile-error-p
		    (progn
		      (throw 'exit nil)))
		(let ((overlays scheme-buffer-overlays))
		  (while overlays
		    (overlay-put (car overlays) 'modifiedp nil)
		    (setq overlays (cdr overlays)))))
	    ;; No - we have to send the overlays separately from top to bottom
	    (let ((overlays (reverse scheme-buffer-overlays)))
	      (if (or (= (point-min) (point-max))
		      (not (eq (char-syntax (char-after (point-min))) ?\()))
		  (setq overlays (cdr overlays)))
	      (while overlays
		(if (and (overlay-get (car overlays) 'modifiedp)
			 (not (overlay-get (car overlays) 'brokenp)))
		    (progn
		      (guile-send-overlay (guile-alloc-finfo (car overlays)))
		      (if guile-error-p (throw 'exit nil))))
		(setq overlays (cdr overlays)))))
	  (setq scheme-buffer-modified-p nil)))
    (if guile-show-overlays-p
	(guile-show-overlays))))

(defun guile-alloc-finfo (overlay)
  (if (not (overlay-get overlay 'id))
      (progn
	(let ((finfo (scheme-virtual-file-list-find (guile-buffer-file-name))))
	  (if finfo
	      (setcdr finfo (delq overlay (cdr finfo)))))
	(guile-new-finfo overlay))
    (let ((finfo (assq (overlay-get overlay 'id)
		       scheme-virtual-file-list)))
      (if finfo
	  (let ((id (guile-alloc-virtual-id overlay)))
	    (setcar finfo id)
	    (overlay-put overlay 'id id)
	    (overlay-put overlay 'virtualp t)
	    finfo)
	(guile-new-finfo overlay)))))

(defun guile-new-finfo (overlay)
  (let* ((id (guile-alloc-virtual-id overlay))
	 (finfo (cons id (list overlay))))
    (overlay-put overlay 'id id)
    (overlay-put overlay 'virtualp t)
    (goto-char (overlay-start overlay))
    (overlay-put overlay 'original-line (guile-current-line))
    (setq scheme-virtual-file-list
	  (cons finfo scheme-virtual-file-list))
    finfo))

(defvar guile-last-prompt-end nil)
(defvar guile-input-sent-p t)

(defun guile-send-input ()
  (interactive)
  (if (and (marker-position guile-last-prompt-end)
	   scheme-ready-p)
      (let ((start (save-excursion
		     (goto-char (point-max))
		     (and (guile-real-safe-backward-sexp)
			  (point)))))
	(if (not (and start
		      (<= (marker-position guile-last-prompt-end) start)
		      (guile-whitespace-between-p guile-last-prompt-end
						  start)))
	    (progn
	      (insert "\n")
	      (put-text-property (1- (point)) (point) 'face 'bold))
	  (goto-char (point-max))
	  (comint-send-input)
	  (setq guile-input-sent-p t)))
    (comint-send-input)))

(defconst guile-whitespace-chars " \t\n\r\f")

(defun guile-whitespace-between-p (beg end)
  (let ((beg (if (markerp beg) (marker-position beg) beg))
	(end (if (markerp end) (marker-position end) end)))
    (if (> beg end)
	(let ((swap beg))
	  (setq beg end end swap)))
    (save-excursion
      (goto-char beg)
      (skip-chars-forward guile-whitespace-chars end)
      (= (point) end))))

;;*fixme* This is redundant code.  Compare sync.
(defun guile-send-changes ()
  (interactive)
  (setq guile-last-displayed-erring-overlay nil)
  (setq guile-displayed-erring-buffers nil)
  (setq guile-quiet nil)
  (if (guile-check-all-1)
      (progn
	(setq guile-error-p nil)
	(catch 'exit
	  (let ((old-buffer (current-buffer)))
	    (for-each (function
		       (lambda (buffer)
			 (set-buffer buffer)
			 (save-excursion
			   (goto-char (point-max))
			   (let ((end (point)))
			     (beginning-of-buffer)
			     (guile-send-region (point) end nil t)))
			 (if guile-show-overlays-p
			     (guile-show-overlays))))
		      (guile-get-enhanced-buffers))
	    (set-buffer old-buffer))))))

(defun scheme-send-region (start end)
  "Send the current region to the inferior Scheme process."
  (interactive "r")
  (if (not (guile-enhancedp (current-buffer)))
      (progn
	(comint-send-region (scheme-proc) start end)
	(comint-send-string (scheme-proc) "\n"))
    (setq guile-error-p nil)
    (catch 'exit
      (guile-send-region start end t)
      (cond (guile-define-header-emitted-p
	     (message "Defined."))
	    (guile-last-result
	     (guile-insert-before-prompt
	      (concat "RESULT: " guile-last-result "\n"))
	     (message "%s" (concat "Result: " guile-last-result)))))
    (if guile-show-overlays-p
	(guile-show-overlays))))

(defvar guile-define-name-marker)

(defun guile-insert-before-prompt (string)
  (save-excursion
    (set-buffer scheme-buffer)
    (save-excursion
      (goto-char guile-last-prompt-end)
      (forward-line 0) ;; ignore field boundary
      (let ((inhibit-read-only t)
	    (before-prompt (point))
	    (w (or (get-buffer-window scheme-buffer 'visible)
		   (get-buffer-window scheme-buffer t))))
	(let ((w-start (and w (window-start w))))
	  (insert-before-markers string)
	  (if (and w (= before-prompt w-start))
	      (let ((selected (selected-window)))
		(unwind-protect
		    (progn
		      (select-window w)
		      (recenter))
		  (select-window selected)
		  (set-buffer scheme-buffer)))))))))

(defvar guile-define-header-emitted-p nil)
(defvar guile-define-startcol 0)
(defvar guile-define-filler "")
(defvar guile-define-fillcol 0)
(defvar guile-last-result nil)

(defun guile-send-region (start end send-all-p &optional multip)
  (if (not scheme-ready-p)
      (error "Scheme is not ready to receive expressions from Emacs."))
  (let ((overlays (reverse scheme-buffer-overlays)))
    (if (or (= (point-min) (point-max))
	    (not (eq (char-syntax (char-after (point-min))) ?\()))
	(setq overlays (cdr overlays)))
    ;; First skip some overlays
    (while (and overlays (<= (overlay-end (car overlays)) start))
      (setq overlays (cdr overlays)))
    (setq guile-define-header-emitted-p nil)
    (setq guile-last-result nil)
    (let ((start (max start (overlay-start (car overlays)))))
      (if (/= start (overlay-start (car overlays)))
	  (guile-send-overlay (save-excursion
				(guile-alloc-finfo (car overlays)))
			      t
			      multip
			      start
			      end)
	(while (and overlays
		    (< (overlay-start (car overlays)) end))
	  (if (and (not (overlay-get (car overlays) 'brokenp))
		   (or send-all-p
		       (overlay-get (car overlays) 'modifiedp)))
	      (guile-send-overlay (save-excursion
				    (guile-alloc-finfo (car overlays)))
				  t
				  multip))
	  (setq overlays (cdr overlays)))))))

(defconst guile-end-of-chunk "\001\n")

;; *fixme* Improve code.
(defun guile-send-overlay (finfo &optional interactivep multip start end)
  (let* ((filename (car finfo))
	 (overlay (car (cdr finfo)))
	 (module-overlay (overlay-get overlay 'module-overlay))
	 (module (or (and module-overlay
			  (overlay-get module-overlay 'define-module))
		     "#f"))
	 (old-buffer (current-buffer))
	 (old-pos (point)))

    ;; Define the module of the overlay if not done before
    (if (and module-overlay
	     (overlay-get module-overlay 'modifiedp))
	(guile-send-overlay (save-excursion
			      (guile-alloc-finfo module-overlay))))
	 
    (set-buffer scheme-buffer)
    ;; Inhibit process output and hamster it
    (setq comint-allow-output-p nil)
    (setq guile-eval-output nil)
    (setq guile-unallowed-output "")

    (set-buffer old-buffer)
    ;; Turn on runlight
    (scheme-enter-load)
    ;; Send load command
    (comint-send-string
     (scheme-proc)
     (if start
	 (let ((column (save-excursion
			 (goto-char start)
			 (current-column))))
	   (format "(%%%%emacs-load %S %d %d '%s #%c)\n"
		   filename
		   (+ (overlay-get overlay 'original-line)
		      -1
		      (count-lines (overlay-get overlay 'original-line)
				   start)
		      (if (zerop column) 0 -1))
		   column
		   module
		   (if interactivep ?t ?f)))
       (format "(%%%%emacs-load %S %d %d '%s #%c)\n"
	       filename
	       (1- (overlay-get overlay 'original-line))
	       0
	       module
	       (if interactivep ?t ?f))))
    ;; Send overlay contents
    (comint-send-string
     (scheme-proc)
     (buffer-substring-no-properties (or start (overlay-start overlay))
				     (or end (overlay-end overlay))))
    ;; If this is the last overlay we may have to send a final newline
    ;;(if (and (eq overlay scheme-buffer-last-overlay)
    ;;	     (/= (overlay-start overlay)
    ;;		 (overlay-end overlay))
    ;;	     (not (eq (char-after (1- (overlay-end overlay))) ?\n)))
    (comint-send-string (scheme-proc) "\n")
    ;; Remove modified mark so that Emacs will trust its idea about positions.
    (or start (overlay-put overlay 'modifiedp nil))
    ;; Send end-of-text
    (comint-send-string (scheme-proc) guile-end-of-chunk)
    ;; Wait for acknowledge.
    (while (and scheme-load-p (not guile-error-p))
      (accept-process-output (scheme-proc) 0 guile-process-timeout))

    ;; Have we received an error?
    (if guile-error-p
	(progn
	  (if interactivep
	      (save-excursion
		(set-buffer scheme-buffer)
		(let ((output guile-unallowed-output))
		  (if (string-match "\\(^ABORT:.*\n\\)+" output)
		      (guile-insert-before-prompt (match-string 1 output))))))
	  (overlay-put overlay 'modifiedp t)
	  (setq scheme-load-p nil)
	  (throw 'exit nil)))		;Abort whatever we was doing.

    ;; The transfer has been successful.  Display defined symbol.
    (if interactivep
	(progn
	  (goto-char (overlay-start overlay))
	  (if (and (not (and start (/= start (overlay-start overlay))))
		   (looking-at guile-defexpr))
	      (progn
		(guile-display-name (match-string guile-defexpr-name)
				    multip)
		(setq guile-last-result nil))
	    (set-buffer scheme-buffer)
	    (if guile-eval-output
		(guile-insert-before-prompt guile-eval-output))
	    (setq guile-last-result guile-eval-result)
	    (set-buffer old-buffer))
	  (goto-char old-pos)
	  (sit-for 0))

      (goto-char old-pos))))

(defun guile-display-name (name multip)
  (save-excursion
    (let ((buffer-file (guile-buffer-file-name))
	  (buffer-name (buffer-name)))
      (set-buffer scheme-buffer)
      (save-excursion
	(let ((inhibit-read-only t))
	  (if (not guile-define-header-emitted-p)
	      (let ((header
		     (format "DEFINED:%s ()\n"
			     (if multip
				 (concat " "
					 (or (and buffer-file
						  (file-name-nondirectory
						   buffer-file))
					     buffer-name))
			       ""))))
		(guile-insert-before-prompt header)
		(set-marker guile-define-name-marker
			    (save-excursion
			      (goto-char guile-last-prompt-end)
			      (forward-line 0)
			      (- (point) 2)))
		(setq guile-define-startcol (- (length header) 2))
		(setq guile-define-filler
		      (concat "\n"
			      (make-string guile-define-startcol ? )))
		(setq guile-define-fillcol
		      (let ((window (get-buffer-window scheme-buffer t)))
			(if window
			    (- (window-width window) 3)
			  fill-column)))
		(setq guile-define-header-emitted-p t)))
	  (goto-char guile-define-name-marker)
	  (cond ((= (current-column) guile-define-startcol))
		((> (+ (current-column) (length name)) guile-define-fillcol)
		 (insert-before-markers guile-define-filler))
		(t (insert-before-markers " ")))
	  (insert-before-markers name))))))

;;; Enhanced editing
;;;

(defvar guile-n-enhanced-buffers 0
  "Number of buffers in enhanced edit mode.")

(defun guile-enhancedp (&optional buffer)
  (interactive)
  (if (not buffer)
      scheme-buffer-overlays
    (save-excursion
      (set-buffer buffer)
      scheme-buffer-overlays)))

(defun guile-get-enhanced-buffers ()
  (let ((ls (buffer-list))
	(ans '()))
    (while ls
      (if (guile-enhancedp (car ls))
	  (setq ans (cons (car ls) ans)))
      (setq ls (cdr ls)))
    (reverse ans)))

(defun guile-enhanced-edit (buffer &optional known-by-scheme)
  "Put the current scheme buffer into enhanced editing mode."
  (interactive (list (current-buffer)))
  (if (guile-enhancedp buffer)
      (error "Already in enhanced editing mode!"))
  (save-excursion
    (set-buffer buffer)
    (guile-parse-buffer known-by-scheme)
    (setq scheme-overlay-repair-function 'guile-repair-overlays)
    (if (not (memq scheme-overlay-repair-idle-timer timer-idle-list))
	(setq scheme-overlay-repair-idle-timer
	      (run-with-idle-timer 0.1 t 'run-hook-with-args
				   'scheme-overlay-repair-function)))
    (setq guile-n-enhanced-buffers (1+ guile-n-enhanced-buffers)))
  (force-mode-line-update))

(defun guile-normal-edit (buffer)
  "Exit enhanced editing mode."
  (interactive (list (current-buffer)))
  (if (guile-attachedp)
      (error "Can't exit enhanced editing mode while attached to scheme.  Detach first."))
  (save-excursion
    (set-buffer buffer)
    (for-each (function (lambda (overlay)
			  (if (overlayp overlay) ; For stability's sake
			      (progn
				(if (guile-virtual-p overlay)
				    (scheme-virtual-unlink (overlay-get overlay 'id)))
				(delete-overlay overlay)))))
	      scheme-buffer-overlays)
    (setq scheme-buffer-overlays ())
    (setq scheme-buffer-last-overlay nil)
    ;; Since we let go of the control, we have to mark the buffer...
    ;(setq scheme-buffer-modified-p t) Now using first-change-hook.
    (setq scheme-overlay-repair-function nil)
    (scheme-virtual-unlink (guile-buffer-file-name buffer))
    (setq guile-n-enhanced-buffers (1- guile-n-enhanced-buffers)))
  (force-mode-line-update))

;;; Overlay lists
;;;
;;; Every non-broken overlay containing a sexp starts with a character
;;; with syntax ?\(.
;;; The first overlay in the overlay list is never broken.

(defun guile-current-line ()
  (+ (count-lines 1 (point))
     (if (= (current-column) 0) 1 0)))

(defun guile-safe-forward-sexp ()
  "Move point one sexp forwards.
Returns non-nil if no error was encountered."
  (not (condition-case err
	   (forward-sexp)
	 (error err))))

(defun guile-safe-backward-sexp ()
  "Move point one sexp forwards.
Returns non-nil if no error was encountered."
  (not (condition-case err
	   (backward-sexp)
	 (error err))))

(defun guile-real-safe-backward-sexp ()
  (and (guile-safe-backward-sexp)
       (progn
	 (and (char-before)
	      (char-before (1- (point)))
	      (eq (char-before (1- (point))) ?#)
	      (eq (char-syntax (char-before)) ?w)
	      (forward-char -2))
	 t)))

(defun guile-parse-buffer (&optional initialp)
  (interactive)
  (if (= (point-min) (point-max))
      ;; Apparently, the buffer is empty
      (progn
	(setq overlay (make-overlay (point-min) (point-max) nil nil t))
	(overlay-put overlay 'modification-hooks
		     '(guile-handle-modification))
	(overlay-put overlay 'insert-behind-hooks
		     '(rear-sticky-overlay-function guile-handle-modification))
	(setq scheme-buffer-overlays (list overlay))
	(setq scheme-buffer-last-overlay overlay))
    (setq scheme-buffer-last-overlay nil)
    (guile-reparse-buffer nil (point-min) initialp)
    (guile-modularize scheme-buffer-overlays)))

(defvar guile-tail-cons (cons nil nil))

(defun guile-cons-before-match (x ls)
  "Match X against successive elements of LS.
Return cons before the one with car matching X."
  (if (or (null ls)
	  (eq (car ls) x))
      nil
    (while (and (cdr ls) (not (eq (car (cdr ls)) x)))
      (setq ls (cdr ls)))
    (and (cdr ls)
	 ls)))

;; Here I've sacrificed readability for speed...
;; Geeh! What a monstrum!
;;
(defun guile-reparse-buffer (start-overlay limit &optional initialp)
  "Reparse buffer backwards to build/update `scheme-buffer-overlays'.
Start with overlay START-OVERLAY.  Stop when we have passed LIMIT.
If START-OVERLAY is nil parsing starts from (point-max).
The optional third argument INITIALP should be non-nil if parsing
for the first time.  This will cause initialization of the
original-line property."
  (let* ((tailp (and start-overlay
		     (progn
		       (goto-char (overlay-end start-overlay))
		       (if (bolp)
			   (guile-cons-before-match start-overlay
						    scheme-buffer-overlays)
			 (let ((after (guile-cons-before-match
				       start-overlay
				       scheme-buffer-overlays)))
			   (if after
			       (progn
				 (overlay-put (car after) 'brokenp t)
				 (guile-cons-before-match
				  after
				  scheme-buffer-overlays))))))))
	 (tail (or tailp guile-tail-cons))
	 (overlays (if tailp (cdr tail) scheme-buffer-overlays))
	 (overlay nil)
	 (first-broken nil)
	 (last-broken nil)
	 (last-end (if tailp
		       (overlay-end (car (cdr tail)))
		     (point-max))))
    (goto-char last-end)
    ;; Parse buffer backwards...
    (save-match-data
      (while (> (point) limit)
	;; First try to move one sexp backwards...
	(if (and (guile-safe-backward-sexp)
		 (bolp))
	    (progn
	      ;; Do we have it in the list?
	      (while (and overlays
			  (> (overlay-start (car overlays)) (point)))
		;; First throw away some trash overlays...
		(let ((id (overlay-get (car overlays) 'id)))
		  (delete-overlay (car overlays))
		  (if id
		      ;; It's a stand-alone sexp, remove it from the list
		      (scheme-virtual-unlink id)))
		(setq overlays (cdr overlays)))
	      (if (and overlays
		       (= (overlay-start (car overlays)) (point)))
		  ;; Yes!
		  (progn		; Is it intact?
		    (if (or (overlay-get (car overlays) 'brokenp)
			    (/= (overlay-end (car overlays)) last-end))
			;; No...
			(progn
			  ;; Adjust it.
			  (move-overlay (car overlays) (point) last-end)
			  ;; Can we repair it?
			  (if (if (bobp)
				  (or (eolp)
				      (eq (char-syntax (following-char)) ?\()
				      (eq (char-syntax (following-char)) ?<)
				      (eq (char-syntax (following-char)) ? ))
				(eq (char-syntax (following-char)) ?\())
			      ;; Yes!
			      (progn
				(overlay-put (car overlays) 'brokenp nil)
				(overlay-put (car overlays) 'face nil)
				(overlay-put (car overlays) 'modifiedp t)
				(overlay-put (car overlays)
					     'define-module
					     (and (looking-at "(define-module \\((.*)\\)")
						  (condition-case err
						      (save-excursion
							(goto-char (match-beginning 1))
							(read (current-buffer)))
						    (error nil)))))
			    ;; No...
			    (overlay-put (car overlays) 'face guile-broken-face)
			    (overlay-put (car overlays) 'modifiedp t))))
		    ;; Link it in.
		    (setcdr tail overlays)
		    (setq tail (cdr tail))
		    (setq overlays (cdr overlays)))
		;; We probably have to make a new overlay...
		;; First check if it's OK.
		(if (if (bobp)
			(or (eolp)
			    (eq (char-syntax (following-char)) ?\()
			    (eq (char-syntax (following-char)) ?<)
			    (eq (char-syntax (following-char)) ? ))
		      (eq (char-syntax (following-char)) ?\())
		    ;; Everything seems OK with this one.
		    (progn
		      (setq overlay (make-overlay (point) last-end nil nil t))
		      (if initialp
			  (overlay-put overlay 'original-line
				       (guile-current-line))
			(overlay-put overlay 'modifiedp t))
		      (overlay-put overlay 'modification-hooks
				   '(guile-handle-modification))
		      (overlay-put overlay
				   'define-module
				   (and (looking-at "(define-module \\((.*)\\)")
					(condition-case err
					    (save-excursion
					      (goto-char (match-beginning 1))
					      (read (current-buffer)))
					  (error nil))))
		      ;; And link it in...
		      (setcdr tail (cons overlay overlays))
		      (setq tail (cdr tail)))
		  ;; But this one is broken!
		  ;; Try to find some structure...
		  (guile-backward-broken-sexp)
		  (while (and overlays
			      (> (overlay-start (car overlays)) (point)))
		    (let ((id (overlay-get (car overlays) 'id)))
		      (delete-overlay (car overlays))
		      (if id
			  (scheme-virtual-unlink id)))
		    (setq overlays (cdr overlays)))
		  ;; Is it possibly the first one in the overlay list?
		  (if (and overlays
			   (= (overlay-start (car overlays)) (point)))
		      (progn
			;; Adjust it.
			(move-overlay (car overlays) (point) last-end)
			(overlay-put (car overlays) 'face guile-broken-face)
			(overlay-put (car overlays) 'modifiedp t)
			;; Link it in.
			(setcdr tail overlays)
			(setq tail (cdr tail))
			(setq overlays (cdr overlays)))
		    ;; It wasn't - make a new overlay.
		    (setq overlay (make-overlay (point) last-end nil nil t))
		    (overlay-put overlay 'brokenp t)
		    (overlay-put overlay 'face guile-broken-face)
		    (overlay-put overlay 'modification-hooks
				 '(guile-handle-modification))
		    ;; And link it in...
		    (setcdr tail (cons overlay overlays))
		    (setq tail (cdr tail))))))
	  ;; Broken overlay... Here we go again!
	  (guile-backward-broken-sexp)
	  (while (and overlays
		      (> (overlay-start (car overlays)) (point)))
	    (let ((id (overlay-get (car overlays) 'id)))
	      (delete-overlay (car overlays))
	      (if id
		  (scheme-virtual-unlink id)))
	    (setq overlays (cdr overlays)))
	  (if (and overlays
		   (= (overlay-start (car overlays)) (point)))
	      (progn
		(setq overlay (car overlays))
		(move-overlay overlay (point) last-end)
		(setcdr tail overlays)
		(setq tail (cdr tail))
		(setq overlays (cdr overlays)))
	    (setq overlay (make-overlay (point) last-end nil nil t))
	    (overlay-put overlay 'modification-hooks
			 '(guile-handle-modification))
	    (setcdr tail (cons overlay overlays))
	    (setq tail (cdr tail)))
	  (overlay-put overlay 'brokenp t)
	  (overlay-put overlay 'face guile-broken-face))
	(if (overlay-get (car tail) 'brokenp)
	    (progn
	      (setq first-broken (car tail))
	      (if (not last-broken)
		  (setq last-broken (car tail)))))
	(setq last-end (point))))
    (if (not tailp)
	(progn
	  (setq scheme-buffer-overlays
		(cdr guile-tail-cons))
	  ;; Don't let the rear-stickiness propagate upwards...
	  (if scheme-buffer-last-overlay
	      (if (not (eq (car scheme-buffer-overlays)
			   scheme-buffer-last-overlay))
		  (progn
		    (overlay-put scheme-buffer-last-overlay
				 'insert-behind-hooks
				 nil)
		    (overlay-put (car scheme-buffer-overlays)
				 'insert-behind-hooks
				 '(rear-sticky-overlay-function
				   guile-handle-modification))))
	    (overlay-put (car scheme-buffer-overlays)
			 'insert-behind-hooks
			 '(rear-sticky-overlay-function guile-handle-modification)))
	  (setq scheme-buffer-last-overlay
		(car scheme-buffer-overlays))))
    (setq guile-last-broken last-broken)
    (setq guile-repair-limit
	  (if first-broken
	      ;(overlay-start
	      ; (let ((ovls (memq first-broken scheme-buffer-overlays)))
	      ;   (or (and ovls (cdr ovls) (car (cdr ovls)))
	      ;       first-broken)
	      (overlay-start first-broken)
	    guile-big-integer)))
  (if guile-show-overlays-p
      (guile-show-overlays))
  )

(defvar guile-last-broken nil)
(defvar guile-repair-limit guile-big-integer)

(defun guile-handle-modification (overlay after from to &optional length)
  (if after
      (progn
	(overlay-put overlay 'brokenp t)
	(setq scheme-buffer-overlays-modified-p t)
	(if guile-last-broken
	    (if (< (overlay-start overlay) guile-repair-limit)
		(setq guile-repair-limit
		      ;(overlay-start
		      ; (let ((ovls (memq overlay scheme-buffer-overlays)))
		      ;   (or (and ovls (cdr ovls) (car (cdr ovls)))
		      ;      overlay)))
		      (overlay-start overlay))
	      (if (> (overlay-start overlay)
		     (overlay-start guile-last-broken))
		  (setq guile-last-broken overlay)))
	  (setq guile-last-broken overlay)
	  (setq guile-repair-limit
		;(overlay-start
		; (let ((ovls (memq overlay scheme-buffer-overlays)))
		;   (or (and ovls (cdr ovls) (car (cdr ovls)))
		;       overlay)))
		(overlay-start overlay))))))

(defun guile-repair-overlays ()
  (if (and (eq major-mode 'scheme-mode)
	   scheme-buffer-overlays-modified-p)
      (save-excursion
	;(ding)
	;(message "Repair!")
	(setq scheme-buffer-modified-p t)
	(if scheme-associated-process-buffer
	    (setq guile-synchronizedp nil))
	(guile-reparse-buffer guile-last-broken guile-repair-limit)
	(guile-modularize scheme-buffer-overlays)
	(setq scheme-buffer-overlays-modified-p nil))))

(defun guile-modularize (r-overlays)
  (let ((overlays (reverse r-overlays))
	(module nil))
    (while overlays
      (if (overlay-get (car overlays) 'define-module)
	  (progn
	    (overlay-put (car overlays) 'module-overlay nil)
	    (setq module (car overlays)))
	(overlay-put (car overlays) 'module-overlay module))
      (setq overlays (cdr overlays)))))

(defun guile-backward-broken-sexp ()
  (interactive)
  (beginning-of-line)
  (let ((last (point)))
    (while (not (or (bobp)
		    (and (eq (following-char) ?\()
			 (guile-safe-backward-sexp)
			 (bolp))))
      (forward-line -1)
      (beginning-of-line)
      (setq last (point)))
    (let ((end (point)))
      (goto-char (if (guile-safe-forward-sexp)
		     last
		   end)))))

;; rear-sticky-overlay-function:
;; Put this function in the `insert-behind-hooks' of an overlay
;; in order to make the overlay rear-sticky.

(defun rear-sticky-overlay-function (overlay after from to &optional length)
  (if after
      (move-overlay overlay (overlay-start overlay) to)))

;;; Some debugging utilities
;;;

(defvar guile-show-overlays-p nil)

(defun guile-show-overlays ()
  (interactive)
  (if (guile-enhancedp)
      (let ((n 1)
	    (color nil)
	    (previous nil)
	    (overlays scheme-buffer-overlays))
	(if (null overlays)
	    (progn
	      (ding)
	      (message "Empty overlay list!"))
	  (if (not (memq 'rear-sticky-overlay-function
			 (overlay-get (car overlays) 'insert-behind-hooks)))
	      (progn
		(ding)
		(message "Last overlay not rear-sticky!")))
	  (while overlays
	    (overlay-put (car overlays)
			 'face
			 (if (setq color (not color))
			     (if (overlay-get (car overlays) 'brokenp)
				 guile-broken-face-1
			       (if (overlay-get (car overlays) 'modifiedp)
				   guile-modified-face-1
				 guile-unmodified-face-1))
			   (if (overlay-get (car overlays) 'brokenp)
			       guile-broken-face-2
			     (if (overlay-get (car overlays) 'modifiedp)
				 guile-modified-face-2
			       guile-unmodified-face-2))))
	    (if previous
		(progn
		  (if (/= (overlay-end (car overlays))
			  (overlay-start previous))
		      (progn (ding)
			     (message "Bad end boundary at overlay no. %d" n)))
		  (if (overlay-get (car overlays) 'insert-behind-hooks)
		      (progn
			(ding)
			(message "Inner overlay no. %d rear-sticky!" n)))))
	    (setq previous (car overlays))
	    (setq n (1+ n))
	    (setq overlays (cdr overlays)))
	  (if (/= (overlay-start previous) (point-min))
	      (progn
		(ding)
		(message "First overlay doesn't start at %d" (point-min)))))))
  (setq guile-show-overlays-p t))

(defun guile-hide-overlays ()
  (interactive)
  (let ((color nil)
	(overlays scheme-buffer-overlays))
    (while overlays
      (overlay-put (car overlays)
		   'face
		   (if (overlay-get (car overlays) 'brokenp)
		       guile-broken-face
		     nil))
      (setq overlays (cdr overlays))))
  (setq guile-show-overlays-p nil))

;; *fixme* Consider removing this function
(defun guile-kill-overlays (&optional ls)
  (interactive)
  (if (not ls)
      (progn
	(setq ls (apply (function append)
			(mapcar (function cdr)
				scheme-virtual-file-list)))
	(setq scheme-virtual-file-list ())))
  (while ls
    (delete-overlay (car ls))
    (setq ls (cdr ls))))

;; *fixme* Consider removing this function
(defun overlay-kill ()
  (interactive)
  (delete-overlay (car (overlays-at (point)))))

(defun for-each (func ls)
  (while ls
    (funcall func (car ls))
    (setq ls (cdr ls))))


;;; Completion

(defconst guile-symbol-chars "---A-ZÅÄÖa-zåäö0-9!$%&/=?@+*<>|-_:.")

(defun guile-match-symnames (word &optional exactp)
  (if (not word)
      '()
    (save-excursion
      (set-buffer scheme-buffer)
      (guile-eval `(map symbol->string
			(%%apropos-internal
			 ,(concat "^"
				  (regexp-quote word)
				  (and exactp "$"))))))))

(defmacro guile-force-splittable (&rest forms)
  `(let ((f (selected-frame))
	 (w (selected-window)))
     (let ((unsplittable (assq 'unsplittable (frame-parameters f)))
	   (dedicatedp (window-dedicated-p w))
	   (same-window-buffer-names
	    (append same-window-buffer-names
		    (list (buffer-name (window-buffer w))))))
       (unwind-protect
	   (progn
	     (modify-frame-parameters f '((unsplittable . nil)))
	     (set-window-dedicated-p w nil)
	     ,@forms)
	 (modify-frame-parameters f (list unsplittable))
	 (set-window-dedicated-p w dedicatedp)))))

(defvar guile-complete-function 'comint-dynamic-complete)

(defun guile-indent-or-complete ()
  (interactive)
  (let ((beg (save-excursion
	       (beginning-of-line)
	       (point))))
    (if (guile-whitespace-between-p beg (point))
	(funcall 'indent-for-tab-command)
      (funcall guile-complete-function))))

(defun guile-complete-symbol ()
  (interactive)
  (let ((word (comint-word guile-symbol-chars)))
    (if word
	(progn
	  (guile-force-splittable
	   (comint-dynamic-simple-complete word (guile-match-symnames word)))
	  (if (string= (buffer-name) scheme-buffer)
	      (put-text-property comint-last-output-start
				 (point) 'face 'bold))))))

(defun guile-list-completions ()
  (interactive)
  (let* ((word (comint-word guile-symbol-chars))
	 (candidates (mapcar (function (lambda (x) (list x)))
			     (guile-match-symnames word)))
	 (completions (all-completions word candidates)))
    (if (null completions)
	(message "No completions of %s" word)
      (guile-force-splittable
       (comint-dynamic-list-completions completions))
      (if (string= (buffer-name) scheme-buffer)
	  (put-text-property comint-last-output-start (point) 'face 'bold)))))

;;; Documentation

(defun guile-documentation-symbols ()
  (save-excursion
    (set-buffer scheme-buffer)
    (guile-eval '(map symbol->string (%%apropos-internal "")))))

(defun guile-variable-at-point (symnames)
  (condition-case ()
      (let ((stab (syntax-table)))
	(unwind-protect
	    (save-excursion
	      (set-syntax-table scheme-mode-syntax-table)
	      (or (not (zerop (skip-syntax-backward "_w")))
		  (eq (char-syntax (following-char)) ?w)
		  (eq (char-syntax (following-char)) ?_)
		  (forward-sexp -1))
	      (skip-chars-forward "'")
	      (let ((obj (read (current-buffer))))
		(and (symbolp obj) (member (symbol-name obj) symnames) obj)))
	  (set-syntax-table stab)))
    (error nil)))

(defun guile-describe-variable (variable)
  "Display the full documentation of Guile variable VARIABLE."
  (interactive
   (let ((symnames (guile-documentation-symbols)))
     (let ((symbol (guile-variable-at-point symnames))
	   (enable-recursive-minibuffers t)
	   val)
     (setq val (completing-read (if symbol
				    (format "Describe Guile variable (default %s): " symbol)
				  "Describe Guile variable: ")
				(mapcar (lambda (s)
					  (cons s '()))
					symnames)
				nil t))
     (list (if (equal val "")
	       symbol
	     (intern val))))))
  (guile-force-splittable
   (with-output-to-temp-buffer "*Help*"
     (prin1 variable)
     (princ ": ")
     (princ (save-excursion
	      (set-buffer scheme-buffer)
	      (guile-eval variable t)))
     (terpri)
     (terpri)
     (let ((doc (save-excursion
		  (set-buffer scheme-buffer)
		  (guile-eval `(%%emacs-symdoc ',variable)))))
       (if doc
	   (princ doc)
	 (princ "not documented")))
     (print-help-return-message)
     (save-excursion
       (set-buffer standard-output)
       (help-mode)
       ;; Return the text we displayed.
       (buffer-string)))))

(provide 'guile)
(run-hooks 'guile-load-hook)
