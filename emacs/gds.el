;;; gds.el -- frontend for Guile development in Emacs

;;;; Copyright (C) 2003 Free Software Foundation, Inc.
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2.1 of the License, or (at your option) any later
;;;; version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free
;;;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;;;; 02111-1307 USA


;;;; Prerequisites.

(require 'widget)
(require 'wid-edit)
(require 'scheme)


;;;; Debugging (of this code!).

(defsubst dmessage (msg &rest args)
  ;;(apply (function message) msg args)
  )


;;;; Customization group setup.

(defgroup gds nil
  "Customization options for Guile Emacs frontend."
  :group 'scheme)


;;;; Communication with the (emacs gds-server) subprocess.

;; The subprocess object.
(defvar gds-process nil)

;; Subprocess output goes into the `*GDS Process*' buffer, and
;; is then read from there one form at a time.  `gds-read-cursor' is
;; the buffer position of the start of the next unread form.
(defvar gds-read-cursor nil)

(defun gds-start ()
  "Start (or restart, if already running) the GDS subprocess."
  (interactive)
  (if gds-process (gds-shutdown))
  (with-current-buffer (get-buffer-create "*GDS Process*")
    (erase-buffer)
    (setq gds-process
	  (let ((process-connection-type nil)) ; use a pipe
	    (start-process "gds"
			   (current-buffer)
			   "guile"
			   "-q"
			   "--debug"
			   "-c"
			   "(begin (use-modules (emacs gds-server)) (run-server))"))))
  (setq gds-read-cursor (point-min))
  (set-process-filter gds-process (function gds-filter))
  (set-process-sentinel gds-process (function gds-sentinel))
  (set-process-coding-system gds-process 'latin-1-unix))

;; Shutdown the subprocess and cleanup all associated data.
(defun gds-shutdown ()
  "Shut down the GDS subprocess."
  (interactive)
  ;; Reset variables.
  (setq gds-buffers nil
	gds-focus-client nil
	gds-waiting nil)
  ;; Kill the subprocess.
  (process-kill-without-query gds-process)
  (condition-case nil
      (progn
	(kill-process gds-process)
	(accept-process-output gds-process 0 200))
    (error))
  (setq gds-process nil))

;; Subprocess output filter: inserts normally into the process buffer,
;; then tries to reread the output one form at a time and delegates
;; processing of each form to `gds-handle-input'.
(defun gds-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (process-mark proc))
      (insert-before-markers string))
    (goto-char gds-read-cursor)
    (while (let ((form (condition-case nil
			   (read (current-buffer))
			 (error nil))))
	     (if form
		 (save-excursion
		   (gds-handle-input form)))
	     form)
      (setq gds-read-cursor (point)))))

;; Subprocess sentinel: do nothing.  (Currently just here to avoid
;; inserting un-`read'able process status messages into the process
;; buffer.)
(defun gds-sentinel (proc event)
  )

;; Send input to the subprocess.
(defun gds-send (string)
  (process-send-string gds-process string))


;;;; Multiple application scheduling.

;; Here is how we schedule the display of multiple clients that are
;; competing for user attention.
;;
;; - `gds-waiting' holds a list of clients that want attention but
;; haven't yet got it.  A client is added to this list for two
;; reasons.  (1) When it is blocked waiting for user input.  (2) When
;; it first connects to GDS, even if not blocked.
;;
;; - `gds-focus-client' holds the client, if any, that currently has
;; the user's attention.  A client can be given the focus if
;; `gds-focus-client' is nil at the time that the client wants
;; attention, or if another client relinquishes it.  A client can
;; relinquish the focus in two ways.  (1) If the client application
;; says that it is no longer blocked, and a small time passes without
;; it becoming blocked again.  (2) If the user explicitly `quits' that
;; client.
(defvar gds-focus-client nil)
(defvar gds-waiting nil)

;; Sometimes we want to display a client buffer immediately even if it
;; isn't already in the selected window.  To do we this, we bind the
;; following variable to non-nil.
(defvar gds-immediate-display nil)

(defun gds-request-focus (client)
  (cond ((eq client gds-focus-client)
	 ;; CLIENT already has the focus.  Display its buffer.
	 (gds-display-buffers))
	(gds-focus-client
	 ;; Another client has the focus. Add CLIENT to `gds-waiting'.
	 (or (memq client gds-waiting)
	     (setq gds-waiting (append gds-waiting (list client)))))
	(t
	 ;; Give focus to CLIENT and display its buffer.
	 (setq gds-focus-client client)
	 (gds-display-buffers))))

;; Explicitly give up focus.
(defun gds-quit ()
  (interactive)
  (if (or (car gds-waiting)
	  (not (gds-client-blocked))
	  (y-or-n-p
	   "Client is blocked and no others are waiting.  Still quit? "))
      (let ((gds-immediate-display
	     (eq (window-buffer (selected-window)) (current-buffer))))
	(bury-buffer (current-buffer))
	;; Pass on the focus.
	(setq gds-focus-client (car gds-waiting)
	      gds-waiting (cdr gds-waiting))
	;; If this client is blocked, add it back into the waiting list.
	(if (gds-client-blocked)
	    (gds-request-focus gds-client))
	;; If there is a new focus client, request display for it.
	(if gds-focus-client
	    (gds-request-focus gds-focus-client)))))


;;;; Per-client buffer state.

(define-derived-mode gds-mode
  scheme-mode
  "Guile Interaction"
  "Major mode for interacting with a Guile client application.")

(defvar gds-client nil
  "GDS client's port number.")
(make-variable-buffer-local 'gds-client)

(defvar gds-current-module "()"
  "GDS client's current module.")
(make-variable-buffer-local 'gds-current-module)

(defvar gds-stack nil
  "GDS client's stack when last stopped.")
(make-variable-buffer-local 'gds-stack)

(defvar gds-modules nil
  "GDS client's module information.
Alist mapping module names to their symbols and related information.
This looks like:

 (((guile) t sym1 sym2 ...)
  ((guile-user))
  ((ice-9 debug) nil sym3 sym4)
  ...)

The `t' or `nil' after the module name indicates whether the module is
displayed in expanded form (that is, showing the bindings in that
module).  The syms are actually all strings because some Guile symbols
are not readable by Emacs.")
(make-variable-buffer-local 'gds-modules)

(defvar gds-output nil
  "GDS client's recent output (printed).")
(make-variable-buffer-local 'gds-output)

(defvar gds-status nil
  "GDS client's latest status, one of the following symbols.

`running' - application is running.

`waiting-for-input' - application is blocked waiting for instruction
from the frontend.

`ready-for-input' - application is not blocked but can also accept
asynchronous instructions from the frontend.")
(make-variable-buffer-local 'gds-status)

(defvar gds-pid nil
  "GDS client's process ID.")
(make-variable-buffer-local 'gds-pid)

(defvar gds-debug-exceptions nil
  "Whether to debug exceptions.")
(make-variable-buffer-local 'gds-debug-exceptions)

(defvar gds-exception-keys "signal misc-error"
  "The exception keys for which to debug a GDS client.")
(make-variable-buffer-local 'gds-exception-keys)

;; Cached display variables for `gds-update-buffers'.
(defvar gds-displayed-modules nil)
(make-variable-buffer-local 'gds-displayed-modules)

;; Types of display areas in the *Guile* buffer.
(defvar gds-display-types '("\\`"
			    "^Modules:"
			    "^Transcript:"))
(defvar gds-display-type-regexp
  (concat "\\("
	  (substring (apply (function concat)
			    (mapcar (lambda (type)
				      (concat "\\|" type))
				    gds-display-types))
		     2)
	  "\\)"))

(defun gds-maybe-delete-region (regexp)
  (let ((beg (save-excursion
	       (goto-char (point-min))
	       (and (re-search-forward regexp nil t)
		    (match-beginning 0)))))
    (if beg
	(delete-region beg
		       (save-excursion
			 (goto-char beg)
			 (end-of-line)
			 (or (and (re-search-forward gds-display-type-regexp
						     nil t)
				  (match-beginning 0))
			     (point-max)))))))

(defun gds-maybe-skip-region (regexp)
  (if (looking-at regexp)
      (if (re-search-forward gds-display-type-regexp nil t 2)
	  (beginning-of-line)
	(goto-char (point-max)))))

(defun gds-update-buffers (client)
  (dmessage "gds-update-buffers")
  ;; Avoid continually popping up the last associated source buffer
  ;; unless it really is still current.
  (setq gds-selected-frame-source-buffer nil)
  (set-buffer (cdr (assq client gds-buffers)))
  (force-mode-line-update t)
  (let ((inhibit-read-only t)
	(p (if (eq client gds-focus-client)
	       (point)
	     (point-min)))
	stack-changed)
    ;; Start at top of buffer.
    (goto-char (point-min))
    ;; Display status; too simple to be worth caching.
    (gds-maybe-delete-region (concat "\\`" (regexp-quote (buffer-name))))
    (widget-insert (buffer-name)
		   ", "
		   (cdr (assq gds-status
			      '((running . "running (cannot accept input)")
				(waiting-for-input . "waiting for input")
				(ready-for-input . "running")
				(closed . "closed"))))
		   ", in "
		   gds-current-module
		   "\n")
    (widget-create 'push-button
		   :notify (function gds-sigint)
		   "SIGINT")
    (widget-insert " ")
    (widget-create 'push-button
		   :notify (function gds-async-break)
		   "Break")
    (widget-insert "\n")
    (widget-create 'checkbox
		   :notify (function gds-toggle-debug-exceptions)
		   gds-debug-exceptions)
    (widget-insert " Debug exception keys: ")
    (widget-create 'editable-field
		   :notify (function gds-set-exception-keys)
		   gds-exception-keys)
    (widget-insert "\n")
;     (widget-insert "\n\n")
;     (if (> (length gds-output) 0)
; 	(widget-insert gds-output "\n\n"))
    ;; Display stack.
    (dmessage "insert stack")
    (let ((stack gds-stack)
	  (buf (get-buffer-create (concat (buffer-name) " - stack"))))
      (with-current-buffer buf
	(if (equal stack gds-stack)
	    ;; No change needed.
	    nil
	  (erase-buffer)
	  (gds-mode)
	  ;; Insert new stack.		       
	  (if stack (gds-insert-stack stack))
	  ;; Record displayed stack.
	  (setq gds-stack stack))))      
    ;; Display module list.
    (dmessage "insert modules")
    (if (equal gds-modules gds-displayed-modules)
	(gds-maybe-skip-region "^Modules:")
      ;; Delete existing module list.
      (gds-maybe-delete-region "^Modules:")
      ;; Insert new list.
      (if gds-modules (gds-insert-modules gds-modules))
      ;; Record displayed list.
      (setq gds-displayed-modules (copy-tree gds-modules)))
    ;; Finish off.
    (dmessage "widget-setup")
    (widget-setup)
    (if stack-changed
	;; Stack is being seen for the first time, so make sure top of
	;; buffer is visible.
	(progn
	  (goto-char (point-min))
	  (forward-line (+ 1 (cadr gds-stack))))
      ;; Restore point from before buffer was redrawn.
      (goto-char p))))

(defun gds-sigint (w &rest ignore)
  (interactive)
  (signal-process gds-pid 2))

(defun gds-async-break (w &rest ignore)
  (interactive)
  (gds-send (format "(%S async-break)\n" gds-focus-client)))

(defun gds-toggle-debug-exceptions (w &rest ignore)
  (interactive)
  (setq gds-debug-exceptions (widget-value w))
  (gds-eval-expression (concat "(use-modules (ice-9 debugger))"
			       "(debug-on-error '("
			       gds-exception-keys
			       "))")))

(defun gds-set-exception-keys (w &rest ignore)
  (interactive)
  (setq gds-exception-keys (widget-value w)))

(defun gds-display-buffers ()
  (if gds-focus-client
      (let ((gds-focus-buffer (cdr (assq gds-focus-client gds-buffers))))
	;; If there's already a window showing the buffer, use it.
	(let ((window (get-buffer-window gds-focus-buffer t)))
	  (if window
	      (progn
		(make-frame-visible (window-frame window))
		(select-frame (window-frame window))
		(select-window window))
	    ;(select-window (display-buffer gds-focus-buffer))
	    (display-buffer gds-focus-buffer)))
	;; If there is an associated source buffer, display it as well.
	(if gds-selected-frame-source-buffer
	    (let ((window (display-buffer gds-selected-frame-source-buffer)))
	      (set-window-point window
				(overlay-start
				 gds-selected-frame-source-overlay))))
	;; If there is a stack to display, display it.
	(if gds-stack
	    (let ((buf (get-buffer (concat (buffer-name) " - stack"))))
	      (if (get-buffer-window buf)
		  nil
		(split-window)
		(set-window-buffer (selected-window) buf)))))))

(defun gds-insert-stack (stack)
  (let ((frames (car stack))
	(index (cadr stack))
	(flags (caddr stack))
	frame items)
    (cond ((memq 'application flags)
	   (widget-insert "Calling procedure:\n"))
	  ((memq 'evaluation flags)
	   (widget-insert "Evaluating expression:\n"))
	  ((memq 'return flags)
	   (widget-insert "Return value: "
			  (cadr (memq 'return flags))
			  "\n"))
	  (t
	   (widget-insert "Stack: " (prin1-to-string flags) "\n")))
    (let ((i -1))
      (gds-show-selected-frame (caddr (nth index frames)))
      (while frames
	(setq frame (car frames)
	      frames (cdr frames)
	      i (+ i 1)
	      items (cons (list 'item
				(let ((s (cadr frame)))
				  (put-text-property 0 1 'index i s)
				  s))
			  items))))
    (setq items (nreverse items))
    (apply (function widget-create)
	     'radio-button-choice
	     :value (cadr (nth index items))
	     :notify (function gds-select-stack-frame)
	     items)
    (widget-insert "\n")))

(defun gds-select-stack-frame (widget &rest ignored)
  (let* ((s (widget-value widget))
	 (ind (memq 'index (text-properties-at 0 s))))
    (gds-send (format "(%S debugger-command frame %d)\n"
		      gds-focus-client
		      (cadr ind)))))

;; Overlay used to highlight the source expression corresponding to
;; the selected frame.
(defvar gds-selected-frame-source-overlay nil)

;; Buffer containing source for the selected frame.
(defvar gds-selected-frame-source-buffer nil)

(defun gds-show-selected-frame (source)
  ;; Highlight the frame source, if possible.
  (if (and source
	   (file-readable-p (car source)))
      (with-current-buffer (find-file-noselect (car source))
	(if gds-selected-frame-source-overlay
	    nil
	  (setq gds-selected-frame-source-overlay (make-overlay 0 0))
	  (overlay-put gds-selected-frame-source-overlay 'face 'highlight))
	;; Move to source line.  Note that Guile line numbering is
	;; 0-based, while Emacs numbering is 1-based.
	(save-restriction
	  (widen)
	  (goto-line (+ (cadr source) 1))
	  (move-to-column (caddr source))
	  (move-overlay gds-selected-frame-source-overlay
			(point)
			(if (not (looking-at ")"))
			    (save-excursion (forward-sexp 1) (point))
			  ;; It seems that the source coordinates for
			  ;; backquoted expressions are at the end of
			  ;; the sexp rather than the beginning...
			  (save-excursion (forward-char 1)
					  (backward-sexp 1) (point)))
			(current-buffer)))
	(setq gds-selected-frame-source-buffer (current-buffer)))
    (if gds-selected-frame-source-overlay
	(move-overlay gds-selected-frame-source-overlay 0 0))))

(defcustom gds-module-filter '(t (guile nil) (ice-9 nil) (oop nil))
  "Specification of which Guile modules the debugger should display.
This is a list with structure (DEFAULT EXCEPTION EXCEPTION...), where
DEFAULT is `t' or `nil' and each EXCEPTION has the structure (SYMBOL
DEFAULT EXCEPTION EXCEPTION...).

A Guile module name `(x y z)' is matched against this filter as
follows.  If one of the top level EXCEPTIONs has SYMBOL `x', continue
by matching the rest of the module name, in this case `(y z)', against
that SYMBOL's DEFAULT and next level EXCEPTION list.  Otherwise, if
the current DEFAULT is `t' display the module, and if the current
DEFAULT is `nil', don't display it.

This variable is usually set to exclude Guile system modules that are
not of primary interest when debugging application code."
  :type 'sexp
  :group 'gds)

(defun gds-show-module-p (name)
  ;; Determine whether to display the NAMEd module by matching NAME
  ;; against `gds-module-filter'.
  (let ((default (car gds-module-filter))
	(exceptions (cdr gds-module-filter)))
    (let ((exception (assq (car name) exceptions)))
      (if exception
	  (let ((gds-module-filter (cdr exception)))
	    (gds-show-module-p (cdr name)))
	default))))

(defun gds-insert-modules (modules)
  (insert "Modules:\n")
  (while modules
    (let ((minfo (car modules)))
      (if (gds-show-module-p (car minfo))
	  (let ((w (widget-create 'push-button
				  :notify (function gds-module-notify)
				  (if (and (cdr minfo)
					   (cadr minfo))
				      "-" "+"))))
	    (widget-put w :module (cons client (car minfo)))
	    (widget-insert " " (prin1-to-string (car minfo)) "\n")
	    (if (cadr minfo)
		(let ((syms (cddr minfo)))
		  (while syms
		    (widget-insert " > " (car syms) "\n")
		    (setq syms (cdr syms))))))))
    (setq modules (cdr modules)))
  (insert "\n"))

(defun gds-module-notify (w &rest ignore)
  (let* ((module (widget-get w :module))
	 (client (car module))
	 (name (cdr module))
	 (minfo (assoc name gds-modules)))
    (if (cdr minfo)
	;; Just toggle expansion state.
	(progn
	  (setcar (cdr minfo) (not (cadr minfo)))
	  (gds-update-buffers client))
      ;; Set flag to indicate module expanded.
      (setcdr minfo (list t))
      ;; Get symlist from Guile.
      (gds-send (format "(%S query-module %S)\n" client name)))))

(defun gds-query-modules ()
  (interactive)
  (gds-send (format "(%S query-modules)\n" gds-focus-client)))


;;;; Handling debugging instructions.

;; Alist mapping each client port number to corresponding buffer.
(defvar gds-buffers nil)

;; Return client buffer for specified client and protocol input.
(defun gds-client-buffer (client proc args)
  (if (eq proc 'name)
      ;; Introduction from client - create a new buffer.
      (with-current-buffer (generate-new-buffer (car args))
	(gds-mode)
	(insert "Transcript:\n")
	(setq gds-buffers
	      (cons (cons client (current-buffer))
		    gds-buffers))
	(current-buffer))
    ;; Otherwise there should be an existing buffer that we can
    ;; return.
    (let ((existing (assq client gds-buffers)))
      (if (buffer-live-p (cdr existing))
	  (cdr existing)
	(setq gds-buffers (delq existing gds-buffers))
	(gds-client-buffer client 'name '("(GDS buffer killed)"))))))

;; General dispatch function called by the subprocess filter.
(defun gds-handle-input (form)
  (dmessage "Form: %S" form)
  (let ((client (car form)))
    (or (eq client '*)
	(let* ((proc (cadr form))
	       (args (cddr form))
	       (buf (gds-client-buffer client proc args)))
	  (if buf (gds-handle-client-input buf client proc args))))))

(defun gds-handle-client-input (buf client proc args)
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-read-only t))
	(insert (format "<%S %S %S>" client proc args) "\n")))
    (dmessage "Buffer: %S" (current-buffer))
    (cond (;; (name ...) - Client name.
	   (eq proc 'name)
	   (setq gds-pid (cadr args))
	   (gds-request-focus client))

	  (;; (current-module ...) - Current module.
	   (eq proc 'current-module)
	   (setq gds-current-module (car args))
	   (dmessage "Current module: %S" gds-current-module))

	  (;; (stack ...) - Stack at an error or breakpoint.
	   (eq proc 'stack)
	   (setq gds-stack args))

	  (;; (modules ...) - Application's loaded modules.
	   (eq proc 'modules)
	   (while args
	     (or (assoc (car args) gds-modules)
		 (setq gds-modules (cons (list (car args)) gds-modules)))
	     (setq args (cdr args))))

	  (;; (output ...) - Last printed output.
	   (eq proc 'output)
	   (setq gds-output (car args)))

	  (;; (status ...) - Application status indication.
	   (eq proc 'status)
	   (setq gds-status (car args))
	   (or (eq gds-status 'waiting-for-input)
	       (setq gds-stack nil))
	   (gds-update-buffers client)
	   (if (eq gds-status 'waiting-for-input)
	       (gds-request-focus client)
	     (setq gds-stack nil)))

	  (;; (module MODULE ...) - The specified module's bindings.
	   (eq proc 'module)
	   (let ((minfo (assoc (car args) gds-modules)))
	     (if minfo
		 (setcdr (cdr minfo) (cdr args)))))

	  (;; (closed) - Client has gone away.
	   (eq proc 'closed)
	   (setq gds-status 'closed)
	   (gds-update-buffers client)
	   (setq gds-buffers
		 (delq (assq client gds-buffers) gds-buffers))
	   (if (eq client gds-focus-client)
	       (gds-quit)))

	  (;; (eval-results ...) - Results of evaluation.
	   (eq proc 'eval-results)
	   (gds-display-results client args))

	  ((eq proc 'completion-result)
	   (setq gds-completion-results (or (car args) t)))

	  )))


;;;; Guile Debugging keymap.

(set-keymap-parent gds-mode-map widget-keymap)
(define-key gds-mode-map "g" (function gds-go))
(define-key gds-mode-map "b" (function gds-set-breakpoint))
(define-key gds-mode-map "q" (function gds-quit))
(define-key gds-mode-map " " (function gds-next))
(define-key gds-mode-map "e" (function gds-evaluate))
(define-key gds-mode-map "i" (function gds-step-in))
(define-key gds-mode-map "o" (function gds-step-out))
(define-key gds-mode-map "t" (function gds-trace-finish))
(define-key gds-mode-map "I" (function gds-frame-info))
(define-key gds-mode-map "A" (function gds-frame-args))
(define-key gds-mode-map "M" (function gds-query-modules))

(defun gds-client-blocked ()
  (eq gds-status 'waiting-for-input))

(defun gds-go ()
  (interactive)
  (gds-send (format "(%S debugger-command continue)\n" gds-focus-client)))

(defun gds-next ()
  (interactive)
  (gds-send (format "(%S debugger-command next 1)\n" gds-focus-client)))

(defun gds-evaluate (expr)
  (interactive "sEvaluate (in this stack frame): ")
  (gds-send (format "(%S debugger-command evaluate %s)\n"
		    gds-focus-client
		    (prin1-to-string expr))))

(defun gds-step-in ()
  (interactive)
  (gds-send (format "(%S debugger-command step 1)\n" gds-focus-client)))

(defun gds-step-out ()
  (interactive)
  (gds-send (format "(%S debugger-command finish)\n" gds-focus-client)))

(defun gds-trace-finish ()
  (interactive)
  (gds-send (format "(%S debugger-command trace-finish)\n"
		    gds-focus-client)))

(defun gds-frame-info ()
  (interactive)
  (gds-send (format "(%S debugger-command info-frame)\n" gds-focus-client)))

(defun gds-frame-args ()
  (interactive)
  (gds-send (format "(%S debugger-command info-args)\n" gds-focus-client)))

(defun gds-set-breakpoint ()
  (interactive)
  (cond ((gds-in-source-buffer)
	 (gds-set-source-breakpoint))
	((gds-in-stack)
	 (gds-set-stack-breakpoint))
	((gds-in-modules)
	 (gds-set-module-breakpoint))
	(t
	 (error "No way to set a breakpoint from here"))))

(defun gds-in-source-buffer ()
  ;; Not yet worked out what will be available in Scheme source
  ;; buffers.
  nil)

(defun gds-in-stack ()
  (save-excursion
    (and (re-search-backward "^\\(Stack\\|Modules\\):" nil t)
	 (looking-at "Stack"))))

(defun gds-in-modules ()
  (save-excursion
    (and (re-search-backward "^\\(Stack\\|Modules\\):" nil t)
	 (looking-at "Modules"))))

(defun gds-set-module-breakpoint ()
  (let ((sym (save-excursion
	       (beginning-of-line)
	       (and (looking-at " > \\([^ \n\t]+\\)")
		    (match-string 1))))
	(module (save-excursion
		  (and (re-search-backward "^\\[[+---]\\] \\(([^)]+)\\)" nil t)
		       (match-string 1)))))
    (or sym
	(error "Couldn't find procedure name on current line"))
    (or module
	(error "Couldn't find module name for current line"))
    (let ((behaviour
	   (completing-read
	    (format "Behaviour for breakpoint at %s:%s (default debug-here): "
		    module sym)
	    '(("debug-here")
	      ("trace-here")
	      ("trace-subtree"))
	    nil
	    t
	    nil
	    nil
	    "debug-here")))
      (gds-send (format "(%S set-breakpoint %s %s %s)\n"
			gds-focus-client
			module
			sym
			behaviour)))))


;;;; Evaluating code.

;; The following commands send code for evaluation through the GDS TCP
;; connection, receive the result and any output generated through the
;; same connection, and display the result and output to the user.
;;
;; Where there are multiple Guile applications known to GDS, GDS by
;; default sends code to the one that holds the debugging focus,
;; i.e. `gds-focus-client'.  Where no application has the focus,
;; or the command is invoked with `C-u', GDS asks the user which
;; application is intended.

(defun gds-read-client ()
  (let* ((def (if gds-focus-client
		  (cdr (assq gds-focus-client gds-names))))
	 (prompt (if def
		     (concat "Application for eval (default "
			     def
			     "): ")
		   "Application for eval: "))
	 (name
	  (completing-read prompt
			   (mapcar (function list)
				   (mapcar (function cdr) gds-names))
			   nil t nil nil
			   def)))
    (let (client (names gds-names))
      (while (and names (not client))
	(if (string-equal (cdar names) name)
	    (setq client (caar names)))
	(setq names (cdr names)))
      client)))

(defun gds-choose-client (client)
  (or ;; If client is an integer, it is the port number of the
      ;; intended client.
      (if (integerp client) client)
      ;; Any other non-nil value indicates invocation with a prefix
      ;; arg, which forces asking the user which application is
      ;; intended.
      (if client (gds-read-client))
      ;; If ask not forced, and there is a client with the focus,
      ;; default to that one.
      gds-focus-client
      ;; If there are no clients at this point, and we are allowed to
      ;; autostart a captive Guile, do so.
      (and (null gds-buffers)
	   gds-autostart-captive
	   (progn
	     (gds-start-captive t)
	     (while (null gds-buffers)
	       (accept-process-output (get-buffer-process gds-captive)
				      0 100000))
	     (caar gds-buffers)))
      ;; If there is only one known client, use that one.
      (if (and (car gds-buffers)
	       (null (cdr gds-buffers)))
	  (caar gds-buffers))
      ;; Last resort - ask the user.
      (gds-read-client)
      ;; Signal an error.
      (error "No application chosen.")))

(defun gds-module-name (start end)
  "Determine and return the name of the module that governs the
specified region.  The module name is returned as a list of symbols."
  (interactive "r")			; why not?
  (save-excursion
    (goto-char start)
    (let (module-name)
      (while (and (not module-name)
		  (beginning-of-defun-raw 1))
	(if (looking-at "(define-module ")
	    (setq module-name
		  (progn
		    (goto-char (match-end 0))
		    (read (current-buffer))))))
      module-name)))

(defun gds-port-name (start end)
  "Return port name for the specified region of the current buffer.
The name will be used by Guile as the port name when evaluating that
region's code."
  (or (buffer-file-name)
      (concat "Emacs buffer: " (buffer-name))))

(defun gds-eval-region (start end &optional client)
  "Evaluate the current region."
  (interactive "r\nP")
  (setq client (gds-choose-client client))
  (let ((module (gds-module-name start end))
	(port-name (gds-port-name start end))
	line column)
    (save-excursion
      (goto-char start)
      (setq column (current-column))	; 0-based
      (beginning-of-line)
      (setq line (count-lines (point-min) (point)))) ; 0-based
    (gds-send (format "(%S eval %s %S %d %d %S)\n"
		      client
		      (if module (prin1-to-string module) "#f")
		      port-name line column
		      (buffer-substring-no-properties start end)))))

(defun gds-eval-expression (expr &optional client)
  "Evaluate the supplied EXPR (a string)."
  (interactive "sEvaluate expression: \nP")
  (setq client (gds-choose-client client))
  (gds-send (format "(%S eval #f \"Emacs expression\" 0 0 %S)\n"
		    client expr)))

(defun gds-eval-defun (&optional client)
  "Evaluate the defun (top-level form) at point."
  (interactive "P")
  (save-excursion
   (end-of-defun)
   (let ((end (point)))
     (beginning-of-defun)
     (gds-eval-region (point) end client))))

(defun gds-eval-last-sexp (&optional client)
  "Evaluate the sexp before point."
  (interactive "P")
  (gds-eval-region (save-excursion (backward-sexp) (point)) (point) client))


;;;; Help.

;; Help is implemented as a special case of evaluation, where we
;; arrange for the evaluation result to be a known symbol that is
;; unlikely to crop up otherwise.  When the evaluation result is this
;; symbol, we only display the output from the evaluation.

(defvar gds-help-symbol '%-gds-help-%
  "Symbol used by GDS to identify an evaluation response as help.")

(defun gds-help-symbol (sym &optional client)
  "Get help for SYM (a Scheme symbol)."
  (interactive
   (let ((sym (thing-at-point 'symbol))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (read-from-minibuffer
		(if sym
		    (format "Describe Guile symbol (default %s): " sym)
		  "Describe Guile symbol: ")))
     (list (if (zerop (length val)) sym val)
	   current-prefix-arg)))
  (gds-eval-expression (format "(begin (help %s) '%S)" sym gds-help-symbol)
		       client))

(defun gds-apropos (regex &optional client)
  "List Guile symbols matching REGEX."
  (interactive
   (let ((sym (thing-at-point 'symbol))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (read-from-minibuffer
		(if sym
		    (format "Guile apropos (regexp, default \"%s\"): " sym)
		  "Guile apropos (regexp): ")))
     (list (if (zerop (length val)) sym val)
	   current-prefix-arg)))
  (gds-eval-expression (format "(begin (apropos %S) '%S)" regex gds-help-symbol)
		       client))

(defvar gds-completion-results nil)

(defun gds-complete-symbol (&optional client)
  "Complete the Guile symbol before point.  Returns `t' if anything
interesting happened, `nil' if not."
  (interactive "P")
  (let* ((chars (- (point) (save-excursion
			     (while (let ((syntax (char-syntax (char-before (point)))))
				      (or (eq syntax ?w) (eq syntax ?_)))
			       (forward-char -1))
			     (point)))))
    (if (zerop chars)
	nil
      (setq client (gds-choose-client client))
      (setq gds-completion-results nil)
      (gds-send (format "(%S complete %s)\n" client
			(prin1-to-string
			 (buffer-substring-no-properties (- (point) chars)
							 (point)))))
      (while (null gds-completion-results)
	(accept-process-output gds-process 0 200))
      (cond ((eq gds-completion-results t)
	     nil)
	    ((stringp gds-completion-results)
	     (if (<= (length gds-completion-results) chars)
		 nil
	       (insert (substring gds-completion-results chars))
	       (message "Sole completion")
	       t))
	    ((= (length gds-completion-results) 1)
	     (if (<= (length (car gds-completion-results)) chars)
		 nil
	       (insert (substring (car gds-completion-results) chars))
	       t))
	    (t
	     (with-output-to-temp-buffer "*Completions*"
	       (display-completion-list gds-completion-results))
	     t)))))


;;;; Display of evaluation and help results.

(defun gds-display-results (client results)
  (let ((helpp (and (= (length results) 2)
		    (= (length (cadr results)) 1)
		    (string-equal (caadr results)
				  (prin1-to-string gds-help-symbol)))))
    (let ((buf (get-buffer-create (if helpp
				      "*Guile Help*"
				    "*Guile Results*"))))
      (save-excursion
	(set-buffer buf)
	(erase-buffer)
	(scheme-mode)
	(while results
	  (insert (car results))
	  (if helpp
	      nil
	    (mapcar (function (lambda (value)
				(insert " => " value "\n")))
		    (cadr results))
	    (insert "\n"))
	  (setq results (cddr results)))
	(goto-char (point-min))
	(if (and helpp (looking-at "Evaluating in "))
	    (delete-region (point) (progn (forward-line 1) (point)))))
      (pop-to-buffer buf)
      (run-hooks 'temp-buffer-show-hook)
      (other-window 1))))


;;;; Loading (evaluating) a whole Scheme file.

(defcustom gds-source-modes '(scheme-mode)
  "*Used to determine if a buffer contains Scheme source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a scheme source file by `gds-load-file'."
  :type '(repeat function)
  :group 'gds)

(defvar gds-prev-load-dir/file nil
  "Holds the last (directory . file) pair passed to `gds-load-file'.
Used for determining the default for the next `gds-load-file'.")

(defun gds-load-file (file-name &optional client)
  "Load a Scheme file into the inferior Scheme process."
  (interactive (list (car (comint-get-source "Load Scheme file: "
					     gds-prev-load-dir/file
					     gds-source-modes t))
					; T because LOAD needs an
					; exact name
		     current-prefix-arg))
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq gds-prev-load-dir/file (cons (file-name-directory    file-name)
				     (file-name-nondirectory file-name)))
  (setq client (gds-choose-client client))
  (gds-send (format "(%S load %S)\n" client file-name)))

;; Install the process communication commands in the scheme-mode keymap.
(define-key scheme-mode-map "\M-\C-x" 'gds-eval-defun);gnu convention
(define-key scheme-mode-map "\C-x\C-e" 'gds-eval-last-sexp);gnu convention
(define-key scheme-mode-map "\C-c\C-e" 'gds-eval-expression)
(define-key scheme-mode-map "\C-c\C-r" 'gds-eval-region)
(define-key scheme-mode-map "\C-c\C-l" 'gds-load-file)
(define-key scheme-mode-map "\C-hg" 'gds-help-symbol)
(define-key scheme-mode-map "\C-h\C-g" 'gds-apropos)
(define-key scheme-mode-map "\e\t" 'gds-complete-symbol)


;;;; Menu bar entries.

(defvar gds-debug-menu nil
  "GDS debugging menu.")
(if gds-debug-menu
    nil
  (setq gds-debug-menu (make-sparse-keymap "Debug"))
  (define-key gds-debug-menu [go]
    '(menu-item "Go" gds-go))
  (define-key gds-debug-menu [trace-finish]
    '(menu-item "Trace This Frame" gds-trace-finish))
  (define-key gds-debug-menu [step-out]
    '(menu-item "Finish This Frame" gds-step-out))
  (define-key gds-debug-menu [next]
    '(menu-item "Next" gds-next))
  (define-key gds-debug-menu [step-in]
    '(menu-item "Single Step" gds-step-in))
  (define-key gds-debug-menu [eval]
    '(menu-item "Eval In This Frame..." gds-evaluate)))

(defvar gds-eval-menu nil
  "GDS evaluation menu.")
(if gds-eval-menu
    nil
  (setq gds-eval-menu (make-sparse-keymap "Evaluate"))
  (define-key gds-eval-menu [load-file]
    '(menu-item "Load Scheme File" gds-load-file))
  (define-key gds-eval-menu [defun]
    '(menu-item "Defun At Point" gds-eval-defun))
  (define-key gds-eval-menu [region]
    '(menu-item "Region" gds-eval-region))
  (define-key gds-eval-menu [last-sexp]
    '(menu-item "Sexp Before Point" gds-eval-last-sexp))
  (define-key gds-eval-menu [expr]
    '(menu-item "Expression..." gds-eval-expression)))

(defvar gds-help-menu nil
  "GDS help menu.")
(if gds-help-menu
    nil
  (setq gds-help-menu (make-sparse-keymap "Help"))
  (define-key gds-help-menu [apropos]
    '(menu-item "Apropos..." gds-apropos))
  (define-key gds-help-menu [sym]
    '(menu-item "Symbol..." gds-help-symbol)))

(defvar gds-advanced-menu nil
  "Menu of rarely needed GDS operations.")
(if gds-advanced-menu
    nil
  (setq gds-advanced-menu (make-sparse-keymap "Advanced"))
  (define-key gds-advanced-menu [run-captive]
    '(menu-item "Run Captive Guile" gds-start-captive
		:enable (not (comint-check-proc gds-captive))))
  (define-key gds-advanced-menu [restart-gds]
    '(menu-item "Restart IDE" gds-start :enable gds-process))
  (define-key gds-advanced-menu [kill-gds]
    '(menu-item "Shutdown IDE" gds-shutdown :enable gds-process))
  (define-key gds-advanced-menu [start-gds]
    '(menu-item "Start IDE" gds-start :enable (not gds-process))))

(defvar gds-menu nil
  "Global menu for GDS commands.")
(if gds-menu
    nil
  (setq gds-menu (make-sparse-keymap "Guile"))
  (define-key gds-menu [advanced]
    (cons "Advanced" gds-advanced-menu))
  (define-key gds-menu [separator-1]
    '("--"))
  (define-key gds-menu [debug]
    `(menu-item "Debug" ,gds-debug-menu :enable (and gds-focus-client
						     (gds-client-blocked))))
  (define-key gds-menu [eval]
    `(menu-item "Evaluate" ,gds-eval-menu :enable (or gds-buffers
						      gds-autostart-captive)))
  (define-key gds-menu [help]
    `(menu-item "Help" ,gds-help-menu :enable (or gds-buffers
						  gds-autostart-captive)))
  (setq menu-bar-final-items
	(cons 'guile menu-bar-final-items))
  (define-key scheme-mode-map [menu-bar guile]
    (cons "Guile" gds-menu)))


;;;; Autostarting the GDS server.

(defcustom gds-autostart-server t
  "Whether to automatically start the GDS server when `gds.el' is loaded."
  :type 'boolean
  :group 'gds)

(if (and gds-autostart-server
	 (not gds-process))
    (gds-start))


;;;; `Captive' Guile - a Guile process that is started when needed to
;;;; provide help, completion, evaluations etc.

(defcustom gds-autostart-captive t
  "Whether to automatically start a `captive' Guile process when needed."
  :type 'boolean
  :group 'gds)

(defvar gds-captive nil
  "Buffer of captive Guile.")

(defun gds-start-captive (&optional restart)
  (interactive)
  (if (and restart
	   (comint-check-proc gds-captive))
      (gds-kill-captive))
  (if (comint-check-proc gds-captive)
      nil
    (let ((process-connection-type nil))
      (setq gds-captive (make-comint "captive-guile"
				     "guile"
				     nil
				     "-q")))
    (let ((proc (get-buffer-process gds-captive)))
      (comint-send-string proc "(set! %load-path (cons \"/home/neil/Guile/cvs/guile-core\" %load-path))\n")
      (comint-send-string proc "(debug-enable 'backtrace)\n")
      (comint-send-string proc "(use-modules (emacs gds-client))\n")
      (comint-send-string proc "(gds-connect \"Captive Guile\" #f)\n"))))

(defun gds-kill-captive ()
  (if gds-captive
      (let ((proc (get-buffer-process gds-captive)))
	(process-kill-without-query proc)
	(condition-case nil
	    (progn
	      (kill-process proc)
	      (accept-process-output gds-process 0 200))
	  (error)))))


;;;; The end!

(provide 'gds)

;;; gds.el ends here.
