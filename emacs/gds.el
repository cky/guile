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
(require 'cl)
(require 'comint)
(require 'info)


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

;; The guile executable used by the GDS server and captive client
;; processes.
(defcustom gds-guile-program "guile"
  "*The guile executable used by GDS, specifically by its server and
captive client processes."
  :type 'string
  :group 'gds)

(defun gds-start ()
  "Start (or restart, if already running) the GDS subprocess."
  (interactive)
  (gds-kill-captive)
  (if gds-process (gds-shutdown))
  (with-current-buffer (get-buffer-create "*GDS Process*")
    (erase-buffer)
    (setq gds-process
	  (let ((process-connection-type nil)) ; use a pipe
	    (start-process "gds"
			   (current-buffer)
			   gds-guile-program
			   "-q"
			   "--debug"
			   "-c"
			   "(begin (use-modules (emacs gds-server)) (run-server))"))))
  (setq gds-read-cursor (point-min))
  (set-process-filter gds-process (function gds-filter))
  (set-process-sentinel gds-process (function gds-sentinel))
  (set-process-coding-system gds-process 'latin-1-unix)
  (process-kill-without-query gds-process))

;; Shutdown the subprocess and cleanup all associated data.
(defun gds-shutdown ()
  "Shut down the GDS subprocess."
  (interactive)
  ;; Reset variables.
  (setq gds-buffers nil)
  ;; Kill the subprocess.
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
(defun gds-send (string client)
  (process-send-string gds-process (format "(%S %s)\n" client string))
  (let ((buf (gds-client-ref 'gds-transcript)))
    (if buf
	(with-current-buffer buf
	  (goto-char (point-max))
	  (let ((inhibit-read-only t))
	    (insert (format "tx (%S %s)\n" client string)))))))


;;;; Focussing in and out on interaction with a particular client.

;;;; The slight possible problems here are that popping up a client's
;;;; interaction windows when that client wants attention might
;;;; interrupt something else that the Emacs user was working on at
;;;; the time, and that if multiple clients are being debugged at the
;;;; same time, their popping up of interaction windows might become
;;;; confusing.  For this reason, we allow GDS's behavior to be
;;;; customized via the variables `gds-focus-in-function' and
;;;; `gds-focus-out-function'.
;;;;
;;;; That said, the default policy, which is probably OK for most
;;;; users most of the time, is very simple: when a client wants
;;;; attention, its interaction windows are popped up immediately.

(defun gds-request-focus (client)
  (funcall gds-focus-in-function client))

(defcustom gds-focus-in-function (function gds-focus-in)
  "Function to call when a GDS client program wants user attention.
The function is called with one argument, the CLIENT in question."
  :type 'function
  :group 'gds)

(defun gds-focus-in (client)
  (gds-display-buffers client))

(defun gds-quit ()
  (interactive)
  (funcall gds-focus-out-function))

(defcustom gds-focus-out-function (function gds-focus-out)
  "Function to call when user quits interacting with a GDS client."
  :type 'function
  :group 'gds)

(defun gds-focus-out ()
  (if (if (gds-client-blocked)
	  (y-or-n-p "Client is waiting for input.  Quit anyway? ")
	t)
      (bury-buffer (current-buffer))))


;;;; Multiple client focus -- an alternative implementation.

;;;; The following code is provided as an alternative example of how a
;;;; customized GDS could schedule the display of multiple clients
;;;; that are competing for user attention.

;; - `gds-waiting' holds a list of clients that want attention but
;;   haven't yet got it.  A client is added to this list for two
;;   reasons.  (1) When it is blocked waiting for user input.
;;   (2) When it first connects to GDS, even if not blocked.
;;
;; - `gds-focus-client' holds the client, if any, that currently has
;;   the user's attention.  A client can be given the focus if
;;   `gds-focus-client' is nil at the time that the client wants
;;   attention, or if another client relinquishes it.  A client can
;;   relinquish the focus in two ways.  (1) If the client application
;;   says that it is no longer blocked, and a small time passes without
;;   it becoming blocked again.  (2) If the user explicitly `quits'
;;   that client.
;;
;; (defvar gds-focus-client nil)
;; (defvar gds-waiting nil)
;; 
;; (defun gds-focus-in-alternative (client)
;;   (cond ((eq client gds-focus-client)
;;	      ;; CLIENT already has the focus.  Display its buffer.
;;	      (gds-display-buffers client))
;;	     (gds-focus-client
;;	      ;; Another client has the focus. Add CLIENT to `gds-waiting'.
;;	      (or (memq client gds-waiting)
;;		  (setq gds-waiting (append gds-waiting (list client)))))
;;	     (t
;;	      ;; Give focus to CLIENT and display its buffer.
;;	      (setq gds-focus-client client)
;;	      (gds-display-buffers client))))
;; 
;; (defun gds-focus-out-alternative ()
;;   (if (or (car gds-waiting)
;;	       (not (gds-client-blocked))
;;	       (y-or-n-p
;;		"Client is blocked and no others are waiting.  Still quit? "))
;;	   (progn
;;	     (bury-buffer (current-buffer))
;;	     ;; Pass on the focus.
;;	     (setq gds-focus-client (car gds-waiting)
;;		   gds-waiting (cdr gds-waiting))
;;	     ;; If this client is blocked, add it back into the waiting list.
;;	     (if (gds-client-blocked)
;;		 (gds-request-focus gds-client))
;;	     ;; If there is a new focus client, request display for it.
;;	     (if gds-focus-client
;;		 (gds-request-focus gds-focus-client)))))


;;;; GDS protocol dispatch.

;; General dispatch function called by the subprocess filter.
(defun gds-handle-input (form)
  (let ((client (car form)))
    (or (eq client '*)
	(let* ((proc (cadr form))
	       (args (cddr form))
	       (buf (gds-client-buffer client proc args)))
	  (if buf (gds-handle-client-input buf client proc args))))))

(defun gds-handle-client-input (buf client proc args)
  (with-current-buffer buf
    (with-current-buffer gds-transcript
      (goto-char (point-max))
      (let ((inhibit-read-only t))
	(insert (format "rx %S" (cons client (cons proc args))) "\n")))

    (cond (;; (name ...) - Client name.
	   (eq proc 'name)
	   (setq gds-pid (cadr args))
	   (gds-promote-view 'interaction)
	   (gds-request-focus client))

	  (;; (current-module ...) - Current module.
	   (eq proc 'current-module)
	   (setq gds-current-module (car args)))

	  (;; (stack ...) - Stack at an error or breakpoint.
	   (eq proc 'stack)
	   (setq gds-stack args)
	   (gds-promote-view 'stack))

	  (;; (modules ...) - Application's loaded modules.
	   (eq proc 'modules)
	   (while args
	     (or (assoc (car args) gds-modules)
		 (setq gds-modules (cons (list (car args)) gds-modules)))
	     (setq args (cdr args))))

	  (;; (output ...) - Last printed output.
	   (eq proc 'output)
	   (setq gds-output (car args))
	   (gds-add-view 'messages))

	  (;; (status ...) - Application status indication.
	   (eq proc 'status)
	   (setq gds-status (car args))
	   (if (eq gds-status 'running)
	       (gds-delete-view 'browser)
	     (gds-add-view 'browser))
	   (if (eq gds-status 'waiting-for-input)
	       (progn
		 (gds-promote-view 'stack)
		 (gds-update-buffers)
		 (gds-request-focus client))
	     (setq gds-stack nil)
	     (gds-delete-view 'stack)
	     (gds-update-buffers-in-a-while)))

	  (;; (module MODULE ...) - The specified module's bindings.
	   (eq proc 'module)
	   (let ((minfo (assoc (car args) gds-modules)))
	     (if minfo
		 (setcdr (cdr minfo) (cdr args)))))

	  (;; (closed) - Client has gone away.
	   (eq proc 'closed)
	   (setq gds-status 'closed)
	   (gds-update-buffers)
	   (setq gds-buffers
		 (delq (assq client gds-buffers) gds-buffers)))

	  (;; (eval-results ...) - Results of evaluation.
	   (eq proc 'eval-results)
	   (gds-display-results client (car args) (cdr args)))

	  (;; (completion-result ...) - Available completions.
	   (eq proc 'completion-result)
	   (setq gds-completion-results (or (car args) t)))

	  (;; (breakpoint-set FILE LINE COLUMN INFO) - Breakpoint set.
	   (eq proc 'breakpoint-set)
	   (let ((file (nth 0 args))
		 (line (nth 1 args))
		 (column (nth 2 args))
		 (info (nth 3 args)))
	     (with-current-buffer (find-file-noselect file)
	       (save-excursion
		 (goto-char (point-min))
		 (or (zerop line)
		     (forward-line line))
		 (move-to-column column)
		 (let ((os (overlays-at (point))) o)
		   (while os
		     (if (and (overlay-get (car os) 'gds-breakpoint-info)
			      (= (overlay-start (car os)) (point)))
			 (progn
			   (overlay-put (car os)
					'gds-breakpoint-info
					info)
			   (overlay-put (car os)
					'before-string
					gds-active-breakpoint-before-string)
			   (overlay-put (car os)
					'after-string
					gds-active-breakpoint-after-string)
			   (setq os nil))
		       (setq os (cdr os)))))))))

	  (;; (thread-status THREAD-TYPE THREAD-NUMBER STATUS [CORRELATOR])
	   (eq proc 'thread-status)
	   (if (eq (car args) 'eval)
	       (let ((number (nth 1 args))
		     (status (nth 2 args))
		     (correlator (nth 3 args)))
		 (if (eq status 'busy)
		     (progn
		       (setq gds-evals-in-progress
			     (append gds-evals-in-progress
				     (list (cons number correlator))))
		       (run-at-time 0.5 nil
				    (function gds-display-slow-eval)
				    buf number correlator)
		       (gds-promote-view 'interaction))
		   (let ((existing (assq number gds-evals-in-progress)))
		     (if existing
			 (setq gds-evals-in-progress
			       (delq existing gds-evals-in-progress)))))
		 (gds-update-buffers))))

	  )))

(defun gds-display-slow-eval (buf number correlator)
  (with-current-buffer buf
    (let ((entry (assq number gds-evals-in-progress)))
      (if (and entry
	       (eq (cdr entry) correlator))
	  (progn
	    (gds-promote-view 'interaction)
	    (gds-request-focus gds-client))))))


;;;; Per-client buffer state.

;; This section contains code that is specific to each Guile client's
;; buffer but independent of any particular `view'.

;; Alist mapping each client port number to corresponding buffer.
(defvar gds-buffers nil)

(define-derived-mode gds-mode
  scheme-mode
  "Guile Interaction"
  "Major mode for interacting with a Guile client application."
  (widget-minor-mode 1))

(defvar gds-client nil
  "GDS client's port number.")
(make-variable-buffer-local 'gds-client)

(defvar gds-status nil
  "GDS client's latest status, one of the following symbols.
`running' - Application is running.
`waiting-for-input' - Application is blocked waiting for instruction
                      from the frontend.
`ready-for-input' - Application is not blocked but can also accept
                    asynchronous instructions from the frontend.")
(make-variable-buffer-local 'gds-status)

(defvar gds-transcript nil
  "Transcript buffer for this GDS client.")
(make-variable-buffer-local 'gds-transcript)

;; Return client buffer for specified client and protocol input.
(defun gds-client-buffer (client proc args)
  (if (eq proc 'name)
      ;; Introduction from client - create a new buffer.
      (with-current-buffer (generate-new-buffer (car args))
	(gds-mode)
	(setq gds-client client)
	(setq gds-transcript
	      (find-file-noselect
	       (expand-file-name (concat "~/.gds-transcript-" (car args)))))
	(with-current-buffer gds-transcript
	  (goto-char (point-max))
	  (insert "\nTranscript:\n"))
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

;; Get the current buffer's associated client's value of SYM.
(defun gds-client-ref (sym &optional client)
  (and (or client gds-client)
       (let ((buf (assq (or client gds-client) gds-buffers)))
	 (and buf
	      (cdr buf)
	      (buffer-live-p (cdr buf))
	      (with-current-buffer (cdr buf)
		(symbol-value sym))))))

(defun gds-client-blocked ()
  (eq (gds-client-ref 'gds-status) 'waiting-for-input))

(defvar gds-delayed-update-timer nil)

(defvar gds-delayed-update-buffers nil)

(defun gds-update-delayed-update-buffers ()
  (while gds-delayed-update-buffers
    (with-current-buffer (car gds-delayed-update-buffers)
      (setq gds-delayed-update-buffers
	    (cdr gds-delayed-update-buffers))
      (gds-update-buffers))))
      
(defun gds-update-buffers ()
  (if (timerp gds-delayed-update-timer)
      (cancel-timer gds-delayed-update-timer))
  (setq gds-delayed-update-timer nil)
  (let ((view (car gds-views))
	(inhibit-read-only t))
    (cond ((eq view 'stack)
	   (gds-insert-stack))
	  ((eq view 'interaction)
	   (gds-insert-interaction))
	  ((eq view 'browser)
	   (gds-insert-modules))
	  ((eq view 'messages)
	   (gds-insert-messages))
	  (t
	   (error "Bad GDS view %S" view)))
    ;; Finish off.
    (force-mode-line-update t)))

(defun gds-update-buffers-in-a-while ()
  (or (memq (current-buffer) gds-delayed-update-buffers)
      (setq gds-delayed-update-buffers
	    (cons (current-buffer) gds-delayed-update-buffers)))
  (if (timerp gds-delayed-update-timer)
      nil
    (setq gds-delayed-update-timer
	  (run-at-time 0.5 nil (function gds-update-delayed-update-buffers)))))

(defun gds-display-buffers (client)
  (let ((buf (cdr (assq client gds-buffers))))
    ;; If there's already a window showing the buffer, use it.
    (let ((window (get-buffer-window buf t)))
      (if window
	  (make-frame-visible (window-frame window))
	(display-buffer buf)))
    ;; If there is an associated source buffer, display it as well.
    (if (and (eq (car gds-views) 'stack)
	     gds-frame-source-overlay
	     (> (overlay-end gds-frame-source-overlay) 1))
	(let ((window (display-buffer
		       (overlay-buffer gds-frame-source-overlay))))
	  (set-window-point window
			    (overlay-start gds-frame-source-overlay))))))


;;;; Management of `views'.

;; The idea here is to keep the buffer describing a Guile client
;; relatively uncluttered by only showing one kind of information
;; about that client at a time.  Menu items and key sequences are
;; provided to switch easily between the available views.

(defvar gds-views nil
  "List of available views for a GDS client.  Each element is one of
the following symbols.
`interaction' - Interaction with running client.
`stack' - Call stack view.
`browser' - Modules and bindings browser view.
`breakpoints' - List of set breakpoints.
`messages' - Non-GDS-protocol output from the debugger.")
(make-variable-buffer-local 'gds-views)

(defun gds-promote-view (view)
  (setq gds-views (cons view (delq view gds-views))))

(defun gds-switch-to-view (view)
  (or (memq view gds-views)
      (error "View %S is not available" view))
  (gds-promote-view view)
  (gds-update-buffers))

(defun gds-add-view (view)
  (or (memq view gds-views)
      (setq gds-views (append gds-views (list view)))))

(defun gds-delete-view (view)
  (setq gds-views (delq view gds-views)))


;;;; `Interaction' view.

;; This view provides interaction with a normally running Guile
;; client, in other words one that is not stopped in the debugger but
;; is still available to take input from GDS (usually via a thread for
;; that purpose).  The view supports evaluation, help requests,
;; control of `debug-on-exception' function, and methods for breaking
;; into the running code.

(defvar gds-current-module "()"
  "GDS client's current module.")
(make-variable-buffer-local 'gds-current-module)

(defvar gds-pid nil
  "GDS client's process ID.")
(make-variable-buffer-local 'gds-pid)

(defvar gds-debug-exceptions nil
  "Whether to debug exceptions.")
(make-variable-buffer-local 'gds-debug-exceptions)

(defvar gds-exception-keys "signal misc-error"
  "The exception keys for which to debug a GDS client.")
(make-variable-buffer-local 'gds-exception-keys)

(defvar gds-evals-in-progress nil
  "Alist describing evaluations in progress.")
(make-variable-buffer-local 'gds-evals-in-progress)

(defvar gds-results nil
  "Last help or evaluation results.")
(make-variable-buffer-local 'gds-results)

(defcustom gds-heading-face 'info-menu-header
  "*Face used for headings in Guile Interaction buffers."
  :type 'face
  :group 'gds)

(defun gds-insert-interaction ()
  (erase-buffer)
  ;; Insert stuff for interacting with a running (non-blocked) Guile
  ;; client.
  (gds-heading-insert (buffer-name))
  (widget-insert " "
		 (cdr (assq gds-status
			    '((running . "running (cannot accept input)")
			      (waiting-for-input . "waiting for input")
			      (ready-for-input . "running")
			      (closed . "closed"))))
		 ", in "
		 gds-current-module
		 "\n\n")
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
  ;; Evaluation report area.
  (widget-insert "\n")
  (gds-heading-insert "Recent Evaluations")
  (widget-insert "  To run an evaluation, see the Guile->Evaluate menu.\n")
  (if gds-results
      (widget-insert "\n" (cdr gds-results)))
  (let ((evals gds-evals-in-progress))
    (while evals
      (widget-insert "\n" (cddar evals) " - running ")
      (let ((w (widget-create 'push-button
			      :notify (function gds-interrupt-eval)
			      "Interrupt")))
	(widget-put w :thread-number (caar evals)))
      (widget-insert "\n")
      (setq evals (cdr evals)))))

(defun gds-heading-insert (text)
  (let ((start (point)))
    (widget-insert text)
    (let ((o (make-overlay start (point))))
      (overlay-put o 'face gds-heading-face)
      (overlay-put o 'evaporate t))))

(defun gds-sigint (w &rest ignore)
  (interactive)
  (signal-process gds-pid 2))

(defun gds-async-break (w &rest ignore)
  (interactive)
  (gds-send "async-break" gds-client))

(defun gds-interrupt-eval (w &rest ignore)
  (interactive)
  (gds-send (format "interrupt-eval %S" (widget-get w :thread-number))
	    gds-client))

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

(defun gds-view-interaction ()
  (interactive)
  (gds-switch-to-view 'interaction))


;;;; `Stack' view.

;; This view shows the Guile call stack after the application has hit
;; an error, or when it is stopped in the debugger.

(defvar gds-stack nil
  "GDS client's stack when last stopped.")
(make-variable-buffer-local 'gds-stack)

(defun gds-insert-stack ()
  (erase-buffer)
  (let ((frames (car gds-stack))
	(index (cadr gds-stack))
	(flags (caddr gds-stack))
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
    (widget-insert "\n")
    (goto-char (point-min))))

(defun gds-select-stack-frame (widget &rest ignored)
  (let* ((s (widget-value widget))
	 (ind (memq 'index (text-properties-at 0 s))))
    (gds-send (format "debugger-command frame %d" (cadr ind))
	      gds-client)))

;; Overlay used to highlight the source expression corresponding to
;; the selected frame.
(defvar gds-frame-source-overlay nil)

(defun gds-show-selected-frame (source)
  ;; Highlight the frame source, if possible.
  (if (and source
	   (file-readable-p (car source)))
      (with-current-buffer (find-file-noselect (car source))
	(if gds-frame-source-overlay
	    nil
	  (setq gds-frame-source-overlay (make-overlay 0 0))
	  (overlay-put gds-frame-source-overlay 'face 'highlight))
	;; Move to source line.  Note that Guile line numbering is
	;; 0-based, while Emacs numbering is 1-based.
	(save-restriction
	  (widen)
	  (goto-line (+ (cadr source) 1))
	  (move-to-column (caddr source))
	  (move-overlay gds-frame-source-overlay
			(point)
			(if (not (looking-at ")"))
			    (save-excursion (forward-sexp 1) (point))
			  ;; It seems that the source coordinates for
			  ;; backquoted expressions are at the end of
			  ;; the sexp rather than the beginning...
			  (save-excursion (forward-char 1)
					  (backward-sexp 1) (point)))
			(current-buffer))))
    (if gds-frame-source-overlay
	(move-overlay gds-frame-source-overlay 0 0))))

(defun gds-view-stack ()
  (interactive)
  (gds-switch-to-view 'stack))


;;;; `Breakpoints' view.

;; This view shows a list of breakpoints.

(defun gds-view-breakpoints ()
  (interactive)
  (gds-switch-to-view 'breakpoints))


;;;; `Browser' view.

;; This view shows a list of modules and module bindings.

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

(defun gds-insert-modules ()
  (let ((p (if (eq (window-buffer (selected-window)) (current-buffer))
	       (point)
	     (point-min)))
	(modules gds-modules))
    (erase-buffer)
    (insert "Modules:\n")
    (while modules
      (let ((minfo (car modules)))
	(if (gds-show-module-p (car minfo))
	    (let ((w (widget-create 'push-button
				    :notify (function gds-module-notify)
				    (if (and (cdr minfo)
					     (cadr minfo))
					"-" "+"))))
	      (widget-put w :module (cons gds-client (car minfo)))
	      (widget-insert " " (prin1-to-string (car minfo)) "\n")
	      (if (cadr minfo)
		  (let ((syms (cddr minfo)))
		    (while syms
		      (widget-insert " > " (car syms) "\n")
		      (setq syms (cdr syms))))))))
      (setq modules (cdr modules)))
    (insert "\n")
    (goto-char p)))

(defun gds-module-notify (w &rest ignore)
  (let* ((module (widget-get w :module))
	 (client (car module))
	 (name (cdr module))
	 (minfo (assoc name gds-modules)))
    (if (cdr minfo)
	;; Just toggle expansion state.
	(progn
	  (setcar (cdr minfo) (not (cadr minfo)))
	  (gds-update-buffers))
      ;; Set flag to indicate module expanded.
      (setcdr minfo (list t))
      ;; Get symlist from Guile.
      (gds-send (format "query-module %S" name) client))))

(defun gds-query-modules ()
  (interactive)
  (gds-send "query-modules" gds-client))

(defun gds-view-browser ()
  (interactive)
  (or gds-modules (gds-query-modules))
  (gds-switch-to-view 'browser))


;;;; `Messages' view.

;; This view shows recent non-GDS-protocol messages output from the
;; (ice-9 debugger) code.

(defvar gds-output nil
  "GDS client's recent output (printed).")
(make-variable-buffer-local 'gds-output)

(defun gds-insert-messages ()
  (erase-buffer)
  ;; Insert recent non-protocol output from (ice-9 debugger).
  (insert gds-output)
  (goto-char (point-min)))

(defun gds-view-messages ()
  (interactive)
  (gds-switch-to-view 'messages))


;;;; Debugger commands.

;; Typically but not necessarily used from the `stack' view.

(defun gds-go ()
  (interactive)
  (gds-send "debugger-command continue" gds-client))

(defun gds-next ()
  (interactive)
  (gds-send "debugger-command next 1" gds-client))

(defun gds-evaluate (expr)
  (interactive "sEvaluate (in this stack frame): ")
  (gds-send (format "debugger-command evaluate %s" (prin1-to-string expr))
	    gds-client))

(defun gds-step-in ()
  (interactive)
  (gds-send "debugger-command step 1" gds-client))

(defun gds-step-out ()
  (interactive)
  (gds-send "debugger-command finish" gds-client))

(defun gds-trace-finish ()
  (interactive)
  (gds-send "debugger-command trace-finish" gds-client))

(defun gds-frame-info ()
  (interactive)
  (gds-send "debugger-command info-frame" gds-client))

(defun gds-frame-args ()
  (interactive)
  (gds-send "debugger-command info-args" gds-client))

(defun gds-debug-trap-hooks ()
  (interactive)
  (gds-send "debugger-command debug-trap-hooks" gds-client))

(defun gds-up ()
  (interactive)
  (gds-send "debugger-command up 1" gds-client))

(defun gds-down ()
  (interactive)
  (gds-send "debugger-command down 1" gds-client))


;;;; Setting breakpoints.

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
      (gds-send (format "set-breakpoint %s %s %s"
			module
			sym
			behaviour)
		gds-client))))


;;;; Scheme source breakpoints.

(defcustom gds-breakpoint-face 'default
  "*Face used to highlight the location of a source breakpoint.
Specifically, this face highlights the opening parenthesis of the
form where the breakpoint is set."
  :type 'face
  :group 'gds)

(defcustom gds-new-breakpoint-before-string ""
  "*String used to show the presence of a new source breakpoint.
`New' means that the breakpoint has been set but isn't yet known to
Guile because the containing code hasn't been reevaluated yet.
This string appears before the opening parenthesis of the form where
the breakpoint is set.  If you prefer a marker to appear after the
opening parenthesis, make this string empty and use
`gds-new-breakpoint-after-string'."
  :type 'string 
  :group 'gds)

(defcustom gds-new-breakpoint-after-string "=?= "
  "*String used to show the presence of a new source breakpoint.
`New' means that the breakpoint has been set but isn't yet known to
Guile because the containing code hasn't been reevaluated yet.
This string appears after the opening parenthesis of the form where
the breakpoint is set.  If you prefer a marker to appear before the
opening parenthesis, make this string empty and use
`gds-new-breakpoint-before-string'."
  :type 'string 
  :group 'gds)

(defcustom gds-active-breakpoint-before-string ""
  "*String used to show the presence of a source breakpoint.
`Active' means that the breakpoint is known to Guile.
This string appears before the opening parenthesis of the form where
the breakpoint is set.  If you prefer a marker to appear after the
opening parenthesis, make this string empty and use
`gds-active-breakpoint-after-string'."
  :type 'string 
  :group 'gds)

(defcustom gds-active-breakpoint-after-string "=|= "
  "*String used to show the presence of a source breakpoint.
`Active' means that the breakpoint is known to Guile.
This string appears after the opening parenthesis of the form where
the breakpoint is set.  If you prefer a marker to appear before the
opening parenthesis, make this string empty and use
`gds-active-breakpoint-before-string'."
  :type 'string 
  :group 'gds)

(defun gds-source-breakpoint-pos ()
  "Return the position of the starting parenthesis of the innermost
Scheme pair around point."
  (if (eq (char-syntax (char-after)) ?\()
      (point)
    (save-excursion
      (condition-case nil
	  (while t (forward-sexp -1))
	(error))
      (forward-char -1)
      (while (not (eq (char-syntax (char-after)) ?\())
	(forward-char -1))
      (point))))

(defun gds-source-breakpoint-overlay-at (pos)
  "Return the source breakpoint overlay at POS, if any."
  (let* (o (os (overlays-at pos)))
    (while os
      (if (and (overlay-get (car os) 'gds-breakpoint-info)
	       (= (overlay-start (car os)) pos))
	  (setq o (car os)
		os nil))
      (setq os (cdr os)))
    o))

(defun gds-set-source-breakpoint ()
  (interactive)
  (let* ((pos (gds-source-breakpoint-pos))
	 (o (gds-source-breakpoint-overlay-at pos)))
    (if o
	(error "There is already a breakpoint here!")
      (setq o (make-overlay pos (+ pos 1)))
      (overlay-put o 'evaporate t)
      (overlay-put o 'face gds-breakpoint-face)
      (overlay-put o 'gds-breakpoint-info 0)
      (overlay-put o 'before-string gds-new-breakpoint-before-string)
      (overlay-put o 'after-string gds-new-breakpoint-after-string))))

(defun gds-delete-source-breakpoint ()
  (interactive)
  (let* ((pos (gds-source-breakpoint-pos))
	 (o (gds-source-breakpoint-overlay-at pos)))
    (or o
	(error "There is no breakpoint here to delete!"))
    (delete-overlay o)))

(defun gds-region-breakpoint-info (beg end)
  "Return an alist of breakpoints in REGION.
The car of each alist element is a cons (LINE . COLUMN) giving the
source location of the breakpoint.  The cdr is information describing
breakpoint properties.  Currently `information' is just the breakpoint
index, for an existing Guile breakpoint, or 0 for a breakpoint that
isn't yet known to Guile."
  (interactive "r")
  (let ((os (overlays-in beg end))
	info o)
    (while os
      (setq o (car os)
	    os (cdr os))
      (if (overlay-get o 'gds-breakpoint-info)
	  (progn
	    (setq info
		  (cons (cons (save-excursion
				(goto-char (overlay-start o))
				(cons (save-excursion
					(beginning-of-line)
					(count-lines (point-min) (point)))
				      (current-column)))
			      (overlay-get o 'gds-breakpoint-info))
			info))
	    ;; Also now mark the breakpoint as `new'.  It will become
	    ;; `active' (again) when we receive a notification from
	    ;; Guile that the breakpoint has been set.
	    (overlay-put o 'gds-breakpoint-info 0)
	    (overlay-put o 'before-string gds-new-breakpoint-before-string)
	    (overlay-put o 'after-string gds-new-breakpoint-after-string))))
    (nreverse info)))


;;;; Evaluating code.

;; The following commands send code for evaluation through the GDS TCP
;; connection, receive the result and any output generated through the
;; same connection, and display the result and output to the user.
;;
;; For each buffer where evaluations can be requested, GDS uses the
;; buffer-local variable `gds-client' to track which GDS client
;; program should receive and handle that buffer's evaluations.  In
;; the common case where GDS is only managing one client program, a
;; buffer's value of `gds-client' is set automatically to point to
;; that program the first time that an evaluation (or help or
;; completion) is requested.  If there are multiple GDS clients
;; running at that time, GDS asks the user which one is intended.

(defun gds-read-client ()
  (let* ((def (and gds-client (cdr (assq gds-client gds-names))))
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
  ;; Only keep the supplied client number if it is still valid.
  (if (integerp client)
      (setq client (gds-client-ref 'gds-client client)))
  ;; Only keep the current buffer's setting of `gds-client' if it is
  ;; still valid.
  (if gds-client
      (setq gds-client (gds-client-ref 'gds-client)))
  
  (or ;; If client is an integer, it is the port number of the
      ;; intended client.
      (if (integerp client)
	  client)
      ;; Any other non-nil value indicates invocation with a prefix
      ;; arg, which forces asking the user which application is
      ;; intended.
      (if client
	  (setq gds-client (gds-read-client)))
      ;; If ask not forced, and current buffer is associated with a
      ;; client, use that client.
      gds-client
      ;; If there are no clients at this point, and we are
      ;; allowed to autostart a captive Guile, do so.
      (and (null gds-buffers)
	   gds-autostart-captive
	   (progn
	     (gds-start-captive t)
	     (while (null gds-buffers)
	       (accept-process-output (get-buffer-process gds-captive)
				      0 100000))
	     (setq gds-client (caar gds-buffers))))
      ;; If there is only one known client, use that one.
      (if (and (car gds-buffers)
	       (null (cdr gds-buffers)))
	  (setq gds-client (caar gds-buffers)))
      ;; Last resort - ask the user.
      (setq gds-client (gds-read-client))
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
    (let ((code (buffer-substring-no-properties start end)))
      (gds-send (format "eval (region . %S) %s %S %d %d %s %S"
			(gds-abbreviated code)
			(if module (prin1-to-string module) "#f")
			port-name line column
			(let ((bpinfo (gds-region-breakpoint-info start end)))
			  ;; Make sure that "no bpinfo" is represented
			  ;; as "()", not "nil", as Scheme doesn't
			  ;; understand "nil".
			  (if bpinfo (format "%S" bpinfo) "()"))
			code)
		client))))

(defun gds-eval-expression (expr &optional client correlator)
  "Evaluate the supplied EXPR (a string)."
  (interactive "sEvaluate expression: \nP")
  (setq client (gds-choose-client client))
  (set-text-properties 0 (length expr) nil expr)
  (gds-send (format "eval (%S . %S) #f \"Emacs expression\" 0 0 () %S"
		    (or correlator 'expression)
		    (gds-abbreviated expr)
		    expr)
	    client))

(defconst gds-abbreviated-length 35)

(defun gds-abbreviated (code)
  (let ((nlpos (string-match (regexp-quote "\n") code)))
    (while nlpos
      (setq code
	    (if (= nlpos (- (length code) 1))
		(substring code 0 nlpos)
	      (concat (substring code 0 nlpos)
		      "\\n"
		      (substring code (+ nlpos 1)))))
      (setq nlpos (string-match (regexp-quote "\n") code))))
  (if (> (length code) gds-abbreviated-length)
      (concat (substring code 0 (- gds-abbreviated-length 3)) "...")
    code))

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

;; Help is implemented as a special case of evaluation, identified by
;; the evaluation correlator 'help.

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
  (gds-eval-expression (format "(help %s)" sym) client 'help))

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
  (set-text-properties 0 (length regex) nil regex)
  (gds-eval-expression (format "(apropos %S)" regex) client 'help))

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
      (gds-send (format "complete %s"
			(prin1-to-string
			 (buffer-substring-no-properties (- (point) chars)
							 (point))))
		 client)
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

(defun gds-display-results (client correlator results)
  (let ((helpp (eq (car correlator) 'help)))
    (let ((buf (get-buffer-create (if helpp
				      "*Guile Help*"
				    "*Guile Results*"))))
      (setq gds-results
	    (save-excursion
	      (set-buffer buf)
	      (erase-buffer)
	      (scheme-mode)
	      (insert (cdr correlator) "\n\n")
	      (while results
		(insert (car results))
		(or (bolp) (insert "\\\n"))
		(if helpp
		    nil
		  (if (cadr results)
		      (mapcar (function (lambda (value)
					  (insert " => " value "\n")))
			      (cadr results))
		    (insert " => no (or unspecified) value\n"))
		  (insert "\n"))
		(setq results (cddr results)))
	      (goto-char (point-min))
	      (if (and helpp (looking-at "Evaluating in "))
		  (delete-region (point) (progn (forward-line 1) (point))))
	      (cons correlator (buffer-string))))
      ;;(pop-to-buffer buf)
      ;;(run-hooks 'temp-buffer-show-hook)
      ;;(other-window 1)
      ))
  (gds-promote-view 'interaction)
  (gds-request-focus client))


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
  (gds-send (format "load %S" file-name) client))


;;;; Scheme mode keymap items.

(define-key scheme-mode-map "\M-\C-x" 'gds-eval-defun);gnu convention
(define-key scheme-mode-map "\C-x\C-e" 'gds-eval-last-sexp);gnu convention
(define-key scheme-mode-map "\C-c\C-e" 'gds-eval-expression)
(define-key scheme-mode-map "\C-c\C-r" 'gds-eval-region)
(define-key scheme-mode-map "\C-c\C-l" 'gds-load-file)
(define-key scheme-mode-map "\C-hg" 'gds-help-symbol)
(define-key scheme-mode-map "\C-h\C-g" 'gds-apropos)
(define-key scheme-mode-map "\e\t" 'gds-complete-symbol)
(define-key scheme-mode-map "\C-x " 'gds-set-source-breakpoint)
(define-key scheme-mode-map "\C-x\e " 'gds-delete-source-breakpoint)


;;;; Guile Interaction mode keymap and menu items.

(define-key gds-mode-map "M" (function gds-query-modules))

(define-key gds-mode-map "g" (function gds-go))
(define-key gds-mode-map "q" (function gds-quit))
(define-key gds-mode-map " " (function gds-next))
(define-key gds-mode-map "e" (function gds-evaluate))
(define-key gds-mode-map "i" (function gds-step-in))
(define-key gds-mode-map "o" (function gds-step-out))
(define-key gds-mode-map "t" (function gds-trace-finish))
(define-key gds-mode-map "I" (function gds-frame-info))
(define-key gds-mode-map "A" (function gds-frame-args))
(define-key gds-mode-map "H" (function gds-debug-trap-hooks))
(define-key gds-mode-map "u" (function gds-up))
(define-key gds-mode-map "d" (function gds-down))
(define-key gds-mode-map "b" (function gds-set-breakpoint))

(define-key gds-mode-map "vi" (function gds-view-interaction))
(define-key gds-mode-map "vs" (function gds-view-stack))
(define-key gds-mode-map "vb" (function gds-view-breakpoints))
(define-key gds-mode-map "vB" (function gds-view-browser))
(define-key gds-mode-map "vm" (function gds-view-messages))

(defvar gds-view-menu nil
  "GDS view menu.")
(if gds-view-menu
    nil
  (setq gds-view-menu (make-sparse-keymap "View"))
  (define-key gds-view-menu [messages]
    '(menu-item "Messages" gds-view-messages
		:enable (memq 'messages gds-views)))
  (define-key gds-view-menu [browser]
    '(menu-item "Browser" gds-view-browser
		:enable (memq 'browser gds-views)))
  (define-key gds-view-menu [breakpoints]
    '(menu-item "Breakpoints" gds-view-breakpoints
		:enable (memq 'breakpoints gds-views)))
  (define-key gds-view-menu [stack]
    '(menu-item "Stack" gds-view-stack
		:enable (memq 'stack gds-views)))
  (define-key gds-view-menu [interaction]
    '(menu-item "Interaction" gds-view-interaction
		:enable (memq 'interaction gds-views))))

(defvar gds-debug-menu nil
  "GDS debugging menu.")
(if gds-debug-menu
    nil
  (setq gds-debug-menu (make-sparse-keymap "Debug"))
  (define-key gds-debug-menu [go]
    '(menu-item "Go" gds-go))
  (define-key gds-debug-menu [down]
    '(menu-item "Move Down 1 Frame" gds-down))
  (define-key gds-debug-menu [up]
    '(menu-item "Move Up 1 Frame" gds-up))
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

(defvar gds-breakpoint-menu nil
  "GDS breakpoint menu.")
(if gds-breakpoint-menu
    nil
  (setq gds-breakpoint-menu (make-sparse-keymap "Breakpoint"))
  (define-key gds-breakpoint-menu [last-sexp]
    '(menu-item "Delete Breakpoint" gds-delete-source-breakpoint))
  (define-key gds-breakpoint-menu [set]
    '(menu-item "Set Breakpoint" gds-set-source-breakpoint)))

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
  (define-key gds-menu [view]
    `(menu-item "View" ,gds-view-menu :enable gds-views))
  (define-key gds-menu [debug]
    `(menu-item "Debug" ,gds-debug-menu :enable (and gds-client
						     (gds-client-blocked))))
  (define-key gds-menu [breakpoint]
    `(menu-item "Breakpoints" ,gds-breakpoint-menu :enable t))
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
				     gds-guile-program
				     nil
				     "-q")))
    (let ((proc (get-buffer-process gds-captive)))
      (process-kill-without-query proc)
      (comint-send-string proc "(set! %load-path (cons \"/home/neil/Guile/cvs/guile-core\" %load-path))\n")
      (comint-send-string proc "(debug-enable 'backtrace)\n")
      (comint-send-string proc "(use-modules (emacs gds-client))\n")
      (comint-send-string proc "(gds-connect \"Captive Guile\" #f)\n"))))

(defun gds-kill-captive ()
  (if gds-captive
      (condition-case nil
	  (progn
	    (kill-process (get-buffer-process gds-captive))
	    (accept-process-output gds-process 0 200))
	(error))))


;;;; If requested, autostart the server after loading.

(if (and gds-autostart-server
	 (not gds-process))
    (gds-start))


;;;; The end!

(provide 'gds)

;;; gds.el ends here.
