;;; gds.el -- Guile debugging frontend

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


;;;; Debugging (of this code!).

(defsubst dmessage (msg &rest args)
  ;;(apply (function message) msg args)
  )


;;;; Customization group setup.

(defgroup gds nil
  "Customization options for Guile Debugging."
  :group 'scheme)


;;;; Communication with the (ice-9 debugger ui-server) subprocess.

;; The subprocess object.
(defvar gds-process nil)

;; Subprocess output goes into the `*GDS Process*' buffer, and
;; is then read from there one form at a time.  `gds-read-cursor' is
;; the buffer position of the start of the next unread form.
(defvar gds-read-cursor nil)

;; Start (or restart) the subprocess.
(defun gds-start ()
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
			   "-e"
			   "run"
			   "-s"
			   "/home/neil/Guile/cvs/guile-core/ice-9/debugger/ui-server.scm"))))
  (setq gds-read-cursor (point-min))
  (set-process-filter gds-process (function gds-filter))
  (set-process-sentinel gds-process (function gds-sentinel))
  (set-process-coding-system gds-process 'latin-1-unix))

;; Shutdown the subprocess and cleanup all associated data.
(defun gds-shutdown ()
  ;; Do cleanup for all clients.
  (while gds-names
    (gds-client-cleanup (caar gds-names)))
  ;; Reset any remaining variables.
  (setq gds-displayed-client nil
	gds-waiting nil)
  ;; If the timer is running, cancel it.
  (if gds-timer
      (cancel-timer gds-timer))
  (setq gds-timer nil)
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

;; At any moment one Guile application has the focus of the frontend
;; code.  `gds-displayed-client' holds the port number of that client.
;; If there are no Guile applications wanting the focus - that is,
;; ready for debugging instructions - `gds-displayed-client' is nil.
(defvar gds-displayed-client nil)

;; The list of other Guile applications waiting for focus, referenced
;; by their port numbers.
(defvar gds-waiting nil)

;; An idle timer that we use to avoid confusing any user work when
;; popping up debug buffers.  `gds-timer' is non-nil whenever the
;; timer is running and nil whenever it is not running.
(defvar gds-timer nil)

;; Debug the specified client.  If it already has the focus, do so
;; immediately, but using the idle timer to ensure that it doesn't
;; confuse any work the user may be doing.  Non-structural work is
;; delegated to `gds-display-state'.
(defun gds-debug (&optional client)
  (dmessage "gds-debug")
  ;; If `client' is specified, add it to the end of `gds-waiting',
  ;; unless that client is already the current client or it is already
  ;; in the waiting list.
  (if (and client
	   (not (eq client gds-displayed-client))
	   (not (memq client gds-waiting)))
      (setq gds-waiting (append gds-waiting (list client))))
  ;; Now update `client' to be the next client in the list.
  (setq client (or gds-displayed-client (car gds-waiting)))
  ;; If conditions are right, start the idle timer.
  (if (and client
	   (or (null gds-displayed-client)
	       (eq gds-displayed-client client)))
      (gds-display-state (or gds-displayed-client
			     (prog1 (car gds-waiting)
			       (setq gds-waiting
				     (cdr gds-waiting)))))))

;; Give up focus because debugging is done for now.  Display detail in
;; case of no waiting clients is delegated to `gds-clear-display'.
(defun gds-focus-done ()
  (gds-clear-display)
  (gds-debug))

;; Although debugging of this client isn't done, yield focus to the
;; next waiting client.
(defun gds-focus-yield ()
  (interactive)
  (if (and (null gds-waiting)
	   (y-or-n-p "No other clients waiting - bury *Guile Debug* buffer? "))
      (bury-buffer)
    (or (memq gds-displayed-client gds-waiting)
	(setq gds-waiting (append gds-waiting (list gds-displayed-client))))
    (gds-focus-done)))


;;;; Per-client state information.

;; Alist mapping client port numbers to application names.  The names
;; in this list have been uniquified by `gds-uniquify'.
(defvar gds-names nil)

;; Return unique form of NAME.
(defun gds-uniquify (name)
  (let ((count 1)
	(maybe-unique name))
    (while (member maybe-unique (mapcar (function cdr) gds-names))
      (setq count (1+ count)
	    maybe-unique (concat name "<" (number-to-string count) ">")))
    maybe-unique))

;; Alist mapping client port numbers to last known status.
;;
;; Status is one of the following symbols.
;;
;;   `running' - application is running.
;;
;;   `waiting-for-input' - application is blocked waiting for
;;   instruction from the frontend.
;;
;;   `ready-for-input' - application is not blocked but can also
;;   accept asynchronous instructions from the frontend.
;;
(defvar gds-statuses nil)

;; Alist mapping client port numbers to last printed outputs.
(defvar gds-outputs nil)

;; Alist mapping client port numbers to last known stacks.
(defvar gds-stacks nil)

;; Alist mapping client port numbers to module information.  This
;; looks like:
;;
;; ((4 ((guile) t sym1 sym2 ...) ((guile-user)) ((ice-9 debug) nil sym3 sym4) ...) ...)
;;
;; So, for example:
;;
;; (assq client gds-modules)
;; =>
;; (4 ((guile) t sym1 sym2 ...) ((guile-user)) ((ice-9 debug) nil sym3 sym4) ...)
;;
;; The t or nil after the module name indicates whether the module is
;; displayed in expanded form (that is, showing the bindings in that
;; module).
;;
;; The syms are actually all strings, because some Guile symbols are
;; not readable by Emacs.
(defvar gds-modules nil)


;;;; Handling debugging instructions.

;; General dispatch function called by the subprocess filter.
(defun gds-handle-input (form)
  (dmessage "Form: %S" form)
  (let ((client (car form)))
    (cond ((eq client '*))
	  (t
	   (let ((proc (cadr form)))

	     (cond ((eq proc 'name)
		    ;; (name ...) - Application's name.
		    (setq gds-names
			  (cons (cons client (gds-uniquify (caddr form)))
				gds-names)))

		   ((eq proc 'stack)
		    ;; (stack ...) - Stack at an error or breakpoint.
		    (gds-set gds-stacks client (cddr form)))

		   ((eq proc 'modules)
		    ;; (modules ...) - Application's loaded modules.
		    (gds-set gds-modules client
			     (mapcar (function list) (cddr form))))

		   ((eq proc 'output)
		    ;; (output ...) - Last printed output.
		    (gds-set gds-outputs client (caddr form)))

		   ((eq proc 'status)
		    ;; (status ...) - Application status indication.
		    (let ((status (caddr form)))
		      (gds-set gds-statuses client status)
		      (cond ((eq status 'waiting-for-input)
			     (gds-debug client))
			    ((or (eq status 'running)
				 (eq status 'ready-for-input))
			     (if (eq client gds-displayed-client)
				 (gds-display-state client)))
			    (t
			     (error "Unexpected status: %S" status)))))

		   ((eq proc 'module)
		    ;; (module MODULE ...) - The specified module's bindings.
		    (let* ((modules (assq client gds-modules))
			   (minfo (assoc (caddr form) modules)))
		      (if minfo
			  (setcdr (cdr minfo) (cdddr form)))))

		   ((eq proc 'closed)
		    ;; (closed) - Client has gone away.
		    (gds-client-cleanup client))

		   ))))))

;; Store latest status, stack or module list for the specified client.
(defmacro gds-set (alist client val)
  `(let ((existing (assq ,client ,alist)))
     (if existing
	 (setcdr existing ,val)
       (setq ,alist
	     (cons (cons client ,val) ,alist)))))

;; Cleanup processing when CLIENT goes away.
(defun gds-client-cleanup (client)
  (if (eq client gds-displayed-client)
      (gds-focus-done))
  (setq gds-names
	(delq (assq client gds-names) gds-names))
  (setq gds-stacks
	(delq (assq client gds-stacks) gds-stacks))
  (setq gds-modules
	(delq (assq client gds-modules) gds-modules)))


;;;; Displaying debugging information.

(defvar gds-client-buffer nil)

(define-derived-mode gds-mode
  fundamental-mode
  "Guile Debugging"
  "Major mode for Guile debugging information buffers.")

(defun gds-set-client-buffer (&optional client)
  (if (and gds-client-buffer
	   (buffer-live-p gds-client-buffer))
      (set-buffer gds-client-buffer)
    (setq gds-client-buffer (get-buffer-create "*Guile Debug*"))
    (set-buffer gds-client-buffer)
    (gds-mode))
  ;; Rename to something we don't want first.  Otherwise, if the
  ;; buffer is already correctly named, we get a confusing change
  ;; from, say, `*Guile Debug: REPL*' to `*Guile Debug: REPL*<2>'.
  (rename-buffer "*Guile Debug Fake Buffer Name*" t)
  (rename-buffer (if client
		     (concat "*Guile Debug: "
			     (cdr (assq client gds-names))
			     "*")
		   "*Guile Debug*")
		 t)			; Rename uniquely if needed,
					; although it shouldn't be.
  (force-mode-line-update t))

(defun gds-clear-display ()
  ;; Clear the client buffer.
  (gds-set-client-buffer)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Stack:\nNo clients ready for debugging.\n")
    (goto-char (point-min)))
  (setq gds-displayed-stack 'no-clients)
  (setq gds-displayed-modules nil)
  (setq gds-displayed-client nil)
  (bury-buffer))

;; Determine whether the client display buffer is visible in the
;; currently selected frame (i.e. where the user is editing).
(defun gds-buffer-visible-in-selected-frame-p ()
  (let ((visible-p nil))
    (walk-windows (lambda (w)
		    (if (eq (window-buffer w) gds-client-buffer)
			(setq visible-p t))))
    visible-p))

;; Cached display variables for `gds-display-state'.
(defvar gds-displayed-stack nil)
(defvar gds-displayed-modules nil)

;; Types of display areas in the *Guile Debug* buffer.
(defvar gds-display-types '("Status" "Stack" "Modules"))
(defvar gds-display-type-regexp
  (concat "^\\("
	  (substring (apply (function concat)
			    (mapcar (lambda (type)
				      (concat "\\|" type))
				    gds-display-types))
		     2)
	  "\\):"))

(defun gds-maybe-delete-region (type)
  (let ((beg (save-excursion
	       (goto-char (point-min))
	       (and (re-search-forward (concat "^"
					       (regexp-quote type)
					       ":")
				       nil t)
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

(defun gds-maybe-skip-region (type)
  (if (looking-at (regexp-quote type))
      (if (re-search-forward gds-display-type-regexp nil t 2)
	  (beginning-of-line)
	(goto-char (point-max)))))

(defun gds-display-state (client)
  (dmessage "gds-display-state")
  ;; Avoid continually popping up the last associated source buffer
  ;; unless it really is still current.
  (setq gds-selected-frame-source-buffer nil)
  (gds-set-client-buffer client)
  (let ((stack (cdr (assq client gds-stacks)))
	(modules (cdr (assq client gds-modules)))
	(inhibit-read-only t)
	(p (if (eq client gds-displayed-client)
	       (point)
	     (point-min)))
	stack-changed)
    ;; Start at top of buffer.
    (goto-char (point-min))
    ;; Display status; too simple to be worth caching.
    (gds-maybe-delete-region "Status")
    (widget-insert "Status: "
		   (cdr (assq (cdr (assq client gds-statuses))
			      '((running . "running (cannot accept input)")
				(waiting-for-input . "waiting for input")
				(ready-for-input . "running"))))
		   "\n\n")
    (let ((output (cdr (assq client gds-outputs))))
      (if (> (length output) 0)
	  (widget-insert output "\n\n")))
    ;; Display stack.
    (dmessage "insert stack")
    (if (equal stack gds-displayed-stack)
	(gds-maybe-skip-region "Stack")
      ;; Note that stack has changed.
      (if stack (setq stack-changed t))
      ;; Delete existing stack.
      (gds-maybe-delete-region "Stack")
      ;; Insert new stack.		       
      (if stack (gds-insert-stack stack))
      ;; Record displayed stack.
      (setq gds-displayed-stack stack))
    ;; Display module list.
    (dmessage "insert modules")
    (if (equal modules gds-displayed-modules)
	(gds-maybe-skip-region "Modules")
      ;; Delete existing module list.
      (gds-maybe-delete-region "Modules")
      ;; Insert new list.
      (if modules (gds-insert-modules modules))
      ;; Record displayed list.
      (setq gds-displayed-modules (copy-tree modules)))
    ;; Finish off.
    (dmessage "widget-setup")
    (widget-setup)
    (if stack-changed
	;; Stack is being seen for the first time, so make sure top of
	;; buffer is visible.
	(progn
	  (goto-char (point-min))
	  (re-search-forward "^Stack:")
	  (forward-line (+ 1 (cadr stack))))
      ;; Restore point from before buffer was redrawn.
      (goto-char p)))
  (setq gds-displayed-client client)
  (dmessage "consider display")
  (if (eq (window-buffer (selected-window)) gds-client-buffer)
      ;; *Guile Debug* buffer already selected.
      (gds-display-buffers)
    (dmessage "Running GDS timer")
    (setq gds-timer
	  (run-with-idle-timer 0.5
			       nil
			       (lambda ()
				 (setq gds-timer nil)
				 (gds-display-buffers))))))

(defun gds-display-buffers ()
  ;; If there's already a window showing the *Guile Debug* buffer, use
  ;; it.
  (let ((window (get-buffer-window gds-client-buffer t)))
    (if window
	(progn
	  (make-frame-visible (window-frame window))
	  (raise-frame (window-frame window))
	  (select-frame (window-frame window))
	  (select-window window))
      (switch-to-buffer gds-client-buffer)))
  ;; If there is an associated source buffer, display it as well.
  (if gds-selected-frame-source-buffer
      (let ((window (display-buffer gds-selected-frame-source-buffer)))
	(set-window-point window
			  (overlay-start gds-selected-frame-source-overlay))))
  ;; Force redisplay.
  (sit-for 0))

(defun old-stuff ()
  (if (gds-buffer-visible-in-selected-frame-p)
      ;; Buffer already visible enough.
      nil
    ;; Delete any views of the buffer in other frames - we don't want
    ;; views all over the place.
    (delete-windows-on gds-client-buffer)
    ;; Run idle timer to display the buffer as soon as user isn't in
    ;; the middle of something else.
    ))

(defun gds-insert-stack (stack)
  (let ((frames (car stack))
	(index (cadr stack))
	(flags (caddr stack))
	frame items)
    (widget-insert "Stack: " (prin1-to-string flags) "\n")
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
		      gds-displayed-client
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
    (setq modules (cdr modules))))

(defun gds-module-notify (w &rest ignore)
  (let* ((module (widget-get w :module))
	 (client (car module))
	 (name (cdr module))
	 (modules (assq client gds-modules))
	 (minfo (assoc name modules)))
    (if (cdr minfo)
	;; Just toggle expansion state.
	(progn
	  (setcar (cdr minfo) (not (cadr minfo)))
	  (gds-display-state client))
      ;; Set flag to indicate module expanded.
      (setcdr minfo (list t))
      ;; Get symlist from Guile.
      (gds-send (format "(%S query-module %S)\n" client name)))))


;;;; Guile Debugging keymap.

(set-keymap-parent gds-mode-map widget-keymap)
(define-key gds-mode-map "g" (function gds-go))
(define-key gds-mode-map "b" (function gds-set-breakpoint))
(define-key gds-mode-map "q" (function gds-quit))
(define-key gds-mode-map "y" (function gds-yield))
(define-key gds-mode-map " " (function gds-next))
(define-key gds-mode-map "e" (function gds-evaluate))
(define-key gds-mode-map "i" (function gds-step-in))
(define-key gds-mode-map "o" (function gds-step-out))
(define-key gds-mode-map "t" (function gds-trace-finish))

(defun gds-client-waiting ()
  (eq (cdr (assq gds-displayed-client gds-statuses)) 'waiting-for-input))

(defun gds-go ()
  (interactive)
  (gds-send (format "(%S debugger-command continue)\n" gds-displayed-client)))

(defun gds-quit ()
  (interactive)
  (if (gds-client-waiting)
      (if (y-or-n-p "Client is waiting for instruction - tell it to continue? ")
	  (gds-go)))
  (gds-yield))

(defun gds-yield ()
  (interactive)
  (if (gds-client-waiting)
      (gds-focus-yield)
    (gds-focus-done)))

(defun gds-next ()
  (interactive)
  (gds-send (format "(%S debugger-command next 1)\n" gds-displayed-client)))

(defun gds-evaluate (expr)
  (interactive "sEvaluate (in this stack frame): ")
  (gds-send (format "(%S debugger-command evaluate %s)\n"
		    gds-displayed-client
		    (prin1-to-string expr))))

(defun gds-step-in ()
  (interactive)
  (gds-send (format "(%S debugger-command step 1)\n" gds-displayed-client)))

(defun gds-step-out ()
  (interactive)
  (gds-send (format "(%S debugger-command finish)\n" gds-displayed-client)))

(defun gds-trace-finish ()
  (interactive)
  (gds-send (format "(%S debugger-command trace-finish)\n"
		    gds-displayed-client)))

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
  (and (eq (current-buffer) gds-client-buffer)
       (save-excursion
	 (and (re-search-backward "^\\(Stack\\|Modules\\):" nil t)
	      (looking-at "Stack")))))

(defun gds-in-modules ()
  (and (eq (current-buffer) gds-client-buffer)
       (save-excursion
	 (and (re-search-backward "^\\(Stack\\|Modules\\):" nil t)
	      (looking-at "Modules")))))

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
			gds-displayed-client
			module
			sym
			behaviour)))))


;;;; Evaluating code.

;; The Scheme process to which code is sent is determined in the usual
;; cmuscheme.el way by the `scheme-buffer' variable (q.v.).
;; Customizations to the way that code is sent, for example pro- and
;; postlogs to set up and restore evaluation context correctly in the
;; Scheme process, are achieved (elsewhere than this file) by advising
;; `scheme-send-region' accordingly.
