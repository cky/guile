;;; gds-scheme.el -- GDS function for Scheme mode buffers

;;;; Copyright (C) 2005 Neil Jerram
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

(require 'comint)
(require 'scheme)
(require 'derived)
(require 'pp)

;;;; Maintaining an association between a Guile client process and a
;;;; set of Scheme mode buffers.

(defcustom gds-auto-create-utility-client t
  "Whether to automatically create a utility Guile client, and
associate the current buffer with it, if there are no existing Guile
clients available to GDS when the user does something that requires a
running Guile client."
  :type 'boolean
  :group 'gds)

(defcustom gds-auto-associate-single-client t
  "Whether to automatically associate the current buffer with an
existing Guile client, if there is only only client known to GDS when
the user does something that requires a running Guile client, and the
current buffer is not already associated with a Guile client."
  :type 'boolean
  :group 'gds)

(defcustom gds-auto-associate-last-client t
  "Whether to automatically associate the current buffer with the
Guile client that most recently caused that buffer to be displayed,
when the user does something that requires a running Guile client and
the current buffer is not already associated with a Guile client."
  :type 'boolean
  :group 'gds)

(defvar gds-last-touched-by nil
  "For each Scheme mode buffer, this records the GDS client that most
recently `touched' that buffer in the sense of using it to display
source code, for example for the source code relevant to a debugger
stack frame.")
(make-variable-buffer-local 'gds-last-touched-by)

(defun gds-auto-associate-buffer ()
  "Automatically associate the current buffer with a Guile client, if
possible."
  (let* ((num-clients (length gds-client-info))
	 (client
	  (or
	   ;; If there are no clients yet, and
	   ;; `gds-auto-create-utility-client' allows us to create one
	   ;; automatically, do that.
	   (and (= num-clients 0)
		gds-auto-create-utility-client
		(gds-start-utility-guile))
	   ;; Otherwise, if there is a single existing client, and
	   ;; `gds-auto-associate-single-client' allows us to use it
	   ;; for automatic association, do that.
	   (and (= num-clients 1)
		gds-auto-associate-single-client
		(caar gds-client-info))
	   ;; Otherwise, if the current buffer was displayed because
	   ;; of a Guile client trapping somewhere in its code, and
	   ;; `gds-auto-associate-last-client' allows us to associate
	   ;; with that client, do so.
	   (and gds-auto-associate-last-client
		gds-last-touched-by))))
    (if client
	(gds-associate-buffer client))))	 

(defun gds-associate-buffer (client)
  "Associate the current buffer with the Guile process CLIENT.
This means that operations in this buffer that require a running Guile
process - such as evaluation, help, completion and setting traps -
will be sent to the Guile process whose name or connection number is
CLIENT."
  (interactive (list (gds-choose-client)))
  ;; If this buffer is already associated, dissociate from its
  ;; existing client first.
  (if gds-client (gds-dissociate-buffer))
  ;; Store the client number in the buffer-local variable gds-client.
  (setq gds-client client)
  ;; Add this buffer to the list of buffers associated with the
  ;; client.
  (gds-client-put client 'associated-buffers
		  (cons (current-buffer)
			(gds-client-get client 'associated-buffers))))

(defun gds-dissociate-buffer ()
  "Dissociate the current buffer from any specific Guile process."
  (interactive)
  (if gds-client
      (progn
        ;; Remove this buffer from the list of buffers associated with
        ;; the current client.
	(gds-client-put gds-client 'associated-buffers
			(delq (current-buffer)
			      (gds-client-get gds-client 'associated-buffers)))
        ;; Reset the buffer-local variable gds-client.
        (setq gds-client nil)
        ;; Clear any process status indication from the modeline.
        (setq mode-line-process nil)
        (force-mode-line-update))))

(defun gds-show-client-status (client status-string)
  "Show a client's status in the modeline of all its associated
buffers."
  (let ((buffers (gds-client-get client 'associated-buffers)))
    (while buffers
      (if (buffer-live-p (car buffers))
          (with-current-buffer (car buffers)
            (setq mode-line-process status-string)
            (force-mode-line-update)))
      (setq buffers (cdr buffers)))))

(defcustom gds-running-text ":running"
  "*Mode line text used to show that a Guile process is \"running\".
\"Running\" means that the process cannot currently accept any input
from the GDS frontend in Emacs, because all of its threads are busy
running code that GDS cannot easily interrupt."
  :type 'string
  :group 'gds)

(defcustom gds-ready-text ":ready"
  "*Mode line text used to show that a Guile process is \"ready\".
\"Ready\" means that the process is ready to interact with the GDS
frontend in Emacs, because at least one of its threads is waiting for
GDS input."
  :type 'string
  :group 'gds)

(defcustom gds-debug-text ":debug"
  "*Mode line text used to show that a Guile process is \"debugging\".
\"Debugging\" means that the process is using the GDS frontend in
Emacs to display an error or trap so that the user can debug it."
  :type 'string
  :group 'gds)

(defun gds-choose-client ()
  "Ask the user to choose a GDS client process from a list."
  (let ((table '())
        (default nil))
    ;; Prepare a table containing all current clients.
    (mapcar (lambda (client-info)
               (setq table (cons (cons (cadr (assq 'name client-info))
				       (car client-info))
				 table)))
             gds-client-info)
    ;; Add an entry to allow the user to ask for a new process.
    (setq table (cons (cons "Start a new Guile process" nil) table))
    ;; Work out a good default.  If the buffer has a good value in
    ;; gds-last-touched-by, we use that; otherwise default to starting
    ;; a new process.
    (setq default (or (and gds-last-touched-by
                           (gds-client-get gds-last-touched-by 'name))
                      (caar table)))
    ;; Read using this table.
    (let* ((name (completing-read "Choose a Guile process: "
                                  table
                                  nil
                                  t     ; REQUIRE-MATCH
                                  nil   ; INITIAL-INPUT
                                  nil   ; HIST
                                  default))
           ;; Convert name to a client number.
           (client (cdr (assoc name table))))
      ;; If the user asked to start a new Guile process, do that now.
      (or client (setq client (gds-start-utility-guile)))
      ;; Return the chosen client ID.
      client)))

(defvar gds-last-utility-number 0
  "Number of the last started Guile utility process.")

(defun gds-start-utility-guile ()
  "Start a new utility Guile process."
  (setq gds-last-utility-number (+ gds-last-utility-number 1))
  (let* ((procname (format "gds-util[%d]" gds-last-utility-number))
         (code (format "(begin
                          %s
                          (use-modules (ice-9 gds-client))
                          (run-utility))"
		       (if gds-scheme-directory
			   (concat "(set! %load-path (cons "
				   (format "%S" gds-scheme-directory)
				   " %load-path))")
			 "")))
         (proc (start-process procname
                              (get-buffer-create procname)
                              gds-guile-program
                              "-q"
                              "--debug"
                              "-c"
                              code))
         (client nil))
    ;; Note that this process can be killed automatically on Emacs
    ;; exit.
    (process-kill-without-query proc)
    ;; Set up a process filter to catch the new client's number.
    (set-process-filter proc
                        (lambda (proc string)
                          (setq client (string-to-number string))
                          (if (process-buffer proc)
                              (with-current-buffer (process-buffer proc)
                                (insert string)))))
    ;; Accept output from the new process until we have its number.
    (while (not client)
      (accept-process-output proc))
    ;; Return the new process's client number.
    client))

;;;; Evaluating code.

;; The following commands send code for evaluation through the GDS TCP
;; connection, receive the result and any output generated through the
;; same connection, and display the result and output to the user.
;;
;; For each buffer where evaluations can be requested, GDS uses the
;; buffer-local variable `gds-client' to track which GDS client
;; program should receive and handle that buffer's evaluations.

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

(defcustom gds-emacs-buffer-port-name-prefix "Emacs buffer: "
  "Prefix used when telling Guile the name of the port from which a
chunk of Scheme code (to be evaluated) comes.  GDS uses this prefix,
followed by the buffer name, in two cases: when the buffer concerned
is not associated with a file, or if the buffer has been modified
since last saving to its file.  In the case where the buffer is
identical to a saved file, GDS uses the file name as the port name."
  :type '(string)
  :group 'gds)

(defun gds-port-name (start end)
  "Return port name for the specified region of the current buffer.
The name will be used by Guile as the port name when evaluating that
region's code."
  (or (and (not (buffer-modified-p))
	   buffer-file-name)
      (concat gds-emacs-buffer-port-name-prefix (buffer-name))))

(defun gds-line-and-column (pos)
  "Return 0-based line and column number at POS."
  (let (line column)
    (save-excursion
      (goto-char pos)
      (setq column (current-column))
      (beginning-of-line)
      (setq line (count-lines (point-min) (point))))
    (cons line column)))

(defun gds-eval-region (start end)
  "Evaluate the current region."
  (interactive "r")
  (or gds-client
      (gds-auto-associate-buffer)
      (call-interactively 'gds-associate-buffer))
  (let ((module (gds-module-name start end))
	(port-name (gds-port-name start end))
	(lc (gds-line-and-column start)))
    (let ((code (buffer-substring-no-properties start end)))
      (gds-send (format "eval (region . %S) %s %S %d %d %S"
			(gds-abbreviated code)
			(if module (prin1-to-string module) "#f")
			port-name (car lc) (cdr lc)
			code)
		gds-client))))

(defun gds-eval-expression (expr &optional correlator)
  "Evaluate the supplied EXPR (a string)."
  (interactive "sEvaluate expression: \nP")
  (or gds-client
      (gds-auto-associate-buffer)
      (call-interactively 'gds-associate-buffer))
  (set-text-properties 0 (length expr) nil expr)
  (gds-send (format "eval (%S . %S) #f \"Emacs expression\" 0 0 %S"
		    (or correlator 'expression)
		    (gds-abbreviated expr)
		    expr)
	    gds-client))

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

(defun gds-eval-defun ()
  "Evaluate the defun (top-level form) at point."
  (interactive)
  (save-excursion
   (end-of-defun)
   (let ((end (point)))
     (beginning-of-defun)
     (gds-eval-region (point) end))))

(defun gds-eval-last-sexp ()
  "Evaluate the sexp before point."
  (interactive)
  (gds-eval-region (save-excursion (backward-sexp) (point)) (point)))

;;;; Help.

;; Help is implemented as a special case of evaluation, identified by
;; the evaluation correlator 'help.

(defun gds-help-symbol (sym)
  "Get help for SYM (a Scheme symbol)."
  (interactive
   (let ((sym (thing-at-point 'symbol))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (read-from-minibuffer
		(if sym
		    (format "Describe Guile symbol (default %s): " sym)
		  "Describe Guile symbol: ")))
     (list (if (zerop (length val)) sym val))))
  (gds-eval-expression (format "(help %s)" sym) 'help))

(defun gds-apropos (regex)
  "List Guile symbols matching REGEX."
  (interactive
   (let ((sym (thing-at-point 'symbol))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (read-from-minibuffer
		(if sym
		    (format "Guile apropos (regexp, default \"%s\"): " sym)
		  "Guile apropos (regexp): ")))
     (list (if (zerop (length val)) sym val))))
  (set-text-properties 0 (length regex) nil regex)
  (gds-eval-expression (format "(apropos %S)" regex) 'apropos))

;;;; Displaying results of help and eval.

(defun gds-display-results (client correlator stack-available results)
  (let* ((helpp+bufname (cond ((eq (car correlator) 'help)
                               '(t . "*Guile Help*"))
                              ((eq (car correlator) 'apropos)
                               '(t . "*Guile Apropos*"))
                              (t
                               '(nil . "*Guile Evaluation*"))))
         (helpp (car helpp+bufname)))
    (let ((buf (get-buffer-create (cdr helpp+bufname))))
      (save-excursion
        (set-buffer buf)
	(gds-dissociate-buffer)
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
        (if stack-available
            (let ((beg (point))
                  (map (make-sparse-keymap)))
              (define-key map [mouse-1] 'gds-show-last-stack)
              (insert "[click here to show error stack]")
              (add-text-properties beg (point)
                                   (list 'keymap map
                                         'mouse-face 'highlight))
              (insert "\n")))
        (goto-char (point-min))
        (gds-associate-buffer client))
      (pop-to-buffer buf)
      (run-hooks 'temp-buffer-show-hook)
      (other-window 1))))

(defun gds-show-last-stack ()
  "Show stack of the most recent error."
  (interactive)
  (or gds-client
      (gds-auto-associate-buffer)
      (call-interactively 'gds-associate-buffer))
  (gds-send "debug-lazy-trap-context" gds-client))

;;;; Completion.

(defvar gds-completion-results nil)

(defun gds-complete-symbol ()
  "Complete the Guile symbol before point.  Returns `t' if anything
interesting happened, `nil' if not."
  (interactive)
  (or gds-client
      (gds-auto-associate-buffer)
      (call-interactively 'gds-associate-buffer))
  (let* ((chars (- (point) (save-excursion
			     (while (let ((syntax (char-syntax (char-before (point)))))
				      (or (eq syntax ?w) (eq syntax ?_)))
			       (forward-char -1))
			     (point)))))
    (if (zerop chars)
	nil
      (setq gds-completion-results nil)
      (gds-send (format "complete %s"
			(prin1-to-string
			 (buffer-substring-no-properties (- (point) chars)
							 (point))))
		 gds-client)
      (while (null gds-completion-results)
	(accept-process-output gds-debug-server 0 200))
      (cond ((eq gds-completion-results 'error)
             (error "Internal error - please report the contents of the *Guile Evaluation* window"))
	    ((eq gds-completion-results t)
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

;;;; Breakpoints.

(defvar gds-bufferless-breakpoints nil
  "The list of breakpoints that are not yet associated with a
particular buffer.  Each element looks like (BPDEF BPNUM) where BPDEF
is the breakpoint definition and BPNUM the breakpoint's unique
GDS-assigned number.  A breakpoint definition BPDEF is a list of the
form (BEHAVIOUR TYPE FILENAME TYPE-ARGS...), where BEHAVIOUR is 'debug
or 'trace, TYPE is 'in or 'at, FILENAME is the full name of the file
where the breakpoint is (or will be) set, and TYPE-ARGS is:

- the name of the procedure to break in, if TYPE is 'in

- the line number and column number to break at, if TYPE is 'at.

If persistent breakpoints are enabled (by configuring
gds-breakpoints-file-name), this list is initialized when GDS is
loaded by reading gds-breakpoints-file-name.")

(defsubst gds-bpdef:behaviour (bpdef)
  (nth 0 bpdef))

(defsubst gds-bpdef:type (bpdef)
  (nth 1 bpdef))

(defsubst gds-bpdef:file-name (bpdef)
  (nth 2 bpdef))

(defsubst gds-bpdef:proc-name (bpdef)
  (nth 3 bpdef))

(defsubst gds-bpdef:lc (bpdef)
  (nth 3 bpdef))

(defvar gds-breakpoint-number 0
  "The last assigned breakpoint number.  GDS increments this whenever
it creates a new breakpoint.")

(defvar gds-breakpoint-buffers nil
  "The list of buffers that contain GDS breakpoints.  When Emacs
visits a Scheme file, GDS checks to see if any of the breakpoints in
the bufferless list can be assigned to that file's buffer.  If they
can, they are removed from the bufferless list and become breakpoint
overlays in that buffer.  To retain the ability to enumerate all
breakpoints, therefore, we keep a list of all such buffers.")

(defvar gds-breakpoint-programming nil
  "Information about how each breakpoint is actually programmed in the
Guile clients that GDS is connected to.  This is an alist of the form
\((BPNUM (CLIENT . TRAPLIST) ...) ...), where BPNUM is the breakpoint
number, CLIENT is the number of a GDS client, and TRAPLIST is the list
of traps that that client has created for the breakpoint concerned (in
an arbitrary but Emacs-readable format).")

(defvar gds-breakpoint-cache nil
  "Buffer-local cache of breakpoints in a particular buffer.  When a
breakpoint is represented as an overlay is a Scheme mode buffer, we
need to be able to detect when the user has caused that overlay to
evaporate by deleting a region of code that included it.  We do this
detection when the buffer is next saved, by comparing the current set
of overlays with this cache.  The cache is a list in which each
element has the form (BPDEF BPNUM), with BPDEF and BPNUM as already
described.  The handling of such breakpoints (which we call \"lost\")
is controlled by the setting of gds-delete-lost-breakpoints.")
(make-variable-buffer-local 'gds-breakpoint-cache)

(defface gds-breakpoint-face
  '((((background dark)) (:background "red"))
    (t (:background "pink")))
  "*Face used to highlight the location of a breakpoint."
  :group 'gds)

(defcustom gds-breakpoints-file-name "~/.gds-breakpoints"
  "Name of file used to store GDS breakpoints between sessions.
You can disable breakpoint persistence by setting this to nil."
  :group 'gds
  :type '(choice (const :tag "nil" nil) file))

(defcustom gds-delete-lost-breakpoints nil
  "Whether to delete lost breakpoints.

A non-nil value means that the Guile clients where lost breakpoints
were programmed will be told immediately to delete their breakpoints.
\"Immediately\" means when the lost breakpoints are detected, which
means when the buffer that previously contained them is saved.  Thus,
even if the affected code (which the GDS user has deleted from his/her
buffer in Emacs) is still in use in the Guile clients, the breakpoints
that were previously set in that code will no longer take effect.

Nil (which is the default) means that GDS leaves such breakpoints
active in their Guile clients.  This allows those breakpoints to
continue taking effect until the affected code is no longer used by
the Guile clients."
  :group 'gds
  :type 'boolean)

(defvar gds-bpdefs-cache nil)

(defun gds-read-breakpoints-file ()
  "Read the persistent breakpoints file, and use its contents to
initialize GDS's global breakpoint variables."
  (let ((bpdefs (condition-case nil
		    (with-current-buffer
			(find-file-noselect gds-breakpoints-file-name)
		      (goto-char (point-min))
		      (read (current-buffer)))
		  (error nil))))
    ;; Cache the overall value so we don't unnecessarily modify the
    ;; breakpoints buffer when `gds-write-breakpoints-file' is called.
    (setq gds-bpdefs-cache bpdefs)
    ;; Move definitions into the bufferless breakpoint list, assigning
    ;; breakpoint numbers as we go.
    (setq gds-bufferless-breakpoints
	  (mapcar (function (lambda (bpdef)
			      (setq gds-breakpoint-number
				    (1+ gds-breakpoint-number))
			      (list bpdef gds-breakpoint-number)))
		  bpdefs))
    ;; Check each existing Scheme buffer to see if it wants to take
    ;; ownership of any of these breakpoints.
    (mapcar (function (lambda (buffer)
			(with-current-buffer buffer
			  (if (eq (derived-mode-class major-mode) 'scheme-mode)
			      (gds-adopt-breakpoints)))))
	    (buffer-list))))

(defun gds-adopt-breakpoints ()
  "Take ownership of any of the breakpoints in the bufferless list
that match the current buffer."
  (mapcar (function gds-adopt-breakpoint)
	  (copy-sequence gds-bufferless-breakpoints)))

(defun gds-adopt-breakpoint (bpdefnum)
  "Take ownership of the specified breakpoint if it matches the
current buffer."
  (let ((bpdef (car bpdefnum))
	(bpnum (cadr bpdefnum)))
    ;; Check if breakpoint's file name matches.  If it does, try to
    ;; convert the breakpoint definition to a breakpoint overlay in
    ;; the current buffer.
    (if (and (string-equal (gds-bpdef:file-name bpdef) buffer-file-name)
	     (gds-make-breakpoint-overlay bpdef bpnum))
	;; That all succeeded, so this breakpoint is no longer
	;; bufferless.
	(setq gds-bufferless-breakpoints
	      (delq bpdefnum gds-bufferless-breakpoints)))))

(defun gds-make-breakpoint-overlay (bpdef &optional bpnum)
  ;; If no explicit number given, assign the next available breakpoint
  ;; number.
  (or bpnum
      (setq gds-breakpoint-number (+ gds-breakpoint-number 1)
	    bpnum gds-breakpoint-number))
  ;; First decide where the overlay should be, and create it there.
  (let ((o (cond ((eq (gds-bpdef:type bpdef) 'at)
		  (save-excursion
		    (goto-line (+ (car (gds-bpdef:lc bpdef)) 1))
		    (move-to-column (cdr (gds-bpdef:lc bpdef)))
		    (make-overlay (point) (1+ (point)))))
		 ((eq (gds-bpdef:type bpdef) 'in)
		  (save-excursion
		    (goto-char (point-min))
		    (and (re-search-forward (concat "^(define +(?\\("
						    (regexp-quote
						     (gds-bpdef:proc-name
						      bpdef))
						    "\\>\\)")
					    nil t)
			 (make-overlay (match-beginning 1) (match-end 1)))))
		 (t
		  (error "Bad breakpoint type")))))
    ;; If that succeeded, initialize the overlay's properties.
    (if o
	(progn
	  (overlay-put o 'evaporate t)
	  (overlay-put o 'face 'gds-breakpoint-face)
	  (overlay-put o 'gds-breakpoint-number bpnum)
	  (overlay-put o 'gds-breakpoint-definition bpdef)
	  (overlay-put o 'help-echo (format "Breakpoint %d: %S" bpnum bpdef))
	  (overlay-put o 'priority 1000)
	  ;; Make sure that the current buffer is included in
	  ;; `gds-breakpoint-buffers'.
	  (or (memq (current-buffer) gds-breakpoint-buffers)
	      (setq gds-breakpoint-buffers
		    (cons (current-buffer) gds-breakpoint-buffers)))
	  ;; Add the new breakpoint to this buffer's cache.
	  (setq gds-breakpoint-cache
		(cons (list bpdef bpnum) gds-breakpoint-cache))
	  ;; If this buffer is associated with a client, tell the
	  ;; client about the new breakpoint.
	  (if gds-client (gds-send-breakpoint-to-client bpnum bpdef))))
    ;; Return the overlay, or nil if we weren't able to convert the
    ;; breakpoint definition.
    o))

(defun gds-send-breakpoint-to-client (bpnum bpdef)
  "Send specified breakpoint to this buffer's Guile client."
  (gds-send (format "set-breakpoint %d %S" bpnum bpdef) gds-client))

(add-hook 'scheme-mode-hook (function gds-adopt-breakpoints))

(defcustom gds-default-breakpoint-type 'debug
  "The type of breakpoint set by `C-x SPC'."
  :group 'gds
  :type '(choice (const :tag "debug" debug) (const :tag "trace" trace)))

(defun gds-set-breakpoint ()
  "Create a new GDS breakpoint at point."
  (interactive)
  ;; Set up beg and end according to whether the mark is active.
  (if mark-active
      ;; Set new breakpoints on all opening parentheses in the region.
      (let ((beg (region-beginning))
	    (end (region-end)))
	(save-excursion
	  (goto-char beg)
	  (beginning-of-defun)
	  (let ((defun-start (point)))
	    (goto-char beg)
	    (while (search-forward "(" end t)
	      (let ((state (parse-partial-sexp defun-start (point)))
		    (pos (- (point) 1)))
		(or (nth 3 state)
		    (nth 4 state)
		    (gds-breakpoint-overlays-at pos)
		    (gds-make-breakpoint-overlay (list gds-default-breakpoint-type
						       'at
						       buffer-file-name
						       (gds-line-and-column
							pos)))))))))
    ;; Set a new breakpoint on the defun at point.
    (let ((region (gds-defun-name-region)))
      ;; Complain if there is no defun at point.
      (or region
	  (error "Point is not in a procedure definition"))
      ;; Don't create another breakpoint if there is already one here.
      (if (gds-breakpoint-overlays-at (car region))
	  (error "There is already a breakpoint here"))
      ;; Create and return the new breakpoint overlay.
      (gds-make-breakpoint-overlay (list gds-default-breakpoint-type
					 'in
					 buffer-file-name
					 (buffer-substring-no-properties
					  (car region)
					  (cdr region))))))
  ;; Update the persistent breakpoints file.
  (gds-write-breakpoints-file))

(defun gds-defun-name-region ()
  "If point is in a defun, return the beginning and end positions of
the identifier being defined."
  (save-excursion
    (let ((p (point)))
      (beginning-of-defun)
      ;; Check that we are looking at some kind of procedure
      ;; definition.
      (and (looking-at "(define +(?\\(\\(\\s_\\|\\w\\)+\\)")
	   (let ((beg (match-beginning 1))
		 (end (match-end 1)))
	     (end-of-defun)
	     ;; Check here that we have reached past the original point
	     ;; position.
	     (and (>= (point) p)
		  (cons beg end)))))))

(defun gds-breakpoint-overlays-at (pos)
  "Return a list of GDS breakpoint overlays at the specified position."
  (let ((os (overlays-at pos))
	(breakpoint-os nil))
    ;; Of the overlays at POS, select all those that have a
    ;; gds-breakpoint-definition property.
    (while os
      (if (overlay-get (car os) 'gds-breakpoint-definition)
	  (setq breakpoint-os (cons (car os) breakpoint-os)))
      (setq os (cdr os)))
    breakpoint-os))

(defun gds-write-breakpoints-file ()
  "Write the persistent breakpoints file, if configured."
  (if gds-breakpoints-file-name
      (let ((bpdefs (gds-fold-breakpoints (function (lambda (bpnum bpdef init)
						      (cons bpdef init)))
					  t)))
        (or (equal bpdefs gds-bpdefs-cache)
            (with-current-buffer (find-file-noselect gds-breakpoints-file-name)
              (erase-buffer)
              (pp (reverse bpdefs) (current-buffer))
              (setq gds-bpdefs-cache bpdefs)
              (let ((auto-fill-function normal-auto-fill-function))
                (newline)))))))

(defun gds-fold-breakpoints (fn &optional foldp init)
  ;; Run through bufferless breakpoints first.
  (let ((bbs gds-bufferless-breakpoints))
    (while bbs
      (let ((bpnum (cadr (car bbs)))
	    (bpdef (caar bbs)))
	(if foldp
	    (setq init (funcall fn bpnum bpdef init))
	  (funcall fn bpnum bpdef)))
      (setq bbs (cdr bbs))))
  ;; Now run through breakpoint buffers.
  (let ((outbuf (current-buffer))
	(bpbufs gds-breakpoint-buffers))
    (while bpbufs
      (let ((buf (car bpbufs)))
	(if (buffer-live-p buf)
	    (with-current-buffer buf
	      (save-restriction
		(widen)
		(let ((os (overlays-in (point-min) (point-max))))
		  (while os
		    (let ((bpnum (overlay-get (car os)
					      'gds-breakpoint-number))
			  (bpdef (overlay-get (car os)
					      'gds-breakpoint-definition)))
		      (if bpdef
			  (with-current-buffer outbuf
			    (if foldp
				(setq init (funcall fn bpnum bpdef init))
			      (funcall fn bpnum bpdef)))))
		    (setq os (cdr os))))))))
      (setq bpbufs (cdr bpbufs))))
  init)

(defun gds-delete-breakpoints ()
  "Delete GDS breakpoints in the region or at point."
  (interactive)
  (if mark-active
      ;; Delete all breakpoints in the region.
      (let ((os (overlays-in (region-beginning) (region-end))))
	(while os
	  (if (overlay-get (car os) 'gds-breakpoint-definition)
	      (gds-delete-breakpoint (car os)))
	  (setq os (cdr os))))
    ;; Delete the breakpoint "at point".
    (call-interactively (function gds-delete-breakpoint))))

(defun gds-delete-breakpoint (o)
  (interactive (list (or (gds-breakpoint-at-point)
			 (error "There is no breakpoint here"))))
  (let ((bpdef (overlay-get o 'gds-breakpoint-definition))
	(bpnum (overlay-get o 'gds-breakpoint-number)))
    ;; If this buffer is associated with a client, tell the client
    ;; that the breakpoint has been deleted.
    (if (and bpnum gds-client)
	(gds-send (format "delete-breakpoint %d" bpnum) gds-client))
    ;; Remove this breakpoint from the cache also, so it isn't later
    ;; detected as having been "lost".
    (setq gds-breakpoint-cache
	  (delq (assq bpdef gds-breakpoint-cache) gds-breakpoint-cache)))
  ;; Remove the overlay from its buffer.
  (delete-overlay o)
  ;; If that was the last breakpoint in this buffer, remove this
  ;; buffer from gds-breakpoint-buffers.
  (or gds-breakpoint-cache
      (setq gds-breakpoint-buffers
	    (delq (current-buffer) gds-breakpoint-buffers)))
  ;; Update the persistent breakpoints file.
  (gds-write-breakpoints-file))

(defun gds-breakpoint-at-point ()
  "Find and return the overlay for a breakpoint `at' the current
cursor position.  This is intended for use in other functions'
interactive forms, so it intentionally uses the minibuffer in some
situations."
  (let* ((region (gds-defun-name-region))
	 (os (gds-union (gds-breakpoint-overlays-at (point))
			(and region
			     (gds-breakpoint-overlays-at (car region))))))
    ;; Switch depending whether we found 0, 1 or more overlays.
    (cond ((null os)
	   ;; None found: return nil.
	   nil)
	  ((= (length os) 1)
	   ;; One found: return it.
	   (car os))
	  (t
	   ;; More than 1 found: ask the user to choose.
	   (gds-user-selected-breakpoint os)))))

(defun gds-union (first second &rest others)
  (if others
      (gds-union first (apply 'gds-union second others))
    (progn
      (while first
	(or (memq (car first) second)
	    (setq second (cons (car first) second)))
	(setq first (cdr first)))
      second)))

(defun gds-user-selected-breakpoint (os)
  "Ask the user to choose one of the given list of breakpoints, and
return the one that they chose."
  (let ((table (mapcar
		(lambda (o)
		  (cons (format "%S"
				(overlay-get o 'gds-breakpoint-definition))
			o))
		os)))
    (cdr (assoc (completing-read "Which breakpoint do you mean? "
				 table nil t)
		table))))

(defun gds-describe-breakpoints ()
  "Describe all breakpoints and their programming status."
  (interactive)
  (with-current-buffer (get-buffer-create "*GDS Breakpoints*")
    (erase-buffer)
    (gds-fold-breakpoints (function gds-describe-breakpoint))
    (display-buffer (current-buffer))))

(defun gds-describe-breakpoint (bpnum bpdef)
  (insert (format "Breakpoint %d: %S\n" bpnum bpdef))
  (let ((bpproglist (cdr (assq bpnum gds-breakpoint-programming))))
    (mapcar (lambda (clientprog)
	      (let ((client (car clientprog))
		    (traplist (cdr clientprog)))
		(mapcar (lambda (trap)
			  (insert (format "  Client %d: %S\n" client trap)))
			traplist)))
	    bpproglist)))

(defun gds-after-save-update-breakpoints ()
  "Function called when a buffer containing breakpoints is saved."
  (if (eq (derived-mode-class major-mode) 'scheme-mode)
      (save-restriction
	(widen)
	;; Get the current breakpoint overlays.
	(let ((os (overlays-in (point-min) (point-max)))
	      (cache (copy-sequence gds-breakpoint-cache)))
	  ;; Identify any overlays that have disappeared by comparing
	  ;; against this buffer's definition cache, and
	  ;; simultaneously rebuild the cache to reflect the current
	  ;; set of overlays.
	  (setq gds-breakpoint-cache nil)
	  (while os
	    (let* ((o (car os))
		   (bpdef (overlay-get o 'gds-breakpoint-definition))
		   (bpnum (overlay-get o 'gds-breakpoint-number)))
	      (if bpdef
		  ;; o and bpdef describe a current breakpoint.
		  (progn
		    ;; Remove this breakpoint from the old cache list,
		    ;; so we don't think it got lost.
		    (setq cache (delq (assq bpdef cache) cache))
		    ;; Check whether this breakpoint's location has
		    ;; moved.  If it has, update the breakpoint
		    ;; definition and the associated client.
		    (let ((lcnow (gds-line-and-column (overlay-start o))))
		      (if (equal lcnow (gds-bpdef:lc bpdef))
			  nil		; Breakpoint hasn't moved.
			(gds-bpdef:setlc bpdef lcnow)
			(if gds-client
			    (gds-send-breakpoint-to-client bpnum bpdef))))
		    ;; Add this breakpoint to the new cache list.
		    (setq gds-breakpoint-cache
			  (cons (list bpdef bpnum) gds-breakpoint-cache)))))
	    (setq os (cdr os)))
	  ;; cache now holds the set of lost breakpoints.  If we are
	  ;; supposed to explicitly delete these from the associated
	  ;; client, do that now.
	  (if (and gds-delete-lost-breakpoints gds-client)
	      (while cache
		(gds-send (format "delete-breakpoint %d" (cadr (car cache)))
			  gds-client)
		(setq cache (cdr cache)))))
	;; If this buffer now has no breakpoints, remove it from
	;; gds-breakpoint-buffers.
	(or gds-breakpoint-cache
	    (setq gds-breakpoint-buffers
		  (delq (current-buffer) gds-breakpoint-buffers)))
	;; Update the persistent breakpoints file.
	(gds-write-breakpoints-file))))

(add-hook 'after-save-hook (function gds-after-save-update-breakpoints))

;;;; Dispatcher for non-debug protocol.

(defun gds-nondebug-protocol (client proc args)
  (cond (;; (eval-results ...) - Results of evaluation.
         (eq proc 'eval-results)
         (gds-display-results client (car args) (cadr args) (cddr args))
         ;; If these results indicate an error, set
         ;; gds-completion-results to non-nil in case the error arose
         ;; when trying to do a completion.
         (if (eq (caar args) 'error)
             (setq gds-completion-results 'error)))

        (;; (completion-result ...) - Available completions.
         (eq proc 'completion-result)
         (setq gds-completion-results (or (car args) t)))

	(;; (breakpoint NUM STATUS) - Breakpoint set.
	 (eq proc 'breakpoint)
	 (let* ((bpnum (car args))
		(traplist (cdr args))
		(bpentry (assq bpnum gds-breakpoint-programming)))
	   (message "Breakpoint %d: %s" bpnum traplist)
	   (if bpentry
	       (let ((cliententry (assq client (cdr bpentry))))
		 (if cliententry
		     (setcdr cliententry traplist)
		   (setcdr bpentry
			   (cons (cons client traplist) (cdr bpentry)))))
	     (setq gds-breakpoint-programming
		   (cons (list bpnum (cons client traplist))
			 gds-breakpoint-programming)))))

	(;; (get-breakpoints) - Set all breakpoints.
	 (eq proc 'get-breakpoints)
	 (let ((gds-client client))
	   (gds-fold-breakpoints (function gds-send-breakpoint-to-client)))
	 (gds-send "continue" client))

	(;; (note ...) - For debugging only.
	 (eq proc 'note))

        (;; (trace ...) - Tracing.
         (eq proc 'trace)
         (with-current-buffer (get-buffer-create "*GDS Trace*")
           (save-excursion
             (goto-char (point-max))
             (or (bolp) (insert "\n"))
             (insert "[client " (number-to-string client) "] " (car args) "\n"))))

        (t
         ;; Unexpected.
         (error "Bad protocol: %S" form))))
  
;;;; Scheme mode keymap items.

(define-key scheme-mode-map "\M-\C-x" 'gds-eval-defun)
(define-key scheme-mode-map "\C-x\C-e" 'gds-eval-last-sexp)
(define-key scheme-mode-map "\C-c\C-e" 'gds-eval-expression)
(define-key scheme-mode-map "\C-c\C-r" 'gds-eval-region)
(define-key scheme-mode-map "\C-hg" 'gds-help-symbol)
(define-key scheme-mode-map "\C-h\C-g" 'gds-apropos)
(define-key scheme-mode-map "\e\t" 'gds-complete-symbol)
(define-key scheme-mode-map "\C-x " 'gds-set-breakpoint)

(define-prefix-command 'gds-breakpoint-map)
(define-key scheme-mode-map "\C-c\C-b" 'gds-breakpoint-map)
(define-key gds-breakpoint-map " " 'gds-set-breakpoint)
(define-key gds-breakpoint-map "d"
  (function (lambda ()
              (interactive)
              (let ((gds-default-breakpoint-type 'debug))
                (gds-set-breakpoint)))))
(define-key gds-breakpoint-map "t"
  (function (lambda ()
              (interactive)
              (let ((gds-default-breakpoint-type 'trace))
                (gds-set-breakpoint)))))
(define-key gds-breakpoint-map "T"
  (function (lambda ()
              (interactive)
              (let ((gds-default-breakpoint-type 'trace-subtree))
                (gds-set-breakpoint)))))
(define-key gds-breakpoint-map [backspace] 'gds-delete-breakpoints)
(define-key gds-breakpoint-map "?" 'gds-describe-breakpoints)

;;;; The end!

(provide 'gds-scheme)

;;; gds-scheme.el ends here.
