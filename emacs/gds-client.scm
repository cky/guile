;;;; Guile Debugger UI client

;;; Copyright (C) 2003 Free Software Foundation, Inc.
;;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 2.1 of the License, or (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(define-module (emacs gds-client)
  #:use-module (ice-9 debugger)
  #:use-module (ice-9 debugger behaviour)
  #:use-module (ice-9 debugger breakpoints)
  #:use-module (ice-9 debugger breakpoints procedural)
  #:use-module (ice-9 debugger breakpoints source)
  #:use-module (ice-9 debugger state)
  #:use-module (ice-9 debugger utils)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 session)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 threads)
  #:export (gds-port-number
	    gds-connected?
	    gds-connect
	    gds-command-loop
	    gds-server-died-hook)
  #:no-backtrace)

;; The TCP port number that the UI server listens for application
;; connections on.
(define gds-port-number 8333)

;; Once connected, the TCP socket port to the UI server.
(define gds-port #f)

(define* (gds-connect name debug #:optional host)
  "Connect to the debug UI server as @var{name}, a string that should
be sufficient to describe the calling application to the debug UI
user.  The optional @var{host} arg specifies the hostname or dotted
decimal IP address where the UI server is running; default is
127.0.0.1."
  (if (gds-connected?)
      (error "Already connected to UI server!"))
  ;; Connect to debug server.
  (set! gds-port
	(let ((s (socket PF_INET SOCK_STREAM 0))
	      (SOL_TCP 6)
	      (TCP_NODELAY 1))
	  (setsockopt s SOL_TCP TCP_NODELAY 1)
	  (connect s AF_INET (inet-aton (or host "127.0.0.1")) gds-port-number)
	  s))
  ;; Set debugger-output-port so that stuff written to it is
  ;; accumulated for sending to the debug server.
  (set! (debugger-output-port)
	(make-soft-port (vector accumulate-output
				accumulate-output
				#f #f #f #f)
			"w"))
  ;; Write initial context to debug server.
  (write-form (list 'name name (getpid)))
  ;(write-form (cons 'modules (map module-name (loaded-modules))))
  ;; Start the asynchronous UI thread.
  (start-async-gds-thread)
  ;; If `debug' is true, debug immediately.
  (if debug
      (debug-stack (make-stack #t gds-connect) #:continuable))
;  (gds-command-loop #f)
  )

(define gds-disable-async-thread noop)
(define gds-continue-async-thread noop)
(define async-gds-thread #f)

(define (start-async-gds-thread)
  (let ((mutex (make-mutex))
	(condition (make-condition-variable))
	(admin (pipe)))
    ;; Start the asynchronous UI thread.
    (begin-thread
      (set! async-gds-thread (current-thread))
      ;;(write (cons admin gds-port))
      ;;(newline)
      (lock-mutex mutex)
      (catch 'server-died
	     (lambda ()
	       (let loop ((avail '()))
		 (write-note 'startloop)
		 ;;(write avail)
		 ;;(newline)
		 (cond ((not gds-port))	; exit loop
		       ((null? avail)
			(write-status 'ready-for-input)
			(unlock-mutex mutex)
			(let ((ports (car (select (list gds-port (car admin))
						  '() '()))))
			  (lock-mutex mutex)
			  (loop ports)))
		       (else
			(write-note 'sthg-to-read)
			(let ((port (car avail)))
			  (if (eq? port gds-port)
			      (handle-instruction #f (read gds-port))
			      (begin
				(write-note 'debugger-takeover)
				;; Notification from debugger that it
				;; wants to take over.  Read the
				;; notification char.
				(read-char (car admin))
				;; Wait on condition variable - this allows the
				;; debugger thread to grab the mutex.
				(write-note 'cond-wait)
				(signal-condition-variable condition)
				(wait-condition-variable condition mutex)
				))
			  ;; Loop.
			  (loop '()))))
		 (write-note 'loopexited)))
	     (lambda args #f))
      (set! gds-disable-async-thread noop)
      (set! gds-continue-async-thread noop)
      (set! async-gds-thread #f)
      (unlock-mutex mutex))
    ;; Redefine procs used by debugger thread to take control.
    (set! gds-disable-async-thread
	  (lambda ()
	    (lock-mutex mutex)
	    (write-char #\x (cdr admin))
	    (force-output (cdr admin))
	    (write-note 'char-written)
	    (wait-condition-variable condition mutex)
	    ;;(display "gds-disable-async-thread: locking mutex...\n"
	    ;;	     (current-error-port))
	    ))
    (set! gds-continue-async-thread
	  (lambda ()
	    (write-note 'cond-signal)
	    (signal-condition-variable condition)
	    ;; Make sure that the async thread has got the message
	    ;; before we could possibly try to grab the main mutex
	    ;; again.
	    (unlock-mutex mutex)))))

(define accumulated-output '())

(define (accumulate-output obj)
  (set! accumulated-output
	(cons (if (string? obj) obj (make-string 1 obj))
	      accumulated-output)))

(define (get-accumulated-output)
  (let ((s (apply string-append (reverse! accumulated-output))))
    (set! accumulated-output '())
    s))

(define (gds-connected?)
  "Return @code{#t} if a UI server connected has been made; else @code{#f}."
  (not (not gds-port)))

(define (gds-command-loop state)
  "Interact with the UI frontend."
  (or (gds-connected?)
      (error "Not connected to UI server."))
  (gds-disable-async-thread)
  (catch #t	; Only expect here 'exit-debugger or 'server-died.
	 (lambda ()
	   (let loop ((state state))
	     ;; Write accumulated debugger output.
	     (write-form (list 'output 
			       (sans-surrounding-whitespace
				(get-accumulated-output))))
	     ;; Write current state to the frontend.
	     (if state (write-stack state))
	     ;; Tell the frontend that we're waiting for input.
	     (write-status 'waiting-for-input)
	     ;; Read next instruction, act on it, and loop with
	     ;; updated state.
	     (loop (handle-instruction state (read gds-port)))))
	 (lambda args *unspecified*))
  (gds-continue-async-thread))

(define (write-stack state)
  ;; Write Emacs-readable representation of current state to UI
  ;; frontend.
  (let ((frames (stack->emacs-readable (state-stack state)))
	(index (index->emacs-readable (state-index state)))
	(flags (flags->emacs-readable (state-flags state))))
    (if (memq 'backwards (debug-options))
	(write-form (list 'stack
			  frames
			  index
			  flags))
	;; Calculate (length frames) here because `reverse!' will make
	;; the original `frames' invalid.
	(let ((nframes (length frames)))
	  (write-form (list 'stack
			    (reverse! frames)
			    (- nframes index 1)
			    flags))))))

(define (write-form form)
  ;; Write any form FORM to UI frontend.
  (write form gds-port)
  (newline gds-port)
  (force-output gds-port))

(define (write-note note)
  ;; Write a note (for debugging this code) to UI frontend.
  (false-if-exception (write-form `(note ,note))))

(define (stack->emacs-readable stack)
  ;; Return Emacs-readable representation of STACK.
  (map (lambda (index)
	 (frame->emacs-readable (stack-ref stack index)))
       (iota (stack-length stack))))

(define (frame->emacs-readable frame)
  ;; Return Emacs-readable representation of FRAME.
  (if (frame-procedure? frame)
      (list 'application
	    (with-output-to-string
	     (lambda ()
	       (display (if (frame-real? frame) "  " "t "))
	       (write-frame-short/application frame)))
	    (source->emacs-readable (or (frame-source frame)
					(let ((proc (frame-procedure frame)))
					  (and proc
					       (procedure-source proc))))))
      (list 'evaluation
	    (with-output-to-string
	     (lambda ()
	       (display (if (frame-real? frame) "  " "t "))
	       (write-frame-short/expression frame)))
	    (source->emacs-readable (frame-source frame)))))

(define (source->emacs-readable source)
  ;; Return Emacs-readable representation of the filename, line and
  ;; column source properties of SOURCE.
  (if (and source
	   (string? (source-property source 'filename)))
      (list (source-property source 'filename)
	    (source-property source 'line)
	    (source-property source 'column))
      'nil))

(define (index->emacs-readable index)
  ;; Return Emacs-readable representation of INDEX (the current stack
  ;; index).
  index)

(define (flags->emacs-readable flags)
  ;; Return Emacs-readable representation of FLAGS passed to
  ;; debug-stack.
  (map (lambda (flag)
	 (if (keyword? flag)
	     (keyword->symbol flag)
	     (format #f "~S" flag)))
       flags))

(define the-ice-9-debugger-commands-module
  (resolve-module '(ice-9 debugger commands)))

(define internal-error-stack #f)

(define (handle-instruction state ins)
  (if (eof-object? ins)
      (server-died)
      (catch #t
	     (lambda ()
	       (lazy-catch #t
			   (lambda ()
			     (handle-instruction-1 state ins))
			   (lambda (key . args)
			     (set! internal-error-stack (make-stack #t))
			     (apply throw key args))))
	     (lambda (key . args)
	       (case key
		 ((exit-debugger)
		  (apply throw key args))
		 (else
		  (write-form
		   `(eval-results "GDS Internal Error\n"
				  ,(list (with-output-to-string
					  (lambda ()
					    (write key)
					    (display ": ")
					    (write args)
					    (newline)
					    (display-backtrace internal-error-stack
							       (current-output-port)))))))))
	       state))))

(define (server-died)
  (get-accumulated-output)
  (close-port gds-port)
  (set! gds-port #f)
  (run-hook gds-server-died-hook)
  (throw 'server-died))

(define gds-server-died-hook (make-hook))

(define (handle-instruction-1 state ins)
  ;; Read the newline that always follows an instruction.
  (read-char gds-port)
  ;; Handle instruction from the UI frontend, and return updated state.
  (case (car ins)
    ((query-modules)
     (write-form (cons 'modules (map module-name (loaded-modules))))
     state)
    ((query-module)
     (let ((name (cadr ins)))
       (write-form `(module ,name
			    ,(or (loaded-module-source name) "(no source file)")
			    ,@(sort (module-map (lambda (key value)
						  (symbol->string key))
						(resolve-module name))
				    string<?))))
     state)
    ((debugger-command)
     (write-status 'running)
     (let ((name (cadr ins))
	   (args (cddr ins)))
       (let ((proc (module-ref the-ice-9-debugger-commands-module name)))
	 (if proc
	     (apply proc state args)
	     (throw 'internal-error proc name args))))
     state)
    ((set-breakpoint)
     (set-breakpoint! (case (cadddr ins)
			((debug-here) debug-here)
			((trace-here) trace-here)
			((trace-subtree) trace-subtree)
			(else
			 (lambda ()
			   (display "Don't know `")
			   (display (cadddr ins))
			   (display "' behaviour; doing `debug-here' instead.\n")
			   (debug-here))))
		      (module-ref (resolve-module (cadr ins)) (caddr ins)))
     state)
    ((eval)
     (apply (lambda (module port-name line column bpinfo code)
	      (with-input-from-string code
	        (lambda ()
		  (set-port-filename! (current-input-port) port-name)
		  (set-port-line! (current-input-port) line)
		  (set-port-column! (current-input-port) column)
		  (let ((m (and module (resolve-module module))))
		    (let loop ((results '()) (x (read)))
		      (if (eof-object? x)
			  (write-form `(eval-results ,@results))
			  (loop (append results (gds-eval x bpinfo m))
				(read))))))))
	    (cdr ins))
     state)
    ((complete)
     (let ((matches (apropos-internal
		     (string-append "^" (regexp-quote (cadr ins))))))
       (cond ((null? matches)
	      (write-form '(completion-result nil)))
	     (else
	      ;;(write matches (current-error-port))
	      ;;(newline (current-error-port))
	      (let ((match
		     (let loop ((match (symbol->string (car matches)))
				(matches (cdr matches)))
		       ;;(write match (current-error-port))
		       ;;(newline (current-error-port))
		       ;;(write matches (current-error-port))
		       ;;(newline (current-error-port))
		       (if (null? matches)
			   match
			   (if (string-prefix=? match
						(symbol->string (car matches)))
			       (loop match (cdr matches))
			       (loop (substring match 0
						(- (string-length match) 1))
				     matches))))))
		(if (string=? match (cadr ins))
		    (write-form `(completion-result
				  ,(map symbol->string matches)))
		    (write-form `(completion-result
				  ,match)))))))
     state)
    ((async-break)
     (let ((thread (car (delq async-gds-thread (all-threads)))))
       (write (cons 'target-thread thread))
       (newline)
       (write (cons 'async-thread async-gds-thread))
       (newline)
       (system-async-mark (lambda ()
			    (debug-stack (make-stack #t 3) #:continuable))
			  thread))
     state)
    (else state)))

(define (install-breakpoints x bpinfo)
  (define (install-recursive x)
    (if (list? x)
	(begin
	  ;; Check source properties of x itself.
	  (let* ((infokey (cons (source-property x 'line)
				(source-property x 'column)))
		 (bpentry (assoc infokey bpinfo)))
	    (if bpentry
		(let ((bp (set-breakpoint! debug-here x x)))
		  ;; FIXME: Here should transfer properties from the
		  ;; old breakpoint with index (cdr bpentry) to the
		  ;; new breakpoint.  (Or else provide an alternative
		  ;; to set-breakpoint! that reuses the same
		  ;; breakpoint.)
		  (write-form (list 'breakpoint-set
				    (source-property x 'filename)
				    (car infokey)
				    (cdr infokey)
				    (bp-number bp))))))
	  ;; Check each of x's elements.
	  (for-each install-recursive x))))
  (install-recursive x))

(define (gds-eval x bpinfo m)
  ;; Consumer to accept possibly multiple values and present them for
  ;; Emacs as a list of strings.
  (define (value-consumer . values)
    (if (unspecified? (car values))
	'()
	(map (lambda (value)
	       (with-output-to-string (lambda () (write value))))
	     values)))
  ;; Before evaluation, set breakpoints in the read code as specified
  ;; by bpinfo.
  (install-breakpoints x bpinfo)
  ;; Now do evaluation.
  (let ((value #f))
    (let* ((do-eval (if m
			(lambda ()
			  (display "Evaluating in module ")
			  (write (module-name m))
			  (newline)
			  (set! value
				(call-with-values (lambda ()
						    (eval x m))
				  value-consumer)))
			(lambda ()
			  (display "Evaluating in current module ")
			  (write (module-name (current-module)))
			  (newline)
			  (set! value
				(call-with-values (lambda ()
						    (primitive-eval x))
				  value-consumer)))))
	   (output
	    (with-output-to-string
	     (lambda ()
	       (catch #t
		      do-eval
		      (lambda (key . args)
			(case key
			  ((misc-error signal unbound-variable
			    numerical-overflow)
			   (apply display-error #f
				  (current-output-port) args)
			   (set! value '("error-in-evaluation")))
			  (else
			   (display "EXCEPTION: ")
			   (display key)
			   (display " ")
			   (write args)
			   (newline)
			   (set! value
				 '("unhandled-exception-in-evaluation"))))))))))
      (list output value))))

(define (write-status status)
  (write-form (list 'current-module
		    (format #f "~S" (module-name (current-module)))))
  (write-form (list 'status status)))

(define (loaded-module-source module-name)
  ;; Return the file name that (ice-9 boot-9) probably loaded the
  ;; named module from.  (The `probably' is because `%load-path' might
  ;; have changed since the module was loaded.)
  (let* ((reverse-name (reverse module-name))
	 (name (symbol->string (car reverse-name)))
	 (dir-hint-module-name (reverse (cdr reverse-name)))
	 (dir-hint (apply string-append
			  (map (lambda (elt)
				 (string-append (symbol->string elt) "/"))
			       dir-hint-module-name))))
    (%search-load-path (in-vicinity dir-hint name))))

(define (loaded-modules)
  ;; Return list of all loaded modules sorted by name.
  (sort (apropos-fold-all (lambda (module acc) (cons module acc)) '())
	(lambda (m1 m2)
	  (symlist<? (module-name m1) (module-name m2)))))

(define (symlist<? l1 l2)
  ;; Return #t if symbol list L1 is alphabetically less than L2.
  (cond ((null? l1) #t)
	((null? l2) #f)
	((eq? (car l1) (car l2)) (symlist<? (cdr l1) (cdr l2)))
	(else (string<? (symbol->string (car l1)) (symbol->string (car l2))))))

;;; (emacs gds-client) ends here.
