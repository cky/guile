;;;; Guile Debugger UI client

;;; Copyright (C) 2003, 2004 Free Software Foundation, Inc.
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
  #:use-module (ice-9 debugger trap-hooks)
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


;;;; {Internal Tracing and Debugging}

;; Some of this module's thread and mutex code is quite tricky and
;; includes `trc' statements to trace out useful information if the
;; environment variable GDS_TRC is defined.
(define trc
  (if (getenv "GDS_TRC")
      (let ((port (open-output-file "/home/neil/gds-client.log"))
	    (trc-mutex (make-mutex)))
	(lambda args
	  (with-mutex trc-mutex
		      (write args port)
		      (newline port)
		      (force-output port))))
      noop))

(define-macro (assert expr)
  `(or ,expr
       (error "Assertion failed" expr)))


;;;; {TCP Connection}

;; Communication between this module (running in the application being
;; debugged) and the GDS server and UI code (running in/under Emacs)
;; is through a TCP connection.  `gds-port-number' is the TCP port
;; number where the server listens for application connections.
(define gds-port-number 8333)

;; Once connected, the TCP socket port to the server.
(define gds-port #f)

;; Public procedure to discover whether there is a GDS connection yet.
(define (gds-connected?)
  "Return @code{#t} if a UI server connected has been made; else @code{#f}."
  (not (not gds-port)))

;; Public procedure to create the connection to the GDS server.
(define* (gds-connect name #:optional host)
  "Connect to the GDS server as @var{name}, a string that should be
sufficient to describe the calling application to the GDS frontend
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
  ;; Set debugger-output-port so that messages written to it are not
  ;; displayed on the application's stdout, but instead accumulated
  ;; for sending to the GDS frontend.
  (set! (debugger-output-port)
	(make-soft-port (vector accumulate-output
				accumulate-output
				#f #f #f #f)
			"w"))
  ;; Announce ourselves to the server.
  (write-form (list 'name name (getpid)))
  (add-trapped-stack-id! 'gds-eval-stack)
  ;; Start the UI read thread.
  (set! ui-read-thread (make-thread ui-read-thread-proc)))

(define accumulated-output '())

(define (accumulate-output obj)
  (set! accumulated-output
	(cons (if (string? obj) obj (make-string 1 obj))
	      accumulated-output)))

(define (get-accumulated-output)
  (let ((s (apply string-append (reverse! accumulated-output))))
    (set! accumulated-output '())
    s))


;;;; {UI Read Thread}

;; Except when the application enters the debugger, communication with
;; the GDS server and frontend is managed by a dedicated thread for
;; this purpose.  This design avoids having to modify application code
;; at the expense of requiring a Guile with threads support.
(define (ui-read-thread-proc)
  (write-status 'running)
  (let ((eval-thread-needed? #t))
    ;; Start up the default eval thread.
    (make-thread eval-thread 1 (lambda () (not eval-thread-needed?)))
    (with-mutex ui-read-mutex
      (catch 'server-died
	;; Protected thunk: loop reading either protocol input from
	;; the server, or an indication (through ui-read-switch-pipe)
	;; that a thread in the debugger wants to take over the
	;; interaction with the server.
	(lambda ()
	  (let loop ((avail '()))
	    (write-note 'startloop)
	    (cond ((not gds-port))	; exit loop
		  ((null? avail)
		   (write-status 'ready-for-input)
		   (loop (without-mutex ui-read-mutex
			   (car (select (list gds-port
					      (car ui-read-switch-pipe))
					'() '())))))
		  (else
		   (write-note 'sthg-to-read)
		   (let ((port (car avail)))
		     (if (eq? port gds-port)
			 (handle-instruction #f (read gds-port))
			 (begin
			   (write-note 'debugger-takeover)
			   ;; Notification from debugger that it wants
			   ;; to take over.  Read the notification
			   ;; char.
			   (read-char (car ui-read-switch-pipe))
			   ;; Wait on ui-read-switch variable - this
			   ;; allows the debugger thread to grab the
			   ;; mutex.
			   (write-note 'cond-wait)
			   (signal-condition-variable ui-read-switch)
			   (wait-condition-variable ui-read-switch
						    ui-read-mutex)))
		     ;; Loop.
		     (loop '()))))
	    (write-note 'loopexited)))
	;; Catch handler.
	(lambda args #f)))
    ;; Tell the eval thread that it can exit.
    (with-mutex eval-work-mutex
      (set! eval-thread-needed? #f)
      (broadcast-condition-variable eval-work-changed))))

;; It's useful to keep a note of the UI thread's id.
(define ui-read-thread #f)

;; Mutex used to control which thread is currently reading the TCP
;; connection to the server/UI.
(define ui-read-mutex (make-mutex))

;; Condition variable used by threads interested in reading the TCP
;; connection to signal changes in their state.
(define ui-read-switch (make-condition-variable))

;; Pipe used by application threads that enter the debugger to tell
;; the UI read thread that they'd like to take over reading the TCP
;; connection.
(define ui-read-switch-pipe (pipe))


;;;; {Debugger Integration}

;; When a thread enters the Guile debugger and a GDS connection is
;; present, the debugger calls `gds-command-loop' instead of entering
;; its usual command loop.
(define (gds-command-loop state)
  "Interact with the UI frontend."
  (or (gds-connected?)
      (error "Not connected to UI server."))
  ;; Take over server/UI interaction from the normal UI read thread.
  (with-mutex ui-read-mutex
    (write-char #\x (cdr ui-read-switch-pipe))
    (force-output (cdr ui-read-switch-pipe))
    (write-note 'char-written)
    (wait-condition-variable ui-read-switch ui-read-mutex)
    ;; We now "have the com", as they say on Star Trek.
    (catch #t	; Only expect here 'exit-debugger or 'server-died.
      (lambda ()
	(let loop ((state state))
	  ;; Write accumulated debugger output.
	  (write-form (list 'output (sans-surrounding-whitespace
				     (get-accumulated-output))))
	  ;; Write current state to the frontend.
	  (if state (write-stack state))
	  ;; Tell the frontend that we're waiting for input.
	  (write-status 'waiting-for-input)
	  ;; Read next instruction, act on it, and loop with updated
	  ;; state.
	  (loop (handle-instruction state (read gds-port)))))
      (lambda args *unspecified*))
    (write-note 'cond-signal)
    ;; Tell the UI read thread that it can take control again.
    (signal-condition-variable ui-read-switch)))


;;;; {General Output to Server/UI}

(define write-form
  (let ((protocol-mutex (make-mutex)))
    (lambda (form)
      ;; Write any form FORM to UI frontend.
      (with-mutex protocol-mutex
        (write form gds-port)
	(newline gds-port)
	(force-output gds-port)))))

(define (write-note note)
  ;; Write a note (for debugging this code) to UI frontend.
  (false-if-exception (write-form `(note ,note))))

(define (write-status status)
  (write-form (list 'current-module
		    (format #f "~S" (module-name (current-module)))))
  (write-form (list 'status status)))


;;;; {Stack Output to Server/UI}

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

(define (stack->emacs-readable stack)
  ;; Return Emacs-readable representation of STACK.
  (map (lambda (index)
	 (frame->emacs-readable (stack-ref stack index)))
       (iota (min (stack-length stack)
		  (cadr (memq 'depth (debug-options)))))))

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


;;;; {Handling GDS Protocol Instructions}

;; Instructions from the server/UI always come through here.  If
;; `state' is non-#f, we are in the debugger; otherwise, not.
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
		   `(eval-results (error . "")
				  "GDS Internal Error\n"
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

(define internal-error-stack #f)

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
						(resolve-module-from-root name))
				    string<?))))
     state)
    ((debugger-command)
     (or state (error "Not currently in debugger!"))
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
		      (module-ref (resolve-module-from-root (cadr ins)) (caddr ins)))
     state)
    ((eval)
     (apply (lambda (correlator module port-name line column bpinfo code)
	      (with-input-from-string code
	        (lambda ()
		  (set-port-filename! (current-input-port) port-name)
		  (set-port-line! (current-input-port) line)
		  (set-port-column! (current-input-port) column)
		  (let ((m (and module (resolve-module-from-root module))))
		    (let loop ((exprs '()) (x (read)))
		      (if (eof-object? x)
			  ;; Expressions to be evaluated have all been
			  ;; read.  Now hand them off to an
			  ;; eval-thread for the actual evaluation.
			  (with-mutex eval-work-mutex
			    (trc 'protocol-thread "evaluation work available")
			    (set! eval-work (cons* correlator m (reverse! exprs)))
			    (set! eval-work-available #t)
			    (broadcast-condition-variable eval-work-changed)
			    (wait-condition-variable eval-work-taken
						     eval-work-mutex)
			    (assert (not eval-work-available))
			    (trc 'protocol-thread "evaluation work underway"))
			  ;; Another complete expression read.  Set
			  ;; breakpoints in the read code as specified
			  ;; by bpinfo, and add it to the list.
			  (begin
			    (install-breakpoints x bpinfo)
			    (loop (cons x exprs) (read)))))))))
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
     (let ((thread (car (delq ui-read-thread (all-threads)))))
       (write (cons 'target-thread thread))
       (newline)
       (write (cons 'ui-read-thread ui-read-thread))
       (newline)
       (system-async-mark (lambda ()
			    (debug-stack (make-stack #t 3) #:continuable))
			  thread))
     state)
    ((interrupt-eval)
     (let ((thread (hash-ref eval-thread-table (cadr ins))))
       (system-async-mark (lambda ()
			    (debug-stack (make-stack #t 3) #:continuable))
			  thread))
     state)
    (else state)))

(define the-ice-9-debugger-commands-module
  (resolve-module '(ice-9 debugger commands)))

(define (resolve-module-from-root name)
  (save-module-excursion
   (lambda ()
     (set-current-module the-root-module)
     (resolve-module name))))


;;;; {Module Browsing}

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


;;;; {Source Breakpoint Installation}

(define (install-breakpoints x bpinfo)
  (define (install-recursive x)
    (if (and (list? x) (not (null? x)))
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


;;;; {Evaluation}

;; Evaluation threads are unleashed by two possible triggers.  One is
;; a boolean variable, specific to each thread, that tells the thread
;; to exit when set to #t.  The other is another boolean variable, but
;; global, indicating that there is an evaluation to perform:
(define eval-work-available #f)

;; This variable, which is only valid when `eval-work-available' is
;; #t, holds the evaluation to perform:
(define eval-work #f)

;; A mutex protects against concurrent access to these variables.
(define eval-work-mutex (make-mutex))

;; Changes in these variables are signaled by broadcasting the
;; following condition variable.
(define eval-work-changed (make-condition-variable))

;; When an evaluation thread takes some work, it tells the main GDS
;; thread by signaling this condition variable.
(define eval-work-taken (make-condition-variable))

(define-macro (without-mutex m . body)
  `(dynamic-wind
       (lambda () (unlock-mutex ,m))
       (lambda () (begin ,@body))
       (lambda () (lock-mutex ,m))))

(define next-thread-number
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))

(define eval-thread-table (make-hash-table 3))

(define (eval-thread depth thread-should-exit-thunk)
  ;; Acquire mutex to check trigger variables.
  (with-mutex eval-work-mutex
    (let ((thread-number (next-thread-number)))
      ;; Add this thread to global hash, so we can correlate back to
      ;; this thread from the ID used by the GDS front end.
      (hash-set! eval-thread-table thread-number (current-thread))
      (trc 'eval-thread depth thread-number "entering loop")
      (let loop ()
	;; Tell the front end this thread is ready.
	(write-form `(thread-status eval ,thread-number ready))
	(cond ((thread-should-exit-thunk)
	       ;; Allow thread to exit.
	       )

	      (eval-work-available
	       ;; Take a local copy of the work, reset global
	       ;; variables, then do the work with mutex released.
	       (trc 'eval-thread depth thread-number "starting work")
	       (let* ((work eval-work)
		      (subthread-needed? #t)
		      (correlator (car work)))
		 ;; Tell the front end this thread is busy.
		 (write-form `(thread-status eval ,thread-number busy ,correlator))
		 (set! eval-work-available #f)
		 (signal-condition-variable eval-work-taken)
		 (without-mutex eval-work-mutex
    	           ;; Before starting evaluation, create another eval
		   ;; thread like this one, so that it can take over
		   ;; if another evaluation is requested before this
		   ;; one is finished.
		   (make-thread eval-thread (+ depth 1)
				(lambda () (not subthread-needed?)))
		   ;; Do the evaluation(s).
		   (let loop2 ((m (cadr work))
			       (exprs (cddr work))
			       (results '())
			       (n 1))
		     (if (null? exprs)
			 (write-form `(eval-results ,correlator ,@results))
			 (loop2 m
				(cdr exprs)
				(append results (gds-eval (car exprs) m
							  (if (and (null? (cdr exprs))
								   (= n 1))
							      #f n)))
				(+ n 1)))))
		 (trc 'eval-thread depth thread-number "work done")
		 ;; Tell the subthread that it should now exit.
		 (set! subthread-needed? #f)
		 (broadcast-condition-variable eval-work-changed)
		 ;; Loop for more work for this thread.
		 (loop)))

	      (else
	       ;; Wait for something to change, then loop to check
	       ;; trigger variables again.
	       (trc 'eval-thread depth thread-number "wait")
	       (wait-condition-variable eval-work-changed eval-work-mutex)
	       (trc 'eval-thread depth thread-number "wait done")
	       (loop))))
      (trc 'eval-thread depth thread-number "exiting")
      ;; Tell the front end this thread is ready.
      (write-form `(thread-status eval ,thread-number exiting)))))

(define (gds-eval x m part)
  ;; Consumer to accept possibly multiple values and present them for
  ;; Emacs as a list of strings.
  (define (value-consumer . values)
    (if (unspecified? (car values))
	'()
	(map (lambda (value)
	       (with-output-to-string (lambda () (write value))))
	     values)))
  ;; Now do evaluation.
  (let ((intro (if part
		   (format #f ";;; Evaluating subexpression ~A" part)
		   ";;; Evaluating"))
	(value #f))
    (let* ((do-eval (if m
			(lambda ()
			  (display intro)
			  (display " in module ")
			  (write (module-name m))
			  (newline)
			  (set! value
				(call-with-values (lambda ()
						    (start-stack 'gds-eval-stack
								 (eval x m)))
				  value-consumer)))
			(lambda ()
			  (display intro)
			  (display " in current module ")
			  (write (module-name (current-module)))
			  (newline)
			  (set! value
				(call-with-values (lambda ()
						    (start-stack 'gds-eval-stack
								 (primitive-eval x)))
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


;;; (emacs gds-client) ends here.
