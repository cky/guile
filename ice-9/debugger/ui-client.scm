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

(define-module (ice-9 debugger ui-client)
  #:use-module (ice-9 debugger)
  #:use-module (ice-9 debugger behaviour)
  #:use-module (ice-9 debugger breakpoints)
  #:use-module (ice-9 debugger breakpoints procedural)
  #:use-module (ice-9 debugger state)
  #:use-module (ice-9 debugger utils)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 session)
  #:use-module (ice-9 string-fun)
  #:export (ui-port-number
	    ui-connected?
	    ui-connect
	    ui-command-loop)
  #:no-backtrace)

;; The TCP port number that the UI server listens for application
;; connections on.
(define ui-port-number 8333)

;; Once connected, the TCP socket port to the UI server.
(define ui-port #f)

(define* (ui-connect name #:optional host)
  "Connect to the debug UI server as @var{name}, a string that should
be sufficient to describe the calling application to the debug UI
user.  The optional @var{host} arg specifies the hostname or dotted
decimal IP address where the UI server is running; default is
127.0.0.1."
  (if (ui-connected?)
      (error "Already connected to UI server!"))
  ;; Connect to debug server.
  (set! ui-port
	(let ((s (socket PF_INET SOCK_STREAM 0))
	      (SOL_TCP 6)
	      (TCP_NODELAY 1))
	  (setsockopt s SOL_TCP TCP_NODELAY 1)
	  (connect s AF_INET (inet-aton (or host "127.0.0.1")) ui-port-number)
	  s))
  ;; Set debugger-output-port so that stuff written to it is
  ;; accumulated for sending to the debug server.
  (set! (debugger-output-port)
	(make-soft-port (vector accumulate-output
				accumulate-output
				#f #f #f #f)
			"w"))
  ;; Write initial context to debug server.
  (write-form (list 'name name))
  (write-form (cons 'modules (map module-name (loaded-modules))))
  (debug-stack (make-stack #t ui-connect) #:continuable)
;  (ui-command-loop #f)
  )

(define accumulated-output '())

(define (accumulate-output obj)
  (set! accumulated-output
	(cons (if (string? obj) obj (make-string 1 obj))
	      accumulated-output)))

(define (get-accumulated-output)
  (let ((s (apply string-append (reverse! accumulated-output))))
    (set! accumulated-output '())
    s))

(define (ui-connected?)
  "Return @code{#t} if a UI server connected has been made; else @code{#f}."
  (not (not ui-port)))

(define (ui-command-loop state)
  "Interact with the UI frontend."
  (or (ui-connected?)
      (error "Not connected to UI server."))
  (catch 'exit-debugger
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
	     (loop (handle-instruction state (read ui-port)))))
	 (lambda args *unspecified*)))

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
  (write form ui-port)
  (newline ui-port)
  (force-output ui-port))

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
	       (display (if (frame-real? frame) "  " "T "))
	       (write-frame-short/application frame)))
	    (source->emacs-readable (frame-source frame)))
      (list 'evaluation
	    (with-output-to-string
	     (lambda ()
	       (display (if (frame-real? frame) "  " "T "))
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
  (map keyword->symbol flags))

(define the-ice-9-debugger-commands-module
  (resolve-module '(ice-9 debugger commands)))

(define (handle-instruction state ins)
  ;; Handle instruction from the UI frontend, and return updated state.
  (case (car ins)
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
       (apply (module-ref the-ice-9-debugger-commands-module name)
	      state
	      args)
       state))
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
    (else state)))

(define (write-status status)
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

;;; (ice-9 debugger ui-client) ends here.
