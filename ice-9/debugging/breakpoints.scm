;;;; (ice-9 debugging breakpoints) -- practical breakpoints

;;; Copyright (C) 2005 Neil Jerram
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

;;; This module provides a practical interface for setting and
;;; manipulating breakpoints.

(define-module (ice-9 debugging breakpoints)
  #:use-module (ice-9 debugger)
  #:use-module (ice-9 ls)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 regex)
  #:use-module (oop goops)
  #:use-module (ice-9 debugging ice-9-debugger-extensions)
  #:use-module (ice-9 debugging traps)
  #:use-module (ice-9 debugging trc)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (break-in
	    break-at
	    default-breakpoint-behaviour
	    delete-breakpoint
	    for-each-breakpoint
	    setup-before-load
	    setup-after-load
	    setup-after-read
	    setup-after-eval))

;; If the running Guile does not provide before- and after- load hooks
;; itself, install them using the (ice-9 debugging load-hooks) module.
(or (defined? 'after-load-hook)
    (begin
      (use-modules (ice-9 debugging load-hooks))
      (install-load-hooks)))

;; Getter/setter for default breakpoint behaviour.
(define default-breakpoint-behaviour
  (let ((behaviour debug-trap))
    (make-procedure-with-setter
     ;; Getter: return current default behaviour.
     (lambda ()
       behaviour)
     ;; Setter: set default behaviour to given procedure.
     (lambda (new-behaviour)
       (set! behaviour new-behaviour)))))

;; Base class for breakpoints.  (We don't need to use GOOPS to
;; represent breakpoints, but it's a nice way to describe a composite
;; object.)
(define-class <breakpoint> ()
  ;; This breakpoint's trap options, which include its behaviour.
  (trap-options #:init-keyword #:trap-options)
  ;; All the traps relating to this breakpoint.
  (traps #:init-value '())
  ;; Observer.  This is a procedure that is called when the breakpoint
  ;; trap list changes.
  (observer #:init-value #f))

;; Noop base class definitions of all the possible setup methods.
(define-method (setup-before-load (bp <breakpoint>) filename)
  *unspecified*)
(define-method (setup-after-load (bp <breakpoint>) filename)
  *unspecified*)
(define-method (setup-after-read (bp <breakpoint>) x)
  *unspecified*)
(define-method (setup-after-eval (bp <breakpoint>) filename)
  *unspecified*)

;; Call the breakpoint's observer, if it has one.
(define-method (call-observer (bp <breakpoint>))
  (cond ((slot-ref bp 'observer)
	 =>
	 (lambda (proc)
	   (proc)))))

;; Delete a breakpoint.
(define (delete-breakpoint bp)
  ;; Remove this breakpoint from the global list.
  (set! breakpoints (delq! bp breakpoints))
  ;; Uninstall and discard all its traps.
  (for-each uninstall-trap (slot-ref bp 'traps))
  (slot-set! bp 'traps '()))

;; Class for `break-in' breakpoints.
(define-class <break-in> (<breakpoint>)
  ;; The name of the procedure to break in.
  (procedure-name #:init-keyword #:procedure-name)
  ;; The name of the module or file that the procedure is defined in.
  ;; A module name is a list of symbols that exactly names the
  ;; relevant module.  A file name is a string, which can in fact be
  ;; any substring of the relevant full file name.
  (module-or-file-name #:init-keyword #:module-or-file-name))

;; Class for `break-at' breakpoints.
(define-class <break-at> (<breakpoint>)
  ;; The name of the file to break in.  This is a string, which can in
  ;; fact be any substring of the relevant full file name.
  (file-name #:init-keyword #:file-name)
  ;; Line and column number to break at.
  (line #:init-keyword #:line)
  (column #:init-keyword #:column))

;; Global list of non-deleted breakpoints.
(define breakpoints '())

;; Add to the above list.
(define-method (add-to-global-breakpoint-list (bp <breakpoint>))
  (set! breakpoints (append! breakpoints (list bp))))

;; break-in: create a `break-in' breakpoint.
(define (break-in procedure-name . options)
  ;; Sort out the optional args.
  (let* ((module-or-file-name+options
	  (cond ((and (not (null? options))
		      (or (string? (car options))
			  (list? (car options))))
		 options)
		(else
		 (cons (module-name (current-module)) options))))
	 (module-or-file-name (car module-or-file-name+options))
	 (trap-options (cdr module-or-file-name+options))
	 ;; Create the new breakpoint object.
	 (bp (make <break-in>
	       #:procedure-name procedure-name
	       #:module-or-file-name module-or-file-name
	       #:trap-options (if (memq #:behaviour trap-options)
				  trap-options
				  (cons* #:behaviour
					 (default-breakpoint-behaviour)
					 trap-options)))))
    ;; Add it to the global breakpoint list.
    (add-to-global-breakpoint-list bp)
    ;; Set the new breakpoint, if possible, in already loaded code.
    (set-in-existing-code bp)
    ;; Return the breakpoint object to our caller.
    bp))

;; break-at: create a `break-at' breakpoint.
(define (break-at file-name line column . trap-options)
  ;; Create the new breakpoint object.
  (let* ((bp (make <break-at>
	       #:file-name file-name
	       #:line line
	       #:column column
	       #:trap-options (if (memq #:behaviour trap-options)
				  trap-options
				  (cons* #:behaviour
					 (default-breakpoint-behaviour)
					 trap-options)))))
    ;; Add it to the global breakpoint list.
    (add-to-global-breakpoint-list bp)
    ;; Set the new breakpoint, if possible, in already loaded code.
    (set-in-existing-code bp)
    ;; Return the breakpoint object to our caller.
    bp))

;; Set a `break-in' breakpoint in already loaded code, if possible.
(define-method (set-in-existing-code (bp <break-in>))
  ;; Get the module or file name that was specified for this
  ;; breakpoint.
  (let ((module-or-file-name (slot-ref bp 'module-or-file-name)))
    ;; Handling is simpler for a module name.
    (cond ((list? module-or-file-name)
	   ;; See if the named module exists yet.
	   (let ((m (module-if-already-loaded module-or-file-name)))
	     (maybe-break-in-module-proc m bp)))
	  ((string? module-or-file-name)
	   ;; Try all loaded modules.
	   (or-map (lambda (m)
		     (maybe-break-in-module-proc m bp))
		   (all-loaded-modules)))
	  (else
	   (error "Bad module-or-file-name:" module-or-file-name)))))

(define (make-observer bp trap)
  (lambda (event)
    (trap-target-gone bp trap)))

;; Set a `break-at' breakpoint in already loaded code, if possible.
(define-method (set-in-existing-code (bp <break-at>) . code)
  ;; Procedure to install a source trap on each expression that we
  ;; find matching this breakpoint.
  (define (install-source-trap x)
    (or (or-map (lambda (trap)
		  (and (is-a? trap <source-trap>)
		       (eq? (slot-ref trap 'expression) x)))
		(slot-ref bp 'traps))
	(let ((trap (apply make <source-trap>
			   #:expression x
			   (slot-ref bp 'trap-options))))
	  (slot-set! trap 'observer (make-observer bp trap))
	  (install-trap trap)
	  (trc 'install-source-trap (object-address trap) (object-address x))
	  (trap-installed bp trap #t))))
  ;; Scan the source whash, and install a trap on all code matching
  ;; this breakpoint.
  (trc 'set-in-existing-code (length code))
  (if (null? code)
      (scan-source-whash (slot-ref bp 'file-name)
			 (slot-ref bp 'line)
			 (slot-ref bp 'column)
			 install-source-trap)
      (scan-code (car code)
		 (slot-ref bp 'file-name)
		 (slot-ref bp 'line)
		 (slot-ref bp 'column)
		 install-source-trap)))

;; Temporary implementation of scan-source-whash - this _really_ needs
;; to be implemented in C.
(define (scan-source-whash file-name line column proc)
  ;; Procedure to call for each source expression in the whash.
  (define (folder x props acc)
    (if (and (= line (source-property x 'line))
	     (= column (source-property x 'column))
	     (let ((fn (source-property x 'filename)))
	       (trc 'scan-source-whash fn)
	       (and (string? fn)
		    (string-contains fn file-name))))
	(proc x)))
  ;; Tracing.
  (trc 'scan-source-whash file-name line column)
  ;; Apply this procedure to the whash.
  (hash-fold folder 0 source-whash))

(define (scan-code x file-name line column proc)
  (trc 'scan-code file-name line column)
  (if (pair? x)
      (begin
	(if (and (eq? line (source-property x 'line))
		 (eq? column (source-property x 'column))
		 (let ((fn (source-property x 'filename)))
		   (trc 'scan-code fn)
		   (and (string? fn)
			(string-contains fn file-name))))
	    (proc x))
	(scan-code (car x) file-name line column proc)
	(scan-code (cdr x) file-name line column proc))))

;; If a module named MODULE-NAME has been loaded, return its module
;; object; otherwise return #f.
(define (module-if-already-loaded module-name)
  (nested-ref the-root-module (append '(app modules) module-name)))

;; Construct and return a list of all loaded modules.
(define (all-loaded-modules)
  ;; This is the list that accumulates known modules.  It has to be
  ;; defined outside the following functions, and accumulated using
  ;; set!, so as to avoid infinite loops - because of the fact that
  ;; all non-pure modules have a variable `app'.
  (define known-modules '())
  ;; Return an alist of submodules of the given PARENT-MODULE-NAME.
  ;; Each element of the alist is (NAME . MODULE), where NAME is the
  ;; module's leaf name (i.e. relative to PARENT-MODULE-NAME) and
  ;; MODULE is the module object.  By a "submodule of a parent
  ;; module", we mean any module value that is bound to a symbol in
  ;; the parent module, and which is not an interface module.
  (define (direct-submodules parent-module-name)
    (filter (lambda (name+value)
	      (and (module? (cdr name+value))
		   (not (eq? (module-kind (cdr name+value)) 'interface))))
	    (map (lambda (name)
		   (cons name (local-ref (append parent-module-name
						 (list name)))))
		 (cdar (lls parent-module-name)))))
  ;; Add all submodules (direct and indirect) of the module named
  ;; PARENT-MODULE-NAME to `known-modules', if not already there.
  (define (add-submodules-of parent-module-name)
    (let ((ds (direct-submodules parent-module-name)))
      (for-each
       (lambda (name+module)
         (or (memq (cdr name+module) known-modules)
             (begin
               (set! known-modules (cons (cdr name+module) known-modules))
               (add-submodules-of (append parent-module-name
                                          (list (car name+module)))))))
       ds)))
  ;; Add submodules recursively, starting from the root of all
  ;; modules.
  (add-submodules-of '(app modules))
  ;; Return the result.
  known-modules)

;; Before-load setup for `break-at' breakpoints.
(define-method (setup-before-load (bp <break-at>) filename)
  (let ((trap (apply make <location-trap>
		     #:file-regexp (regexp-quote (slot-ref bp 'file-name))
		     #:line (slot-ref bp 'line)
		     #:column (slot-ref bp 'column)
		     (slot-ref bp 'trap-options))))
    (install-trap trap)
    (trap-installed bp trap #f)
    (letrec ((uninstaller
	      (lambda (file-name)
		(uninstall-trap trap)
		(remove-hook! after-load-hook uninstaller))))
      (add-hook! after-load-hook uninstaller))))

;; After-load setup for `break-in' breakpoints.
(define-method (setup-after-load (bp <break-in>) filename)
  ;; Get the module that the loaded file created or was loaded into,
  ;; and the module or file name that were specified for this
  ;; breakpoint.
  (let ((m (current-module))
	(module-or-file-name (slot-ref bp 'module-or-file-name)))
    ;; Decide whether the breakpoint spec matches this load.
    (if (or (and (string? module-or-file-name)
		 (string-contains filename module-or-file-name))
	    (and (list? module-or-file-name)
		 (equal? (module-name (current-module)) module-or-file-name)))
	;; It does, so try to install the breakpoint.
	(maybe-break-in-module-proc m bp))))

;; After-load setup for `break-at' breakpoints.
(define-method (setup-after-load (bp <break-at>) filename)
  (if (string-contains filename (slot-ref bp 'file-name))
      (set-in-existing-code bp)))

(define (maybe-break-in-module-proc m bp)
  "If module M defines a procedure matching the specification of
breakpoint BP, install a trap on it."
  (let ((proc (module-ref m (slot-ref bp 'procedure-name) #f)))
    (if (and proc
	     (procedure? proc)
	     (let ((module-or-file-name (slot-ref bp 'module-or-file-name)))
	       (if (string? module-or-file-name)
		   (source-file-matches (procedure-source proc)
					module-or-file-name)
		   #t))
	     (not (or-map (lambda (trap)
			    (and (is-a? trap <procedure-trap>)
				 (eq? (slot-ref trap 'procedure) proc)))
			  (slot-ref bp 'traps))))
	;; There is, so install a <procedure-trap> on it.
	(letrec ((trap (apply make <procedure-trap>
			      #:procedure proc
			      (slot-ref bp 'trap-options))))
	  (slot-set! trap 'observer (make-observer bp trap))
	  (install-trap trap)
	  (trap-installed bp trap #t)
	  ;; Tell caller that we installed a trap.
	  #t)
	;; Tell caller that we did not install a trap.
	#f)))

;; After-read setup for `break-at' breakpoints.
(define-method (setup-after-read (bp <break-at>) x)
  (set-in-existing-code bp x))

;; Common code for associating a newly created and installed trap with
;; a breakpoint object.
(define (trap-installed bp trap record?)
  (if record?
      ;; Remember this trap in the breakpoint object.
      (slot-set! bp 'traps (append! (slot-ref bp 'traps) (list trap))))
  ;; Update the breakpoint status.
  (call-observer bp))

;; Common code for handling when the target of one of a breakpoint's
;; traps is being GC'd.
(define (trap-target-gone bp trap)
  (trc 'trap-target-gone (object-address trap))
  ;; Remove this trap from the breakpoint's list.
  (slot-set! bp 'traps (delq! trap (slot-ref bp 'traps)))
  ;; Update the breakpoint status.
  (call-observer bp))

(define (source-file-matches source file-name)
  "Return #t if any of the expressions in SOURCE have a 'filename
source property that includes FILE-NAME; otherwise return #f."
  (and (pair? source)
       (or (let ((source-file-name (source-property source 'filename)))
	     (and source-file-name
		  (string? source-file-name)
		  (string-contains source-file-name file-name)))
	   (let loop ((source source))
	     (and (pair? source)
		  (or (source-file-matches (car source) file-name)
		      (loop (cdr source))))))))

;; Install load hook functions.
(add-hook! before-load-hook
	   (lambda (fn)
	     (for-each-breakpoint setup-before-load fn)))

(add-hook! after-load-hook
	   (lambda (fn)
	     (for-each-breakpoint setup-after-load fn)))

;;; Apply generic function GF to each breakpoint, passing the
;;; breakpoint object and ARGS as args on each call.
(define (for-each-breakpoint gf . args)
  (for-each (lambda (bp)
	      (apply gf bp args))
	    breakpoints))

;; Make sure that recording of source positions is enabled.  Without
;; this break-at breakpoints will obviously not work.
(read-enable 'positions)

;;; (ice-9 debugging breakpoints) ends here.
