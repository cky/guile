;;;; (ice-9 debugger breakpoints) -- general breakpoints interface

;;; Copyright (C) 2002 Free Software Foundation, Inc.
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

(define-module (ice-9 debugger breakpoints)
  #:use-module (ice-9 debugger behaviour)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:export (;; For <breakpoint> subclass implementations.
	    <breakpoint>
	    register-breakpoint-subclass
	    ;; For application use and subclass implementations.
	    bp-number
	    bp-enabled?
	    bp-behaviour
	    bp-run
	    bp-message
	    bp-delete!
	    bp-describe
	    break!
	    trace!
	    trace-subtree!
	    set-breakpoint!
	    get-breakpoint
	    describe-breakpoint
	    disable-breakpoint!
	    enable-breakpoint!
	    delete-breakpoint!
	    all-breakpoints
	    describe-all-breakpoints))

;;; {Breakpoints -- General Properties and Behaviour}

;;; Generics with names beginning `bp-' all take a single breakpoint
;;; argument (i.e. an instance of a subclass of <breakpoint>).

(define-generic bp-number)		; implemented
(define-generic bp-enabled?)		; implemented
(define-generic bp-behaviour)		; implemented
(define-generic bp-run)			; implemented
(define-generic bp-message)		; virtual
(define-generic bp-delete!)		; virtual
(define-generic bp-describe)		; implemented

;;; The following all take arguments that describe (in whatever way
;;; the various subclasses support) a breakpoint location.  The
;;; <breakpoint> implementations of `break!' and `trace!' just call
;;; `set-breakpoint!' specifying the `debug-here' and `trace-here'
;;; behaviours respectively.

(define-generic set-breakpoint!)	; semi-virtual
(define-generic get-breakpoint)		; semi-virtual

;;; Common base class for breakpoints.

(define-class <breakpoint> ()

  ;; Breakpoint number.
  (number #:accessor bp-number
	  #:init-thunk (let ((count 0))
			 (lambda ()
			   (set! count (+ count 1))
			   count)))

  ;; Whether this breakpoint is currently enabled.
  (enabled? #:accessor bp-enabled?
	    #:init-value #t)

  ;; Breakpoint behaviour, either a list of behaviour indicators, or a
  ;; thunk that, when called, returns such a list.
  (behaviour #:accessor bp-behaviour
	     #:init-value '()))

;;; Registration of <breakpoint> subclasses.  The only current reason
;;; for this is so that we can provide `all-breakpoints'.

(define subclass-registrations '())

(define (register-breakpoint-subclass class list-thunk)
  ;; LIST-THUNK should be a thunk that returns a list containing all
  ;; current breakpoints of the corresponding class.
  (set! subclass-registrations
	(assq-set! subclass-registrations class list-thunk)))

(define (all-breakpoints)
  (sort (apply append
	       (map (lambda (list-thunk) (list-thunk))
		    (map cdr subclass-registrations)))
	(lambda (bp1 bp2)
	  (< (bp-number bp1) (bp-number bp2)))))

(define (describe-all-breakpoints)
  (for-each (lambda (bp)
	      (bp-describe bp #t))
	    (all-breakpoints)))

(define-method (get-breakpoint (number <integer>))
  (let loop ((bps (all-breakpoints)))
    (if (null? bps)
	#f
	(let* ((bp (car bps))
	       (bp-num (bp-number bp)))
	  (cond ((= bp-num number) bp)
		((> bp-num number) #f)
		(else (loop (cdr bps))))))))

(define (make-breakpoint-command proc)
  (lambda args
    (let ((bp (apply get-breakpoint args)))
      (if bp
	  (proc bp)
	  (display "Breakpoint not found\n")))))

(define describe-breakpoint
  (make-breakpoint-command (lambda (bp)
			     (bp-describe bp #t))))

(define disable-breakpoint!
  (make-breakpoint-command (lambda (bp)
			     (set! (bp-enabled? bp) #f)
			     (bp-describe bp #t))))

(define enable-breakpoint!
  (make-breakpoint-command (lambda (bp)
			     (set! (bp-enabled? bp) #t)
			     (bp-describe bp #t))))

(define delete-breakpoint!
  (make-breakpoint-command bp-delete!))

(define-method (set-breakpoint! behaviour (number <integer>))
  (let ((bp (get-breakpoint number)))
    (if bp
	(begin
	  (set! (bp-behaviour bp) behaviour)
	  (bp-describe bp #t))
	(display "No such breakpoint\n"))))

;;; `bp-run' is what trap hook functions should call when they
;;; establish that the program is at a breakpoint location.

(define-method (bp-run (bp <breakpoint>))
  ;; Only do anything if the breakpoint is enabled.
  (add-debug-entry-message (bp-message bp "Hit breakpoint" #f))
  (if (bp-enabled? bp)
      ;; Get behaviour for this breakpoint.
      (let ((behaviour (bp-behaviour bp)))
	;; Behaviour should be a thunk or a list of thunks.
	(cond ((thunk? behaviour)
	       (behaviour))
	      ((list? behaviour)
	       (for-each (lambda (thunk) (thunk)) behaviour))
	      (else
	       (bp-message bp "Bad behaviour for breakpoint" #t)))
; 	(if (thunk? behaviour)
; 	    (set! behaviour (behaviour)))
; 	;; If not a list, wrap as a list.
; 	(or (list? behaviour)
; 	    (set! behaviour (list behaviour)))
; 	;; If behaviour indicates tracing, do so.
; 	(if (memq #:trace behaviour)
; 	    (trace-here))
; 	;; If behaviour indicates a thunk to be run when we exit the
; 	;; current frame, register it.
; 	(let ((at-exit (memq #:at-exit behaviour)))
; 	  (if (and at-exit (not (null? (cdr at-exit))))
; 	      (set-at-exit (cadr at-exit))))
; 	;; If behaviour indicates interactive debugging, set flag that
; 	;; will cause us to enter the debugger.
; 	(if (memq #:debug behaviour)
; 	    (begin
; 	      (bp-message "Hit breakpoint" bp)
; 	      (debug-here)))
	)))

;;; `break! ...' is just shorthand for `set-breakpoint! debug-here ...'.

(define (break! . args)
  (apply set-breakpoint! debug-here args))

;;; Similarly `trace! ...' and `set-breakpoint! trace-here ...'.

(define (trace! . args)
  (apply set-breakpoint! trace-here args))

;;; And so on.

(define (trace-subtree! . args)
  (apply set-breakpoint! trace-subtree args))

;;; `bp-describe' is expected to be overridden/extended by subclasses,
;;; but subclass implementations may want to leverage this
;;; implementation by beginning with `(next-method)'.

(define-method (bp-describe (bp <breakpoint>) port)
  (bp-message bp "Breakpoint" port)
  (format port "\tenabled? = ~S\n" (bp-enabled? bp))
  (format port "\tbehaviour = ~S\n" (bp-behaviour bp))
  *unspecified*)

;;; (ice-9 debugger breakpoints) ends here.
