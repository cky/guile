;;;; (ice-9 debugger behaviour) -- what to do when you hit a breakpoint

;;; Copyright (C) 2002 Free Software Foundation, Inc.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this software; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;; Boston, MA 02111-1307 USA
;;;
;;; As a special exception, the Free Software Foundation gives permission
;;; for additional uses of the text contained in its release of GUILE.
;;;
;;; The exception is that, if you link the GUILE library with other files
;;; to produce an executable, this does not by itself cause the
;;; resulting executable to be covered by the GNU General Public License.
;;; Your use of that executable is in no way restricted on account of
;;; linking the GUILE library code into it.
;;;
;;; This exception does not however invalidate any other reasons why
;;; the executable file might be covered by the GNU General Public License.
;;;
;;; This exception applies only to the code released by the
;;; Free Software Foundation under the name GUILE.  If you copy
;;; code from other Free Software Foundation releases into a copy of
;;; GUILE, as the General Public License permits, the exception does
;;; not apply to the code that you add in this way.  To avoid misleading
;;; anyone as to the status of such modified files, you must delete
;;; this exception notice from them.
;;;
;;; If you write modifications of your own for GUILE, it is your choice
;;; whether to permit this exception to apply to your modifications.
;;; If you do not wish that, delete this exception notice.

(define-module (ice-9 debugger behaviour)
  #:use-module (ice-9 and-let-star)
  #:use-module (ice-9 debug)
  #:use-module (ice-9 debugger)
  #:use-module (ice-9 debugger trap-hooks)
  #:use-module (ice-9 debugger trc)
  #:use-module (ice-9 debugger utils)
  #:use-module (ice-9 optargs)
  #:export (at-exit
	    at-entry
	    at-apply
	    at-step
	    at-next
	    debug-here
	    trace-here
	    trace-until-exit
	    trace-subtree
	    trace-exit-value
	    add-debug-entry-message
	    with-reference-frame
	    with-reference-frame*))

;;; This module defines useful kinds of behaviour for breakpoints.

(define *cont* #f)
(define *frame* #f)
(define *depth* #f)
(define *expr* #f)
(define *retval* #f)
(define *trace-retval* #f)
(define *trace-entry* #f)
(define *trace-depths* '())
(define *debug-flag* #f)

(add-hook! before-enter-frame-hook
	   (lambda (cont tail? expr)
	     (trc 'before-enter-frame-hook cont tail? expr)
	     (set! *cont* cont)
	     (set! *frame* (last-stack-frame cont))
	     (set! *depth* (stack-length (make-stack cont)))
	     (set! *expr* expr)
	     (set! *trace-entry* #t)
	     (set! *debug-flag* #f)
	     (set! *debug-entry-messages* '())))

(add-hook! before-apply-frame-hook
	   (lambda (cont tail?)
	     (trc 'before-apply-frame-hook cont tail?)
	     (set! *cont* cont)
	     (set! *frame* (last-stack-frame cont))
	     (set! *depth* (stack-length (make-stack cont)))
	     (set! *expr* #f)
	     (set! *trace-entry* #t)
	     (set! *debug-flag* #f)
	     (set! *debug-entry-messages* '())))

(add-hook! before-exit-frame-hook
	   (lambda (cont retval)
	     (trc 'before-exit-frame-hook cont retval)
	     (set! *cont* cont)
	     (set! *frame* (last-stack-frame cont))
	     (set! *depth* (stack-length (make-stack cont)))
	     (set! *expr* #f)
	     (set! *retval* retval)
	     (set! *trace-entry* #f)
	     (set! *trace-retval* #f)
	     (set! *debug-flag* #f)
	     (set! *debug-entry-messages* '())))

(define (debug-if-flag-set)
  (if *debug-flag*
      (begin
	(for-each display (reverse! *debug-entry-messages*))
	(set! *debug-entry-messages* '())
	(debug-stack (make-stack *cont*) #:continuable))))

(add-hook! after-enter-frame-hook debug-if-flag-set)

(add-hook! after-apply-frame-hook debug-if-flag-set)

(add-hook! after-exit-frame-hook
	   (lambda ()
	     (if *trace-retval*
		 (begin
		   (let indent ((td *trace-depths*))
		     (cond ((null? td))
			   (else (display "|  ")
				 (indent (cdr td)))))
		   (display "|  ")
		   (write *retval*)
		   (newline)
		   (set! *trace-retval* #f)))
	     (debug-if-flag-set)))

(define (frame-depth frame)
  (- (stack-length (car frame)) (cdr frame)))

(define (with-reference-frame* frame thunk)
  (let ((saved-*frame* *frame*)
	(saved-*depth* *depth*))
    (dynamic-wind
	(lambda ()
	  (set! *frame* frame)
	  (set! *depth* (frame-depth frame)))
	thunk
	(lambda ()
	  (set! *frame* saved-*frame*)
	  (set! *depth* saved-*depth*)))))

(define-macro (with-reference-frame frame . body)
  `(with-reference-frame* ,frame (lambda () ,@body)))

;;; at-exit THUNK
;;;
;;; Install a thunk to run when we exit the current frame.

(define* (at-exit #:optional thunk)
  (or thunk (set! thunk debug-here))
  (let ((depth *depth*))
    (letrec ((exit (lambda ()
		     (if (<= *depth* depth)
			 (begin
			   (remove-exit-frame-hook! exit)
			   (thunk))))))
      (add-exit-frame-hook! exit))))

;;; at-entry [COUNT [THUNK]]
;;;
;;; Install a thunk to run when we get to the COUNT'th next frame
;;; entry.  COUNT defaults to 1; THUNK defaults to debug-here.

(define* (at-entry #:optional count thunk)
  (or count (set! count 1))
  (or thunk (set! thunk debug-here))
  (letrec ((enter (lambda ()
		    (set! count (- count 1))
		    (if (<= count 0)
			(begin
			  (remove-enter-frame-hook! enter)
			  (thunk))))))
    (add-enter-frame-hook! enter)))

;;; at-apply [COUNT [THUNK]]
;;;
;;; Install a thunk to run when we get to the COUNT'th next
;;; application.  COUNT defaults to 1; THUNK defaults to debug-here.

(define* (at-apply #:optional count thunk)
  (or count (set! count 1))
  (or thunk (set! thunk debug-here))
  (letrec ((apply (lambda ()
		    (set! count (- count 1))
		    (if (<= count 0)
			(begin
			  (remove-apply-frame-hook! apply)
			  (thunk))))))
    (add-apply-frame-hook! apply)))

;;; at-step [COUNT [THUNK]]
;;;
;;; Install a thunk to run when we get to the COUNT'th next
;;; application or frame entry.  COUNT defaults to 1; THUNK defaults
;;; to debug-here.

(define* (at-step #:optional count thunk)
  (or count (set! count 1))
  (or thunk (set! thunk debug-here))
  (letrec ((step (lambda ()
		   (set! count (- count 1))
		   (if (<= count 0)
		       (begin
			 (remove-enter-frame-hook! step)
			 (remove-apply-frame-hook! step)
			 (thunk))))))
    (add-enter-frame-hook! step)
    (add-apply-frame-hook! step)))

;;; at-next [COUNT [THUNK]]
;;;
;;; Install a thunk to run when we get to the COUNT'th next frame
;;; entry in the same source file as the current location.  COUNT
;;; defaults to 1; THUNK defaults to debug-here.  If the current
;;; location has no filename, fall back silently to `at-entry'
;;; behaviour.

(define (current-file-name)
  (and-let* ((source (frame-source *frame*))
	     (position (source-position source)))
    (and position (car position))))

(define* (at-next #:optional count thunk)
  (or count (set! count 1))
  (or thunk (set! thunk debug-here))
  (let ((filename (current-file-name)))
    (if filename
	(letrec ((next (lambda ()
			 (if (equal? (current-file-name) filename)
			     (begin
			       (set! count (- count 1))
			       (if (<= count 0)
				   (begin
				     (remove-enter-frame-hook! next)
				     (thunk))))))))
	  (add-enter-frame-hook! next))
	(at-entry count thunk))))

;;; debug-here
;;;
;;; Set flag to enter the debugger once all behaviour hooks for this
;;; location have been run.

(define (debug-here)
  (set! *debug-flag* #t))

;;; trace-here
;;;
;;; Trace the current location, and install a hook to trace the return
;;; value when we exit the current frame.

(define (trace-here)
  (if *trace-entry*
      (let ((stack (make-stack *cont*))
	    (push-current-depth #f))
	(cond ((null? *trace-depths*)
	       (set! push-current-depth #t))
	      (else
	       (let loop ((frame-number (car *trace-depths*)))
		 (cond ((>= frame-number *depth*))
		       ((frame-real? (stack-ref stack
						(frame-number->index frame-number stack)))
			(set! push-current-depth #t))
		       (else (loop (+ frame-number 1)))))))
	(if push-current-depth
	    (set! *trace-depths* (cons *depth* *trace-depths*)))
	(let indent ((td *trace-depths*))
	  (cond ((null? td))
		(else
		 (display "|  ")
		 (indent (cdr td)))))
	((if *expr*
	     write-frame-short/expression
	     write-frame-short/application) *frame*)
	(newline)
	(if push-current-depth
	    (at-exit (lambda ()
		       (set! *trace-depths* (cdr *trace-depths*))
		       (set! *trace-retval* #t))))
	(set! *trace-entry* #f))))

;;; trace-subtree
;;;
;;; Install hooks to trace everything until exit from the current
;;; frame.  Variable lookups are omitted, as they would (usually) just
;;; clog up the trace without conveying any useful information

(define (trace-until-exit)
  (let ((trace (lambda ()
		 (or (variable? *expr*)
		     (trace-here)))))
    (add-enter-frame-hook! trace)
    (add-apply-frame-hook! trace)
    (at-exit (lambda ()
	       (remove-enter-frame-hook! trace)
	       (remove-apply-frame-hook! trace)))))

(define (trace-subtree)
  (trace-until-exit)
  (trace-here))

;;; trace-exit-value
;;;
;;; Trace the returned value in an exit frame handler.

(define (trace-exit-value)
  (set! *trace-retval* #t))

;;; {Debug Entry Messages}
;;;
;;; Messages to be displayed if we decide to enter the debugger.

(define *debug-entry-messages* '())

(define (add-debug-entry-message message)
  (set! *debug-entry-messages*
	(cons message *debug-entry-messages*)))

;;; {Error Hook Utilities}

;(define (single-instance-installer hook handler)
;  (let ((installed? #f))
;    (lambda (yes/no?)
;      (if (and yes/no? (not installed?))
;	    (begin
;	      (add-hook! hook handler)
;	      (set! installed? #t)))
;      (if (and (not yes/no?) installed?)
;	    (begin
;	      (remove-hook! hook handler)
;	      (set! installed? #f))))))
;
;(define-public save-stack-on-error
;  (letrec ((handler (lambda (key a b c d)
;			(save-stack handler))))
;    (single-instance-installer error-hook handler)))
;
;(define-public display-stack-on-error
;  (letrec ((handler (lambda (key a b c d)
;			(display "DISPLAY-STACK-ON-ERROR: ")
;			(write (list key a b c d))
;			(newline)
;			(display-backtrace (make-stack #t handler)
;					   (current-error-port)))))
;    (single-instance-installer error-hook handler)))
;
;(define-public debug-on-error
;  (letrec ((handler (lambda (key a b c d)
;			(let ((stack (make-stack #t handler)))
;			  (display "DEBUG-ON-ERROR: ")
;			  (write (list key a b c d))
;			  (newline)
;			  (display-error stack (current-error-port) a b c d)
;			  (debug-stack stack)))))
;    (single-instance-installer error-hook handler)))

;;; (ice-9 debugger behaviour) ends here.
