;;;; (ice-9 debugger commands) -- debugger commands

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

(define-module (ice-9 debugger commands)
  #:use-module (ice-9 debug)
  #:use-module (ice-9 debugger)
  #:use-module (ice-9 debugger behaviour)
  #:use-module (ice-9 debugger state)
  #:use-module (ice-9 debugger utils)
  #:export (backtrace
	    evaluate
	    info-args
	    info-frame
	    position
	    up
	    down
	    frame
	    continue
	    finish
	    trace-finish
	    next
	    step))

(define (backtrace state n-frames)
  "Print backtrace of all stack frames, or innermost COUNT frames.
With a negative argument, print outermost -COUNT frames.
If the number of frames isn't explicitly given, the debug option
`depth' determines the maximum number of frames printed."
  (let ((stack (state-stack state)))
    ;; Kludge around lack of call-with-values.
    (let ((values
	   (lambda (start end)
	     (display-backtrace stack
				(current-output-port)
				(if (memq 'backwards (debug-options))
				    start
				    (- end 1))
				(- end start))
	     )))
      (let ((end (stack-length stack)))
	(cond ((not n-frames) ;(>= (abs n-frames) end))
	       (values 0 (min end (cadr (memq 'depth (debug-options))))))
	      ((>= n-frames 0)
	       (values 0 n-frames))
	      (else
	       (values (+ end n-frames) end)))))))

(define (eval-handler key . args)
  (let ((stack (make-stack #t eval-handler)))
    (if (= (length args) 4)
	(apply display-error stack (current-error-port) args)
	;; We want display-error to be the "final common pathway"
	(catch #t
	       (lambda ()
		 (apply bad-throw key args))
	       (lambda (key . args)
		 (apply display-error stack (current-error-port) args)))))
  (throw 'continue))

(define (evaluate state expression)
  "Evaluate an expression.
The expression must appear on the same line as the command,
however it may be continued over multiple lines."
  (let ((source (frame-source (stack-ref (state-stack state)
					 (state-index state)))))
    (if (not source)
	(display "No environment for this frame.\n")
	(catch 'continue
	       (lambda ()
		 (lazy-catch #t
			     (lambda ()
			       (let* ((env (memoized-environment source))
				      (value (local-eval expression env)))
				 (display ";value: ")
				 (write value)
				 (newline)))
			     eval-handler))
	       (lambda args args)))))

(define (info-args state)
  "Argument variables of current stack frame."
  (let ((index (state-index state)))
    (let ((frame (stack-ref (state-stack state) index)))
      (write-frame-index-long frame)
      (write-frame-args-long frame))))

(define (info-frame state)
  "All about selected stack frame."
  (write-state-long state))

(define (position state)
  "Display the position of the current expression."
  (let* ((frame (stack-ref (state-stack state) (state-index state)))
	 (source (frame-source frame)))
    (if (not source)
	(display "No source available for this frame.")
	(let ((position (source-position source)))
	  (if (not position)
	      (display "No position information available for this frame.")
	      (display-position position)))))
  (newline))

(define (up state n)
  "Move @var{n} frames up the stack.  For positive @var{n}, this
advances toward the outermost frame, to higher frame numbers, to
frames that have existed longer.  @var{n} defaults to one."
  (set-stack-index! state (+ (state-index state) (or n 1)))
  (write-state-short state))

(define (down state n)
  "Move @var{n} frames down the stack.  For positive @var{n}, this
advances toward the innermost frame, to lower frame numbers, to frames
that were created more recently.  @var{n} defaults to one."
  (set-stack-index! state (- (state-index state) (or n 1)))
  (write-state-short state))

(define (frame state n)
  "Select and print a stack frame.
With no argument, print the selected stack frame.  (See also \"info frame\").
An argument specifies the frame to select; it must be a stack-frame number."
  (if n (set-stack-index! state (frame-number->index n (state-stack state))))
  (write-state-short state))

;;;; Additional commands that make sense when debugging code that has
;;;; stopped at a breakpoint.

(define (assert-continuable state)
  ;; Check that debugger is in a state where `continuing' makes sense.
  ;; If not, signal an error.
  (or (memq #:continuable (state-flags state))
      (debugger-error "This debug session is not continuable.")))

(define (continue state)
  "Continue program execution."
  (assert-continuable state)
  (debugger-quit))

(define (finish state)
  "Continue until evaluation of the current frame is complete, and
print the result obtained."
  (assert-continuable state)
  (with-reference-frame (stack-ref (state-stack state) (state-index state))
    (at-exit (lambda ()
	       (trace-exit-value)
	       (debug-here))))
  (continue state))

(define (next state n)
  "Continue until entry to @var{n}th next frame in same file."
  (assert-continuable state)
  (with-reference-frame (stack-ref (state-stack state) (state-index state))
    (at-next (or n 1) debug-here))
  (continue state))

(define (step state n)
  "Continue until entry to @var{n}th next frame."
  (assert-continuable state)
  (at-step (or n 1) debug-here)
  ;; An alternative behaviour that might be interesting ...
  ;; (with-reference-frame (stack-ref (state-stack state) (state-index state))
  ;;   (at-exit (lambda () (at-step (or n 1) debug-here))))
  (continue state))

(define (trace-finish state)
  "Trace until evaluation of the current frame is complete."
  (assert-continuable state)
  (with-reference-frame (stack-ref (state-stack state) (state-index state))
    (trace-until-exit)
    (at-exit debug-here))
  (continue state))

;;; (ice-9 debugger commands) ends here.
