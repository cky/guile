;;;; Guile Debugger

;;; Copyright (C) 1999, 2001, 2002 Free Software Foundation, Inc.
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

(define-module (ice-9 debugger)
  #:use-module (ice-9 debugger command-loop)
  #:use-module (ice-9 debugger state)
  #:use-module (ice-9 debugger utils)
  #:use-module (ice-9 format)
  #:export (debug-stack
	    debug
	    debug-last-error
	    debugger-error
	    debugger-quit
	    debugger-input-port
	    debugger-output-port)
  #:no-backtrace)

;;; The old (ice-9 debugger) has been factored into its constituent
;;; parts:
;;;
;;; (ice-9 debugger) - public interface to all of the following
;;;
;;; (... commands) - procedures implementing the guts of the commands
;;;                  provided by the interactive debugger
;;;
;;; (... command-loop) - binding these commands into the interactive
;;;                      debugger command loop
;;;
;;; (... state) - implementation of an object that tracks current
;;;               debugger state
;;;
;;; (... utils) - utilities for printing out frame and stack
;;;               information in various formats
;;;
;;; The division between (... commands) and (... command-loop) exists
;;; because I (NJ) have another generic command loop implementation
;;; under development, and I want to be able to switch easily between
;;; that and the command loop implementation here.  Thus the
;;; procedures in this file delegate to a debugger command loop
;;; implementation via the `debugger-command-loop-*' interface.  The
;;; (ice-9 debugger command-loop) implementation can be replaced by
;;; any other that implements the `debugger-command-loop-*' interface
;;; simply by changing the relevant #:use-module line above.
;;;
;;; The following new parts add breakpoint support:
;;;
;;; (... behaviour) - codification of the things that can happen when
;;;                   a breakpoint is hit, regardless of the type of
;;;                   the breakpoint
;;;
;;; (... breakpoints) - management of breakpoints in general
;;;
;;; (... breakpoints procedural) - breakpoints that trigger upon
;;;                                application of a specified
;;;                                procedure
;;;
;;; (... breakpoints source) - breakpoints that trigger upon
;;;                            evaluation of a specific source
;;;                            expression
;;;
;;; (... trap-hooks) - a (slightly) higher-level abstraction of
;;;                    Guile's evaluator traps interface
;;;
;;; (... trc) - generic tracing interface for debugging tricky code
;;;             using the `printf' method :-)
;;;
;;; Note that (... breakpoints range) doesn't work yet.  If loaded, it
;;; seems to cause some kind of explosion in the GOOPS method cache
;;; calculation code.
;;;
;;; - Neil Jerram <neil@ossau.uklinux.net> 2002-10-26

(define *not-yet-introduced* #t)

(define (debug-stack stack . flags)
  "Invoke the Guile debugger to explore the specified @var{stack}.

@var{flags}, if present, are keywords indicating characteristics of
the debugging session: the valid keywords are as follows.

@table @code
@item #:continuable
Indicates that the debugger is being invoked from a context (such as
an evaluator trap handler) where it is possible to return from the
debugger and continue normal code execution.  This enables the
@dfn{continuing execution} commands, for example @code{continue} and
@code{step}.

@item #:with-introduction
Indicates that the debugger should display an introductory message.
@end table"
  (start-stack 'debugger
    (let ((state (apply make-state stack 0 flags)))
      (with-input-from-port (debugger-input-port)
	(lambda ()
	  (with-output-to-port (debugger-output-port)
	    (lambda ()
	      (if (or *not-yet-introduced*
		      (memq #:with-introduction flags))
		  (let ((ssize (stack-length stack)))
		    (display "This is the Guile debugger -- for help, type `help'.\n")
		    (set! *not-yet-introduced* #f)
		    (if (= ssize 1)
			(display "There is 1 frame on the stack.\n\n")
			(format #t "There are ~A frames on the stack.\n\n" ssize))))
	      (write-state-short state)
	      (debugger-command-loop state))))))))

(define (debug)
  "Invoke the Guile debugger to explore the context of the last error."
  (let ((stack (fluid-ref the-last-stack)))
    (if stack
	(debug-stack stack)
	(display "Nothing to debug.\n"))))

(define debug-last-error debug)

(define (debugger-error message)
  "Signal a debugger usage error with message @var{message}."
  (debugger-command-loop-error message))

(define (debugger-quit)
  "Exit the debugger."
  (debugger-command-loop-quit))

;;; {Debugger Input and Output Ports}

(define debugger-input-port
  (let ((input-port (current-input-port)))
    (make-procedure-with-setter
     (lambda () input-port)
     (lambda (port) (set! input-port port)))))

(define debugger-output-port
  (let ((output-port (current-output-port)))
    (make-procedure-with-setter
     (lambda () output-port)
     (lambda (port) (set! output-port port)))))

;;; (ice-9 debugger) ends here.
