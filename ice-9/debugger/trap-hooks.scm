;;;; (ice-9 debugger trap-hooks) -- abstraction of libguile's traps interface

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

;;; This module provides an abstraction around Guile's low level trap
;;; handler interface; its aim is to make the low level trap mechanism
;;; shareable between the debugger and other applications, and to
;;; insulate the rest of the debugger code a bit from changes that may
;;; occur in the low level trap interface in future.

(define-module (ice-9 debugger trap-hooks)
  #:use-module (ice-9 debugger trc)
  #:export (add-trapped-stack-id!
	    remove-trapped-stack-id!
	    before-apply-frame-hook
	    before-enter-frame-hook
	    before-exit-frame-hook
	    after-apply-frame-hook
	    after-enter-frame-hook
	    after-exit-frame-hook
	    add-apply-frame-hook!
	    add-breakpoint-hook!
	    add-enter-frame-hook!
	    add-exit-frame-hook!
	    add-trace-hook!
	    remove-apply-frame-hook!
	    remove-breakpoint-hook!
	    remove-enter-frame-hook!
	    remove-exit-frame-hook!
	    remove-trace-hook!))

;;; The current low level traps interface is as follows.
;;;
;;; All trap handlers are subject to SCM_TRAPS_P, which is controlled
;;; by the `traps' setting of `(evaluator-traps-interface)' but also
;;; (and more relevant in most cases) by the `with-traps' procedure.
;;; Basically, `with-traps' sets SCM_TRAPS_P to 1 during execution of
;;; its thunk parameter.
;;;
;;; Note that all trap handlers are called with SCM_TRAPS_P set to 0
;;; for the duration of the call, to avoid nasty recursive trapping
;;; loops.  If a trap handler knows what it is doing, it can override
;;; this by `(trap-enable traps)'.
;;;
;;; The apply-frame handler is called when Guile is about to perform
;;; an application if EITHER the `apply-frame' evaluator trap option
;;; is set, OR the `trace' debug option is set and the procedure to
;;; apply has its `trace' procedure property set.  The arguments
;;; passed are:
;;;
;;; - the symbol 'apply-frame
;;;
;;; - a continuation or debug object describing the current stack
;;;
;;; - a boolean indicating whether the application is tail-recursive.
;;;
;;; The enter-frame handler is called when the evaluator begins a new
;;; evaluation frame if EITHER the `enter-frame' evaluator trap option
;;; is set, OR the `breakpoints' debug option is set and the code to
;;; be evaluated has its `breakpoint' source property set.  The
;;; arguments passed are:
;;;
;;; - the symbol 'enter-frame
;;;
;;; - a continuation or debug object describing the current stack
;;;
;;; - a boolean indicating whether the application is tail-recursive.
;;;
;;; - an unmemoized copy of the expression to be evaluated.
;;;
;;; If the `enter-frame' evaluator trap option is set, the enter-frame
;;; handler is also called when about to perform an application in
;;; SCM_APPLY, immediately before possible calling the apply-frame
;;; handler.  (I don't totally understand this.)  In this case, the
;;; arguments passed are:
;;;
;;; - the symbol 'enter-frame
;;;
;;; - a continuation or debug object describing the current stack.
;;;
;;; The exit-frame handler is called when Guile exits an evaluation
;;; frame (in SCM_CEVAL) or an application frame (in SCM_APPLY), if
;;; EITHER the `exit-frame' evaluator trap option is set, OR the
;;; `trace' debug option is set and the frame is marked as having been
;;; traced.  The frame will be marked as having been traced if the
;;; apply-frame handler was called for this frame.  (This is trickier
;;; than it sounds because of tail recursion: the same debug frame
;;; could have been used for multiple applications, only some of which
;;; were traced - I think.)  The arguments passed are:
;;;
;;; - the symbol 'exit-frame
;;;
;;; - a continuation or debug object describing the current stack
;;;
;;; - the result of the evaluation or application.

;;; {Stack IDs}
;;;
;;; Mechanism for limiting trapping to contexts whose stack ID matches
;;; one of a registered set.  The default set up is to limit trapping
;;; to events in the contexts of the Guile REPL and of file loading.

(define trapped-stack-ids (list 'repl-stack 'load-stack))
(define all-stack-ids-trapped? #f)

(define (add-trapped-stack-id! id)
  "Add ID to the set of stack ids for which traps are active.
If `#t' is in this set, traps are active regardless of stack context.
To remove ID again, use `remove-trapped-stack-id!'.  If you add the
same ID twice using `add-trapped-stack-id!', you will need to remove
it twice."
  (set! trapped-stack-ids (cons id trapped-stack-ids))
  (set! all-stack-ids-trapped? (memq #t trapped-stack-ids)))

(define (remove-trapped-stack-id! id)
  "Remove ID from the set of stack ids for which traps are active."
  (set! trapped-stack-ids (delq1! id trapped-stack-ids))
  (set! all-stack-ids-trapped? (memq #t trapped-stack-ids)))

(define (trap-here? cont)
  ;; Return true if the stack id of the specified continuation (or
  ;; debug object) is in the set that we should trap for; otherwise
  ;; false.
  (or all-stack-ids-trapped?
      (memq (stack-id cont) trapped-stack-ids)))

;;; {Global State}
;;;
;;; Variables tracking registered handlers, relevant procedures, and
;;; what's turned on as regards the evaluator's debugging options.

(define before-enter-frame-hook (make-hook 3))
(define enter-frame-hook (make-hook))
(define breakpoint-hook (make-hook))
(define after-enter-frame-hook (make-hook))

(define before-exit-frame-hook (make-hook 2))
(define exit-frame-hook (make-hook))
(define after-exit-frame-hook (make-hook))

(define before-apply-frame-hook (make-hook 2))
(define apply-frame-hook (make-hook))
(define trace-hook (make-hook))
(define after-apply-frame-hook (make-hook))

(define (hook-not-empty? hook)
  (not (hook-empty? hook)))

(define set-debug-and-trap-options
  (let ((dopts (debug-options))
	(topts (evaluator-traps-interface))
	(setting (lambda (key opts)
		   (let ((l (memq key opts)))
		     (and l
			  (not (null? (cdr l)))
			  (cadr l)))))
	(debug-set-boolean! (lambda (key value)
			      ((if value debug-enable debug-disable) key)))
	(trap-set-boolean! (lambda (key value)
			     ((if value trap-enable trap-disable) key))))
    (let ((save-debug (memq 'debug dopts))
	  (save-trace (memq 'trace dopts))
	  (save-breakpoints (memq 'breakpoints dopts))
	  (save-enter-frame (memq 'enter-frame topts))
	  (save-apply-frame (memq 'apply-frame topts))
	  (save-exit-frame (memq 'exit-frame topts))
	  (save-enter-frame-handler (setting 'enter-frame-handler topts))
	  (save-apply-frame-handler (setting 'apply-frame-handler topts))
	  (save-exit-frame-handler (setting 'exit-frame-handler topts)))
      (lambda ()
	(let ((need-trace (hook-not-empty? trace-hook))
	      (need-breakpoints (hook-not-empty? breakpoint-hook))
	      (need-enter-frame (hook-not-empty? enter-frame-hook))
	      (need-apply-frame (hook-not-empty? apply-frame-hook))
	      (need-exit-frame (hook-not-empty? exit-frame-hook)))
	  (debug-set-boolean! 'debug
			      (or need-trace
				  need-breakpoints
				  need-enter-frame
				  need-apply-frame
				  need-exit-frame
				  save-debug))
	  (debug-set-boolean! 'trace
			      (or need-trace
				  save-trace))
	  (debug-set-boolean! 'breakpoints
			      (or need-breakpoints
				  save-breakpoints))
	  (trap-set-boolean! 'enter-frame
			     (or need-enter-frame
				 save-enter-frame))
	  (trap-set-boolean! 'apply-frame
			     (or need-apply-frame
				 save-apply-frame))
	  (trap-set-boolean! 'exit-frame
			     (or need-exit-frame
				 save-exit-frame))
	  (trap-set! enter-frame-handler
		     (cond ((or need-breakpoints
				need-enter-frame)
			    enter-frame-handler)
			   (else save-enter-frame-handler)))
	  (trap-set! apply-frame-handler
		     (cond ((or need-trace
				need-apply-frame)
			    apply-frame-handler)
			   (else save-apply-frame-handler)))
	  (trap-set! exit-frame-handler
		     (cond ((or need-exit-frame)
			    exit-frame-handler)
			   (else save-exit-frame-handler))))
	;;(write (evaluator-traps-interface))
	*unspecified*))))

(define (enter-frame-handler key cont . args)
  ;; For a non-application entry, ARGS is (TAIL? EXP), where EXP is an
  ;; unmemoized copy of the source expression.  For an application
  ;; entry, ARGS is empty.
  (if (trap-here? cont)
      (let ((application-entry? (null? args)))
	(trc 'enter-frame-handler)
	(if application-entry?
	    (run-hook before-enter-frame-hook cont #f #f)
	    (run-hook before-enter-frame-hook cont (car args) (cadr args)))
	(run-hook enter-frame-hook)
	(or application-entry?
	    (run-hook breakpoint-hook))
	(run-hook after-enter-frame-hook))))

(define (exit-frame-handler key cont retval)
  (if (trap-here? cont)
      (begin
	(trc 'exit-frame-handler retval (stack-length (make-stack cont)))
	(run-hook before-exit-frame-hook cont retval)
	(run-hook exit-frame-hook)
	(run-hook after-exit-frame-hook))))

(define (apply-frame-handler key cont tail?)
  (if (trap-here? cont)
      (begin
	(trc 'apply-frame-handler tail?)
	(run-hook before-apply-frame-hook cont tail?)
	(run-hook apply-frame-hook)
	(run-hook trace-hook)
	(run-hook after-apply-frame-hook))))

(define-public (add-enter-frame-hook! proc)
  (add-hook! enter-frame-hook proc)
  (set-debug-and-trap-options))

(define-public (add-breakpoint-hook! proc)
  (add-hook! breakpoint-hook proc)
  (set-debug-and-trap-options))

(define-public (add-exit-frame-hook! proc)
  (add-hook! exit-frame-hook proc)
  (set-debug-and-trap-options))

(define-public (add-apply-frame-hook! proc)
  (add-hook! apply-frame-hook proc)
  (set-debug-and-trap-options))

(define-public (add-trace-hook! proc)
  (add-hook! trace-hook proc)
  (set-debug-and-trap-options))

(define-public (remove-enter-frame-hook! proc)
  (remove-hook! enter-frame-hook proc)
  (set-debug-and-trap-options))

(define-public (remove-breakpoint-hook! proc)
  (remove-hook! breakpoint-hook proc)
  (set-debug-and-trap-options))

(define-public (remove-exit-frame-hook! proc)
  (remove-hook! exit-frame-hook proc)
  (set-debug-and-trap-options))

(define-public (remove-apply-frame-hook! proc)
  (remove-hook! apply-frame-hook proc)
  (set-debug-and-trap-options))

(define-public (remove-trace-hook! proc)
  (remove-hook! trace-hook proc)
  (set-debug-and-trap-options))

;;; (ice-9 debugger trap-hooks) ends here.
