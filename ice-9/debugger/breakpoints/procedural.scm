;;;; (ice-9 debugger breakpoints procedural) -- procedural breakpoints

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

(define-module (ice-9 debugger breakpoints procedural)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (ice-9 debugger breakpoints)
  #:use-module (ice-9 debugger trc)
  #:use-module (ice-9 debugger trap-hooks)
  #:export (<procedure-breakpoint>
	    bp-procedure))

;;; {Procedure Breakpoints}
;;;
;;; Breakpoints that activate upon application of a particular
;;; procedure.

(define-generic bp-procedure)
(define-generic bp-hook)

(define-class <procedure-breakpoint> (<breakpoint>)

  ;; The procedure to which this breakpoint applies.
  (procedure #:accessor bp-procedure
	     #:init-keyword #:procedure)
  
  ;; The procedure that is registered as a trace hook for this
  ;; breakpoint, stored here so that we can easily deregister it.
  (hook #:accessor bp-hook))

(define (nameify proc)
  (or (procedure-name proc) proc))

(define-method (bp-message (bp <procedure-breakpoint>) message port)
  (format port
	  "~A ~A: [~A]\n"
	  message
	  (bp-number bp)
	  (nameify (bp-procedure bp))))

;;; Alist of all procedure breakpoints:
;;;   ((PROCEDURE . BREAKPOINT) ...)
;;; Keys are unique according to `eq?'.
(define procedure-breakpoints '())

(define-method (get-breakpoint (proc <procedure>))
  (assq-ref procedure-breakpoints proc))

(define *proc* #f)

(add-hook! before-apply-frame-hook
	   (lambda (cont tail?)
	     (trc 'before-apply-frame-hook tail?)
	     ;(set! *expr* #f)
	     (set! *proc* (frame-procedure (last-stack-frame cont)))))

(define (add-breakpoint proc)
  (let* ((bp (make <procedure-breakpoint> #:procedure proc))
	 (hook (lambda ()
		 (if (eq? proc *proc*)
		     (bp-run bp)))))
    (set-procedure-property! proc 'trace #t)
    (set! (bp-hook bp) hook)
    (add-trace-hook! hook)
    (set! procedure-breakpoints (assq-set! procedure-breakpoints proc bp))
    bp))

(define-method (set-breakpoint! behaviour (proc <procedure>))
  (let ((bp (or (get-breakpoint proc)
		(add-breakpoint proc))))
    (set! (bp-behaviour bp) behaviour)
    (bp-message bp "Set breakpoint" #t)
    bp))

(define-method (bp-delete! (bp <procedure-breakpoint>))
  (let ((proc (bp-procedure bp)))
    (set! procedure-breakpoints
	  (assq-remove! procedure-breakpoints proc))
    (set-procedure-property! proc 'trace #f)
    (remove-trace-hook! (bp-hook bp))
    (bp-message bp "Deleted breakpoint" #t))
  *unspecified*)
	  
(register-breakpoint-subclass <procedure-breakpoint>
			      (lambda ()
				(map cdr procedure-breakpoints)))

;;; (ice-9 debugger breakpoints procedure) ends here.
