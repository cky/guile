;;;; 	Copyright (C) 1996, 1997 Mikael Djurfeldt
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;;
;;;; The author can be reached at djurfeldt@nada.kth.se
;;;; Mikael Djurfeldt, SANS/NADA KTH, 10044 STOCKHOLM, SWEDEN
;;;;


;;; *******************************
;;; * Experimental hack           *
;;; * Shouldn't go into snapshots *
;;; * Don't distribute!           *
;;; *******************************

(define-module (ice-9 gwish)
  :use-module (guile)
  :use-module (ice-9 threads)
  :use-module (ice-9 nonblocking)
  :use-module (ice-9 gtcl))


;;; {The Interpreter}
;;;

(set! the-interpreter (tcl-create-interp))

(define gtcl-module (local-ref '(app modules ice-9 gtcl)))
(define tcl-binder (make-tcl-binder the-interpreter))

(set-module-binder! (module-public-interface gtcl-module) tcl-binder)

;;; {Namespace cleaning}
;;;

;; These are the names of procedures already defined 
;; in Scheme but which, in this context, ought to refer
;; to Tcl/Tk commands.

(define override-scheme-list '(bind raise))

(for-each
 (lambda (name)
   (eval `(set! ,name (reify-tcl-command the-interpreter ',name))))
 override-scheme-list)

;;; {Non-blocking ports}

(define stdin-avail #t)

(proc set-stdin-flag args (set! stdin-avail #t) "")

(define wait TCL_ALL_EVENTS)
(define dont-wait (+ wait TCL_DONT_WAIT))

(set! handle-input-events
      (lambda ()
	(cond ((single-active-thread?) (tcl-do-one-event wait))
	      ((zero? (tcl-do-one-event dont-wait))
	       (yield)
	       ))
	  (if stdin-avail
	      (begin
		(set! stdin-avail #f)
		(signal-condition-variable repl-input-port-condvar)))))

(fileevent 'stdin 'readable 'set-stdin-flag)

(activate-non-blocking-input)

;;; {The application window}

(let ((init-status (tk-init-main-window the-interpreter
					(or (getenv "DISPLAY") ":0")
					"gwish"
					"Gwish")))
  (if (not (eq? #t init-status))
      (error init-status)))
