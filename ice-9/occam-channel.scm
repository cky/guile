;;;; Occam-like channels

;;; Copyright (C) 2003 Free Software Foundation, Inc.
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

(define-module (ice-9 occam-channel)
  #:use-syntax (ice-9 syncase)
  #:use-module (oop goops)
  #:use-module (ice-9 threads)
  #:export-syntax (alt
		   ;; macro use:
		   oc:lock oc:unlock oc:consequence
		   oc:immediate-dispatch oc:late-dispatch oc:first-channel
		   oc:set-handshake-channel oc:unset-handshake-channel)
  #:export (make-channel
	    ?
	    !
	    make-timer
	    ;; macro use:
	    handshake-channel mutex
	    sender-waiting?
	    immediate-receive late-receive
	    )
  )

(define no-data '(no-data))
(define receiver-waiting '(receiver-waiting))

(define-class <channel> ())

(define-class <data-channel> (<channel>)
  (handshake-channel #:accessor handshake-channel)
  (data #:accessor data #:init-value no-data)
  (cv #:accessor cv #:init-form (make-condition-variable))
  (mutex #:accessor mutex #:init-form (make-mutex)))

(define-method (initialize (ch <data-channel>) initargs)
  (next-method)
  (set! (handshake-channel ch) ch))

(define-method (make-channel)
  (make <data-channel>))

(define-method (sender-waiting? (ch <data-channel>))
  (not (eq? (data ch) no-data)))

(define-method (receiver-waiting? (ch <data-channel>))
  (eq? (data ch) receiver-waiting))

(define-method (immediate-receive (ch <data-channel>))
  (signal-condition-variable (cv ch))
  (let ((res (data ch)))
    (set! (data ch) no-data)
    res))

(define-method (late-receive (ch <data-channel>))
  (let ((res (data ch)))
    (set! (data ch) no-data)
    res))

(define-method (? (ch <data-channel>))
  (lock-mutex (mutex ch))
  (let ((res (cond ((receiver-waiting? ch)
		    (unlock-mutex (mutex ch))
		    (scm-error 'misc-error '?
			       "another process is already receiving on ~A"
			       (list ch) #f))
		   ((sender-waiting? ch)
		    (immediate-receive ch))
		   (else
		    (set! (data ch) receiver-waiting)
		    (wait-condition-variable (cv ch) (mutex ch))
		    (late-receive ch)))))
    (unlock-mutex (mutex ch))
    res))

(define-method (! (ch <data-channel>))
  (! ch *unspecified*))

(define-method (! (ch <data-channel>) (x <top>))
  (lock-mutex (mutex (handshake-channel ch)))
  (cond ((receiver-waiting? ch)
	 (set! (data ch) x)
	 (signal-condition-variable (cv (handshake-channel ch))))
	((sender-waiting? ch)
	 (unlock-mutex (mutex (handshake-channel ch)))
	 (scm-error 'misc-error '! "another process is already sending on ~A"
		    (list ch) #f))
	(else
	 (set! (data ch) x)
	 (wait-condition-variable (cv ch) (mutex ch))))
  (unlock-mutex (mutex (handshake-channel ch))))

;;; Add protocols?

(define-class <port-channel> (<channel>)
  (port #:accessor port #:init-keyword #:port))

(define-method (make-channel (port <port>))
  (make <port-channel> #:port port))

(define-method (? (ch <port-channel>))
  (read (port ch)))

(define-method (! (ch <port-channel>))
  (write (port ch)))

(define-class <timer-channel> (<channel>))

(define the-timer (make <timer-channel>))

(define timer-cv (make-condition-variable))
(define timer-mutex (make-mutex))

(define (make-timer)
  the-timer)

(define (timeofday->us t)
  (+ (* 1000000 (car t)) (cdr t)))

(define (us->timeofday n)
  (cons (quotient n 1000000)
	(remainder n 1000000)))

(define-method (? (ch <timer-channel>))
  (timeofday->us (gettimeofday)))

(define-method (? (ch <timer-channel>) (t <integer>))
  (lock-mutex timer-mutex)
  (wait-condition-variable timer-cv timer-mutex (us->timeofday t))
  (unlock-mutex timer-mutex))

;;; (alt CLAUSE ...)
;;;
;;; CLAUSE ::= ((? CH) FORM ...)
;;;            | (EXP (? CH) FORM ...)
;;;            | (EXP FORM ...)
;;;
;;; where FORM ... can be => (lambda (x) ...)
;;;
;;; *fixme* Currently only handles <data-channel>:s
;;;

(define-syntax oc:lock
  (syntax-rules (?)
    ((_ ((? ch) form ...)) (lock-mutex (mutex ch)))
    ((_ (exp (? ch) form ...)) (lock-mutex (mutex ch)))
    ((_ (exp form ...)) #f)))

(define-syntax oc:unlock
  (syntax-rules (?)
    ((_ ((? ch) form ...)) (unlock-mutex (mutex ch)))
    ((_ (exp (? ch) form ...)) (unlock-mutex (mutex ch)))
    ((_ (exp form ...)) #f)))

(define-syntax oc:consequence
  (syntax-rules (=>)
    ((_ data) data)
    ((_ data => (lambda (x) e1 e2 ...))
     (let ((x data)) e1 e2 ...))
    ((_ data e1 e2 ...)
     (begin data e1 e2 ...))))

(define-syntax oc:immediate-dispatch
  (syntax-rules (?)
    ((_ ((? ch) e1 ...))
     ((sender-waiting? ch)
      (oc:consequence (immediate-receive ch) e1 ...)))
    ((_ (exp (? ch) e1 ...))
     ((and exp (sender-waiting? ch))
      (oc:consequence (immediate-receive ch) e1 ...)))
    ((_ (exp e1 ...))
     (exp e1 ...))))

(define-syntax oc:late-dispatch
  (syntax-rules (?)
    ((_ ((? ch) e1 ...))
     ((sender-waiting? ch)
      (oc:consequence (late-receive ch) e1 ...)))
    ((_ (exp (? ch) e1 ...))
     ((and exp (sender-waiting? ch))
      (oc:consequence (late-receive ch) e1 ...)))
    ((_ (exp e1 ...))
     (#f))))

(define-syntax oc:first-channel
  (syntax-rules (?)
    ((_ ((? ch) e1 ...) c2 ...)
     ch)
    ((_ (exp (? ch) e1 ...) c2 ...)
     ch)
    ((_ c1 c2 ...)
     (first-channel c2 ...))))

(define-syntax oc:set-handshake-channel
  (syntax-rules (?)
    ((_ ((? ch) e1 ...) handshake)
     (set! (handshake-channel ch) handshake))
    ((_ (exp (? ch) e1 ...) handshake)
     (and exp (set! (handshake-channel ch) handshake)))
    ((_ (exp e1 ...) handshake)
     #f)))

(define-syntax oc:unset-handshake-channel
  (syntax-rules (?)
    ((_ ((? ch) e1 ...))
     (set! (handshake-channel ch) ch))
    ((_ (exp (? ch) e1 ...))
     (and exp (set! (handshake-channel ch) ch)))
    ((_ (exp e1 ...))
     #f)))

(define-syntax alt
  (lambda (x)
    (define (else-clause? x)
      (syntax-case x (else)
	((_) #f)
	((_ (else e1 e2 ...)) #t)
	((_ c1 c2 ...) (else-clause? (syntax (_ c2 ...))))))
    
    (syntax-case x (else)
      ((_ c1 c2 ...)
       (else-clause? x)
       (syntax (begin
		 (oc:lock c1)
		 (oc:lock c2) ...
		 (let ((res (cond (oc:immediate-dispatch c1)
				  (oc:immediate-dispatch c2) ...)))
		   (oc:unlock c1)
		   (oc:unlock c2) ...
		   res))))
      ((_ c1 c2 ...)
       (syntax (begin
		 (oc:lock c1)
		 (oc:lock c2) ...
		 (let ((res (cond (oc:immediate-dispatch c1)
				  (oc:immediate-dispatch c2) ...
				  (else (let ((ch (oc:first-channel c1 c2 ...)))
					  (oc:set-handshake-channel c1 ch)
					  (oc:set-handshake-channel c2 ch) ...
					  (wait-condition-variable (cv ch)
								   (mutex ch))
					  (oc:unset-handshake-channel c1)
					  (oc:unset-handshake-channel c2) ...
					  (cond (oc:late-dispatch c1)
						(oc:late-dispatch c2) ...))))))
		   (oc:unlock c1)
		   (oc:unlock c2) ...
		   res)))))))
