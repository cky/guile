;;;; 	Copyright (C) 1996 Mikael Djurfeldt
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
;;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;
;;;; The author can be reached at djurfeldt@nada.kth.se
;;;; Mikael Djurfeldt, SANS/NADA KTH, 10044 STOCKHOLM, SWEDEN
;;;;


;;; *******************************
;;; * Experimental hack           *
;;; * Shouldn't go into snapshots *
;;; * Don't distribute!           *
;;; *******************************

;;; {Non-blocking ports}
;;;

(define-module (ice-9 nonblocking)
  :use-module (ice-9 threads))

(define mu (make-mutex))
(define-public repl-input-port-condvar (make-condition-variable))

(define non-blocking-input #f)

(define-public (make-non-blocking-port wait-port read-port)
  (letrec ((read-char-fn  (lambda args
			    (if (char-ready? wait-port)
				(read-char read-port)
				(begin
				  (wait-condition-variable repl-input-port-condvar
							   mu)
				  (unlock-mutex mu)
				  (read-char-fn))))))
    (make-soft-port
     (vector #f #f #f
	     read-char-fn
	     (lambda () (close-port orig-port)))
     "r")))

(define-public repl-input-port (current-input-port))
(define-public basic-repl-input-port repl-input-port)

(define-public handle-input-events
  (lambda ()
    (if (single-active-thread?)
	(select (list basic-repl-input-port) '() '()))
    (if (char-ready? basic-repl-input-port)
	(signal-condition-variable repl-input-port-condvar)
	(yield))))

(define (kick)
  (call-with-new-thread
   (lambda ()
     (error-catching-loop
      (lambda ()
	(let loop ()
	  (handle-input-events)
	  (loop)))))
   (lambda args args)))

(define-public (activate-non-blocking-input)
  (if (not non-blocking-input)
      (begin
	(set! repl-input-port (make-non-blocking-port basic-repl-input-port
						      (current-input-port)))
	(set-current-input-port repl-input-port)
	(kick)
	(set! non-blocking-input #t))))
