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

;;; {Session support for Emacs}
;;;

(define-module (ice-9 emacs)
  :use-module (ice-9 threads)
  :use-module (ice-9 nonblocking))

(define emacs-escape-character #\sub)

(define emacs-output-port (current-output-port))

(define (make-emacs-command char)
  (let ((cmd (list->string (list emacs-escape-character char))))
    (lambda ()
      (display cmd emacs-output-port))))

(define enter-input-wait (make-emacs-command #\s))
(define exit-input-wait  (make-emacs-command #\f))
(define enter-read-character #\r)
(define sending-error	    (make-emacs-command #\F))
(define sending-backtrace (make-emacs-command #\B))
(define sending-result    (make-emacs-command #\x))
(define end-of-text	    (make-emacs-command #\.))

;; {Error handling}
;;

(add-hook! before-backtrace-hook sending-backtrace)
(add-hook! after-backtrace-hook end-of-text)
(add-hook! before-error-hook sending-error)
(add-hook! after-error-hook end-of-text)

;; {Repl}
;;

(set-current-error-port emacs-output-port)

(add-hook! before-read-hook
	   (lambda ()
	     (enter-input-wait)
	     (force-output emacs-output-port)))

(add-hook! after-read-hook
	   (lambda ()
	     (exit-input-wait)
	     (force-output emacs-output-port)))

;;; {Misc.}

(define (make-emacs-load-port orig-port)
  (letrec ((read-char-fn  (lambda args
			    (let ((c (read-char orig-port)))
			      (if (eq? c #\soh)
				  (throw 'end-of-chunk)
				  c)))))
    
    (make-soft-port
     (vector #f #f #f
	     read-char-fn
	     (lambda () (close-port orig-port)))
     "r")))

(set! repl-input-port (make-emacs-load-port (current-input-port)))
(set-current-input-port repl-input-port)

(define (emacs-eval-request form)
  (let ((port (current-output-port)))
    (sending-result port)
    (write (eval form) port)
    (end-of-text port)
    (force-output port)))

(define load-acknowledge (make-emacs-command #\l))

(define load-port (current-input-port))

(define (flush-line port)
  (let loop ((c (read-char port)))
    (if (not (eq? c #\nl))
	(loop (read-char port)))))

(define whitespace-chars (list #\space #\tab #\nl))

(define (flush-whitespace port)
  (catch 'end-of-chunk
	 (lambda ()
	   (let loop ((c (read-char port)))
	     (cond ((eq? c the-eof-object)
		    (error "End of file while recieving Emacs data"))
		   ((memq c whitespace-chars) (loop (read-char port)))
		   ((eq? c #\;) (flush-line port) (loop (read-char port)))
		   (else (unread-char c port))))
	   #f)
	 (lambda args
	   (read-char port) ; Read final newline
	   #t)))

(define (emacs-load filename linum)
  (set-port-filename! %%load-port filename)
  (set-port-line! %%load-port linum)
  (set-port-column! %%load-port 0)
  (lazy-catch #t
	      (lambda ()
		(let loop ((endp (flush-whitespace %%load-port)))
		  (if (not endp)
		      (begin
			(start-stack read-and-eval! (read-and-eval! %%load-port))
			(loop (flush-whitespace %%load-port)))
		      (begin
			(load-acknowledge))))
		)
	      (lambda (key . args)
		(cond ((eq? key 'end-of-chunk)
		       (set! the-last-stack #f)
		       (set! stack-saved? #t)
		       (scm-error 'misc-error
				  'emacs-load
				  "Incomplete expression"
				  '()
				  '()))
		      ((eq? key 'exit))
		      (else
		       (save-stack 2)
		       (catch 'end-of-chunk
			      (lambda ()
				(let loop ()
				  (read-char %%load-port)
				  (loop)))
			      (lambda args
				#f))
		       (apply throw key args))))))

;;; A fix to get the emacs interface to work together with the module system.
;;;
(variable-set! (builtin-variable '%%load-port) load-port)
(variable-set! (builtin-variable '%%emacs-load) emacs-load)
