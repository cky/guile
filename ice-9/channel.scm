;;; Guile object channel

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(define-module (ice-9 channel))

;;;
;;; Channel type
;;;

(define channel-type
  (make-record-type 'channel '(stdin stdout printer token-module)))

(define make-channel (record-constructor channel-type))

(define-public (make-object-channel printer)
  (make-channel (current-input-port)
		(current-output-port)
		printer
		(make-module)))

(define channel-stdin (record-accessor channel-type 'stdin))
(define channel-stdout (record-accessor channel-type 'stdout))
(define channel-printer (record-accessor channel-type 'printer))
(define channel-token-module (record-accessor channel-type 'token-module))

;;;
;;; Channel
;;;

(define-public (channel-open ch)
  (let ((stdin (channel-stdin ch))
	(stdout (channel-stdout ch))
	(printer (channel-printer ch))
	(token-module (channel-token-module ch)))
    (let loop ()
      (catch #t
	(lambda ()
	  (channel:prompt stdout)
	  (let ((cmd (read stdin)))
	    (if (eof-object? cmd)
	      (throw 'quit)
	      (case cmd
		((eval)
		 (module-use! (current-module) token-module)
		 (printer ch (eval (read stdin) (current-module))))
		((destroy)
		 (let ((token (read stdin)))
		   (if (module-defined? token-module token)
		     (module-remove! token-module token)
		     (channel:error stdout "Invalid token: ~S" token))))
		((quit)
		 (throw 'quit))
		(else
		 (channel:error stdout "Unknown command: ~S" cmd)))))
	  (loop))
	(lambda (key . args)
	  (case key
	    ((quit) (throw 'quit))
	    (else
	     (format stdout "exception = ~S\n"
		     (list key (apply format #f (cadr args) (caddr args))))
	     (loop))))))))

(define-public (channel-print-value ch val)
  (format (channel-stdout ch) "value = ~S\n" val))

(define-public (channel-print-token ch val)
  (let* ((token (symbol-append (gensym "%%") '%%))
	 (pair (cons token (object->string val))))
    (format (channel-stdout ch) "token = ~S\n" pair)
    (module-define! (channel-token-module ch) token val)))

(define (channel:prompt port)
  (display "channel> " port)
  (force-output port))

(define (channel:error port msg . args)
  (display "ERROR: " port)
  (apply format port msg args)
  (newline port))

;;;
;;; Guile 1.4 compatibility
;;;

(define guile:eval eval)
(define eval
  (if (= (car (procedure-property guile:eval 'arity)) 1)
    (lambda (x e) (guile:eval x))
    guile:eval))

(define object->string
  (if (defined? 'object->string)
    object->string
    (lambda (x) (format #f "~S" x))))
