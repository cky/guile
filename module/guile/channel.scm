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

(define-module (guile channel)
  :use-syntax (system base syntax)
  :export (open-object-channel))

(define-record (<channel> (stdin (current-input-port))
			  (stdout (current-output-port))
			  (token-module (make-module))))

(define (make-channel) (<channel>))

(define (native-type? x)
  (or (boolean? x) (integer? x) (null? x) (symbol? x) (string? x)
      (pair? x) (vector? x)))

(define (open-object-channel)
  (let ((ch (make-channel)))
    (let loop ()
      (catch #t
	(lambda ()
	  (channel:prompt ch)
	  (let ((cmd (read ch.stdin)))
	    (if (eof-object? cmd)
	      (throw 'quit)
	      (case cmd
		((eval)
		 (module-use! (current-module) ch.token-module)
		 (let ((val (eval (read ch.stdin) (current-module))))
		   (if (native-type? val)
		     (format ch.stdout "value = ~S\n" val)
		     (let* ((token (gensym "%object-token%"))
			    (pair (cons token (object->string val))))
		       (format ch.stdout "token = ~S\n" pair)
		       (module-define! ch.token-module token val)))))
		((destroy)
		 (let ((token (read ch.stdin)))
		   (if (module-defined? ch.token-module token)
		     (module-remove! ch.token-module token)
		     (channel:error ch "Invalid token: ~S" token))))
		((quit)
		 (throw 'quit))
		(else
		 (channel:error ch "Unknown command: ~S" cmd)))))
	  (loop))
	(lambda args
	  (case (car args)
	    ((quit) (throw 'quit))
	    (else
	     (format ch.stdout "exception = ~S\n" args)
	     (loop))))))))

(define (channel:prompt ch)
  (display "channel> " ch.stdout)
  (force-output ch.stdout))

(define (channel:error ch msg . args)
  (display "ERROR: " ch.stdout)
  (apply format ch.stdout msg args)
  (newline ch.stdout))
