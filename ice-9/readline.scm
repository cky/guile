;;;; readline.scm --- support functions for command-line editing
;;;;
;;;; 	Copyright (C) 1997 Free Software Foundation, Inc.
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
;;;; Contributed by Daniel Risacher <risacher@worldnet.att.net>.
;;;; Extensions based upon code by
;;;; Andrew Archibald <aarchiba@undergrad.math.uwaterloo.ca>.

(define-module (ice-9 readline)
  :use-module (ice-9 session))

(define prompt "")

(define (make-readline-port)
  (let ((read-string "")
	(string-index -1))
    (letrec ((get-character
	      (lambda ()
		(cond 
		 ((eof-object? read-string)
		  read-string)
		 ((>= string-index (string-length read-string))
		  (begin
		    (set! string-index -1)
		    #\nl))
		 ((= string-index -1)
		  (begin
		    (set! read-string (readline (if (string? prompt)
						    prompt
						    (prompt))))
		    (set! string-index 0)
		    (if (not (eof-object? read-string))
			(begin
			  (or (string=? read-string "")
			      (add-history read-string))
			  (get-character))
			read-string)))
		 (else 
		  (let ((res (string-ref read-string string-index)))
		    (set! string-index (+ 1 string-index))
		    res))))))	      
      (make-soft-port
       (vector write-char display #f get-character #f)
       "rw"))))

;;; We only create one readline port.  There's no point in having
;;; more, since they would all share the tty and history ---
;;; everything except the prompt.  And don't forget the
;;; compile/load/run phase distinctions.
(define the-readline-port #f)

(define-public (readline-port)
  (if (not the-readline-port)
      (set! the-readline-port (make-readline-port)))
  the-readline-port)

(define-public (set-readline-prompt! p)
  (set! prompt p))

(define-public apropos-completion-function
  (let ((completions '()))
    (lambda (text cont?)
      (if (not cont?)
	  (set! completions
		(map symbol->string
		     (apropos-internal (string-append "^" text)))))
      (if (null? completions)
	  #f
	  (let ((retval (car completions)))
	    (begin (set! completions (cdr completions))
		   retval))))))

(set! *readline-completion-function* apropos-completion-function)

;(define myport (make-readline-port))
;(define (doit)
;  (set-current-input-port myport))
