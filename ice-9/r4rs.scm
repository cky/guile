;;;; r4rs.scm --- definitions needed for libguile to be R4RS compliant
;;;; Jim Blandy <jimb@cyclic.com> --- October 1996

;;;; 	Copyright (C) 1996 Free Software Foundation, Inc.
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


;;;; apply and call-with-current-continuation

;;; We want these to be tail-recursive, so instead of using primitive
;;; procedures, we define them as closures in terms of the primitive
;;; macros @apply and @call-with-current-continuation.
(set! apply (lambda (fun . args) (@apply fun (apply:nconc2last args))))
(define (call-with-current-continuation proc)
  (@call-with-current-continuation proc))


;;;; Basic Port Code

;;; Specifically, the parts of the low-level port code that are written in 
;;; Scheme rather than C.
;;;
;;; WARNING: the parts of this interface that refer to file ports
;;; are going away.   It would be gone already except that it is used
;;; "internally" in a few places.


;; OPEN_READ, OPEN_WRITE, and OPEN_BOTH are used to request the proper
;; mode to open files in.  MSDOS does carraige return - newline
;; translation if not opened in `b' mode.
;;
(define OPEN_READ (case (software-type)
		    ((MS-DOS WINDOWS ATARIST) "rb")
		    (else "r")))
(define OPEN_WRITE (case (software-type)
		     ((MS-DOS WINDOWS ATARIST) "wb")
		     (else "w")))
(define OPEN_BOTH (case (software-type)
		    ((MS-DOS WINDOWS ATARIST) "r+b")
		    (else "r+")))

(define *null-device* "/dev/null")

(define (open-input-file str)
  (open-file str OPEN_READ))

(define (open-output-file str)
  (open-file str OPEN_WRITE))

(define (open-io-file str) (open-file str OPEN_BOTH))
(define close-input-port close-port)
(define close-output-port close-port)
(define close-io-port close-port)

(define (call-with-input-file str proc)
  (let* ((file (open-input-file str))
	 (ans (proc file)))
    (close-input-port file)
    ans))

(define (call-with-output-file str proc)
  (let* ((file (open-output-file str))
	 (ans (proc file)))
    (close-output-port file)
    ans))

(define (with-input-from-port port thunk)
  (let* ((swaports (lambda () (set! port (set-current-input-port port)))))
    (dynamic-wind swaports thunk swaports)))

(define (with-output-to-port port thunk)
  (let* ((swaports (lambda () (set! port (set-current-output-port port)))))
    (dynamic-wind swaports thunk swaports)))

(define (with-error-to-port port thunk)
  (let* ((swaports (lambda () (set! port (set-current-error-port port)))))
    (dynamic-wind swaports thunk swaports)))

(define (with-input-from-file file thunk)
  (let* ((nport (open-input-file file))
	 (ans (with-input-from-port nport thunk)))
    (close-port nport)
    ans))

(define (with-output-to-file file thunk)
  (let* ((nport (open-output-file file))
	 (ans (with-output-to-port nport thunk)))
    (close-port nport)
    ans))

(define (with-error-to-file file thunk)
  (let* ((nport (open-output-file file))
	 (ans (with-error-to-port nport thunk)))
    (close-port nport)
    ans))

(define (with-input-from-string string thunk)
  (call-with-input-string string
   (lambda (p) (with-input-from-port p thunk))))

(define (with-output-to-string thunk)
  (call-with-output-string
   (lambda (p) (with-output-to-port p thunk))))

(define (with-error-to-string thunk)
  (call-with-output-string
   (lambda (p) (with-error-to-port p thunk))))

(define the-eof-object (call-with-input-string "" (lambda (p) (read-char p))))


;;;; Loading

(if (not (defined? '%load-verbosely))
    (define %load-verbosely #f))
(define (assert-load-verbosity v) (set! %load-verbosely v))

(define (%load-announce file)
  (if %load-verbosely
      (with-output-to-port (current-error-port)
	(lambda ()
	  (display ";;; ")
	  (display "loading ")
	  (display file)
	  (newline)
	  (force-output)))))

(set! %load-hook %load-announce)

;;; If we load boot-9.scm, it provides a definition for this which is
;;; more sophisticated.
(define read-sharp #f)

(define (load name)
  (start-stack 'load-stack
	       (primitive-load name #t read-sharp)))
