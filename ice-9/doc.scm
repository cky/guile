;;; $Id: doc.scm,v 1.1 1999-12-13 03:19:11 gjb Exp $
;;;; 	Copyright (C) 1999 Free Software Foundation, Inc.
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
;;;; Written by Sam Steingold, Greg J. Badros, and Maciej Stachowiak
;;;; Ported from Scwm by Greg J. Badros
;;;;
;;;; Hopefully this will just be temporary... --12/12/99 gjb

(define-module (ice-9 doc)
  :use-module (ice-9 regex)
  :use-module (ice-9 optargs))




(define-public (write-all port . lst)
  "Write all arguments into the port. #t means `current-output-port'."
  (if (eq? port #t) (set! port (current-output-port)))
  (do ((zz lst (cdr zz))) ((null? zz))
    (if (string? (car zz)) (display (car zz) port) (write (car zz) port))))

(define-public doc-files
  (list (string-append (library-dir) "/guile-procedures.txt")
	"./guile-procedures.txt"))

(define-public (hook-documentation hook)
  "Return the docstring for HOOK."
  (object-property hook 'doc))

(define-public documentation-debug #f)

(define*-public (documentation func #&optional (port (current-output-port)))
  "Print the documentation for the string or symbol.
Works by searching through the files listed in `doc-files'.
Returns #t if any documentation was found, #f otherwise."
  (let* ((func (if (string? func) func (symbol->string func)))
         (head (string-append "(" func))
         (len (string-length head))
         (delim? (lambda (st) (and (= 1 (string-length st))
                                   (char=? (string-ref st 0) #\np)))))
    (do ((fl doc-files (cdr fl)) (done #f) (fd #f))
        ((or (null? fl) done)
         (if (not done) (write-all port "No documentation for `" func "'\n"))
         done)
      (if documentation-debug (write-all port "trying `" (car fl) "'..."))
      (cond ((file-exists? (car fl))
             (if documentation-debug (display "file exists\n" port))
             (set! fd (open-input-file (car fl)))
             (do ((ln (read-line fd)))
                 ((or (eof-object? ln) done) (close-input-port fd))
               (cond ((and (delim? ln)
                           (begin (set! ln (read-line fd))
                                  (not (eof-object? ln)))
                           (or (and (< len (string-length ln))
                                    (string=? head (substring ln 0 len))
                                    (string-index " )" (string-ref ln len)))
                               (string=? func ln)
			       (and (< (- len 1) (string-length ln))
				    (string=? func (substring ln 0 (- len 1))))))
                      (set! done #t)
                      (display ln port) (newline port)
                      (do ((ln (read-line fd) (read-line fd)))
                          ((delim? ln))
                        (display ln port) (newline port)))
                     ((set! ln (read-line fd))))))
            (documentation-debug (display "file not found\n" port))))))

(define*-public (help obj #&optional (port (current-output-port)))
  "Print all possible documentation for string or symbol."
  (display " *** documentation for `" port)
  (display obj port)
  (display "':\n\n" port)
  (documentation obj port)
  (let ((bb (symbol-binding #f (if (string? obj) (string->symbol obj) obj))))
    (cond ((procedure? bb)
           (display "\n *** procedure-documentation for `" port)
           (display obj port) (display "':\n\n" port)
           (with-output-to-port port
             (lambda () (procedure-documentation bb))))))
  (display "\n\n" port))

(define-public (object-documentation sym)
  "Return documentation attached to SYM or to (eval SYM)."
  (let ((evalsym (catch #t
			(lambda () (eval sym))
			(lambda (key . args)
			  #f))))
    (cond
     ((procedure? evalsym) 
      (procedure-documentation evalsym))
     ((hook? evalsym)
      (hook-documentation evalsym))
     (else (scwm-option-documentation sym)))))
      

;; (proc-doc get-window)
;; (procedure-name get-window)
;; (procedure-properties get-window)
;; (procedure-property get-window 'documentation)
;; (procedure-property window-frame-size 'documentation)
;; (proc-doc get-window)
;; (proc-doc window-frame-size)
;; (set-procedure-property! get-window 'documentation "foo")
(define-public (proc-doc proc)
  "Return documentation for PROC."
  (or (procedure-documentation proc)
      (procedure-property proc 'documentation)
      (let* ((docstring (with-output-to-string
			 (lambda () (documentation 
				     (procedure-name proc)))))
	     (len (string-length docstring)))
	;; GJB:FIXME:: remove extra newline
	(set! docstring (substring docstring 0 (- len 1)))
	(set-procedure-property! proc 'documentation docstring)
	docstring)))


;; For testing...
;; (documentation "window-position")
;; (documentation "make-cl-constraint")
;; (apropos-internal "")
;; (substring (proc-doc get-window) 0 (- (string-length (proc-doc get-window)) 2))
