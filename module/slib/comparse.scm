;;; "comparse.scm" Break command line into arguments.
;Copyright (C) 1995, 1997 Aubrey Jaffer
;
;Permission to copy this software, to redistribute it, and to use it
;for any purpose is granted, subject to the following restrictions and
;understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;;;; This is a simple command-line reader.  It could be made fancier
;;; to handle lots of `shell' syntaxes.

;;; Albert L. Ting points out that a similar process can be used for
;;; reading files of options -- therefore READ-OPTIONS-FILE.

(require 'string-port)
(define (read-command-from-port port nl-term?)
  (define argv '())
  (define obj "")
  (define chars '())
  (define readc (lambda () (read-char port)))
  (define peekc (lambda () (peek-char port)))
  (define s-expression
    (lambda ()
      (splice-arg (call-with-output-string
		   (lambda (p) (display (slib:eval (read port)) p))))))
  (define backslash
    (lambda (goto)
      (readc)
      (let ((c (readc)))
	(cond ((eqv? #\newline c) (goto (peekc)))
	      ((and (char-whitespace? c) (eqv? #\newline (peekc))
		    (eqv? 13 (char->integer c)))
	       (readc) (goto (peekc)))
	      (else (set! chars (cons c chars)) (build-token (peekc)))))))
  (define loop
    (lambda (c)
      (case c
	((#\\) (backslash loop))
	((#\") (splice-arg (read port)))
	((#\( #\') (s-expression))
	((#\#) (do ((c (readc) (readc)))
		   ((or (eof-object? c) (eqv? #\newline c))
		    (if nl-term? c (loop (peekc))))))
	((#\;) (readc))
	((#\newline) (readc) (and (not nl-term?) (loop (peekc))))
	(else (cond ((eof-object? c) c)
		    ((char-whitespace? c) (readc) (loop (peekc)))
		    (else (build-token c)))))))
  (define splice-arg
    (lambda (arg)
      (set! obj (string-append obj (list->string (reverse chars)) arg))
      (set! chars '())
      (build-token (peekc))))
  (define buildit
    (lambda ()
      (readc)
      (set! argv (cons (string-append obj (list->string (reverse chars)))
		       argv))))
  (define build-token
    (lambda (c)
      (case c
	((#\") (splice-arg (read port)))
	((#\() (s-expression))
	((#\\) (backslash build-token))
	((#\;) (buildit))
	(else (cond ((or (eof-object? c) (char-whitespace? c))
		     (buildit)
		     (cond ((not (and nl-term? (eqv? c #\newline)))
			    (set! obj "")
			    (set! chars '())
			    (loop (peekc)))))
		    (else (set! chars (cons (readc) chars))
			  (build-token (peekc))))))))
  (let ((c (loop (peekc))))
    (cond ((and (null? argv) (eof-object? c)) c)
	  (else (reverse argv)))))

(define (read-command . port)
  (read-command-from-port (cond ((null? port) (current-input-port))
				((= 1 (length port)) (car port))
				(else
				 (slib:error 'read-command
					     "Wrong Number of ARGs:" port)))
			  #t))

(define (read-options-file filename)
  (call-with-input-file filename
    (lambda (port) (read-command-from-port port #f))))
