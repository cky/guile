; "repl.scm", read-eval-print-loop for Scheme
; Copyright (c) 1993, Aubrey Jaffer
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

(require 'dynamic-wind)
(define (repl:quit) (slib:error "not in repl:repl"))

(define (repl:top-level repl:eval)
  (repl:repl (lambda () (display "> ")
		     (force-output (current-output-port))
		     (read))
	     repl:eval
	     (lambda objs
	       (cond ((null? objs))
		     (else
		      (write (car objs))
		      (for-each (lambda (obj)
				  (display " ;") (newline) (write obj))
				(cdr objs))))
	       (newline))))

(define (repl:repl repl:read repl:eval repl:print)
  (let* ((old-quit repl:quit)
	 (old-error slib:error)
	 (old-eval slib:eval)
	 (old-load load)
	 (repl:load (lambda (<pathname>)
		      (call-with-input-file <pathname>
			(lambda (port)
			  (let ((old-load-pathname *load-pathname*))
			    (set! *load-pathname* <pathname>)
			    (do ((o (read port) (read port)))
				((eof-object? o))
			      (repl:eval o))
			    (set! *load-pathname* old-load-pathname))))))
	 (repl:restart #f)
	 (values? (provided? 'values))
	 (has-char-ready? (provided? 'char-ready?))
	 (repl:error (lambda args (require 'debug) (apply qpn args)
			     (repl:restart #f))))
    (dynamic-wind
     (lambda ()
       (set! load repl:load)
       (set! slib:eval repl:eval)
       (set! slib:error repl:error)
       (set! repl:quit
	     (lambda () (let ((cont repl:restart))
			  (set! repl:restart #f)
			  (cont #t)))))
     (lambda ()
       (do () ((call-with-current-continuation
		(lambda (cont)
		  (set! repl:restart cont)
		  (do ((obj (repl:read) (repl:read)))
		      ((eof-object? obj) (repl:quit))
		    (cond
		     (has-char-ready?
		      (let loop ()
			(cond ((char-ready?)
			       (let ((c (peek-char)))
				 (cond
				  ((eof-object? c))
				  ((char=? #\newline c) (read-char))
				  ((char-whitespace? c)
				   (read-char) (loop))
				  (else (newline)))))))))
		    (if values?
			(call-with-values (lambda () (repl:eval obj))
					  repl:print)
			(repl:print (repl:eval obj)))))))))
     (lambda () (cond (repl:restart
		       (display ">>ERROR<<") (newline)
		       (repl:restart #f)))
	     (set! load old-load)
	     (set! slib:eval old-eval)
	     (set! slib:error old-error)
	     (set! repl:quit old-quit)))))
