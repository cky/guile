;;;; getopt-gnu-style.scm --- command-line argument parsing functions
;;;;
;;;; author:    russ mcmanus
;;;; Id: getopt-gnu-style.scm,v 1.5 1998/01/05 17:28:45 mcmanr Exp 

(define-module (ice-9 getopt-gnu-style))

(define (split-arg-list arg-ls)
  "Given an arg-ls, decide which part to process for options.  
Everything before an arg of \"--\" is fair game, everything 
after it should not be processed.  the \"--\" is discarded.
A cons pair is returned whose car is the list to process for
options, and whose cdr is the list to not process."
  (let loop ((process-ls '())
	     (not-process-ls arg-ls))
    (cond ((null? not-process-ls)
	   (cons process-ls '()))
	  ((equal? "--" (car not-process-ls))
	   (cons process-ls (cdr not-process-ls)))
	  (#t
	   (loop (cons (car not-process-ls) process-ls)
		 (cdr not-process-ls))))))

(define arg-rx (make-regexp "^--[^=]+="))
(define no-arg-rx (make-regexp "^--[^=]+$"))

(define (getopt-gnu-style arg-ls)
  "Parse a list of program arguments into an alist of option descriptions.

Each item in the list of program arguments is examined to see if it
meets the syntax of a GNU long-named option.  An argument like
`--MUMBLE' produces an element of the form (MUMBLE . #t) in the
returned alist, where MUMBLE is a keyword object with the same name as
the argument.  An argument like `--MUMBLE=FROB' produces an element of
the form (MUMBLE . FROB), where FROB is a string.

As a special case, the returned alist also contains a pair whose car
is the symbol `rest'.  The cdr of this pair is a list containing all
the items in the argument list that are not options of the form
mentioned above.

The argument `--' is treated specially: all items in the argument list
appearing after such an argument are not examined, and are returned in
the special `rest' list.

This function does not parse normal single-character switches.  You
will need to parse them out of the `rest' list yourself."
  (let* ((pair (split-arg-list arg-ls))
	 (eligible-arg-ls (car pair))
	 (ineligible-arg-ls (cdr pair)))
    (let loop ((arg-ls eligible-arg-ls)
	       (alist (list (cons 'rest ineligible-arg-ls))))
      (if (null? arg-ls) alist
	  (let ((first (car arg-ls))
		(rest (cdr arg-ls))
		(result #f))
	    (cond ((begin (set! result (regexp-exec arg-rx first)) result)
		   (loop rest 
			 (cons (cons (symbol->keyword 
				      (string->symbol
				       (substring first 2 (- (cdr (vector-ref result 1)) 1))))
				     (substring first (cdr (vector-ref result 1))))
			       alist)))
		  ((begin (set! result (regexp-exec no-arg-rx first)) result)
		   (loop rest
			 (cons (cons (symbol->keyword
				      (string->symbol
				       (substring first 2 (cdr (vector-ref result 1)))))
				     #t)
			       alist)))
		  (#t
		   (let ((pair (assq 'rest alist)))
		     (set-cdr! pair (cons first (cdr pair)))
		     (loop rest alist)))))))))

(define-public getopt-gnu-style getopt-gnu-style)
