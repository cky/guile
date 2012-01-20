;;; common-test.scm --
;;;

;; Slightly modified for Guile by Ludovic Courtès <ludo@gnu.org>, 2010.

(use-modules (system base lalr)
             (ice-9 pretty-print))

(define *error* '())

(define-syntax check
  (syntax-rules (=>)
    ((_ ?expr => ?expected-result)
     (check ?expr (=> equal?) ?expected-result))

    ((_ ?expr (=> ?equal) ?expected-result)
     (let ((result	?expr)
	   (expected	?expected-result))
       (set! *error* '())
       (when (not (?equal result expected))
	 (display "Failed test: \n")
	 (pretty-print (quote ?expr))(newline)
	 (display "\tresult was: ")
	 (pretty-print result)(newline)
	 (display "\texpected: ")
	 (pretty-print expected)(newline)
         (exit 1))))))

;;; --------------------------------------------------------------------

(define (display-result v)
  (if v
      (begin
        (display "==> ")
        (display v)
        (newline))))

(define eoi-token
  (make-lexical-token '*eoi* #f #f))

(define (make-lexer tokens)
  (lambda ()
    (if (null? tokens)
	eoi-token
      (let ((t (car tokens)))
	(set! tokens (cdr tokens))
	t))))

(define (error-handler message . args)
  (set! *error* (cons `(error-handler ,message . ,(if (pair? args)
						      (lexical-token-category (car args))
						    '()))
		      *error*))
  (cons message args))

;;; end of file
