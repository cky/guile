;;; test-lr-basics-03.scm --
;;
;;A grammar  that accepts  fixed sequences of  a single terminal  or the
;;EOI.

(load "common-test.scm")

(define (doit . tokens)
  (let ((parser (lalr-parser (expect: 0)
			     (driver: glr)
			     (A)
			     (e (A)     : (list $1)
				(A A)   : (list $1 $2)
				(A A A) : (list $1 $2 $3)
				()      : 0))))
    (parser (make-lexer tokens) error-handler)))

(check
    (doit (make-lexical-token 'A #f 1))
  => '((1)))

(check
    (doit (make-lexical-token 'A #f 1)
	  (make-lexical-token 'A #f 2))
  => '((1 2)))

(check
    (doit (make-lexical-token 'A #f 1)
	  (make-lexical-token 'A #f 2)
	  (make-lexical-token 'A #f 3))
  => '((1 2 3)))

(check
    (doit)
  => '(0))

;;; end of file
