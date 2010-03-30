;;; test-lr-basics-02.scm --
;;
;;A grammar that only accept a single terminal or the EOI.
;;

(load "common-test.scm")

(define (doit . tokens)
  (let ((parser (lalr-parser (expect: 0)
			     (driver: glr)
			     (A)
			     (e (A) : $1
				()  : 0))))
    (parser (make-lexer tokens) error-handler)))

(check
    (doit)
  => '(0))

(check
    (doit (make-lexical-token 'A #f 1))
  => '(1))

(check
    (doit (make-lexical-token 'A #f 1)
	  (make-lexical-token 'A #f 2)
	  (make-lexical-token 'A #f 3))
  => '())

;;; end of file
