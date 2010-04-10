;;; test-lr-basics-04.scm --
;;
;;A grammar  accepting a sequence  of equal tokens of  arbitrary length.
;;The return value is the value of the last parsed token.


(load "common-test.scm")

(define (doit . tokens)
  (let ((parser (lalr-parser (expect: 0)
			     (A)
			     (e (e A) : $2
				(A)   : $1
				()    : 0))))
    (parser (make-lexer tokens) error-handler)))

(check
    (doit)
  => 0)

(check
    (doit (make-lexical-token 'A #f 1))
  => 1)

(check
    (doit (make-lexical-token 'A #f 1)
	  (make-lexical-token 'A #f 2)
	  (make-lexical-token 'A #f 3))
  => 3)

;;; end of file
