;;; test-lr-basics-05.scm --
;;
;;A grammar  accepting a sequence  of equal tokens of  arbitrary length.
;;The return value is the list of values.
;;

(load "common-test.scm")

(define (doit . tokens)
  (let ((parser (lalr-parser (expect: 0)
			     (driver: glr)
			     (A)
			     (e (e A) : (cons $2 $1)
				(A)   : (list $1)
				()    : (list 0)))))
    (parser (make-lexer tokens) error-handler)))

(check
    (doit)
  => '((0)))

(check
    (doit (make-lexical-token 'A #f 1))
  => '((1 0)
       (1)))

(check
    (doit (make-lexical-token 'A #f 1)
	  (make-lexical-token 'A #f 2))
  => '((2 1 0)
       (2 1)))

(check
    (doit (make-lexical-token 'A #f 1)
	  (make-lexical-token 'A #f 2)
	  (make-lexical-token 'A #f 3))
  => '((3 2 1 0)
       (3 2 1)))

;;; end of file
