;;; test-lr-associativity-04.scm --
;;
;;Show how to use associativity.
;;

(load "common-test.scm")

(define (doit . tokens)
  (let ((parser (lalr-parser
		 (expect: 0)
		 (N (left: A)
		    (left: M))
		 (E	(N)		: $1

			(E A E)         : (list $1 $2 $3)
			(E A E A E)     : (list (list $1 $2 $3) $4 $5)

			(E M E)         : (list $1 $2 $3)
			(E M E M E)     : (list $1 $2 (list $3 $4 $5))

			(E A E M E)     : (list $1 $2 $3 $4 $5)
			(E M E A E)     : (list $1 $2 $3 $4 $5)
			))))
    (parser (make-lexer tokens) error-handler)))

;;; --------------------------------------------------------------------
;;; Single operator.

(check
    (doit (make-lexical-token 'N #f 1))
  => 1)

(check
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token 'A #f '+)
	  (make-lexical-token 'N #f 2))
  => '(1 + 2))

(check
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token 'M #f '*)
	  (make-lexical-token 'N #f 2))
  => '(1 * 2))

;;; --------------------------------------------------------------------
;;; Precedence.

(check
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token 'A #f '+)
	  (make-lexical-token 'N #f 2)
	  (make-lexical-token 'M #f '*)
	  (make-lexical-token 'N #f 3))
  => '(1 + (2 * 3)))

(check
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token 'M #f '*)
	  (make-lexical-token 'N #f 2)
	  (make-lexical-token 'A #f '+)
	  (make-lexical-token 'N #f 3))
  => '((1 * 2) + 3))

;;; --------------------------------------------------------------------
;;; Associativity.

(check
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token 'A #f '+)
	  (make-lexical-token 'N #f 2)
	  (make-lexical-token 'A #f '+)
	  (make-lexical-token 'N #f 3))
  => '((1 + 2) + 3))

(check
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token 'M #f '*)
	  (make-lexical-token 'N #f 2)
	  (make-lexical-token 'M #f '*)
	  (make-lexical-token 'N #f 3))
  => '((1 * 2) * 3))

;;; end of file
