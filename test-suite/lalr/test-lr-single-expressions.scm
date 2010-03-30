;;; test-lr-single-expressions.scm --
;;
;;Grammar accepting single expressions.
;;

(load "common-test.scm")

(define (doit . tokens)
  (let ((parser (lalr-parser (expect: 0)
			     (N O C (left: A) (left: M) (nonassoc: U))

			     (E	(N)		: $1
				(E A E)		: ($2 $1 $3)
				(E M E)		: ($2 $1 $3)
				(A E (prec: U))	: ($1 $2)
				(O E C)		: $2))))
    (parser (make-lexer tokens) error-handler)))

;;; --------------------------------------------------------------------

(check	;correct input
    (doit (make-lexical-token 'N #f 1))
  => 1)

(check	;correct input
    (doit (make-lexical-token 'A #f -)
	  (make-lexical-token 'N #f 1))
  => -1)

(check	;correct input
    (doit (make-lexical-token 'A #f +)
	  (make-lexical-token 'N #f 1))
  => 1)

(check	;correct input
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token 'A #f +)
	  (make-lexical-token 'N #f 2))
  => 3)

(check	;correct input
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token 'A #f +)
	  (make-lexical-token 'N #f 2)
	  (make-lexical-token 'M #f *)
	  (make-lexical-token 'N #f 3))
  => 7)

(check	;correct input
    (doit (make-lexical-token 'O #f #\()
	  (make-lexical-token 'N #f 1)
	  (make-lexical-token 'A #f +)
	  (make-lexical-token 'N #f 2)
	  (make-lexical-token 'C #f #\))
	  (make-lexical-token 'M #f *)
	  (make-lexical-token 'N #f 3))
  => 9)

;;; end of file
