;;; test-lr-script-expression.scm --
;;
;;Parse scripts, each line an expression.
;;

(load "common-test.scm")

(define (doit . tokens)
  (let ((parser (lalr-parser (expect: 0)
			     (driver: glr)
			     (N O C T (left: A) (left: M) (nonassoc: U))

			     (script	(lines)		: (reverse $1))

			     (lines	(lines line)	: (cons $2 $1)
					(line)		: (list $1))

			     (line	(T)		: #\newline
					(E T)		: $1
					(error T)	: (list 'error-clause $2))

			     (E	(N)		: $1
				(E A E)		: ($2 $1 $3)
				(E M E)		: ($2 $1 $3)
				(A E (prec: U))	: ($1 $2)
				(O E C)		: $2))))
    (parser (make-lexer tokens) error-handler)))

;;; --------------------------------------------------------------------
;;; Correct input

(check
    (doit (make-lexical-token 'T #f #\newline))
  => '((#\newline)))

(check
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token 'T #f #\newline))
  => '((1)))

(check
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token 'A #f +)
	  (make-lexical-token 'N #f 2)
	  (make-lexical-token 'T #f #\newline))
  => '((3)))

(check
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token 'A #f +)
	  (make-lexical-token 'N #f 2)
	  (make-lexical-token 'M #f *)
	  (make-lexical-token 'N #f 3)
	  (make-lexical-token 'T #f #\newline))
  => '((9) (7)))

(check
    (doit (make-lexical-token 'N #f 10)
	  (make-lexical-token 'M #f *)
	  (make-lexical-token 'N #f 2)
	  (make-lexical-token 'A #f +)
	  (make-lexical-token 'N #f 3)
	  (make-lexical-token 'T #f #\newline))
  => '((23)))

(check
    (doit (make-lexical-token 'O #f #\()
	  (make-lexical-token 'N #f 1)
	  (make-lexical-token 'A #f +)
	  (make-lexical-token 'N #f 2)
	  (make-lexical-token 'C #f #\))
	  (make-lexical-token 'M #f *)
	  (make-lexical-token 'N #f 3)
	  (make-lexical-token 'T #f #\newline))
  => '((9)))

(check
    (doit (make-lexical-token 'O #f #\()
	  (make-lexical-token 'N #f 1)
	  (make-lexical-token 'A #f +)
	  (make-lexical-token 'N #f 2)
	  (make-lexical-token 'C #f #\))
	  (make-lexical-token 'M #f *)
	  (make-lexical-token 'N #f 3)
	  (make-lexical-token 'T #f #\newline)

	  (make-lexical-token 'N #f 4)
	  (make-lexical-token 'M #f /)
	  (make-lexical-token 'N #f 5)
	  (make-lexical-token 'T #f #\newline))
  => '((9 4/5)))

;;; --------------------------------------------------------------------

(check
    ;;Successful error recovery.
    (doit (make-lexical-token 'O #f #\()
	  (make-lexical-token 'N #f 1)
	  (make-lexical-token 'A #f +)
	  (make-lexical-token 'N #f 2)
	  (make-lexical-token 'M #f *)
	  (make-lexical-token 'N #f 3)
	  (make-lexical-token 'T #f #\newline)

	  (make-lexical-token 'N #f 4)
	  (make-lexical-token 'M #f /)
	  (make-lexical-token 'N #f 5)
	  (make-lexical-token 'T #f #\newline))
  => '())

(check
    ;;Unexpected end of input.
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token 'A #f +)
	  (make-lexical-token 'N #f 2))
  => '())

(check
    ;;Unexpected end of input.
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token 'A #f +)
	  (make-lexical-token 'T #f #\newline))
  => '())

;;; end of file
