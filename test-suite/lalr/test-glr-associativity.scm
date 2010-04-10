;;; test-glr-associativity.scm
;;
;;With the GLR parser both  the terminal precedence and the non-terminal
;;associativity  are  not  respected;  rather they  generate  two  child
;;processes.
;;

(load "common-test.scm")

(define parser
  (lalr-parser
   (driver: glr)
   (expect: 0)

   (N LPAREN RPAREN
	(left: + -)
	(right: * /)
	(nonassoc: uminus))

   (output	(expr)			: $1)
   (expr	(expr + expr)           : (list $1 '+ $3)
		(expr - expr)           : (list $1 '- $3)
		(expr * expr)           : (list $1 '* $3)
		(expr / expr)           : (list $1 '/ $3)
		(- expr (prec: uminus)) : (list '- $2)
		(N)                   : $1
		(LPAREN expr RPAREN)    : $2)))

(define (doit . tokens)
  (parser (make-lexer tokens) error-handler))

;;; --------------------------------------------------------------------

;;Remember that the result of the GLR  driver is a list of parses, not a
;;single parse.

(check
    (doit (make-lexical-token 'N #f 1))
  => '(1))

(check
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token '+ #f '+)
	  (make-lexical-token 'N #f 2))
  => '((1 + 2)))

(check
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token '* #f '*)
	  (make-lexical-token 'N #f 2))
  => '((1 * 2)))

(check
    (doit (make-lexical-token '- #f '-)
	  (make-lexical-token 'N #f 1))
  => '((- 1)))

(check
    (doit (make-lexical-token '- #f '-)
	  (make-lexical-token '- #f '-)
	  (make-lexical-token 'N #f 1))
  => '((- (- 1))))

(check
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token '+ #f '+)
	  (make-lexical-token '- #f '-)
	  (make-lexical-token 'N #f 2))
  => '((1 + (- 2))))

;;; --------------------------------------------------------------------

(check
    ;;left-associativity
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token '+ #f '+)
	  (make-lexical-token 'N #f 2)
	  (make-lexical-token '+ #f '+)
	  (make-lexical-token 'N #f 3))
  => '(((1 + 2) + 3)))

(check
    ;;right-associativity
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token '* #f '*)
	  (make-lexical-token 'N #f 2)
	  (make-lexical-token '* #f '*)
	  (make-lexical-token 'N #f 3))
  => '(((1 * 2) * 3)
       (1 * (2 * 3))))

(check
    ;;precedence
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token '+ #f '+)
	  (make-lexical-token 'N #f 2)
	  (make-lexical-token '* #f '*)
	  (make-lexical-token 'N #f 3))
  => '(((1 + 2) * 3)
       (1 + (2 * 3))))

;;; end of file
