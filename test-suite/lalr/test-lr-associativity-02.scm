;;; test-lr-associativity-02.scm --
;;
;;Show  how  to use  left  and  right  associativity.  Notice  that  the
;;terminal  M  is declared  as  left  associative;  this influences  the
;;binding  of values to  the $n  symbols in  the semantic  clauses.  The
;;semantic clause in the rule:
;;
;;  (E M E M E)     : (list $1 $2 (list $3 $4 $5))
;;
;;looks like  it is right-associated, but the  result is left-associated
;;because we have declared M as "left:".
;;

(load "common-test.scm")

(define (doit . tokens)
  (let ((parser (lalr-parser
		 (expect: 0)
		 (N (left: A)
		    (left: M)
		    (nonassoc: U))
		 (E	(N)		: $1
			(E A E)         : (list $1 $2 $3)
			(E M E)         : (list $1 $2 $3)
			(E M E M E)     : (list $1 $2 (list $3 $4 $5))
			(A E (prec: U))	: (list '- $2)))))
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

(check
    (doit (make-lexical-token 'A #f '-)
	  (make-lexical-token 'N #f 1))
  => '(- 1))

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
