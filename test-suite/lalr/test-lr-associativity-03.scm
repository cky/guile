;;; test-lr-associativity-01.scm --
;;
;;Show  how  to use  left  and  right  associativity.  Notice  that  the
;;terminal M is declared as non-associative; this influences the binding
;;of values  to the  $n symbols in  the semantic clauses.   The semantic
;;clause in the rule:
;;
;;  (E M E M E)     : (list $1 $2 (list $3 $4 $5))
;;
;;looks like it is right-associated,  and it is because we have declared
;;M as "right:".
;;

(load "common-test.scm")

(define (doit . tokens)
  (let ((parser (lalr-parser
		 (expect: 0)
		 (N (nonassoc: A)
		    (nonassoc: M))
		 (E	(N)		: $1
			(E A E)         : (list $1 $2 $3)
			(E A E A E)     : (list (list $1 $2 $3) $4 $5)
			(E M E)         : (list $1 $2 $3)
			(E M E M E)     : (list $1 $2 (list $3 $4 $5))))))
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
  => '(1 * (2 * 3)))

;;; end of file
