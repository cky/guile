;;; test-lr-error-recovery-02.scm --
;;
;;Test error  recovery policy when the synchronisation  terminal has the
;;same category of the lookahead that raises the error.
;;

(load "common-test.scm")

(define (doit . tokens)
  (let ((parser (lalr-parser (expect: 0)
			     (A B C)
			     (alphas (alpha)		: $1
				     (alphas alpha)	: $2)
			     (alpha (A B)	: (list $1 $2)
				    (C)		: $1
				    (error C)	: 'error-form))))
    (parser (make-lexer tokens) error-handler)))

;;; --------------------------------------------------------------------
;;; No error, just grammar tests.

(check
    (doit (make-lexical-token 'A #f 1)
	  (make-lexical-token 'B #f 2))
  => '(1 2))

(check
    (doit (make-lexical-token 'C #f 3))
  => '3)

;;; --------------------------------------------------------------------
;;; Successful error recovery.

(check
    ;;Error, recovery, end-of-input.
    (let ((r (doit (make-lexical-token 'A #f 1)
		   (make-lexical-token 'C #f 3))))
      (cons r *error*))
  => '(error-form (error-handler "Syntax error: unexpected token : " . C)))

(check
    ;;Error, recovery, correct parse of "A B".
    (let ((r (doit (make-lexical-token 'A #f 1)
		   (make-lexical-token 'C #f 3)
		   (make-lexical-token 'A #f 1)
		   (make-lexical-token 'B #f 2))))
      (cons r *error*))
  => '((1 2)
       (error-handler "Syntax error: unexpected token : " . C)))

;;; end of file
