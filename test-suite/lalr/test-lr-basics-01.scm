;;; test-lr-basics-01.scm --
;;
;;A grammar that only accept a single terminal as input.  It refuses the
;;end-of-input as first token.
;;

(load "common-test.scm")

(define (doit . tokens)
  (let* ((lexer		(make-lexer tokens))
	 (parser	(lalr-parser (expect: 0)
				     (A)
				     (e (A) : $1))))
    (parser lexer error-handler)))

(check
    (doit (make-lexical-token 'A #f 1))
  => 1)

(check
    (let ((r (doit)))
      (cons r *error*))
  => '(#f (error-handler "Syntax error: unexpected end of input")))

(check
    ;;Parse correctly the first A  and reduce it.  The second A triggers
    ;;an  error which  empties  the  stack and  consumes  all the  input
    ;;tokens.   Finally, an  unexpected end-of-input  error  is returned
    ;;because EOI is invalid as first token after the start.
    (let ((r (doit (make-lexical-token 'A #f 1)
		   (make-lexical-token 'A #f 2)
		   (make-lexical-token 'A #f 3))))
      (cons r *error*))
  => '(#f
       (error-handler "Syntax error: unexpected end of input")
       (error-handler "Syntax error: unexpected token : " . A)))

;;; end of file
