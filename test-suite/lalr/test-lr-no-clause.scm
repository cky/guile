;;; test-lr-no-clause.scm --
;;

(load "common-test.scm")

(define (doit . tokens)
  (let ((parser (lalr-parser (expect: 0)
			     (NUMBER COMMA NEWLINE)

			     (lines (lines line)	: (list $2)
				    (line)		: (list $1))
			     (line (NEWLINE)		: #\newline
				   (NUMBER NEWLINE)	: $1
				   ;;this is a rule with no semantic action
				   (COMMA NUMBER NEWLINE)))))
    (parser (make-lexer tokens) error-handler)))

(check
    ;;correct input
    (doit (make-lexical-token 'NUMBER  #f 1)
	  (make-lexical-token 'NEWLINE #f #\newline))
  => '(1))

(check
    ;;correct input with comma, which is a rule with no client form
    (doit (make-lexical-token 'COMMA   #f #\,)
	  (make-lexical-token 'NUMBER  #f 1)
	  (make-lexical-token 'NEWLINE #f #\newline))
  => '(#(line-3 #\, 1 #\newline)))

(check
    ;;correct input with comma, which is a rule with no client form
    (doit (make-lexical-token 'NUMBER  #f 1)
	  (make-lexical-token 'NEWLINE #f #\newline)
	  (make-lexical-token 'COMMA   #f #\,)
	  (make-lexical-token 'NUMBER  #f 2)
	  (make-lexical-token 'NEWLINE #f #\newline))
  => '(#(line-3 #\, 2 #\newline)))

;;; end of file
