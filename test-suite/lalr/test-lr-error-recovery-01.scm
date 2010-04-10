;;; test-lr-error-recovery-01.scm --
;;
;;Test error recovery with a terminator terminal.
;;

(load "common-test.scm")

(define (doit . tokens)
  (let ((parser (lalr-parser
		 (expect: 0)
		 (NUMBER BAD NEWLINE)

		 (script	(lines)		: (reverse $1)
				()		: 0)
		 (lines	(lines line)		: (cons $2 $1)
			(line)			: (list $1))
		 (line	(NEWLINE)		: (list 'line $1)
			(NUMBER NEWLINE)	: (list 'line $1 $2)
			(NUMBER NUMBER NEWLINE)	: (list 'line $1 $2 $3)

			;;This semantic  action will cause  "(recover $1
			;;$2)" to be the result of the offending line.
			(error NEWLINE)		: (list 'recover $1 $2)))))
    (parser (make-lexer tokens) error-handler)))

;;; --------------------------------------------------------------------
;;; No errors, grammar tests.

(check
    (doit)
  => 0)

(check
    (doit (make-lexical-token 'NEWLINE #f #\newline))
  => '((line #\newline)))

(check
    (doit (make-lexical-token 'NUMBER  #f 1)
	  (make-lexical-token 'NEWLINE #f #\newline))
  => '((line 1 #\newline)))

(check
    (doit (make-lexical-token 'NUMBER  #f 1)
	  (make-lexical-token 'NUMBER  #f 2)
	  (make-lexical-token 'NEWLINE #f #\newline))
  => '((line 1 2 #\newline)))

(check
    (doit (make-lexical-token 'NUMBER  #f 1)
	  (make-lexical-token 'NEWLINE #f #\newline)
	  (make-lexical-token 'NUMBER  #f 2)
	  (make-lexical-token 'NEWLINE #f #\newline))
  => '((line 1 #\newline)
       (line 2 #\newline)))

(check
    (doit (make-lexical-token 'NUMBER  #f 1)
	  (make-lexical-token 'NEWLINE #f #\newline)
	  (make-lexical-token 'NUMBER  #f 2)
	  (make-lexical-token 'NEWLINE #f #\newline)
	  (make-lexical-token 'NUMBER  #f 3)
	  (make-lexical-token 'NEWLINE #f #\newline))
  => '((line 1 #\newline)
       (line 2 #\newline)
       (line 3 #\newline)))

(check
    (doit (make-lexical-token 'NUMBER  #f 1)
	  (make-lexical-token 'NEWLINE #f #\newline)
	  (make-lexical-token 'NUMBER  #f 2)
	  (make-lexical-token 'NEWLINE #f #\newline)
	  (make-lexical-token 'NUMBER  #f 3)
	  (make-lexical-token 'NEWLINE #f #\newline)
	  (make-lexical-token 'NUMBER  #f 41)
	  (make-lexical-token 'NUMBER  #f 42)
	  (make-lexical-token 'NEWLINE #f #\newline))
  => '((line 1 #\newline)
       (line 2 #\newline)
       (line 3 #\newline)
       (line 41 42 #\newline)))

;;; --------------------------------------------------------------------
;;; Successful error recovery.

(check
    ;;The BAD triggers an error,  recovery happens, the first NEWLINE is
    ;;correctly parsed as recovery token; the second line is correct.
    (let ((r (doit (make-lexical-token 'NUMBER  #f 1)
		   (make-lexical-token 'BAD      #f 'alpha)
		   (make-lexical-token 'NEWLINE #f #\newline)
		   (make-lexical-token 'NUMBER  #f 2)
		   (make-lexical-token 'NEWLINE #f #\newline))))
      (cons r *error*))
  => '(((recover #f #f)
	(line 2 #\newline))
       (error-handler "Syntax error: unexpected token : " . BAD)))


(check
    ;;The  first BAD triggers  an error,  recovery happens  skipping the
    ;;second  and   third  BADs,  the  first  NEWLINE   is  detected  as
    ;;synchronisation token; the second line is correct.
    (let ((r (doit (make-lexical-token 'NUMBER  #f 1)
		   (make-lexical-token 'BAD     #f 'alpha)
		   (make-lexical-token 'BAD     #f 'beta)
		   (make-lexical-token 'BAD     #f 'delta)
		   (make-lexical-token 'NEWLINE #f #\newline)
		   (make-lexical-token 'NUMBER  #f 2)
		   (make-lexical-token 'NEWLINE #f #\newline))))
      (cons r *error*))
  => '(((recover #f #f)
	(line 2 #\newline))
       (error-handler "Syntax error: unexpected token : " . BAD)))

;;; --------------------------------------------------------------------
;;; Failed error recovery.

(check
    ;;End-of-input is found after NUMBER.
    (let ((r (doit (make-lexical-token 'NUMBER  #f 1))))
      (cons r *error*))
  => '(#f (error-handler "Syntax error: unexpected end of input")))

(check
    ;;The BAD triggers  the error, the stack is rewind  up to the start,
    ;;then end-of-input  happens while trying  to skip tokens  until the
    ;;synchronisation one is found.  End-of-input is an acceptable token
    ;;after the start.
    (let ((r (doit (make-lexical-token 'NUMBER  #f 1)
		   (make-lexical-token 'BAD     #f 'alpha)
		   (make-lexical-token 'BAD     #f 'beta)
		   (make-lexical-token 'BAD     #f 'delta))))
      (cons r *error*))
  => '(0 (error-handler "Syntax error: unexpected token : " . BAD)))

(check
    ;;The BAD triggers  the error, the stack is rewind  up to the start,
    ;;then end-of-input  happens while trying  to skip tokens  until the
    ;;synchronisation one is found.  End-of-input is an acceptable token
    ;;after the start.
    (let ((r (doit (make-lexical-token 'BAD #f 'alpha))))
      (cons r *error*))
  => '(0 (error-handler "Syntax error: unexpected token : " . BAD)))

;;; end of file
