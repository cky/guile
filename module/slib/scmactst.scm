;;;"scmactst.scm" test syntactic closures macros
;;; From "sc-macro.doc", A Syntactic Closures Macro Facility by Chris Hanson

(define errs '())
(define test
  (lambda (expect fun . args)
    (write (cons fun args))
    (display "  ==> ")
    ((lambda (res)
       (write res)
       (newline)
       (cond ((not (equal? expect res))
	      (set! errs (cons (list res expect (cons fun args)) errs))
	      (display " BUT EXPECTED ")
	      (write expect)
	      (newline)
	      #f)
	     (else #t)))
     (if (procedure? fun) (apply fun args) (car args)))))

(require 'syntactic-closures)

(macro:expand
 '(define-syntax push
    (syntax-rules ()
		  ((push item list)
		   (set! list (cons item list))))))

(test '(set! foo (cons bar foo)) 'push (macro:expand '(push bar foo)))

(macro:expand
 '(define-syntax push1
    (transformer
     (lambda (exp env)
       (let ((item
	      (make-syntactic-closure env '() (cadr exp)))
	     (list
	      (make-syntactic-closure env '() (caddr exp))))
	 `(set! ,list (cons ,item ,list)))))))

(test '(set! foo (cons bar foo)) 'push1 (macro:expand '(push1 bar foo)))

(macro:expand
 '(define-syntax loop
    (transformer
     (lambda (exp env)
       (let ((body (cdr exp)))
	 `(call-with-current-continuation
	   (lambda (exit)
	     (let f ()
	       ,@(map (lambda  (exp)
			(make-syntactic-closure env '(exit)
						exp))
		      body)
	       (f)))))))))

(macro:expand
 '(define-syntax let1
    (transformer
     (lambda (exp env)
       (let ((id (cadr exp))
	     (init (caddr exp))
	     (exp (cadddr exp)))
	 `((lambda (,id)
	     ,(make-syntactic-closure env (list id) exp))
	   ,(make-syntactic-closure env '() init)))))))

(test 93 'let1 (macro:eval '(let1 a 90 (+ a 3))))

(macro:expand
 '(define-syntax loop-until
    (syntax-rules
     ()
     ((loop-until id init test return step)
      (letrec ((loop
		(lambda (id)
		  (if test return (loop step)))))
	(loop init))))))

(test (macro:expand '(letrec ((loop (lambda (foo) (if #t 12 (loop 33)))))
		       (loop 3)))
      'loop
      (macro:expand '(loop-until foo 3 #t 12 33)))

(macro:expand
 '(define-syntax loop-until1
    (transformer
     (lambda (exp env)
       (let ((id (cadr exp))
	     (init (caddr exp))
	     (test (cadddr exp))
	     (return (cadddr (cdr exp)))
	     (step (cadddr (cddr exp)))
	     (close
	      (lambda (exp free)
		(make-syntactic-closure env free exp))))
	 `(letrec ((loop
		    ,(capture-syntactic-environment
		      (lambda (env)
			`(lambda (,id)
			   (,(make-syntactic-closure env '() `if)
			    ,(close test (list id))
			    ,(close return (list id))
			    (,(make-syntactic-closure env '()
						      `loop)
			     ,(close step (list id)))))))))
	    (loop ,(close init '()))))))))

(test (macro:expand '(letrec ((loop (lambda (foo) (if #t 12 (loop 33)))))
			      (loop 3)))
      'loop1
      (macro:expand '(loop-until1 foo 3 #t 12 33)))

(test '#t 'identifier (identifier? 'a))
;;; this needs to setup ENV.
;;;(test '#t 'identifier
;;;      (identifier? (macro:expand (make-syntactic-closure env '() 'a))))
(test #f 'identifier (identifier? "a"))
(test #f 'identifier (identifier? #\a))
(test #f 'identifier (identifier? 97))
(test #f 'identifier (identifier? #f))
(test #f 'identifier (identifier? '(a)))
(test #f 'identifier (identifier? '#(a)))

(test '(#t #f)
      'syntax
      (macro:eval
       '(let-syntax
	    ((foo
	      (transformer
	       (lambda (form env)
		 (capture-syntactic-environment
		  (lambda (transformer-env)
		    (identifier=? transformer-env 'x env 'x)))))))
	  (list (foo)
		(let ((x 3))
		  (foo))))))


(test '(#f #t)
      'syntax
      (macro:eval
       '(let-syntax ((bar foo))
	  (let-syntax
	      ((foo
		(transformer
		 (lambda (form env)
		   (capture-syntactic-environment
		    (lambda (transformer-env)
		      (identifier=? transformer-env 'foo
				    env (cadr form))))))))
	    (list (foo foo)
		  (foo bar))))))

(newline)
(cond ((null? errs) (display "Passed all tests"))
      (else (display "errors were:") (newline)
	    (display "(got expected (call))") (newline)
	    (for-each (lambda (l) (write l) (newline)) errs)))
(newline)
