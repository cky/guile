(define-module (lang elisp primitives syntax)
  #:use-module (lang elisp internals evaluation)
  #:use-module (lang elisp internals fset)
  #:use-module (lang elisp internals trace)
  #:use-module (lang elisp transform))

;;; Define Emacs Lisp special forms as macros.  This is much more
;;; flexible than handling them specially in the translator: allows
;;; them to be redefined, and hopefully allows better source location
;;; tracking.

;;; {Variables}

(define (setq exp env)
  (cons begin
	(let loop ((sets (cdr exp)) (last-sym #f))
	  (if (null? sets)
	      (list last-sym)
	      (cons `(,module-define! ,the-elisp-module
				      (,quote ,(car sets))
				      ,(transformer (cadr sets)))
		    (loop (cddr sets) (car sets)))))))

(fset 'setq
      (procedure->memoizing-macro setq))

(fset 'defvar
      (procedure->memoizing-macro
        (lambda (exp env)
	  (trc 'defvar (cadr exp))
	  (if (null? (cddr exp))
	      `(,quote ,(cadr exp))
	      `(,begin (,if (,not (,defined? (,quote ,(cadr exp))))
			    ,(setq (list (car exp) (cadr exp) (caddr exp)) env))
		       ;; (,macro-setq ,(cadr exp) ,(caddr exp)))
		       (,quote ,(cadr exp)))))))

(fset 'defconst
      (procedure->memoizing-macro
        (lambda (exp env)
	  (trc 'defconst (cadr exp))
	  `(,begin ,(setq (list (car exp) (cadr exp) (caddr exp)) env)
		   (,quote ,(cadr exp))))))

;;; {lambda, function and macro definitions}

;;; Parses a list of elisp formals, e.g. (x y &optional b &rest r) and
;;; returns three values: (i) list of symbols for required arguments,
;;; (ii) list of symbols for optional arguments, (iii) rest symbol, or
;;; #f if there is no rest argument.
(define (parse-formals formals)
  (letrec ((do-required
	    (lambda (required formals)
	      (if (null? formals)
		  (values (reverse required) '() #f)
		  (let ((next-sym (car formals)))
		    (cond ((not (symbol? next-sym))
			   (error "Bad formals (non-symbol in required list)"))
			  ((eq? next-sym '&optional)
			   (do-optional required '() (cdr formals)))
			  ((eq? next-sym '&rest)
			   (do-rest required '() (cdr formals)))
			  (else
			   (do-required (cons next-sym required)
					(cdr formals))))))))
	   (do-optional
	    (lambda (required optional formals)
	      (if (null? formals)
		  (values (reverse required) (reverse optional) #f)
		  (let ((next-sym (car formals)))
		    (cond ((not (symbol? next-sym))
			   (error "Bad formals (non-symbol in optional list)"))
			  ((eq? next-sym '&rest)
			   (do-rest required optional (cdr formals)))
			  (else
			   (do-optional required
					(cons next-sym optional)
					(cdr formals))))))))
	   (do-rest
	    (lambda (required optional formals)
	      (if (= (length formals) 1)
		  (let ((next-sym (car formals)))
		    (if (symbol? next-sym)
			(values (reverse required) (reverse optional) next-sym)
			(error "Bad formals (non-symbol rest formal)")))
		  (error "Bad formals (more than one rest formal)")))))

    (do-required '() (cond ((list? formals)
			    formals)
			   ((symbol? formals)
			    (list '&rest formals))
			   (else
			    (error "Bad formals (not a list or a single symbol)"))))))

(define (transform-lambda exp)
  (call-with-values (lambda () (parse-formals (cadr exp)))
    (lambda (required optional rest)
      (let ((num-required (length required))
	    (num-optional (length optional)))
	`(,lambda %--args
	   (,let ((%--num-args (,length %--args)))
	     (,cond ((,< %--num-args ,num-required)
		     (,error "Wrong number of args (not enough required args)"))
		    ,@(if rest
			  '()
			  `(((,> %--num-args ,(+ num-required num-optional))
			     (,error "Wrong number of args (too many args)"))))
		    (else
		     (@bind ,(append (map (lambda (i)
					    (list (list-ref required i)
						  `(,list-ref %--args ,i)))
					  (iota num-required))
				     (map (lambda (i)
					    (let ((i+nr (+ i num-required)))
					      (list (list-ref optional i)
						    `(,if (,> %--num-args ,i+nr)
							  (,list-ref %--args ,i+nr)
							  #f))))
					  (iota num-optional))
				     (if rest
					 (list (list rest
						     `(,if (,> %--num-args
							       ,(+ num-required
								   num-optional))
							   (,list-tail %--args
								       ,(+ num-required
									   num-optional))
							   '())))
					 '()))
			    ,@(map transformer (cddr exp)))))))))))

(define interactive-spec (make-fluid))

(define (set-not-subr! proc boolean)
  (set! (not-subr? proc) boolean))

(define (transform-lambda/interactive exp name)
  (fluid-set! interactive-spec #f)
  (let* ((x (transform-lambda exp))
	 (is (fluid-ref interactive-spec)))
    `(,let ((%--lambda ,x))
       (,set-procedure-property! %--lambda (,quote name) (,quote ,name))
       (,set-not-subr! %--lambda #t)
       ,@(if is
	     `((,set! (,interactive-spec %--lambda) (,quote ,is)))
	     '())
       %--lambda)))

(fset 'lambda
      (procedure->memoizing-macro
       (lambda (exp env)
	 (transform-lambda/interactive exp '<elisp-lambda>))))

(fset 'defun
      (procedure->memoizing-macro
       (lambda (exp env)
	 (trc 'defun (cadr exp))
	 `(,begin (,fset (,quote ,(cadr exp))
			 ,(transform-lambda/interactive (cdr exp)
							(symbol-append '<elisp-defun:
								       (cadr exp)
								       '>)))
		  (,quote ,(cadr exp))))))

(fset 'interactive
      (procedure->memoizing-macro
        (lambda (exp env)
	  (fluid-set! interactive-spec exp)
	  #f)))

(fset 'defmacro
      (procedure->memoizing-macro
       (lambda (exp env)
	 (trc 'defmacro (cadr exp))
	 (call-with-values (lambda () (parse-formals (caddr exp)))
	   (lambda (required optional rest)
	     (let ((num-required (length required))
		   (num-optional (length optional)))
	       `(,begin (,fset (,quote ,(cadr exp))
			       (,procedure->memoizing-macro
				(,lambda (exp1 env1)
				  (,trc (,quote using) (,quote ,(cadr exp)))
				  (,let* ((%--args (,cdr exp1))
					  (%--num-args (,length %--args)))
				    (,cond ((,< %--num-args ,num-required)
					    (,error "Wrong number of args (not enough required args)"))
					   ,@(if rest
						 '()
						 `(((,> %--num-args ,(+ num-required num-optional))
						    (,error "Wrong number of args (too many args)"))))
					   (else (,transformer
						  (@bind ,(append (map (lambda (i)
									 (list (list-ref required i)
									       `(,list-ref %--args ,i)))
								       (iota num-required))
								  (map (lambda (i)
									 (let ((i+nr (+ i num-required)))
									   (list (list-ref optional i)
										 `(,if (,> %--num-args ,i+nr)
										       (,list-ref %--args ,i+nr)
										       #f))))
								       (iota num-optional))
								  (if rest
								      (list (list rest
										  `(,if (,> %--num-args
											    ,(+ num-required
												num-optional))
											(,list-tail %--args
												    ,(+ num-required
													num-optional))
											'())))
								      '()))
							 ,@(map transformer (cdddr exp)))))))))))))))))

;;; {Sequencing}

(fset 'progn
      (procedure->memoizing-macro
        (lambda (exp env)
	  `(,begin ,@(map transformer (cdr exp))))))

(fset 'prog1
      (procedure->memoizing-macro
        (lambda (exp env)
	  `(,let ((%res1 ,(transformer (cadr exp))))
	     ,@(map transformer (cddr exp))
	     %res1))))

(fset 'prog2
      (procedure->memoizing-macro
        (lambda (exp env)
	  `(,begin ,(transformer (cadr exp))
		   (,let ((%res2 ,(transformer (caddr exp))))
		     ,@(map transformer (cdddr exp))
		     %res2)))))

;;; {Conditionals}

(define <-- *unspecified*)

(fset 'if
      (procedure->memoizing-macro
        (lambda (exp env)
	  (let ((else-case (cdddr exp)))
	    (cond ((null? else-case)
		   `(nil-cond ,(transformer (cadr exp)) ,(transformer (caddr exp)) #f))
		  ((null? (cdr else-case))
		   `(nil-cond ,(transformer (cadr exp))
			      ,(transformer (caddr exp))
			      ,(transformer (car else-case))))
		  (else
		   `(nil-cond ,(transformer (cadr exp))
			      ,(transformer (caddr exp))
			      (,begin ,@(map transformer else-case)))))))))

(fset 'and
      (procedure->memoizing-macro
        (lambda (exp env)
	  (cond ((null? (cdr exp)) #t)
		((null? (cddr exp)) (transformer (cadr exp)))
		(else
		 (cons nil-cond
		       (let loop ((args (cdr exp)))
			 (if (null? (cdr args))
			     (list (transformer (car args)))
			     (cons (list not (transformer (car args)))
				   (cons #f
					 (loop (cdr args))))))))))))

(fset 'or
      (procedure->memoizing-macro
        (lambda (exp env)
	  (cond ((null? (cdr exp)) #f)
		((null? (cddr exp)) (transformer (cadr exp)))
		(else
		 (cons nil-cond
		       (let loop ((args (cdr exp)))
			 (if (null? (cdr args))
			     (list (transformer (car args)))
			     (cons (transformer (car args))
				   (cons <--
					 (loop (cdr args))))))))))))

(fset 'cond
      (procedure->memoizing-macro
       (lambda (exp env)
	 (if (null? (cdr exp))
	     #f
	     (cons
	      nil-cond
	      (let loop ((clauses (cdr exp)))
		(if (null? clauses)
		    '(#f)
		    (let ((clause (car clauses)))
		      (if (eq? (car clause) #t)
			  (cond ((null? (cdr clause)) '(t))
				((null? (cddr clause))
				 (list (transformer (cadr clause))))
				(else `((,begin ,@(map transformer (cdr clause))))))
			  (cons (transformer (car clause))
				(cons (cond ((null? (cdr clause)) <--)
					    ((null? (cddr clause))
					     (transformer (cadr clause)))
					    (else
					     `(,begin ,@(map transformer (cdr clause)))))
				      (loop (cdr clauses)))))))))))))

(fset 'while
      (procedure->memoizing-macro
        (lambda (exp env)
	  `((,letrec ((%--while (,lambda ()
				  (,nil-cond ,(transformer (cadr exp))
					     (,begin ,@(map transformer (cddr exp))
						     (%--while))
					     #f))))
	      %--while)))))

;;; {Local binding}

(fset 'let
      (procedure->memoizing-macro
        (lambda (exp env)
	  `(@bind ,(map (lambda (binding)
			  (trc 'let binding)
			  (if (pair? binding)
			      `(,(car binding) ,(transformer (cadr binding)))
			      `(,binding #f)))
			(cadr exp))
		  ,@(map transformer (cddr exp))))))

(fset 'let*
      (procedure->memoizing-macro
        (lambda (exp env)
	  (if (null? (cadr exp))
	      `(,begin ,@(map transformer (cddr exp)))
	      (car (let loop ((bindings (cadr exp)))
		     (if (null? bindings)
			 (map transformer (cddr exp))
			 `((@bind (,(let ((binding (car bindings)))
				      (if (pair? binding)
					  `(,(car binding) ,(transformer (cadr binding)))
					  `(,binding #f))))
				  ,@(loop (cdr bindings)))))))))))

;;; {Exception handling}

(fset 'unwind-protect
      (procedure->memoizing-macro
        (lambda (exp env)
	  (trc 'unwind-protect (cadr exp))
	  `(,let ((%--throw-args #f))
	     (,catch #t
	       (,lambda ()
		 ,(transformer (cadr exp)))
	       (,lambda args
		 (,set! %--throw-args args)))
	     ,@(map transformer (cddr exp))
	     (,if %--throw-args
		  (,apply ,throw %--throw-args))))))
