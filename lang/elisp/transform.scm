(define-module (lang elisp transform)
  #:use-module (lang elisp internals trace)
  #:use-module (lang elisp internals fset)
  #:use-module (lang elisp internals evaluation)
  #:use-module (ice-9 session)
  #:export (transformer))

(define interactive-spec (make-fluid))

;;; {S-expressions}
;;;

(define (syntax-error x)
  (error "Syntax error in expression" x))

;; Should be made mutating instead of constructing
;;
(define (transformer x)
  (cond ((null? x) '())
	((not (pair? x)) x)
	((and (pair? (car x))
	      (eq? (caar x) 'quasiquote))
	 (transformer (car x)))
	((symbol? (car x))
	 (case (car x)
	   ((@fop @bind define-module use-modules use-syntax) x)
	   ; Escape to Scheme syntax
	   ((scheme) (cons 'begin (cdr x)))
	   ; Should be handled in reader
	   ((quote function) (cons 'quote (cars->nil (cdr x))))
	   ((quasiquote) (m-quasiquote x '()))
	   ((nil-cond) (transform-1 x))
	   ((let) (m-let x '()))
	   ((let*) (m-let* x '()))
	   ((if) (m-if x '()))
	   ((and) (m-and x '()))
	   ((or) (m-or x '()))
	   ((while) (m-while x '()))
	   ;((while) (cons macro-while (cdr x)))
	   ((prog1) (m-prog1 x '()))
	   ((prog2) (m-prog2 x '()))
	   ((progn begin) (cons 'begin (map transformer (cdr x))))
	   ((cond) (m-cond x '()))
	   ((lambda) (transform-lambda/interactive x '<elisp-lambda>))
	   ((defun) (m-defun x '()))
	   ((defmacro) (m-defmacro x '()))
	   ((setq) (m-setq x '()))
	   ((defvar) (m-defvar x '()))
	   ((defconst) (m-defconst x '()))
	   ((interactive) (fluid-set! interactive-spec x) #f)
	   ((unwind-protect) (m-unwind-protect x '()))
	   (else (transform-application x))))
	(else (syntax-error x))))

(define (m-unwind-protect exp env)
  (trc 'unwind-protect (cadr exp))
  `(let ((%--throw-args #f))
     (catch #t
	    (lambda ()
	      ,(transformer (cadr exp)))
	    (lambda args
	      (set! %--throw-args args)))
     ,@(transform-list (cddr exp))
     (if %--throw-args
	 (apply throw %--throw-args))))

(define (m-quasiquote exp env)
  (cons 'quasiquote
	(map transform-inside-qq (cdr exp))))

(define (transform-inside-qq x)
  (trc 'transform-inside-qq x)
  (cond ((not (pair? x)) x)
	((symbol? (car x))
	 (case (car x)
	   ((unquote) (list 'unquote (transformer (cadr x))))
	   ((unquote-splicing) (list 'unquote-splicing (transformer (cadr x))))
	   (else (cons (car x) (map transform-inside-qq (cdr x))))))
	(else
	 (cons (transform-inside-qq (car x)) (transform-inside-qq (cdr x))))))
	
(define (transform-1 x)
  (cons (car x) (map transformer (cdr x))))

(define (transform-2 x)
  (cons (car x)
	(cons (cadr x)
	      (map transformer (cddr x)))))

(define (transform-3 x)
  (cons (car x)
	(cons (cadr x)
	      (cons (caddr x)
		    (map transformer (cdddr x))))))

(define (transform-list x)
  (map transformer x))

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

(define (transform-lambda/interactive exp name)
  (fluid-set! interactive-spec #f)
  (let* ((x (transform-lambda exp))
	 (is (fluid-ref interactive-spec)))
    `(let ((%--lambda ,x))
       (set-procedure-property! %--lambda 'name ',name)
       (set! (,not-subr? %--lambda) #t)
       ,@(if is
	     `((set! (,interactive-specification %--lambda) ',is))
	     '())
       %--lambda)))

(define (transform-lambda exp)
  (call-with-values (lambda () (parse-formals (cadr exp)))
    (lambda (required optional rest)
      (let ((num-required (length required))
	    (num-optional (length optional)))
	`(lambda %--args
	   (let ((%--num-args (length %--args)))
	     (cond ((< %--num-args ,num-required)
		    (error "Wrong number of args (not enough required args)"))
		   ,@(if rest
			 '()
			 `(((> %--num-args ,(+ num-required num-optional))
			    (error "Wrong number of args (too many args)"))))
		   (else
		    (@bind ,(append (map (lambda (i)
					   (list (list-ref required i)
						 `(list-ref %--args ,i)))
					 (iota num-required))
				    (map (lambda (i)
					   (let ((i+nr (+ i num-required)))
					     (list (list-ref optional i)
						   `(if (> %--num-args ,i+nr)
							(list-ref %--args ,i+nr)
							#f))))
					 (iota num-optional))
				    (if rest
					(list (list rest
						    `(if (> %--num-args
							    ,(+ num-required
								num-optional))
							 (list-tail %--args
								    ,(+ num-required
									num-optional))
							 '())))
					'()))
			   ,@(transform-list (cddr exp)))))))
	))))

(define (m-defun exp env)
  (trc 'defun (cadr exp))
  `(begin (,fset ',(cadr exp)
		 ,(transform-lambda/interactive (cdr exp)
						(symbol-append '<elisp-defun:
							       (cadr exp)
							       '>)))
	  ',(cadr exp)))

(define (m-defmacro exp env)
  (trc 'defmacro (cadr exp))
  (call-with-values (lambda () (parse-formals (caddr exp)))
    (lambda (required optional rest)
      (let ((num-required (length required))
	    (num-optional (length optional)))
	`(begin (,fset ',(cadr exp)
		       (procedure->memoizing-macro
			(lambda (exp1 env1)
			  (,trc 'using ',(cadr exp))
			  (let* ((%--args (cdr exp1))
				 (%--num-args (length %--args)))
			    (cond ((< %--num-args ,num-required)
				   (error "Wrong number of args (not enough required args)"))
				  ,@(if rest
					'()
					`(((> %--num-args ,(+ num-required num-optional))
					   (error "Wrong number of args (too many args)"))))
				  (else (,transformer
					 (@bind ,(append (map (lambda (i)
								(list (list-ref required i)
								      `(list-ref %--args ,i)))
							      (iota num-required))
							 (map (lambda (i)
								(let ((i+nr (+ i num-required)))
								  (list (list-ref optional i)
									`(if (> %--num-args ,i+nr)
									     (list-ref %--args ,i+nr)
									     #f))))
							      (iota num-optional))
							 (if rest
							     (list (list rest
									 `(if (> %--num-args
										 ,(+ num-required
										     num-optional))
									      (list-tail %--args
											 ,(+ num-required
											     num-optional))
									      '())))
							     '()))
						,@(transform-list (cdddr exp)))))))))))))))

(define (transform-application x)
  `(@fop ,(car x)
	 (,transformer-macro ,@(cdr x))))

(define transformer-macro
  (procedure->memoizing-macro
   (lambda (exp env)
     (cons 'list (map transformer (cdr exp))))))

;  (cons '@fop
;	(cons (car x)
;	      (map transformer (cdr x)))))

(define (cars->nil ls)
  (cond ((not (pair? ls)) ls)
	((null? (car ls)) (cons '() (cars->nil (cdr ls))))
	(else (cons (cars->nil (car ls))
		    (cars->nil (cdr ls))))))

;;; {Special forms}
;;;

(define (m-setq exp env)
  (cons 'begin
	(let loop ((sets (cdr exp)) (last-sym #f))
	  (if (null? sets)
	      (list last-sym)
	      (cons `(module-define! ,the-elisp-module
				     ',(car sets)
				     ,(transformer (cadr sets)))
		    (loop (cddr sets) (car sets)))))))

;(define (m-setq exp env)
;  (let* ((binder (car (last-pair env)))
;	 (varvals (let loop ((ls (cdr exp)))
;		    (if (null? ls)
;			'()
;			;; Ensure existence only at macro expansion time
;			(let ((var (or (binder (car ls) #f)
;				       (binder (car ls) #t))))
;			  (if (not (variable-bound? var))
;			      (variable-set! var #f))
;			  (cons (list 'set! (car ls) (transformer (cadr ls)))
;				(loop (cddr ls))))))))
;    (cond ((null? varvals) '())
;	  ((null? (cdr varvals)) (car varvals))
;	  (else (cons 'begin varvals)))))

(define (m-let exp env)
  `(@bind ,(map (lambda (binding)
		  (trc 'let binding)
		  (if (pair? binding)
		      `(,(car binding) ,(transformer (cadr binding)))
		      `(,binding #f)))
		(cadr exp))
	  ,@(transform-list (cddr exp))))

(define (m-let* exp env)
  (if (null? (cadr exp))
      `(begin ,@(transform-list (cddr exp)))
      (car (let loop ((bindings (cadr exp)))
	     (if (null? bindings)
		 (transform-list (cddr exp))
		 `((@bind (,(let ((binding (car bindings)))
			      (if (pair? binding)
				  `(,(car binding) ,(transformer (cadr binding)))
				  `(,binding #f))))
			  ,@(loop (cdr bindings)))))))))

(define (m-prog1 exp env)
  `(,let ((%res1 ,(transformer (cadr exp))))
	 ,@(transform-list (cddr exp))
	 %res1))

(define (m-prog2 exp env)
  `(begin ,(transformer (cadr exp))
	  (,let ((%res2 ,(transformer (caddr exp))))
		,@(transform-list (cdddr exp))
		%res2)))

(define <-- *unspecified*)

(define (m-if exp env)
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
		      (begin ,@(transform-list else-case)))))))

(define (m-and exp env)
  (cond ((null? (cdr exp)) #t)
	((null? (cddr exp)) (transformer (cadr exp)))
	(else
	 (cons 'nil-cond
	       (let loop ((args (cdr exp)))
		 (if (null? (cdr args))
		     (list (transformer (car args)))
		     (cons (list 'not (transformer (car args)))
			   (cons #f
				 (loop (cdr args))))))))))

(define (m-or exp env)
  (cond ((null? (cdr exp)) #f)
	((null? (cddr exp)) (transformer (cadr exp)))
	(else
	 (cons 'nil-cond
	       (let loop ((args (cdr exp)))
		 (if (null? (cdr args))
		     (list (transformer (car args)))
		     (cons (transformer (car args))
			   (cons <--
				 (loop (cdr args))))))))))

(define m-cond
  (lambda (exp env)
    (if (null? (cdr exp))
	#f
	(cons
	 'nil-cond
	 (let loop ((clauses (cdr exp)))
	   (if (null? clauses)
	       '(#f)
	       (let ((clause (car clauses)))
		 (if (eq? (car clause) #t)
		     (cond ((null? (cdr clause)) '(t))
			   ((null? (cddr clause))
			    (list (transformer (cadr clause))))
			   (else `((begin ,@(transform-list (cdr clause))))))
		     (cons (transformer (car clause))
			   (cons (cond ((null? (cdr clause)) <--)
				       ((null? (cddr clause))
					(transformer (cadr clause)))
				       (else
					`(begin ,@(transform-list (cdr clause)))))
				 (loop (cdr clauses))))))))))))

(define (m-while exp env)
  `(,let %while ()
	 (nil-cond ,(transformer (cadr exp))
		   (begin ,@(transform-list (cddr exp)) (%while))
		   #f)))

(define (m-defvar exp env)
  (trc 'defvar (cadr exp))
  (if (null? (cddr exp))
      `',(cadr exp)
      `(begin (if (not (defined? ',(cadr exp)))
		  (,macro-setq ,(cadr exp) ,(caddr exp)))
	      ',(cadr exp))))

(define (m-defconst exp env)
  (trc 'defconst (cadr exp))
  `(begin ,(m-setq (list (car exp) (cadr exp) (caddr exp)) env)
	  ',(cadr exp)))

;(export-mmacros
; '(setq defun let let* if and or cond while prog1 prog2 progn)
; (list m-setq m-defun m-let m-let* m-if m-and m-or m-cond m-while m-prog1 m-prog2 begin))

(define macro-setq (procedure->memoizing-macro m-setq))
(define macro-while (procedure->memoizing-macro m-while))
