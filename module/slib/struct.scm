;;; "struct.scm": defmacros for RECORDS
;;; Copyright 1992 Jeff Alexander, Shinnder Lee, and Lewis Patterson

;;; Defmacros which implement RECORDS from the book:
;;; "Essentials of Programming Languages" by Daniel P. Friedman,
;;;   M. Wand and C.T. Haynes.

;;; jaffer@ai.mit.edu, Feb 1993 ported to SLIB.

;;;  Date: Sun, 20 Aug 1995 19:20:35 -0500
;;;  From: Gary Leavens <leavens@cs.iastate.edu>
;;; I thought you might want to know that, for using the file
;;; struct.scm with the EOPL book, one has to make 2 corrections.  To
;;; correct it, there are two places where "-" has to be replaced by
;;; "->" as in the code below.

(require 'common-list-functions)

(defmacro define-record args
  (check-define-record-syntax args
    (lambda (name make-name name? field-accessors field-setters)
      (letrec
	((make-fields
	   (lambda (field-accessors i)
	     (if (null? field-accessors)
	       '()
	       (cons
		 `(define ,(car field-accessors)
		    (lambda (obj)
		      (if (,name? obj)
			(vector-ref obj ,i)
			(slib:error ',(car field-accessors)
			  ": bad record" obj))))
		 (make-fields (cdr field-accessors) (+ i 1))))))
	 (make-setters
	  (lambda (field-accessors i)
	    (if (null? field-accessors)
		'()
		(cons
		 `(define ,(car field-accessors)
		    (lambda (obj val)
		      (if (,name? obj)
			  (vector-set! obj ,i val)
			  (slib:error ',(car field-accessors)
				      ": bad record" obj))))
		 (make-setters (cdr field-accessors) (+ i 1)))))))
	 `(begin
	   ,@(make-fields field-accessors 1)
	   ,@(make-setters field-setters 1)
	   (define ,name?
	     (lambda (obj)
	       (and (vector? obj)
		 (= (vector-length obj) ,(+ 1 (length field-accessors)))
		 (eq? (vector-ref obj 0) ',name))))
	   (define ,make-name
	     (lambda ,field-accessors
	       (vector ',name ,@field-accessors))))))))

(defmacro variant-case args
  (check-variant-case-syntax args
    (lambda (exp clauses)
      (let ((var (gentemp)))
	(let
	  ((make-clause
	     (lambda (clause)
	       (if (eq? (car clause) 'else)
		 `(#t ,@(cdr clause))
		 `((,(car clause) ,var)
		   (let ,(map (lambda (field)
				`(,(car field) (,(cdr field) ,var)))
			   (cadr clause))
		     ,@(cddr clause)))))))
	  `(let ((,var ,exp))
	     (cond ,@(map make-clause clauses))))))))

;;; syntax checkers

;;; name make-name name? field-accessors

(define check-define-record-syntax
  (lambda (x k)
      (cond
	((and (list? x)
	   (= (length x) 2)
	   (symbol? (car x))
	   (list? (cadr x))
	   (comlist:every symbol? (cadr x))
	   (not (struct:duplicate-fields? (cadr x))))
	 (let ((name (symbol->string (car x))))
	   (let ((make-name (string->symbol
			      (string-append (symbol->string 'make-) name)))
		 (name? (string->symbol (string-append name "?")))
		 (field-accessors
		   (map
		     (lambda (field)
		       (string->symbol
			 (string-append name "->" (symbol->string field))))
		     (cadr x)))
		 (field-setters
		  (map
		   (lambda (field)
		     (string->symbol
		      (string-append
		       "set-" name "-" (symbol->string field) "!")))
		   (cadr x))))
	     (k (car x) make-name name? field-accessors field-setters))))
	(else (slib:error "define-record: invalid syntax" x)))))

(define check-variant-case-syntax
  (let
    ((make-clause
       (lambda (clause)
	 (if (eq? (car clause) 'else)
	   clause
	   (let ((name (symbol->string (car clause))))
	     (let ((name? (string->symbol (string-append name "?")))
		   (fields
		     (map
		       (lambda (field)
			 (cons field
			   (string->symbol
			     (string-append name "->"
			       (symbol->string field)))))
		       (cadr clause))))
	       (cons name? (cons fields (cddr clause)))))))))
    (lambda (args k)
      (if (and (list? args)
	    (<= 2 (length args))
	    (struct:clauses? (cdr args)))
	(k (car args) (map make-clause (cdr args)))
	(slib:error "variant-case: invalid syntax" args)))))

(define struct:duplicate-fields?
  (lambda (fields)
    (cond
      ((null? fields) #f)
      ((memq (car fields) (cdr fields)) #t)
      (else (struct:duplicate-fields? (cdr fields))))))

(define struct:clauses?
  (let
    ((clause?
       (lambda (clause)
	 (and (list? clause)
	      (not (null? clause))
	      (cond
		((eq? (car clause) 'else)
		 (not (null? (cdr clause))))
		(else (and (symbol? (car clause))
			   (not (null? (cdr clause)))
			   (list? (cadr clause))
			   (comlist:every symbol? (cadr clause))
			   (not (struct:duplicate-fields? (cadr clause)))
			   (not (null? (cddr clause))))))))))
    (letrec
      ((struct:duplicate-tags?
	 (lambda (tags)
	   (cond
	     ((null? tags) #f)
	     ((eq? (car tags) 'else) (not (null? (cdr tags))))
	     ((memq (car tags) (cdr tags)) #t)
	     (else (struct:duplicate-tags? (cdr tags)))))))
      (lambda (clauses)
	(and (comlist:every clause? clauses)
	     (not (struct:duplicate-tags? (map car clauses))))))))
