;;; syntactic.scm --- Syntactic support for R6RS records

;;      Copyright (C) 2010 Free Software Foundation, Inc.
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA


(library (rnrs records syntactic (6))
  (export define-record-type 
	  record-type-descriptor 
	  record-constructor-descriptor)
  (import (only (guile) *unspecified* and=> gensym unspecified?)
          (rnrs base (6))
	  (rnrs conditions (6))
	  (rnrs exceptions (6))
	  (rnrs hashtables (6))
	  (rnrs lists (6))
	  (rnrs records procedural (6))
	  (rnrs syntax-case (6))
	  (only (srfi :1) take))

  (define record-type-registry (make-eq-hashtable))

  (define (guess-constructor-name record-name)
    (string->symbol (string-append "make-" (symbol->string record-name))))
  (define (guess-predicate-name record-name)
    (string->symbol (string-append (symbol->string record-name) "?")))
  (define (register-record-type name rtd rcd)
    (hashtable-set! record-type-registry name (cons rtd rcd)))
  (define (lookup-record-type-descriptor name)
    (and=> (hashtable-ref record-type-registry name #f) car))
  (define (lookup-record-constructor-descriptor name)
    (and=> (hashtable-ref record-type-registry name #f) cdr))
  
  (define-syntax define-record-type
    (lambda (stx)
      (syntax-case stx ()
	((_ (record-name constructor-name predicate-name) record-clause ...)
	 #'(define-record-type0 
	     (record-name constructor-name predicate-name)
	     record-clause ...))
	((_ record-name record-clause ...)
	 (let* ((record-name-sym (syntax->datum #'record-name))
		(constructor-name 
		 (datum->syntax 
		  #'record-name (guess-constructor-name record-name-sym)))
		(predicate-name 
		 (datum->syntax 
		  #'record-name (guess-predicate-name record-name-sym))))
	   #`(define-record-type0 
	       (record-name #,constructor-name #,predicate-name) 
	       record-clause ...))))))

  (define (sequence n)
    (define (seq-inner n) (if (= n 0) '(0) (cons n (seq-inner (- n 1)))))
    (reverse (seq-inner n)))
  (define (number-fields fields)
    (define (number-fields-inner fields counter)
      (if (null? fields)
	  '()
	  (cons (cons fields counter) 
		(number-fields-inner (cdr fields) (+ counter 1)))))
    (number-fields-inner fields 0))
  
  (define (process-fields record-name fields)
    (define record-name-str (symbol->string record-name))
    (define (guess-accessor-name field-name)
      (string->symbol (string-append 
		       record-name-str "-" (symbol->string field-name))))
    (define (guess-mutator-name field-name)
      (string->symbol 
       (string-append 
	record-name-str "-" (symbol->string field-name) "-set!")))
    
    (define (f x)
      (cond ((symbol? x) (list 'immutable x (guess-accessor-name x) #f))
	    ((not (list? x)) (error))
	    ((eq? (car x) 'immutable)
	     (cons 'immutable
		   (case (length x)
		     ((2) (list (cadr x) (guess-accessor-name (cadr x)) #f))
		     ((3) (list (cadr x) (caddr x) #f))
		     (else (error)))))
	    ((eq? (car x) 'mutable)
	     (cons 'mutable
		   (case (length x)
		     ((2) (list (cadr x) 
				(guess-accessor-name (cadr x))
				(guess-mutator-name (cadr x))))
		     ((4) (cdr x))
		     (else (error)))))
	    (else (error))))
    (map f fields))
  
  (define-syntax define-record-type0
    (lambda (stx)	  
      (syntax-case stx ()
	((_ (record-name constructor-name predicate-name) record-clause ...)
	 (let loop ((fields *unspecified*)
		    (parent *unspecified*)
		    (protocol *unspecified*)
		    (sealed *unspecified*)
		    (opaque *unspecified*)
		    (nongenerative *unspecified*)
		    (constructor *unspecified*)
		    (parent-rtd *unspecified*)
		    (record-clauses (syntax->datum #'(record-clause ...))))
	   (if (null? record-clauses)
	       (let*
		((fields (if (unspecified? fields) '() fields))
		 (field-names
		  (datum->syntax 
		   #'record-name
		   (list->vector (map (lambda (x) (take x 2)) fields))))
		 (field-accessors
		  (fold-left (lambda (x c lst) 
			       (cons #`(define #,(datum->syntax 
						  #'record-name (caddr x))
					 (record-accessor record-name #,c))
				     lst))
			     '() fields (sequence (length fields))))
		 (field-mutators
		  (fold-left (lambda (x c lst) 
			       (if (cadddr x)
				   (cons #`(define #,(datum->syntax 
						      #'record-name (cadddr x))
					     (record-mutator record-name #,c))
					 lst)
				   lst))
			     '() fields (sequence (length fields))))

		 (parent-cd 
		  (datum->syntax
		   stx (cond ((not (unspecified? parent))
			      `(record-constructor-descriptor ,parent))
			     ((not (unspecified? parent-rtd)) (cadr parent-rtd))
			     (else #f))))
		 (parent-rtd
		  (datum->syntax 
		   stx (cond ((not (unspecified? parent))
			      `(record-type-descriptor ,parent))
			     ((not (unspecified? parent-rtd)) (car parent-rtd))
			     (else #f))))

		 (protocol (datum->syntax
			    #'record-name (if (unspecified? protocol) 
					      #f protocol)))
		 (uid (datum->syntax 
		       #'record-name (if (unspecified? nongenerative) 
					 #f nongenerative)))
		 (sealed? (if (unspecified? sealed) #f sealed))
		 (opaque? (if (unspecified? opaque) #f opaque))

		 (record-name-sym (datum->syntax 
				   stx (list 'quote 
					     (syntax->datum #'record-name)))))
		  
		#`(begin 
		    (define record-name 
		      (make-record-type-descriptor 
		       #,record-name-sym
		       #,parent-rtd #,uid #,sealed? #,opaque? 
		       #,field-names))
		    (define constructor-name 
		      (record-constructor
		       (make-record-constructor-descriptor 
			record-name #,parent-cd #,protocol)))
                    (define dummy
                      (let ()
                        (register-record-type 
                         #,record-name-sym 
                         record-name (make-record-constructor-descriptor 
                                      record-name #,parent-cd #,protocol))
                        'dummy))
		    (define predicate-name (record-predicate record-name))
		    #,@field-accessors
		    #,@field-mutators))
	       (let ((cr (car record-clauses)))
		 (case (car cr)
		   ((fields) 
		    (if (unspecified? fields)
			(loop (process-fields (syntax->datum #'record-name) 
					      (cdr cr))
			      parent protocol sealed opaque nongenerative 
			      constructor parent-rtd (cdr record-clauses))
			(raise (make-assertion-violation))))
		   ((parent)
		    (if (not (unspecified? parent-rtd))
			(raise (make-assertion-violation)))
		    (if (unspecified? parent)
			(loop fields (cadr cr) protocol sealed opaque
			      nongenerative constructor parent-rtd
			      (cdr record-clauses))
			(raise (make-assertion-violation))))
		   ((protocol) 
		    (if (unspecified? protocol)
			(loop fields parent (cadr cr) sealed opaque
			      nongenerative constructor parent-rtd
			      (cdr record-clauses))
			(raise (make-assertion-violation))))
		   ((sealed) 
		    (if (unspecified? sealed)
			(loop fields parent protocol (cadr cr) opaque
			      nongenerative constructor parent-rtd
			      (cdr record-clauses))
			(raise (make-assertion-violation))))
		   ((opaque) (if (unspecified? opaque)
				 (loop fields parent protocol sealed (cadr cr)
				       nongenerative constructor parent-rtd
				       (cdr record-clauses))
				 (raise (make-assertion-violation))))
		   ((nongenerative) 
		    (if (unspecified? nongenerative)
			(let ((uid (list 'quote
					 (or (and (> (length cr) 1) (cadr cr))
					     (gensym)))))
			  (loop fields parent protocol sealed
				opaque uid constructor
				parent-rtd (cdr record-clauses)))
			(raise (make-assertion-violation))))
		   ((parent-rtd) 
		    (if (not (unspecified? parent))
			(raise (make-assertion-violation)))
		    (if (unspecified? parent-rtd)
			(loop fields parent protocol sealed opaque
			      nongenerative constructor (cdr cr)
			      (cdr record-clauses))
			(raise (make-assertion-violation))))
		   (else (raise (make-assertion-violation)))))))))))

  (define-syntax record-type-descriptor
    (lambda (stx)
      (syntax-case stx ()
	((_ name) #`(lookup-record-type-descriptor 
		     #,(datum->syntax 
			stx (list 'quote (syntax->datum #'name))))))))

  (define-syntax record-constructor-descriptor
    (lambda (stx)
      (syntax-case stx ()
	((_ name) #`(lookup-record-constructor-descriptor 
		     #,(datum->syntax 
			stx (list 'quote (syntax->datum #'name))))))))
)
