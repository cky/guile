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
  (export define-record-type)
  (import (only (guile) *unspecified* unspecified? @ @@)
          (rnrs base (6))
	  (rnrs lists (6))
	  (rnrs records procedural (6))
	  (rnrs syntax-case (6))
	  (only (srfi :1) take))

  (define-syntax define-record-type
    (lambda (stx)
      (define (guess-constructor-name record-name)
	(string->symbol (string-append "make-" (symbol->string record-name))))
      (define (guess-predicate-name record-name)
	(string->symbol (string-append (symbol->string record-name) "?")))
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

  (define-syntax define-record-type0
    (lambda (stx)
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
	       (let
		((field-names
		  (datum->syntax 
		   #'record-name
		   (if (unspecified? fields) '() 
		       (list->vector (map (lambda (x) (take x 2)) fields)))))
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
		 (parent (datum->syntax 
			  #'record-name (if (unspecified? parent) #f parent)))
		 (protocol (datum->syntax
			    #'record-name (if (unspecified? protocol) 
					      #f protocol)))
		 (uid (datum->syntax 
		       #'record-name (if (unspecified? nongenerative) 
					 #f nongenerative)))
		 (sealed? (if (unspecified? sealed) #f sealed))
		 (opaque? (if (unspecified? opaque) #f opaque))
		 (parent-cd (datum->syntax 
			     #'record-name (if (unspecified? parent-rtd) 
					       #f (caddr parent-rtd))))
		 (parent-rtd (datum->syntax 
			      #'record-name (if (unspecified? parent-rtd) 
						#f (cadr parent-rtd)))))
		  
		#`(begin 
		    (define record-name 
		      (make-record-type-descriptor 
		       #,(datum->syntax 
			  stx (list 'quote (syntax->datum #'record-name)))
		       #,parent #,uid #,sealed? #,opaque? 
		       #,field-names))
		    (define constructor-name 
		      (record-constructor
		       (make-record-constructor-descriptor 
			record-name #,parent-cd #,protocol)))
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
			(error)))
		   ((parent) (if (unspecified? parent)
				 (loop fields (cadr cr) protocol sealed opaque
				       nongenerative constructor parent-rtd
				       (cdr record-clauses))
				 (error)))
		   ((protocol) (if (unspecified? protocol)
				   (loop fields parent (cadr cr) sealed opaque
					 nongenerative constructor parent-rtd
					 (cdr record-clauses))
				   (error)))
		   ((sealed) (if (unspecified? sealed)
				 (loop fields parent protocol (cadr cr) opaque
				       nongenerative constructor parent-rtd
				       (cdr record-clauses))
				 (error)))
		   ((opaque) (if (unspecified? opaque)
				 (loop fields parent protocol sealed (cadr cr)
				       nongenerative constructor parent-rtd
				       (cdr record-clauses))
				 (error)))
		   ((nongenerative) (if (unspecified? nongenerative)
					(loop fields parent protocol sealed
					      opaque (cadr cr) constructor
					      parent-rtd (cdr record-clauses))
					(error)))
		   ((parent-rtd) (if (unspecified? parent-rtd)
				     (loop fields parent protocol sealed opaque
					   nongenerative constructor parent-rtd
					   (cdr record-clauses))
				     (error)))
		   (else (error))))))))))
)
