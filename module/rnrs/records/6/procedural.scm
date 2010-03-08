;;; procedural.scm --- Procedural interface to R6RS records

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


(library (rnrs records procedural (6))
  (export make-record-type-descriptor 
	  record-type-descriptor?
	  make-record-constructor-descriptor
	  
	  record-constructor
	  record-predicate
	  record-accessor	  
	  record-mutator)
	  
  (import (rnrs base (6))
          (only (guile) and=>
			throw
			display
		        make-struct 
			make-vtable 
			map
			simple-format
			string-append 
			
			struct? 
			struct-ref 
			struct-set! 
			struct-vtable
			vtable-index-layout

                        make-hash-table
			hashq-ref
			hashq-set!

			vector->list)
	  (ice-9 receive)
	  (only (srfi :1) fold-right split-at take))

  (define (record-rtd record) (struct-ref record 1))
  (define (record-type-name rtd) (struct-ref rtd 0))
  (define (record-type-parent rtd) (struct-ref rtd 2))
  (define (record-type-uid rtd) (struct-ref rtd 1))
  (define (record-type-generative? rtd) (not (record-type-uid rtd))) 
  (define (record-type-sealed? rtd) (struct-ref rtd 3))
  (define (record-type-opaque? rtd) (struct-ref rtd 4))
  (define (record-type-field-names rtd) (struct-ref rtd 6))

  (define record-type-vtable 
    (make-vtable "prprprprprprprprpr" 
		 (lambda (obj port) 
		   (display "#<r6rs:record-type-vtable>" port))))

  (define record-constructor-vtable 
    (make-vtable "prprpr"
		 (lambda (obj port) 
		   (display "#<r6rs:record-constructor-vtable>" port))))

  (define uid-table (make-hash-table))    

  (define (make-record-type-descriptor name parent uid sealed? opaque? fields)
    (define fields-vtable
      (make-vtable (fold-right (lambda (x p) 
				 (string-append p (case (car x)
						    ((immutable) "pr")
						    ((mutable) "pw"))))
			       "prpr" (vector->list fields))
		   (lambda (obj port)
		     (simple-format
		      port "#<r6rs:record-field-layout-vtable:~A>" name))))
    (define field-names (map cadr (vector->list fields)))
    (define late-rtd #f)
    (define (private-record-predicate obj)       
      (and (struct? obj)
	   (let* ((vtable (struct-vtable obj))
		  (layout (symbol->string
			   (struct-ref vtable vtable-index-layout))))
	     (and (>= (string-length layout) 3)
		  (let ((rtd (struct-ref obj 1)))
		    (and (record-type-descriptor? rtd)
			 (or (eq? (struct-ref rtd 7) fields-vtable)
			     (and=> (struct-ref obj 0)
				    private-record-predicate))))))))

    (define (field-binder parent-struct . args)
      (apply make-struct (append (list fields-vtable 0 
				       parent-struct 
				       late-rtd) 
				 args)))
    (if (and parent (record-type-sealed? parent))
	(r6rs-raise (make-assertion-violation)))

    (let ((matching-rtd (and uid (hashq-ref uid-table uid))))
      (if matching-rtd
	  (if (equal? (list name 
			    parent 
			    sealed? 
			    opaque?
			    field-names
			    (struct-ref fields-vtable vtable-index-layout))
		      (list (record-type-name matching-rtd)
			    (record-type-parent matching-rtd)
			    (record-type-sealed? matching-rtd)
			    (record-type-opaque? matching-rtd)
			    (record-type-field-names matching-rtd)
			    (struct-ref (struct-ref matching-rtd 7)
					vtable-index-layout)))
	      matching-rtd
	      (r6rs-raise (make-assertion-violation)))

	  (let ((rtd (make-struct record-type-vtable 0
				  
				  name
				  uid
				  parent 
				  sealed? 
				  opaque?
				  
				  private-record-predicate
				  field-names
				  fields-vtable
				  field-binder)))
	    (set! late-rtd rtd)
	    (if uid (hashq-set! uid-table uid rtd))
	    rtd))))

  (define (record-type-descriptor? obj)
    (and (struct? obj) (eq? (struct-vtable obj) record-type-vtable)))

  (define (make-record-constructor-descriptor rtd 
					      parent-constructor-descriptor
					      protocol)
    (define rtd-arity (length (struct-ref rtd 6)))
    (define (default-inherited-protocol n)
      (lambda args
	(receive 
          (n-args p-args) 
	  (split-at args (- (length args) rtd-arity))
	  (let ((p (apply n n-args)))
	    (apply p p-args)))))
    (define (default-protocol p) p)
    
    (let* ((prtd (struct-ref rtd 1))
	   (pcd (or parent-constructor-descriptor
		    (and=> prtd (lambda (d) (make-record-constructor-descriptor 
					     prtd #f #f)))))
	   (prot (or protocol (if pcd 
				  default-inherited-protocol 
				  default-protocol))))
      (make-struct record-constructor-vtable 0 rtd pcd prot)))

  (define (record-constructor rctd)
    (let* ((rtd (struct-ref rctd 0))
	   (parent-rctd (struct-ref rctd 1))
	   (protocol (struct-ref rctd 2)))
      (protocol 
       (if parent-rctd
	   (let ((parent-record-constructor (record-constructor parent-rctd))
		 (parent-rtd (struct-ref parent-rctd 0)))
	     (lambda args
	       (let ((struct (apply parent-record-constructor args)))
		 (lambda args
		   (apply (struct-ref rtd 8)
			  (cons struct args))))))
	   (lambda args (apply (struct-ref rtd 8) (cons #f args)))))))
		    
  (define (record-predicate rtd) (struct-ref rtd 5))

  (define (record-accessor rtd k)
    (define (record-accessor-inner obj)
      (and obj 
	   (or (and (eq? (struct-ref obj 1) rtd) (struct-ref obj (+ k 2)))
	       (record-accessor-inner (struct-ref obj 0)))))
    (lambda (obj) (record-accessor-inner obj)))

  (define (record-mutator rtd k)
    (define (record-mutator-inner obj val)
      (and obj 
	   (or (and (eq? (struct-ref obj 1) rtd) (struct-set! obj (+ k 2) val))
	       (record-mutator-inner (struct-ref obj 0) val))))
    (let* ((rtd-vtable (struct-ref rtd 7))
	   (field-layout (symbol->string
			  (struct-ref rtd-vtable vtable-index-layout))))
      (if (not (eqv? (string-ref field-layout (+ (* (+ k 2) 2) 1)) #\w))
	  (r6rs-raise (make-assertion-violation))))
    (lambda (obj val) (record-mutator-inner obj val)))

  ;; Condition types that are used in the current library.  These are defined
  ;; here and not in (rnrs conditions) to avoid a circular dependency.

  (define &condition (make-record-type-descriptor '&condition #f #f #f #f '#()))
  (define &condition-constructor-descriptor 
    (make-record-constructor-descriptor &condition #f #f))

  (define &serious (make-record-type-descriptor 
		    '&serious &condition #f #f #f '#()))
  (define &serious-constructor-descriptor
    (make-record-constructor-descriptor 
     &serious &condition-constructor-descriptor #f))

  (define make-serious-condition 
    (record-constructor &serious-constructor-descriptor))
  (define serious-condition? (record-predicate &serious))

  (define &violation (make-record-type-descriptor
		      '&violation &serious #f #f #f '#()))
  (define &violation-constructor-descriptor
    (make-record-constructor-descriptor 
     &violation &serious-constructor-descriptor #f))
  (define make-violation (record-constructor &violation-constructor-descriptor))
  (define violation? (record-predicate &violation))

  (define &assertion (make-record-type-descriptor
		      '&assertion &violation #f #f #f '#()))
  (define make-assertion-violation 
    (record-constructor 
     (make-record-constructor-descriptor
      &assertion &violation-constructor-descriptor #f)))
  (define assertion-violation? (record-predicate &assertion))

  ;; Exception wrapper type, along with a wrapping `throw' implementation.
  ;; These are used in the current library, and so they are defined here and not
  ;; in (rnrs exceptions) to avoid a circular dependency.

  (define &raise-object-wrapper
    (make-record-type-descriptor '&raise-object-wrapper #f #f #f #f
				 '#((immutable obj) (immutable continuation))))
  (define make-raise-object-wrapper 
    (record-constructor (make-record-constructor-descriptor 
			 &raise-object-wrapper #f #f)))
  (define raise-object-wrapper? (record-predicate &raise-object-wrapper))
  (define raise-object-wrapper-obj (record-accessor &raise-object-wrapper 0))
  (define raise-object-wrapper-continuation 
    (record-accessor &raise-object-wrapper 1))

  (define (r6rs-raise obj) 
    (throw 'r6rs:exception (make-raise-object-wrapper obj #f)))
  (define (r6rs-raise-continuable obj)
    (define (r6rs-raise-continuable-internal continuation)
      (raise (make-raise-object-wrapper obj continuation)))
    (call/cc r6rs-raise-continuable-internal))
)
