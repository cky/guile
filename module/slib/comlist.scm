;;"comlist.scm" Implementation of COMMON LISP list functions for Scheme
; Copyright (C) 1991, 1993, 1995 Aubrey Jaffer.
; Copyright (C) 2000 Colin Walters
;
;Permission to copy this software, to redistribute it, and to use it
;for any purpose is granted, subject to the following restrictions and
;understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;;; Some of these functions may be already defined in your Scheme.
;;; Comment out those definitions for functions which are already defined.

;;;; LIST FUNCTIONS FROM COMMON LISP

;;; Some tail-recursive optimizations made by
;;; Colin Walters <walters@cis.ohio-state.edu>

;;;From: hugh@ear.mit.edu (Hugh Secker-Walker)
(define (comlist:make-list k . init)
  (set! init (if (pair? init) (car init)))
  (do ((k k (+ -1 k))
       (result '() (cons init result)))
      ((<= k 0) result)))

(define (comlist:copy-list lst) (append lst '()))

(define (comlist:adjoin e l) (if (memv e l) l (cons e l)))

(define (comlist:union l1 l2)
  (cond ((null? l1) l2)
	((null? l2) l1)
	(else (comlist:union (cdr l1) (comlist:adjoin (car l1) l2)))))

(define (comlist:intersection l1 l2)
  ;; optimization
  (if (null? l2)
      l2
      (let build-intersection ((l1 l1)
			       (result '()))
	(cond ((null? l1)
	       result)
	      ((memv (car l1) l2) (build-intersection (cdr l1) (cons (car l1) result)))
	      (else (build-intersection (cdr l1) result))))))

(define (comlist:set-difference l1 l2)
  ;; optimization
  (if (null? l2)
      l1
      (let build-difference ((l1 l1)
			     (result '()))
	(cond ((null? l1)
	       result)
	      ((memv (car l1) l2) (build-difference (cdr l1) result))
	      (else (build-difference (cdr l1) (cons (car l1) result)))))))

(define (comlist:position obj lst)
  (letrec ((pos (lambda (n lst)
		  (cond ((null? lst) #f)
			((eqv? obj (car lst)) n)
			(else (pos (+ 1 n) (cdr lst)))))))
    (pos 0 lst)))

(define (comlist:reduce-init p init l)
  (if (null? l)
      init
      (comlist:reduce-init p (p init (car l)) (cdr l))))

(define (comlist:reduce p l)
  (cond ((null? l) l)
	((null? (cdr l)) (car l))
	(else (comlist:reduce-init p (car l) (cdr l)))))

(define (comlist:some pred l . rest)
  (cond ((null? rest)
	 (let mapf ((l l))
	   (and (not (null? l))
		(or (pred (car l)) (mapf (cdr l))))))
	(else (let mapf ((l l) (rest rest))
		(and (not (null? l))
		     (or (apply pred (car l) (map car rest))
			 (mapf (cdr l) (map cdr rest))))))))

(define (comlist:every pred l . rest)
  (cond ((null? rest)
	 (let mapf ((l l))
	   (or (null? l)
	       (and (pred (car l)) (mapf (cdr l))))))
	(else (let mapf ((l l) (rest rest))
		(or (null? l)
		    (and (apply pred (car l) (map car rest))
			 (mapf (cdr l) (map cdr rest))))))))

(define (comlist:notany pred . ls) (not (apply comlist:some pred ls)))

(define (comlist:notevery pred . ls) (not (apply comlist:every pred ls)))

(define (comlist:list-of?? predicate . bound)
  (define (errout) (apply slib:error 'list-of?? predicate bound))
  (case (length bound)
    ((0)
     (lambda (obj)
       (and (list? obj)
	    (every predicate obj))))
    ((1)
     (set! bound (car bound))
     (cond ((negative? bound)
	    (set! bound (- bound))
	    (lambda (obj)
	      (and (list? obj)
		   (<= bound (length obj))
		   (every predicate obj))))
	   (else
	    (lambda (obj)
	      (and (list? obj)
		   (<= (length obj) bound)
		   (every predicate obj))))))
    ((2)
     (let ((low (car bound))
	   (high (cadr bound)))
       (cond ((or (negative? low) (negative? high)) (errout))
	     ((< high low)
	      (set! high (car bound))
	      (set! low (cadr bound))))
       (lambda (obj)
	 (and (list? obj)
	      (<= low (length obj) high)
	      (every predicate obj)))))
    (else (errout))))

(define (comlist:find-if t l)
  (cond ((null? l) #f)
	((t (car l)) (car l))
	(else (comlist:find-if t (cdr l)))))

(define (comlist:member-if t l)
  (cond ((null? l) #f)
	((t (car l)) l)
	(else (comlist:member-if t (cdr l)))))

(define (comlist:remove p l)
  (let remove ((l l)
	       (result '()))
    (cond ((null? l) result)
	  ((eqv? p (car l)) (remove (cdr l) result))
	  (else (remove (cdr l) (cons (car l) result))))))

(define (comlist:remove-if p l)
  (let remove-if ((l l)
		  (result '()))
    (cond ((null? l) result)
	  ((p (car l)) (remove-if (cdr l) result))
	  (else (remove-if (cdr l) (cons (car l) result))))))

(define (comlist:remove-if-not p l)
  (let remove-if-not ((l l)
		      (result '()))
    (cond ((null? l) result)
	  ((p (car l)) (remove-if-not (cdr l) (cons (car l) result)))
	  (else (remove-if-not (cdr l) result)))))

(define comlist:nconc
  (if (provided? 'rev2-procedures) append!
      (lambda args
	(cond ((null? args) '())
	      ((null? (cdr args)) (car args))
	      ((null? (car args)) (apply comlist:nconc (cdr args)))
	      (else
	       (set-cdr! (last-pair (car args))
			 (apply comlist:nconc (cdr args)))
	       (car args))))))

;;;From: hugh@ear.mit.edu (Hugh Secker-Walker)
(define (comlist:nreverse rev-it)
;;; Reverse order of elements of LIST by mutating cdrs.
  (cond ((null? rev-it) rev-it)
	((not (list? rev-it))
	 (slib:error "nreverse: Not a list in arg1" rev-it))
	(else (do ((reved '() rev-it)
		   (rev-cdr (cdr rev-it) (cdr rev-cdr))
		   (rev-it rev-it rev-cdr))
		  ((begin (set-cdr! rev-it reved) (null? rev-cdr)) rev-it)))))

(define (comlist:last lst n)
  (comlist:nthcdr (- (length lst) n) lst))

(define (comlist:butlast lst n)
  (letrec ((l (- (length lst) n))
	   (bl (lambda (lst n)
		 (let build-until-zero ((lst lst)
					(n n)
					(result '()))
		   (cond ((null? lst) (reverse result))
			 ((positive? n)
			  (build-until-zero (cdr lst) (- n 1) (cons (car lst) result)))
			 (else (reverse result)))))))
    (bl lst (if (negative? n)
		(slib:error "negative argument to butlast" n)
		l))))

(define (comlist:nthcdr n lst)
  (if (zero? n) lst (comlist:nthcdr (+ -1 n) (cdr lst))))

(define (comlist:butnthcdr n lst)
  (letrec ((bl (lambda (lst n)
		 (let build-until-zero ((lst lst)
					(n n)
					(result '()))
		   (cond ((null? lst) (reverse result))
			 ((positive? n)
			  (build-until-zero (cdr lst) (- n 1) (cons (car lst) result)))
			 (else (reverse result)))))))
    (bl lst (if (negative? n)
		(slib:error "negative argument to butnthcdr" n)
		n))))

;;;; CONDITIONALS

(define (comlist:and? . args)
  (cond ((null? args) #t)
	((car args) (apply comlist:and? (cdr args)))
	(else #f)))

(define (comlist:or? . args)
  (cond ((null? args) #f)
	((car args) #t)
	(else (apply comlist:or? (cdr args)))))

;;; Checks to see if a list has any duplicate MEMBERs.
(define (comlist:has-duplicates? lst)
  (cond ((null? lst) #f)
	((member (car lst) (cdr lst)) #t)
	(else (comlist:has-duplicates? (cdr lst)))))

;;; remove duplicates of MEMBERs of a list
(define (comlist:remove-duplicates lst)
  (letrec ((rem-dup
	    (lambda (lst nlst)
	      (cond ((null? lst) nlst)
		    ((member (car lst) nlst) (rem-dup (cdr lst) nlst))
		    (else (rem-dup (cdr lst) (cons (car lst) nlst)))))))
    (rem-dup lst '())))

(define (comlist:list* x . y)
  (define (list*1 x)
    (if (null? (cdr x))
	(car x)
	(cons (car x) (list*1 (cdr x)))))
  (if (null? y)
      x
      (cons x (list*1 y))))

(define (comlist:atom? a)
  (not (pair? a)))

(define (comlist:delete obj list)
  (let delete ((list list))
    (cond ((null? list) '())
	  ((equal? obj (car list)) (delete (cdr list)))
	  (else
	   (set-cdr! list (delete (cdr list)))
	   list))))

(define (comlist:delete-if pred list)
  (let delete-if ((list list))
    (cond ((null? list) '())
	  ((pred (car list)) (delete-if (cdr list)))
	  (else
	   (set-cdr! list (delete-if (cdr list)))
	   list))))

(define (comlist:delete-if-not pred list)
  (let delete-if ((list list))
    (cond ((null? list) '())
	  ((not (pred (car list))) (delete-if (cdr list)))
	  (else
	   (set-cdr! list (delete-if (cdr list)))
	   list))))

;;; exports

(define make-list comlist:make-list)
(define copy-list comlist:copy-list)
(define adjoin comlist:adjoin)
(define union comlist:union)
(define intersection comlist:intersection)
(define set-difference comlist:set-difference)
(define position comlist:position)
(define reduce-init comlist:reduce-init)
(define reduce comlist:reduce) ; reduce is also in collect.scm
(define some comlist:some)
(define every comlist:every)
(define notevery comlist:notevery)
(define notany comlist:notany)
(define find-if comlist:find-if)
(define member-if comlist:member-if)
(define remove comlist:remove)
(define remove-if comlist:remove-if)
(define remove-if-not comlist:remove-if-not)
(define nconc comlist:nconc)
(define nreverse comlist:nreverse)
(define last comlist:last)
(define butlast comlist:butlast)
(define nthcdr comlist:nthcdr)
(define butnthcdr comlist:butnthcdr)
(define and? comlist:and?)
(define or? comlist:or?)
(define has-duplicates? comlist:has-duplicates?)
(define remove-duplicates comlist:remove-duplicates)

(define delete-if-not comlist:delete-if-not)
(define delete-if comlist:delete-if)
(define delete comlist:delete)
(define comlist:atom comlist:atom?)
(define atom comlist:atom?)
(define atom? comlist:atom?)
(define list* comlist:list*)
(define list-of?? comlist:list-of??)
