;;;; common-list.scm --- COMMON LISP list functions for Scheme
;;;;
;;;; 	Copyright (C) 1995, 1996, 1997 Free Software Foundation, Inc.
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;; 

(define-module (ice-9 common-list))

;;"comlist.scm" Implementation of COMMON LISP list functions for Scheme
; Copyright (C) 1991, 1993, 1995 Aubrey Jaffer.
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

(define-public (adjoin e l) (if (memq e l) l (cons e l)))

(define-public (union l1 l2)
  (cond ((null? l1) l2)
	((null? l2) l1)
	(else (union (cdr l1) (adjoin (car l1) l2)))))

(define-public (intersection l1 l2)
  (cond ((null? l1) l1)
	((null? l2) l2)
	((memv (car l1) l2) (cons (car l1) (intersection (cdr l1) l2)))
	(else (intersection (cdr l1) l2))))

(define-public (set-difference l1 l2)
  (cond ((null? l1) l1)
	((memv (car l1) l2) (set-difference (cdr l1) l2))
	(else (cons (car l1) (set-difference (cdr l1) l2)))))

(define-public (reduce-init p init l)
  (if (null? l)
      init
      (reduce-init p (p init (car l)) (cdr l))))

(define-public (reduce p l)
  (cond ((null? l) l)
	((null? (cdr l)) (car l))
	(else (reduce-init p (car l) (cdr l)))))

(define-public (some pred l . rest)
  (cond ((null? rest)
	 (let mapf ((l l))
	   (and (not (null? l))
		(or (pred (car l)) (mapf (cdr l))))))
	(else (let mapf ((l l) (rest rest))
		(and (not (null? l))
		     (or (apply pred (car l) (map car rest))
			 (mapf (cdr l) (map cdr rest))))))))

(define-public (every pred l . rest)
  (cond ((null? rest)
	 (let mapf ((l l))
	   (or (null? l)
	       (and (pred (car l)) (mapf (cdr l))))))
	(else (let mapf ((l l) (rest rest))
		(or (null? l)
		    (and (apply pred (car l) (map car rest))
			 (mapf (cdr l) (map cdr rest))))))))

(define-public (notany pred . ls) (not (apply some pred ls)))

(define-public (notevery pred . ls) (not (apply every pred ls)))

(define-public (find-if t l)
  (cond ((null? l) #f)
	((t (car l)) (car l))
	(else (find-if t (cdr l)))))

(define-public (member-if t l)
  (cond ((null? l) #f)
	((t (car l)) l)
	(else (member-if t (cdr l)))))

(define-public (remove-if p l)
  (cond ((null? l) '())
	((p (car l)) (remove-if p (cdr l)))
	(else (cons (car l) (remove-if p (cdr l))))))

(define-public (delete-if! pred list)
  (let delete-if ((list list))
    (cond ((null? list) '())
	  ((pred (car list)) (delete-if (cdr list)))
	  (else
	   (set-cdr! list (delete-if (cdr list)))
	   list)))) 

(define-public (delete-if-not! pred list)
  (let delete-if ((list list))
    (cond ((null? list) '())
	  ((not (pred (car list))) (delete-if (cdr list)))
	  (else
	   (set-cdr! list (delete-if (cdr list)))
	   list))))

(define-public (butlast lst n)
  (letrec ((l (- (length lst) n))
	   (bl (lambda (lst n)
		 (cond ((null? lst) lst)
		       ((positive? n)
			(cons (car lst) (bl (cdr lst) (+ -1 n))))
		       (else '())))))
    (bl lst (if (negative? n)
		(error "negative argument to butlast" n)
		l))))

(define-public (and? . args)
  (cond ((null? args) #t)
	((car args) (apply and? (cdr args)))
	(else #f)))

(define-public (or? . args)
  (cond ((null? args) #f)
	((car args) #t)
	(else (apply or? (cdr args)))))

(define-public (has-duplicates? lst)
  (cond ((null? lst) #f)
	((member (car lst) (cdr lst)) #t)
	(else (has-duplicates? (cdr lst)))))

(define-public (list* x . y)	
  (define (list*1 x)
    (if (null? (cdr x))
	(car x)
	(cons (car x) (list*1 (cdr x)))))
  (if (null? y)
      x
      (cons x (list*1 y))))

;; pick p l
;; Apply P to each element of L, returning a list of elts
;; for which P returns a non-#f value.
;;
(define-public (pick p l)
  (let loop ((s '())
	     (l l))
    (cond
     ((null? l) 	s)
     ((p (car l))	(loop (cons (car l) s) (cdr l)))
     (else		(loop s (cdr l))))))

;; pick p l
;; Apply P to each element of L, returning a list of the 
;; non-#f return values of P.
;;
(define-public (pick-mappings p l)
  (let loop ((s '())
	     (l l))
    (cond
     ((null? l) 	s)
     ((p (car l)) =>	(lambda (mapping) (loop (cons mapping s) (cdr l))))
     (else		(loop s (cdr l))))))

(define-public (uniq l)
  (if (null? l)
      '()
      (let ((u (uniq (cdr l))))
	(if (memq (car l) u)
	    u
	    (cons (car l) u)))))

