;;; {Queues}

(define-module (ice-9 q))

;;;; 	Copyright (C) 1995 Free Software Foundation, Inc.
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

;;;;
;;; Q: Based on the interface to 
;;;
;;; "queue.scm"  Queues/Stacks for Scheme 
;;;  Written by Andrew Wilcox (awilcox@astro.psu.edu) on April 1, 1992.
;;;

;;;;
;;; {Q}
;;;
;;; A list is just a bunch of cons pairs that follows some constrains, right?
;;; Association lists are the same.  Hash tables are just vectors and association
;;; lists.  You can print them, read them, write them as constants, pun them off as other data
;;; structures etc. This is good.  This is lisp.   These structures are fast and compact
;;; and easy to manipulate arbitrarily because of their simple, regular structure and 
;;; non-disjointedness (associations being lists and so forth).   
;;;
;;; So I figured, queues should be the same -- just a "subtype" of cons-pair 
;;; structures in general.
;;;
;;; A queue is a cons pair:
;;;		( <the-q> . <last-pair> )
;;;
;;; <the-q> is a list of things in the q.   New elements go at the end of that list.
;;;
;;; <last-pair> is #f if the q is empty, and otherwise is the last pair of <the-q>.
;;;
;;; q's print nicely, but alas, they do not read well because the eq?-ness of 
;;; <last-pair> and (last-pair <the-q>) is lost by read.   The procedure
;;; 
;;;		(sync-q! q)
;;;
;;; recomputes and resets the <last-pair> component of a queue.
;;;

(define-public (sync-q! obj) (set-cdr! obj (and (car obj) (last-pair (car obj)))))

;;; make-q
;;;  return a new q.
;;;
(define-public (make-q) (cons '() '()))

;;; q? obj
;;;   Return true if obj is a Q.
;;;   An object is a queue if it is equal? to '(#f . #f) or
;;;   if it is a pair P with (list? (car P)) and (eq? (cdr P) (last-pair P)).
;;;
(define-public (q? obj) (and (pair? obj)
				 (or (and (null? (car obj))
					  (null? (cdr obj)))
				     (and
				      (list? (car obj))
				      (eq? (cdr obj) (last-pair (car obj)))))))

;;; q-empty? obj
;;;  
(define-public (q-empty? obj) (null? (car obj)))

;;; q-empty-check q
;;;  Throw a q-empty exception if Q is empty.
(define-public (q-empty-check q) (if (q-empty? q) (throw 'q-empty q)))


;;; q-front q
;;;  Return the first element of Q.
(define-public (q-front q) (q-empty-check q) (caar q))

;;; q-rear q
;;;  Return the last element of Q.
(define-public (q-rear q) (q-empty-check q) (cadr q))

;;; q-remove! q obj
;;;  Remove all occurences of obj from Q.
(define-public (q-remove! q obj)
  (while (memq obj (car q))
	 (set-car! q (delq! obj (car q))))
  (set-cdr! q (last-pair (car q))))

;;; q-push! q obj
;;;  Add obj to the front of Q
(define-public (q-push! q d)
  (let ((h (cons d (car q))))
    (set-car! q h)
    (if (null? (cdr q))
	(set-cdr! q h))))

;;; enq! q obj
;;;  Add obj to the rear of Q
(define-public (enq! q d)
  (let ((h (cons d '())))
    (if (not (null? (cdr q)))
       	(set-cdr! (cdr q) h)
	(set-car! q h))
    (set-cdr! q h)))

;;; q-pop! q
;;;  Take the front of Q and return it.
(define-public (q-pop! q)
  (q-empty-check q)
  (let ((it (caar q))
	(next (cdar q)))
    (if (not next)
	(set-cdr! q #f))
    (set-car! q next)
    it))

;;; deq! q
;;;  Take the front of Q and return it.
(define-public deq! q-pop!)

;;; q-length q
;;;  Return the number of enqueued elements.
;;;
(define-public (q-length q) (length (car q)))



