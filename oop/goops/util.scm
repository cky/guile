;;;; 	Copyright (C) 1999, 2000 Free Software Foundation, Inc.
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


(define-module (oop goops util)
  :no-backtrace
  )

(export any every filter
	mapappend find-duplicate top-level-env top-level-env?
	map* for-each* length* improper->proper
 )

;;;
;;; {Utilities}
;;;

(define (any pred lst . rest)
  (if (null? rest) ;fast path
      (and (not (null? lst))
           (let loop ((head (car lst)) (tail (cdr lst)))
             (if (null? tail)
                 (pred head)
                 (or (pred head)
                     (loop (car tail) (cdr tail))))))
      (let ((lsts (cons lst rest)))
        (and (not (any null? lsts))
             (let loop ((heads (map car lsts)) (tails (map cdr lsts)))
               (if (any null? tails)
                   (apply pred heads)
                   (or (apply pred heads)
                       (loop (map car tails) (map cdr tails)))))))))

(define (every pred lst . rest)
  (if (null? rest) ;fast path
      (or (null? lst)
          (let loop ((head (car lst)) (tail (cdr lst)))
            (if (null? tail)
                (pred head)
                (and (pred head)
                     (loop (car tail) (cdr tail))))))
      (let ((lsts (cons lst rest)))
        (or (any null? lsts)
            (let loop ((heads (map car lsts)) (tails (map cdr lsts)))
              (if (any null? tails)
                  (apply pred heads)
                  (and (apply pred heads)
                       (loop (map car tails) (map cdr tails)))))))))

(define (filter test? list)
  (cond ((null? list) '())
	((test? (car list)) (cons (car list) (filter test? (cdr list))))
	(else (filter test? (cdr list)))))

(define (mapappend func . args)
  (if (memv '()  args)
      '()
      (append (apply func (map car args))
	      (apply mapappend func (map cdr args)))))

(define (find-duplicate l)	; find a duplicate in a list; #f otherwise
  (cond 
    ((null? l)			#f)
    ((memv (car l) (cdr l))	(car l))
    (else 			(find-duplicate (cdr l)))))

(define (top-level-env)
  (if *top-level-lookup-closure*
      (list *top-level-lookup-closure*)
      '()))

(define (top-level-env? env)
  (or (null? env)
      (procedure? (car env))))

(define (map* fn . l) 		; A map which accepts dotted lists (arg lists  
  (cond 			; must be "isomorph"
   ((null? (car l)) '())
   ((pair? (car l)) (cons (apply fn      (map car l))
			  (apply map* fn (map cdr l))))
   (else            (apply fn l))))

(define (for-each* fn . l) 	; A for-each which accepts dotted lists (arg lists  
  (cond 			; must be "isomorph"
   ((null? (car l)) '())
   ((pair? (car l)) (apply fn (map car l)) (apply for-each* fn (map cdr l)))
   (else            (apply fn l))))

(define (length* ls)
  (do ((n 0 (+ 1 n))
       (ls ls (cdr ls)))
      ((not (pair? ls)) n)))

(define (improper->proper ls)
  (if (pair? ls)
      (cons (car ls) (improper->proper (cdr ls)))
      (list ls)))
