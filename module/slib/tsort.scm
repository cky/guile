;;; "tsort.scm" Topological sort
;;; Copyright (C) 1995 Mikael Djurfeldt
;
; This code is in the public domain.

;;; The algorithm is inspired by Cormen, Leiserson and Rivest (1990)
;;; "Introduction to Algorithms", chapter 23

(require 'hash-table)
(require 'primes)

(define (topological-sort dag pred)
  (if (null? dag)
      '()
      (let* ((adj-table (make-hash-table
			 (car (primes> (length dag) 1))))
	     (insert (hash-associator pred))
	     (lookup (hash-inquirer pred))
	     (sorted '()))
	(letrec ((visit
		  (lambda (u adj-list)
		    ;; Color vertex u
		    (insert adj-table u 'colored)
		    ;; Visit uncolored vertices which u connects to
		    (for-each (lambda (v)
				(let ((val (lookup adj-table v)))
				  (if (not (eq? val 'colored))
				      (visit v (or val '())))))
			      adj-list)
		    ;; Since all vertices downstream u are visited
		    ;; by now, we can safely put u on the output list
		    (set! sorted (cons u sorted)))))
	  ;; Hash adjacency lists
	  (for-each (lambda (def)
		      (insert adj-table (car def) (cdr def)))
		    (cdr dag))
	  ;; Visit vertices
	  (visit (caar dag) (cdar dag))
	  (for-each (lambda (def)
		      (let ((val (lookup adj-table (car def))))
			(if (not (eq? val 'colored))
			    (visit (car def) (cdr def)))))
		    (cdr dag)))
	sorted)))

(define tsort topological-sort)
