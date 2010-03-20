;;; hashtables.scm --- The R6RS hashtables library

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


(library (rnrs hashtables (6))
  (export make-eq-hashtable
	  make-eqv-hashtable
	  make-hashtable

	  hashtable?
	  hashtable-size
	  hashtable-ref
	  hashtable-set!
	  hashtable-delete!
	  hashtable-contains?
	  hashtable-update!
	  hashtable-copy
	  hashtable-clear!
	  hashtable-keys
	  hashtable-entries
	  
	  hashtable-equivalence-function
	  hashtable-hash-function
	  hashtable-mutable?

	  equal-hash
	  string-hash
	  string-ci-hash
	  symbol-hash)
  (import (rename (only (guile) string-hash-ci string-hash hashq)
		  (string-hash-ci string-ci-hash))
	  (only (ice-9 optargs) define*)
	  (rename (only (srfi :69) make-hash-table
			           hash
				   hash-by-identity
			           hash-table-size
				   hash-table-ref/default
				   hash-table-set!
				   hash-table-delete!
				   hash-table-exists
				   hash-table-update!/default
				   hash-table-copy
				   hash-table-equivalence-function
				   hash-table-hash-function
				   hash-table-keys
				   hash-table-fold)
		  (hash equal-hash)
		  (hash-by-identity symbol-hash))
	  (rnrs base (6))
	  (rnrs records procedural (6)))
  
  (define r6rs:hashtable 
    (make-record-type-descriptor 
     'r6rs:hashtable #f #f #t #t 
     '#((mutable wrapped-table) (immutable mutable))))

  (define hashtable? (record-predicate r6rs:hashtable))
  (define make-r6rs-hashtable 
    (record-constructor (make-record-constructor-descriptor 
			 r6rs:hashtable #f #f)))
  (define r6rs:hashtable-wrapped-table (record-accessor r6rs:hashtable 0))
  (define r6rs:hashtable-set-wrapped-table! (record-mutator r6rs:hashtable 0))
  (define hashtable-mutable? (record-accessor r6rs:hashtable 1))

  (define* (make-eq-hashtable #:optional k)
    (make-r6rs-hashtable 
     (if k (make-hash-table eq? hashq k) (make-hash-table eq? hashq))
     #t))

  (define* (make-eqv-hashtable #:optional k)
    (make-r6rs-hashtable 
     (if k (make-hash-table eqv? hashv k) (make-hash-table eqv? hashv))
     #t))

  (define* (make-hashtable hash-function equiv #:optional k)
    (make-r6rs-hashtable
     (if k 
	 (make-hash-table equiv hash-function k)
	 (make-hash-table equiv hash-function))
     #t))
 
  (define (hashtable-size hashtable)
    (hash-table-size (r6rs:hashtable-wrapped-table hashtable)))

  (define (hashtable-ref hashtable key default)
    (hash-table-ref/default 
     (r6rs:hashtable-wrapped-table hashtable) key default))

  (define (hashtable-set! hashtable key obj)
    (if (hashtable-mutable? hashtable)
	(hash-table-set! (r6rs:hashtable-wrapped-table hashtable) key obj))
    *unspecified*)

  (define (hashtable-delete! hashtable key)
    (if (hashtable-mutable? hashtable)
	(hash-table-delete! (r6rs:hashtable-wrapped-table hashtable) key))
    *unspecified*)

  (define (hashtable-contains? hashtable key)
    (hash-table-exists? (r6rs:hashtable-wrapped-table hashtable) key))

  (define (hashtable-update! hashtable key proc default)
    (if (hashtable-mutable? hashtable)
	(hash-table-update!/default 
	 (r6rs:hashtable-wrapped-table hashtable) key proc default))
    *unspecified*)

  (define* (hashtable-copy hashtable #:optional mutable)
    (make-r6rs-hashtable 
     (hash-table-copy (r6rs:hashtable-wrapped-table hashtable))
     (and mutable #t)))

  (define* (hashtable-clear! hashtable #:optional k)
    (if (hashtable-mutable? hashtable)
	(let* ((ht (r6rs:hashtable-wrapped-table hashtable))
	       (equiv (hash-table-equivalence-function ht))
	       (hash-function (hash-table-hash-function ht)))
	  (r6rs:hashtable-set-wrapped-table!
	   (if k 
	       (make-hash-table equiv hash-function k)
	       (make-hash-table equiv hash-function)))))
    *unspecified*)

  (define (hashtable-keys hashtable)
    (list->vector (hash-table-keys (r6rs:hashtable-wrapped-table hashtable))))

  (define (hashtable-entries hashtable)
    (let* ((ht (r6rs:hashtable-wrapped-table hashtable))
	   (size (hash-table-size ht))
	   (keys (make-vector size))
	   (vals (make-vector size)))
      (hash-table-fold (r6rs:hashtable-wrapped-table hashtable)
		       (lambda (k v i)
			 (vector-set! keys i k)
			 (vector-set! vals i v)
			 (+ i 1))
		       0)
      (values keys vals)))

  (define (hashtable-equivalence-function hashtable)
    (hash-table-equivalence-function (r6rs:hashtable-wrapped-table hashtable)))

  (define (hashtable-hash-function hashtable)
    (hash-table-hash-function (r6rs:hashtable-wrapped-table hashtable))))
