;;; installed-scm-file

;;;; Copyright (C) 1999, 2001 Free Software Foundation, Inc.
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

(define make-uniform-vector dimensions->uniform-array)

(define uniform-vector-fill! array-fill!)
(define uniform-vector-read! uniform-array-read!)
(define uniform-vector-write uniform-array-write)

(define (make-array fill . args)
  (dimensions->uniform-array args '() fill))
(define (make-uniform-array prot . args)
  (dimensions->uniform-array args prot))
(define (list->array ndim lst)
  (list->uniform-array ndim '() lst))
(define (list->uniform-vector prot lst)
  (list->uniform-array 1 prot lst))
(define (array-shape a)
  (map (lambda (ind) (if (number? ind) (list 0 (+ -1 ind)) ind))
       (array-dimensions a)))
