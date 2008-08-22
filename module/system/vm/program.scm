;;; Guile VM program functions

;;; Copyright (C) 2001 Free Software Foundation, Inc.
;;; Copyright (C) 2005 Ludovic Courtès  <ludovic.courtes@laas.fr>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA

;;; Code:

(define-module (system vm program)
  :export (arity:nargs arity:nrest arity:nlocs arity:nexts
           make-binding binding:name binding:extp binding:index
           source:addr source:line source:column source:file
           program-bindings program-sources
           program-properties program-property program-documentation
           
           program-arity program-external-set! program-meta
           program-bytecode program? program-objects
           program-base program-external))

(dynamic-call "scm_init_programs" (dynamic-link "libguile"))

(define arity:nargs car)
(define arity:nrest cadr)
(define arity:nlocs caddr)
(define arity:nexts cadddr)

(define (make-binding name extp index)
  (list name extp index))

(define binding:name car)
(define binding:extp cadr)
(define binding:index caddr)

(define (program-bindings prog)
  (cond ((program-meta prog) => car)
	(else '())))

(define (source:addr source)
  (car source))
(define (source:line source)
  (vector-ref (cdr source) 0))
(define (source:column source)
  (vector-ref (cdr source) 1))
(define (source:file source)
  (vector-ref (cdr source) 2))

(define (program-sources prog)
  (cond ((program-meta prog) => cadr)
	(else '())))

(define (program-properties prog)
  (or (and=> (program-meta prog) cddr)
      '()))

(define (program-property prog prop)
  (assq-ref (program-properties proc) prop))

(define (program-documentation prog)
  (assq-ref (program-properties prog) 'documentation))

