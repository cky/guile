;;; Tree Intermediate Language

;; Copyright (C) 2009 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(define-module (language tree-il spec)
  #:use-module (system base language)
  #:use-module (language glil)
  #:use-module (language tree-il)
  #:use-module (language tree-il compile-glil)
  #:export (tree-il))

(define (write-tree-il exp . port)
  (apply write (unparse-tree-il exp) port))

(define (parse x)
  (make-lambda #f '() '() (parse-tree-il x)))

(define (join exps env)
  (if (or-map (lambda (x)
                (or (not (lambda? x))
                    (not (null? (lambda-vars x)))))
              exps)
      (error "tree-il expressions to join must be thunks"))

  (make-lambda #f '() '() (make-sequence #f (map lambda-body exps))))

(define-language tree-il
  #:title	"Tree Intermediate Language"
  #:version	"1.0"
  #:reader	read
  #:printer	write-tree-il
  #:parser      parse
  #:joiner      join
  #:compilers   `((glil . ,compile-glil))
  )
