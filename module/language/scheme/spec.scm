;;; Guile Scheme specification

;; Copyright (C) 2001 Free Software Foundation, Inc.

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Code:

(define-module (language scheme spec)
  #:use-module (system base language)
  #:use-module (language scheme compile-ghil)
  #:use-module (language scheme compile-tree-il)
  #:use-module (language scheme decompile-tree-il)
  #:export (scheme))

;;;
;;; Reader
;;;

(read-enable 'positions)

;;;
;;; Language definition
;;;

(define-language scheme
  #:title	"Guile Scheme"
  #:version	"0.5"
  #:reader	read
  #:compilers   `((tree-il . ,compile-tree-il)
                  (ghil . ,compile-ghil))
  #:decompilers `((tree-il . ,decompile-tree-il))
  #:evaluator	(lambda (x module) (primitive-eval x))
  #:printer	write
  )
