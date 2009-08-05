;;; transformation of letrec into simpler forms

;; Copyright (C) 2009 Free Software Foundation, Inc.

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

(define-module (language tree-il fix-letrec)
  #:use-module (system base syntax)
  #:use-module (language tree-il)
  #:export (fix-letrec!))

;; For a detailed discussion, see "Fixing Letrec: A Faithful Yet
;; Efficient Implementation of Scheme’s Recursive Binding Construct", by
;; Oscar Waddell, Dipanwita Sarkar, and R. Kent Dybvig.

(define (fix-letrec! x)
  x)
