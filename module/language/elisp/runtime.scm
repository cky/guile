;;; Guile Emac Lisp

;; Copyright (C) 2001 Free Software Foundation, Inc.

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

(define-module (language elisp runtime)
  #:export (void nil-value t-value elisp-bool)
  #:export-syntax (built-in-func))

; This module provides runtime support for the Elisp front-end.


; The reserved value to mean (when eq?) void.

(define void (list 42))


; Values for t and nil.

; FIXME: Use real nil.
(define nil-value #f)
(define t-value #t)


; Convert a scheme boolean to Elisp.

(define (elisp-bool b)
  (if b
    t-value
    nil-value))


; Define a predefined function; convenient macro for this task.

(define-macro (built-in-func name value)
  `(begin
     (define-public ,name (make-fluid))
     (fluid-set! ,name ,value)))
