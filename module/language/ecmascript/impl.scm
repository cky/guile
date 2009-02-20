;;; ECMAScript for Guile

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

(define-module (language ecmascript impl)
  #:use-module (oop goops)
  #:use-module (language ecmascript base)
  #:use-module (language ecmascript function)
  #:use-module (language ecmascript array)
  #:export (get-this typeof)
  #:re-export (*undefined* *this* call/this*
               pget pput pdel
               new-object
               new
               new-array))

(define (get-this)
  (fluid-ref *this*))

(define (typeof x)
  (cond ((eq? x *undefined*) "undefined")
        ((null? x) "object")
        ((boolean? x) "boolean")
        ((number? x) "number")
        ((string? x) "string")
        ((procedure? x) "function")
        ((is-a? x <js-object>) "object")
        (else "scm")))
