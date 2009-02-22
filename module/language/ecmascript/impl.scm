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
  #:re-export (*undefined* *this* call/this*
               pget pput pdel has-property?
               ->boolean
               new-object new new-array)
  #:export (js-init get-this
            typeof
            bitwise-not logical-not
            shift
            mod
            band bxor bior))


(define-class <js-global-object> (<js-object>))
(define-method (pget (o <js-global-object>) (p <string>))
  (pget o (string->symbol p)))
(define-method (pget (o <js-global-object>) (p <symbol>))
  (let ((v (module-variable (current-module) p)))
    (if v
        (variable-ref v)
        (next-method))))
(define-method (pput (o <js-global-object>) (p <string>) v)
  (pput o (string->symbol p) v))
(define-method (pput (o <js-global-object>) (p <symbol>) v)
  (module-define! (current-module) p v))
(define-method (prop-attrs (o <js-global-object>) (p <symbol>))
  (cond ((module-local-variable (current-module) p)
         '())
        ((module-variable (current-module) p)
         '(DontDelete ReadOnly))
        (else (next-method))))
(define-method (prop-attrs (o <js-global-object>) (p <string>))
  (prop-attrs o (string->symbol p)))

(define (init-js-bindings! mod)
  (module-define! mod 'NaN +nan.0)
  (module-define! mod 'Infinity +inf.0)
  (module-define! mod 'undefined *undefined*)
  ;; isNAN, isFinite, parseFloat, parseInt, eval
  ;; decodeURI, decodeURIComponent, encodeURI, encodeURIComponent
  ;; Object Function Array String Boolean Number Date RegExp Error EvalError
  ;; RangeError ReferenceError SyntaxError TypeError URIError
  (module-define! mod 'Object *object-prototype*)
  (module-define! mod 'Array *array-prototype*))

(define (js-init)
  (cond ((get-this))
        (else
         (fluid-set! *this* (make <js-global-object>))
         (init-js-bindings! (current-module)))))

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

(define bitwise-not lognot)
(define (logical-not x)
  (not (->boolean (->primitive x))))

(define shift ash)

(define band logand)
(define bxor logxor)
(define bior logior)

(define mod modulo)

(define-method (+ (a <string>) (b <string>))
  (string-append a b))

(define-method (+ (a <string>) b)
  (string-append a (->string b)))

(define-method (+ a (b <string>))
  (string-append (->string a) b))

(define-method (+ a b)
  (+ (->number a) (->number b)))
