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
  #:use-modules (oop goops)
  #:export (*undefined*
            <js-object>
            pget prop-attrs prop-has-attr? pput has-property? pdel

            object->string object->number object->value/string
            object->value/number object->value

            ->primitive ->boolean ->number ->integer ->int32 ->uint32
            ->uint16 ->string ->object))

(define *undefined* ((@@ (oop goops) make-unbound)))

(define NaN +nan.0)
(define Infinity +inf.0)

(define-class <js-object> ()
  (prototype #:getter js-prototype #:init-keyword #:prototype
             #:init-value #f)
  (props #:getter js-props #:init-form (make-hash-table 7))
  (prop-attrs #:getter js-prop-attrs #:init-value #f)
  (value #:getter js-value #:init-value #f #:init-keyword #:value))

(define-method (pget (o <js-object>) p)
  (let ((h (hashq-get-handle (js-props o) p)))
    (if h
        (cdr h)
        (let ((proto (js-prototype o)))
          (if proto
              (pget proto p)
              *undefined*)))))

(define-method (prop-attrs (o <js-object>) p)
  (or (hashq-ref (js-prop-attrs o) p)
      (let ((proto (js-prototype o)))
        (if proto
            (prop-attrs proto p)
            '()))))

(define-method (prop-has-attr? (o <js-object>) p attr)
  (memq attr (prop-attrs o p)))

(define-method (pput (o <js-object>) p v)
  (if (prop-has-attr? o p 'ReadOnly)
      (throw 'ReferenceError o p)
      (hashq-set! (js-props o) p v)))

(define-method (has-property? (o <js-object>) p v)
  (if (prop-has-attr? o p 'ReadOnly)
      (throw 'ReferenceError o p)
      (hashq-set! (js-props o) p v)))

(define-method (pdel (o <js-object>) p)
  (if (prop-has-attr? o p 'DontDelete)
      #f
      (begin
        (pput o p *undefined*)
        #t)))

(define (object->string o error?)
  (let ((toString (pget o 'toString)))
    (if (procedure? toString)
        (let ((x (toString o)))
          (if (and error? (is-a? x <js-object>))
              (throw 'TypeError o 'default-value)
              x))
        (if error?
            (throw 'TypeError o 'default-value)
            o))))
              
(define (object->number o error?)
  (let ((valueOf (pget o 'valueOf)))
    (if (procedure? valueOf)
        (let ((x (valueOf o)))
          (if (and error? (is-a? x <js-object>))
              (throw 'TypeError o 'default-value)
              x))
        (if error?
            (throw 'TypeError o 'default-value)
            o))))
              
(define (object->value/string o)
  (let ((v (object->string o #f)))
    (if (is-a? x <js-object>)
        (object->number o #t)
        x)))
              
(define (object->value/number o)
  (let ((v (object->number o #f)))
    (if (is-a? x <js-object>)
        (object->string o #t)
        x)))
              
(define (object->value o)
  ;; FIXME: if it's a date, we should try numbers first
  (object->value/string o))
              
(define (->primitive x)
  (if (is-a? x <js-object>)
      (object->value x)
      x))

(define (->boolean x)
  (not (or (not x) (null? x) (eq? x *undefined*) (zero? x) (nan? x)
           (and (string? x) (= (string-length x) 0)))))

(define (->number x)
  (cond ((number? x) x)
        ((boolean? x) (if x 1 0))
        ((null? x) 0)
        ((eq? x *undefined*) +nan.0)
        ((is-a? x <js-object>) (object->number o))
        ((string? x) (string->number x))
        (else (throw 'TypeError o '->number))))

(define (->integer x)
  (let ((n (->number x)))
    (cond ((nan? n) 0)
          ((zero? n) n)
          ((inf? n) n)
          (else (inexact->exact (round n))))))

(define (->int32 x)
  (let ((n (->number x)))
    (if (or (nan? n) (zero? n) (inf? n))
        0
        (let ((m (logand (1- (ash 1 32)) (inexact->exact (round n)))))
          (if (negative? n)
              (- m (ash 1 32))
              m)))))

(define (->uint32 x)
  (let ((n (->number x)))
    (if (or (nan? n) (zero? n) (inf? n))
        0
        (logand (1- (ash 1 32)) (inexact->exact (round n))))))

(define (->uint16 x)
  (let ((n (->number x)))
    (if (or (nan? n) (zero? n) (inf? n))
        0
        (logand (1- (ash 1 16)) (inexact->exact (round n))))))

(define (->string x)
  (cond ((eq? x *undefined*) "undefined")
        ((null? x) "null")
        ((boolean? x) (if x "true" "false"))
        ((string? x) x)
        ((number? x)
         (cond ((nan? x) "NaN")
               ((zero? x) "0")
               ((inf? x) "Infinity")
               (else (number->string x))))
        (else (->string (object->value/string x)))))

(define (->object x)
  (cond ((eq? x *undefined*) (throw 'TypeError x '->object))
        ((null? x) (throw 'TypeError x '->object))
        ((boolean? x) (make <js-object> #:prototype Boolean #:value x))
        ((number? x) (make <js-object> #:prototype String #:value x))
        ((string? x) (make <js-object> #:prototype Number #:value x))
        (else x)))
