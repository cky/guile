;;; Extensions to SRFI-4

;; 	Copyright (C) 2009, 2010, 2011 Free Software Foundation, Inc.
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

;;; Commentary:

;; Extensions to SRFI-4. Fully documented in the Guile Reference Manual.

;;; Code:

(define-module (srfi srfi-4 gnu)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-4)
  #:export (;; Complex numbers with 32- and 64-bit components.
            c32vector? make-c32vector c32vector c32vector-length c32vector-ref
            c32vector-set! c32vector->list list->c32vector

            c64vector? make-c64vector c64vector c64vector-length c64vector-ref
            c64vector-set! c64vector->list list->c64vector

            make-srfi-4-vector

            ;; Somewhat polymorphic conversions.
            any->u8vector any->s8vector any->u16vector any->s16vector
            any->u32vector any->s32vector any->u64vector any->s64vector
            any->f32vector any->f64vector any->c32vector any->c64vector))


(define make-srfi-4-vector (@@ (srfi srfi-4) make-srfi-4-vector))

;; Need quasisyntax to do this effectively using syntax-case
(define-macro (define-bytevector-type tag infix size)
  `(begin
     (define (,(symbol-append tag 'vector?) obj)
       (and (uniform-vector? obj)
            (eq? (uniform-vector-element-type obj) ',tag)))
     (define (,(symbol-append 'make- tag 'vector) len . fill)
       (apply make-srfi-4-vector ',tag len fill))
     (define (,(symbol-append tag 'vector-length) v)
       (let ((len (* (uniform-vector-length v)
                     (/ ,size (uniform-vector-element-size v)))))
         (if (integer? len)
             len
             (error "fractional length" v ',tag ,size))))
     (define (,(symbol-append tag 'vector) . elts)
       (,(symbol-append 'list-> tag 'vector) elts))
     (define (,(symbol-append 'list-> tag 'vector) elts)
       (let* ((len (length elts))
              (v (,(symbol-append 'make- tag 'vector) len)))
         (let lp ((i 0) (elts elts))
           (if (and (< i len) (pair? elts))
               (begin
                 (,(symbol-append tag 'vector-set!) v i (car elts))
                 (lp (1+ i) (cdr elts)))
               v))))
     (define (,(symbol-append tag 'vector->list) v)
       (let lp ((i (1- (,(symbol-append tag 'vector-length) v))) (elts '()))
         (if (< i 0)
             elts
             (lp (1- i) (cons (,(symbol-append tag 'vector-ref) v i) elts)))))
     (define (,(symbol-append tag 'vector-ref) v i)
       (,(symbol-append 'bytevector- infix '-ref) v (* i ,size)))
     (define (,(symbol-append tag 'vector-set!) v i x)
       (,(symbol-append 'bytevector- infix '-set!) v (* i ,size) x))
     (define (,(symbol-append tag 'vector-set!) v i x)
       (,(symbol-append 'bytevector- infix '-set!) v (* i ,size) x))))

(define (bytevector-c32-native-ref v i)
  (make-rectangular (bytevector-ieee-single-native-ref v i)
                    (bytevector-ieee-single-native-ref v (+ i 4))))
(define (bytevector-c32-native-set! v i x)
  (bytevector-ieee-single-native-set! v i (real-part x))
  (bytevector-ieee-single-native-set! v (+ i 4) (imag-part x)))
(define (bytevector-c64-native-ref v i)
  (make-rectangular (bytevector-ieee-double-native-ref v i)
                    (bytevector-ieee-double-native-ref v (+ i 8))))
(define (bytevector-c64-native-set! v i x)
  (bytevector-ieee-double-native-set! v i (real-part x))
  (bytevector-ieee-double-native-set! v (+ i 8) (imag-part x)))
(define-bytevector-type c32 c32-native 8)
(define-bytevector-type c64 c64-native 16)

(define-macro (define-any->vector . tags)
  `(begin
     ,@(map (lambda (tag)
              `(define (,(symbol-append 'any-> tag 'vector) obj)
                 (cond ((,(symbol-append tag 'vector?) obj) obj)
                       ((pair? obj) (,(symbol-append 'list-> tag 'vector) obj))
                       ((generalized-vector? obj)
                        (let* ((len (generalized-vector-length obj))
                               (v (,(symbol-append 'make- tag 'vector) len)))
                          (let lp ((i 0))
                            (if (< i len)
                                (begin
                                  (,(symbol-append tag 'vector-set!)
                                   v i (generalized-vector-ref obj i))
                                  (lp (1+ i)))
                                v))))
                       (else (scm-error 'wrong-type-arg #f "" '() (list obj))))))
            tags)))

(define-any->vector u8 s8 u16 s16 u32 s32 u64 s64 f32 f64 c32 c64)
