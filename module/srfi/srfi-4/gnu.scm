;;; Extensions to SRFI-4

;; 	Copyright (C) 2009 Free Software Foundation, Inc.
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
  #:use-module (srfi srfi-4)
  #:export (;; Somewhat polymorphic conversions.
            any->u8vector any->s8vector any->u16vector any->s16vector
            any->u32vector any->s32vector any->u64vector any->s64vector
            any->f32vector any->f64vector any->c32vector any->c64vector))


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
