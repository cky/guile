;;; Compilation targets

;; Copyright (C) 2011 Free Software Foundation, Inc.

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
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Code:

(define-module (system base target)
  #:use-module (rnrs bytevectors)
  #:export (target-type with-target

            target-cpu target-vendor target-os

            target-endianness target-word-size))



;;;
;;; Target types
;;;

(define %target-type (make-fluid))

(define (target-type)
  (or (fluid-ref %target-type)
      %host-type))

(define (validate-target target)
  (if (or (not (string? target))
          (let ((parts (string-split target #\-)))
            (or (< 3 (length parts))
                (or-map string-null? parts))))
      (error "invalid target" target)))

(define (with-target target thunk)
  (validate-target target)
  (with-fluids ((%target-type target))
    (thunk)))

(define (target-cpu)
  (let ((t (target-type)))
    (substring t 0 (string-index t #\-))))

(define (target-vendor)
  (let* ((t (target-type))
         (start (1+ (string-index t #\-))))
    (substring t start (string-index t #\- start))))

(define (target-os)
  (let* ((t (target-type))
         (start (1+ (string-index t #\- (1+ (string-index t #\-))))))
    (substring t start)))

(define (target-endianness)
  (if (equal? (target-type) %host-type)
      (native-endianness)
      (error "cross-compilation not yet handled" %host-type (target-type))))

(define (target-word-size)
  (if (equal? (target-type) %host-type)
      ((@ (system foreign) sizeof) '*)
      (error "cross-compilation not yet handled" %host-type (target-type))))
