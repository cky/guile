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
  #:use-module (ice-9 regex)
  #:export (target-type with-target

            target-cpu target-vendor target-os

            target-endianness target-word-size))



;;;
;;; Target types
;;;

(define %native-word-size
  ;; The native word size.  Note: don't use `word-size' from
  ;; (system vm objcode) to avoid a circular dependency.
  ((@ (system foreign) sizeof) '*))

(define %target-type (make-fluid %host-type))
(define %target-endianness (make-fluid (native-endianness)))
(define %target-word-size (make-fluid %native-word-size))

(define (validate-target target)
  (if (or (not (string? target))
          (let ((parts (string-split target #\-)))
            (or (< (length parts) 3)
                (or-map string-null? parts))))
      (error "invalid target" target)))

(define (with-target target thunk)
  (validate-target target)
  (let ((cpu (triplet-cpu target)))
    (with-fluids ((%target-type target)
                  (%target-endianness (cpu-endianness cpu))
                  (%target-word-size (cpu-word-size cpu)))
      (thunk))))

(define (cpu-endianness cpu)
  "Return the endianness for CPU."
  (if (string=? cpu (triplet-cpu %host-type))
      (native-endianness)
      (cond ((string-match "^i[0-9]86$" cpu)
             (endianness little))
            ((member cpu '("x86_64" "ia64"
                           "powerpcle" "powerpc64le" "mipsel" "mips64el"))
             (endianness little))
            ((member cpu '("sparc" "sparc64" "powerpc" "powerpc64" "spu"
                           "mips" "mips64"))
             (endianness big))
            ((string-match "^arm.*el" cpu)
             (endianness little))
            (else
             (error "unknown CPU endianness" cpu)))))

(define (cpu-word-size cpu)
  "Return the word size for CPU."
  (if (string=? cpu (triplet-cpu %host-type))
      %native-word-size
      (cond ((string-match "^i[0-9]86$" cpu) 4)
            ((string-match "64$" cpu) 8)
            ((string-match "64[lbe][lbe]$" cpu) 8)
            ((member cpu '("sparc" "powerpc" "mips")) 4)
            ((string-match "^arm.*" cpu) 4)
            (else "unknown CPU word size" cpu))))

(define (triplet-cpu t)
  (substring t 0 (string-index t #\-)))

(define (triplet-vendor t)
  (let ((start (1+ (string-index t #\-))))
    (substring t start (string-index t #\- start))))

(define (triplet-os t)
  (let ((start (1+ (string-index t #\- (1+ (string-index t #\-))))))
    (substring t start)))


(define (target-type)
  "Return the GNU configuration triplet of the target platform."
  (fluid-ref %target-type))

(define (target-cpu)
  "Return the CPU name of the target platform."
  (triplet-cpu (target-type)))

(define (target-vendor)
  "Return the vendor name of the target platform."
  (triplet-vendor (target-type)))

(define (target-os)
  "Return the operating system name of the target platform."
  (triplet-os (target-type)))

(define (target-endianness)
  "Return the endianness object of the target platform."
  (fluid-ref %target-endianness))

(define (target-word-size)
  "Return the word size, in bytes, of the target platform."
  (fluid-ref %target-word-size))
