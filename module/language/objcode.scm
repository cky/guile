;;; Guile Virtual Machine Object Code

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

(define-module (language objcode)
  #:export (encode-length decode-length))


;;;
;;; Variable-length interface
;;;

;; NOTE: decoded in vm_fetch_length in vm.c as well.

(define (encode-length len)
  (cond ((< len 254) (u8vector len))
	((< len (* 256 256))
	 (u8vector 254 (quotient len 256) (modulo len 256)))
	((< len most-positive-fixnum)
	 (u8vector 255
		   (quotient len (* 256 256 256))
		   (modulo (quotient len (* 256 256)) 256)
		   (modulo (quotient len 256) 256)
		   (modulo len 256)))
	(else (error "Too long code length:" len))))

(define (decode-length pop)
  (let ((x (pop)))
    (cond ((< x 254) x)
	  ((= x 254) (+ (ash x 8) (pop)))
	  (else
           (let* ((b2 (pop))
                  (b3 (pop))
                  (b4 (pop)))
             (+ (ash x 24) (ash b2 16) (ash b3 8) b4))))))
