;;; Guile Lowlevel Intermediate Language

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

(define-module (language glil spec)
  #:use-module (system base language)
  #:use-module (language glil)
  #:use-module (language glil compile-assembly)
  #:export (glil))

(define (write-glil exp . port)
  (apply write (unparse-glil exp) port))

(define (compile-asm x e opts)
  (values (compile-assembly x) e))

(define-language glil
  #:title	"Guile Lowlevel Intermediate Language (GLIL)"
  #:version	"0.3"
  #:reader	read
  #:printer	write-glil
  #:parser      parse-glil
  #:compilers   `((assembly . ,compile-asm))
  )
