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

(define-module (language bytecode spec)
  #:use-module (system base language)
  #:use-module (system vm objcode)
  #:export (bytecode))

(define (compile-objcode x e opts)
  (values (bytecode->objcode x) e))

(define (decompile-objcode x e opts)
  (values (objcode->bytecode x) e))

(define-language bytecode
  #:title	"Guile Bytecode Vectors"
  #:version	"0.3"
  #:reader	read
  #:printer	write
  #:compilers   `((objcode . ,compile-objcode))
  #:decompilers `((objcode . ,decompile-objcode))
  )
