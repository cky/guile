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
  #:use-module (language objcode spec)
  #:use-module (system il glil)
  #:use-module (system vm assemble)
  #:export (glil))

(define (write-glil exp . port)
  (apply write (unparse-glil exp) port))

(define (translate x)
  ;; Don't wrap in a thunk -- if you're down in these weeds you can
  ;; thunk it yourself. We don't know how many locs there will be,
  ;; anyway.
  (parse-glil x))

(define (compile x e opts)
  (values (assemble x e) e))

(define-language glil
  #:title	"Guile Lowlevel Intermediate Language (GLIL)"
  #:version	"0.3"
  #:reader	read
  #:printer	write-glil
  #:parser      translate
  #:compilers   `((,objcode . ,compile))
  )
