;;; Guile Emac Lisp language specification

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; This file is part of Guile VM.

;; Guile VM is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; Guile VM is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with Guile VM; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(define-module (lang elisp spec)
  :use-module (system lang language)
  :use-module (lang elisp translate)
  :export (elisp))

(define-language elisp
  #:title	"Emacs Lisp"
  #:version	"0.0"
  #:reader	read
  #:expander	id
  #:translator	translate
  )
