;;; Guile VM core

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

(define-module (system vm core))

(dynamic-call "scm_init_vm" (dynamic-link "libguilevm.so"))

(export vms:cons vms:time vms:clock)

(define (vms:cons stat) (vector-ref stat 0))
(define (vms:time stat) (vector-ref stat 1))
(define (vms:clock stat) (vector-ref stat 2))

(module-export! (current-module)
		(delq! '%module-public-interface
		       (hash-fold (lambda (k v d) (cons k d)) '()
				  (module-obarray (current-module)))))
