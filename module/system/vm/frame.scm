;;; Guile VM frame utilities

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

(define-module (system vm frame)
  :use-module (system vm core)
  :export (frame->call))

(define (frame->call frame)
  (let* ((prog (frame-program frame))
	 (nargs (car (program-arity prog))))
    (do ((i 0 (1+ i))
	 (l (vector->list (frame-variables frame)) (cdr l))
	 (r '() (cons (car l) r)))
	((= i nargs) (cons (program-name prog) r)))))

(define (program-name x)
  (hash-fold (lambda (s v d) (if (eq? x (variable-ref v)) s d)) x
	     (module-obarray (current-module))))
