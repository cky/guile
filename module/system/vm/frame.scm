;;; Guile VM frame functions

;;; Copyright (C) 2001 Free Software Foundation, Inc.
;;; Copyright (C) 2005 Ludovic Courtès  <ludovic.courtes@laas.fr>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA

;;; Code:

(define-module (system vm frame)
  :use-module ((system vm core) :renamer (symbol-prefix-proc 'vm:))
  :export (frame-number frame-address
           vm-current-frame-chain vm-last-frame-chain
           print-frame print-frame-call))


;;;
;;; Frame chain
;;;

(define frame-number (make-object-property))
(define frame-address (make-object-property))

(define (vm-current-frame-chain vm)
  (make-frame-chain (vm:vm-this-frame vm) (vm:vm:ip vm)))

(define (vm-last-frame-chain vm)
  (make-frame-chain (vm:vm-last-frame vm) (vm:vm:ip vm)))

(define (make-frame-chain frame addr)
  (let* ((link (vm:frame-dynamic-link frame))
	 (chain (if (eq? link #t)
		  '()
		  (cons frame (make-frame-chain
			       link (vm:frame-return-address frame))))))
    (set! (frame-number frame) (length chain))
    (set! (frame-address frame)
	  (- addr (program-base (vm:frame-program frame))))
    chain))


;;;
;;; Pretty printing
;;;

(define (print-frame frame)
  (format #t "#~A " (vm:frame-number frame))
  (print-frame-call frame)
  (newline))

(define (print-frame-call frame)
  (define (abbrev x)
    (cond ((list? x)   (if (> (length x) 3)
			 (list (abbrev (car x)) (abbrev (cadr x)) '...)
			 (map abbrev x)))
	  ((pair? x)   (cons (abbrev (car x)) (abbrev (cdr x))))
	  ((vector? x) (case (vector-length x)
			 ((0) x)
			 ((1) (vector (abbrev (vector-ref x 0))))
			 (else (vector (abbrev (vector-ref x 0)) '...))))
	  (else x)))
  (write (abbrev (cons (program-name frame)
		       (vm:frame-arguments frame)))))

(define (program-name frame)
  (let ((prog (vm:frame-program frame))
	(link (vm:frame-dynamic-link frame)))
    (or (object-property prog 'name)
	(vm:frame-object-name link (1- (vm:frame-address link)) prog)
	(hash-fold (lambda (s v d) (if (eq? prog (variable-ref v)) s d))
		   prog (module-obarray (current-module))))))
