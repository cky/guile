;;; Guile VM frame functions

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
  :use-module (system vm core))


;;;
;;; Frame chain
;;;

(define-public frame-number (make-object-property))
(define-public frame-address (make-object-property))

(define-public (vm-current-frame-chain vm)
  (make-frame-chain (vm-this-frame vm) (vm:ip vm)))

(define-public (vm-last-frame-chain vm)
  (make-frame-chain (vm-last-frame vm) (vm:ip vm)))

(define (make-frame-chain frame addr)
  (let* ((link (frame-dynamic-link frame))
	 (chain (if (eq? link #t)
		  '()
		  (cons frame (make-frame-chain
			       link (frame-return-address frame))))))
    (set! (frame-number frame) (length chain))
    (set! (frame-address frame) (- addr (program-base (frame-program frame))))
    chain))


;;;
;;; Pretty printing
;;;

(define-public (print-frame frame)
  (format #t "#~A " (frame-number frame))
  (print-frame-call frame)
  (newline))

(define-public (print-frame-call frame)
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
  (write (abbrev (cons (program-name frame) (frame-arguments frame)))))

(define (program-name frame)
  (let ((prog (frame-program frame))
	(link (frame-dynamic-link frame)))
    (or (object-property prog 'name)
	(frame-object-name link (1- (frame-address link)) prog)
	(hash-fold (lambda (s v d) (if (eq? prog (variable-ref v)) s d))
		   prog (module-obarray (current-module))))))
