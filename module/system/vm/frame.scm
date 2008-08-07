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
  :use-module (system vm program)
  :export (frame-number frame-address
           print-frame print-frame-call
           frame-arguments frame-local-variables frame-external-variables
           frame-environment
           frame-variable-exists? frame-variable-ref frame-variable-set!
           frame-object-name
           frame-local-ref frame-external-link frame-local-set!
           frame-return-address frame-program
           frame-dynamic-link frame?))

(dynamic-call "scm_init_frames" (dynamic-link "libguile-vm"))

;;;
;;; Frame chain
;;;

(define frame-number (make-object-property))
(define frame-address (make-object-property))

(define (make-frame-chain frame addr)
  (let* ((link (frame-dynamic-link frame))
	 (chain (if (eq? link #t)
		  '()
		  (cons frame (make-frame-chain
			       link (frame-return-address frame))))))
    (set! (frame-number frame) (length chain))
    (set! (frame-address frame)
	  (- addr (program-base (frame-program frame))))
    chain))


;;;
;;; Pretty printing
;;;

(define (print-frame frame)
  (format #t "#~A " (frame-number frame))
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
		       (frame-arguments frame)))))

(define (program-name frame)
  (let ((prog (frame-program frame))
	(link (frame-dynamic-link frame)))
    (or (object-property prog 'name)
	(frame-object-name link (1- (frame-address link)) prog)
	(hash-fold (lambda (s v d) (if (eq? prog (variable-ref v)) s d))
		   prog (module-obarray (current-module))))))


;;;
;;; Frames
;;;

(define (frame-arguments frame)
  (let* ((prog (frame-program frame))
	 (arity (program-arity prog)))
    (do ((n (+ (arity:nargs arity) -1) (1- n))
	 (l '() (cons (frame-local-ref frame n) l)))
	((< n 0) l))))

(define (frame-local-variables frame)
  (let* ((prog (frame-program frame))
	 (arity (program-arity prog)))
    (do ((n (+ (arity:nargs arity) (arity:nlocs arity) -1) (1- n))
	 (l '() (cons (frame-local-ref frame n) l)))
	((< n 0) l))))

(define (frame-external-variables frame)
  (frame-external-link frame))

(define (frame-external-ref frame index)
  (list-ref (frame-external-link frame) index))

(define (frame-external-set! frame index val)
  (list-set! (frame-external-link frame) index val))

(define (frame-binding-ref frame binding)
  (if (binding:extp binding)
    (frame-external-ref frame (binding:index binding))
    (frame-local-ref frame (binding:index binding))))

(define (frame-binding-set! frame binding val)
  (if (binding:extp binding)
    (frame-external-set! frame (binding:index binding) val)
    (frame-local-set! frame (binding:index binding) val)))

(define (frame-bindings frame addr)
  (do ((bs (program-bindings (frame-program frame)) (cdr bs))
       (ls '() (if (cdar bs) (cons (cdar bs) ls) (cdr ls))))
      ((or (null? bs) (> (caar bs) addr))
       (apply append ls))))

(define (frame-lookup-binding frame addr sym)
  (do ((bs (frame-bindings frame addr) (cdr bs)))
      ((or (null? bs) (eq? sym (binding:name (car bs))))
       (and (pair? bs) (car bs)))))

(define (frame-object-binding frame addr obj)
  (do ((bs (frame-bindings frame addr) (cdr bs)))
      ((or (null? bs) (eq? obj (frame-binding-ref frame (car bs))))
       (and (pair? bs) (car bs)))))

(define (frame-environment frame addr)
  (map (lambda (binding)
	 (cons (binding:name binding) (frame-binding-ref frame binding)))
       (frame-bindings frame addr)))

(define (frame-variable-exists? frame addr sym)
  (if (frame-lookup-binding frame addr sym) #t #f))

(define (frame-variable-ref frame addr sym)
  (cond ((frame-lookup-binding frame addr sym) =>
	 (lambda (binding) (frame-binding-ref frame binding)))
	(else (error "Unknown variable:" sym))))

(define (frame-variable-set! frame addr sym val)
  (cond ((frame-lookup-binding frame addr sym) =>
	 (lambda (binding) (frame-binding-set! frame binding val)))
	(else (error "Unknown variable:" sym))))

(define (frame-object-name frame addr obj)
  (cond ((frame-object-binding frame addr obj) => binding:name)
	(else #f)))
