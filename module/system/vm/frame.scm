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

(define-public (vm-return-value vm)
  (car (vm-fetch-stack vm)))

(define-public (frame-local-ref frame index)
  (vector-ref (frame-local-variables frame) index))

(define-public (frame-external-ref frame index)
  (list-ref (frame-external-link frame) index))


;;;
;;; Debug frames
;;;

(define-public frame-index (make-object-property))
(define-public frame-address (make-object-property))

(define-public (vm-last-frame-stack vm)
  (make-frame-stack (vm-last-frame vm) (vm:ip vm)))

(define-public (vm-current-frame-stack vm)
  (make-frame-stack (vm-current-frame vm) (vm:ip vm)))

(define (make-frame-stack frame addr)
  (cond ((frame-dynamic-link frame) =>
	 (lambda (link)
	   (let ((stack (make-frame-stack link (frame-return-address frame)))
		 (base (program-base (frame-program frame))))
	     (set! (frame-index frame) (1+ (length stack)))
	     (set! (frame-address frame) (- addr base))
	     (cons frame stack))))
	(else '())))

(define-public (frame-bindings frame addr)
  (do ((bs (program-bindings (frame-program frame)) (cdr bs))
       (ls '() (if (cdar bs) (cons (cdar bs) ls) (cdr ls))))
      ((or (null? bs) (> (caar bs) addr))
       (apply append ls))))

(define-public (frame-environment frame addr)
  (map (lambda (binding)
	 (let ((name (car binding))
	       (extp (cadr binding))
	       (index (caddr binding)))
	   (cons name (if extp
			(frame-external-ref frame index)
			(frame-local-ref frame index)))))
       (frame-bindings frame addr)))

(define (frame-variable-ref frame sym)
  (cond ((assq sym (frame-environment frame)) => cdr)
	(else (error "Unbound"))))

(define (frame-object-name frame obj)
  (display (frame-address frame))
  (let loop ((alist (frame-environment frame (frame-address frame))))
    (cond ((null? alist) #f)
	  ((eq? obj (cdar alist)) (caar alist))
	  (else (loop (cdr alist))))))


;;;
;;; Pretty printing
;;;

(define-public (frame-call-list frame)
  (let* ((prog (frame-program frame))
	 (locs (vector->list (frame-local-variables frame)))
	 (args (list-truncate locs (car (program-arity prog))))
	 (name (or (frame-object-name (frame-dynamic-link frame) prog)
		   (object-name prog))))
    (cons name args)))

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
  (write (abbrev (frame-call-list frame))))

(define-public (print-frame frame)
  (format #t "#~A " (frame-index frame))
  (print-frame-call frame)
  (newline))

(define (list-truncate l n)
  (do ((i 0 (1+ i))
       (l l (cdr l))
       (r '() (cons (car l) r)))
      ((= i n) (reverse! r))))

(define (object-name x)
  (or (object-property x 'name)
      (hash-fold (lambda (s v d) (if (eq? x (variable-ref v)) s d))
		 x (module-obarray (current-module)))))
