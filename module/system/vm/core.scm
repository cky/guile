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

(define-module (system vm core)
  :export (arity:nargs arity:nrest arity:nlocs arity:nexts
           make-binding binding:name binding:extp binding:index
           program-bindings program-sources
           frame-arguments frame-local-variables frame-external-variables
           frame-environment
           frame-variable-exists? frame-variable-ref frame-variable-set!
           frame-object-name
           vm-fetch-locals vm-fetch-externals vm-return-value
           vms:time vms:clock vm-load))

;;;
;;; Core procedures
;;;

(dynamic-call "scm_init_vm" (dynamic-link "libguile-vm"))

(module-export! (current-module)
		(delq! '%module-public-interface
		       (hash-fold (lambda (k v d) (cons k d)) '()
				  (module-obarray (current-module)))))


;;;
;;; Programs
;;;

(define arity:nargs car)
(define arity:nrest cadr)
(define arity:nlocs caddr)
(define arity:nexts cadddr)

(define (make-binding name extp index)
  (list name extp index))

(define binding:name car)
(define binding:extp cadr)
(define binding:index caddr)

(define (program-bindings prog)
  (cond ((program-meta prog) => car)
	(else '())))

(define (program-sources prog)
  (cond ((program-meta prog) => cdr)
	(else '())))


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


;;;
;;; Current status
;;;

(define (vm-fetch-locals vm)
  (frame-local-variables (vm-this-frame vm)))

(define (vm-fetch-externals vm)
  (frame-external-variables (vm-this-frame vm)))

(define (vm-return-value vm)
  (car (vm-fetch-stack vm)))


;;;
;;; Statistics
;;;

(define (vms:time stat) (vector-ref stat 0))
(define (vms:clock stat) (vector-ref stat 1))


;;;
;;; Loader
;;;

(define (vm-load vm objcode)
  (vm (objcode->program objcode)))

;; `load-compiled' is referred to by `boot-9.scm' and used by `use-modules'
;; and friends.
(set! load-compiled (lambda (file) (vm-load (the-vm) (load-objcode file))))
