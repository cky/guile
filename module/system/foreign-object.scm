;;; Wrapping foreign objects in Scheme

;;; Copyright (C) 2014 Free Software Foundation, Inc.
;;; 
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;; 
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;; 
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;;; 

;;; Commentary:
;;
;;
;;; Code:

(define-module (system foreign-object)
  #:use-module (oop goops)
  #:export     (make-foreign-object-type
                define-foreign-object-type))

(eval-when (eval load expand)
  (load-extension (string-append "libguile-" (effective-version))
                  "scm_init_foreign_object"))

(define-class <finalizer-class> (<class>)
  (finalizer #:init-keyword #:finalizer #:init-value #f
             #:getter finalizer))

(define-method (allocate-instance (class <finalizer-class>) initargs)
  (let ((instance (next-method))
        (finalizer (finalizer class)))
    (when finalizer
      (%add-finalizer! instance finalizer))
    instance))

(define (getter-method class slot-name existing)
  (let ((getter (ensure-generic existing slot-name))
        (slot-def (or (assq slot-name (slot-ref class 'getters-n-setters))
                      (slot-missing class slot-name))))
    (add-method! getter (compute-getter-method class slot-def))
    getter))

(define* (make-foreign-object-type name slots #:key finalizer)
  (unless (symbol? name)
    (error "type name should be a symbol" name))
  (unless (or (not finalizer) (procedure? finalizer))
    (error "finalizer should be a procedure" finalizer))
  (let ((dslots (map (lambda (slot)
                       (unless (symbol? slot)
                         (error "slot name should be a symbol" slot))
                       (list slot #:class <foreign-slot>
                             #:init-keyword (symbol->keyword slot)
                             #:init-value 0))
                   slots)))
    (if finalizer
        (make-class '() dslots #:name name
                    #:finalizer finalizer #:metaclass <finalizer-class>)
        (make-class '() dslots #:name name))))

(define-syntax define-foreign-object-type
  (lambda (x)
    (define (kw-apply slots)
      (syntax-case slots ()
        (() #'())
        ((slot . slots)
         (let ((kw (symbol->keyword (syntax->datum #'slot))))
           #`(#,kw slot . #,(kw-apply #'slots))))))

    (syntax-case x ()
      ((_ name constructor (slot ...) kwarg ...)
       #`(begin
           (define name
             (make-foreign-object-type 'name '(slot ...) kwarg ...))
           (define slot
             (getter-method name 'slot (and (defined? 'slot) slot)))
           ...
           (define constructor
             (lambda (slot ...)
               (make name #,@(kw-apply #'(slot ...))))))))))
