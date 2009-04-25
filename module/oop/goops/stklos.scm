;;;; Copyright (C) 1999,2002, 2006 Free Software Foundation, Inc.
;;;; 
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2.1 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;;;; 


(define-module (oop goops stklos)
  :use-module (oop goops internal)
  :no-backtrace
  )

;;;
;;; This is the stklos compatibility module.
;;;
;;; WARNING: This module is under construction.  While we expect to be able
;;; to run most stklos code without problems in the future, this is not the
;;; case now.  The current compatibility is only superficial.
;;;
;;; Any comments/complaints/patches are welcome.  Tell us about
;;; your incompatibility problems (bug-guile@gnu.org).
;;;

;; Export all bindings that are exported from (oop goops)...
(module-for-each (lambda (sym var)
		   (module-add! %module-public-interface sym var))
		 (nested-ref the-root-module '(app modules oop goops
						   %module-public-interface)))

;; ...but replace the following bindings:
(export define-class define-method)

;; Also export the following
(export write-object)

;;; Enable keyword support (*fixme*---currently this has global effect)
(read-set! keywords 'prefix)

(define-syntax define-class
  (syntax-rules ()
    ((_ name supers (slot ...) rest ...)
     (standard-define-class name supers slot ... rest ...))))

(define (toplevel-define! name val)
  (module-define! (current-module) name val))

(define-syntax define-method
  (syntax-rules (setter)
    ((_ (setter name) rest ...)
     (begin
       (if (or (not (defined? 'name))
               (not (is-a? name <generic-with-setter>)))
           (toplevel-define! 'name
                             (ensure-accessor
                              (if (defined? 'name) name #f) 'name)))
       (add-method! (setter name) (method rest ...))))
    ((_ name rest ...)
     (begin
       (if (or (not (defined? 'name))
               (not (or (is-a? name <generic>)
                        (is-a? name <primitive-generic>))))
           (toplevel-define! 'name
                             (ensure-generic
                              (if (defined? 'name) name #f) 'name)))
       (add-method! name (method rest ...))))))
