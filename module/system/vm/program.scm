;;; Guile VM program functions

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

(define-module (system vm program)
  #:export (make-program

            arity:nargs arity:nrest arity:nlocs arity:nexts

            make-binding binding:name binding:extp binding:index
            binding:start binding:end

            source:addr source:line source:column source:file
            program-bindings program-sources program-source
            program-properties program-property program-documentation
            program-name program-arguments
           
            program-arity program-external-set! program-meta
            program-objcode program? program-objects
            program-module program-base program-external))

(load-extension "libguile" "scm_init_programs")

(define arity:nargs car)
(define arity:nrest cadr)
(define arity:nlocs caddr)
(define arity:nexts cadddr)

(define (make-binding name extp index start end)
  (list name extp index start end))
(define (binding:name b) (list-ref b 0))
(define (binding:extp b) (list-ref b 1))
(define (binding:index b) (list-ref b 2))
(define (binding:start b) (list-ref b 3))
(define (binding:end b) (list-ref b 4))

(define (source:addr source)
  (car source))
(define (source:file source)
  (cadr source))
(define (source:line source)
  (caddr source))
(define (source:column source)
  (cdddr source))

(define (program-property prog prop)
  (assq-ref (program-properties proc) prop))

(define (program-documentation prog)
  (assq-ref (program-properties prog) 'documentation))

(define (program-arguments prog)
  (let ((bindings (program-bindings prog))
        (nargs (arity:nargs (program-arity prog)))
        (rest? (not (zero? (arity:nrest (program-arity prog))))))
    (if bindings
        (let ((args (map binding:name (list-head bindings nargs))))
          (if rest?
              `((required . ,(list-head args (1- (length args))))
                (rest . ,(car (last-pair args))))
              `((required . ,args))))
        #f)))

(define (program-bindings-as-lambda-list prog)
  (let ((bindings (program-bindings prog))
        (nargs (arity:nargs (program-arity prog)))
        (rest? (not (zero? (arity:nrest (program-arity prog))))))
    (if (not bindings)
        (if rest? (cons (1- nargs) 1) (list nargs))
        (let ((args (map binding:name (list-head bindings nargs))))
          (if rest?
              (apply cons* args)
              args)))))

(define (write-program prog port)
  (format port "#<program ~a ~a>"
          (or (program-name prog)
              (and=> (program-source prog 0)
                     (lambda (s)
                       (format #f "~a at ~a:~a:~a"
                               (number->string (object-address prog) 16)
                               (or (source:file s) "<unknown port>")
                               (source:line s) (source:column s))))
              (number->string (object-address prog) 16))
          (program-bindings-as-lambda-list prog)))
