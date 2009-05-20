;;; Tree-il optimizer

;; Copyright (C) 2009 Free Software Foundation, Inc.

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

(define-module (language tree-il optimize)
  #:use-module (system base syntax)
  #:use-module (language tree-il)
  #:use-module (language tree-il inline)
  #:export (optimize! add-interesting-primitive!))

(define (env-module e)
  (if e (car e) (current-module)))

(define (optimize! x env opts)
  (expand-primitives! (resolve-primitives! x (env-module env))))

;; Possible optimizations:
;; * constant folding, propagation
;; * procedure inlining
;;   * always when single call site
;;   * always for "trivial" procs
;;   * otherwise who knows
;; * dead code elimination
;; * degenerate case optimizations
;; * "fixing letrec"

(define *interesting-primitive-names* 
  '(apply @apply
    call-with-values @call-with-values
    call-with-current-continuation @call-with-current-continuation
    values
    eq? eqv? equal?
    = < > <= >= zero?
    + * - / 1- 1+ quotient remainder modulo
    not
    pair? null? list? acons cons cons*

    list vector

    car cdr
    set-car! set-cdr!

    caar cadr cdar cddr

    caaar caadr cadar caddr cdaar cdadr cddar cdddr

    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr))

(define (add-interesting-primitive! name)
  (hashq-set! *interesting-primitive-vars*
              (module-variable (current-module) name) name))

(define *interesting-primitive-vars* (make-hash-table))

(for-each add-interesting-primitive! *interesting-primitive-names*)

(define (resolve-primitives! x mod)
  (post-order!
   (lambda (x)
     (record-case x
       ((<toplevel-ref> src name)
        (and (hashq-ref *interesting-primitive-vars*
                        (module-variable mod name))
             (make-primitive-ref src name)))
       ((<module-ref> src mod name public?)
        ;; for the moment, we're disabling primitive resolution for
        ;; public refs because resolve-interface can raise errors.
        (let ((m (and (not public?) (resolve-module mod))))
          (and m (hashq-ref *interesting-primitive-vars*
                            (module-variable m name))
               (make-primitive-ref src name))))
       (else #f)))
   x))
