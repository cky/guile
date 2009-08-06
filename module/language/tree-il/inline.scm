;;; a simple inliner

;; Copyright (C) 2009 Free Software Foundation, Inc.

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

(define-module (language tree-il inline)
  #:use-module (system base syntax)
  #:use-module (language tree-il)
  #:export (inline!))

;; Possible optimizations:
;; * constant folding, propagation
;; * procedure inlining
;;   * always when single call site
;;   * always for "trivial" procs
;;   * otherwise who knows
;; * dead code elimination
;; * degenerate case optimizations
;; * "fixing letrec"

;; This is a completely brain-dead optimization pass whose sole claim to
;; fame is ((lambda () x)) => x.
(define (inline! x)
  (post-order!
   (lambda (x)
     (record-case x
       ((<application> src proc args)
        (cond

         ;; ((lambda () x)) => x
         ((and (lambda? proc) (null? args))
          (lambda-body proc))

         ;; (call-with-values (lambda () foo) (lambda (a b . c) bar))
         ;; => (let-values (((a b . c) foo)) bar)
         ;;
         ;; Note that this is a singly-binding form of let-values. Also
         ;; note that Scheme's let-values expands into call-with-values,
         ;; then here we reduce it to tree-il's let-values.
         ((and (primitive-ref? proc)
               (eq? (primitive-ref-name proc) '@call-with-values)
               (= (length args) 2)
               (lambda? (cadr args)))
          (let ((producer (car args))
                (consumer (cadr args)))
            (make-let-values src
                             (lambda-names consumer)
                             (lambda-vars consumer)
                             (if (and (lambda? producer)
                                      (null? (lambda-names producer)))
                                 (lambda-body producer)
                                 (make-application src producer '()))
                             (lambda-body consumer))))

         (else #f)))

       (else #f)))
   x))
