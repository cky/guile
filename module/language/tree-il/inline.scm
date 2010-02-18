;;; a simple inliner

;; Copyright (C) 2009, 2010 Free Software Foundation, Inc.

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
  #:use-module (system base pmatch)
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
  (define (inline1 x)
    (record-case x
      ((<application> src proc args)
       (record-case proc
         ;; ((lambda (y ...) x) z ...) => (let ((y z) ...) x)
         ((<lambda> body)
          (let lp ((lcase body))
            (and lcase
                 (record-case lcase
                   ((<lambda-case> req opt rest kw inits vars body alternate)
                    (if (and (= (length vars) (length req) (length args)))
                        (let ((x (make-let src req vars args body)))
                          (or (inline1 x) x))
                        (lp alternate)))))))

         ((<primitive-ref> name)
          (case name
            ((@call-with-values)
             (pmatch args
               ;; (call-with-values (lambda () foo) (lambda (a b . c) bar))
               ;; => (let-values (((a b . c) foo)) bar)
               ;;
               ;; Note that this is a singly-binding form of let-values.
               ;; Also note that Scheme's let-values expands into
               ;; call-with-values, then here we reduce it to tree-il's
               ;; let-values.
               ((,producer ,consumer)
                (guard (lambda? consumer)
                       (lambda-case? (lambda-body consumer))
                       (not (lambda-case-opt (lambda-body consumer)))
                       (not (lambda-case-kw (lambda-body consumer)))
                       (not (lambda-case-alternate (lambda-body consumer))))
                (make-let-values
                 src
                 (let ((x (make-application src producer '())))
                   (or (inline1 x) x))
                 (lambda-body consumer)))
               (else #f)))

            ((memq memv)
             (pmatch args
               ((,k ,l) (guard (const? l) (list? (const-exp l)))
                (if (null? (const-exp l))
                    (make-const #f #f)
                    (let lp ((elts (const-exp l)))
                      (let ((test (make-application
                                   #f
                                   (make-primitive-ref #f (case name
                                                            ((memq) 'eq?)
                                                            ((memv) 'eqv?)
                                                            (else (error "what"))))
                                   (list k (make-const #f (car elts))))))
                        (if (null? (cdr elts))
                            test
                            (make-conditional
                             src
                             test
                             (make-const #f #t)
                             (lp (cdr elts))))))))

               (else #f)))

            (else #f)))

         (else #f)))
       
      ((<let> vars body)
       (if (null? vars) body x))
       
      ((<letrec> vars body)
       (if (null? vars) body x))
       
      ((<fix> vars body)
       (if (null? vars) body x))
       
      ((<prompt> src tag body handler)
       ;; If the handler is a simple lambda, inline it.
       (if (and (lambda? handler)
                (record-case (lambda-body handler)
                  ((<lambda-case> req opt kw rest alternate)
                   (and (pair? req) (not opt) (not kw) (not alternate)))
                  (else #f)))
           (make-prompt src tag body (lambda-body handler))
           x))
       
      (else #f)))
  (post-order! inline1 x))
