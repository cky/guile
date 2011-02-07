;;; a simple inliner

;; Copyright (C) 2009, 2010, 2011 Free Software Foundation, Inc.

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

(define (boolean-value x)
  (let ((src (tree-il-src x)))
    (record-case x
      ((<void>)
       (make-const src #t))

      ((<conditional> test consequent alternate)
       (record-case (boolean-value test)
         ((<const> exp)
          (case exp
            ((#t) (boolean-value consequent))
            ((#f) (boolean-value alternate))
            (else x)))
         (else x)))
      
      ((<application> src proc args)
       (record-case proc
         ;; ((lambda (y ...) x) z ...) => (let ((y z) ...) x)
         ((<primitive-ref> name)
          (case name
            ((memq memv)
             (pmatch args
               ((,k ,l) (guard (const? l) (list? (const-exp l)))
                (cond
                 ((null? (const-exp l))
                  (make-const #f #f))
                 ((const? k)
                  (make-const #f (->bool ((case name
                                            ((memq) memq)
                                            ((memv) memv)
                                            (else (error "unexpected member func" name)))
                                          (const-exp k) (const-exp l)))))
                 (else
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
                           (lp (cdr elts)))))))))

               (else x)))

            (else x)))

         (else x)))
       
      ((<lambda> meta body)
       (make-const src #t))

      ((<const> exp)
       (make-const src (not (not exp))))

      (else
       x))))

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
                   ((<lambda-case> req opt rest kw inits gensyms body alternate)
                    (if (and (= (length gensyms) (length req) (length args)))
                        (let ((x (make-let src req gensyms args body)))
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

            (else #f)))

         (else #f)))
       
      ((<conditional> test consequent alternate)
       (let ((btest (boolean-value test)))
         (or (record-case btest
               ((<const> exp)
                (case exp
                  ((#t) consequent)
                  ((#f) alternate)
                  (else #f)))
               (else #f))
             (if (eq? test btest)
                 x
                 (make-conditional (conditional-src x)
                                   btest consequent alternate)))))

      ((<let> gensyms body)
       (if (null? gensyms) body x))
       
      ((<letrec> gensyms body)
       (if (null? gensyms) body x))
       
      ((<fix> gensyms body)
       (if (null? gensyms) body x))
       
      ((<lambda-case> req opt rest kw gensyms body alternate)
       (define (args-compatible? args gensyms)
         (let lp ((args args) (gensyms gensyms))
           (cond
            ((null? args) (null? gensyms))
            ((null? gensyms) #f)
            ((and (lexical-ref? (car args))
                  (eq? (lexical-ref-gensym (car args)) (car gensyms)))
             (lp (cdr args) (cdr gensyms)))
            (else #f))))
         
       (and (not opt) (not kw) rest (not alternate)
            (record-case body
              ((<application> proc args)
               ;; (lambda args (apply (lambda ...) args)) => (lambda ...)
               (and (primitive-ref? proc)
                    (eq? (primitive-ref-name proc) '@apply)
                    (pair? args)
                    (lambda? (car args))
                    (args-compatible? (cdr args) gensyms)
                    (lambda-body (car args))))
              (else #f))))

      ;; Actually the opposite of inlining -- if the prompt cannot be proven to
      ;; be escape-only, ensure that its body is the application of a thunk.
      ((<prompt> src tag body handler)
       (define (escape-only? handler)
         (and (pair? (lambda-case-req handler))
              (let ((cont (car (lambda-case-gensyms handler))))
                (tree-il-fold (lambda (leaf escape-only?)
                                (and escape-only?
                                     (not
                                      (and (lexical-ref? leaf)
                                           (eq? (lexical-ref-gensym leaf) cont)))))
                              (lambda (down escape-only?) escape-only?)
                              (lambda (up escape-only?) escape-only?)
                              #t
                              (lambda-case-body handler)))))
       (define (make-thunk body)
         (make-lambda #f '() (make-lambda-case #f '() #f #f #f '() '() body #f)))

       (if (or (and (application? body)
                    (lambda? (application-proc body))
                    (null? (application-args body)))
               (escape-only? handler))
           x
           (make-prompt src tag
                        (make-application #f (make-thunk body) '())
                        handler)))
      
      (else #f)))
  (post-order! inline1 x))
