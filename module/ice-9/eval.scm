;;; -*- mode: scheme; coding: utf-8; -*-

;;;; Copyright (C) 2009
;;;; Free Software Foundation, Inc.
;;;;
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
;;;;



;;; Commentary:

;;; Scheme eval, written in Scheme!
;;;

;;; Code:



;; (put 'memoized-expression-case 'scheme-indent-function 1)
(eval-when (compile)
  (define-syntax capture-env
    (syntax-rules ()
      ((_ env)
       (if (null? env)
           (current-module)
           (if (not env)
               the-root-module
               env)))))

  ;; could be more straightforward if we had better copy propagation
  (define-syntax mx-bind
    (lambda (x)
      (syntax-case x ()
        ((_ data () body)
         #'body)
        ((_ data (a . b) body) (and (identifier? #'a) (identifier? #'b))
         #'(let ((a (car data))
                 (b (cdr data)))
             body))
        ((_ data (a . b) body) (identifier? #'a)
         #'(let ((a (car data))
                 (xb (cdr data)))
             (mx-bind xb b body)))
        ((_ data (a . b) body) 
         #'(let ((xa (car data))
                 (xb (cdr data)))
             (mx-bind xa a (mx-bind xb b body))))
        ((_ data v body) (identifier? #'v)
         #'(let ((v data))
             body)))))
  
  (define-syntax mx-match
    (lambda (x)
      (syntax-case x (quote)
        ((_ mx data tag)
         #'(error "what" mx))
        ((_ mx data tag (('type pat) body) c* ...)
         #`(if (eqv? tag #,(or (memoized-typecode (syntax->datum #'type))
                               (error "not a typecode" #'type)))
               (mx-bind data pat body)
               (mx-match mx data tag c* ...))))))

  (define-syntax memoized-expression-case
    (lambda (x)
      (syntax-case x ()
        ((_ mx c ...)
         #'(let ((tag (memoized-expression-typecode mx))
                 (data (memoized-expression-data mx)))
             (mx-match mx data tag c ...)))))))


(define primitive-eval
  (let ()
    (define (eval exp env)
      (memoized-expression-case exp
        (('begin (first . rest))
         (let lp ((first first) (rest rest))
           (if (null? rest)
               (eval first env)
               (begin
                 (eval first env)
                 (lp (car rest) (cdr rest))))))
      
        (('if (test consequent . alternate))
         (if (eval test env)
             (eval consequent env)
             (eval alternate env)))
      
        (('let (inits . body))
         (let lp ((inits inits) (new-env (capture-env env)))
           (if (null? inits)
               (eval body new-env)
               (lp (cdr inits)
                   (cons (eval (car inits) env) new-env)))))
      
        (('lambda (nreq rest? . body))
         (let ((env (capture-env env)))
           (lambda args
             (let lp ((env env) (nreq nreq) (args args))
               (if (zero? nreq)
                   (eval body
                         (if rest?
                             (cons args env)
                             (if (not (null? args))
                                 (error "too many args" args)
                                 env)))
                   (if (null? args)
                       (error "too few args" nreq)
                       (lp (cons (car args) env)
                           (1- nreq)
                           (cdr args))))))))

        (('quote x)
         x)

        (('define (name . x))
         (define! name (eval x env)))
      
        (('apply (f args))
         (apply (eval f env) (eval args env)))

        (('call (f . args))
         (let ((proc (eval f env)))
           (let eval-args ((in args) (out '()))
             (if (null? in)
                 (apply proc (reverse out))
                 (eval-args (cdr in)
                            (cons (eval (car in) env) out))))))
      
        (('call/cc proc)
         (call/cc (eval proc env)))

        (('call-with-values (producer . consumer))
         (call-with-values (eval producer env)
           (eval consumer env)))

        (('lexical-ref n)
         (let lp ((n n) (env env))
           (if (zero? n)
               (car env)
               (lp (1- n) (cdr env)))))
      
        (('lexical-set! (n . x))
         (let ((val (eval x env)))
           (let lp ((n n) (env env))
             (if (zero? n)
                 (set-car! env val)
                 (lp (1- n) (cdr env))))))
        
        (('toplevel-ref var-or-sym)
         (variable-ref
          (if (variable? var-or-sym)
              var-or-sym
              (let lp ((env env))
                (if (pair? env)
                    (lp (cdr env))
                    (memoize-variable-access! exp (capture-env env)))))))

        (('toplevel-set! (var-or-sym . x))
         (variable-set!
          (if (variable? var-or-sym)
              var-or-sym
              (let lp ((env env))
                (if (pair? env)
                    (lp (cdr env))
                    (memoize-variable-access! exp (capture-env env)))))
          (eval x env)))
      
        (('module-ref var-or-spec)
         (variable-ref
          (if (variable? var-or-spec)
              var-or-spec
              (memoize-variable-access! exp #f))))

        (('module-set! (x . var-or-spec))
         (variable-set!
          (if (variable? var-or-spec)
              var-or-spec
              (memoize-variable-access! exp #f))
          (eval x env)))))
  
    (lambda (exp)
      (eval 
       (memoize-expression ((or (module-transformer (current-module)) identity)
                            exp))
       '()))))

