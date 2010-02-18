;;; -*- mode: scheme; coding: utf-8; -*-

;;;; Copyright (C) 2009, 2010
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

;;; Scheme eval, written in Scheme.
;;;
;;; Expressions are first expanded, by the syntax expander (i.e.
;;; psyntax), then memoized into internal forms. The evaluator itself
;;; only operates on the internal forms ("memoized expressions").
;;;
;;; Environments are represented as linked lists of the form (VAL ... .
;;; MOD). If MOD is #f, it means the environment was captured before
;;; modules were booted. If MOD is the literal value '(), we are
;;; evaluating at the top level, and so should track changes to the
;;; current module.
;;;
;;; Evaluate this in Emacs to make code indentation work right:
;;;
;;;    (put 'memoized-expression-case 'scheme-indent-function 1)
;;;

;;; Code:



(eval-when (compile)
  (define-syntax capture-env
    (syntax-rules ()
      ((_ env)
       (if (null? env)
           (current-module)
           (if (not env)
               ;; the and current-module checks that modules are booted,
               ;; and thus the-root-module is defined
               (and (current-module) the-root-module)
               env)))))

  (define-syntax make-closure
    (lambda (x)
      (define *max-static-argument-count* 8)
      (define (make-formals n)
        (map (lambda (i)
               (datum->syntax
                x 
                (string->symbol
                 (string (integer->char (+ (char->integer #\a) i))))))
             (iota n)))
      (syntax-case x ()
        ((_ eval nreq rest? body env) (not (identifier? #'env))
         #'(let ((e env))
             (make-closure eval nreq rest? body e)))
        ((_ eval nreq rest? body env)
         #`(case nreq
             #,@(map (lambda (nreq)
                       (let ((formals (make-formals nreq)))
                         #`((#,nreq)
                            (if rest?
                                (lambda (#,@formals . rest)
                                  (eval body
                                        (cons* rest #,@(reverse formals)
                                               env)))
                                (lambda (#,@formals)
                                  (eval body
                                        (cons* #,@(reverse formals) env)))))))
                     (iota *max-static-argument-count*))
             (else
              #,(let ((formals (make-formals *max-static-argument-count*)))
                  #`(lambda (#,@formals . more)
                      (let lp ((new-env (cons* #,@(reverse formals) env))
                               (nreq (- nreq #,*max-static-argument-count*))
                               (args more))
                        (if (zero? nreq)
                            (eval body
                                  (if rest?
                                      (cons args new-env)
                                      (if (not (null? args))
                                          (scm-error 'wrong-number-of-args
                                                     "eval" "Wrong number of arguments"
                                                     '() #f)
                                          new-env)))
                            (if (null? args)
                                (scm-error 'wrong-number-of-args
                                           "eval" "Wrong number of arguments"
                                           '() #f)
                                (lp (cons (car args) new-env)
                                    (1- nreq)
                                    (cdr args)))))))))))))

  (define-syntax call
    (lambda (x)
      (define *max-static-call-count* 4)
      (syntax-case x ()
        ((_ eval proc nargs args env) (identifier? #'env)
         #`(case nargs
             #,@(map (lambda (nargs)
                       #`((#,nargs)
                          (proc
                           #,@(map
                               (lambda (n)
                                 (let lp ((n n) (args #'args))
                                   (if (zero? n)
                                       #`(eval (car #,args) env)
                                       (lp (1- n) #`(cdr #,args)))))
                               (iota nargs)))))
                     (iota *max-static-call-count*))
             (else
              (apply proc
                     #,@(map
                         (lambda (n)
                           (let lp ((n n) (args #'args))
                             (if (zero? n)
                                 #`(eval (car #,args) env)
                                 (lp (1- n) #`(cdr #,args)))))
                         (iota *max-static-call-count*))
                     (let lp ((exps #,(let lp ((n *max-static-call-count*)
                                               (args #'args))
                                        (if (zero? n)
                                            args
                                            (lp (1- n) #`(cdr #,args)))))
                              (args '()))
                       (if (null? exps)
                           (reverse args)
                           (lp (cdr exps)
                               (cons (eval (car exps) env) args)))))))))))

  ;; This macro could be more straightforward if the compiler had better
  ;; copy propagation. As it is we do some copy propagation by hand.
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
  
  ;; The resulting nested if statements will be an O(n) dispatch. Once
  ;; we compile `case' effectively, this situation will improve.
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


;;;
;;; On 18 Feb 2010, I did a profile of how often the various memoized expression
;;; types occur when getting to a prompt on a fresh build. Here are the numbers
;;; I got:
;;;
;;;      lexical-ref: 32933054
;;;             call: 20281547
;;;     toplevel-ref: 13228724
;;;               if: 9156156
;;;            quote: 6610137
;;;              let: 2619707
;;;           lambda: 1010921
;;;            begin: 948945
;;;      lexical-set: 509862
;;; call-with-values: 139668
;;;            apply: 49402
;;;       module-ref: 14468
;;;           define: 1259
;;;     toplevel-set: 328
;;;          dynwind: 162
;;;      with-fluids: 0
;;;          call/cc: 0
;;;       module-set: 0
;;;
;;; So until we compile `case' into a computed goto, we'll order the clauses in
;;; `eval' in this order, to put the most frequent cases first.
;;;

(define primitive-eval
  (let ()
    ;; The "engine". EXP is a memoized expression.
    (define (eval exp env)
      (memoized-expression-case exp
        (('lexical-ref n)
         (let lp ((n n) (env env))
           (if (zero? n)
               (car env)
               (lp (1- n) (cdr env)))))
      
        (('call (f nargs . args))
         (let ((proc (eval f env)))
           (call eval proc nargs args env)))
        
        (('toplevel-ref var-or-sym)
         (variable-ref
          (if (variable? var-or-sym)
              var-or-sym
              (let lp ((env env))
                (if (pair? env)
                    (lp (cdr env))
                    (memoize-variable-access! exp (capture-env env)))))))

        (('if (test consequent . alternate))
         (if (eval test env)
             (eval consequent env)
             (eval alternate env)))
      
        (('quote x)
         x)

        (('let (inits . body))
         (let lp ((inits inits) (new-env (capture-env env)))
           (if (null? inits)
               (eval body new-env)
               (lp (cdr inits)
                   (cons (eval (car inits) env) new-env)))))
      
        (('lambda (nreq rest? . body))
         (make-closure eval nreq rest? body (capture-env env)))
        
        (('begin (first . rest))
         (let lp ((first first) (rest rest))
           (if (null? rest)
               (eval first env)
               (begin
                 (eval first env)
                 (lp (car rest) (cdr rest))))))
      
        (('lexical-set! (n . x))
         (let ((val (eval x env)))
           (let lp ((n n) (env env))
             (if (zero? n)
                 (set-car! env val)
                 (lp (1- n) (cdr env))))))
        
        (('call-with-values (producer . consumer))
         (call-with-values (eval producer env)
           (eval consumer env)))

        (('apply (f args))
         (apply (eval f env) (eval args env)))

        (('module-ref var-or-spec)
         (variable-ref
          (if (variable? var-or-spec)
              var-or-spec
              (memoize-variable-access! exp #f))))

        (('define (name . x))
         (define! name (eval x env)))
      
        (('toplevel-set! (var-or-sym . x))
         (variable-set!
          (if (variable? var-or-sym)
              var-or-sym
              (let lp ((env env))
                (if (pair? env)
                    (lp (cdr env))
                    (memoize-variable-access! exp (capture-env env)))))
          (eval x env)))
      
        (('dynwind (in exp . out))
         (dynamic-wind (eval in env)
                       (lambda () (eval exp env))
                       (eval out env)))
        
        (('with-fluids (fluids vals . exp))
         (let* ((fluids (map (lambda (x) (eval x env)) fluids))
                (vals (map (lambda (x) (eval x env)) vals)))
           (with-fluids* fluids vals (lambda () (eval exp env)))))
        
        (('call/cc proc)
         (call/cc (eval proc env)))

        (('module-set! (x . var-or-spec))
         (variable-set!
          (if (variable? var-or-spec)
              var-or-spec
              (memoize-variable-access! exp #f))
          (eval x env)))))
  
    ;; primitive-eval
    (lambda (exp)
      "Evaluate @var{exp} in the current module."
      (eval 
       (memoize-expression ((or (module-transformer (current-module))
                                (lambda (x) x))
                            exp))
       '()))))

