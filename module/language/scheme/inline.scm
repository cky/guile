;;; GHIL macros

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

(define-module (language scheme inline)
  #:use-module (system base syntax)
  #:use-module (language ghil)
  #:use-module (srfi srfi-16)
  #:export (*inline-table* define-inline try-inline try-inline-with-env))

(define *inline-table* '())

(define-macro (define-inline sym . clauses)
  (define (inline-args args)
    (let lp ((in args) (out '()))
      (cond ((null? in) `(list ,@(reverse out)))
            ((symbol? in) `(cons* ,@(reverse out) ,in))
            ((pair? (car in))
             (lp (cdr in)
                 (cons `(or (try-inline ,(caar in) ,(inline-args (cdar in)))
                            (error "what" ',(car in)))
                       out)))
            ((symbol? (car in))
             ;; assume it's locally bound
             (lp (cdr in) (cons (car in) out)))
            ((number? (car in))
             (lp (cdr in) (cons `(make-ghil-quote #f #f ,(car in)) out)))
            (else
             (error "what what" (car in))))))
  (define (consequent exp)
    (cond
     ((pair? exp)
      `(make-ghil-inline #f #f ',(car exp) ,(inline-args (cdr exp))))
     ((symbol? exp)
      ;; assume locally bound
      exp)
     ((number? exp)
      `(make-ghil-quote #f #f ,exp))
     (else (error "bad consequent yall" exp))))
  `(set! (@ (language scheme inline) *inline-table*)
         (assq-set! (@ (language scheme inline) *inline-table*)
                    ,sym
                    (let ((make-ghil-inline (@ (language ghil) make-ghil-inline))
                          (make-ghil-quote (@ (language ghil) make-ghil-quote))
                          (try-inline (@ (language scheme inline) try-inline)))
                      (case-lambda
                       ,@(let lp ((in clauses) (out '()))
                           (if (null? in)
                               (reverse (cons '(else #f) out))
                               (lp (cddr in)
                                   (cons `(,(car in)
                                           ,(consequent (cadr in))) out)))))))))

(define (try-inline head-value args)
  (and=> (assq-ref *inline-table* head-value)
         (lambda (proc) (apply proc args))))


(define (try-inline-with-env env loc exp)
  (let ((sym (car exp)))
    (let loop ((e env))
      (record-case e
        ((<ghil-toplevel-env> table)
         (let ((mod (current-module)))
           (and (not (assoc-ref table (cons (module-name mod) sym)))
                (module-bound? mod sym)
                (try-inline (module-ref mod sym) (cdr exp)))))
        ((<ghil-env> parent table variables)
         (and (not (assq-ref table sym))
              (loop parent)))))))

(define-inline eq? (x y)
  (eq? x y))

(define-inline eqv? (x y)
  (eqv? x y))

(define-inline equal? (x y)
  (equal? x y))
  
(define-inline = (x y)
  (ee? x y))

(define-inline < (x y)
  (lt? x y))

(define-inline > (x y)
  (gt? x y))

(define-inline <= (x y)
  (le? x y))

(define-inline >= (x y)
  (ge? x y))

(define-inline zero? (x)
  (ee? x 0))
  
(define-inline +
  () 0
  (x) x
  (x y) (add x y)
  (x y . rest) (add x (+ y . rest)))
  
(define-inline *
  () 1
  (x) x
  (x y) (mul x y)
  (x y . rest) (mul x (* y . rest)))
  
(define-inline -
  (x) (sub 0 x)
  (x y) (sub x y)
  (x y . rest) (sub x (+ y . rest)))
  
(define-inline 1-
  (x) (sub x 1))

(define-inline /
  (x) (div 1 x)
  (x y) (div x y)
  (x y . rest) (div x (* y . rest)))
  
(define-inline quotient (x y)
  (quo x y))

(define-inline remainder (x y)
  (rem x y))

(define-inline modulo (x y)
  (mod x y))

(define-inline not (x)
  (not x))

(define-inline pair? (x)
  (pair? x))

(define-inline cons (x y)
  (cons x y))

(define-inline car (x) (car x))
(define-inline cdr (x) (cdr x))

(define-inline set-car! (x y) (set-car! x y))
(define-inline set-cdr! (x y) (set-cdr! x y))

(define-inline caar (x) (car (car x)))
(define-inline cadr (x) (car (cdr x)))
(define-inline cdar (x) (cdr (car x)))
(define-inline cddr (x) (cdr (cdr x)))
(define-inline caaar (x) (car (car (car x))))
(define-inline caadr (x) (car (car (cdr x))))
(define-inline cadar (x) (car (cdr (car x))))
(define-inline caddr (x) (car (cdr (cdr x))))
(define-inline cdaar (x) (cdr (car (car x))))
(define-inline cdadr (x) (cdr (car (cdr x))))
(define-inline cddar (x) (cdr (cdr (car x))))
(define-inline cdddr (x) (cdr (cdr (cdr x))))
(define-inline caaaar (x) (car (car (car (car x)))))
(define-inline caaadr (x) (car (car (car (cdr x)))))
(define-inline caadar (x) (car (car (cdr (car x)))))
(define-inline caaddr (x) (car (car (cdr (cdr x)))))
(define-inline cadaar (x) (car (cdr (car (car x)))))
(define-inline cadadr (x) (car (cdr (car (cdr x)))))
(define-inline caddar (x) (car (cdr (cdr (car x)))))
(define-inline cadddr (x) (car (cdr (cdr (cdr x)))))
(define-inline cdaaar (x) (cdr (car (car (car x)))))
(define-inline cdaadr (x) (cdr (car (car (cdr x)))))
(define-inline cdadar (x) (cdr (car (cdr (car x)))))
(define-inline cdaddr (x) (cdr (car (cdr (cdr x)))))
(define-inline cddaar (x) (cdr (cdr (car (car x)))))
(define-inline cddadr (x) (cdr (cdr (car (cdr x)))))
(define-inline cdddar (x) (cdr (cdr (cdr (car x)))))
(define-inline cddddr (x) (cdr (cdr (cdr (cdr x)))))

(define-inline null? (x)
  (null? x))

(define-inline list? (x)
  (list? x))

(define-inline cons*
  (x) x
  (x y) (cons x y)
  (x y . rest) (cons x (cons* y . rest)))

(define-inline acons
  (x y z) (cons (cons x y) z))
