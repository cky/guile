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

(define-module (language tree-il inline)
  #:use-module (system base syntax)
  #:use-module (language tree-il)
  #:use-module (srfi srfi-16)
  #:export (expand-primitives!))

(define *primitive-expand-table* (make-hash-table))

(define (expand-primitives! x)
  (pre-order!
   (lambda (x)
     (record-case x
       ((<application> src proc args)
        (and (primitive-ref? proc)
             (let ((expand (hashq-ref *primitive-expand-table*
                                      (primitive-ref-name proc))))
               (and expand (apply expand src args)))))
       (else #f)))
   x))

;;; I actually did spend about 10 minutes trying to redo this with
;;; syntax-rules. Patches appreciated.
;;;
(define-macro (define-primitive-expander sym . clauses)
  (define (inline-args args)
    (let lp ((in args) (out '()))
      (cond ((null? in) `(list ,@(reverse out)))
            ((symbol? in) `(cons* ,@(reverse out) ,in))
            ((pair? (car in))
             (lp (cdr in)
                 (cons `(make-application src (make-primitive-ref src ',(caar in))
                                          ,(inline-args (cdar in)))
                       out)))
            ((symbol? (car in))
             ;; assume it's locally bound
             (lp (cdr in) (cons (car in) out)))
            ((number? (car in))
             (lp (cdr in) (cons `(make-const src ,(car in)) out)))
            (else
             (error "what what" (car in))))))
  (define (consequent exp)
    (cond
     ((pair? exp)
      `(make-application src (make-primitive-ref src ',(car exp))
                         ,(inline-args (cdr exp))))
     ((symbol? exp)
      ;; assume locally bound
      exp)
     ((number? exp)
      `(make-const src ,exp))
     (else (error "bad consequent yall" exp))))
  `(hashq-set! *primitive-expand-table*
               ',sym
               (case-lambda
                ,@(let lp ((in clauses) (out '()))
                    (if (null? in)
                        (reverse (cons '(else #f) out))
                        (lp (cddr in)
                            (cons `((src . ,(car in))
                                    ,(consequent (cadr in))) out)))))))

(define-primitive-expander +
  () 0
  (x) x
  (x y z . rest) (+ x (+ y z . rest)))
  
(define-primitive-expander *
  () 1
  (x) x
  (x y z . rest) (* x (* y z . rest)))
  
(define-primitive-expander -
  (x) (- 0 x)
  (x y z . rest) (- x (+ y z . rest)))
  
(define-primitive-expander 1-
  (x) (- x 1))

(define-primitive-expander /
  (x) (/ 1 x)
  (x y z . rest) (div x (* y z . rest)))
  
(define-primitive-expander caar (x) (car (car x)))
(define-primitive-expander cadr (x) (car (cdr x)))
(define-primitive-expander cdar (x) (cdr (car x)))
(define-primitive-expander cddr (x) (cdr (cdr x)))
(define-primitive-expander caaar (x) (car (car (car x))))
(define-primitive-expander caadr (x) (car (car (cdr x))))
(define-primitive-expander cadar (x) (car (cdr (car x))))
(define-primitive-expander caddr (x) (car (cdr (cdr x))))
(define-primitive-expander cdaar (x) (cdr (car (car x))))
(define-primitive-expander cdadr (x) (cdr (car (cdr x))))
(define-primitive-expander cddar (x) (cdr (cdr (car x))))
(define-primitive-expander cdddr (x) (cdr (cdr (cdr x))))
(define-primitive-expander caaaar (x) (car (car (car (car x)))))
(define-primitive-expander caaadr (x) (car (car (car (cdr x)))))
(define-primitive-expander caadar (x) (car (car (cdr (car x)))))
(define-primitive-expander caaddr (x) (car (car (cdr (cdr x)))))
(define-primitive-expander cadaar (x) (car (cdr (car (car x)))))
(define-primitive-expander cadadr (x) (car (cdr (car (cdr x)))))
(define-primitive-expander caddar (x) (car (cdr (cdr (car x)))))
(define-primitive-expander cadddr (x) (car (cdr (cdr (cdr x)))))
(define-primitive-expander cdaaar (x) (cdr (car (car (car x)))))
(define-primitive-expander cdaadr (x) (cdr (car (car (cdr x)))))
(define-primitive-expander cdadar (x) (cdr (car (cdr (car x)))))
(define-primitive-expander cdaddr (x) (cdr (car (cdr (cdr x)))))
(define-primitive-expander cddaar (x) (cdr (cdr (car (car x)))))
(define-primitive-expander cddadr (x) (cdr (cdr (car (cdr x)))))
(define-primitive-expander cdddar (x) (cdr (cdr (cdr (car x)))))
(define-primitive-expander cddddr (x) (cdr (cdr (cdr (cdr x)))))

(define-primitive-expander cons*
  (x) x
  (x y) (cons x y)
  (x y . rest) (cons x (cons* y . rest)))

(define-primitive-expander acons
  (x y z) (cons (cons x y) z))
