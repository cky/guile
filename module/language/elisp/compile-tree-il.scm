;;; Guile Emac Lisp

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

(define-module (language elisp compile-tree-il)
  #:use-module (language tree-il)
  #:use-module (system base pmatch)
  #:export (compile-tree-il))


; Find the source properties of some parsed expression if there are any
; associated with it.

(define (location x)
  (and (pair? x)
       (let ((props (source-properties x)))
         (and (not (null? props))
              props))))


; Value to use for Elisp's nil.

(define (nil-value loc) (make-const loc #f))


; Compile a symbol expression.  This is a variable reference or maybe some
; special value like nil.

(define (compile-symbol loc sym)
  (case sym

    ((nil)
     (nil-value loc))

    ((t)
     (make-const loc #t))
    
    ; FIXME: Use fluids.
    (else
      (make-module-ref loc '(language elisp variables) sym #f))))


; Compile a pair-expression (that is, any structure-like construct).

(define (compile-pair loc expr)
  (pmatch expr

    ((progn . ,forms)
     (make-sequence loc (map compile-expr forms)))

    ((if ,condition ,ifclause)
     (make-conditional loc (compile-expr condition)
                           (compile-expr ifclause)
                           (nil-value loc)))
    ((if ,condition ,ifclause ,elseclause)
     (make-conditional loc (compile-expr condition)
                           (compile-expr ifclause)
                           (compile-expr elseclause)))
    ((if ,condition ,ifclause . ,elses)
     (make-conditional loc (compile-expr condition)
                           (compile-expr ifclause)
                           (make-sequence loc (map compile-expr elses))))

    ; For (cond ...) forms, a special case is a (condition) clause without
    ; body.  In this case, the value of condition itself should be returned,
    ; and thus is saved in a local variable for testing and returning, if it
    ; is found true.
    ((cond . ,clauses) (guard (and-map (lambda (el)
                                         (and (list? el) (not (null? el))))
                                       clauses))
     (let iterate ((tail clauses))
       (if (null? tail)
         (nil-value loc)
         (let ((cur (car tail)))
           (if (null? (cdr cur))
             (let ((var (gensym)))
               (make-let loc
                 '(condition) `(,var) `(,(compile-expr (car cur)))
                 (make-conditional loc
                   (make-lexical-ref loc 'condition var)
                   (make-lexical-ref loc 'condition var)
                   (iterate (cdr tail)))))
             (make-conditional loc
               (compile-expr (car cur))
               (make-sequence loc (map compile-expr (cdr cur)))
               (iterate (cdr tail))))))))

    ((and) (nil-value loc))
    ((and . ,expressions)
     (let iterate ((tail expressions))
       (if (null? (cdr tail))
         (compile-expr (car tail))
         (make-conditional loc
           (compile-expr (car tail))
           (iterate (cdr tail))
           (nil-value loc)))))

    (('quote ,val)
     (make-const loc val))

    (else
      (error "unrecognized elisp" expr))))


; Compile a single expression to TreeIL.

(define (compile-expr expr)
  (let ((loc (location expr)))
    (cond
      ((symbol? expr)
       (compile-symbol loc expr))
      ((pair? expr)
       (compile-pair loc expr))
      (else (make-const loc expr)))))


; Entry point for compilation to TreeIL.

(define (compile-tree-il expr env opts)
  (values
    (compile-expr expr)
    env
    env))
