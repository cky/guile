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


; Value to use for Elisp's nil and t.

(define (nil-value loc) (make-const loc #f))
(define (t-value loc) (make-const loc #t))


; Modules that contain the value and function slot bindings.

(define runtime '(language elisp runtime))
(define value-slot '(language elisp runtime value-slot))
(define function-slot '(language elisp runtime function-slot))


; Error reporting routine for syntax/compilation problems.

(define (report-error loc . args)
  (apply error args))


; Generate code to ensure a fluid is there for further use of a given symbol.

(define (ensure-fluid! loc sym module)
  ; FIXME: Do this!
  (make-void loc))


; Generate code to reference a fluid saved variable.

(define (reference-variable loc sym module)
  (make-sequence loc
    (list (ensure-fluid! loc sym module)
          (make-application loc (make-primitive-ref loc 'fluid-ref)
            (list (make-module-ref loc module sym #f))))))


; Reference a variable and error if the value is void.

(define (reference-with-check loc sym module)
  (let ((var (gensym)))
    (make-let loc '(value) `(,var) `(,(reference-variable loc sym module))
      (make-conditional loc
        (make-application loc (make-primitive-ref loc 'eq?)
          (list (make-module-ref loc runtime 'void #t)
                (make-lexical-ref loc 'value var)))
        (make-application loc (make-primitive-ref loc 'error)
          (list (make-const loc "variable is void:")
                (make-const loc sym)))
        (make-lexical-ref loc 'value var)))))


; Generate code to set a fluid saved variable.

(define (set-variable! loc sym module value)
  (make-sequence loc
    (list (ensure-fluid! loc sym module)
          (make-application loc (make-primitive-ref loc 'fluid-set!)
            (list (make-module-ref loc module sym #f)
                  value)))))


; Compile a symbol expression.  This is a variable reference or maybe some
; special value like nil.

(define (compile-symbol loc sym)
  (case sym

    ((nil) (nil-value loc))

    ((t) (t-value loc))
    
    (else
      (reference-with-check loc sym value-slot))))


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

    ((and) (t-value loc))
    ((and . ,expressions)
     (let iterate ((tail expressions))
       (if (null? (cdr tail))
         (compile-expr (car tail))
         (make-conditional loc
           (compile-expr (car tail))
           (iterate (cdr tail))
           (nil-value loc)))))

    ((or . ,expressions)
     (let iterate ((tail expressions))
       (if (null? tail)
         (nil-value loc)
         (let ((var (gensym)))
           (make-let loc
             '(condition) `(,var) `(,(compile-expr (car tail)))
             (make-conditional loc
               (make-lexical-ref loc 'condition var)
               (make-lexical-ref loc 'condition var)
               (iterate (cdr tail))))))))

    ; Build a set form for possibly multiple values.  The code is not formulated
    ; tail recursive because it is clearer this way and large lists of symbol
    ; expression pairs are very unlikely.
    ((setq . ,args)
     (make-sequence loc
       (let iterate ((tail args))
         (if (null? tail)
           (list (make-void loc))
           (let ((sym (car tail))
                 (tailtail (cdr tail)))
             (if (not (symbol? sym))
               (report-error loc "expected symbol in setq")
               (if (null? tailtail)
                 (report-error loc "missing value for symbol in setq" sym)
                 (let* ((val (compile-expr (car tailtail)))
                        (op (set-variable! loc sym value-slot val)))
                   (cons op (iterate (cdr tailtail)))))))))))

    (('quote ,val)
     (make-const loc val))

    (else
      (report-error loc "unrecognized elisp" expr))))


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
