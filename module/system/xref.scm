;;;; 	Copyright (C) 2009 Free Software Foundation, Inc.
;;;; 
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2.1 of the License, or (at your option) any later version.
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


(define-module (system xref)
  #:use-module (system base pmatch)
  #:use-module (system base compile)
  #:use-module (system vm program)
  #:export (*xref-ignored-modules*
            procedure-callees
            procedure-callers))

(define (program-callee-rev-vars prog)
  (cond
   ((program-objects prog)
    => (lambda (objects)
          (let ((n (vector-length objects))
                (progv (make-vector (vector-length objects) #f))
                (asm (decompile (program-objcode prog) #:to 'assembly)))
            (pmatch asm
              ((load-program ,nargs ,nrest ,nlocs ,next ,labels ,len . ,body)
               (for-each
                (lambda (x)
                  (pmatch x
                    ((toplevel-ref ,n) (vector-set! progv n #t))
                    ((toplevel-set ,n) (vector-set! progv n #t))))
                body)))
            (let lp ((i 0) (out '()))
              (cond
               ((= i n) out)
               ((program? (vector-ref objects i))
                (lp (1+ i)
                    (append (program-callee-rev-vars (vector-ref objects i))
                            out)))
               ((vector-ref progv i)
                (let ((obj (vector-ref objects i)))
                  (if (variable? obj)
                      (lp (1+ i) (cons obj out))
                      ;; otherwise it's an unmemoized binding
                      (pmatch obj
                        (,sym (guard (symbol? sym))
                         (let ((v (module-variable (or (program-module prog)
                                                       the-root-module)
                                                   sym)))
                           (lp (1+ i) (if v (cons v out) out))))
                        ((,mod ,sym ,public?)
                         ;; hm, hacky.
                         (let* ((m (nested-ref the-root-module
                                               (append '(%app modules) mod)))
                                (v (and m
                                        (module-variable
                                         (if public?
                                             (module-public-interface m)
                                             m)
                                         sym))))
                           (lp (1+ i)
                               (if v (cons v out) out))))))))
               (else (lp (1+ i) out)))))))
   (else '())))

(define (procedure-callee-rev-vars proc)
  (cond
   ((program? proc) (program-callee-rev-vars proc))
   (else '())))

(define (procedure-callees prog)
  (let lp ((in (procedure-callee-rev-vars prog)) (out '()))
    (cond ((null? in) out)
          ((variable-bound? (car in))
           (lp (cdr in) (cons (variable-ref (car in)) out)))
          (else (lp (cdr in) out)))))

(define *callers-db* #f)

(define *xref-ignored-modules* '((value-history)))
(define (on-module-modified m)
  (if (not (member (module-name m) *xref-ignored-modules*))
      (set! *callers-db* #f)))

(define (ensure-callers-db)
  (let ((visited '())
        (db #f))
    (define (visit-variable var)
      (if (variable-bound? var)
          (let ((x (variable-ref var)))
            (cond
             ((procedure? x)
              (for-each
               (lambda (callee)
                 (if (variable-bound? callee)
                     (let ((y (variable-ref callee)))
                       (hashq-set! db callee (cons x (hashq-ref db callee '()))))))
               (procedure-callee-rev-vars x)))
             ((and (module? x) (not (memq x visited)))
              (visit-module x))))))

    (define (visit-module mod)
      (set! visited (cons mod visited))
      (if (not (memq on-module-modified (module-observers mod)))
          (module-observe mod on-module-modified))
      (module-for-each (lambda (sym var)
                         (visit-variable var))
                       mod))

    (if (not *callers-db*)
        (begin
          (set! db (make-hash-table))
          (visit-module the-root-module)
          (set! *callers-db* db)))))

(define (procedure-callers var)
  (let ((v (cond ((variable? var) var)
                 ((symbol? var) (module-variable (current-module) var))
                 (else
                  (pmatch var
                    ((,modname . ,sym)
                     (module-variable (resolve-module modname) sym))
                    (else
                     (error "expected a variable, symbol, or (modname sym)" var)))))))
    (ensure-callers-db)
    (hashq-ref *callers-db* v '())))
