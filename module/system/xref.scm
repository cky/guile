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
  #:use-module (srfi srfi-1)
  #:export (*xref-ignored-modules*
            procedure-callees
            procedure-callers))

(define (program-callee-rev-vars prog)
  (define (cons-uniq x y)
    (if (memq x y) y (cons x y)))
  (cond
   ((program-objects prog)
    => (lambda (objects)
          (let ((n (vector-length objects))
                (progv (make-vector (vector-length objects) #f))
                (asm (decompile (program-objcode prog) #:to 'assembly)))
            (pmatch asm
              ((load-program _ _ _ _ _ . ,body)
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
                    (fold cons-uniq out
                          (program-callee-rev-vars (vector-ref objects i)))))
               ((vector-ref progv i)
                (let ((obj (vector-ref objects i)))
                  (if (variable? obj)
                      (lp (1+ i) (cons-uniq obj out))
                      ;; otherwise it's an unmemoized binding
                      (pmatch obj
                        (,sym (guard (symbol? sym))
                         (let ((v (module-variable (or (program-module prog)
                                                       the-root-module)
                                                   sym)))
                           (lp (1+ i) (if v (cons-uniq v out) out))))
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
                               (if v (cons-uniq v out) out))))))))
               (else (lp (1+ i) out)))))))
   (else '())))

(define (procedure-callee-rev-vars proc)
  (cond
   ((program? proc) (program-callee-rev-vars proc))
   (else '())))

(define (procedure-callees prog)
  "Evaluates to a list of the given program callees."
  (let lp ((in (procedure-callee-rev-vars prog)) (out '()))
    (cond ((null? in) out)
          ((variable-bound? (car in))
           (lp (cdr in) (cons (variable-ref (car in)) out)))
          (else (lp (cdr in) out)))))

;; var -> ((module-name caller ...) ...)
(define *callers-db* #f)
;; module-name -> (callee ...)
(define *module-callees-db* (make-hash-table))
;; (module-name ...)
(define *tainted-modules* '())

(define *xref-ignored-modules* '((value-history)))
(define (on-module-modified m)
  (let ((name (module-name m)))
    (if (and (not (member name *xref-ignored-modules*))
             (not (member name *tainted-modules*))
             (pair? name))
        (set! *tainted-modules* (cons name *tainted-modules*)))))

(define (add-caller callee caller mod-name)
  (let ((all-callers (hashq-ref *callers-db* callee)))
    (if (not all-callers)
        (hashq-set! *callers-db* callee `((,mod-name ,caller)))
        (let ((callers (assoc mod-name all-callers)))
          (if callers
              (if (not (member caller callers))
                  (set-cdr! callers (cons caller (cdr callers))))
              (hashq-set! *callers-db* callee
                          (cons `(,mod-name ,caller) all-callers)))))))

(define (forget-callers callee mod-name)
  (hashq-set! *callers-db* callee
             (assoc-remove! (hashq-ref *callers-db* callee '()) mod-name)))

(define (add-callees callees mod-name)
  (hash-set! *module-callees-db* mod-name
             (append callees (hash-ref *module-callees-db* mod-name '()))))

(define (untaint-modules)
  (define (untaint m)
    (for-each (lambda (callee) (forget-callers callee m))
              (hash-ref *module-callees-db* m '()))
    (ensure-callers-db m))
  (ensure-callers-db #f)
  (for-each untaint *tainted-modules*)
  (set! *tainted-modules* '()))

(define (ensure-callers-db mod-name)
  (let ((mod (and mod-name (resolve-module mod-name)))
        (visited #f))
    (define (visit-variable var recurse mod-name)
      (if (variable-bound? var)
          (let ((x (variable-ref var)))
            (cond
             ((and visited (hashq-ref visited x)))
             ((procedure? x)
              (if visited (hashq-set! visited x #t))
              (let ((callees (filter variable-bound?
                                     (procedure-callee-rev-vars x))))
                (for-each (lambda (callee)
                            (add-caller callee x mod-name))
                          callees)
                (add-callees callees mod-name)))
             ((and recurse (module? x))
              (visit-module x #t))))))

    (define (visit-module mod recurse)
      (if visited (hashq-set! visited mod #t))
      (if (not (memq on-module-modified (module-observers mod)))
          (module-observe mod on-module-modified))
      (let ((name (module-name mod)))
        (module-for-each (lambda (sym var)
                           (visit-variable var recurse name))
                         mod)))

    (cond ((and (not mod-name) (not *callers-db*))
           (set! *callers-db* (make-hash-table 1000))
           (set! visited (make-hash-table 1000))
           (visit-module the-root-module #t))
          (mod-name (visit-module mod #f)))))

(define (procedure-callers var)
  "Returns an association list, keyed by module name, of known callers
of the given procedure. The latter can specified directly as a
variable, a symbol (which gets resolved in the current module) or a
pair of the form (module-name . variable-name), "
  (let ((v (cond ((variable? var) var)
                 ((symbol? var) (module-variable (current-module) var))
                 (else
                  (pmatch var
                    ((,modname . ,sym)
                     (module-variable (resolve-module modname) sym))
                    (else
                     (error "expected a variable, symbol, or (modname . sym)" var)))))))
    (untaint-modules)
    (hashq-ref *callers-db* v '())))
