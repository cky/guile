;;; Guile VM specific syntaxes and utilities

;; Copyright (C) 2001 Free Software Foundation, Inc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA

;;; Code:

(define-module (system base syntax)
  :use-module (srfi srfi-9)
  :export (%make-struct slot
           %slot-1 %slot-2 %slot-3 %slot-4 %slot-5
           %slot-6 %slot-7 %slot-8 %slot-9
           list-fold)
  :export-syntax (define-type define-record record-case)
  :re-export-syntax (define-record-type))
(export-syntax |) ;; emacs doesn't like the |


;;;
;;; Keywords by `:KEYWORD
;;;

(read-set! keywords 'prefix)


;;;
;;; Type
;;;

(define-macro (define-type name sig) sig)

;;;
;;; Record
;;;

(define (symbol-trim-both sym pred)
  (string->symbol (string-trim-both (symbol->string sym) pred)))


(define-macro (define-record def)
  (let* ((name (car def)) (slots (cdr def))
         (slot-names (map (lambda (slot) (if (pair? slot) (car slot) slot))
                          slots))
         (stem (symbol-trim-both name (list->char-set '(#\< #\>))))
         (type (make-record-type (symbol->string name) slot-names)))
    `(begin
       (define ,name ,type)
       (define ,(symbol-append 'make- stem)
         (let ((slots (list ,@(map (lambda (slot)
                                     (if (pair? slot)
                                         `(cons ',(car slot) ,(cadr slot))
                                         `',slot))
                                   slots))))
           (lambda args
             (apply ,(record-constructor type)
                    (,%compute-initargs args slots)))))
       (define ,(symbol-append name '?) ,(record-predicate type))
       ,@(map (lambda (sname)
                `(define ,(symbol-append stem '- sname)
                   ,(make-procedure-with-setter
                     (record-accessor type sname)
                     (record-modifier type sname))))
              slot-names))))

(define (%compute-initargs args slots)
  (define (finish out)
    (map (lambda (slot)
           (let ((name (if (pair? slot) (car slot) slot)))
             (cond ((assq name out) => cdr)
                   ((pair? slot) (cdr slot))
                   (else (error "unbound slot" args slots name)))))
         slots))
  (let lp ((in args) (positional slots) (out '()))
    (cond
     ((null? in)
      (finish out))
     ((keyword? (car in))
      (let ((sym (keyword->symbol (car in))))
        (cond
         ((and (not (memq sym slots))
               (not (assq sym (filter pair? slots))))
          (error "unknown slot" sym))
         ((assq sym out) (error "slot already set" sym out))
         (else (lp (cddr in) '() (acons sym (cadr in) out))))))
     ((null? positional)
      (error "too many initargs" args slots))
     (else
      (lp (cdr in) (cdr positional)
          (acons (car positional) (car in) out))))))


;;;
;;; Variants
;;;

(define-macro (| . rest)
  `(begin ,@(map (lambda (def) `(define-record ,def)) rest)))

(define (%slot-1 x) (vector-ref x 1))
(define (%slot-2 x) (vector-ref x 2))
(define (%slot-3 x) (vector-ref x 3))
(define (%slot-4 x) (vector-ref x 4))
(define (%slot-5 x) (vector-ref x 5))
(define (%slot-6 x) (vector-ref x 6))
(define (%slot-7 x) (vector-ref x 7))
(define (%slot-8 x) (vector-ref x 8))
(define (%slot-9 x) (vector-ref x 9))

(define-macro (record-case record . clauses)
  (let ((r (gensym)))
    (define (process-clause clause)
      (if (eq? (car clause) 'else)
          clause
          (let ((record-type (caar clause))
                (slots (cdar clause))
                (body (cdr clause)))
            `(((record-predicate ,record-type) ,r)
              (let ,(map (lambda (slot)
                           (if (pair? slot)
                               `(,(car slot) ((record-accessor ,record-type ',(cadr slot)) ,r))
                               `(,slot ((record-accessor ,record-type ',slot) ,r))))
                         slots)
                ,@body)))))
    `(let ((,r ,record))
       (cond ,@(let ((clauses (map process-clause clauses)))
                 (if (assq 'else clauses)
                     clauses
                     (append clauses `((else (error "unhandled record" ,r))))))))))


;;;
;;; Utilities
;;;

(define (list-fold f d l)
  (if (null? l)
      d
      (list-fold f (f (car l) d) (cdr l))))
