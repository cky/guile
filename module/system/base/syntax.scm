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
  #:export (%compute-initargs)
  #:export-syntax (define-type define-record record-case))


;;;
;;; Type
;;;

(define-macro (define-type name . rest)
  `(begin ,@(map (lambda (def) `(define-record ,def)) rest)))


;;;
;;; Record
;;;

(define (symbol-trim-both sym pred)
  (string->symbol (string-trim-both (symbol->string sym) pred)))

(define-macro (define-record def)
  (let* ((name (car def)) (slots (cdr def))
         (slot-names (map (lambda (slot) (if (pair? slot) (car slot) slot))
                          slots))
         (stem (symbol-trim-both name (list->char-set '(#\< #\>)))))
    `(begin
       (define ,name (make-record-type ,(symbol->string name) ',slot-names))
       (define ,(symbol-append 'make- stem)
         (let ((slots (list ,@(map (lambda (slot)
                                     (if (pair? slot)
                                         `(cons ',(car slot) ,(cadr slot))
                                         `',slot))
                                   slots)))
               (constructor (record-constructor ,name)))
           (lambda args
             (apply constructor (%compute-initargs args slots)))))
       (define ,(symbol-append stem '?) (record-predicate ,name))
       ,@(map (lambda (sname)
                `(define ,(symbol-append stem '- sname)
                   (make-procedure-with-setter
                    (record-accessor ,name ',sname)
                    (record-modifier ,name ',sname))))
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
          (let ((slot (car positional)))
            (acons (if (pair? slot) (car slot) slot)
                   (car in)
                   out)))))))

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
