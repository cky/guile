;;; srfi-9.scm --- define-record-type

;; 	Copyright (C) 2001, 2002, 2006, 2009, 2010, 2011, 2012 Free Software Foundation, Inc.
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Commentary:

;; This module exports the syntactic form `define-record-type', which
;; is the means for creating record types defined in SRFI-9.
;;
;; The syntax of a record type definition is:
;;
;;  <record type definition>
;;    -> (define-record-type <type name>
;;         (<constructor name> <field tag> ...)
;;         <predicate name>
;;         <field spec> ...)
;;
;;  <field spec> -> (<field tag> <accessor name>)
;;               -> (<field tag> <accessor name> <modifier name>)
;;
;;  <field tag> -> <identifier>
;;  <... name>  -> <identifier>
;;
;; Usage example:
;;
;; guile> (use-modules (srfi srfi-9))
;; guile> (define-record-type :foo (make-foo x) foo?
;;                            (x get-x) (y get-y set-y!))
;; guile> (define f (make-foo 1))
;; guile> f
;; #<:foo x: 1 y: #f>
;; guile> (get-x f)
;; 1
;; guile> (set-y! f 2)
;; 2
;; guile> (get-y f)
;; 2
;; guile> f
;; #<:foo x: 1 y: 2>
;; guile> (foo? f)
;; #t
;; guile> (foo? 1)
;; #f

;;; Code:

(define-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:export (define-record-type))

(cond-expand-provide (current-module) '(srfi-9))

;; Roll our own instead of using the public `define-inlinable'.  This is
;; because the public one has a different `make-procedure-name', so
;; using it would require users to recompile code that uses SRFI-9.  See
;; <http://lists.gnu.org/archive/html/guile-devel/2011-04/msg00111.html>.

(define-syntax define-inlinable
  (lambda (x)
    (define (make-procedure-name name)
      (datum->syntax name
                     (symbol-append '% (syntax->datum name)
                                    '-procedure)))

    (syntax-case x ()
      ((_ (name formals ...) body ...)
       (identifier? #'name)
       (with-syntax ((proc-name  (make-procedure-name #'name))
                     ((args ...) (generate-temporaries #'(formals ...))))
         #`(begin
             (define (proc-name formals ...)
               body ...)
             (define-syntax name
               (lambda (x)
                 (syntax-case x ()
                   ((_ args ...)
                    #'((lambda (formals ...)
                         body ...)
                       args ...))
                   (_
                    (identifier? x)
                    #'proc-name))))))))))

(define (default-record-printer s p)
  (display "#<" p)
  (display (record-type-name (record-type-descriptor s)) p)
  (let loop ((fields (record-type-fields (record-type-descriptor s)))
             (off 0))
    (cond
     ((not (null? fields))
      (display " " p)
      (display (car fields) p)
      (display ": " p)
      (write (struct-ref s off) p)
      (loop (cdr fields) (+ 1 off)))))
  (display ">" p))

(define-syntax define-record-type
  (lambda (x)
    (define (field-identifiers field-specs)
      (syntax-case field-specs ()
        (()
         '())
        ((field-spec)
         (syntax-case #'field-spec ()
           ((name accessor) #'(name))
           ((name accessor modifier) #'(name))))
        ((field-spec rest ...)
         (append (field-identifiers #'(field-spec))
                 (field-identifiers #'(rest ...))))))

    (define (field-indices fields)
      (fold (lambda (field result)
              (let ((i (if (null? result)
                           0
                           (+ 1 (cdar result)))))
                (alist-cons field i result)))
            '()
            fields))

    (define (constructor type-name constructor-spec indices)
      (syntax-case constructor-spec ()
        ((ctor field ...)
         (let ((field-count (length indices))
               (ctor-args   (map (lambda (field)
                                   (cons (syntax->datum field) field))
                                 #'(field ...))))
           #`(define-inlinable #,constructor-spec
               (make-struct #,type-name 0
                            #,@(unfold
                                (lambda (field-num)
                                  (>= field-num field-count))
                                (lambda (field-num)
                                  (let* ((name
                                          (car (find (lambda (f+i)
                                                       (= (cdr f+i) field-num))
                                                     indices)))
                                         (arg (assq name ctor-args)))
                                    (if (pair? arg)
                                        (cdr arg)
                                        #'#f)))
                                1+
                                0)))))))

    (define (accessors type-name field-specs indices)
      (syntax-case field-specs ()
        (()
         #'())
        ((field-spec)
         (syntax-case #'field-spec ()
           ((name accessor)
            (with-syntax ((index (assoc-ref indices (syntax->datum #'name))))
              #`((define-inlinable (accessor s)
                   (if (eq? (struct-vtable s) #,type-name)
                       (struct-ref s index)
                       (throw 'wrong-type-arg 'accessor
                              "Wrong type argument: ~S" (list s)
                              (list s)))))))
           ((name accessor modifier)
            (with-syntax ((index (assoc-ref indices (syntax->datum #'name))))
              #`(#,@(accessors type-name #'((name accessor)) indices)
                 (define-inlinable (modifier s val)
                   (if (eq? (struct-vtable s) #,type-name)
                       (struct-set! s index val)
                       (throw 'wrong-type-arg 'modifier
                              "Wrong type argument: ~S" (list s)
                              (list s)))))))))
        ((field-spec rest ...)
         #`(#,@(accessors type-name #'(field-spec) indices)
            #,@(accessors type-name #'(rest ...) indices)))))

    (syntax-case x ()
      ((_ type-name constructor-spec predicate-name field-spec ...)
       (let* ((fields      (field-identifiers #'(field-spec ...)))
              (field-count (length fields))
              (layout      (string-concatenate (make-list field-count "pw")))
              (indices     (field-indices (map syntax->datum fields)))
              (ctor-name   (syntax-case #'constructor-spec ()
                             ((ctor args ...) #'ctor))))
         #`(begin
             #,(constructor #'type-name #'constructor-spec indices)

             (define type-name
               (let ((rtd (make-struct/no-tail
                           record-type-vtable
                           '#,(datum->syntax #'here (make-struct-layout layout))
                           default-record-printer
                           'type-name
                           '#,fields)))
                 (set-struct-vtable-name! rtd 'type-name)
                 (struct-set! rtd (+ 2 vtable-offset-user) #,ctor-name)
                 rtd))

             (define-inlinable (predicate-name obj)
               (and (struct? obj)
                    (eq? (struct-vtable obj) type-name)))

             #,@(accessors #'type-name #'(field-spec ...) indices)))))))

;;; srfi-9.scm ends here
