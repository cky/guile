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


(define-module (ice-9 annotate)
  :export (<annotation> annotation? annotate deannotate make-annotation
           annotation-expression annotation-source annotation-stripped
           set-annotation-stripped!
           deannotate/source-properties))

(define <annotation>          
  (make-vtable "prprpw"
               (lambda (struct port)
                 (display "#<annotated " port)
                 (display (struct-ref struct 0) port)
                 (display ">" port))))

(define (annotation? x)
  (and (struct? x) (eq? (struct-vtable x) <annotation>)))

(define (make-annotation e s . stripped?)
  (if (null? stripped?)
      (make-struct <annotation> 0 e s #f)
      (apply make-struct <annotation> 0 e s stripped?)))

(define (annotation-expression a)
  (struct-ref a 0))
(define (annotation-source a)
  (struct-ref a 1))
(define (annotation-stripped a)
  (struct-ref a 2))
(define (set-annotation-stripped! a stripped?)
  (struct-set! a 2 stripped?))

(define (annotate e)
  (let ((p (if (pair? e) (source-properties e) #f))
        (out (cond ((and (list? e) (not (null? e)))
                    (map annotate e))
                   ((pair? e)
                    (cons (annotate (car e)) (annotate (cdr e))))
                   (else e))))
    (if (pair? p)
        (make-annotation out p #f)
        out)))
                          
(define (deannotate e)
  (cond ((list? e)
         (map deannotate e))
        ((pair? e)
         (cons (deannotate (car e)) (deannotate (cdr e))))
        ((annotation? e) (deannotate (annotation-expression e)))
        (else e)))

(define (deannotate/source-properties e)
  (cond ((list? e)
         (map deannotate/source-properties e))
        ((pair? e)
         (cons (deannotate/source-properties (car e))
               (deannotate/source-properties (cdr e))))
        ((annotation? e)
         (let ((e (deannotate/source-properties (annotation-expression e)))
               (source (annotation-source e)))
           (if (pair? e)
               (set-source-properties! e source))
           e))
        (else e)))
