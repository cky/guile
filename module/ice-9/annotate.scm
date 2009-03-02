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
           set-annotation-stripped!))

(define <annotation>          
  (make-vtable "prprpw"
               (lambda (struct port)
                 (display "#<annotation of " port)
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
(define (set-annotation-stripped! a)
  (struct-set! a 2 #t))

(define (annotate e)
  (cond ((and (list? e) (not (null? e)))
         (make-annotation (map annotate e) (source-properties e) #f))
        ((pair? e)
         (make-annotation (cons (annotate (car e)) (annotate (cdr e)))
                          (source-properties e) #f))
        (else e)))
                          
(define (deannotate e)
  (cond ((list? e)
         (map deannotate e))
        ((pair? e)
         (cons (deannotate (car e)) (deannotate (cdr e))))
        ((annotation? e) (deannotate (annotation-expression e)))
        (else e)))
