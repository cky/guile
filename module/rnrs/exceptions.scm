;;; exceptions.scm --- The R6RS exceptions library

;;      Copyright (C) 2010, 2011 Free Software Foundation, Inc.
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


(library (rnrs exceptions (6))
  (export guard with-exception-handler raise raise-continuable)
  (import (rnrs base (6))
          (rnrs control (6))
          (rnrs conditions (6))
	  (rnrs records procedural (6))
	  (rnrs records inspection (6))
	  (only (guile)
                format
                newline
                display
                filter
                set-exception-printer!
                with-throw-handler
                *unspecified*
                @@))

  (define raise (@@ (rnrs records procedural) r6rs-raise))
  (define raise-continuable 
    (@@ (rnrs records procedural) r6rs-raise-continuable))
  (define raise-object-wrapper? 
    (@@ (rnrs records procedural) raise-object-wrapper?))
  (define raise-object-wrapper-obj
    (@@ (rnrs records procedural) raise-object-wrapper-obj))
  (define raise-object-wrapper-continuation
    (@@ (rnrs records procedural) raise-object-wrapper-continuation))

  (define (with-exception-handler handler thunk)
    (with-throw-handler 'r6rs:exception
     thunk
     (lambda (key . args)
       (if (and (not (null? args))
		(raise-object-wrapper? (car args)))
	   (let* ((cargs (car args))
		  (obj (raise-object-wrapper-obj cargs))
		  (continuation (raise-object-wrapper-continuation cargs))
		  (handler-return (handler obj)))
	     (if continuation
		 (continuation handler-return)
		 (raise (make-non-continuable-violation))))
	   *unspecified*))))

  (define-syntax guard0
    (syntax-rules ()
      ((_ (variable cond-clause ...) . body)
       (call/cc (lambda (continuation)
		  (with-exception-handler
		   (lambda (variable)
		     (continuation (cond cond-clause ...)))
		   (lambda () . body)))))))

  (define-syntax guard
    (syntax-rules (else)
      ((_ (variable cond-clause ... . ((else else-clause ...))) . body)
       (guard0 (variable cond-clause ... (else else-clause ...)) . body))
      ((_ (variable cond-clause ...) . body)
       (guard0 (variable cond-clause ... (else (raise variable))) . body))))

  ;;; Exception printing

  (define (exception-printer port key args punt)
    (cond ((and (= 1 (length args))
                (raise-object-wrapper? (car args)))
           (let ((obj (raise-object-wrapper-obj (car args))))
             (cond ((condition? obj)
                    (display "ERROR: R6RS exception:\n" port)
                    (format-condition port obj))
                   (else
                    (format port "ERROR: R6RS exception: `~s'" obj)))))
          (else
           (punt))))

  (define (format-condition port condition)
    (let ((components (simple-conditions condition)))
      (if (null? components)
          (format port "Empty condition object")
          (let loop ((i 1) (components components))
            (cond ((pair? components)
                   (format port "  ~a. " i)
                   (format-simple-condition port (car components))
                   (when (pair? (cdr components))
                     (newline port))
                   (loop (+ i 1) (cdr components))))))))

  (define (format-simple-condition port condition)
    (define (print-rtd-fields rtd field-names)
      (let ((n-fields (vector-length field-names)))
        (do ((i 0 (+ i 1)))
            ((>= i n-fields))
          (format port "      ~a: ~s"
                  (vector-ref field-names i)
                  ((record-accessor rtd i) condition))
          (unless (= i (- n-fields 1))
            (newline port)))))
    (let ((condition-name (record-type-name (record-rtd condition))))
      (let loop ((rtd (record-rtd condition))
                 (rtd.fields-list '())
                 (n-fields 0))
        (cond (rtd
               (let ((field-names (record-type-field-names rtd)))
                 (loop (record-type-parent rtd)
                       (cons (cons rtd field-names) rtd.fields-list)
                       (+ n-fields (vector-length field-names)))))
              (else
               (let ((rtd.fields-list
                      (filter (lambda (rtd.fields)
                                (not (zero? (vector-length (cdr rtd.fields)))))
                              (reverse rtd.fields-list))))
                 (case n-fields
                   ((0) (format port "~a" condition-name))
                   ((1) (format port "~a: ~s"
                                condition-name
                                ((record-accessor (caar rtd.fields-list) 0)
                                 condition)))
                   (else
                    (format port "~a:\n" condition-name)
                    (let loop ((lst rtd.fields-list))
                      (when (pair? lst)
                        (let ((rtd.fields (car lst)))
                          (print-rtd-fields (car rtd.fields) (cdr rtd.fields))
                          (when (pair? (cdr lst))
                            (newline port))
                          (loop (cdr lst)))))))))))))

  (set-exception-printer! 'r6rs:exception exception-printer)

)
