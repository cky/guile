;;; exceptions.scm --- The R6RS exceptions library

;;      Copyright (C) 2010 Free Software Foundation, Inc.
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
          (rnrs conditions (6))
	  (rnrs records procedural (6))
	  (only (guile) with-throw-handler *unspecified* @@))

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
)
