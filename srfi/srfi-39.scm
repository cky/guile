;;; srfi-39.scm --- Parameter objects

;; 	Copyright (C) 2004 Free Software Foundation, Inc.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA
;;
;; As a special exception, the Free Software Foundation gives permission
;; for additional uses of the text contained in its release of GUILE.
;;
;; The exception is that, if you link the GUILE library with other files
;; to produce an executable, this does not by itself cause the
;; resulting executable to be covered by the GNU General Public License.
;; Your use of that executable is in no way restricted on account of
;; linking the GUILE library code into it.
;;
;; This exception does not however invalidate any other reasons why
;; the executable file might be covered by the GNU General Public License.
;;
;; This exception applies only to the code released by the
;; Free Software Foundation under the name GUILE.  If you copy
;; code from other Free Software Foundation releases into a copy of
;; GUILE, as the General Public License permits, the exception does
;; not apply to the code that you add in this way.  To avoid misleading
;; anyone as to the status of such modified files, you must delete
;; this exception notice from them.
;;
;; If you write modifications of your own for GUILE, it is your choice
;; whether to permit this exception to apply to your modifications.
;; If you do not wish that, delete this exception notice.

;;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;;; Date: 2004-05-05

;;; Commentary:

;; This is an implementation of SRFI-39 (Parameter objects).
;;
;; The implementation is based on Guile's fluid objects, and is, therefore,
;; thread-safe (parameters are thread-local).
;;
;; In addition to the forms defined in SRFI-39 (`make-parameter',
;; `parameterize'), a new procedure `with-parameters*' is provided.
;; This procedures is analogous to `with-fluids*' but taking as first
;; argument a list of parameter objects instead of a list of fluids.
;;

;;; Code:

(define-module (srfi srfi-39)
  #:use-module (ice-9 syncase)
  #:use-module (srfi srfi-16)

  #:export (make-parameter)
  #:export-syntax (parameterize)

  ;; helper procedure not in srfi-39.
  #:export (with-parameters*))

;; Make 'srfi-39 available as a feature identifiere to `cond-expand'.
;;
(cond-expand-provide (current-module) '(srfi-39))

(define make-parameter
  (case-lambda
    ((val) (make-parameter/helper val (lambda (x) x)))
    ((val conv) (make-parameter/helper val conv))))

(define get-fluid-tag (lambda () 'get-fluid)) ;; arbitrary unique (as per eq?) value
(define get-conv-tag (lambda () 'get-conv)) ;; arbitrary unique (as per eq?) value

(define (make-parameter/helper val conv)
  (let ((value (make-fluid))
        (conv conv))
    (begin
      (fluid-set! value (conv val))
      (lambda new-value
        (cond
         ((null? new-value) (fluid-ref value))
         ((eq? (car new-value) get-fluid-tag) value)
         ((eq? (car new-value) get-conv-tag) conv)
         ((null? (cdr new-value)) (fluid-set! value (conv (car new-value))))
         (else (error "make-parameter expects 0 or 1 arguments" new-value)))))))

(define-syntax parameterize
  (syntax-rules ()
    ((_ ((?param ?value) ...) ?body ...)
     (with-parameters* (list ?param ...)
                       (list ?value ...)
                       (lambda () ?body ...)))))

(define (with-parameters* params values thunk)
  (with-fluids* (map (lambda (p) (p get-fluid-tag)) params)
                (map (lambda (p v) ((p get-conv-tag) v)) params values)
                thunk))
