;;; ECMAScript for Guile

;; Copyright (C) 2009 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(define-module (language ecmascript compile-ghil)
  #:use-module (language ghil)
  #:use-module (system base pmatch)
  #:export (compile-ghil))

(define (compile-ghil exp env opts)
  (values
  (call-with-ghil-environment (make-ghil-toplevel-env) '()
    (lambda (env vars)
      (make-ghil-lambda env #f vars #f '() (comp exp env))))
  env))

(define (location x)
  (and (pair? x)
       (let ((props (source-properties x)))
	 (and (not (null? props))
              props))))

(define (comp x e)
  (let ((l (location x)))
    (pmatch x
      (null
       ;; FIXME, null doesn't have much relation to EOL...
       (make-ghil-quote e l '()))
      (true
       (make-ghil-quote e l #t))
      (false
       (make-ghil-quote e l #f))
      ((number ,num)
       (make-ghil-quote e l num))
      ((string ,str)
       (make-ghil-quote e l str))
      ((+ ,a ,b)
       (make-ghil-inline e l 'add (list (comp a e) (comp b e))))
      ((- ,a ,b)
       (make-ghil-inline e l 'sub (list (comp a e) (comp b e))))
      ((/ ,a ,b)
       (make-ghil-inline e l 'div (list (comp a e) (comp b e))))
      ((* ,a ,b)
       (make-ghil-inline e l 'mul (list (comp a e) (comp b e))))
      ((ref ,id)
       (make-ghil-ref e l (ghil-var-for-ref! e (string->symbol id))))
      ((define ,id ,val)
       (make-ghil-define e l (ghil-var-define! (ghil-env-parent e) (string->symbol id))
                         (comp val e)))
      ((begin . ,forms)
       (make-ghil-begin e l (map (lambda (x) (comp x e)) forms)))
      ((lambda ,formals ,body)
       (call-with-ghil-environment e formals
         (lambda (env vars)
           (make-ghil-lambda env l vars #f '() (comp body env)))))
      ((call ,proc ,args)
       (make-ghil-call e l (comp proc e) (map (lambda (x) (comp x e)) args)))
      (else
       (error "compilation not yet implemented:" x)))))




