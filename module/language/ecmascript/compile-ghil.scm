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
  #:use-module (ice-9 receive)
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

(define-macro (@impl e l sym . args)
  `(make-ghil-call ,e ,l
                   (ghil-var-at-module! ,e '(language ecmascript impl) ',sym #t)
                   (list ,@(map (lambda (x) `(comp x ,e)) ,args))))

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
       (make-ghil-ref e l (ghil-var-for-ref! e id)))
      ((var ,id ,val)
       (make-ghil-define e l (ghil-var-define! (ghil-env-parent e) id)
                         (comp val e)))
      ((begin . ,forms)
       (make-ghil-begin e l (map (lambda (x) (comp x e)) forms)))
      ((lambda ,formals ,body)
       (call-with-ghil-environment e '(%args)
         (lambda (env vars)
           (make-ghil-lambda env l vars #t '()
                             (comp-body env l body formals '%args)))))
      ((call ,proc ,args)
       (make-ghil-call e l (comp proc e) (map (lambda (x) (comp x e)) args)))
      ((return ,expr)
       (make-ghil-inline e l 'return (list (comp expr e))))
      (else
       (error "compilation not yet implemented:" x)))))

(define (comp-body env loc body formals %args)
  (define (process)
    (let lp ((in body) (out '()) (rvars (reverse formals)))
      (pmatch in
        (((var ,x) . ,rest)
         (lp rest
             out
             (if (memq x rvars) rvars (cons x rvars))))
        (((var ,x ,y) . ,rest)
         (lp rest
             `((= (ref ,x) ,y) . ,out)
             (if (memq x rvars) rvars (cons x rvars))))
        ((,x . ,rest) (guard (and (pair? x) (eq? (car x) 'lambda)))
         (lp rest
             (cons x out)
             rvars))
        ((,x . ,rest) (guard (pair? x))
         (receive (sub-out rvars)
             (lp x '() rvars)
           (lp rest
               (cons sub-out out)
               rvars)))
        ((,x . ,rest)
         (lp rest
             (cons x out)
             rvars))
        (()
         (values (reverse! out)
                 rvars)))))
  (receive (out rvars)
      (process)
    (call-with-ghil-bindings env (reverse rvars)
      (lambda (vars)
        (let ((%argv (assq-ref (ghil-env-table env) %args)))
          (make-ghil-begin
           env loc
           `(,@(map (lambda (f)
                      (make-ghil-if
                       env loc
                       (make-ghil-inline
                        env loc 'null?
                        (list (make-ghil-ref env loc %argv)))
                       (make-ghil-begin env loc '())
                       (make-ghil-begin
                        env loc
                        (list (make-ghil-set
                               env loc
                               (ghil-var-for-ref! env f)
                               (make-ghil-inline
                                env loc 'car
                                (list (make-ghil-ref env loc %argv))))
                              (make-ghil-set
                               env loc %argv
                               (make-ghil-inline
                                env loc 'cdr
                                (list (make-ghil-ref env loc %argv))))))))
                    formals)
             ;; fixme: here check for too many args
             ,(comp out env))))))))
