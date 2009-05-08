;;; Guile Scheme specification

;; Copyright (C) 2001 Free Software Foundation, Inc.

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

(define-module (language scheme compile-tree-il)
  #:use-module (language tree-il)
  #:export (compile-tree-il))

;;; environment := #f
;;;                | MODULE
;;;                | COMPILE-ENV
;;; compile-env := (MODULE LEXICALS . EXTERNALS)
(define (cenv-module env)
  (cond ((not env) #f)
        ((module? env) env)
        ((and (pair? env) (module? (car env))) (car env))
        (else (error "bad environment" env))))

(define (cenv-lexicals env)
  (cond ((not env) '())
        ((module? env) '())
        ((pair? env) (cadr env))
        (else (error "bad environment" env))))

(define (cenv-externals env)
  (cond ((not env) '())
        ((module? env) '())
        ((pair? env) (cddr env))
        (else (error "bad environment" env))))

(define (make-cenv module lexicals externals)
  (cons module (cons lexicals externals)))

(define (location x)
  (and (pair? x)
       (let ((props (source-properties x)))
	 (and (not (null? props))
              props))))

(define (compile-tree-il x e opts)
  (save-module-excursion
   (lambda ()
     (and=> (cenv-module e) set-current-module)
     (let ((x (sc-expand x 'c '(compile load eval)))
           (cenv (make-cenv (current-module)
                            (cenv-lexicals e) (cenv-externals e))))
       (values x cenv cenv)))))
