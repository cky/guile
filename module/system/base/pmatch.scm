;;; pmatch

;;; Copyright (C) 2009, 2010 Free Software Foundation, Inc
;;; Copyright (C) 2005,2006,2007 Oleg Kiselyov
;;; Copyright (C) 2007 Daniel P. Friedman
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Originally written by Oleg Kiselyov for LeanTAP in Kanren, which is
;;; available under the MIT license.
;;;
;;; http://kanren.cvs.sourceforge.net/viewvc/kanren/kanren/mini/leanTAP.scm?view=log
;;;
;;; This version taken from:
;;; αKanren: A Fresh Name in Nominal Logic Programming
;;; by William E. Byrd and Daniel P. Friedman
;;; Proceedings of the 2007 Workshop on Scheme and Functional Programming
;;; Université Laval Technical Report DIUL-RT-0701

;;; Code:

(define-module (system base pmatch)
  #:export (pmatch))

(define-syntax pmatch
  (syntax-rules (else guard)
    ((_ (op arg ...) cs ...)
     (let ((v (op arg ...)))
       (pmatch v cs ...)))
    ((_ v) (if #f #f))
    ((_ v (else e0 e ...)) (let () e0 e ...))
    ((_ v (pat (guard g ...) e0 e ...) cs ...)
     (let ((fk (lambda () (pmatch v cs ...))))
       (ppat v pat
             (if (and g ...) (let () e0 e ...) (fk))
             (fk))))
    ((_ v (pat e0 e ...) cs ...)
     (let ((fk (lambda () (pmatch v cs ...))))
       (ppat v pat (let () e0 e ...) (fk))))))

(define-syntax ppat
  (syntax-rules (_ quote unquote)
    ((_ v _ kt kf) kt)
    ((_ v () kt kf) (if (null? v) kt kf))
    ((_ v (quote lit) kt kf)
     (if (equal? v (quote lit)) kt kf))
    ((_ v (unquote var) kt kf) (let ((var v)) kt))
    ((_ v (x . y) kt kf)
     (if (pair? v)
         (let ((vx (car v)) (vy (cdr v)))
           (ppat vx x (ppat vy y kt kf) kf))
         kf))
    ((_ v lit kt kf) (if (eq? v (quote lit)) kt kf))))
