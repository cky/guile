;;; srfi-31.scm --- special form for recursive evaluation

;; 	Copyright (C) 2004 Free Software Foundation, Inc.
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 2.1 of the License, or (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;;; Original author: Rob Browning <rlb@defaultvalue.org>

(define-module (srfi srfi-31)
  :export-syntax (rec))

(define-macro (rec arg-form . body)
  (cond
   ((and (symbol? arg-form) (= 1 (length body)))
    ;; (rec S (cons 1 (delay S)))
    `(letrec ((,arg-form ,(car body)))
       ,arg-form))
   ;; (rec (f x) (+ x 1))
   ((list? arg-form)
    `(letrec ((,(car arg-form) (lambda ,(cdr arg-form) ,@body)))
       ,(car arg-form)))
   (else "syntax error in rec form" `(rec ,arg-form ,@body))))
