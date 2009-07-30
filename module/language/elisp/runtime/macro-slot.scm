;;; Guile Emac Lisp

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

(define-module (language elisp runtime macro-slot)
  #:use-module (language elisp runtime))

; This module contains the macro definitions of elisp symbols.  In contrast to
; the other runtime modules, those are used directly during compilation, of
; course, so not really in runtime.  But I think it fits well to the others
; here.


; The prog1 and prog2 constructs can easily be defined as macros using progn
; and some lexical-let's to save the intermediate value to return at the end.

(built-in-macro prog1
  (lambda (form1 . rest)
    (let ((temp (gensym)))
      `(without-void-checks (,temp)
         (lexical-let ((,temp ,form1))
           ,@rest
           ,temp)))))

(built-in-macro prog2
  (lambda (form1 form2 . rest)
    `(progn ,form1 (prog1 ,form2 ,@rest))))


; Define the conditionals when and unless as macros.

(built-in-macro when
  (lambda (condition . thens)
    `(if ,condition (progn ,@thens) nil)))

(built-in-macro unless
  (lambda (condition . elses)
    `(if ,condition nil (progn ,@elses))))


; Impement the cond form as nested if's.  A special case is a (condition)
; subform, in which case we need to return the condition itself if it is true
; and thus save it in a local variable before testing it.

(built-in-macro cond
  (lambda (. clauses)
    (let iterate ((tail clauses))
      (if (null? tail)
        'nil
        (let ((cur (car tail))
              (rest (iterate (cdr tail))))
          (prim cond
            ((prim or (not (list? cur)) (null? cur))
             (macro-error "invalid clause in cond" cur))
            ((null? (cdr cur))
             (let ((var (gensym)))
               `(without-void-checks (,var)
                  (lexical-let ((,var ,(car cur)))
                    (if ,var
                      ,var
                      ,rest)))))
            (else
              `(if ,(car cur)
                 (progn ,@(cdr cur))
                 ,rest))))))))


; The and and or forms can also be easily defined with macros.

(built-in-macro and
  (lambda (. args)
    (if (null? args)
      't
      (let iterate ((tail args))
        (if (null? (cdr tail))
          (car tail)
          `(if ,(car tail)
             ,(iterate (cdr tail))
             nil))))))

(built-in-macro or
  (lambda (. args)
    (let iterate ((tail args))
      (if (null? tail)
        'nil
        (let ((var (gensym)))
          `(without-void-checks (,var)
             (lexical-let ((,var ,(car tail)))
               (if ,var
                 ,var
                 ,(iterate (cdr tail))))))))))


; Define the dotimes and dolist iteration macros.

(built-in-macro dotimes
  (lambda (args . body)
    (if (prim or (not (list? args))
                 (< (length args) 2)
                 (> (length args) 3))
      (macro-error "invalid dotimes arguments" args)
      (let ((var (car args))
            (count (cadr args)))
        (if (not (symbol? var))
          (macro-error "expected symbol as dotimes variable"))
        `(let ((,var 0))
           (while (< ,var ,count)
             ,@body
             (setq ,var (1+ ,var)))
           ,@(if (= (length args) 3)
               (list (caddr args))
               '()))))))

(built-in-macro dolist
  (lambda (args . body)
    (if (prim or (not (list? args))
                 (< (length args) 2)
                 (> (length args) 3))
      (macro-error "invalid dolist arguments" args)
      (let ((var (car args))
            (iter-list (cadr args))
            (tailvar (gensym)))
        (if (not (symbol? var))
          (macro-error "expected symbol as dolist variable")
          `(let (,var)
             (without-void-checks (,tailvar)
               (lexical-let ((,tailvar ,iter-list))
                 (while (not (null ,tailvar))
                   (setq ,var (car ,tailvar))
                   ,@body
                   (setq ,tailvar (cdr ,tailvar)))
                 ,@(if (= (length args) 3)
                     (list (caddr args))
                     '())))))))))


; Pop off the first element from a list or push one to it.

(built-in-macro pop
  (lambda (list-name)
    `(prog1 (car ,list-name)
            (setq ,list-name (cdr ,list-name)))))

(built-in-macro push
  (lambda (new-el list-name)
    `(setq ,list-name (cons ,new-el ,list-name))))
