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


; Define the conditionals when and unless as macros.

(built-in-macro when
  (lambda (condition . thens)
    `(if ,condition (progn ,@thens) nil)))

(built-in-macro unless
  (lambda (condition . elses)
    `(if ,condition nil (progn ,@elses))))


; Define the dotimes and dolist iteration macros.
; As the variable has to be bound locally for elisp, this needs to go through
; the dynamic scoping fluid system.  So we can't speed these forms up by
; implementing them directly in the compiler with just a lexical variable
; anyways.

(built-in-macro dotimes
  (lambda (args . body)
    (if (or (not (list? args))
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
