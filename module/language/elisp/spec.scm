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

(define-module (lang elisp spec)
  :use-module (system lang language)
  :export (elisp))


;;;
;;; Translator
;;;

(define (translate x)
  (if (pair? x)
      (translate-pair x)
      x))

(define (translate-pair x)
  (let ((name (car x)) (args (cdr x)))
    (case name
      ((quote) `(@quote ,@args))
      ((defvar) `(@define ,@(map translate args)))
      ((setq) `(@set! ,@(map translate args)))
      ((if) `(@if ,(translate (car args))
		  (@begin ,@(map translate (cdr args)))))
      ((and) `(@and ,@(map translate args)))
      ((or) `(@or ,@(map translate args)))
      ((progn) `(@begin ,@(map translate args)))
      ((defun) `(@define ,(car args)
			 (@lambda ,(cadr args) ,@(map translate (cddr args)))))
      ((lambda) `(@lambda ,(car args) ,@(map translate (cdr args))))
      (else x))))


;;;
;;; Language definition
;;;

(define-language elisp
  #:title	"Emacs Lisp"
  #:version	"0.0"
  #:reader	read
  #:expander	id
  #:translator	translate
  )
