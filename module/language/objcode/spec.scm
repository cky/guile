;;; Guile Lowlevel Intermediate Language

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

(define-module (language objcode spec)
  #:use-module (system base language)
  #:use-module (system vm objcode)
  #:use-module (system vm program)
  #:export (objcode make-objcode-env))

(define (make-objcode-env module externals)
  (cons module externals))

(define (objcode-env-module env)
  (if env (car env) (current-module)))

(define (objcode-env-externals env)
  (if env (cdr env) '()))

(define (objcode->value x e opts)
  (let ((thunk (make-program x #f (objcode-env-externals e))))
    (if e
        (save-module-excursion
         (lambda ()
           (set-current-module (objcode-env-module e))
           (values (thunk) #f)))
        (values (thunk) #f))))

(define (decompile-value x env opts)
  (cond
   ((program? x)
    (let ((objs  (program-objects x))
          (meta  (program-meta x))
          (exts  (program-external x))
          (binds (program-bindings x))
          (srcs  (program-sources x))
          (nargs (arity:nargs (program-arity x))))
      (let ((blocs (and binds
                        (append (list-head binds nargs)
                                (filter (lambda (x) (not (binding:extp x)))
                                        (list-tail binds nargs)))))
            (bexts (and binds
                        (filter binding:extp binds))))
        (values (program-objcode x)
                `((objects . ,objs)
                  (meta    . ,(and meta (meta)))
                  (exts    . ,exts)
                  (blocs   . ,blocs)
                  (bexts   . ,bexts)
                  (sources . ,srcs))))))
   ((objcode? x)
    (values x #f))
   (else
    (error "can't decompile ~A: not a program or objcode" x))))

(define-language objcode
  #:title	"Guile Object Code"
  #:version	"0.3"
  #:reader	#f
  #:printer	write-objcode
  #:compilers   `((value . ,objcode->value))
  #:decompilers `((value . ,decompile-value))
  )
