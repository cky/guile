;;; Guile Scheme specification

;; Copyright (C) 2001, 2009 Free Software Foundation, Inc.

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Code:

(define-module (language scheme spec)
  #:use-module (system base compile)
  #:use-module (system base language)
  #:use-module (language scheme compile-tree-il)
  #:use-module (language scheme decompile-tree-il)
  #:export (scheme))

;;;
;;; Reader
;;;

(read-enable 'positions)

;;;
;;; Language definition
;;;

(define-language scheme
  #:title	"Guile Scheme"
  #:version	"0.5"
  #:reader      (lambda args
                  ;; Read using the compilation environment's current reader.
                  ;; Don't use the current module's `current-reader' because
                  ;; it might be set, e.g., to the REPL's reader, so we'd
                  ;; enter an infinite recursion.
                  ;; FIXME: Handle `read-options' as well.
                  (let* ((mod  (current-compilation-environment))
                         (cr   (and (module? mod)
                                    (module-ref mod 'current-reader)))
                         (read (if (and cr (fluid-ref cr))
                                   (fluid-ref cr)
                                   read)))
                    (apply read args)))

  #:compilers   `((tree-il . ,compile-tree-il))
  #:decompilers `((tree-il . ,decompile-tree-il))
  #:evaluator	(lambda (x module) (primitive-eval x))
  #:printer	write
  )
