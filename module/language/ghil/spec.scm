;;; Guile High Intermediate Language

;; Copyright (C) 2001 Free Software Foundation, Inc.

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

(define-module (language ghil spec)
  #:use-module (system base language)
  #:use-module (language glil)
  #:use-module (language ghil)
  #:use-module (language ghil compile-glil)
  #:export (ghil))

(define (write-ghil exp . port)
  (apply write (unparse-ghil exp) port))

(define (parse x)
  (call-with-ghil-environment (make-ghil-toplevel-env (current-module)) '()
    (lambda (env vars)
      (make-ghil-lambda env #f vars #f '() (parse-ghil env x)))))

(define (join exps env)
  (if (or-map (lambda (x)
                (or (not (ghil-lambda? x))
                    (ghil-lambda-rest x)
                    (memq 'argument
                          (map ghil-var-kind
                               (ghil-env-variables (ghil-lambda-env x))))))
              exps)
      (error "GHIL expressions to join must be thunks"))

  (let ((env (make-ghil-env env '()
                            (apply append
                                   (map ghil-env-variables
                                        (map ghil-lambda-env exps))))))
    (make-ghil-lambda env #f '() #f '()
                      (make-ghil-begin env #f
                                       (map ghil-lambda-body exps)))))

(define-language ghil
  #:title	"Guile High Intermediate Language (GHIL)"
  #:version	"0.3"
  #:reader	read
  #:printer	write-ghil
  #:parser      parse
  #:joiner      join
  #:compilers   `((glil . ,compile-glil))
  )
