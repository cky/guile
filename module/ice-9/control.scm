;;; Beyond call/cc

;; Copyright (C) 2010 Free Software Foundation, Inc.

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

(define-module (ice-9 control)
  #:use-module (language tree-il primitives)
  #:export (% prompt control))

(eval-when (eval load compile)
  (load-extension "libguile" "scm_init_control")
  (add-interesting-primitive! '@prompt)
  (add-interesting-primitive! '@control)

  (define (prompt tag thunk handler)
    (@prompt tag thunk handler #f))

  (define (control tag . args)
    (apply @control tag 'throw args))

  (define-syntax %
    (syntax-rules ()
      ((_ expr handler)
       (prompt (lambda () expr) handler))))

  (add-interesting-primitive! 'prompt)
  (add-interesting-primitive! 'control))
