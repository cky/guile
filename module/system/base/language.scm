;;; Multi-language support

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

(define-module (system base language)
  :use-module (oop goops)
  :use-syntax (system base syntax)
  :use-module (system il compile)
  :use-module (system vm core)
  :use-module (system vm assemble)
  :use-module (ice-9 regex)
  :export (define-language lookup-language read-in compile-in print-in
	    compile-file-in))


;;;
;;; Language class
;;;

(define-vm-class <language> ()
  name title version environment
  (reader)
  (expander (lambda (x e) x))
  (translator (lambda (x e) x))
  (evaluator #f)
  (printer)
  (compiler)
  )

(define-method (write (lang <language>) port)
  (display "#<language " port)
  (display lang.name port)
  (display ">"))

(define-macro (define-language name . spec)
  `(define ,name (,make ,<language> :name ',name ,@spec)))

(define (lookup-language name)
  (let ((m (resolve-module `(language ,name spec))))
    (if (module-bound? m name)
	(module-ref m name)
	(error "No such language:" name))))


;;;
;;; Evaluation interface
;;;

(define (read-in lang . port)
  (lang.reader (if (null? port) (current-input-port) (car port))))

(define (compile-in x e lang . opts)
  (catch 'result
    (lambda ()
      ;; expand
      (set! x (lang.expander x e))
      (if (memq :e opts) (throw 'result x))
      ;; translate
      (set! x (lang.translator x e))
      (if (memq :t opts) (throw 'result x))
      ;; compile
      (set! x (apply compile x e opts))
      (if (memq :c opts) (throw 'result x))
      ;; assemble
      (apply assemble x e opts))
    (lambda (key val) val)))

(define (print-in val lang . port)
  (lang.printer val (if (null? port) (current-output-port) (car port))))

(define (compile-file-in file lang . opts)
  (call-with-input-file file
    (lambda (port) (apply lang.compiler port (current-module) opts))))

(define-public (syntax-error loc msg exp)
  (throw 'syntax-error loc msg exp))

(define-public (call-with-compile-error-catch thunk)
  (catch 'syntax-error
    thunk
    (lambda (key loc msg exp)
      (format #t "~A:~A: ~A: ~A" (car loc) (cdr loc) msg exp))))
