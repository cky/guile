;;; Repl common routines

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

(define-module (system repl common)
  :use-module (oop goops)
  :use-syntax (system base syntax)
  :use-module (system base language)
  :use-module (system vm core)
  :use-module (system vm trace)
  :export (make-repl repl-welcome repl-prompt repl-read repl-compile
		     repl-eval repl-print repl-compile-file repl-load-file))


;;;
;;; Repl
;;;

(define-vm-class <repl> ()
  vm language module value-count value-history tm-stats vm-stats gc-stats)

(define (make-repl lang)
  (let ((vm (make-vm)))
    (make <repl>
	  :vm vm
	  :language (lookup-language lang)
	  :module #f ;; (global-ref 'user)
	  :value-count 0
;	  :value-history (make-vmodule)
	  :tm-stats (times)
	  :vm-stats (vm-stats vm)
	  :gc-stats (gc-stats))))

(define (repl-welcome repl)
  (format #t "~A interpreter ~A on Guile ~A\n"
	  repl.language.title repl.language.version (version))
  (display "Copyright (C) 2001 Free Software Foundation, Inc.\n\n")
  (display "Enter `,help' for help.\n"))

(define (repl-prompt repl)
  (format #t "~A@~A> " repl.language.name 'guile)
  ;; (env-identifier repl.module))
  (force-output))

(define (repl-read repl . args)
  (apply read-in repl.language args))

(define (repl-compile repl form . opts)
  (apply compile-in form repl.module repl.language opts))

(define (repl-eval repl form)
  (let ((evaler repl.language.evaler))
    (if evaler
	(evaler form repl.module)
	(vm-load repl.vm (repl-compile repl form)))))

(define (repl-print repl val)
  (if (not (eq? val *unspecified*))
      (let* ((num (1+ repl.value-count))
	     (sym (string->symbol (format #f "$~A" num))))
;	(vmodule-define repl.value-history sym val)
	(format #t "~A = " sym)
	(print-in val repl.language)
	(newline)
	(set! repl.value-count num))))

(define (repl-compile-file repl form . opts)
  (apply compile-file-in form repl.module repl.language opts))

(define (repl-load-file repl file . opts)
  (let ((bytes (apply load-file-in file repl.module repl.language opts)))
    (if (memq :t opts)
	(vm-trace repl.vm bytes :a)
	(vm-load repl.vm bytes))))
