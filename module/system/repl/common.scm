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
  :use-module (system base compile)
  :use-module (system base language)
  :use-module (system vm core)
  :use-module (system vm trace))


;;;
;;; Repl type
;;;

(define-vm-class <repl> () env tm-stats vm-stats gc-stats)

(define-public (make-repl lang)
  (let ((cenv (make-cenv :vm (the-vm)
			 :language (lookup-language lang)
			 :module (current-module))))
    (make <repl>
	  :env cenv
	  :tm-stats (times)
	  :vm-stats (vm-stats cenv.vm)
	  :gc-stats (gc-stats))))

(define-public (repl-welcome repl)
  (format #t "~A interpreter ~A on Guile ~A\n"
	  repl.env.language.title repl.env.language.version (version))
  (display "Copyright (C) 2001 Free Software Foundation, Inc.\n\n")
  (display "Enter `,help' for help.\n"))

(define-public (repl-prompt repl)
  (let ((module-name (car (last-pair (module-name repl.env.module)))))
    (format #t "~A@~A> " repl.env.language.name module-name)
    (force-output)))

(define-public (repl-read repl)
  (repl.env.language.reader))

(define-public (repl-compile repl form . opts)
  (apply compile-in form repl.env.module repl.env.language opts))

(define-public (repl-eval repl form)
  (let ((eval repl.env.language.evaluator))
    (if eval
	(eval form repl.env.module)
	(vm-load repl.env.vm (repl-compile repl form)))))

(define-public (repl-print repl val)
  (if (not (eq? val *unspecified*))
      (begin
	(repl.env.language.printer val)
	(newline))))


;;;
;;; Utilities
;;;

(define-public (puts x) (display x) (newline))

(define-public (->string x)
  (object->string x display))

(define-public (user-error msg . args)
  (throw 'user-error #f msg args #f))
