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
  :use-syntax (system base syntax)
  :use-module (system base compile)
  :use-module (system base language)
  :use-module (system vm core)
  :export (<repl> make-repl repl-env repl-options repl-tm-stats
                  repl-gc-stats repl-vm-stats
           repl-welcome repl-prompt repl-read repl-compile repl-eval
           repl-print repl-option-ref repl-option-set!
           puts ->string user-error))


;;;
;;; Repl type
;;;

(define-record (<repl> env options tm-stats gc-stats vm-stats))

(define repl-default-options
  '((trace . #f)))

(define %make-repl make-repl)
(define (make-repl lang)
  (let ((cenv (make-cenv :vm (the-vm)
			 :language (lookup-language lang)
			 :module (current-module))))
    (%make-repl :env cenv
                :options repl-default-options
                :tm-stats (times)
                :gc-stats (gc-stats)
                :vm-stats (vm-stats (cenv-vm cenv)))))

(define (repl-welcome repl)
  (let ((language (cenv-language (repl-env repl))))
    (format #t "~A interpreter ~A on Guile ~A\n"
            (language-title language) (language-version language) (version)))
  (display "Copyright (C) 2001 Free Software Foundation, Inc.\n\n")
  (display "Enter `,help' for help.\n"))

(define (repl-prompt repl)
  (format #f "~A@~A> " (language-name (cenv-language (repl-env repl)))
          (module-name (cenv-module (repl-env repl)))))

(define (repl-read repl)
  ((language-reader (cenv-language (repl-env repl)))))

(define (repl-compile repl form . opts)
  (apply compile-in form (cenv-module (repl-env repl))
         (cenv-language (repl-env repl)) opts))

(define (repl-eval repl form)
  (let ((eval (language-evaluator (cenv-language (repl-env repl)))))
    (if eval
	(eval form (cenv-module (repl-env repl)))
	(vm-load (cenv-vm (repl-env repl)) (repl-compile repl form)))))

(define (repl-print repl val)
  (if (not (eq? val *unspecified*))
      (begin
	((language-printer (cenv-language (repl-env repl))) val)
	(newline))))

(define (repl-option-ref repl key)
  (assq-ref (repl-options repl) key))

(define (repl-option-set! repl key val)
  (set! (repl-options repl) (assq-set! (repl-options repl) key val)))


;;;
;;; Utilities
;;;

(define (puts x) (display x) (newline))

(define (->string x)
  (object->string x display))

(define (user-error msg . args)
  (throw 'user-error #f msg args #f))
