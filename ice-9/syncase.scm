;;;; 	Copyright (C) 1997, 2000, 2001 Free Software Foundation, Inc.
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;;
;;;; As a special exception, the Free Software Foundation gives permission
;;;; for additional uses of the text contained in its release of GUILE.
;;;;
;;;; The exception is that, if you link the GUILE library with other files
;;;; to produce an executable, this does not by itself cause the
;;;; resulting executable to be covered by the GNU General Public License.
;;;; Your use of that executable is in no way restricted on account of
;;;; linking the GUILE library code into it.
;;;;
;;;; This exception does not however invalidate any other reasons why
;;;; the executable file might be covered by the GNU General Public License.
;;;;
;;;; This exception applies only to the code released by the
;;;; Free Software Foundation under the name GUILE.  If you copy
;;;; code from other Free Software Foundation releases into a copy of
;;;; GUILE, as the General Public License permits, the exception does
;;;; not apply to the code that you add in this way.  To avoid misleading
;;;; anyone as to the status of such modified files, you must delete
;;;; this exception notice from them.
;;;;
;;;; If you write modifications of your own for GUILE, it is your choice
;;;; whether to permit this exception to apply to your modifications.
;;;; If you do not wish that, delete this exception notice.
;;;; 


(define-module (ice-9 syncase)
  :use-module (ice-9 debug)
  :export-syntax (sc-macro define-syntax eval-when fluid-let-syntax
		  identifier-syntax let-syntax
		  letrec-syntax syntax syntax-case  syntax-rules
		  with-syntax
		  include)
  :export (sc-expand sc-expand3 install-global-transformer
	   syntax-dispatch syntax-error bound-identifier=?
	   datum->syntax-object free-identifier=?
	   generate-temporaries identifier? syntax-object->datum
	   void eval syncase))



(define sc-macro
  (procedure->memoizing-macro
    (lambda (exp env)
      (sc-expand exp))))

;;; Exported variables

(define sc-expand #f)
(define sc-expand3 #f)
(define install-global-transformer #f)
(define syntax-dispatch #f)
(define syntax-error #f)

(define bound-identifier=? #f)
(define datum->syntax-object #f)
(define define-syntax sc-macro)
(define eval-when sc-macro)
(define fluid-let-syntax sc-macro)
(define free-identifier=? #f)
(define generate-temporaries #f)
(define identifier? #f)
(define identifier-syntax sc-macro)
(define let-syntax sc-macro)
(define letrec-syntax sc-macro)
(define syntax sc-macro)
(define syntax-case sc-macro)
(define syntax-object->datum #f)
(define syntax-rules sc-macro)
(define with-syntax sc-macro)
(define include sc-macro)

(define primitive-syntax '(quote lambda letrec if set! begin define or
			      and let let* cond do quasiquote unquote
			      unquote-splicing case))

(for-each (lambda (symbol)
	    (set-symbol-property! symbol 'primitive-syntax #t))
	  primitive-syntax)

;;; Hooks needed by the syntax-case macro package

(define (void) *unspecified*)

(define andmap
  (lambda (f first . rest)
    (or (null? first)
        (if (null? rest)
            (let andmap ((first first))
              (let ((x (car first)) (first (cdr first)))
                (if (null? first)
                    (f x)
                    (and (f x) (andmap first)))))
            (let andmap ((first first) (rest rest))
              (let ((x (car first))
                    (xr (map car rest))
                    (first (cdr first))
                    (rest (map cdr rest)))
                (if (null? first)
                    (apply f (cons x xr))
                    (and (apply f (cons x xr)) (andmap first rest)))))))))

(define (error who format-string why what)
  (start-stack 'syncase-stack
	       (scm-error 'misc-error
			  who
			  "~A ~S"
			  (list why what)
			  '())))

(define the-syncase-module (current-module))

(define (putprop symbol key binding)
  (let* ((m (current-module))
	 (v (or (module-variable m symbol)
		(module-make-local-var! m symbol))))
    (if (symbol-property symbol 'primitive-syntax)
	(if (eq? (current-module) the-syncase-module)
	    (set-object-property! (module-variable the-root-module symbol)
				  key
				  binding))
	(variable-set! v sc-macro))
    (set-object-property! v key binding)))

(define (getprop symbol key)
  (let* ((m (current-module))
	 (v (module-variable m symbol)))
    (and v (or (object-property v key)
	       (let ((root-v (module-local-variable the-root-module symbol)))
		 (and (equal? root-v v)
		      (object-property root-v key)))))))

(define generated-symbols (make-weak-key-hash-table 1019))

;; We define our own gensym here because the Guile built-in one will
;; eventually produce uninterned and unreadable symbols (as needed for
;; safe macro expansions) and will the be inappropriate for dumping to
;; pssyntax.pp.
;;
;; syncase is supposed to only require that gensym produce unique
;; readable symbols, and they only need be unique with respect to
;; multiple calls to gensym, not globally unique.
;;
(define gensym
  (let ((counter 0))
    (lambda (. rest)
      (let ((val (number->string counter)))
        (set! counter (+ counter 1))
        (cond
         ((null? rest)
          (string->symbol (string-append "syntmp-" val)))
         ((null? (cdr rest))
          (string->symbol (string-append "syntmp-" (car rest) "-" val)))
         (else
          (error
           "syncase's gensym called with the wrong number of arguments")))))))

;;; Load the preprocessed code

(let ((old-debug #f)
      (old-read #f))
  (dynamic-wind (lambda ()
		  (set! old-debug (debug-options))
		  (set! old-read (read-options)))
		(lambda ()
		  (debug-disable 'debug 'procnames)
		  (read-disable 'positions)
		  (load-from-path "ice-9/psyntax.pp"))
		(lambda ()
		  (debug-options old-debug)
		  (read-options old-read))))


;;; The following lines are necessary only if we start making changes
;; (use-syntax sc-expand)
;; (load-from-path "ice-9/psyntax.ss")

(define internal-eval (nested-ref the-scm-module '(app modules guile eval)))

(define (eval x environment)
  (internal-eval (if (and (pair? x)
			  (equal? (car x) "noexpand"))
		     (cadr x)
		     (sc-expand x))
		 environment))

;;; Hack to make syncase macros work in the slib module
(let ((m (nested-ref the-root-module '(app modules ice-9 slib))))
  (if m
      (set-object-property! (module-local-variable m 'define)
			    '*sc-expander*
			    '(define))))

(define syncase sc-expand)
