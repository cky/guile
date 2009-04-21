;;;; 	Copyright (C) 1997, 2000, 2001, 2002, 2003, 2006 Free Software Foundation, Inc.
;;;; 
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2.1 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;;;; 


(define-module (ice-9 syncase)
  :use-module (ice-9 expand-support)
  :use-module (ice-9 debug)
  :use-module (ice-9 threads)
  :export-syntax (sc-macro define-syntax define-syntax-public 
                  fluid-let-syntax
		  identifier-syntax let-syntax
		  letrec-syntax syntax syntax-case  syntax-rules
		  with-syntax
		  include)
  :export (sc-expand sc-expand3 install-global-transformer
	   syntax-dispatch syntax-error bound-identifier=?
	   datum->syntax-object free-identifier=?
	   generate-temporaries identifier? syntax-object->datum
	   void syncase)
  :replace (eval eval-when))



(define (annotation? x) #f)

(define sc-macro
  (procedure->memoizing-macro
    (lambda (exp env)
      (save-module-excursion
       (lambda ()
         ;; Because memoization happens lazily, env's module isn't
         ;; necessarily the current module.
         (set-current-module (eval-closure-module (car (last-pair env))))
         (strip-expansion-structures (sc-expand exp)))))))

;;; Exported variables

(define sc-expand #f)
(define sc-expand3 #f)
(define sc-chi #f)
(define install-global-transformer #f)
(define syntax-dispatch #f)
(define syntax-error #f)

(define bound-identifier=? #f)
(define datum->syntax-object #f)
(define free-identifier=? #f)
(define generate-temporaries #f)
(define identifier? #f)
(define syntax-object->datum #f)

(define primitive-syntax '(quote lambda letrec if set! begin define or
			   and let let* cond do quasiquote unquote
			   unquote-splicing case @ @@))

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

(define guile-macro
  (cons 'external-macro
	(lambda (e r w s mod)
	  (let ((e (syntax-object->datum e)))
	    (if (symbol? e)
		;; pass the expression through
		e
		(let* ((mod (resolve-module mod))
                       (m (module-ref mod (car e))))
		  (if (eq? (macro-type m) 'syntax)
		      ;; pass the expression through
		      e
		      ;; perform Guile macro transform
		      (let ((e ((macro-transformer m)
				(strip-expansion-structures e)
				(append r (list (module-eval-closure mod))))))
			(if (variable? e)
			    e
			    (if (null? r)
				(sc-expand e)
				(sc-chi e r w (module-name mod))))))))))))

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

    (define next-id
      (if (provided? 'threads)
          (let ((symlock (make-mutex)))
            (lambda ()
              (let ((result #f))
                (with-mutex symlock
                  (set! result counter)
                  (set! counter (+ counter 1)))
                result)))
          ;; faster, non-threaded case.
          (lambda ()
            (let ((result counter))
              (set! counter (+ counter 1))
              result))))
    
    ;; actual gensym body code.
    (lambda (. rest)
      (let* ((next-val (next-id))
             (valstr (number->string next-val)))
          (cond
           ((null? rest)
            (string->symbol (string-append "syntmp-" valstr)))
           ((null? (cdr rest))
            (string->symbol (string-append "syntmp-" (car rest) "-" valstr)))
           (else
            (error
             (string-append
              "syncase's gensym expected 0 or 1 arguments, got "
              (length rest)))))))))

;;; Load the preprocessed code

(let ((old-debug #f)
      (old-read #f))
  (dynamic-wind (lambda ()
		  (set! old-debug (debug-options))
		  (set! old-read (read-options)))
		(lambda ()
                  (debug-disable 'debug 'procnames)
                  (read-disable 'positions)
		  (load-from-path "ice-9/psyntax-pp"))
		(lambda ()
		  (debug-options old-debug)
		  (read-options old-read))))

(define internal-eval (nested-ref the-scm-module '(%app modules guile eval)))

(define (eval x environment)
  (internal-eval (if (and (pair? x)
			  (equal? (car x) "noexpand"))
		     (strip-expansion-structures (cadr x))
		     (strip-expansion-structures (sc-expand x)))
		 environment))

;;; Hack to make syncase macros work in the slib module
(let ((m (nested-ref the-root-module '(%app modules ice-9 slib))))
  (if m
      (set-object-property! (module-local-variable m 'define)
			    '*sc-expander*
			    '(define))))

(define (syncase exp)
  (strip-expansion-structures (sc-expand exp)))

(set-module-transformer! the-syncase-module syncase)

(define-syntax define-syntax-public
  (syntax-rules ()
    ((_ name rules ...)
     (begin
       ;(eval-case ((load-toplevel) (export-syntax name)))
       (define-syntax name rules ...)))))
