;;;; 	Copyright (C) 1997 Free Software Foundation, Inc.
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
;;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;; 


(define-module (ice-9 syncase)
  :use-module (ice-9 debug))



;;; Exported variables

(define-public install-global-transformer #f)
(define-public syntax-dispatch #f)
(define-public syntax-error #f)

(define-public bound-identifier=? #f)
(define-public datum->syntax-object #f)
(define-public define-syntax #f)
(define-public fluid-let-syntax #f)
(define-public free-identifier=? #f)
(define-public generate-temporaries #f)
(define-public identifier? #f)
(define-public identifier-syntax #f)
(define-public let-syntax #f)
(define-public letrec-syntax #f)
(define-public syntax #f)
(define-public syntax-case #f)
(define-public syntax-object->datum #f)
(define-public syntax-rules #f)
(define-public with-syntax #f)


;;; Hooks needed by the syntax-case macro package

(define-public (void) *unspecified*)

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
			  "%s %S"
			  (list why what)
			  '())))

(define (putprop symbol key binding)
  (let* ((m (current-module))
	 (v (or (module-variable m symbol)
		(module-make-local-var! m symbol))))
    (set-object-property! v key binding)))

(define (getprop symbol key)
  (let* ((m (current-module))
	 (v (module-variable m symbol)))
    (and v (object-property v key))))


;;; Compatibility

(define values:*values-rtd*
  (make-record-type "values"
		    '(values)))

(define values
  (let ((make-values (record-constructor values:*values-rtd*)))
    (lambda x
      (if (and (not (null? x))
	       (null? (cdr x)))
	  (car x)
	  (make-values x)))))

(define call-with-values
  (let ((access-values (record-accessor values:*values-rtd* 'values))
	(values-predicate? (record-predicate values:*values-rtd*)))
    (lambda (producer consumer)
      (let ((result (producer)))
	(if (values-predicate? result)
	    (apply consumer (access-values result))
	    (consumer result))))))


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


;;; The following line is necessary only if we start making changes
;; (load-from-path "ice-9/psyntax.ss")


;;; Setup some hooks for the module system and the evaluator

(variable-set! (builtin-variable 'sc-interface)
	       (module-public-interface (current-module)))
(variable-set! (builtin-variable 'sc-expand) sc-expand)
(variable-set! (builtin-variable 'scm:eval-transformer) #f)
