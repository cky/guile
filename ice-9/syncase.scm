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

(define (putprop s p v)
  (builtin-variable s)
  (set-symbol-property! s p v))
(define getprop symbol-property)

(define-public sc-expand #f)
(define-public install-global-transformer #f)
(define-public syntax-dispatch #f)
(define-public syntax-error #f)

;;;*fixme* builtin-variable
(define-public bound-identifier=? #f)
(define-public datum->syntax-object #f)
(builtin-variable 'define-syntax)
(builtin-variable 'fluid-let-syntax)
(define-public free-identifier=? #f)
(define-public generate-temporaries #f)
(define-public identifier? #f)
(builtin-variable 'identifier-syntax)
(builtin-variable 'let-syntax)
(builtin-variable 'letrec-syntax)
(builtin-variable 'syntax)
(builtin-variable 'syntax-case)
(define-public syntax-object->datum #f)
(builtin-variable 'syntax-rules)
(builtin-variable 'with-syntax)

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

;; The followin line is necessary only if we start making changes
;; (load-from-path "ice-9/psyntax.ss")

(define-public (eval-options . args)
  '())

;;; *fixme*
(define-public (eval-enable x)
  (variable-set! (builtin-variable 'scm:eval-transformer) sc-expand))

(define-public (eval-disable x)
  (variable-set! (builtin-variable 'scm:eval-transformer) #f))

(eval-enable 'syncase)
