;;; types.scm --- data types used in the compiler and assembler

;; Copyright (C) 2000 Free Software Foundation, Inc.

;; This file is part of Guile VM.

;; Guile VM is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; Guile VM is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with Guile VM; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(define-module (vm types)
  :use-module (vm vm)
  :use-module (vm utils)
  :use-module (oop goops))


;;; 
;;; VM code
;;;

(define-class <vm:code> ()
  (tag  #:accessor code-tag  #:init-keyword #:tag)
  (env  #:accessor code-env  #:init-keyword #:env)
  (args #:accessor code-args #:init-keyword #:args)
  (type #:accessor code-type #:init-value #f))

(export code-tag code-env code-args code-type)

(define-method (write (obj <vm:code>) port)
  (display "#<vm:")
  (display (keyword->symbol (code-tag obj)))
  (map (lambda (obj) (display " ") (write obj port))
       (code-args obj))
  (display ">"))

(define-public (code? obj)
  (is-a? obj <vm:code>))

(define-public (make-code tag env . args)
  (make <vm:code> #:tag tag #:env env #:args args))


;;;
;;; VM label
;;;

(define-class <vm:label> ()
  (pos #:accessor label-position))

(export label-position)

(define-public (label? obj)
  (is-a? obj <vm:label>))

(define-public (make-label)
  (make <vm:label>))


;;;
;;; VM location
;;;

(define-class <vm:location> ())

(define (make-location)
  (make <vm:location>))


;;;
;;; VM variable
;;;

(define-class <vm:var> ()
  (name  #:accessor variable-name  #:init-keyword #:name)
  (type  #:accessor variable-type  #:init-value #f)
  (value #:accessor variable-value)
  (loc   #:accessor variable-location #:init-keyword #:location)
  (count #:accessor variable-count #:init-value 0))

(define-class <vm:local-var> (<vm:var>))
(define-class <vm:external-var> (<vm:var>))
(define-class <vm:top-level-var> (<vm:var>))

(export variable-name variable-type variable-value variable-count)

(define-method (write (obj <vm:var>) port)
  (display "#")
  (display (class-name (class-of obj)))
  (display " ")
  (display (variable-name obj))
  (display ">"))

(define-public (make-local-variable name location)
  (make <vm:local-var> #:name name #:location location))

(define-public (make-top-level-variable name)
  (make <vm:top-level-var> #:name name))

(define-public (variable? obj)
  (is-a? obj <vm:var>))

(define-public (local-variable? obj)
  (is-a? obj <vm:local-var>))

(define-public (external-variable? obj)
  (is-a? obj <vm:external-var>))

(define-public (top-level-variable? obj)
  (is-a? obj <vm:top-level-var>))

(define-public (variable-bound? var)
  (assert variable? var)
  (slot-bound? var 'value))

(define-public (variable-externalize! var)
  (assert variable? var)
  (if (local-variable? var)
      (change-class var <vm:external-var>)))


;;;
;;; VM environment
;;;

(define-class <vm:env> ()
  (space #:accessor env-name-space #:init-value '())
  (args  #:accessor env-arguments  #:init-keyword #:args)
  (vars  #:accessor env-variables  #:init-value '())
  (locs  #:accessor env-locations  #:init-value '())
  (exts  #:accessor env-externals  #:init-value #f)
  (link  #:accessor env-external-link #:init-keyword #:link))

(define-public (make-env syms link)
  (let* ((syms (reverse syms))
	 (args (map (lambda (sym)
		      (make-local-variable sym (make-location)))
		    syms))
	 (env (make <vm:env> #:args args #:link link)))
    (for-each (lambda (sym var)
		(set! (env-name-space env)
		      (acons sym var (env-name-space env))))
	      syms args)
    env))

(define-public (make-top-level-env)
  (make-env '() #f))

(define-public (env? obj) (is-a? obj <vm:env>))

(define-public (top-level-env? obj)
  (and (env? obj) (not (env-external-link obj))))

(define-public (env-finalized? env)
  (if (env-externals env) #t #f))

(define-public (env-add-variable! env sym)
  (assert env? env)
  (assert symbol? sym)
  (if (env-finalized? env)
      (error "You may not add a variable after finalization"))
  (let ((var (if (top-level-env? env)
		 (make-top-level-variable sym)
		 (let* ((locs (env-locations env))
			(loc (if (null? locs)
				 (make-location)
				 (begin
				   (set! (env-locations env) (cdr locs))
				   (car locs)))))
		   (make-local-variable sym loc)))))
    (set! (env-name-space env) (acons sym var (env-name-space env)))
    (set! (env-variables env) (cons var (env-variables env)))
    var))

(define-public (env-remove-variable! env sym)
  (assert env? env)
  (assert symbol? sym)
  (if (env-finalized? env)
      (error "You may not remove a variable after finalization"))
  (let ((var (assq-ref (env-name-space env) sym)))
    (if (not var)
	(error "No such variable: ~A\n" sym))
    (if (local-variable? var)
	(set! (env-locations env)
	      (cons (variable-location var) (env-locations env))))
    (set! (env-name-space env)
	  (delq! (assq sym (env-name-space env)) (env-name-space env)))
    var))

;; Find a varialbe in the environment

(define-public (env-ref env sym)
  (assert env? env)
  (assert symbol? sym)
  (if (env-finalized? env)
      (error "You may not find a variable after finalization"))
  (or (env-local-ref env sym)
      (env-external-ref env sym)
      (env-top-level-ref env sym)
      (error "No way!")))

(define (env-local-ref env sym)
  (if (assq sym (env-name-space env))
      (let ((var (assq-ref (env-name-space env) sym)))
	(set! (variable-count var) (1+ (variable-count var)))
	var)
      #f))

(define (env-external-ref env sym)
  (let ((ext-env (env-external-link env)))
    (if (not ext-env)
	#f
	(let ((var (env-local-ref ext-env sym)))
	  (if var
	      (begin
		(variable-externalize! var)
		var)
	      (env-external-ref ext-env sym))))))

(define (env-top-level-ref env sym)
  (let ((var (make-top-level-variable sym)))
    (if (defined? sym)
	;; Get the value in the top-level
	(let ((obj (eval sym (interaction-environment))))
	  (set! (variable-value var) obj)
	  (set! (variable-type var)
		(cond ((macro? obj) 'macro)
		      ((program? obj) 'program)
		      ((procedure? obj) 'function)
		      (else #f)))))
    var))

;; Finalization

(define-public (env-finalize! env)
  (if (not (env-finalized? env))
      (let ((locs (uniq! (map variable-location
			      (append (filter local-variable?
					      (env-variables env))
				      (env-arguments env)))))
	    (exts (filter external-variable?
			  (append (env-variables env) (env-arguments env)))))
	(set! (env-locations env) locs)
	(set! (env-externals env) (reverse! exts)))))

(define-public (env-header env)
  (env-finalize! env)
  (let ((nvars (length (uniq! (map variable-location
				   (filter local-variable?
					   (env-variables env))))))
	(nexts (length (env-externals env)))
	(exts (list->vector
	       (map (lambda (var)
		      (env-local-variable-address env var))
		    (filter external-variable?
			    (reverse (env-arguments env)))))))
    (list nvars nexts exts)))

(define (get-offset obj list)
  (- (length list) (length (memq obj list))))

(define-generic env-variable-address)

(define-method (env-variable-address (env <vm:env>) (var <vm:local-var>))
  (env-finalize! env)
  (get-offset (variable-location var) (env-locations env)))

(define-method (env-variable-address (env <vm:env>) (var <vm:external-var>))
  (env-finalize! env)
  (let loop ((depth 0) (env env))
    (let ((list (env-externals env)))
      (cond ((null? list)
	     (loop depth (env-external-link env)))
	    ((memq var list)
	     (cons depth (get-offset var list)))
	    (else (loop (1+ depth) (env-external-link env)))))))


;;;
;;; Intermediate codes
;;;

(define-public (make-code:unspecified env)
  (assert env? env)
  (make-code #:unspecified env))

(define-public (make-code:constant env obj)
  (assert env? env)
  (make-code #:constant env obj))

(define-public (make-code:ref env var)
  (assert env? env)
  (assert variable? var)
  (let ((code (make-code #:ref env var)))
    (set! (code-type code) (variable-type var))
    code))

(define-public (make-code:set env var val)
  (assert env? env)
  (assert variable? var)
  (assert code? val)
  (let ((code (make-code #:set env var val)))
    (set! (variable-type var) (code-type val))
    (set! (code-type code) (variable-type var))
    code))

(define-public (make-code:program env nreqs restp body)
  (assert env? env)
  (assert integer? nreqs)
  (assert boolean? restp)
  (assert code? body)
  (let ((code (make-code #:make-program env nreqs restp body)))
    (set! (code-type code) 'program)
    code))

(define-public (make-code:call env proc . args)
  (assert env? env)
  (assert (lambda (x) (or (variable? x) (code? x))) proc)
  (assert-for-each code? args)
  (apply make-code #:call env proc args))

(define-public (make-code:if env test consequent alternate)
  (assert env? env)
  (assert code? test)
  (assert code? consequent)
  (assert code? alternate)
  (let ((code (make-code #:if env test consequent alternate)))
    (if (eq? (code-type consequent) (code-type alternate))
	(set! (code-type code) (code-type consequent)))
    code))

(define-public (make-code:and env . args)
  (assert env? env)
  (assert-for-each code? args)
  (apply make-code #:and args))

(define-public (make-code:or env . args)
  (assert env? env)
  (assert-for-each code? args)
  (apply make-code #:or args))

(define-public (make-code:begin env . body)
  (assert env? env)
  (assert-for-each code? body)
  (let ((code (apply make-code #:begin env body)))
    (set! (code-type code) (code-type (last body)))
    code))

(define-public (make-code:until env test . body)
  (assert env? env)
  (assert code? test)
  (assert-for-each code? body)
  (apply make-code #:until env test body))

;;; types.scm ends here
