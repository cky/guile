;;; compile.scm --- Compile Scheme codes

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

(define-module (vm compile)
  :use-module (vm vm)
  :use-module (vm utils)
  :use-module (vm types)
  :use-module (vm bytecomp)
  :use-module (ice-9 syncase)
  :export (compile compile-file))

(define (compile form . opts)
  (catch 'result
    (lambda ()
      (let ((x (syncase form)))
	(if (or (memq #:e opts) (memq #:expand-only opts))
	    (throw 'result x))
	(set! x (parse x (make-env '() (make-top-level-env))))
	(if (or (memq #:p opts) (memq #:parse-only opts))
	    (throw 'result x))
	(set! x (byte-compile 0 #f x))
	(if (or (memq #:c opts) (memq #:compile-only opts))
	    (throw 'result x))
	(make-program (make-bytecode x) #f)))
    (lambda (key arg) arg)))

(define (compile-file file)
  (let ((out-file (string-append (substring file 0 (1- (string-length file)))
				 "c")))
    (with-input-from-file file
      (lambda ()
	(with-output-to-file out-file
	  (lambda ()
	    (format #t ";;; Compiled from ~A\n\n" file)
	    (display "(use-modules (vm vm))\n\n")
	    (display "(let ((vm (make-vm)))\n")
	    (display "  (define (vm-exec code)\n")
	    (display "    (vm-run vm (make-program (make-bytecode code) #f)))\n")
	    (do ((input (read) (read)))
		((eof-object? input))
	      (display "(vm-exec ")
	      (write (compile input #:compile-only))
	      (display ")\n"))
	    (display ")\n")))))))


;;;
;;; Parser
;;;

(define (parse x env)
  (cond ((pair? x) (parse-pair x env))
	((symbol? x) (make-code:ref env (env-ref env x)))
	(else (make-code:constant env x))))

(define (parse-pair x env)
  (let ((name (car x)) (args (cdr x)))
    (if (assq name *syntax-alist*)
	;; syntax
	((assq-ref *syntax-alist* name) args env)
	;; procedure
	(let ((proc (if (symbol? name)
			(env-ref env name)
			(parse name env))))
	  (if (and (variable? proc)
		   (variable-bound? proc)
		   (assq (variable-value proc) *procedure-alist*))
	      ;; procedure macro
	      ((assq-ref *procedure-alist* (variable-value proc)) args env)
	      ;; procedure call
	      (apply make-code:call env proc (map-parse args env)))))))

(define (map-parse x env)
  (map (lambda (x) (parse x env)) x))


;;;
;;; Syntax
;;;

(define *syntax-list*
  '(quote lambda set! define if cond and or begin let let* letrec
	  local-set! until))

(define (parse-quote args env)
  (make-code:constant env (car args)))

(define (canon-formals formals)
  ;; foo             -> (() . foo)
  ;; (foo bar baz)   -> ((foo bar baz) . #f)
  ;; (foo bar . baz) -> ((foo bar) . baz)
  (cond ((symbol? formals)
	 (cons '() formals))
	((or (null? formals)
	     (null? (cdr (last-pair formals))))
	 (cons formals #f))
	(else
	 (let* ((copy (list-copy formals))
		(pair (last-pair copy))
		(last (cdr pair)))
	   (set-cdr! pair '())
	   (cons copy last)))))

(define (parse-lambda args env)
  (let ((formals (car args)) (body (cdr args)))
    (let* ((pair (canon-formals formals))
	   (reqs (car pair))
	   (rest (cdr pair))
	   (syms (append reqs (if rest (list rest) '())))
	   (new-env (make-env syms env)))
      (make-code:program env (length reqs) (if rest #t #f)
			 (parse-begin body new-env)))))

(define (parse-set! args env)
  (let ((var (env-ref env (car args)))
	(val (parse (cadr args) env)))
    (variable-externalize! var)
    (make-code:set env var val)))

(define (parse-local-set! args env)
  (let ((var (env-ref env (car args)))
	(val (parse (cadr args) env)))
    (make-code:set env var val)))

(define (parse-define args env)
  (parse-set! args env))

(define (parse-if args env)
  (let ((test (parse (car args) env))
	(consequent (parse (cadr args) env))
	(alternate (if (null? (cddr args))
		       (make-code:unspecified env)
		       (parse (caddr args) env))))
    (make-code:if env test consequent alternate)))

;; FIXME: This should be expanded by syncase.
(define (parse-cond args env)
  (cond ((null? args) (make-code:unspecified env))
	((eq? (caar args) 'else)
	 (parse-begin (cdar args) env))
	(else
	 (let* ((clause (car args))
		(test (parse (car clause) env))
		(body (parse-begin (cdr clause) env))
		(alternate (parse-cond (cdr args) env)))
	   (make-code:if env test body alternate)))))

(define (parse-and args env)
  (apply make-code:and env (map-parse args env)))

(define (parse-or args env)
  (apply make-code:or env (map-parse args env)))

(define (parse-begin args env)
  (apply make-code:begin env (map-parse args env)))

(define (%parse-let:finish env bindings init body)
  (for-each (lambda (binding)
	      (env-remove-variable! env (car binding)))
	    bindings)
  (apply make-code:begin env (append! init body)))

(define (parse-let args env)
  (if (symbol? (car args))
      ;; named let
      (let ((tag (car args)) (bindings (cadr args)) (body (cddr args)))
	(let* ((var (env-add-variable! env tag))
	       (proc (parse-lambda (cons (map car bindings) body) env))
	       (init (make-code:set env var proc))
	       (call (apply make-code:call env var
			    (map-parse (map cadr bindings) env))))
	  (env-remove-variable! env tag)
	  (make-code:begin env init call)))
      ;; normal let
      (let ((bindings (car args)) (body (cdr args)))
	(let* (;; create values before binding
	       (vals (map-parse (map cadr bindings) env))
	       ;; create bindings
	       (init (map (lambda (sym val)
			    (let ((var (env-add-variable! env sym)))
			      (make-code:set env var val)))
			  (map car bindings) vals)))
	  (%parse-let:finish env bindings init (map-parse body env))))))

(define (parse-let* args env)
  (let ((bindings (car args)) (body (cdr args)))
    (let (;; create values and bindings one after another
	  (init (map (lambda (binding)
		       (let* ((val (parse (cadr binding) env))
			      (var (env-add-variable! env (car binding))))
			 (make-code:set env var val)))
		     bindings)))
      (%parse-let:finish env bindings init (map-parse body env)))))

(define (parse-letrec args env)
  (let ((bindings (car args)) (body (cdr args)))
    (let* (;; create all variables before values
	   (vars (map (lambda (sym)
			(env-add-variable! env sym))
		      (map car bindings)))
	   ;; create and set values
	   (init (map (lambda (var val)
			(make-code:set env var (parse val env)))
		      vars (map cadr bindings))))
      (%parse-let:finish env bindings init (map-parse body env)))))

(define (parse-until args env)
  (apply make-code:until env (parse (car args) env)
	 (map-parse (cdr args) env)))

(define *syntax-alist*
  (map (lambda (name)
	 (cons name (eval (symbol-append 'parse- name) (current-module))))
       *syntax-list*))


;;;
;;; Procedure
;;;

(define *procedure-list*
  '(caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr
	 caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
	 cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
	 map for-each))

(define (parse-caar args env) (parse `(car (car ,@args)) env))
(define (parse-cadr args env) (parse `(car (cdr ,@args)) env))
(define (parse-cdar args env) (parse `(cdr (car ,@args)) env))
(define (parse-cddr args env) (parse `(cdr (cdr ,@args)) env))

(define (parse-caaar args env) (parse `(car (car (car ,@args)))	env))
(define (parse-caadr args env) (parse `(car (car (cdr ,@args)))	env))
(define (parse-cadar args env) (parse `(car (cdr (car ,@args)))	env))
(define (parse-caddr args env) (parse `(car (cdr (cdr ,@args)))	env))
(define (parse-cdaar args env) (parse `(cdr (car (car ,@args)))	env))
(define (parse-cdadr args env) (parse `(cdr (car (cdr ,@args)))	env))
(define (parse-cddar args env) (parse `(cdr (cdr (car ,@args)))	env))
(define (parse-cdddr args env) (parse `(cdr (cdr (cdr ,@args)))	env))

(define (parse-caaaar args env) (parse `(car (car (car (car ,@args)))) env))
(define (parse-caaadr args env) (parse `(car (car (car (cdr ,@args)))) env))
(define (parse-caadar args env) (parse `(car (car (cdr (car ,@args)))) env))
(define (parse-caaddr args env) (parse `(car (car (cdr (cdr ,@args)))) env))
(define (parse-cadaar args env) (parse `(car (cdr (car (car ,@args)))) env))
(define (parse-cadadr args env) (parse `(car (cdr (car (cdr ,@args)))) env))
(define (parse-caddar args env) (parse `(car (cdr (cdr (car ,@args)))) env))
(define (parse-cadddr args env) (parse `(car (cdr (cdr (cdr ,@args)))) env))
(define (parse-cdaaar args env) (parse `(cdr (car (car (car ,@args)))) env))
(define (parse-cdaadr args env) (parse `(cdr (car (car (cdr ,@args)))) env))
(define (parse-cdadar args env) (parse `(cdr (car (cdr (car ,@args)))) env))
(define (parse-cdaddr args env) (parse `(cdr (car (cdr (cdr ,@args)))) env))
(define (parse-cddaar args env) (parse `(cdr (cdr (car (car ,@args)))) env))
(define (parse-cddadr args env) (parse `(cdr (cdr (car (cdr ,@args)))) env))
(define (parse-cdddar args env) (parse `(cdr (cdr (cdr (car ,@args)))) env))
(define (parse-cddddr args env) (parse `(cdr (cdr (cdr (cdr ,@args)))) env))

(define (parse-map args env)
  (check-nargs args >= 2)
  (case (length args)
    ((2)
     (let ((proc (car args)) (list (cadr args)))
       (parse `(let ((list ,list) (result '()))
		 (until (null? list)
		   (local-set! result (cons (,proc (car list)) result))
		   (local-set! list (cdr list)))
		 (reverse! result))
	      env)))
    (else
     (error "Not implemented yet"))))

(define (parse-for-each args env)
  (check-nargs args >= 2)
  (case (length args)
    ((2)
     (let ((proc (car args)) (list (cadr args)))
       (parse `(let ((list ,list))
		 (until (null? list)
		   (,proc (car list))
		   (local-set! list (cdr list))))
	      env)))
    (else
     (error "Not implemented yet"))))

(define *procedure-alist*
  (map (lambda (name)
	 (cons (eval name (current-module))
	       (eval (symbol-append 'parse- name) (current-module))))
       *procedure-list*))

;;; compile.scm ends here
