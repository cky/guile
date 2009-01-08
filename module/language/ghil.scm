;;; Guile High Intermediate Language

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

(define-module (language ghil)
  #:use-module (system base syntax)
  #:use-module (system base pmatch)
  #:use-module (ice-9 regex)
  #:export
  (<ghil-void> make-ghil-void ghil-void?
   ghil-void-env ghil-void-loc

   <ghil-quote> make-ghil-quote ghil-quote?
   ghil-quote-env ghil-quote-loc ghil-quote-obj

   <ghil-quasiquote> make-ghil-quasiquote ghil-quasiquote?
   ghil-quasiquote-env ghil-quasiquote-loc ghil-quasiquote-exp

   <ghil-unquote> make-ghil-unquote ghil-unquote?
   ghil-unquote-env ghil-unquote-loc ghil-unquote-exp

   <ghil-unquote-splicing> make-ghil-unquote-splicing ghil-unquote-splicing?
   ghil-unquote-env ghil-unquote-loc ghil-unquote-exp

   <ghil-ref> make-ghil-ref ghil-ref?
   ghil-ref-env ghil-ref-loc ghil-ref-var

   <ghil-set> make-ghil-set ghil-set?
   ghil-set-env ghil-set-loc ghil-set-var ghil-set-val

   <ghil-define> make-ghil-define ghil-define?
   ghil-define-env ghil-define-loc ghil-define-var ghil-define-val

   <ghil-if> make-ghil-if ghil-if?
   ghil-if-env ghil-if-loc ghil-if-test ghil-if-then ghil-if-else

   <ghil-and> make-ghil-and ghil-and?
   ghil-and-env ghil-and-loc ghil-and-exps

   <ghil-or> make-ghil-or ghil-or?
   ghil-or-env ghil-or-loc ghil-or-exps

   <ghil-begin> make-ghil-begin ghil-begin?
   ghil-begin-env ghil-begin-loc ghil-begin-exps

   <ghil-bind> make-ghil-bind ghil-bind?
   ghil-bind-env ghil-bind-loc ghil-bind-vars ghil-bind-vals ghil-bind-body

   <ghil-mv-bind> make-ghil-mv-bind ghil-mv-bind?
   ghil-mv-bind-env ghil-mv-bind-loc ghil-mv-bind-producer ghil-mv-bind-vars ghil-mv-bind-rest ghil-mv-bind-body

   <ghil-lambda> make-ghil-lambda ghil-lambda?
   ghil-lambda-env ghil-lambda-loc ghil-lambda-vars ghil-lambda-rest
   ghil-lambda-meta ghil-lambda-body

   <ghil-inline> make-ghil-inline ghil-inline?
   ghil-inline-env ghil-inline-loc ghil-inline-inline ghil-inline-args

   <ghil-call> make-ghil-call ghil-call?
   ghil-call-env ghil-call-loc ghil-call-proc ghil-call-args

   <ghil-mv-call> make-ghil-mv-call ghil-mv-call?
   ghil-mv-call-env ghil-mv-call-loc ghil-mv-call-producer ghil-mv-call-consumer

   <ghil-values> make-ghil-values ghil-values?
   ghil-values-env ghil-values-loc ghil-values-values

   <ghil-values*> make-ghil-values* ghil-values*?
   ghil-values*-env ghil-values*-loc ghil-values*-values

   <ghil-var> make-ghil-var ghil-var?
   ghil-var-env ghil-var-name ghil-var-kind ghil-var-index

   <ghil-toplevel-env> make-ghil-toplevel-env ghil-toplevel-env?
   ghil-toplevel-env-table

   <ghil-env> make-ghil-env ghil-env?
   ghil-env-parent ghil-env-table ghil-env-variables

   <ghil-reified-env> make-ghil-reified-env ghil-reified-env?
   ghil-reified-env-env ghil-reified-env-loc

   ghil-env-add!
   ghil-env-reify ghil-env-dereify
   ghil-var-is-bound? ghil-var-for-ref! ghil-var-for-set! ghil-var-define!
   ghil-var-at-module!
   call-with-ghil-environment call-with-ghil-bindings

   parse-ghil unparse-ghil))


;;;
;;; Parse tree
;;;

(define (print-ghil x port)
  (format port "#<ghil ~s>" (unparse-ghil x)))

(define-type (<ghil> #:printer print-ghil)
  ;; Objects
  (<ghil-void> env loc)
  (<ghil-quote> env loc obj)
  (<ghil-quasiquote> env loc exp)
  (<ghil-unquote> env loc exp)
  (<ghil-unquote-splicing> env loc exp)
  ;; Variables
  (<ghil-ref> env loc var)
  (<ghil-set> env loc var val)
  (<ghil-define> env loc var val)
  ;; Controls
  (<ghil-if> env loc test then else)
  (<ghil-and> env loc exps)
  (<ghil-or> env loc exps)
  (<ghil-begin> env loc exps)
  (<ghil-bind> env loc vars vals body)
  (<ghil-mv-bind> env loc producer vars rest body)
  (<ghil-lambda> env loc vars rest meta body)
  (<ghil-call> env loc proc args)
  (<ghil-mv-call> env loc producer consumer)
  (<ghil-inline> env loc inline args)
  (<ghil-values> env loc values)
  (<ghil-values*> env loc values)
  (<ghil-reified-env> env loc))



;;;
;;; Variables
;;;

(define-record <ghil-var> env name kind (index #f))


;;;
;;; Modules
;;;


;;;
;;; Environments
;;;

(define-record <ghil-env> parent (table '()) (variables '()))
(define-record <ghil-toplevel-env> (table '()))

(define (ghil-env-ref env sym)
  (assq-ref (ghil-env-table env) sym))

(define-macro (push! item loc)
  `(set! ,loc (cons ,item ,loc)))
(define-macro (apush! k v loc)
  `(set! ,loc (acons ,k ,v ,loc)))
(define-macro (apopq! k loc)
  `(set! ,loc (assq-remove! ,loc ,k)))

(define (ghil-env-add! env var)
  (apush! (ghil-var-name var) var (ghil-env-table env))
  (push! var (ghil-env-variables env)))

(define (ghil-env-remove! env var)
  (apopq! (ghil-var-name var) (ghil-env-table env)))

(define (force-heap-allocation! var)
  (set! (ghil-var-kind var) 'external))
  


;;;
;;; Public interface
;;;

;; The following four functions used to be one, in ghil-lookup. Now they
;; are four, to reflect the different intents. A bit of duplication, but
;; that's OK. The common current is to find out where a variable will be
;; stored at runtime.
;;
;; These functions first search the lexical environments. If the
;; variable is not in the innermost environment, make sure the variable
;; is marked as being "external" so that it goes on the heap. If the
;; variable is being modified (via a set!), also make sure it's on the
;; heap, so that other continuations see the changes to the var.
;;
;; If the variable is not found lexically, it is a toplevel variable,
;; which will be looked up at runtime with respect to the module that
;; was current when the lambda was bound, at runtime. The variable will
;; be resolved when it is first used.
(define (ghil-var-is-bound? env sym)
  (let loop ((e env))
    (record-case e
      ((<ghil-toplevel-env> table)
       (let ((key (cons (module-name (current-module)) sym)))
         (assoc-ref table key)))
      ((<ghil-env> parent table variables)
       (and (not (assq-ref table sym))
            (loop parent))))))

(define (ghil-var-for-ref! env sym)
  (let loop ((e env))
    (record-case e
      ((<ghil-toplevel-env> table)
       (let ((key (cons (module-name (current-module)) sym)))
         (or (assoc-ref table key)
             (let ((var (make-ghil-var (car key) (cdr key) 'toplevel)))
               (apush! key var (ghil-toplevel-env-table e))
               var))))
      ((<ghil-env> parent table variables)
       (cond
        ((assq-ref table sym)
         => (lambda (var)
              (or (eq? e env)
                  (force-heap-allocation! var))
              var))
        (else
         (loop parent)))))))

(define (ghil-var-for-set! env sym)
  (let loop ((e env))
    (record-case e
      ((<ghil-toplevel-env> table)
       (let ((key (cons (module-name (current-module)) sym)))
         (or (assoc-ref table key)
             (let ((var (make-ghil-var (car key) (cdr key) 'toplevel)))
               (apush! key var (ghil-toplevel-env-table e))
               var))))
      ((<ghil-env> parent table variables)
       (cond
        ((assq-ref table sym)
         => (lambda (var)
              (force-heap-allocation! var)
              var))
        (else
         (loop parent)))))))

(define (ghil-var-at-module! env modname sym interface?)
  (let loop ((e env))
    (record-case e
      ((<ghil-toplevel-env> table)
       (let ((key (list modname sym interface?)))
         (or (assoc-ref table key)
             (let ((var (make-ghil-var modname sym
                                       (if interface? 'public 'private))))
               (apush! key var (ghil-toplevel-env-table e))
               var))))
      ((<ghil-env> parent table variables)
       (loop parent)))))

(define (ghil-var-define! toplevel sym)
  (let ((key (cons (module-name (current-module)) sym)))
    (or (assoc-ref (ghil-toplevel-env-table toplevel) key)
        (let ((var (make-ghil-var (car key) (cdr key) 'toplevel)))
          (apush! key var (ghil-toplevel-env-table toplevel))
          var))))
          
(define (call-with-ghil-environment e syms func)
  (let* ((e (make-ghil-env e))
         (vars (map (lambda (s)
                      (let ((v (make-ghil-var e s 'argument)))
                        (ghil-env-add! e v) v))
                    syms)))
    (func e vars)))

(define (call-with-ghil-bindings e syms func)
  (let* ((vars (map (lambda (s)
		      (let ((v (make-ghil-var e s 'local)))
			(ghil-env-add! e v) v))
		    syms))
	 (ret (func vars)))
    (for-each (lambda (v) (ghil-env-remove! e v)) vars)
    ret))

(define (ghil-env-reify env)
  (let loop ((e env) (out '()))
    (record-case e
      ((<ghil-toplevel-env> table)
       (map (lambda (v)
              (cons (ghil-var-name v)
                    (or (ghil-var-index v)
                        (error "reify called before indices finalized"))))
            out))
      ((<ghil-env> parent table variables)
       (loop parent
             (append out
                     (filter (lambda (v) (eq? (ghil-var-kind v) 'external))
                             variables)))))))

(define (ghil-env-dereify name-index-alist)
  (let* ((e (make-ghil-env (make-ghil-toplevel-env)))
         (vars (map (lambda (pair)
                      (make-ghil-var e (car pair) 'external (cdr pair)))
                    name-index-alist)))
    (set! (ghil-env-table e)
          (map (lambda (v) (cons (ghil-var-name v) v)) vars))
    (set! (ghil-env-variables e) vars)
    e))


;;;
;;; Parser
;;;

(define (location x)
  (and (pair? x)
       (let ((props (source-properties x)))
	 (and (not (null? props))
	      (vector (assq-ref props 'line)
                      (assq-ref props 'column)
                      (assq-ref props 'filename))))))

(define (parse-quasiquote e x level)
  (cond ((not (pair? x)) x)
	((memq (car x) '(unquote unquote-splicing))
	 (let ((l (location x)))
	   (pmatch (cdr x)
	     ((,obj)
              (cond
               ((zero? level) 
                (if (eq? (car x) 'unquote)
                    (make-ghil-unquote e l (parse-ghil e obj))
                    (make-ghil-unquote-splicing e l (parse-ghil e obj))))
               (else
                (list (car x) (parse-quasiquote e obj (1- level))))))
	     (else (syntax-error l (format #f "bad ~A" (car x)) x)))))
        ((eq? (car x) 'quasiquote)
	 (let ((l (location x)))
	   (pmatch (cdr x)
	     ((,obj) (list 'quasiquote (parse-quasiquote e obj (1+ level))))
             (else (syntax-error l (format #f "bad ~A" (car x)) x)))))
	(else (cons (parse-quasiquote e (car x) level)
		    (parse-quasiquote e (cdr x) level)))))

(define (parse-ghil env exp)
  (let ((loc (location exp))
        (retrans (lambda (x) (parse-ghil env x))))
    (pmatch exp
     (,exp (guard (symbol? exp))
           (make-ghil-ref env #f (ghil-var-for-ref! env exp)))

     (,exp (guard (not (pair? exp)))
           (make-ghil-quote #:env env #:loc #f #:obj exp))

     (('quote ,exp) (make-ghil-quote #:env env #:loc loc #:obj exp))

     ((void) (make-ghil-void env loc))

     ((lambda ,syms ,rest ,meta . ,body)
      (call-with-ghil-environment env syms
        (lambda (env vars)
          (make-ghil-lambda env loc vars rest meta
                            (parse-ghil env `(begin ,@body))))))

     ((begin . ,body)
      (make-ghil-begin env loc (map retrans body)))

     ((bind ,syms ,exprs . ,body)
      (let ((vals (map retrans exprs)))
        (call-with-ghil-bindings env syms
          (lambda (vars)
            (make-ghil-bind env loc vars vals (retrans `(begin ,@body)))))))

     ((bindrec ,syms ,exprs . ,body)
      (call-with-ghil-bindings env syms
        (lambda (vars)
          (let ((vals (map (lambda (exp) (parse-ghil env exp)) exprs)))
            (make-ghil-bind env loc vars vals (retrans `(begin ,@body)))))))

     ((set! ,sym ,val)
      (make-ghil-set env loc (ghil-var-for-set! env sym) (retrans val)))

     ((define ,sym ,val)
      (make-ghil-define env loc (ghil-var-define! env sym) (retrans val)))

     ((if ,test ,then ,else)
      (make-ghil-if env loc (retrans test) (retrans then) (retrans else)))

     ((and . ,exps)
      (make-ghil-and env loc (map retrans exps)))

     ((or . ,exps)
      (make-ghil-or env loc (map retrans exps)))

     ((mv-bind ,syms ,rest ,producer . ,body)
      (call-with-ghil-bindings env syms
        (lambda (vars)
          (make-ghil-mv-bind env loc (retrans producer) vars rest
                             (map retrans body)))))

     ((call ,proc . ,args)
      (make-ghil-call env loc (retrans proc) (map retrans args)))

     ((mv-call ,producer . ,consumer)
      (make-ghil-mv-call env loc (retrans producer) (retrans consumer)))

     ((inline ,op . ,args)
      (make-ghil-inline env loc op (map retrans args)))

     ((values . ,values)
      (make-ghil-values env loc (map retrans values)))

     ((values* . ,values)
      (make-ghil-values* env loc (map retrans values)))

     ((compile-time-environment)
      (make-ghil-reified-env env loc))

     ((quasiquote ,exp)
      (make-ghil-quasiquote env loc #:exp (parse-quasiquote env exp 0)))

     (else
      (error "unrecognized GHIL" exp)))))

(define (unparse-ghil ghil)
  (record-case ghil
    ((<ghil-void> env loc)
     '(void))
    ((<ghil-quote> env loc obj)
     (if (symbol? obj)
         `(,'quote ,obj)
         obj))
    ((<ghil-quasiquote> env loc exp)
     `(,'quasiquote ,(map unparse-ghil exp)))
    ((<ghil-unquote> env loc exp)
     `(,'unquote ,(unparse-ghil exp)))
    ((<ghil-unquote-splicing> env loc exp)
     `(,'unquote-splicing ,(unparse-ghil exp)))
  ;; Variables
    ((<ghil-ref> env loc var)
     (ghil-var-name var))
    ((<ghil-set> env loc var val)
     `(set! ,(ghil-var-name var) ,(unparse-ghil val)))
    ((<ghil-define> env loc var val)
     `(define ,(ghil-var-name var) ,(unparse-ghil val)))
  ;; Controls
    ((<ghil-if> env loc test then else)
     `(if ,(unparse-ghil test) ,(unparse-ghil then) ,(unparse-ghil else)))
    ((<ghil-and> env loc exps)
     `(and ,@(map unparse-ghil exps)))
    ((<ghil-or> env loc exps)
     `(or ,@(map unparse-ghil exps)))
    ((<ghil-begin> env loc exps)
     `(begin ,@(map unparse-ghil exps)))
    ((<ghil-bind> env loc vars vals body)
     `(bind ,(map ghil-var-name vars) ,(map unparse-ghil vals)
            ,@(map unparse-ghil body)))
    ((<ghil-mv-bind> env loc producer vars rest body)
     `(mv-bind ,(map ghil-var-name vars) ,rest
               ,(unparse-ghil producer) ,@(map unparse-ghil body)))
    ((<ghil-lambda> env loc vars rest meta body)
     `(lambda ,(map ghil-var-name vars) ,rest ,meta
              ,(unparse-ghil body)))
    ((<ghil-call> env loc proc args)
     `(call ,(unparse-ghil proc) ,@(map unparse-ghil args)))
    ((<ghil-mv-call> env loc producer consumer)
     `(mv-call ,(unparse-ghil producer) ,(unparse-ghil consumer)))
    ((<ghil-inline> env loc inline args)
     `(inline ,inline ,@(map unparse-ghil args)))
    ((<ghil-values> env loc values)
     `(values ,@(map unparse-ghil values)))
    ((<ghil-values*> env loc values)
     `(values* ,@(map unparse-ghil values)))
    ((<ghil-reified-env> env loc)
     `(compile-time-environment))))
