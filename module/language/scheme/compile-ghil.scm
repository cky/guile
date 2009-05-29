;;; Guile Scheme specification

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

(define-module (language scheme compile-ghil)
  #:use-module (system base pmatch)
  #:use-module (system base language)
  #:use-module (language ghil)
  #:use-module (language scheme inline)
  #:use-module (system vm objcode)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 optargs)
  #:use-module (language tree-il)
  #:use-module ((system base compile) #:select (syntax-error))
  #:export (compile-ghil translate-1
            *translate-table* define-scheme-translator))

;;; environment := #f
;;;                | MODULE
;;;                | COMPILE-ENV
;;; compile-env := (MODULE LEXICALS|GHIL-ENV . EXTERNALS)
(define (cenv-module env)
  (cond ((not env) #f)
        ((module? env) env)
        ((and (pair? env) (module? (car env))) (car env))
        (else (error "bad environment" env))))

(define (cenv-ghil-env env)
  (cond ((not env) (make-ghil-toplevel-env))
        ((module? env) (make-ghil-toplevel-env))
        ((pair? env)
         (if (struct? (cadr env))
             (cadr env)
             (ghil-env-dereify (cadr env))))
        (else (error "bad environment" env))))

(define (cenv-externals env)
  (cond ((not env) '())
        ((module? env) '())
        ((pair? env) (cddr env))
        (else (error "bad environment" env))))

(define (make-cenv module lexicals externals)
  (cons module (cons lexicals externals)))



(define (compile-ghil x e opts)
  (save-module-excursion
   (lambda ()
     (and=> (cenv-module e) set-current-module)
     (call-with-ghil-environment (cenv-ghil-env e) '()
       (lambda (env vars)
         (let ((x (tree-il->scheme
                   (sc-expand x 'c '(compile load eval)))))
           (let ((x (make-ghil-lambda env #f vars #f '()
                                      (translate-1 env #f x)))
                 (cenv (make-cenv (current-module)
                                  (ghil-env-parent env)
                                  (if e (cenv-externals e) '()))))
             (values x cenv cenv))))))))


;;;
;;; Translator
;;;

(define *forbidden-primitives*
  ;; Guile's `procedure->macro' family is evil because it crosses the
  ;; compilation boundary.  One solution might be to evaluate calls to
  ;; `procedure->memoizing-macro' at compilation time, but it may be more
  ;; compicated than that.
  '(procedure->syntax procedure->macro))

;; Looks up transformers relative to the current module at
;; compilation-time. See also the discussion of ghil-lookup in ghil.scm.
;;
;; FIXME shadowing lexicals?
(define (lookup-transformer head retrans)
  (define (module-ref/safe mod sym)
    (and mod
         (and=> (module-variable mod sym) 
                (lambda (var)
                  ;; unbound vars can happen if the module
                  ;; definition forward-declared them
                  (and (variable-bound? var) (variable-ref var))))))
  (let* ((mod (current-module))
         (val (cond
               ((symbol? head) (module-ref/safe mod head))
               ((pmatch head
                  ((@ ,modname ,sym)
                   (module-ref/safe (resolve-interface modname) sym))
                  ((@@ ,modname ,sym)
                   (module-ref/safe (resolve-module modname) sym))
                  (else #f)))
               (else #f))))
    (cond
     ((hashq-ref *translate-table* val))

     ((macro? val)
      (syntax-error #f "unknown kind of macro" head))

     (else #f))))

(define (translate-1 e l x)
  (let ((l (or l (location x))))
    (define (retrans x) (translate-1 e #f x))
    (define (retrans/loc x) (translate-1 e (or (location x) l) x))
    (cond ((pair? x)
           (let ((head (car x)) (tail (cdr x)))
             (cond
              ((lookup-transformer head retrans/loc)
               => (lambda (t) (t e l x)))

              ;; FIXME: lexical/module overrides of forbidden primitives
              ((memq head *forbidden-primitives*)
               (syntax-error l (format #f "`~a' is forbidden" head)
                             (cons head tail)))

              (else
               (let ((tail (map retrans tail)))
                 (or (and (symbol? head)
                          (try-inline-with-env e l (cons head tail)))
                     (make-ghil-call e l (retrans head) tail)))))))

          ((symbol? x)
           (make-ghil-ref e l (ghil-var-for-ref! e x)))

          ;; fixme: non-self-quoting objects like #<foo>
          (else
           (make-ghil-quote e l x)))))

(define (valid-bindings? bindings . it-is-for-do)
  (define (valid-binding? b)
    (pmatch b 
      ((,sym ,var) (guard (symbol? sym)) #t)
      ((,sym ,var ,update) (guard (pair? it-is-for-do) (symbol? sym)) #t)
      (else #f)))
  (and (list? bindings) (and-map valid-binding? bindings)))

(define *translate-table* (make-hash-table))

(define-macro (-> form)
  `(,(symbol-append 'make-ghil- (car form)) e l . ,(cdr form)))

(define-macro (define-scheme-translator sym . clauses)
  `(hashq-set! (@ (language scheme compile-ghil) *translate-table*)
               (module-ref (current-module) ',sym)
               (lambda (e l exp)
                 (define (retrans x)
                   ((@ (language scheme compile-ghil) translate-1)
                    e
                    (or ((@@ (language scheme compile-ghil) location) x) l)
                    x))
                 (define syntax-error (@ (system base compile) syntax-error))
                 (pmatch (cdr exp)
                         ,@clauses
                         ,@(if (assq 'else clauses) '()
                               `((else
                                  (syntax-error l (format #f "bad ~A" ',sym) exp))))))))

(define-scheme-translator quote
  ;; (quote OBJ)
  ((,obj)
   (-> (quote obj))))
    
(define-scheme-translator quasiquote
  ;; (quasiquote OBJ)
  ((,obj)
   (-> (quasiquote (trans-quasiquote e l obj 0)))))

(define-scheme-translator define
  ;; (define NAME VAL)
  ((,name ,val) (guard (symbol? name)
                       (ghil-toplevel-env? (ghil-env-parent e)))
   (-> (define (ghil-var-define! (ghil-env-parent e) name)
               (maybe-name-value! (retrans val) name))))
  ;; (define (NAME FORMALS...) BODY...)
  (((,name . ,formals) . ,body) (guard (symbol? name))
   ;; -> (define NAME (lambda FORMALS BODY...))
   (retrans `(define ,name (lambda ,formals ,@body)))))

(define-scheme-translator set!
  ;; (set! NAME VAL)
  ((,name ,val) (guard (symbol? name))
   (-> (set (ghil-var-for-set! e name) (retrans val))))

  ;; FIXME: Would be nice to verify the values of @ and @@ relative
  ;; to imported modules...
  (((@ ,modname ,name) ,val) (guard (symbol? name)
                                    (list? modname)
                                    (and-map symbol? modname)
                                    (not (ghil-var-is-bound? e '@)))
   (-> (set (ghil-var-at-module! e modname name #t) (retrans val))))

  (((@@ ,modname ,name) ,val) (guard (symbol? name)
                                     (list? modname)
                                     (and-map symbol? modname)
                                     (not (ghil-var-is-bound? e '@@)))
   (-> (set (ghil-var-at-module! e modname name #f) (retrans val))))

  ;; (set! (NAME ARGS...) VAL)
  (((,name . ,args) ,val) (guard (symbol? name))
   ;; -> ((setter NAME) ARGS... VAL)
   (retrans `((setter ,name) . (,@args ,val)))))

(define-scheme-translator if
  ;; (if TEST THEN [ELSE])
  ((,test ,then)
   (-> (if (retrans test) (retrans then) (retrans '(begin)))))
  ((,test ,then ,else)
   (-> (if (retrans test) (retrans then) (retrans else)))))

(define-scheme-translator and
  ;; (and EXPS...)
  (,tail
   (-> (and (map retrans tail)))))

(define-scheme-translator or
  ;; (or EXPS...)
  (,tail
   (-> (or (map retrans tail)))))

(define-scheme-translator begin
  ;; (begin EXPS...)
  (,tail
   (-> (begin (map retrans tail)))))

(define-scheme-translator let
  ;; (let NAME ((SYM VAL) ...) BODY...)
  ((,name ,bindings . ,body) (guard (symbol? name)
                                    (valid-bindings? bindings))
   ;; -> (letrec ((NAME (lambda (SYM...) BODY...))) (NAME VAL...))
   (retrans `(letrec ((,name (lambda ,(map car bindings) ,@body)))
               (,name ,@(map cadr bindings)))))

  ;; (let () BODY...)
  ((() . ,body)
   ;; Note: this differs from `begin'
   (-> (begin (list (trans-body e l body)))))
    
  ;; (let ((SYM VAL) ...) BODY...)
  ((,bindings . ,body) (guard (valid-bindings? bindings))
   (let ((vals (map (lambda (b)
                      (maybe-name-value! (retrans (cadr b)) (car b)))
                    bindings)))
     (call-with-ghil-bindings e (map car bindings)
       (lambda (vars)
         (-> (bind vars vals (trans-body e l body))))))))

(define-scheme-translator let*
  ;; (let* ((SYM VAL) ...) BODY...)
  ((() . ,body)
   (retrans `(let () ,@body)))
  ((((,sym ,val) . ,rest) . ,body) (guard (symbol? sym))
   (retrans `(let ((,sym ,val)) (let* ,rest ,@body)))))

(define-scheme-translator letrec
  ;; (letrec ((SYM VAL) ...) BODY...)
  ((,bindings . ,body) (guard (valid-bindings? bindings))
   (call-with-ghil-bindings e (map car bindings)
     (lambda (vars)
       (let ((vals (map (lambda (b)
                          (maybe-name-value!
                           (retrans (cadr b)) (car b)))
                        bindings)))
         (-> (bind vars vals (trans-body e l body))))))))

(define-scheme-translator cond
  ;; (cond (CLAUSE BODY...) ...)
  (() (retrans '(begin)))
  (((else . ,body)) (retrans `(begin ,@body)))
  (((,test) . ,rest) (retrans `(or ,test (cond ,@rest))))
  (((,test => ,proc) . ,rest)
   ;; FIXME hygiene!
   (retrans `(let ((_t ,test)) (if _t (,proc _t) (cond ,@rest)))))
  (((,test . ,body) . ,rest)
   (retrans `(if ,test (begin ,@body) (cond ,@rest)))))

(define-scheme-translator case
  ;; (case EXP ((KEY...) BODY...) ...)
  ((,exp . ,clauses)
   (retrans
    ;; FIXME hygiene!
    `(let ((_t ,exp))
       ,(let loop ((ls clauses))
          (cond ((null? ls) '(begin))
                ((eq? (caar ls) 'else) `(begin ,@(cdar ls)))
                (else `(if (memv _t ',(caar ls))
                           (begin ,@(cdar ls))
                           ,(loop (cdr ls))))))))))

(define-scheme-translator do
  ;; (do ((SYM VAL [UPDATE]) ...) (TEST RESULT...) BODY...)
  ((,bindings (,test . ,result) . ,body)
   (let ((sym (map car bindings))
         (val (map cadr bindings))
         (update (map cddr bindings)))
     (define (next s x) (if (pair? x) (car x) s))
     (retrans
      ;; FIXME hygiene!
      `(letrec ((_l (lambda ,sym
                      (if ,test
                          (begin ,@result)
                          (begin ,@body
                                 (_l ,@(map next sym update)))))))
         (_l ,@val))))))

(define-scheme-translator lambda
  ;; (lambda FORMALS BODY...)
  ((,formals . ,body)
   (receive (syms rest) (parse-formals formals)
     (call-with-ghil-environment e syms
       (lambda (e vars)
         (receive (meta body) (parse-lambda-meta body)
           (-> (lambda vars rest meta (trans-body e l body)))))))))

(define-scheme-translator delay
  ;; FIXME not hygienic
  ((,expr)
   (retrans `(make-promise (lambda () ,expr)))))

(define-scheme-translator @
  ((,modname ,sym)
   (-> (ref (ghil-var-at-module! e modname sym #t)))))

(define-scheme-translator @@
  ((,modname ,sym)
   (-> (ref (ghil-var-at-module! e modname sym #f)))))

(define *the-compile-toplevel-symbol* 'compile-toplevel)
(define-scheme-translator eval-when
  ((,when . ,body) (guard (list? when) (and-map symbol? when))
   (if (memq 'compile when)
       (primitive-eval `(begin . ,body)))
   (if (memq 'load when)
       (retrans `(begin . ,body))
       (retrans `(begin)))))

(define-scheme-translator apply
  ;; FIXME: not hygienic, relies on @apply not being shadowed
  (,args (retrans `(@apply ,@args))))

;; FIXME: we could add inliners for `list' and `vector'

(define-scheme-translator @apply
  ((,proc ,arg1 . ,args)
   (let ((args (cons (retrans arg1) (map retrans args))))
     (cond ((and (symbol? proc)
                 (not (ghil-var-is-bound? e proc))
                 (and=> (module-variable (current-module) proc)
                        (lambda (var)
                          (and (variable-bound? var)
                               (lookup-apply-transformer (variable-ref var))))))
            ;; that is, a variable, not part of this compilation
            ;; unit, but defined in the toplevel environment, and has
            ;; an apply transformer registered
            => (lambda (t) (t e l args)))
           (else
            (-> (inline 'apply (cons (retrans proc) args))))))))

(define-scheme-translator call-with-values
  ;; FIXME: not hygienic, relies on @call-with-values not being shadowed
  ((,producer ,consumer)
   (retrans `(@call-with-values ,producer ,consumer)))
  (else #f))

(define-scheme-translator @call-with-values
  ((,producer ,consumer)
   (-> (mv-call (retrans producer) (retrans consumer)))))

(define-scheme-translator call-with-current-continuation
  ;; FIXME: not hygienic, relies on @call-with-current-continuation
  ;; not being shadowed
  ((,proc)
   (retrans `(@call-with-current-continuation ,proc)))
  (else #f))

(define-scheme-translator @call-with-current-continuation
  ((,proc)
   (-> (inline 'call/cc (list (retrans proc))))))

(define-scheme-translator receive
  ((,formals ,producer-exp . ,body)
   ;; Lovely, self-referential usage. Not strictly necessary, the
   ;; macro would do the trick; but it's good to test the mv-bind
   ;; code.
   (receive (syms rest) (parse-formals formals)
            (let ((producer (retrans `(lambda () ,producer-exp))))
              (call-with-ghil-bindings e syms
                (lambda (vars)
                  (-> (mv-bind producer vars rest
                               (trans-body e l body)))))))))

(define-scheme-translator values
  ((,x) (retrans x))
  (,args
   (-> (values (map retrans args)))))

(define (lookup-apply-transformer proc)
  (cond ((eq? proc values)
         (lambda (e l args)
           (-> (values* args))))
        (else #f)))

(define (trans-quasiquote e l x level)
  (cond ((not (pair? x)) x)
	((memq (car x) '(unquote unquote-splicing))
	 (let ((l (location x)))
	   (pmatch (cdr x)
	     ((,obj)
              (cond
               ((zero? level) 
                (if (eq? (car x) 'unquote)
                    (-> (unquote (translate-1 e l obj)))
                    (-> (unquote-splicing (translate-1 e l obj)))))
               (else
                (list (car x) (trans-quasiquote e l obj (1- level))))))
	     (else (syntax-error l (format #f "bad ~A" (car x)) x)))))
        ((eq? (car x) 'quasiquote)
	 (let ((l (location x)))
	   (pmatch (cdr x)
	     ((,obj) (list 'quasiquote (trans-quasiquote e l obj (1+ level))))
             (else (syntax-error l (format #f "bad ~A" (car x)) x)))))
	(else (cons (trans-quasiquote e l (car x) level)
		    (trans-quasiquote e l (cdr x) level)))))

(define (trans-body e l body)
  (define (define->binding df)
    (pmatch (cdr df)
      ((,name ,val) (guard (symbol? name)) (list name val))
      (((,name . ,formals) . ,body) (guard (symbol? name))
       (list name `(lambda ,formals ,@body)))
      (else (syntax-error (location df) "bad define" df))))
  ;; main
  (let loop ((ls body) (ds '()))
    (pmatch ls
      (() (syntax-error l "bad body" body))
      (((define . _) . _)
       (loop (cdr ls) (cons (car ls) ds)))
      (else
       (if (null? ds)
           (translate-1 e l `(begin ,@ls))
           (translate-1 e l `(letrec ,(map define->binding ds) ,@ls)))))))

(define (parse-formals formals)
  (cond
   ;; (lambda x ...)
   ((symbol? formals) (values (list formals) #t))
   ;; (lambda (x y z) ...)
   ((list? formals) (values formals #f))
   ;; (lambda (x y . z) ...)
   ((pair? formals)
    (let loop ((l formals) (v '()))
      (if (pair? l)
	  (loop (cdr l) (cons (car l) v))
	  (values (reverse! (cons l v)) #t))))
   (else (syntax-error (location formals) "bad formals" formals))))

(define (parse-lambda-meta body)
  (cond ((or (null? body) (null? (cdr body))) (values '() body))
        ((string? (car body))
         (values `((documentation . ,(car body))) (cdr body)))
        (else (values '() body))))

(define (maybe-name-value! val name)
  (cond
   ((ghil-lambda? val)
    (if (not (assq-ref (ghil-lambda-meta val) 'name))
        (set! (ghil-lambda-meta val)
              (acons 'name name (ghil-lambda-meta val))))))
  val)

(define (location x)
  (and (pair? x)
       (let ((props (source-properties x)))
	 (and (not (null? props))
              props))))
