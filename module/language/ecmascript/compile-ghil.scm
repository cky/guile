;;; ECMAScript for Guile

;; Copyright (C) 2009 Free Software Foundation, Inc.

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

(define-module (language ecmascript compile-ghil)
  #:use-module (language ghil)
  #:use-module (ice-9 receive)
  #:use-module (system base pmatch)
  #:export (compile-ghil))

(eval-case ((load-toplevel) (debug-set! stack 0)))

(define (compile-ghil exp env opts)
  (values
  (call-with-ghil-environment (make-ghil-toplevel-env) '()
    (lambda (env vars)
      (make-ghil-lambda env #f vars #f '() (comp exp env))))
  env))

(define (location x)
  (and (pair? x)
       (let ((props (source-properties x)))
	 (and (not (null? props))
              props))))

(define-macro (@implv e l sym)
  `(make-ghil-ref ,e ,l
                  (ghil-var-at-module! ,e '(language ecmascript impl) ',sym #t)))
(define-macro (@impl e l sym args)
  `(make-ghil-call ,e ,l
                   (@implv ,e ,l ,sym)
                   ,args))

(define (comp x e)
  (let ((l (location x)))
    (define (let1 what proc)
      (call-with-ghil-bindings e '(%tmp)
        (lambda (vars)
          (make-ghil-bind e l vars (list what)
                          (proc (car vars))))))
    (define (begin1 what proc)
      (call-with-ghil-bindings e '(%tmp)
        (lambda (vars)
          (make-ghil-bind e l vars (list what)
                          (make-ghil-begin
                           e l (list (proc (car vars))
                                     (make-ghil-ref e l (car vars))))))))
    (pmatch x
      (null
       ;; FIXME, null doesn't have much relation to EOL...
       (make-ghil-quote e l '()))
      (true
       (make-ghil-quote e l #t))
      (false
       (make-ghil-quote e l #f))
      ((number ,num)
       (make-ghil-quote e l num))
      ((string ,str)
       (make-ghil-quote e l str))
      (this
       (@impl e l get-this '()))
      ((+ ,a)
       (make-ghil-inline e l 'add (list (comp a e) (make-ghil-quote e l 0))))
      ((- ,a)
       (make-ghil-inline e l 'sub (list (make-ghil-quote e l 0) (comp a e))))
      ((~ ,a)
       (@impl e l bitwise-not (list (comp a e))))
      ((! ,a)
       (@impl e l logical-not (list (comp a e))))
      ((+ ,a ,b)
       (make-ghil-inline e l 'add (list (comp a e) (comp b e))))
      ((- ,a ,b)
       (make-ghil-inline e l 'sub (list (comp a e) (comp b e))))
      ((/ ,a ,b)
       (make-ghil-inline e l 'div (list (comp a e) (comp b e))))
      ((* ,a ,b)
       (make-ghil-inline e l 'mul (list (comp a e) (comp b e))))
      ((% ,a ,b)
       (@impl e l mod (list (comp a e) (comp b e))))
      ((<< ,a ,b)
       (@impl e l shift (list (comp a e) (comp b e))))
      ((>> ,a ,b)
       (@impl e l shift (list (comp a e) (comp `(- ,b) e))))
      ((< ,a ,b)
       (make-ghil-inline e l 'lt? (list (comp a e) (comp b e))))
      ((<= ,a ,b)
       (make-ghil-inline e l 'le? (list (comp a e) (comp b e))))
      ((> ,a ,b)
       (make-ghil-inline e l 'gt? (list (comp a e) (comp b e))))
      ((>= ,a ,b)
       (make-ghil-inline e l 'ge? (list (comp a e) (comp b e))))
      ((in ,a ,b)
       (@impl e l has-property? (list (comp a e) (comp b e))))
      ((== ,a ,b)
       (make-ghil-inline e l 'equal? (list (comp a e) (comp b e))))
      ((!= ,a ,b)
       (make-ghil-inline e l 'not
                         (list
                          (make-ghil-inline e l 'equal?
                                            (list (comp a e) (comp b e))))))
      ((=== ,a ,b)
       (make-ghil-inline e l 'eqv? (list (comp a e) (comp b e))))
      ((!== ,a ,b)
       (make-ghil-inline e l 'not
                         (list
                          (make-ghil-inline e l 'eqv?
                                            (list (comp a e) (comp b e))))))
      ((& ,a ,b)
       (@impl e l band (list (comp a e) (comp b e))))
      ((^ ,a ,b)
       (@impl e l bxor (list (comp a e) (comp b e))))
      ((bor ,a ,b)
       (@impl e l bior (list (comp a e) (comp b e))))
      ((and ,a ,b)
       (make-ghil-and e l (list (comp a e) (comp b e))))
      ((or ,a ,b)
       (make-ghil-or e l (list (comp a e) (comp b e))))
      ((if ,test ,then ,else)
       (make-ghil-if e l (@impl e l ->boolean (list (comp test e)))
                     (comp then e) (comp else e)))
      ((if ,test ,then ,else)
       (make-ghil-if e l (@impl e l ->boolean (list (comp test e)))
                     (comp then e) (@implv e l *undefined*)))
      ((postinc (ref ,foo))
       (begin1 (comp `(ref ,foo) e)
               (lambda (var)
                 (make-ghil-set e l (ghil-var-for-set! e foo)
                                (make-ghil-inline
                                 e l 'add (list (make-ghil-ref e l var)
                                                (make-ghil-quote e l 1)))))))
      ((postinc (pref ,obj ,prop))
       (let1 (comp obj e)
             (lambda (objvar)
               (begin1 (@impl e l pget (list (make-ghil-ref e l objvar)
                                             (make-ghil-quote e l prop)))
                       (lambda (tmpvar)
                         (@impl e l pput
                                (list (make-ghil-ref e l objvar)
                                      (make-ghil-quote e l prop)
                                      (make-ghil-inline
                                       e l 'add (list (make-ghil-ref e l tmpvar)
                                                      (make-ghil-quote e l 1))))))))))
      ((postinc (aref ,obj ,prop))
       (let1 (comp obj e)
             (lambda (objvar)
               (let1 (comp prop e)
                     (lambda (propvar)
                       (begin1 (@impl e l pget (list (make-ghil-ref e l objvar)
                                                     (make-ghil-ref e l propvar)))
                               (lambda (tmpvar)
                                 (@impl e l pput
                                        (list (make-ghil-ref e l objvar)
                                              (make-ghil-ref e l propvar)
                                              (make-ghil-inline
                                               e l 'add (list (make-ghil-ref e l tmpvar)
                                                              (make-ghil-quote e l 1))))))))))))
      ((postdec (ref ,foo))
       (begin1 (comp `(ref ,foo) e)
               (lambda (var)
                 (make-ghil-set e l (ghil-var-for-set! e foo)
                                (make-ghil-inline
                                 e l 'sub (list (make-ghil-ref e l var)
                                                (make-ghil-quote e l 1)))))))
      ((postdec (pref ,obj ,prop))
       (let1 (comp obj e)
             (lambda (objvar)
               (begin1 (@impl e l pget (list (make-ghil-ref e l objvar)
                                             (make-ghil-quote e l prop)))
                       (lambda (tmpvar)
                         (@impl e l pput
                                (list (make-ghil-ref e l objvar)
                                      (make-ghil-quote e l prop)
                                      (make-ghil-inline
                                       e l 'sub (list (make-ghil-ref e l tmpvar)
                                                      (make-ghil-quote e l 1))))))))))
      ((postdec (aref ,obj ,prop))
       (let1 (comp obj e)
             (lambda (objvar)
               (let1 (comp prop e)
                     (lambda (propvar)
                       (begin1 (@impl e l pget (list (make-ghil-ref e l objvar)
                                                     (make-ghil-ref e l propvar)))
                               (lambda (tmpvar)
                                 (@impl e l pput
                                        (list (make-ghil-ref e l objvar)
                                              (make-ghil-ref e l propvar)
                                              (make-ghil-inline
                                               e l 'sub (list (make-ghil-ref e l tmpvar)
                                                              (make-ghil-quote e l 1))))))))))))
      ((preinc (ref ,foo))
       (let ((v (ghil-var-for-set! e foo)))
         (make-ghil-begin
          e l (list
               (make-ghil-set e l v (make-ghil-inline
                                     e l 'add (list (make-ghil-ref e l v)
                                                    (make-ghil-quote e l 1))))
               (make-ghil-ref e l v)))))
      ((preinc (pref ,obj ,prop))
       (let1 (comp obj e)
             (lambda (objvar)
               (begin1 (make-ghil-inline
                        e l 'add (list (@impl e l pget (list (make-ghil-ref e l objvar)
                                                             (make-ghil-quote e l prop)))
                                       (make-ghil-quote e l 1)))
                       (lambda (tmpvar)
                         (@impl e l pput (list (make-ghil-ref e l objvar)
                                               (make-ghil-quote e l prop)
                                               (make-ghil-ref e l tmpvar))))))))
      ((preinc (aref ,obj ,prop))
       (let1 (comp obj e)
             (lambda (objvar)
               (let1 (comp prop e)
                     (lambda (propvar)
                       (begin1 (make-ghil-inline
                                e l 'add (list (@impl e l pget (list (make-ghil-ref e l objvar)
                                                                     (make-ghil-ref e l propvar)))
                                               (make-ghil-quote e l 1)))
                               (lambda (tmpvar)
                                 (@impl e l pput (list (make-ghil-ref e l objvar)
                                                       (make-ghil-ref e l propvar)
                                                       (make-ghil-ref e l tmpvar))))))))))
      ((predec (ref ,foo))
       (let ((v (ghil-var-for-set! e foo)))
         (make-ghil-begin
          e l (list
               (make-ghil-set e l v (make-ghil-inline
                                     e l 'sub (list (make-ghil-ref e l v)
                                                    (make-ghil-quote e l 1))))
               (make-ghil-ref e l v)))))
      ((predec (pref ,obj ,prop))
       (let1 (comp obj e)
             (lambda (objvar)
               (begin1 (make-ghil-inline
                        e l 'sub (list (@impl e l pget (list (make-ghil-ref e l objvar)
                                                             (make-ghil-quote e l prop)))
                                       (make-ghil-quote e l 1)))
                       (lambda (tmpvar)
                         (@impl e l pput (list (make-ghil-ref e l objvar)
                                               (make-ghil-quote e l prop)
                                               (make-ghil-ref e l tmpvar))))))))
      ((predec (aref ,obj ,prop))
       (let1 (comp obj e)
             (lambda (objvar)
               (let1 (comp prop e)
                     (lambda (propvar)
                       (begin1 (make-ghil-inline
                                e l 'sub (list (@impl e l pget (list (make-ghil-ref e l objvar)
                                                                     (make-ghil-ref e l propvar)))
                                               (make-ghil-quote e l 1)))
                               (lambda (tmpvar)
                                 (@impl e l pput (list (make-ghil-ref e l objvar)
                                                       (make-ghil-ref e l propvar)
                                                       (make-ghil-ref e l tmpvar))))))))))
      ((ref ,id)
       (make-ghil-ref e l (ghil-var-for-ref! e id)))
      ((var . ,forms)
       (make-ghil-begin e l
                        (map (lambda (form)
                               (pmatch form
                                 ((,x ,y)
                                  (make-ghil-define e l
                                                    (ghil-var-define!
                                                     (ghil-env-parent e) x)
                                                    (comp y e)))
                                 ((,x)
                                  (make-ghil-define e l
                                                    (ghil-var-define!
                                                     (ghil-env-parent e) x)
                                                    (@implv e l *undefined*)))
                                 (else (error "bad var form" form))))
                             forms)))
      ((begin . ,forms)
       (make-ghil-begin e l (map (lambda (x) (comp x e)) forms)))
      ((lambda ,formals ,body)
       (call-with-ghil-environment e '(%args)
         (lambda (env vars)
           (make-ghil-lambda env l vars #t '()
                             (comp-body env l body formals '%args)))))
      ((call/this ,obj ,prop ,args)
       ;; FIXME: only evaluate "obj" once
       (@impl e l call/this*
              (list obj (make-ghil-lambda
                         e l '() #f '()
                         (make-ghil-call e l (@impl e l pget (list obj prop))
                                         args)))))
      ((call (pref ,obj ,prop) ,args)
       (comp `(call/this ,(comp obj e) ,(make-ghil-quote e l prop)
                         ,(map (lambda (x) (comp x e)) args))
             e))
      ((call (aref ,obj ,prop) ,args)
       (comp `(call/this ,(comp obj e) ,(comp prop e)
                         ,(map (lambda (x) (comp x e)) args))
             e))
      ((call ,proc ,args)
       (make-ghil-call e l (comp proc e) (map (lambda (x) (comp x e)) args)))
      ((return ,expr)
       (make-ghil-inline e l 'return (list (comp expr e))))
      ((array . ,args)
       (@impl e l new-array (map (lambda (x) (comp x e)) args)))
      ((object . ,args)
       (@impl e l new-object
              (map (lambda (x)
                     (pmatch x
                       ((,prop ,val)
                        (make-ghil-inline e l 'cons
                                          (list (make-ghil-quote e l prop)
                                                (comp val e))))
                       (else
                        (error "bad prop-val pair" x))))
                   args)))
      ((pref ,obj ,prop)
       (@impl e l pget (list (comp obj e) (make-ghil-quote e l prop))))
      ((aref ,obj ,index)
       (@impl e l pget (list (comp obj e) (comp index e))))
      ((= (ref ,name) ,val)
       (let ((v (ghil-var-for-set! e name)))
         (make-ghil-begin e l
                          (list (make-ghil-set e l v (comp val e))
                                (make-ghil-ref e l v)))))
      ((= (pref ,obj ,prop) ,val)
       (@impl e l pput (list (comp obj e) (make-ghil-quote e l prop) (comp val e))))
      ((= (aref ,obj ,prop) ,val)
       (@impl e l pput (list (comp obj e) (comp prop e) (comp val e))))
      ((+= ,what ,val)
       (comp `(= ,what (+ ,what ,val)) e))
      ((-= ,what ,val)
       (comp `(= ,what (- ,what ,val)) e))
      ((/= ,what ,val)
       (comp `(= ,what (/ ,what ,val)) e))
      ((*= ,what ,val)
       (comp `(= ,what (* ,what ,val)) e))
      ((%= ,what ,val)
       (comp `(= ,what (% ,what ,val)) e))
      ((>>= ,what ,val)
       (comp `(= ,what (>> ,what ,val)) e))
      ((<<= ,what ,val)
       (comp `(= ,what (<< ,what ,val)) e))
      ((>>>= ,what ,val)
       (comp `(= ,what (>>> ,what ,val)) e))
      ((&= ,what ,val)
       (comp `(= ,what (& ,what ,val)) e))
      ((bor= ,what ,val)
       (comp `(= ,what (bor ,what ,val)) e))
      ((^= ,what ,val)
       (comp `(= ,what (^ ,what ,val)) e))
      ((new ,what ,args)
       (@impl e l new (map (lambda (x) (comp x e)) (cons what args))))
      ((delete (pref ,obj ,prop))
       (@impl e l pdel (list (comp obj e) (make-ghil-quote e l prop))))
      ((delete (aref ,obj ,prop))
       (@impl e l pdel (list (comp obj e) (comp prop e))))
      ((void ,expr)
       (make-ghil-begin e l (list (comp expr e) (@implv e l *undefined*))))
      ((typeof ,expr)
       (@impl e l typeof (list (comp expr e))))
      ((do ,statement ,test)
       (call-with-ghil-bindings e '(%loop %continue)
         (lambda (vars)
           (make-ghil-bind
            e l vars (list
                      (call-with-ghil-environment e '()
                        (lambda (e _)
                          (make-ghil-lambda
                           e l '() #f '()
                           (make-ghil-begin
                            e l (list (comp statement e)
                                      (make-ghil-call e l (make-ghil-ref
                                                           e l (ghil-var-for-ref! e '%continue))
                                                      '()))))))
                      (call-with-ghil-environment e '()
                        (lambda (e _)
                          (make-ghil-lambda
                           e l '() #f '()
                           (make-ghil-if e l (comp test e)
                                         (make-ghil-call e l (make-ghil-ref
                                                              e l (ghil-var-for-ref! e '%loop))
                                                         '())
                                         (@implv e l *undefined*))))))
            (make-ghil-call e l (make-ghil-ref e l (car vars)) '())))))
      ((while ,test ,statement)
       (call-with-ghil-bindings e '(%continue)
         (lambda (vars)
           (make-ghil-begin
            e l
            (list (make-ghil-set
                   e l (car vars)
                   (call-with-ghil-environment e '()
                     (lambda (e _)
                       (make-ghil-lambda
                        e l '() #f '()
                        (make-ghil-if
                         e l (comp test e)
                         (make-ghil-begin
                          e l (list (comp statement e)
                                    (make-ghil-call e l (make-ghil-ref
                                                         e l (ghil-var-for-ref! e '%continue))
                                                    '())))
                         (@implv e l *undefined*))))))
                  (make-ghil-call e l (make-ghil-ref e l (car vars)) '()))))))
      ((for ,init ,test ,inc ,statement)
       (call-with-ghil-bindings e '(%continue)
         (lambda (vars)
           (make-ghil-begin
            e l
            (list (comp (or init '(begin)) e)
                  (make-ghil-set
                   e l (car vars)
                   (call-with-ghil-environment e '()
                     (lambda (e _)
                       (make-ghil-lambda
                        e l '() #f '()
                        (make-ghil-if
                         e l (comp (or test 'true) e)
                         (make-ghil-begin
                          e l (list (comp (or inc '(begin)) e)
                                    (comp statement e)
                                    (make-ghil-call e l (make-ghil-ref
                                                         e l (ghil-var-for-ref! e '%continue))
                                                    '())))
                         (@implv e l *undefined*))))))
                  (make-ghil-call e l (make-ghil-ref e l (car vars)) '()))))))
      ((break)
       (let ((var (ghil-var-for-ref! e '%continue)))
         (if (and (ghil-env? (ghil-var-env var))
                  (eq? (ghil-var-env var) (ghil-env-parent e)))
             (make-ghil-inline e l 'return (@implv e l *undefined*))
             (error "bad break, yo"))))
      ((continue)
       (let ((var (ghil-var-for-ref! e '%continue)))
         (if (and (ghil-env? (ghil-var-env var))
                  (eq? (ghil-var-env var) (ghil-env-parent e)))
             (make-ghil-inline e l 'goto/args (list (make-ghil-ref e l var)))
             (error "bad continue, yo"))))
      ((block ,x)
       (comp x e))
      (else
       (error "compilation not yet implemented:" x)))))

(define (comp-body env loc body formals %args)
  (define (process)
    (let lp ((in body) (out '()) (rvars (reverse formals)))
      (pmatch in
        (((var (,x) . ,morevars) . ,rest)
         (lp `((var . ,morevars) . ,rest)
             out
             (if (memq x rvars) rvars (cons x rvars))))
        (((var (,x ,y) . ,morevars) . ,rest)
         (lp `((var . ,morevars) . ,rest)
             `((= (ref ,x) ,y) . ,out)
             (if (memq x rvars) rvars (cons x rvars))))
        (((var) . ,rest)
         (lp rest out rvars))
        ((,x . ,rest) (guard (and (pair? x) (eq? (car x) 'lambda)))
         (lp rest
             (cons x out)
             rvars))
        ((,x . ,rest) (guard (pair? x))
         (receive (sub-out rvars)
             (lp x '() rvars)
           (lp rest
               (cons sub-out out)
               rvars)))
        ((,x . ,rest)
         (lp rest
             (cons x out)
             rvars))
        (()
         (values (reverse! out)
                 rvars)))))
  (receive (out rvars)
      (process)
    (call-with-ghil-bindings env (reverse rvars)
      (lambda (vars)
        (let ((%argv (assq-ref (ghil-env-table env) %args)))
          (make-ghil-begin
           env loc
           `(,@(map (lambda (f)
                      (make-ghil-if
                       env loc
                       (make-ghil-inline
                        env loc 'null?
                        (list (make-ghil-ref env loc %argv)))
                       (make-ghil-begin env loc '())
                       (make-ghil-begin
                        env loc
                        (list (make-ghil-set
                               env loc
                               (ghil-var-for-ref! env f)
                               (make-ghil-inline
                                env loc 'car
                                (list (make-ghil-ref env loc %argv))))
                              (make-ghil-set
                               env loc %argv
                               (make-ghil-inline
                                env loc 'cdr
                                (list (make-ghil-ref env loc %argv))))))))
                    formals)
             ;; fixme: here check for too many args
             ,(comp out env))))))))
