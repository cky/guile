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

(define-macro (-> form)
  `(,(symbol-append 'make-ghil- (car form)) e l . ,(cdr form)))

(define-macro (@implv sym)
  `(-> (ref (ghil-var-at-module! e '(language ecmascript impl) ',sym #t))))
(define-macro (@impl sym args)
  `(-> (call (@implv ,sym) ,args)))

(define (compile-ghil exp env opts)
  (values
   (call-with-ghil-environment (make-ghil-toplevel-env) '()
     (lambda (e vars)
       (let ((l #f))
         (-> (lambda vars #f '()
                     (-> (begin (list (@impl js-init '())
                                      (comp exp e)))))))))
   env))

(define (location x)
  (and (pair? x)
       (let ((props (source-properties x)))
	 (and (not (null? props))
              props))))

;; The purpose, you ask? To avoid non-tail recursion when expanding a
;; long pmatch sequence.
(define-macro (ormatch x . clauses)
  (let ((X (gensym)))
    `(let ((,X ,x))
       (or ,@(map (lambda (c)
                    (if (eq? (car c) 'else)
                        `(begin . ,(cdr c))
                        `(pmatch ,X ,c (else #f))))
                  clauses)))))

(define (comp x e)
  (let ((l (location x)))
    (define (let1 what proc)
      (call-with-ghil-bindings e '(%tmp)
        (lambda (vars)
          (-> (bind vars (list what)
                    (proc (car vars)))))))
    (define (begin1 what proc)
      (call-with-ghil-bindings e '(%tmp)
        (lambda (vars)
          (-> (bind vars (list what)
                    (-> (begin (list (proc (car vars))
                                     (-> (ref (car vars)))))))))))
    (ormatch x
      (null
       ;; FIXME, null doesn't have much relation to EOL...
       (-> (quote '())))
      (true
       (-> (quote #t)))
      (false
       (-> (quote #f)))
      ((number ,num)
       (-> (quote num)))
      ((string ,str)
       (-> (quote str)))
      (this
       (@impl get-this '()))
      ((+ ,a)
       (-> (inline 'add
                   (list (@impl ->number (list (comp a e)))
                         (-> (quote 0))))))
      ((- ,a)
       (-> (inline 'sub (list (-> (quote 0)) (comp a e)))))
      ((~ ,a)
       (@impl bitwise-not (list (comp a e))))
      ((! ,a)
       (@impl logical-not (list (comp a e))))
      ((+ ,a ,b)
       (-> (inline 'add (list (comp a e) (comp b e)))))
      ((- ,a ,b)
       (-> (inline 'sub (list (comp a e) (comp b e)))))
      ((/ ,a ,b)
       (-> (inline 'div (list (comp a e) (comp b e)))))
      ((* ,a ,b)
       (-> (inline 'mul (list (comp a e) (comp b e)))))
      ((% ,a ,b)
       (@impl mod (list (comp a e) (comp b e))))
      ((<< ,a ,b)
       (@impl shift (list (comp a e) (comp b e))))
      ((>> ,a ,b)
       (@impl shift (list (comp a e) (comp `(- ,b) e))))
      ((< ,a ,b)
       (-> (inline 'lt? (list (comp a e) (comp b e)))))
      ((<= ,a ,b)
       (-> (inline 'le? (list (comp a e) (comp b e)))))
      ((> ,a ,b)
       (-> (inline 'gt? (list (comp a e) (comp b e)))))
      ((>= ,a ,b)
       (-> (inline 'ge? (list (comp a e) (comp b e)))))
      ((in ,a ,b)
       (@impl has-property? (list (comp a e) (comp b e))))
      ((== ,a ,b)
       (-> (inline 'equal? (list (comp a e) (comp b e)))))
      ((!= ,a ,b)
       (-> (inline 'not
                   (list (-> (inline 'equal?
                                     (list (comp a e) (comp b e))))))))
      ((=== ,a ,b)
       (-> (inline 'eqv? (list (comp a e) (comp b e)))))
      ((!== ,a ,b)
       (-> (inline 'not
                   (list (-> (inline 'eqv?
                                     (list (comp a e) (comp b e))))))))
      ((& ,a ,b)
       (@impl band (list (comp a e) (comp b e))))
      ((^ ,a ,b)
       (@impl bxor (list (comp a e) (comp b e))))
      ((bor ,a ,b)
       (@impl bior (list (comp a e) (comp b e))))
      ((and ,a ,b)
       (-> (and (list (comp a e) (comp b e)))))
      ((or ,a ,b)
       (-> (or (list (comp a e) (comp b e)))))
      ((if ,test ,then ,else)
       (-> (if (@impl ->boolean (list (comp test e)))
               (comp then e)
               (comp else e))))
      ((if ,test ,then ,else)
       (-> (if (@impl ->boolean (list (comp test e)))
               (comp then e)
               (@implv *undefined*))))
      ((postinc (ref ,foo))
       (begin1 (comp `(ref ,foo) e)
               (lambda (var)
                 (-> (set (ghil-var-for-set! e foo)
                          (-> (inline 'add
                                      (list (-> (ref var))
                                            (-> (quote 1))))))))))
      ((postinc (pref ,obj ,prop))
       (let1 (comp obj e)
             (lambda (objvar)
               (begin1 (@impl pget
                              (list (-> (ref objvar))
                                    (-> (quote prop))))
                       (lambda (tmpvar)
                         (@impl pput
                                (list (-> (ref objvar))
                                      (-> (quote prop))
                                      (-> (inline 'add
                                                  (list (-> (ref tmpvar))
                                                        (-> (quote 1))))))))))))
      ((postinc (aref ,obj ,prop))
       (let1 (comp obj e)
             (lambda (objvar)
               (let1 (comp prop e)
                     (lambda (propvar)
                       (begin1 (@impl pget
                                      (list (-> (ref objvar))
                                            (-> (ref propvar))))
                               (lambda (tmpvar)
                                 (@impl pput
                                        (list (-> (ref objvar))
                                              (-> (ref propvar))
                                              (-> (inline 'add
                                                          (list (-> (ref tmpvar))
                                                                (-> (quote 1))))))))))))))
      ((postdec (ref ,foo))
       (begin1 (comp `(ref ,foo) e)
               (lambda (var)
                 (-> (set (ghil-var-for-set! e foo)
                          (-> (inline 'sub
                                      (list (-> (ref var))
                                            (-> (quote 1))))))))))
      ((postdec (pref ,obj ,prop))
       (let1 (comp obj e)
             (lambda (objvar)
               (begin1 (@impl pget
                              (list (-> (ref objvar))
                                    (-> (quote prop))))
                       (lambda (tmpvar)
                         (@impl pput
                                (list (-> (ref objvar))
                                      (-> (quote prop))
                                      (-> (inline 'sub
                                                  (list (-> (ref tmpvar))
                                                        (-> (quote 1))))))))))))
      ((postdec (aref ,obj ,prop))
       (let1 (comp obj e)
             (lambda (objvar)
               (let1 (comp prop e)
                     (lambda (propvar)
                       (begin1 (@impl pget
                                      (list (-> (ref objvar))
                                            (-> (ref propvar))))
                               (lambda (tmpvar)
                                 (@impl pput
                                        (list (-> (ref objvar))
                                              (-> (ref propvar))
                                              (-> (inline
                                                   'sub (list (-> (ref tmpvar))
                                                              (-> (quote 1))))))))))))))
      ((preinc (ref ,foo))
       (let ((v (ghil-var-for-set! e foo)))
         (-> (begin
               (list
                (-> (set v
                         (-> (inline 'add
                                     (list (-> (ref v))
                                           (-> (quote 1)))))))
                (-> (ref v)))))))
      ((preinc (pref ,obj ,prop))
       (let1 (comp obj e)
             (lambda (objvar)
               (begin1 (-> (inline 'add
                                   (list (@impl pget
                                                (list (-> (ref objvar))
                                                      (-> (quote prop))))
                                         (-> (quote 1)))))
                       (lambda (tmpvar)
                         (@impl pput (list (-> (ref objvar))
                                           (-> (quote prop))
                                           (-> (ref tmpvar)))))))))
      ((preinc (aref ,obj ,prop))
       (let1 (comp obj e)
             (lambda (objvar)
               (let1 (comp prop e)
                     (lambda (propvar)
                       (begin1 (-> (inline 'add
                                           (list (@impl pget
                                                        (list (-> (ref objvar))
                                                              (-> (ref propvar))))
                                                 (-> (quote 1)))))
                               (lambda (tmpvar)
                                 (@impl pput
                                        (list (-> (ref objvar))
                                              (-> (ref propvar))
                                              (-> (ref tmpvar)))))))))))
      ((predec (ref ,foo))
       (let ((v (ghil-var-for-set! e foo)))
         (-> (begin
               (list
                (-> (set v
                         (-> (inline 'sub
                                     (list (-> (ref v))
                                           (-> (quote 1)))))))
                (-> (ref v)))))))
      ((predec (pref ,obj ,prop))
       (let1 (comp obj e)
             (lambda (objvar)
               (begin1 (-> (inline 'sub
                                   (list (@impl pget
                                                (list (-> (ref objvar))
                                                      (-> (quote prop))))
                                         (-> (quote 1)))))
                       (lambda (tmpvar)
                         (@impl pput
                                (list (-> (ref objvar))
                                      (-> (quote prop))
                                      (-> (ref tmpvar)))))))))
      ((predec (aref ,obj ,prop))
       (let1 (comp obj e)
             (lambda (objvar)
               (let1 (comp prop e)
                     (lambda (propvar)
                       (begin1 (-> (inline 'sub
                                           (list (@impl pget
                                                        (list (-> (ref objvar))
                                                              (-> (ref propvar))))
                                                 (-> (quote 1)))))
                               (lambda (tmpvar)
                                 (@impl pput
                                        (list (-> (ref objvar))
                                              (-> (ref propvar))
                                              (-> (ref tmpvar)))))))))))
      ((ref ,id)
       (-> (ref (ghil-var-for-ref! e id))))
      ((var . ,forms)
       (-> (begin
             (map (lambda (form)
                    (pmatch form
                      ((,x ,y)
                       (-> (define (ghil-var-define! (ghil-env-parent e) x)
                                   (comp y e))))
                      ((,x)
                       (-> (define (ghil-var-define! (ghil-env-parent e) x)
                                   (@implv *undefined*))))
                      (else (error "bad var form" form))))
                  forms))))
      ((begin . ,forms)
       (-> (begin
             (map (lambda (x) (comp x e)) forms))))
      ((lambda ,formals ,body)
       (call-with-ghil-environment e '(%args)
         (lambda (e vars)
           (-> (lambda vars #t '()
                       (comp-body env l body formals '%args))))))
      ((call/this ,obj ,prop ,args)
       (@impl call/this*
              (list obj
                    (-> (lambda '() #f '()
                                (-> (call (@impl pget (list obj prop))
                                          args)))))))
      ((call (pref ,obj ,prop) ,args)
       (comp `(call/this ,(comp obj e)
                         ,(-> (quote prop))
                         ,(map (lambda (x) (comp x e)) args))
             e))
      ((call (aref ,obj ,prop) ,args)
       (comp `(call/this ,(comp obj e)
                         ,(comp prop e)
                         ,(map (lambda (x) (comp x e)) args))
             e))
      ((call ,proc ,args)
       (-> (call (comp proc e)
                 (map (lambda (x) (comp x e)) args))))
      ((return ,expr)
       (-> (inline 'return
                   (list (comp expr e)))))
      ((array . ,args)
       (@impl new-array
              (map (lambda (x) (comp x e)) args)))
      ((object . ,args)
       (@impl new-object
              (map (lambda (x)
                     (pmatch x
                       ((,prop ,val)
                        (-> (inline 'cons
                                    (list (-> (quote prop))
                                          (comp val e)))))
                       (else
                        (error "bad prop-val pair" x))))
                   args)))
      ((pref ,obj ,prop)
       (@impl pget
              (list (comp obj e)
                    (-> (quote prop)))))
      ((aref ,obj ,index)
       (@impl pget
              (list (comp obj e)
                    (comp index e))))
      ((= (ref ,name) ,val)
       (let ((v (ghil-var-for-set! e name)))
         (-> (begin
               (list (-> (set v (comp val e)))
                     (-> (ref v)))))))
      ((= (pref ,obj ,prop) ,val)
       (@impl pput
              (list (comp obj e)
                    (-> (quote prop))
                    (comp val e))))
      ((= (aref ,obj ,prop) ,val)
       (@impl pput
              (list (comp obj e)
                    (comp prop e)
                    (comp val e))))
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
       (@impl new
              (map (lambda (x) (comp x e))
                   (cons what args))))
      ((delete (pref ,obj ,prop))
       (@impl pdel
              (list (comp obj e)
                    (-> (quote prop)))))
      ((delete (aref ,obj ,prop))
       (@impl pdel
              (list (comp obj e)
                    (comp prop e))))
      ((void ,expr)
       (-> (begin
             (list (comp expr e)
                   (@implv *undefined*)))))
      ((typeof ,expr)
       (@impl typeof
              (list (comp expr e))))
      ((do ,statement ,test)
       (call-with-ghil-bindings e '(%loop %continue)
         (lambda (vars)
           (-> (bind vars
                     (list (call-with-ghil-environment e '()
                             (lambda (e _)
                               (-> (lambda '() #f '()
                                     (-> (begin
                                           (list (comp statement e)
                                                 (-> (call
                                                      (-> (ref (ghil-var-for-ref! e '%continue)))
                                                      '())))))))))
                           (call-with-ghil-environment e '()
                             (lambda (e _)
                               (-> (lambda '() #f '()
                                     (-> (if (@impl ->boolean (list (comp test e)))
                                             (-> (call
                                                  (-> (ref (ghil-var-for-ref! e '%loop)))
                                                  '()))
                                             (@implv *undefined*))))))))
                     (-> (call (-> (ref (car vars))) '())))))))
      ((while ,test ,statement)
       (call-with-ghil-bindings e '(%continue)
         (lambda (vars)
           (-> (begin
                 (list
                  (-> (set (car vars)
                           (call-with-ghil-environment e '()
                             (lambda (e _)
                               (-> (lambda '() #f '()
                                     (-> (if (@impl ->boolean (list (comp test e)))
                                             (-> (begin
                                                   (list (comp statement e)
                                                         (-> (call
                                                              (-> (ref (ghil-var-for-ref! e '%continue)))
                                                              '())))))
                                             (@implv *undefined*)))))))))
                  (-> (call (-> (ref (car vars))) '()))))))))
      ((for ,init ,test ,inc ,statement)
       (call-with-ghil-bindings e '(%continue)
         (lambda (vars)
           (-> (begin
                 (list
                  (comp (or init '(begin)) e)
                  (-> (set (car vars)
                           (call-with-ghil-environment e '()
                             (lambda (e _)
                               (-> (lambda '() #f '()
                                     (-> (if (if test
                                                 (@impl ->boolean (list (comp test e)))
                                                 (comp 'true e))
                                             (-> (begin
                                                   (list (comp statement e)
                                                         (comp (or inc '(begin)) e)
                                                         (-> (call
                                                              (-> (ref (ghil-var-for-ref! e '%continue)))
                                                              '())))))
                                             (@implv *undefined*)))))))))
                  (-> (call (-> (ref (car vars))) '()))))))))
      ((for-in ,var ,object ,statement)
       (call-with-ghil-bindings e '(%continue %enum)
         (lambda (vars)
           (-> (begin
                 (list
                  (-> (set (car vars)
                           (call-with-ghil-environment e '()
                             (lambda (e _)
                               (-> (lambda '() #f '()
                                     (-> (if (@impl ->boolean
                                                    (list (@impl pget
                                                                 (list (-> (ref (ghil-var-for-ref! e '%enum)))
                                                                       (-> (quote 'length))))))
                                             (-> (begin
                                                   (list
                                                    (comp `(= ,var (call/this ,(-> (ref (ghil-var-for-ref! e '%enum)))
                                                                              ,(-> (quote 'pop))
                                                                              ()))
                                                          e)
                                                    (comp statement e)
                                                    (-> (call (-> (ref (ghil-var-for-ref! e '%continue)))
                                                              '())))))
                                             (@implv *undefined*)))))))))
                  (-> (set (cadr vars)
                           (@impl make-enumerator (list (comp object e)))))
                  (-> (call (-> (ref (car vars))) '()))))))))
      ((break)
       (let ((var (ghil-var-for-ref! e '%continue)))
         (if (and (ghil-env? (ghil-var-env var))
                  (eq? (ghil-var-env var) (ghil-env-parent e)))
             (-> (inline 'return (@implv *undefined*)))
             (error "bad break, yo"))))
      ((continue)
       (let ((var (ghil-var-for-ref! e '%continue)))
         (if (and (ghil-env? (ghil-var-env var))
                  (eq? (ghil-var-env var) (ghil-env-parent e)))
             (-> (inline 'goto/args (list (-> (ref var)))))
             (error "bad continue, yo"))))
      ((block ,x)
       (comp x e))
      (else
       (error "compilation not yet implemented:" x)))))

(define (comp-body e l body formals %args)
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
    (call-with-ghil-bindings e (reverse rvars)
      (lambda (vars)
        (let ((%argv (assq-ref (ghil-env-table e) %args)))
          (-> (begin
                `(,@(map
                     (lambda (f)
                       (-> (if (-> (inline 'null?
                                           (list (-> (ref %argv)))))
                               (-> (begin '()))
                               (-> (begin
                                     (list (-> (set (ghil-var-for-ref! e f)
                                                    (-> (inline 'car
                                                                (list (-> (ref %argv)))))))
                                           (-> (set %argv
                                                    (-> (inline 'cdr
                                                                (list (-> (ref %argv)))))))))))))
                     formals)
                  ;; fixme: here check for too many args
                  ,(comp out e)))))))))
