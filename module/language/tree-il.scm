;;;; 	Copyright (C) 2009 Free Software Foundation, Inc.
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


(define-module (language tree-il)
  #:use-module (system base pmatch)
  #:use-module (system base syntax)
  #:export (tree-il-src

            <void> void? make-void void-src
            <const> const? make-const const-src const-exp
            <primitive-ref> primitive-ref? make-primitive-ref primitive-ref-src primitive-ref-name
            <lexical-ref> lexical-ref? make-lexical-ref lexical-ref-src lexical-ref-name lexical-ref-gensym
            <lexical-set> lexical-set? make-lexical-set lexical-set-src lexical-set-name lexical-set-gensym lexical-set-exp
            <module-ref> module-ref? make-module-ref module-ref-src module-ref-mod module-ref-name module-ref-public?
            <module-set> module-set? make-module-set module-set-src module-set-mod module-set-name module-set-public? module-set-exp
            <toplevel-ref> toplevel-ref? make-toplevel-ref toplevel-ref-src toplevel-ref-name
            <toplevel-set> toplevel-set? make-toplevel-set toplevel-set-src toplevel-set-name toplevel-set-exp
            <toplevel-define> toplevel-define? make-toplevel-define toplevel-define-src toplevel-define-name toplevel-define-exp
            <conditional> conditional? make-conditional conditional-src conditional-test conditional-then conditional-else
            <application> application? make-application application-src application-proc application-args
            <sequence> sequence? make-sequence sequence-src sequence-exps
            <lambda> lambda? make-lambda lambda-src lambda-names lambda-vars lambda-meta lambda-body
            <let> let? make-let let-src let-names let-vars let-vals let-body
            <letrec> letrec? make-letrec letrec-src letrec-names letrec-vars letrec-vals letrec-body
            <let-values> let-values? make-let-values let-values-src let-values-names let-values-vars let-values-exp let-values-body
            
            parse-tree-il
            unparse-tree-il
            tree-il->scheme

            post-order!
            pre-order!))

(define-type (<tree-il> #:common-slots (src))
  (<void>)
  (<const> exp)
  (<primitive-ref> name)
  (<lexical-ref> name gensym)
  (<lexical-set> name gensym exp)
  (<module-ref> mod name public?)
  (<module-set> mod name public? exp)
  (<toplevel-ref> name)
  (<toplevel-set> name exp)
  (<toplevel-define> name exp)
  (<conditional> test then else)
  (<application> proc args)
  (<sequence> exps)
  (<lambda> names vars meta body)
  (<let> names vars vals body)
  (<letrec> names vars vals body)
  (<let-values> names vars exp body))
  


(define (location x)
  (and (pair? x)
       (let ((props (source-properties x)))
	 (and (pair? props) props))))

(define (parse-tree-il exp)
  (let ((loc (location exp))
        (retrans (lambda (x) (parse-tree-il x))))
    (pmatch exp
     ((void)
      (make-void loc))

     ((apply ,proc . ,args)
      (make-application loc (retrans proc) (map retrans args)))

     ((if ,test ,then ,else)
      (make-conditional loc (retrans test) (retrans then) (retrans else)))

     ((primitive ,name) (guard (symbol? name))
      (make-primitive-ref loc name))

     ((lexical ,name) (guard (symbol? name))
      (make-lexical-ref loc name name))

     ((lexical ,name ,sym) (guard (symbol? name) (symbol? sym))
      (make-lexical-ref loc name sym))

     ((set! (lexical ,name ,sym) ,exp) (guard (symbol? name) (symbol? sym))
      (make-lexical-set loc name sym (retrans exp)))

     ((@ ,mod ,name) (guard (and-map symbol? mod) (symbol? name))
      (make-module-ref loc mod name #t))

     ((set! (@ ,mod ,name) ,exp) (guard (and-map symbol? mod) (symbol? name))
      (make-module-set loc mod name #t (retrans exp)))

     ((@@ ,mod ,name) (guard (and-map symbol? mod) (symbol? name))
      (make-module-ref loc mod name #f))

     ((set! (@@ ,mod ,name) ,exp) (guard (and-map symbol? mod) (symbol? name))
      (make-module-set loc mod name #f (retrans exp)))

     ((toplevel ,name) (guard (symbol? name))
      (make-toplevel-ref loc name))

     ((set! (toplevel ,name) ,exp) (guard (symbol? name))
      (make-toplevel-set loc name (retrans exp)))

     ((define ,name ,exp) (guard (symbol? name))
      (make-toplevel-define loc name (retrans exp)))

     ((lambda ,names ,vars ,exp)
      (make-lambda loc names vars '() (retrans exp)))

     ((lambda ,names ,vars ,meta ,exp)
      (make-lambda loc names vars meta (retrans exp)))

     ((const ,exp)
      (make-const loc exp))

     ((begin . ,exps)
      (make-sequence loc (map retrans exps)))

     ((let ,names ,vars ,vals ,body)
      (make-let loc names vars (map retrans vals) (retrans body)))

     ((letrec ,names ,vars ,vals ,body)
      (make-letrec loc names vars (map retrans vals) (retrans body)))

     ((let-values ,names ,vars ,exp ,body)
      (make-let-values loc names vars (retrans exp) (retrans body)))

     (else
      (error "unrecognized tree-il" exp)))))

(define (unparse-tree-il tree-il)
  (record-case tree-il
    ((<void>)
     '(void))

    ((<application> proc args)
     `(apply ,(unparse-tree-il proc) ,@(map unparse-tree-il args)))

    ((<conditional> test then else)
     `(if ,(unparse-tree-il test) ,(unparse-tree-il then) ,(unparse-tree-il else)))

    ((<primitive-ref> name)
     `(primitive ,name))

    ((<lexical-ref> name gensym)
     `(lexical ,name ,gensym))

    ((<lexical-set> name gensym exp)
     `(set! (lexical ,name ,gensym) ,(unparse-tree-il exp)))

    ((<module-ref> mod name public?)
     `(,(if public? '@ '@@) ,mod ,name))

    ((<module-set> mod name public? exp)
     `(set! (,(if public? '@ '@@) ,mod ,name) ,(unparse-tree-il exp)))

    ((<toplevel-ref> name)
     `(toplevel ,name))

    ((<toplevel-set> name exp)
     `(set! (toplevel ,name) ,(unparse-tree-il exp)))

    ((<toplevel-define> name exp)
     `(define ,name ,(unparse-tree-il exp)))

    ((<lambda> names vars meta body)
     `(lambda ,names ,vars ,meta ,(unparse-tree-il body)))

    ((<const> exp)
     `(const ,exp))

    ((<sequence> exps)
     `(begin ,@(map unparse-tree-il exps)))

    ((<let> names vars vals body)
     `(let ,names ,vars ,(map unparse-tree-il vals) ,(unparse-tree-il body)))

    ((<letrec> names vars vals body)
     `(letrec ,names ,vars ,(map unparse-tree-il vals) ,(unparse-tree-il body)))

    ((<let-values> names vars exp body)
     `(let-values ,names ,vars ,(unparse-tree-il exp) ,(unparse-tree-il body)))))

(define (tree-il->scheme e)
  (record-case e
    ((<void>)
     '(if #f #f))

    ((<application> proc args)
     `(,(tree-il->scheme proc) ,@(map tree-il->scheme args)))

    ((<conditional> test then else)
     (if (void? else)
         `(if ,(tree-il->scheme test) ,(tree-il->scheme then))
         `(if ,(tree-il->scheme test) ,(tree-il->scheme then) ,(tree-il->scheme else))))

    ((<primitive-ref> name)
     name)
    
    ((<lexical-ref> name gensym)
     gensym)
    
    ((<lexical-set> name gensym exp)
     `(set! ,gensym ,(tree-il->scheme exp)))
    
    ((<module-ref> mod name public?)
     `(,(if public? '@ '@@) ,mod ,name))
    
    ((<module-set> mod name public? exp)
     `(set! (,(if public? '@ '@@) ,mod ,name) ,(tree-il->scheme exp)))
    
    ((<toplevel-ref> name)
     name)
    
    ((<toplevel-set> name exp)
     `(set! ,name ,(tree-il->scheme exp)))
    
    ((<toplevel-define> name exp)
     `(define ,name ,(tree-il->scheme exp)))
    
    ((<lambda> vars meta body)
     `(lambda ,vars
        ,@(cond ((assq-ref meta 'documentation) => list) (else '()))
        ,(tree-il->scheme body)))
    
    ((<const> exp)
     (if (and (self-evaluating? exp) (not (vector? exp)))
         exp
         (list 'quote exp)))
    
    ((<sequence> exps)
     `(begin ,@(map tree-il->scheme exps)))
    
    ((<let> vars vals body)
     `(let ,(map list vars (map tree-il->scheme vals)) ,(tree-il->scheme body)))
    
    ((<letrec> vars vals body)
     `(letrec ,(map list vars (map tree-il->scheme vals)) ,(tree-il->scheme body)))

    ((<let-values> vars exp body)
     `(call-with-values (lambda () ,(tree-il->scheme exp))
        (lambda ,vars ,(tree-il->scheme body))))))

(define (post-order! f x)
  (let lp ((x x))
    (record-case x
      ((<application> proc args)
       (set! (application-proc x) (lp proc))
       (set! (application-args x) (map lp args)))

      ((<conditional> test then else)
       (set! (conditional-test x) (lp test))
       (set! (conditional-then x) (lp then))
       (set! (conditional-else x) (lp else)))
      
      ((<lexical-set> name gensym exp)
       (set! (lexical-set-exp x) (lp exp)))
      
      ((<module-set> mod name public? exp)
       (set! (module-set-exp x) (lp exp)))
      
      ((<toplevel-set> name exp)
       (set! (toplevel-set-exp x) (lp exp)))
      
      ((<toplevel-define> name exp)
       (set! (toplevel-define-exp x) (lp exp)))
      
      ((<lambda> vars meta body)
       (set! (lambda-body x) (lp body)))
      
      ((<sequence> exps)
       (set! (sequence-exps x) (map lp exps)))
      
      ((<let> vars vals body)
       (set! (let-vals x) (map lp vals))
       (set! (let-body x) (lp body)))
      
      ((<letrec> vars vals body)
       (set! (letrec-vals x) (map lp vals))
       (set! (letrec-body x) (lp body)))
      
      ((<let-values> vars exp body)
       (set! (let-values-exp x) (lp exp))
       (set! (let-values-body x) (lp body)))
      
      (else #f))
    
    (or (f x) x)))

(define (pre-order! f x)
  (let lp ((x x))
    (let ((x (or (f x) x)))
      (record-case x
        ((<application> proc args)
         (set! (application-proc x) (lp proc))
         (set! (application-args x) (map lp args)))

        ((<conditional> test then else)
         (set! (conditional-test x) (lp test))
         (set! (conditional-then x) (lp then))
         (set! (conditional-else x) (lp else)))

        ((<lexical-set> name gensym exp)
         (set! (lexical-set-exp x) (lp exp)))
               
        ((<module-set> mod name public? exp)
         (set! (module-set-exp x) (lp exp)))

        ((<toplevel-set> name exp)
         (set! (toplevel-set-exp x) (lp exp)))

        ((<toplevel-define> name exp)
         (set! (toplevel-define-exp x) (lp exp)))

        ((<lambda> vars meta body)
         (set! (lambda-body x) (lp body)))

        ((<sequence> exps)
         (set! (sequence-exps x) (map lp exps)))

        ((<let> vars vals body)
         (set! (let-vals x) (map lp vals))
         (set! (let-body x) (lp body)))

        ((<letrec> vars vals body)
         (set! (letrec-vals x) (map lp vals))
         (set! (letrec-body x) (lp body)))

        ((<let-values> vars exp body)
         (set! (let-values-exp x) (lp exp))
         (set! (let-values-body x) (lp body)))

        (else #f))
      x)))
