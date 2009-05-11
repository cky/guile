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

            <lexical> make-lexical
            lexical-name lexical-gensym

            <application> make-application application-src application-proc application-args
            <conditional> make-conditional conditional-src conditional-test conditional-then conditional-else
            <primitive-ref> make-primitive-ref primitive-ref-src primitive-ref-name
            <lexical-ref> make-lexical-ref lexical-ref-src lexical-ref-name lexical-ref-gensym
            <lexical-set> make-lexical-set lexical-set-src lexical-set-name lexical-set-gensym lexical-set-exp
            <module-ref> make-module-ref module-ref-src module-ref-mod module-ref-name module-ref-public?
            <module-set> make-module-set module-set-src module-set-mod module-set-name module-set-public? module-set-exp
            <toplevel-ref> make-toplevel-ref toplevel-ref-src toplevel-ref-name
            <toplevel-set> make-toplevel-set toplevel-set-src toplevel-set-name toplevel-set-exp
            <toplevel-define> make-toplevel-define toplevel-define-src toplevel-define-name toplevel-define-exp
            <lambda> make-lambda lambda-src lambda-vars lambda-meta lambda-body
            <const> make-const const-src const-exp
            <sequence> make-sequence sequence-src sequence-exps
            <let> make-let let-src let-vars let-vals let-exp
            <letrec> make-letrec letrec-src letrec-vars letrec-vals letrec-exp

            parse-tree-il
            unparse-tree-il
            tree-il->scheme))

(define-type (<tree-il> #:common-slots (src))
  (<application> proc args)
  (<conditional> test then else)
  (<primitive-ref> name)
  (<lexical-ref> name gensym)
  (<lexical-set> name gensym exp)
  (<module-ref> mod name public?)
  (<module-set> mod name public? exp)
  (<toplevel-ref> name)
  (<toplevel-set> name exp)
  (<toplevel-define> name exp)
  (<lambda> vars meta body)
  (<const> exp)
  (<sequence> exps)
  (<let> vars vals exp)
  (<letrec> vars vals exp))
  
(define <lexical> <lexical-ref>)
(define lexical? lexical-ref?)
(define make-lexical make-lexical-ref)
(define lexical-name lexical-ref-name)
(define lexical-gensym lexical-ref-gensym)



;; FIXME: use this in psyntax
(define (location x)
  (and (pair? x)
       (let ((props (source-properties x)))
	 (and (not (null? props))
	      (vector (assq-ref props 'line)
                      (assq-ref props 'column)
                      (assq-ref props 'filename))))))

(define (parse-tree-il env exp)
  (let ((loc (location exp))
        (retrans (lambda (x) (parse-ghil env x))))
    (pmatch exp
     ((apply ,proc ,args)
      (make-application loc (retrans proc) (retrans args)))

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

     ((set! (@ ,mod ,name) ,exp) (guard (and-map symbol? mod) (symbol? name))
      (make-module-set loc mod name #f (retrans exp)))

     ((toplevel ,name) (guard (symbol? name))
      (make-toplevel-ref loc name))

     ((set! (toplevel ,name) exp) (guard (symbol? name))
      (make-toplevel-set loc name (retrans exp)))

     ((define ,name exp) (guard (symbol? name))
      (make-toplevel-define loc name (retrans exp)))

     ((lambda ,vars ,exp)
      (make-lambda loc vars '() (retrans exp)))

     ((lambda ,vars ,meta ,exp)
      (make-lambda loc vars meta (retrans exp)))

     ((const ,exp)
      (make-const loc exp))

     ((begin . ,exps)
      (make-sequence loc (map retrans exps)))

     ((let ,vars ,vals ,exp)
      (make-let loc vars vals (retrans exp)))

     ((letrec ,vars ,vals ,exp)
      (make-letrec loc vars vals (retrans exp)))

     (else
      (error "unrecognized tree-il" exp)))))

(define (unparse-tree-il tree-il)
  (record-case tree-il
    ((<application> proc args)
     `(apply ,(unparse-tree-il proc) ,(map unparse-tree-il args)))

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

    ((<lambda> vars meta body)
     `(lambda ,vars ,meta ,(unparse-tree-il body)))

    ((<const> exp)
     `(const ,exp))

    ((<sequence> exps)
     `(begin ,@(map unparse-tree-il exps)))

    ((<let> vars vals exp)
     `(let ,vars ,(map unparse-tree-il vals) ,(unparse-tree-il exp)))

    ((<letrec> vars vals exp)
     `(letrec ,vars ,(map unparse-tree-il vals) ,(unparse-tree-il exp)))))

(define (tree-il->scheme e)
  (cond ((list? e)
         (map tree-il->scheme e))
        ((pair? e)
         (cons (tree-il->scheme (car e))
               (tree-il->scheme (cdr e))))
        ((record? e)
         (record-case e
           ((<application> proc args)
            `(,(tree-il->scheme proc) ,@(map tree-il->scheme args)))

           ((<conditional> test then else)
            `(if ,(tree-il->scheme test) ,(tree-il->scheme then) ,(tree-il->scheme else)))

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
           
           ((<let> vars vals exp)
            `(let ,(map list vars (map tree-il->scheme vals)) ,(tree-il->scheme exp)))
           
           ((<letrec> vars vals exp)
            `(letrec ,(map list vars (map tree-il->scheme vals)) ,(tree-il->scheme exp)))))
        (else e)))
