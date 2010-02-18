;;;; 	Copyright (C) 2009, 2010 Free Software Foundation, Inc.
;;;; 
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
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
            <conditional> conditional? make-conditional conditional-src conditional-test conditional-consequent conditional-alternate
            <application> application? make-application application-src application-proc application-args
            <sequence> sequence? make-sequence sequence-src sequence-exps
            <lambda> lambda? make-lambda lambda-src lambda-meta lambda-body
            <lambda-case> lambda-case? make-lambda-case lambda-case-src
                          lambda-case-req lambda-case-opt lambda-case-rest lambda-case-kw
                          lambda-case-inits lambda-case-vars
                          lambda-case-body lambda-case-alternate
            <let> let? make-let let-src let-names let-vars let-vals let-body
            <letrec> letrec? make-letrec letrec-src letrec-names letrec-vars letrec-vals letrec-body
            <fix> fix? make-fix fix-src fix-names fix-vars fix-vals fix-body
            <let-values> let-values? make-let-values let-values-src let-values-exp let-values-body
            <dynwind> dynwind? make-dynwind dynwind-src dynwind-winder dynwind-body dynwind-unwinder
            <dynlet> dynlet? make-dynlet dynlet-src dynlet-fluids dynlet-vals dynlet-body
            <prompt> prompt? make-prompt prompt-src prompt-tag prompt-body prompt-handler prompt-pre-unwind-handler 
            <control> control? make-control control-src control-tag control-type control-args

            parse-tree-il
            unparse-tree-il
            tree-il->scheme

            tree-il-fold
            make-tree-il-folder
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
  (<conditional> test consequent alternate)
  (<application> proc args)
  (<sequence> exps)
  (<lambda> meta body)
  (<lambda-case> req opt rest kw inits vars body alternate)
  (<let> names vars vals body)
  (<letrec> names vars vals body)
  (<fix> names vars vals body)
  (<let-values> exp body)
  (<dynwind> winder body unwinder)
  (<dynlet> fluids vals body)
  (<prompt> tag body handler pre-unwind-handler)
  (<control> tag type args))
  


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

     ((if ,test ,consequent ,alternate)
      (make-conditional loc (retrans test) (retrans consequent) (retrans alternate)))

     ((primitive ,name) (guard (symbol? name))
      (make-primitive-ref loc name))

     ((lexical ,name) (guard (symbol? name))
      (make-lexical-ref loc name name))

     ((lexical ,name ,sym) (guard (symbol? name) (symbol? sym))
      (make-lexical-ref loc name sym))

     ((set! (lexical ,name) ,exp) (guard (symbol? name))
      (make-lexical-set loc name name (retrans exp)))

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

     ((lambda ,meta ,body)
      (make-lambda loc meta (retrans body)))

     ((lambda-case ((,req ,opt ,rest ,kw ,inits ,vars) ,body) ,alternate)
      (make-lambda-case loc req opt rest kw 
                        (map retrans inits) vars
                        (retrans body)
                        (and=> alternate retrans)))

     ((lambda-case ((,req ,opt ,rest ,kw ,inits ,vars) ,body))
      (make-lambda-case loc req opt rest kw
                        (map retrans inits) vars
                        (retrans body)
                        #f))

     ((const ,exp)
      (make-const loc exp))

     ((begin . ,exps)
      (make-sequence loc (map retrans exps)))

     ((let ,names ,vars ,vals ,body)
      (make-let loc names vars (map retrans vals) (retrans body)))

     ((letrec ,names ,vars ,vals ,body)
      (make-letrec loc names vars (map retrans vals) (retrans body)))

     ((fix ,names ,vars ,vals ,body)
      (make-fix loc names vars (map retrans vals) (retrans body)))

     ((let-values ,exp ,body)
      (make-let-values loc (retrans exp) (retrans body)))

     ((dynwind ,winder ,body ,unwinder)
      (make-dynwind loc (retrans winder) (retrans body) (retrans unwinder)))
     
     ((dynlet ,fluids ,vals ,body)
      (make-dynlet loc (map retrans fluids) (map retrans vals) (retrans body)))
     
     ((prompt ,tag ,body ,handler ,pre-unwind-handler)
      (make-prompt loc (retrans tag) (retrans body) (retrans handler)
                   (and=> pre-unwind-handler retrans)))
     
     ((control ,tag ,type ,args)
      (make-control loc (retrans tag) type (map retrans args)))

     (else
      (error "unrecognized tree-il" exp)))))

(define (unparse-tree-il tree-il)
  (record-case tree-il
    ((<void>)
     '(void))

    ((<application> proc args)
     `(apply ,(unparse-tree-il proc) ,@(map unparse-tree-il args)))

    ((<conditional> test consequent alternate)
     `(if ,(unparse-tree-il test) ,(unparse-tree-il consequent) ,(unparse-tree-il alternate)))

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

    ((<lambda> meta body)
     `(lambda ,meta ,(unparse-tree-il body)))

    ((<lambda-case> req opt rest kw inits vars body alternate)
     `(lambda-case ((,req ,opt ,rest ,kw ,(map unparse-tree-il inits) ,vars)
                    ,(unparse-tree-il body))
                   . ,(if alternate (list (unparse-tree-il alternate)) '())))

    ((<const> exp)
     `(const ,exp))

    ((<sequence> exps)
     `(begin ,@(map unparse-tree-il exps)))

    ((<let> names vars vals body)
     `(let ,names ,vars ,(map unparse-tree-il vals) ,(unparse-tree-il body)))

    ((<letrec> names vars vals body)
     `(letrec ,names ,vars ,(map unparse-tree-il vals) ,(unparse-tree-il body)))

    ((<fix> names vars vals body)
     `(fix ,names ,vars ,(map unparse-tree-il vals) ,(unparse-tree-il body)))

    ((<let-values> exp body)
     `(let-values ,(unparse-tree-il exp) ,(unparse-tree-il body)))

    ((<dynwind> body winder unwinder)
     `(dynwind ,(unparse-tree-il body)
               ,(unparse-tree-il winder) ,(unparse-tree-il unwinder)))
    
    ((<dynlet> fluids vals body)
     `(dynlet ,(map unparse-tree-il fluids) ,(map unparse-tree-il vals)
              ,(unparse-tree-il body)))
    
    ((<prompt> tag body handler pre-unwind-handler)
     `(prompt ,tag ,(unparse-tree-il body) ,(unparse-tree-il handler)
              ,(and=> pre-unwind-handler unparse-tree-il)))
    
    ((<control> tag type args)
     `(control ,(unparse-tree-il tag) ,type ,(map unparse-tree-il args)))))

(define (tree-il->scheme e)
  (record-case e
    ((<void>)
     '(if #f #f))

    ((<application> proc args)
     `(,(tree-il->scheme proc) ,@(map tree-il->scheme args)))

    ((<conditional> test consequent alternate)
     (if (void? alternate)
         `(if ,(tree-il->scheme test) ,(tree-il->scheme consequent))
         `(if ,(tree-il->scheme test) ,(tree-il->scheme consequent) ,(tree-il->scheme alternate))))

    ((<primitive-ref> name)
     name)
    
    ((<lexical-ref> gensym)
     gensym)
    
    ((<lexical-set> gensym exp)
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
    
    ((<lambda> meta body)
     ;; fixme: put in docstring
     (if (and (lambda-case? body)
              (not (lambda-case-alternate body)))
         `(lambda ,@(car (tree-il->scheme body)))
         `(case-lambda ,@(tree-il->scheme body))))
    
    ((<lambda-case> req opt rest kw inits vars body alternate)
     ;; FIXME! use parse-lambda-case?
     `((,(if rest (apply cons* vars) vars)
        ,(tree-il->scheme body))
       ,@(if alternate (tree-il->scheme alternate) '())))
    
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

    ((<fix> vars vals body)
     ;; not a typo, we really do translate back to letrec
     `(letrec ,(map list vars (map tree-il->scheme vals)) ,(tree-il->scheme body)))

    ((<let-values> exp body)
     `(call-with-values (lambda () ,(tree-il->scheme exp))
        ,(tree-il->scheme (make-lambda #f '() body))))

    ((<dynwind> body winder unwinder)
     `(dynamic-wind ,(tree-il->scheme winder)
                    (lambda () ,(tree-il->scheme body))
                    ,(tree-il->scheme unwinder)))
    
    ((<dynlet> fluids vals body)
     `(with-fluids ,(map list
                         (map tree-il->scheme fluids)
                         (map tree-il->scheme vals))
        (lambda () ,(tree-il->scheme body))))
    
    ((<prompt> tag body handler pre-unwind-handler)
     `((@ (ice-9 control) prompt) 
       ,(tree-il->scheme tag) (lambda () ,(tree-il->scheme body))
       ,(tree-il->scheme handler) ,(and=> pre-unwind-handler tree-il->scheme)))
    

    ((<control> tag type args)
     (case type
       ((throw) `(throw ,(tree-il->scheme tag) ,@(map tree-il->scheme args)))
       (else (error "bad control type" type))))))


(define (tree-il-fold leaf down up seed tree)
  "Traverse TREE, calling LEAF on each leaf encountered, DOWN upon descent
into a sub-tree, and UP when leaving a sub-tree.  Each of these procedures is
invoked as `(PROC TREE SEED)', where TREE is the sub-tree or leaf considered
and SEED is the current result, intially seeded with SEED.

This is an implementation of `foldts' as described by Andy Wingo in
``Applications of fold to XML transformation''."
  (let loop ((tree   tree)
             (result seed))
    (if (or (null? tree) (pair? tree))
        (fold loop result tree)
        (record-case tree
          ((<lexical-set> exp)
           (up tree (loop exp (down tree result))))
          ((<module-set> exp)
           (up tree (loop exp (down tree result))))
          ((<toplevel-set> exp)
           (up tree (loop exp (down tree result))))
          ((<toplevel-define> exp)
           (up tree (loop exp (down tree result))))
          ((<conditional> test consequent alternate)
           (up tree (loop alternate
                          (loop consequent
                                (loop test (down tree result))))))
          ((<application> proc args)
           (up tree (loop (cons proc args) (down tree result))))
          ((<sequence> exps)
           (up tree (loop exps (down tree result))))
          ((<lambda> body)
           (up tree (loop body (down tree result))))
          ((<lambda-case> inits body alternate)
           (up tree (if alternate
                        (loop alternate
                              (loop body (loop inits (down tree result))))
                        (loop body (loop inits (down tree result))))))
          ((<let> vals body)
           (up tree (loop body
                          (loop vals
                                (down tree result)))))
          ((<letrec> vals body)
           (up tree (loop body
                          (loop vals
                                (down tree result)))))
          ((<fix> vals body)
           (up tree (loop body
                          (loop vals
                                (down tree result)))))
          ((<let-values> exp body)
           (up tree (loop body (loop exp (down tree result)))))
          ((<dynwind> body winder unwinder)
           (up tree (loop unwinder
                          (loop winder
                                (loop body (down tree result))))))
          ((<dynlet> fluids vals body)
           (up tree (loop body
                          (loop vals
                                (loop fluids (down tree result))))))
          ((<prompt> tag body handler pre-unwind-handler)
           (up tree (loop tag
                          (loop body
                                (loop handler
                                      (if pre-unwind-handler
                                          (loop pre-unwind-handler
                                                (down tree result))
                                          (down tree result)))))))
          ((<control> tag type args)
           (up tree (loop tag (loop args (down tree result)))))
          (else
           (leaf tree result))))))


(define-syntax make-tree-il-folder
  (syntax-rules ()
    ((_ seed ...)
     (lambda (tree down up seed ...)
       (define (fold-values proc exps seed ...)
         (if (null? exps)
             (values seed ...)
             (let-values (((seed ...) (proc (car exps) seed ...)))
               (fold-values proc (cdr exps) seed ...))))
       (let foldts ((tree tree) (seed seed) ...)
         (let*-values
             (((seed ...) (down tree seed ...))
              ((seed ...)
               (record-case tree
                 ((<lexical-set> exp)
                  (foldts exp seed ...))
                 ((<module-set> exp)
                  (foldts exp seed ...))
                 ((<toplevel-set> exp)
                  (foldts exp seed ...))
                 ((<toplevel-define> exp)
                  (foldts exp seed ...))
                 ((<conditional> test consequent alternate)
                  (let*-values (((seed ...) (foldts test seed ...))
                                ((seed ...) (foldts consequent seed ...)))
                    (foldts alternate seed ...)))
                 ((<application> proc args)
                  (let-values (((seed ...) (foldts proc seed ...)))
                    (fold-values foldts args seed ...)))
                 ((<sequence> exps)
                  (fold-values foldts exps seed ...))
                 ((<lambda> body)
                  (foldts body seed ...))
                 ((<lambda-case> inits body alternate)
                  (let-values (((seed ...) (fold-values foldts inits seed ...)))
                    (if alternate
                        (let-values (((seed ...) (foldts body seed ...)))
                          (foldts alternate seed ...))
                        (foldts body seed ...))))
                 ((<let> vals body)
                  (let*-values (((seed ...) (fold-values foldts vals seed ...)))
                    (foldts body seed ...)))
                 ((<letrec> vals body)
                  (let*-values (((seed ...) (fold-values foldts vals seed ...)))
                    (foldts body seed ...)))
                 ((<fix> vals body)
                  (let*-values (((seed ...) (fold-values foldts vals seed ...)))
                    (foldts body seed ...)))
                 ((<let-values> exp body)
                  (let*-values (((seed ...) (foldts exp seed ...)))
                    (foldts body seed ...)))
                 ((<dynwind> body winder unwinder)
                  (let*-values (((seed ...) (foldts body seed ...))
                                ((seed ...) (foldts winder seed ...)))
                    (foldts unwinder seed ...)))
                 ((<dynlet> fluids vals body)
                  (let*-values (((seed ...) (fold-values foldts fluids seed ...))
                                ((seed ...) (fold-values foldts vals seed ...)))
                    (foldts body seed ...)))
                 ((<prompt> tag body handler pre-unwind-handler)
                  (let*-values (((seed ...) (foldts tag seed ...))
                                ((seed ...) (foldts body seed ...))
                                ((seed ...) (foldts handler seed ...)))
                    (if pre-unwind-handler
                        (values seed ...)
                        (foldts pre-unwind-handler seed ...))))
                 ((<control> tag args)
                  (let*-values (((seed ...) (foldts tag seed ...)))
                    (fold-values foldts args seed ...)))
                 (else
                  (values seed ...)))))
           (up tree seed ...)))))))

(define (post-order! f x)
  (let lp ((x x))
    (record-case x
      ((<application> proc args)
       (set! (application-proc x) (lp proc))
       (set! (application-args x) (map lp args)))

      ((<conditional> test consequent alternate)
       (set! (conditional-test x) (lp test))
       (set! (conditional-consequent x) (lp consequent))
       (set! (conditional-alternate x) (lp alternate)))
      
      ((<lexical-set> name gensym exp)
       (set! (lexical-set-exp x) (lp exp)))
      
      ((<module-set> mod name public? exp)
       (set! (module-set-exp x) (lp exp)))
      
      ((<toplevel-set> name exp)
       (set! (toplevel-set-exp x) (lp exp)))
      
      ((<toplevel-define> name exp)
       (set! (toplevel-define-exp x) (lp exp)))
      
      ((<lambda> body)
       (set! (lambda-body x) (lp body)))
      
      ((<lambda-case> inits body alternate)
       (set! inits (map lp inits))
       (set! (lambda-case-body x) (lp body))
       (if alternate
           (set! (lambda-case-alternate x) (lp alternate))))
      
      ((<sequence> exps)
       (set! (sequence-exps x) (map lp exps)))
      
      ((<let> vars vals body)
       (set! (let-vals x) (map lp vals))
       (set! (let-body x) (lp body)))
      
      ((<letrec> vars vals body)
       (set! (letrec-vals x) (map lp vals))
       (set! (letrec-body x) (lp body)))
      
      ((<fix> vars vals body)
       (set! (fix-vals x) (map lp vals))
       (set! (fix-body x) (lp body)))
      
      ((<let-values> exp body)
       (set! (let-values-exp x) (lp exp))
       (set! (let-values-body x) (lp body)))
      
      ((<dynwind> body winder unwinder)
       (set! (dynwind-body x) (lp body))
       (set! (dynwind-winder x) (lp winder))
       (set! (dynwind-unwinder x) (lp unwinder)))
      
      ((<dynlet> fluids vals body)
       (set! (dynlet-fluids x) (map lp fluids))
       (set! (dynlet-vals x) (map lp vals))
       (set! (dynlet-body x) (lp body)))
      
      ((<prompt> tag body handler pre-unwind-handler)
       (set! (prompt-tag x) (lp tag))
       (set! (prompt-body x) (lp body))
       (set! (prompt-handler x) (lp handler))
       (if pre-unwind-handler
           (set! (prompt-pre-unwind-handler x) (lp pre-unwind-handler))))
      
      ((<control> tag args)
       (set! (control-tag x) (lp tag))
       (set! (control-args x) (map lp args)))
      
      (else #f))
    
    (or (f x) x)))

(define (pre-order! f x)
  (let lp ((x x))
    (let ((x (or (f x) x)))
      (record-case x
        ((<application> proc args)
         (set! (application-proc x) (lp proc))
         (set! (application-args x) (map lp args)))

        ((<conditional> test consequent alternate)
         (set! (conditional-test x) (lp test))
         (set! (conditional-consequent x) (lp consequent))
         (set! (conditional-alternate x) (lp alternate)))

        ((<lexical-set> exp)
         (set! (lexical-set-exp x) (lp exp)))
               
        ((<module-set> exp)
         (set! (module-set-exp x) (lp exp)))

        ((<toplevel-set> exp)
         (set! (toplevel-set-exp x) (lp exp)))

        ((<toplevel-define> exp)
         (set! (toplevel-define-exp x) (lp exp)))

        ((<lambda> body)
         (set! (lambda-body x) (lp body)))

        ((<lambda-case> inits body alternate)
         (set! inits (map lp inits))
         (set! (lambda-case-body x) (lp body))
         (if alternate (set! (lambda-case-alternate x) (lp alternate))))

        ((<sequence> exps)
         (set! (sequence-exps x) (map lp exps)))

        ((<let> vals body)
         (set! (let-vals x) (map lp vals))
         (set! (let-body x) (lp body)))

        ((<letrec> vals body)
         (set! (letrec-vals x) (map lp vals))
         (set! (letrec-body x) (lp body)))

        ((<fix> vals body)
         (set! (fix-vals x) (map lp vals))
         (set! (fix-body x) (lp body)))

        ((<let-values> exp body)
         (set! (let-values-exp x) (lp exp))
         (set! (let-values-body x) (lp body)))

        ((<dynwind> body winder unwinder)
         (set! (dynwind-body x) (lp body))
         (set! (dynwind-winder x) (lp winder))
         (set! (dynwind-unwinder x) (lp unwinder)))
        
        ((<dynlet> fluids vals body)
         (set! (dynlet-fluids x) (map lp fluids))
         (set! (dynlet-vals x) (map lp vals))
         (set! (dynlet-body x) (lp body)))
      
        ((<prompt> tag body handler pre-unwind-handler)
         (set! (prompt-tag x) (lp tag))
         (set! (prompt-body x) (lp body))
         (set! (prompt-handler x) (lp handler))
         (if pre-unwind-handler
             (set! (prompt-pre-unwind-handler x) (lp pre-unwind-handler))))
        
        ((<control> tag args)
         (set! (control-tag x) (lp tag))
         (set! (control-args x) (map lp args)))
        
        (else #f))
      x)))
