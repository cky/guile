;;; TREE-IL -> GLIL compiler

;; Copyright (C) 2001,2008,2009 Free Software Foundation, Inc.

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

;;; Code:

(define-module (language tree-il compile-glil)
  #:use-module (system base syntax)
  #:use-module (system base pmatch)
  #:use-module (system base message)
  #:use-module (ice-9 receive)
  #:use-module (language glil)
  #:use-module (system vm instruction)
  #:use-module (language tree-il)
  #:use-module (language tree-il optimize)
  #:use-module (language tree-il analyze)
  #:export (compile-glil))

;;; TODO:
;;
;; call-with-values -> mv-bind
;; basic degenerate-case reduction

;; allocation:
;;  sym -> {lambda -> address}
;;  lambda -> (nlocs . closure-vars)
;;
;; address := (local? boxed? . index)
;; free-locs ::= ((sym0 . address0) (sym1 . address1) ...)
;; free variable addresses are relative to parent proc.

(define *comp-module* (make-fluid))

(define %warning-passes
  `((unused-variable . ,report-unused-variables)))

(define (compile-glil x e opts)
  (define warnings
    (or (and=> (memq #:warnings opts) cadr)
        '()))

  (let* ((x (make-lambda (tree-il-src x) '() '() '() x))
         (x (optimize! x e opts))
         (allocation (analyze-lexicals x)))

    ;; Go throught the warning passes.
    (for-each (lambda (kind)
                (let ((warn (assoc-ref %warning-passes kind)))
                  (and (procedure? warn)
                       (warn x))))
              warnings)

    (with-fluid* *comp-module* (or (and e (car e)) (current-module))
      (lambda ()
        (values (flatten-lambda x allocation)
                (and e (cons (car e) (cddr e)))
                e)))))



(define *primcall-ops* (make-hash-table))
(for-each
 (lambda (x) (hash-set! *primcall-ops* (car x) (cdr x)))
 '(((eq? . 2) . eq?)
   ((eqv? . 2) . eqv?)
   ((equal? . 2) . equal?)
   ((= . 2) . ee?)
   ((< . 2) . lt?)
   ((> . 2) . gt?)
   ((<= . 2) . le?)
   ((>= . 2) . ge?)
   ((+ . 2) . add)
   ((- . 2) . sub)
   ((1+ . 1) . add1)
   ((1- . 1) . sub1)
   ((* . 2) . mul)
   ((/ . 2) . div)
   ((quotient . 2) . quo)
   ((remainder . 2) . rem)
   ((modulo . 2) . mod)
   ((not . 1) . not)
   ((pair? . 1) . pair?)
   ((cons . 2) . cons)
   ((car . 1) . car)
   ((cdr . 1) . cdr)
   ((set-car! . 2) . set-car!)
   ((set-cdr! . 2) . set-cdr!)
   ((null? . 1) . null?)
   ((list? . 1) . list?)
   (list . list)
   (vector . vector)
   ((@slot-ref . 2) . slot-ref)
   ((@slot-set! . 3) . slot-set)
   ((vector-ref . 2) . vector-ref)
   ((vector-set! . 3) . vector-set)

   ((bytevector-u8-ref . 2) . bv-u8-ref)
   ((bytevector-u8-set! . 3) . bv-u8-set)
   ((bytevector-s8-ref . 2) . bv-s8-ref)
   ((bytevector-s8-set! . 3) . bv-s8-set)

   ((bytevector-u16-ref . 3) . bv-u16-ref)
   ((bytevector-u16-set! . 4) . bv-u16-set)
   ((bytevector-u16-native-ref . 2) . bv-u16-native-ref)
   ((bytevector-u16-native-set! . 3) . bv-u16-native-set)
   ((bytevector-s16-ref . 3) . bv-s16-ref)
   ((bytevector-s16-set! . 4) . bv-s16-set)
   ((bytevector-s16-native-ref . 2) . bv-s16-native-ref)
   ((bytevector-s16-native-set! . 3) . bv-s16-native-set)
    
   ((bytevector-u32-ref . 3) . bv-u32-ref)
   ((bytevector-u32-set! . 4) . bv-u32-set)
   ((bytevector-u32-native-ref . 2) . bv-u32-native-ref)
   ((bytevector-u32-native-set! . 3) . bv-u32-native-set)
   ((bytevector-s32-ref . 3) . bv-s32-ref)
   ((bytevector-s32-set! . 4) . bv-s32-set)
   ((bytevector-s32-native-ref . 2) . bv-s32-native-ref)
   ((bytevector-s32-native-set! . 3) . bv-s32-native-set)
    
   ((bytevector-u64-ref . 3) . bv-u64-ref)
   ((bytevector-u64-set! . 4) . bv-u64-set)
   ((bytevector-u64-native-ref . 2) . bv-u64-native-ref)
   ((bytevector-u64-native-set! . 3) . bv-u64-native-set)
   ((bytevector-s64-ref . 3) . bv-s64-ref)
   ((bytevector-s64-set! . 4) . bv-s64-set)
   ((bytevector-s64-native-ref . 2) . bv-s64-native-ref)
   ((bytevector-s64-native-set! . 3) . bv-s64-native-set)
    
   ((bytevector-ieee-single-ref . 3) . bv-f32-ref)
   ((bytevector-ieee-single-set! . 4) . bv-f32-set)
   ((bytevector-ieee-single-native-ref . 2) . bv-f32-native-ref)
   ((bytevector-ieee-single-native-set! . 3) . bv-f32-native-set)
   ((bytevector-ieee-double-ref . 3) . bv-f64-ref)
   ((bytevector-ieee-double-set! . 4) . bv-f64-set)
   ((bytevector-ieee-double-native-ref . 2) . bv-f64-native-ref)
   ((bytevector-ieee-double-native-set! . 3) . bv-f64-native-set)))




(define (make-label) (gensym ":L"))

(define (vars->bind-list ids vars allocation proc)
  (map (lambda (id v)
         (pmatch (hashq-ref (hashq-ref allocation v) proc)
           ((#t ,boxed? . ,n)
            (list id boxed? n))
           (,x (error "badness" x))))
       ids
       vars))

(define (emit-bindings src ids vars allocation proc emit-code)
  (if (pair? vars)
      (emit-code src (make-glil-bind
                      (vars->bind-list ids vars allocation proc)))))

(define (with-output-to-code proc)
  (let ((out '()))
    (define (emit-code src x)
      (set! out (cons x out))
      (if src
          (set! out (cons (make-glil-source src) out))))
    (proc emit-code)
    (reverse out)))

(define (flatten-lambda x allocation)
  (receive (ids vars nargs nrest)
      (let lp ((ids (lambda-names x)) (vars (lambda-vars x))
               (oids '()) (ovars '()) (n 0))
          (cond ((null? vars) (values (reverse oids) (reverse ovars) n 0))
                ((pair? vars) (lp (cdr ids) (cdr vars)
                                  (cons (car ids) oids) (cons (car vars) ovars)
                                  (1+ n)))
                (else (values (reverse (cons ids oids))
                              (reverse (cons vars ovars))
                              (1+ n) 1))))
    (let ((nlocs (car (hashq-ref allocation x))))
      (make-glil-program
       nargs nrest nlocs (lambda-meta x)
       (with-output-to-code
        (lambda (emit-code)
          ;; write bindings and source debugging info
          (emit-bindings #f ids vars allocation x emit-code)
          (if (lambda-src x)
              (emit-code #f (make-glil-source (lambda-src x))))
          ;; box args if necessary
          (for-each
           (lambda (v)
             (pmatch (hashq-ref (hashq-ref allocation v) x)
               ((#t #t . ,n)
                (emit-code #f (make-glil-lexical #t #f 'ref n))
                (emit-code #f (make-glil-lexical #t #t 'box n)))))
           vars)
          ;; and here, here, dear reader: we compile.
          (flatten (lambda-body x) allocation x emit-code)))))))

(define (flatten x allocation proc emit-code)
  (define (emit-label label)
    (emit-code #f (make-glil-label label)))
  (define (emit-branch src inst label)
    (emit-code src (make-glil-branch inst label)))

  ;; LMVRA == "let-values MV return address"
  (let comp ((x x) (context 'tail) (LMVRA #f))
    (define (comp-tail tree) (comp tree context LMVRA))
    (define (comp-push tree) (comp tree 'push #f))
    (define (comp-drop tree) (comp tree 'drop #f))
    (define (comp-vals tree LMVRA) (comp tree 'vals LMVRA))

    (record-case x
      ((<void>)
       (case context
         ((push vals) (emit-code #f (make-glil-void)))
         ((tail)
          (emit-code #f (make-glil-void))
          (emit-code #f (make-glil-call 'return 1)))))

      ((<const> src exp)
       (case context
         ((push vals) (emit-code src (make-glil-const exp)))
         ((tail)
          (emit-code src (make-glil-const exp))
          (emit-code #f (make-glil-call 'return 1)))))

      ;; FIXME: should represent sequence as exps tail
      ((<sequence> src exps)
       (let lp ((exps exps))
         (if (null? (cdr exps))
             (comp-tail (car exps))
             (begin
               (comp-drop (car exps))
               (lp (cdr exps))))))

      ((<application> src proc args)
       ;; FIXME: need a better pattern-matcher here
       (cond
        ((and (primitive-ref? proc)
              (eq? (primitive-ref-name proc) '@apply)
              (>= (length args) 1))
         (let ((proc (car args))
               (args (cdr args)))
           (cond
            ((and (primitive-ref? proc) (eq? (primitive-ref-name proc) 'values)
                  (not (eq? context 'push)) (not (eq? context 'vals)))
             ;; tail: (lambda () (apply values '(1 2)))
             ;; drop: (lambda () (apply values '(1 2)) 3)
             ;; push: (lambda () (list (apply values '(10 12)) 1))
             (case context
               ((drop) (for-each comp-drop args))
               ((tail)
                (for-each comp-push args)
                (emit-code src (make-glil-call 'return/values* (length args))))))

            (else
             (case context
               ((tail)
                (comp-push proc)
                (for-each comp-push args)
                (emit-code src (make-glil-call 'goto/apply (1+ (length args)))))
               ((push)
                (comp-push proc)
                (for-each comp-push args)
                (emit-code src (make-glil-call 'apply (1+ (length args)))))
               ((vals)
                (comp-vals
                 (make-application src (make-primitive-ref #f 'apply)
                                   (cons proc args))
                 LMVRA))
               ((drop)
                ;; Well, shit. The proc might return any number of
                ;; values (including 0), since it's in a drop context,
                ;; yet apply does not create a MV continuation. So we
                ;; mv-call out to our trampoline instead.
                (comp-drop
                 (make-application src (make-primitive-ref #f 'apply)
                                   (cons proc args)))))))))

        ((and (primitive-ref? proc) (eq? (primitive-ref-name proc) 'values)
              (not (eq? context 'push)))
         ;; tail: (lambda () (values '(1 2)))
         ;; drop: (lambda () (values '(1 2)) 3)
         ;; push: (lambda () (list (values '(10 12)) 1))
         ;; vals: (let-values (((a b ...) (values 1 2 ...))) ...)
         (case context
           ((drop) (for-each comp-drop args))
           ((vals)
            (for-each comp-push args)
            (emit-code #f (make-glil-const (length args)))
            (emit-branch src 'br LMVRA))
           ((tail)
            (for-each comp-push args)
            (emit-code src (make-glil-call 'return/values (length args))))))
        
        ((and (primitive-ref? proc)
              (eq? (primitive-ref-name proc) '@call-with-values)
              (= (length args) 2))
	 ;; CONSUMER
         ;; PRODUCER
         ;; (mv-call MV)
         ;; ([tail]-call 1)
         ;; goto POST
         ;; MV: [tail-]call/nargs
         ;; POST: (maybe-drop)
         (case context
           ((vals)
            ;; Fall back.
            (comp-vals
             (make-application src (make-primitive-ref #f 'call-with-values)
                               args)
             LMVRA))
           (else
            (let ((MV (make-label)) (POST (make-label))
                  (producer (car args)) (consumer (cadr args)))
              (comp-push consumer)
              (comp-push producer)
              (emit-code src (make-glil-mv-call 0 MV))
              (case context
                ((tail) (emit-code src (make-glil-call 'goto/args 1)))
                (else   (emit-code src (make-glil-call 'call 1))
                        (emit-branch #f 'br POST)))
              (emit-label MV)
              (case context
                ((tail) (emit-code src (make-glil-call 'goto/nargs 0)))
                (else   (emit-code src (make-glil-call 'call/nargs 0))
                        (emit-label POST)
                        (if (eq? context 'drop)
                            (emit-code #f (make-glil-call 'drop 1)))))))))

        ((and (primitive-ref? proc)
              (eq? (primitive-ref-name proc) '@call-with-current-continuation)
              (= (length args) 1))
         (case context
           ((tail)
            (comp-push (car args))
            (emit-code src (make-glil-call 'goto/cc 1)))
           ((vals)
            (comp-vals
             (make-application
              src (make-primitive-ref #f 'call-with-current-continuation)
              args)
             LMVRA))
           ((push)
            (comp-push (car args))
            (emit-code src (make-glil-call 'call/cc 1)))
           ((drop)
            ;; Crap. Just like `apply' in drop context.
            (comp-drop
             (make-application
              src (make-primitive-ref #f 'call-with-current-continuation)
              args)))))

        ((and (primitive-ref? proc)
              (or (hash-ref *primcall-ops*
                            (cons (primitive-ref-name proc) (length args)))
                  (hash-ref *primcall-ops* (primitive-ref-name proc))))
         => (lambda (op)
              (for-each comp-push args)
              (emit-code src (make-glil-call op (length args)))
              (case (instruction-pushes op)
                ((0)
                 (case context
                   ((tail) (emit-code #f (make-glil-void))
                           (emit-code #f (make-glil-call 'return 1)))
                   ((push vals) (emit-code #f (make-glil-void)))))
                ((1)
                 (case context
                   ((tail) (emit-code #f (make-glil-call 'return 1)))
                   ((drop) (emit-code #f (make-glil-call 'drop 1)))))
                (else
                 (error "bad primitive op: too many pushes"
                        op (instruction-pushes op))))))
        
        (else
         (comp-push proc)
         (for-each comp-push args)
         (let ((len (length args)))
           (case context
             ((tail) (emit-code src (make-glil-call 'goto/args len)))
             ((push) (emit-code src (make-glil-call 'call len)))
             ((vals) (emit-code src (make-glil-mv-call len LMVRA)))
             ((drop)
              (let ((MV (make-label)) (POST (make-label)))
                (emit-code src (make-glil-mv-call len MV))
                (emit-code #f (make-glil-call 'drop 1))
                (emit-branch #f 'br POST)
                (emit-label MV)
                (emit-code #f (make-glil-mv-bind '() #f))
                (emit-code #f (make-glil-unbind))
                (emit-label POST))))))))

      ((<conditional> src test then else)
       ;;     TEST
       ;;     (br-if-not L1)
       ;;     THEN
       ;;     (br L2)
       ;; L1: ELSE
       ;; L2:
       (let ((L1 (make-label)) (L2 (make-label)))
         (comp-push test)
         (emit-branch src 'br-if-not L1)
         (comp-tail then)
         (if (not (eq? context 'tail))
             (emit-branch #f 'br L2))
         (emit-label L1)
         (comp-tail else)
         (if (not (eq? context 'tail))
             (emit-label L2))))

      ((<primitive-ref> src name)
       (cond
        ((eq? (module-variable (fluid-ref *comp-module*) name)
              (module-variable the-root-module name))
         (case context
           ((push vals)
            (emit-code src (make-glil-toplevel 'ref name)))
           ((tail)
            (emit-code src (make-glil-toplevel 'ref name))
            (emit-code #f (make-glil-call 'return 1)))))
        (else
         (pk 'ew-the-badness x (current-module) (fluid-ref *comp-module*))
         (case context
           ((push vals)
            (emit-code src (make-glil-module 'ref '(guile) name #f)))
           ((tail)
            (emit-code src (make-glil-module 'ref '(guile) name #f))
            (emit-code #f (make-glil-call 'return 1)))))))

      ((<lexical-ref> src name gensym)
       (case context
         ((push vals tail)
          (pmatch (hashq-ref (hashq-ref allocation gensym) proc)
            ((,local? ,boxed? . ,index)
             (emit-code src (make-glil-lexical local? boxed? 'ref index)))
            (,loc
             (error "badness" x loc)))))
       (case context
         ((tail) (emit-code #f (make-glil-call 'return 1)))))
      
      ((<lexical-set> src name gensym exp)
       (comp-push exp)
       (pmatch (hashq-ref (hashq-ref allocation gensym) proc)
         ((,local? ,boxed? . ,index)
          (emit-code src (make-glil-lexical local? boxed? 'set index)))
         (,loc
          (error "badness" x loc)))
       (case context
         ((push vals)
          (emit-code #f (make-glil-void)))
         ((tail) 
          (emit-code #f (make-glil-void))
          (emit-code #f (make-glil-call 'return 1)))))
      
      ((<module-ref> src mod name public?)
       (emit-code src (make-glil-module 'ref mod name public?))
       (case context
         ((drop) (emit-code #f (make-glil-call 'drop 1)))
         ((tail) (emit-code #f (make-glil-call 'return 1)))))
      
      ((<module-set> src mod name public? exp)
       (comp-push exp)
       (emit-code src (make-glil-module 'set mod name public?))
       (case context
         ((push vals)
          (emit-code #f (make-glil-void)))
         ((tail) 
          (emit-code #f (make-glil-void))
          (emit-code #f (make-glil-call 'return 1)))))

      ((<toplevel-ref> src name)
       (emit-code src (make-glil-toplevel 'ref name))
       (case context
         ((drop) (emit-code #f (make-glil-call 'drop 1)))
         ((tail) (emit-code #f (make-glil-call 'return 1)))))
      
      ((<toplevel-set> src name exp)
       (comp-push exp)
       (emit-code src (make-glil-toplevel 'set name))
       (case context
         ((push vals)
          (emit-code #f (make-glil-void)))
         ((tail) 
          (emit-code #f (make-glil-void))
          (emit-code #f (make-glil-call 'return 1)))))
      
      ((<toplevel-define> src name exp)
       (comp-push exp)
       (emit-code src (make-glil-toplevel 'define name))
       (case context
         ((push vals)
          (emit-code #f (make-glil-void)))
         ((tail) 
          (emit-code #f (make-glil-void))
          (emit-code #f (make-glil-call 'return 1)))))

      ((<lambda>)
       (let ((free-locs (cdr (hashq-ref allocation x))))
         (case context
           ((push vals tail)
            (emit-code #f (flatten-lambda x allocation))
            (if (not (null? free-locs))
                (begin
                  (for-each
                   (lambda (loc)
                     (pmatch loc
                       ((,local? ,boxed? . ,n)
                        (emit-code #f (make-glil-lexical local? #f 'ref n)))
                       (else (error "what" x loc))))
                   free-locs)
                  (emit-code #f (make-glil-call 'vector (length free-locs)))
                  (emit-code #f (make-glil-call 'make-closure 2))))
            (if (eq? context 'tail)
                (emit-code #f (make-glil-call 'return 1)))))))
      
      ((<let> src names vars vals body)
       (for-each comp-push vals)
       (emit-bindings src names vars allocation proc emit-code)
       (for-each (lambda (v)
                   (pmatch (hashq-ref (hashq-ref allocation v) proc)
                     ((#t #f . ,n)
                      (emit-code src (make-glil-lexical #t #f 'set n)))
                     ((#t #t . ,n)
                      (emit-code src (make-glil-lexical #t #t 'box n)))
                     (,loc (error "badness" x loc))))
                 (reverse vars))
       (comp-tail body)
       (emit-code #f (make-glil-unbind)))

      ((<letrec> src names vars vals body)
       (for-each (lambda (v)
                   (pmatch (hashq-ref (hashq-ref allocation v) proc)
                     ((#t #t . ,n)
                      (emit-code src (make-glil-lexical #t #t 'empty-box n)))
                     (,loc (error "badness" x loc))))
                 vars)
       (for-each comp-push vals)
       (emit-bindings src names vars allocation proc emit-code)
       (for-each (lambda (v)
                   (pmatch (hashq-ref (hashq-ref allocation v) proc)
                     ((#t #t . ,n)
                      (emit-code src (make-glil-lexical #t #t 'set n)))
                     (,loc (error "badness" x loc))))
                 (reverse vars))
       (comp-tail body)
       (emit-code #f (make-glil-unbind)))

      ((<fix> src names vars vals body)
       ;; For fixpoint procedures, we can do some tricks to avoid
       ;; heap-allocation. Since we know the vals are lambdas, we can
       ;; set them to their local var slots first, then capture their
       ;; bindings, mutating them in place.
       (for-each (lambda (x v)
                   (emit-code #f (flatten-lambda x allocation))
                   (if (not (null? (cdr (hashq-ref allocation x))))
                       ;; But we do have to make-closure them first, so
                       ;; we are mutating fresh closures on the heap.
                       (begin
                         (emit-code #f (make-glil-const #f))
                         (emit-code #f (make-glil-call 'make-closure 2))))
                   (pmatch (hashq-ref (hashq-ref allocation v) proc)
                     ((#t #f . ,n)
                      (emit-code src (make-glil-lexical #t #f 'set n)))
                     (,loc (error "badness" x loc))))
                 vals
                 vars)
       (emit-bindings src names vars allocation proc emit-code)
       ;; Now go back and fix up the bindings.
       (for-each
        (lambda (x v)
          (let ((free-locs (cdr (hashq-ref allocation x))))
            (if (not (null? free-locs))
                (begin
                  (for-each
                   (lambda (loc)
                     (pmatch loc
                       ((,local? ,boxed? . ,n)
                        (emit-code #f (make-glil-lexical local? #f 'ref n)))
                       (else (error "what" x loc))))
                   free-locs)
                  (emit-code #f (make-glil-call 'vector (length free-locs)))
                  (pmatch (hashq-ref (hashq-ref allocation v) proc)
                    ((#t #f . ,n)
                     (emit-code #f (make-glil-lexical #t #f 'fix n)))
                    (,loc (error "badness" x loc)))))))
        vals
        vars)
       (comp-tail body)
       (emit-code #f (make-glil-unbind)))

      ((<let-values> src names vars exp body)
       (let lp ((names '()) (vars '()) (inames names) (ivars vars) (rest? #f))
         (cond
          ((pair? inames)
           (lp (cons (car inames) names) (cons (car ivars) vars)
               (cdr inames) (cdr ivars) #f))
          ((not (null? inames))
           (lp (cons inames names) (cons ivars vars) '() '() #t))
          (else
           (let ((names (reverse! names))
                 (vars (reverse! vars))
                 (MV (make-label)))
             (comp-vals exp MV)
             (emit-code #f (make-glil-const 1))
             (emit-label MV)
             (emit-code src (make-glil-mv-bind
                             (vars->bind-list names vars allocation proc)
                             rest?))
             (for-each (lambda (v)
                         (pmatch (hashq-ref (hashq-ref allocation v) proc)
                           ((#t #f . ,n)
                            (emit-code src (make-glil-lexical #t #f 'set n)))
                           ((#t #t . ,n)
                            (emit-code src (make-glil-lexical #t #t 'box n)))
                           (,loc (error "badness" x loc))))
                       (reverse vars))
             (comp-tail body)
             (emit-code #f (make-glil-unbind))))))))))
