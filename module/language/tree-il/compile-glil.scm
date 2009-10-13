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

;; allocation:
;;  sym -> {lambda -> address}
;;  lambda -> (labels . free-locs)
;;  lambda-case -> (gensym . nlocs)
;;
;; address ::= (local? boxed? . index)
;; labels ::= ((sym . lambda) ...)
;; free-locs ::= ((sym0 . address0) (sym1 . address1) ...)
;; free variable addresses are relative to parent proc.

(define *comp-module* (make-fluid))

(define %warning-passes
  `((unused-variable     . ,report-unused-variables)
    (unbound-variable    . ,report-possibly-unbound-variables)))

(define (compile-glil x e opts)
  (define warnings
    (or (and=> (memq #:warnings opts) cadr)
        '()))

  ;; Go through the warning passes.
  (for-each (lambda (kind)
                (let ((warn (assoc-ref %warning-passes kind)))
                  (and (procedure? warn)
                       (warn x e))))
            warnings)

  (let* ((x (make-lambda (tree-il-src x) '()
                         (make-lambda-case #f '() #f #f #f '() #f x #f)))
         (x (optimize! x e opts))
         (allocation (analyze-lexicals x)))

    (with-fluid* *comp-module* e
      (lambda ()
        (values (flatten-lambda x #f allocation)
                e
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
  (emit-code src (make-glil-bind
                  (vars->bind-list ids vars allocation proc))))

(define (with-output-to-code proc)
  (let ((out '()))
    (define (emit-code src x)
      (set! out (cons x out))
      (if src
          (set! out (cons (make-glil-source src) out))))
    (proc emit-code)
    (reverse out)))

(define (flatten-lambda x self-label allocation)
  (record-case x
    ((<lambda> src meta body)
     (make-glil-program
      meta
      (with-output-to-code
       (lambda (emit-code)
         ;; write source info for proc
         (if src (emit-code #f (make-glil-source src)))
         ;; emit pre-prelude label for self tail calls in which the
         ;; number of arguments doesn't check out at compile time
         (if self-label
             (emit-code #f (make-glil-label self-label)))
         ;; compile the body, yo
         (flatten body allocation x self-label (car (hashq-ref allocation x))
                  emit-code)))))))

(define (flatten x allocation self self-label fix-labels emit-code)
  (define (emit-label label)
    (emit-code #f (make-glil-label label)))
  (define (emit-branch src inst label)
    (emit-code src (make-glil-branch inst label)))

  ;; RA: "return address"; #f unless we're in a non-tail fix with labels
  ;; MVRA: "multiple-values return address"; #f unless we're in a let-values
  (let comp ((x x) (context 'tail) (RA #f) (MVRA #f))
    (define (comp-tail tree) (comp tree context RA MVRA))
    (define (comp-push tree) (comp tree 'push #f #f))
    (define (comp-drop tree) (comp tree 'drop #f #f))
    (define (comp-vals tree MVRA) (comp tree 'vals #f MVRA))
    (define (comp-fix tree RA) (comp tree context RA MVRA))

    ;; A couple of helpers. Note that if we are in tail context, we
    ;; won't have an RA.
    (define (maybe-emit-return)
      (if RA
          (emit-branch #f 'br RA)
          (if (eq? context 'tail)
              (emit-code #f (make-glil-call 'return 1)))))
    
    (record-case x
      ((<void>)
       (case context
         ((push vals tail)
          (emit-code #f (make-glil-void))))
       (maybe-emit-return))

      ((<const> src exp)
       (case context
         ((push vals tail)
          (emit-code src (make-glil-const exp))))
       (maybe-emit-return))

      ;; FIXME: should represent sequence as exps tail
      ((<sequence> exps)
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
               ((drop) (for-each comp-drop args) (maybe-emit-return))
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
                (emit-code src (make-glil-call 'new-frame 0))
                (comp-push proc)
                (for-each comp-push args)
                (emit-code src (make-glil-call 'apply (1+ (length args))))
                (maybe-emit-return))
               ((vals)
                (comp-vals
                 (make-application src (make-primitive-ref #f 'apply)
                                   (cons proc args))
                 MVRA)
                (maybe-emit-return))
               ((drop)
                ;; Well, shit. The proc might return any number of
                ;; values (including 0), since it's in a drop context,
                ;; yet apply does not create a MV continuation. So we
                ;; mv-call out to our trampoline instead.
                (comp-drop
                 (make-application src (make-primitive-ref #f 'apply)
                                   (cons proc args)))
                (maybe-emit-return)))))))
        
        ((and (primitive-ref? proc) (eq? (primitive-ref-name proc) 'values)
              (not (eq? context 'push)))
         ;; tail: (lambda () (values '(1 2)))
         ;; drop: (lambda () (values '(1 2)) 3)
         ;; push: (lambda () (list (values '(10 12)) 1))
         ;; vals: (let-values (((a b ...) (values 1 2 ...))) ...)
         (case context
           ((drop) (for-each comp-drop args) (maybe-emit-return))
           ((vals)
            (for-each comp-push args)
            (emit-code #f (make-glil-const (length args)))
            (emit-branch src 'br MVRA))
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
             MVRA)
            (maybe-emit-return))
           (else
            (let ((MV (make-label)) (POST (make-label))
                  (producer (car args)) (consumer (cadr args)))
              (if (not (eq? context 'tail))
                  (emit-code src (make-glil-call 'new-frame 0)))
              (comp-push consumer)
              (emit-code src (make-glil-call 'new-frame 0))
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
                            (emit-code #f (make-glil-call 'drop 1)))
                        (maybe-emit-return)))))))

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
             MVRA)
            (maybe-emit-return))
           ((push)
            (comp-push (car args))
            (emit-code src (make-glil-call 'call/cc 1))
            (maybe-emit-return))
           ((drop)
            ;; Crap. Just like `apply' in drop context.
            (comp-drop
             (make-application
              src (make-primitive-ref #f 'call-with-current-continuation)
              args))
            (maybe-emit-return))))

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
                   ((tail push vals) (emit-code #f (make-glil-void))))
                 (maybe-emit-return))
                ((1)
                 (case context
                   ((drop) (emit-code #f (make-glil-call 'drop 1))))
                 (maybe-emit-return))
                (else
                 (error "bad primitive op: too many pushes"
                        op (instruction-pushes op))))))
        
        ;; self-call in tail position
        ((and (lexical-ref? proc)
              self-label (eq? (lexical-ref-gensym proc) self-label)
              (eq? context 'tail))
         ;; first, evaluate new values, pushing them on the stack
         (for-each comp-push args)
         (let lp ((lcase (lambda-body self)))
           (cond
            ((and (lambda-case? lcase)
                  (not (lambda-case-kw lcase))
                  (not (lambda-case-opt lcase))
                  (not (lambda-case-rest lcase))
                  (= (length args) (length (lambda-case-req lcase))))
             ;; we have a case that matches the args; rename variables
             ;; and goto the case label
             (for-each (lambda (sym)
                         (pmatch (hashq-ref (hashq-ref allocation sym) self)
                           ((#t #f . ,index) ; unboxed
                            (emit-code #f (make-glil-lexical #t #f 'set index)))
                           ((#t #t . ,index) ; boxed
                            ;; new box
                            (emit-code #f (make-glil-lexical #t #t 'box index)))
                           (,x (error "what" x))))
                       (reverse (lambda-case-vars lcase)))
             (emit-branch src 'br (car (hashq-ref allocation lcase))))
            ((lambda-case? lcase)
             ;; no match, try next case
             (lp (lambda-case-else lcase)))
            (else
             ;; no cases left; shuffle args down and jump before the prelude.
             (for-each (lambda (i)
                         (emit-code #f (make-glil-lexical #t #f 'set index)))
                       (reverse (iota (length args))))
             (emit-branch src 'br self-label)))))
        
        ;; lambda, the ultimate goto
        ((and (lexical-ref? proc)
              (assq (lexical-ref-gensym proc) fix-labels))
         ;; like the self-tail-call case, though we can handle "drop"
         ;; contexts too. first, evaluate new values, pushing them on
         ;; the stack
         (for-each comp-push args)
         ;; find the specific case, rename args, and goto the case label
         (let lp ((lcase (lambda-body
                          (assq-ref fix-labels (lexical-ref-gensym proc)))))
           (cond
            ((and (lambda-case? lcase)
                  (not (lambda-case-kw lcase))
                  (not (lambda-case-opt lcase))
                  (not (lambda-case-rest lcase))
                  (= (length args) (length (lambda-case-req lcase))))
             ;; we have a case that matches the args; rename variables
             ;; and goto the case label
             (for-each (lambda (sym)
                         (pmatch (hashq-ref (hashq-ref allocation sym) self)
                           ((#t #f . ,index) ; unboxed
                            (emit-code #f (make-glil-lexical #t #f 'set index)))
                           ((#t #t . ,index) ; boxed
                            (emit-code #f (make-glil-lexical #t #t 'box index)))
                           (,x (error "what" x))))
                       (reverse (lambda-case-vars lcase)))
             (emit-branch src 'br (car (hashq-ref allocation lcase))))
            ((lambda-case? lcase)
             ;; no match, try next case
             (lp (lambda-case-else lcase)))
            (else
             ;; no cases left. we can't really handle this currently.
             ;; ideally we would push on a new frame, then do a "local
             ;; call" -- which doesn't require consing up a program
             ;; object. but for now error, as this sort of case should
             ;; preclude label allocation.
             (error "couldn't find matching case for label call" x)))))
        
        (else
         (if (not (eq? context 'tail))
             (emit-code src (make-glil-call 'new-frame 0)))
         (comp-push proc)
         (for-each comp-push args)
         (let ((len (length args)))
           (case context
             ((tail) (emit-code src (make-glil-call 'goto/args len)))
             ((push) (emit-code src (make-glil-call 'call len))
                     (maybe-emit-return))
             ((vals) (emit-code src (make-glil-mv-call len MVRA))
                     (maybe-emit-return))
             ((drop) (let ((MV (make-label)) (POST (make-label)))
                       (emit-code src (make-glil-mv-call len MV))
                       (emit-code #f (make-glil-call 'drop 1))
                       (emit-branch #f 'br (or RA POST))
                       (emit-label MV)
                       (emit-code #f (make-glil-mv-bind '() #f))
                       (emit-code #f (make-glil-unbind))
                       (if RA
                           (emit-branch #f 'br RA)
                           (emit-label POST)))))))))

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
         ;; if there is an RA, comp-tail will cause a jump to it -- just
         ;; have to clean up here if there is no RA.
         (if (and (not RA) (not (eq? context 'tail)))
             (emit-branch #f 'br L2))
         (emit-label L1)
         (comp-tail else)
         (if (and (not RA) (not (eq? context 'tail)))
             (emit-label L2))))
      
      ((<primitive-ref> src name)
       (cond
        ((eq? (module-variable (fluid-ref *comp-module*) name)
              (module-variable the-root-module name))
         (case context
           ((tail push vals)
            (emit-code src (make-glil-toplevel 'ref name))))
         (maybe-emit-return))
        ((module-variable the-root-module name)
         (case context
           ((tail push vals)
            (emit-code src (make-glil-module 'ref '(guile) name #f))))
         (maybe-emit-return))
        (else
         (case context
           ((tail push vals)
            (emit-code src (make-glil-module
                            'ref (module-name (fluid-ref *comp-module*)) name #f))))
         (maybe-emit-return))))

      ((<lexical-ref> src gensym)
       (case context
         ((push vals tail)
          (pmatch (hashq-ref (hashq-ref allocation gensym) self)
            ((,local? ,boxed? . ,index)
             (emit-code src (make-glil-lexical local? boxed? 'ref index)))
            (,loc
             (error "badness" x loc)))))
       (maybe-emit-return))
      
      ((<lexical-set> src gensym exp)
       (comp-push exp)
       (pmatch (hashq-ref (hashq-ref allocation gensym) self)
         ((,local? ,boxed? . ,index)
          (emit-code src (make-glil-lexical local? boxed? 'set index)))
         (,loc
          (error "badness" x loc)))
       (case context
         ((tail push vals)
          (emit-code #f (make-glil-void))))
       (maybe-emit-return))
      
      ((<module-ref> src mod name public?)
       (emit-code src (make-glil-module 'ref mod name public?))
       (case context
         ((drop) (emit-code #f (make-glil-call 'drop 1))))
       (maybe-emit-return))
      
      ((<module-set> src mod name public? exp)
       (comp-push exp)
       (emit-code src (make-glil-module 'set mod name public?))
       (case context
         ((tail push vals)
          (emit-code #f (make-glil-void))))
       (maybe-emit-return))

      ((<toplevel-ref> src name)
       (emit-code src (make-glil-toplevel 'ref name))
       (case context
         ((drop) (emit-code #f (make-glil-call 'drop 1))))
       (maybe-emit-return))
      
      ((<toplevel-set> src name exp)
       (comp-push exp)
       (emit-code src (make-glil-toplevel 'set name))
       (case context
         ((tail push vals)
          (emit-code #f (make-glil-void))))
       (maybe-emit-return))
      
      ((<toplevel-define> src name exp)
       (comp-push exp)
       (emit-code src (make-glil-toplevel 'define name))
       (case context
         ((tail push vals)
          (emit-code #f (make-glil-void))))
       (maybe-emit-return))

      ((<lambda>)
       (let ((free-locs (cdr (hashq-ref allocation x))))
         (case context
           ((push vals tail)
            (emit-code #f (flatten-lambda x #f allocation))
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
                  (emit-code #f (make-glil-call 'make-closure 2)))))))
       (maybe-emit-return))
      
      ((<lambda-case> req opt kw rest vars predicate else body)
       ;; the prelude, to check args & reset the stack pointer,
       ;; allowing room for locals
       (let ((nlocs (cdr (hashq-ref allocation x))))
         (if rest
             (emit-code #f (make-glil-opt-prelude (length req) 0 #t nlocs #f))
             (emit-code #f (make-glil-std-prelude (length req) nlocs #f))))
       ;; box args if necessary
       (for-each
        (lambda (v)
          (pmatch (hashq-ref (hashq-ref allocation v) self)
            ((#t #t . ,n)
             (emit-code #f (make-glil-lexical #t #f 'ref n))
             (emit-code #f (make-glil-lexical #t #t 'box n)))))
        vars)
       ;; write bindings info -- FIXME deal with opt/kw
       (if (not (null? vars))
           (emit-bindings #f (append req (if rest (list rest) '()))
                          vars allocation self emit-code))
       ;; post-prelude case label for label calls
       (emit-label (car (hashq-ref allocation x)))
       (let ((else-label (and else (make-label))))
         (if predicate
             (begin
               (comp-push predicate)
               (if else-label
                   ;; fixme: debox if necessary
                   (emit-branch src 'br-if-not else-label)
                   ;; fixme: better error
                   (emit-code src (make-glil-call 'assert-true 0)))))
         (comp-tail body)
         (if (not (null? vars))
             (emit-code #f (make-glil-unbind)))
         (if else-label
             (begin
               (emit-label else-label)
               (comp-tail else)))))
      
      ((<let> src names vars vals body)
       (for-each comp-push vals)
       (emit-bindings src names vars allocation self emit-code)
       (for-each (lambda (v)
                   (pmatch (hashq-ref (hashq-ref allocation v) self)
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
                   (pmatch (hashq-ref (hashq-ref allocation v) self)
                     ((#t #t . ,n)
                      (emit-code src (make-glil-lexical #t #t 'empty-box n)))
                     (,loc (error "badness" x loc))))
                 vars)
       (for-each comp-push vals)
       (emit-bindings src names vars allocation self emit-code)
       (for-each (lambda (v)
                   (pmatch (hashq-ref (hashq-ref allocation v) self)
                     ((#t #t . ,n)
                      (emit-code src (make-glil-lexical #t #t 'set n)))
                     (,loc (error "badness" x loc))))
                 (reverse vars))
       (comp-tail body)
       (emit-code #f (make-glil-unbind)))

      ((<fix> src names vars vals body)
       ;; The ideal here is to just render the lambda bodies inline, and
       ;; wire the code together with gotos. We can do that if
       ;; analyze-lexicals has determined that a given var has "label"
       ;; allocation -- which is the case if it is in `fix-labels'.
       ;;
       ;; But even for closures that we can't inline, we can do some
       ;; tricks to avoid heap-allocation for the binding itself. Since
       ;; we know the vals are lambdas, we can set them to their local
       ;; var slots first, then capture their bindings, mutating them in
       ;; place.
       (let ((new-RA (if (or (eq? context 'tail) RA) #f (make-label))))
         (for-each
          (lambda (x v)
            (cond
             ((hashq-ref allocation x)
              ;; allocating a closure
              (emit-code #f (flatten-lambda x v allocation))
              (if (not (null? (cdr (hashq-ref allocation x))))
                  ;; Need to make-closure first, but with a temporary #f
                  ;; free-variables vector, so we are mutating fresh
                  ;; closures on the heap.
                  (begin
                    (emit-code #f (make-glil-const #f))
                    (emit-code #f (make-glil-call 'make-closure 2))))
              (pmatch (hashq-ref (hashq-ref allocation v) self)
                ((#t #f . ,n)
                 (emit-code src (make-glil-lexical #t #f 'set n)))
                (,loc (error "badness" x loc))))
             (else
              ;; labels allocation: emit label & body, but jump over it
              (let ((POST (make-label)))
                (emit-branch #f 'br POST)
                (let lp ((lcase (lambda-body x)))
                  (if lcase
                      (record-case lcase
                        ((<lambda-case> src req vars body else)
                         (emit-label (car (hashq-ref allocation lcase)))
                         ;; FIXME: opt & kw args in the bindings
                         (emit-bindings #f req vars allocation self emit-code)
                         (if src
                             (emit-code #f (make-glil-source src)))
                         (comp-fix body (or RA new-RA))
                         (emit-code #f (make-glil-unbind))
                         (lp else)))
                      (emit-label POST)))))))
          vals
          vars)
         ;; Emit bindings metadata for closures
         (let ((binds (let lp ((out '()) (vars vars) (names names))
                        (cond ((null? vars) (reverse! out))
                              ((assq (car vars) fix-labels)
                               (lp out (cdr vars) (cdr names)))
                              (else
                               (lp (acons (car vars) (car names) out)
                                   (cdr vars) (cdr names)))))))
           (emit-bindings src (map cdr binds) (map car binds)
                          allocation self emit-code))
         ;; Now go back and fix up the bindings for closures.
         (for-each
          (lambda (x v)
            (let ((free-locs (if (hashq-ref allocation x)
                                 (cdr (hashq-ref allocation x))
                                 ;; can hit this latter case for labels allocation
                                 '())))
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
                    (pmatch (hashq-ref (hashq-ref allocation v) self)
                      ((#t #f . ,n)
                       (emit-code #f (make-glil-lexical #t #f 'fix n)))
                      (,loc (error "badness" x loc)))))))
          vals
          vars)
         (comp-tail body)
         (if new-RA
             (emit-label new-RA))
         (emit-code #f (make-glil-unbind))))

      ((<let-values> src exp body)
       (record-case body
         ((<lambda-case> req opt kw rest vars predicate body else)
          (if (or opt kw predicate else)
              (error "unexpected lambda-case in let-values" x))
          (let ((MV (make-label)))
            (comp-vals exp MV)
            (emit-code #f (make-glil-const 1))
            (emit-label MV)
            (emit-code src (make-glil-mv-bind
                            (vars->bind-list
                             (append req (if rest (list rest) '()))
                             vars allocation self)
                            (and rest #t)))
            (for-each (lambda (v)
                        (pmatch (hashq-ref (hashq-ref allocation v) self)
                          ((#t #f . ,n)
                           (emit-code src (make-glil-lexical #t #f 'set n)))
                          ((#t #t . ,n)
                           (emit-code src (make-glil-lexical #t #t 'box n)))
                          (,loc (error "badness" x loc))))
                      (reverse vars))
            (comp-tail body)
            (emit-code #f (make-glil-unbind)))))))))
