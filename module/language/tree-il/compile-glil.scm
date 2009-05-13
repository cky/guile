;;; TREE-IL -> GLIL compiler

;; Copyright (C) 2001,2008,2009 Free Software Foundation, Inc.

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

(define-module (language tree-il compile-glil)
  #:use-module (system base syntax)
  #:use-module (language glil)
  #:use-module (language tree-il)
  #:use-module (language tree-il optimize)
  #:use-module (ice-9 common-list)
  #:export (compile-glil))

;; parents: lambda -> parent
;;  useful when we see a closed-over var, so we can calculate its
;;  coordinates (depth and index).
;; bindings: lambda -> (sym ...)
;;  useful for two reasons: one, so we know how much space to allocate
;;  when we go into a lambda; and two, so that we know when to stop,
;;  when looking for closed-over vars.
;; heaps: sym -> lambda
;;  allows us to heapify vars in an O(1) fashion

;; allocation: the process of assigning a type and index to each var
;; a var is external if it is heaps; assigning index is easy
;; args are assigned in order
;; locals are indexed as their linear position in the binding path
;; (let (0 1)
;;   (let (2 3) ...)
;;   (let (2) ...))
;;   (let (2 3 4) ...))
;; etc.

;; allocation:
;;  sym -> (local . index) | (heap level . index)


(define (analyze-lexicals x)
  (define (find-diff parent this)
    (let lp ((parent parent) (n 0))
      (if (eq? parent this)
          n
          (lp (hashq-ref parents parent) (1+ n)))))

  (define (find-heap sym parent)
    ;; fixme: check displaced lexicals here?
    (if (memq sym (hashq-ref bindings parent))
        parent
        (find-binder sym (hashq-ref parents parent))))

  (define (analyze! x parent level)
    (define (step y) (analyze! y parent level))
    (define (recur x parent) (analyze! x parent (1+ level)))
    (record-case x
      ((<application> proc args)
       (step proc) (for-each step args))

      ((<conditional> test then else)
       (step test) (step then) (step else))

      ((<lexical-ref> name gensym)
       (if (and (not (memq gensym (hashq-ref bindings parent)))
                (not (hashq-ref heaps gensym)))
           (hashq-set! heaps gensym (find-heap gensym parent level))))
      
      ((<lexical-set> name gensym exp)
       (step exp)
       (if (not (hashq-ref heaps gensym))
           (hashq-set! heaps gensym (find-heap gensym parent level))))
      
      ((<module-set> mod name public? exp)
       (step exp))
      
      ((<toplevel-set> name exp)
       (step exp))
      
      ((<toplevel-define> name exp)
       (step exp))
      
      ((<sequence> exps)
       (for-each step exps))
      
      ((<lambda> vars meta body)
       (hashq-set! parents x parent)
       (hashq-set! bindings x
                   (let rev* ((vars vars) (out '()))
                     (cond ((null? vars) out)
                           ((pair? vars) (rev* (cdr vars)
                                               (cons (car vars) out)))
                           (else (cons vars out)))))
       (recur body x)
       (hashq-set! bindings x (reverse! (hashq-ref bindings x))))

      ((<let> vars vals exp)
       (for-each step vals)
       (hashq-set! bindings parent
                   (append (reverse vars) (hashq-ref bindings parent)))
       (step exp))
      
      ((<letrec> vars vals exp)
       (hashq-set! bindings parent
                   (append (reverse vars) (hashq-ref bindings parent)))
       (for-each step vals)
       (step exp))

      (else #f)))

    (define (allocate-heap! binder)
      (hashq-set! heap-indexes binder
                  (1+ (hashq-ref heap-indexes binder -1))))

    (define (allocate! x level n)
      (define (step y) (allocate! y level n))
      (record-case x
        ((<application> proc args)
         (step proc) (for-each step args))

        ((<conditional> test then else)
         (step test) (step then) (step else))

        ((<lexical-set> name gensym exp)
         (step exp))
        
        ((<module-set> mod name public? exp)
         (step exp))
        
        ((<toplevel-set> name exp)
         (step exp))
        
        ((<toplevel-define> name exp)
         (step exp))
        
        ((<sequence> exps)
         (for-each step exps))
        
        ((<lambda> vars meta body)
         (let lp ((vars vars) (n 0))
           (if (null? vars)
               (allocate! body (1+ level) n)
               (let ((v (if (pair? vars) (car vars) vars)))
                 (let ((binder (hashq-ref heaps v)))
                   (hashq-set!
                    allocation v
                    (if binder
                        (cons* 'heap (1+ level) (allocate-heap! binder))
                        (cons 'stack n))))
                 (lp (if (pair? vars) (cdr vars) '()) (1+ n))))))

        ((<let> vars vals exp)
         (for-each step vals)
         (let lp ((vars vars) (n n))
           (if (null? vars)
               (allocate! exp level n)
               (let ((v (car vars)))
                 (let ((binder (hashq-ref heaps v)))
                   (hashq-set!
                    allocation v
                    (if binder
                        (cons* 'heap level (allocate-heap! binder))
                        (cons 'stack n))))
                 (lp (cdr vars) (1+ n))))))
        
        ((<letrec> vars vals exp)
         (let lp ((vars vars) (n n))
           (if (null? vars)
               (begin
                 (for-each (lambda (x) (allocate! x level n))
                           vals)
                 (allocate! exp level n))
               (let ((v (car vars)))
                 (let ((binder (hashq-ref heaps v)))
                   (hashq-set!
                    allocation v
                    (if binder
                        (cons* 'heap level (allocate-heap! binder))
                        (cons 'stack n))))
                 (lp (cdr vars) (1+ n))))))

        (else #f)))

  (define parents (make-hash-table))
  (define bindings (make-hash-table))
  (define heaps (make-hash-table))
  (define allocation (make-hash-table))
  (define heap-indexes (make-hash-table))

  (hashq-set! bindings #f '())
  (analyze! x #f 0)
  (allocate! x 0 0)

  allocation)

(define (compile-glil x e opts)
  (let ((x (optimize! x e opts)))
    (let ((allocation (analyze-lexicals x)))
      (values (codegen (make-lambda (tree-il-src x) '() '() x)
                       allocation)
              (and e (cons (car e) (cddr e)))
              e))))



(define *ia-void* (make-glil-void))
(define *ia-drop* (make-glil-call 'drop 1))
(define *ia-return* (make-glil-call 'return 1))

(define (make-label) (gensym ":L"))

(define (make-glil-var op env var)
  (case (ghil-var-kind var)
    ((argument)
     (make-glil-argument op (ghil-var-index var)))
    ((local)
     (make-glil-local op (ghil-var-index var)))
    ((external)
     (do ((depth 0 (1+ depth))
	  (e env (ghil-env-parent e)))
	 ((eq? e (ghil-var-env var))
	  (make-glil-external op depth (ghil-var-index var)))))
    ((toplevel)
     (make-glil-toplevel op (ghil-var-name var)))
    ((public private)
     (make-glil-module op (ghil-var-env var) (ghil-var-name var)
                       (eq? (ghil-var-kind var) 'public)))
    (else (error "Unknown kind of variable:" var))))


(define (codegen x)
  (define stack '())
  (define (push-code! src code)
    (set! stack (cons code stack))
    (if src (set! stack (cons (make-glil-source src) stack))))
  (define (var->binding var)
    (list (ghil-var-name var) (ghil-var-kind var) (ghil-var-index var)))
  (define (push-bindings! src vars)
    (if (not (null? vars))
        (push-code! src (make-glil-bind (map var->binding vars)))))
  (define (comp tree tail drop)
    (define (push-label! label)
      (push-code! #f (make-glil-label label)))
    (define (push-branch! src inst label)
      (push-code! src (make-glil-branch inst label)))
    (define (push-call! src inst args)
      (for-each comp-push args)
      (push-code! src (make-glil-call inst (length args))))
    ;; possible tail position
    (define (comp-tail tree) (comp tree tail drop))
    ;; push the result
    (define (comp-push tree) (comp tree #f #f))
    ;; drop the result
    (define (comp-drop tree) (comp tree #f #t))
    ;; drop the result if unnecessary
    (define (maybe-drop)
      (if drop (push-code! #f *ia-drop*)))
    ;; return here if necessary
    (define (maybe-return)
      (if tail (push-code! #f *ia-return*)))
    ;; return this code if necessary
    (define (return-code! src code)
      (if (not drop) (push-code! src code))
      (maybe-return))
    ;; return void if necessary
    (define (return-void!)
      (return-code! #f *ia-void*))
    ;; return object if necessary
    (define (return-object! src obj)
      (return-code! src (make-glil-const obj)))
    ;;
    ;; dispatch
    (record-case tree
      ((<ghil-void>)
       (return-void!))

      ((<ghil-quote> env src obj)
       (return-object! src obj))

      ((<ghil-ref> env src var)
       (return-code! src (make-glil-var 'ref env var)))

      ((<ghil-set> env src var val)
       (comp-push val)
       (push-code! src (make-glil-var 'set env var))
       (return-void!))

      ((<toplevel-define> src name exp)
       (comp-push exp)
       (push-code! src (make-glil-var 'define env var))
       (return-void!))

      ((<conditional> src test then else)
       ;;     TEST
       ;;     (br-if-not L1)
       ;;     THEN
       ;;     (br L2)
       ;; L1: ELSE
       ;; L2:
       (let ((L1 (make-label)) (L2 (make-label)))
         (comp-push test)
         (push-branch! src 'br-if-not L1)
         (comp-tail then)
         (if (not tail) (push-branch! #f 'br L2))
         (push-label! L1)
         (comp-tail else)
         (if (not tail) (push-label! L2))))

      ((<sequence> src exps)
       ;; EXPS...
       ;; TAIL
       (if (null? exps)
           (return-void!)
           (do ((exps exps (cdr exps)))
      	 ((null? (cdr exps))
      	  (comp-tail (car exps)))
             (comp-drop (car exps)))))

      ((<let> src vars vals body)
       ;; VALS...
       ;; (set VARS)...
       ;; BODY
       (for-each comp-push vals)
       (push-bindings! src vars)
       (for-each (lambda (var) (push-code! #f (make-glil-var 'set env var)))
      	   (reverse vars))
       (comp-tail body)
       (push-code! #f (make-glil-unbind)))

      ((<ghil-mv-bind> env src producer vars rest body)
       ;; VALS...
       ;; (set VARS)...
       ;; BODY
       (let ((MV (make-label)))
         (comp-push producer)
         (push-code! src (make-glil-mv-call 0 MV))
         (push-code! #f (make-glil-const 1))
         (push-label! MV)
         (push-code! #f (make-glil-mv-bind (map var->binding vars) rest))
         (for-each (lambda (var) (push-code! #f (make-glil-var 'set env var)))
                   (reverse vars)))
       (comp-tail body)
       (push-code! #f (make-glil-unbind)))

      ((<ghil-lambda> env src vars rest meta body)
       (return-code! src (codegen tree)))

      ((<ghil-inline> env src inline args)
       ;; ARGS...
       ;; (INST NARGS)
       (let ((tail-table '((call . goto/args)
                           (apply . goto/apply)
                           (call/cc . goto/cc))))
         (cond ((and tail (assq-ref tail-table inline))
                => (lambda (tail-inst)
                     (push-call! src tail-inst args)))
               (else
                (push-call! src inline args)
                (maybe-drop)
                (maybe-return)))))

      ((<ghil-values> env src values)
       (cond (tail ;; (lambda () (values 1 2))
              (push-call! src 'return/values values))
             (drop ;; (lambda () (values 1 2) 3)
              (for-each comp-drop values))
             (else ;; (lambda () (list (values 10 12) 1))
              (push-code! #f (make-glil-const 'values))
              (push-code! #f (make-glil-call 'link-now 1))
              (push-code! #f (make-glil-call 'variable-ref 0))
              (push-call! src 'call values))))
              
      ((<ghil-values*> env src values)
       (cond (tail ;; (lambda () (apply values '(1 2)))
              (push-call! src 'return/values* values))
             (drop ;; (lambda () (apply values '(1 2)) 3)
              (for-each comp-drop values))
             (else ;; (lambda () (list (apply values '(10 12)) 1))
              (push-code! #f (make-glil-const 'values))
              (push-code! #f (make-glil-call 'link-now 1))
              (push-code! #f (make-glil-call 'variable-ref 0))
              (push-call! src 'apply values))))
              
      ((<ghil-call> env src proc args)
       ;; PROC
       ;; ARGS...
       ;; ([tail-]call NARGS)
       (comp-push proc)
       (let ((nargs (length args)))
         (cond ((< nargs 255)
                (push-call! src (if tail 'goto/args 'call) args))
               (else
                (push-call! src 'mark '())
                (for-each comp-push args)
                (push-call! src 'list-mark '())
                (push-code! src (make-glil-call (if tail 'goto/apply 'apply) 2)))))
       (maybe-drop))

      ((<ghil-mv-call> env src producer consumer)
       ;; CONSUMER
       ;; PRODUCER
       ;; (mv-call MV)
       ;; ([tail]-call 1)
       ;; goto POST
       ;; MV: [tail-]call/nargs
       ;; POST: (maybe-drop)
       (let ((MV (make-label)) (POST (make-label)))
         (comp-push consumer)
         (comp-push producer)
         (push-code! src (make-glil-mv-call 0 MV))
         (push-code! src (make-glil-call (if tail 'goto/args 'call) 1))
         (cond ((not tail)
                (push-branch! #f 'br POST)))
         (push-label! MV)
         (push-code! src (make-glil-call (if tail 'goto/nargs 'call/nargs) 0))
         (cond ((not tail)
                (push-label! POST)
                (maybe-drop)))))

      ((<ghil-reified-env> env src)
       (return-object! src (ghil-env-reify env)))))

  ;;
  ;; main
  ;;

  ;; analyze vars: partition into args, locs, exts, and assign indices
  (record-case x
    ((<ghil-lambda> env src vars rest meta body)
     (let* ((evars (ghil-env-variables env))
            (srcs (pick (lambda (v) (eq? (ghil-var-kind v) 'local)) evars))
            (exts (pick (lambda (v) (eq? (ghil-var-kind v) 'external)) evars))
            (nargs (allocate-indices-linearly! vars))
            (nlocs (allocate-locals! locs body))
            (nexts (allocate-indices-linearly! exts)))
       ;; meta bindings
       (push-bindings! #f vars)
       ;; push on definition source location
       (if src (set! stack (cons (make-glil-source src) stack)))
       ;; copy args to the heap if they're marked as external
       (do ((n 0 (1+ n))
            (l vars (cdr l)))
           ((null? l))
         (let ((v (car l)))
           (case (ghil-var-kind v)
             ((external)
              (push-code! #f (make-glil-argument 'ref n))
              (push-code! #f (make-glil-external 'set 0 (ghil-var-index v)))))))
       ;; compile body
       (comp body #t #f)
       ;; create GLIL
       (make-glil-program nargs (if rest 1 0) nlocs nexts meta
                          (reverse! stack))))))

(define (allocate-indices-linearly! vars)
  (do ((n 0 (1+ n))
       (l vars (cdr l)))
      ((null? l) n)
    (let ((v (car l))) (set! (ghil-var-index v) n))))

(define (allocate-locals! vars body)
  (let ((free '()) (nlocs 0))
    (define (allocate! var)
      (cond
       ((pair? free)
        (set! (ghil-var-index var) (car free))
        (set! free (cdr free)))
       (else
        (set! (ghil-var-index var) nlocs)
        (set! nlocs (1+ nlocs)))))
    (define (deallocate! var)
      (set! free (cons (ghil-var-index var) free)))
    (let lp ((x body))
      (record-case x
        ((<ghil-void>))
        ((<ghil-quote>))
	((<ghil-quasiquote> exp)
	 (let qlp ((x exp))
           (cond ((list? x) (for-each qlp x))
                 ((pair? x) (qlp (car x)) (qlp (cdr x)))
                 ((record? x)
                  (record-case x
                   ((<ghil-unquote> exp) (lp exp))
                   ((<ghil-unquote-splicing> exp) (lp exp)))))))
        ((<ghil-unquote> exp)
         (lp exp))
        ((<ghil-unquote-splicing> exp)
         (lp exp))
        ((<ghil-reified-env>))
        ((<ghil-set> val)
         (lp val))
        ((<ghil-ref>))
        ((<ghil-define> val)
         (lp val))
        ((<ghil-if> test then else)
         (lp test) (lp then) (lp else))
        ((<ghil-and> exps)
         (for-each lp exps))
        ((<ghil-or> exps)
         (for-each lp exps))
        ((<ghil-begin> exps)
         (for-each lp exps))
        ((<ghil-bind> vars vals body)
         (for-each allocate! vars)
         (for-each lp vals)
         (lp body)
         (for-each deallocate! vars))
        ((<ghil-mv-bind> vars producer body)
         (lp producer)
         (for-each allocate! vars)
         (lp body)
         (for-each deallocate! vars))
        ((<ghil-inline> args)
         (for-each lp args))
        ((<ghil-call> proc args)
         (lp proc)
         (for-each lp args))
        ((<ghil-lambda>))
        ((<ghil-mv-call> producer consumer)
         (lp producer)
         (lp consumer))
        ((<ghil-values> values)
         (for-each lp values))
        ((<ghil-values*> values)
         (for-each lp values))))
    nlocs))
