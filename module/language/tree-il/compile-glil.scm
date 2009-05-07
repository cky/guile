;;; TREE-IL -> GLIL compiler

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

(define-module (language tree-il compile-glil)
  #:use-module (system base syntax)
  #:use-module (language glil)
  #:use-module (language tree-il)
  #:use-module (ice-9 common-list)
  #:export (compile-glil))

(define (compile-glil x e opts)
  (if (memq #:O opts) (set! x (optimize x)))
  (values (codegen x)
          (and e (cons (car e) (cddr e)))
          e))


;;;
;;; Stage 2: Optimization
;;;

(define (lift-variables! env)
  (let ((parent-env (ghil-env-parent env)))
    (for-each (lambda (v)
                (case (ghil-var-kind v)
                  ((argument) (set! (ghil-var-kind v) 'local)))
                (set! (ghil-var-env v) parent-env)
                (ghil-env-add! parent-env v))
              (ghil-env-variables env))))

;; The premise of this, unused, approach to optimization is that you can
;; determine the environment of a variable lexically, because they have
;; been alpha-renamed. It makes the transformations *much* easier.
;; Unfortunately it doesn't work yet.
(define (optimize* x)
  (transform-record (<ghil> env loc) x
    ((quasiquote exp)
     (define (optimize-qq x)
       (cond ((list? x) (map optimize-qq x))
             ((pair? x) (cons (optimize-qq (car x)) (optimize-qq (cdr x))))
             ((record? x) (optimize x))
             (else x)))
     (-> (quasiquote (optimize-qq x))))

    ((unquote exp)
     (-> (unquote (optimize exp))))

    ((unquote-splicing exp)
     (-> (unquote-splicing (optimize exp))))

    ((set var val)
     (-> (set var (optimize val))))

    ((define var val)
     (-> (define var (optimize val))))

    ((if test then else)
     (-> (if (optimize test) (optimize then) (optimize else))))

    ((and exps)
     (-> (and (map optimize exps))))

    ((or exps)
     (-> (or (map optimize exps))))

    ((begin exps)
     (-> (begin (map optimize exps))))

    ((bind vars vals body)
     (-> (bind vars (map optimize vals) (optimize body))))

    ((mv-bind producer vars rest body)
     (-> (mv-bind (optimize producer) vars rest (optimize body))))

    ((inline inst args)
     (-> (inline inst (map optimize args))))

    ((call (proc (lambda vars (rest #f) meta body)) args)
     (-> (bind vars (optimize args) (optimize body))))

    ((call proc args)
     (-> (call (optimize proc) (map optimize args))))

    ((lambda vars rest meta body)
     (-> (lambda vars rest meta (optimize body))))

    ((mv-call producer (consumer (lambda vars rest meta body)))
     (-> (mv-bind (optimize producer) vars rest (optimize body))))

    ((mv-call producer consumer)
     (-> (mv-call (optimize producer) (optimize consumer))))

    ((values values)
     (-> (values (map optimize values))))

    ((values* values)
     (-> (values* (map optimize values))))

    (else
     (error "unrecognized GHIL" x))))

(define (optimize x)
  (record-case x
    ((<ghil-set> env loc var val)
     (make-ghil-set env var (optimize val)))

    ((<ghil-define> env loc var val)
     (make-ghil-define env var (optimize val)))

    ((<ghil-if> env loc test then else)
     (make-ghil-if env loc (optimize test) (optimize then) (optimize else)))

    ((<ghil-and> env loc exps)
     (make-ghil-and env loc (map optimize exps)))

    ((<ghil-or> env loc exps)
     (make-ghil-or env loc (map optimize exps)))

    ((<ghil-begin> env loc exps)
     (make-ghil-begin env loc (map optimize exps)))

    ((<ghil-bind> env loc vars vals body)
     (make-ghil-bind env loc vars (map optimize vals) (optimize body)))

    ((<ghil-lambda> env loc vars rest meta body)
     (make-ghil-lambda env loc vars rest meta (optimize body)))

    ((<ghil-inline> env loc instruction args)
     (make-ghil-inline env loc instruction (map optimize args)))

    ((<ghil-call> env loc proc args)
     (let ((parent-env env))
       (record-case proc
         ;; ((@lambda (VAR...) BODY...) ARG...) =>
         ;;   (@let ((VAR ARG) ...) BODY...)
         ((<ghil-lambda> env loc vars rest meta body)
          (cond
           ((not rest)
            (lift-variables! env)
            (make-ghil-bind parent-env loc (map optimize args)))
           (else
            (make-ghil-call parent-env loc (optimize proc) (map optimize args)))))
         (else
          (make-ghil-call parent-env loc (optimize proc) (map optimize args))))))

    ((<ghil-mv-call> env loc producer consumer)
     (record-case consumer
      ;; (mv-call PRODUCER (lambda ARGS BODY...)) =>
      ;;   (mv-let PRODUCER ARGS BODY...)
      ((<ghil-lambda> env loc vars rest meta body)
       (lift-variables! env)
       (make-ghil-mv-bind producer vars rest body))
      (else
       (make-ghil-mv-call env loc (optimize producer) (optimize consumer)))))

    (else x)))


;;;
;;; Stage 3: Code generation
;;;

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

(define (constant? x)
  (cond ((or (number? x) (string? x) (symbol? x) (keyword? x) (boolean? x)) #t)
        ((pair? x) (and (constant? (car x))
                        (constant? (cdr x))))
        ((vector? x) (let lp ((i (vector-length x)))
                       (or (zero? i)
                           (and (constant? (vector-ref x (1- i)))
                                (lp (1- i))))))))

(define (codegen ghil)
  (let ((stack '()))
    (define (push-code! loc code)
      (set! stack (cons code stack))
      (if loc (set! stack (cons (make-glil-source loc) stack))))
    (define (var->binding var)
      (list (ghil-var-name var) (ghil-var-kind var) (ghil-var-index var)))
    (define (push-bindings! loc vars)
      (if (not (null? vars))
          (push-code! loc (make-glil-bind (map var->binding vars)))))
    (define (comp tree tail drop)
      (define (push-label! label)
	(push-code! #f (make-glil-label label)))
      (define (push-branch! loc inst label)
	(push-code! loc (make-glil-branch inst label)))
      (define (push-call! loc inst args)
	(for-each comp-push args)
	(push-code! loc (make-glil-call inst (length args))))
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
      (define (return-code! loc code)
	(if (not drop) (push-code! loc code))
	(maybe-return))
      ;; return void if necessary
      (define (return-void!)
	(return-code! #f *ia-void*))
      ;; return object if necessary
      (define (return-object! loc obj)
	(return-code! loc (make-glil-const obj)))
      ;;
      ;; dispatch
      (record-case tree
	((<ghil-void>)
	 (return-void!))

	((<ghil-quote> env loc obj)
	 (return-object! loc obj))

	((<ghil-quasiquote> env loc exp)
	 (let loop ((x exp) (in-car? #f))
           (cond
            ((list? x)
             (push-call! #f 'mark '())
             (for-each (lambda (x) (loop x #t)) x)
             (push-call! #f 'list-mark '()))
            ((pair? x)
             (push-call! #f 'mark '())
             (loop (car x) #t)
             (loop (cdr x) #f)
             (push-call! #f 'cons-mark '()))
            ((record? x)
             (record-case x
              ((<ghil-unquote> env loc exp)
               (comp-push exp))
              ((<ghil-unquote-splicing> env loc exp)
               (if (not in-car?)
                   (error "unquote-splicing in the cdr of a pair" exp))
               (comp-push exp)
               (push-call! #f 'list-break '()))))
            ((constant? x)
             (push-code! #f (make-glil-const x)))
            (else
             (error "element of quasiquote can't be compiled" x))))
	 (maybe-drop)
	 (maybe-return))

	((<ghil-unquote> env loc exp)
         (error "unquote outside of quasiquote" exp))

	((<ghil-unquote-splicing> env loc exp)
         (error "unquote-splicing outside of quasiquote" exp))

	((<ghil-ref> env loc var)
	 (return-code! loc (make-glil-var 'ref env var)))

	((<ghil-set> env loc var val)
	 (comp-push val)
	 (push-code! loc (make-glil-var 'set env var))
	 (return-void!))

	((<ghil-define> env loc var val)
	 (comp-push val)
	 (push-code! loc (make-glil-var 'define env var))
	 (return-void!))

	((<ghil-if> env loc test then else)
	 ;;     TEST
	 ;;     (br-if-not L1)
	 ;;     THEN
	 ;;     (br L2)
	 ;; L1: ELSE
	 ;; L2:
	 (let ((L1 (make-label)) (L2 (make-label)))
	   (comp-push test)
	   (push-branch! loc 'br-if-not L1)
	   (comp-tail then)
	   (if (not tail) (push-branch! #f 'br L2))
	   (push-label! L1)
	   (comp-tail else)
	   (if (not tail) (push-label! L2))))

	((<ghil-and> env loc exps)
	 ;;     EXP
	 ;;     (br-if-not L1)
	 ;;     ...
	 ;;     TAIL
	 ;;     (br L2)
	 ;; L1: (const #f)
	 ;; L2:
         (cond ((null? exps) (return-object! loc #t))
               ((null? (cdr exps)) (comp-tail (car exps)))
               (else
                (let ((L1 (make-label)) (L2 (make-label)))
                  (let lp ((exps exps))
                    (cond ((null? (cdr exps))
                           (comp-tail (car exps))
                           (push-branch! #f 'br L2)
                           (push-label! L1)
                           (return-object! #f #f)
                           (push-label! L2)
                           (maybe-return))
                          (else
                           (comp-push (car exps))
                           (push-branch! #f 'br-if-not L1)
                           (lp (cdr exps)))))))))

	((<ghil-or> env loc exps)
	 ;;     EXP
	 ;;     (dup)
	 ;;     (br-if L1)
	 ;;     (drop)
	 ;;     ...
	 ;;     TAIL
	 ;; L1:
         (cond ((null? exps) (return-object! loc #f))
               ((null? (cdr exps)) (comp-tail (car exps)))
               (else
                (let ((L1 (make-label)))
                  (let lp ((exps exps))
                    (cond ((null? (cdr exps))
                           (comp-tail (car exps))
                           (push-label! L1)
                           (maybe-return))
                          (else
                           (comp-push (car exps))
                           (if (not drop)
                               (push-call! #f 'dup '()))
                           (push-branch! #f 'br-if L1)
                           (if (not drop)
                               (push-code! loc (make-glil-call 'drop 1)))
                           (lp (cdr exps)))))))))

	((<ghil-begin> env loc exps)
	 ;; EXPS...
	 ;; TAIL
	 (if (null? exps)
	     (return-void!)
	     (do ((exps exps (cdr exps)))
		 ((null? (cdr exps))
		  (comp-tail (car exps)))
	       (comp-drop (car exps)))))

	((<ghil-bind> env loc vars vals body)
	 ;; VALS...
	 ;; (set VARS)...
	 ;; BODY
	 (for-each comp-push vals)
         (push-bindings! loc vars)
	 (for-each (lambda (var) (push-code! #f (make-glil-var 'set env var)))
		   (reverse vars))
	 (comp-tail body)
	 (push-code! #f (make-glil-unbind)))

	((<ghil-mv-bind> env loc producer vars rest body)
	 ;; VALS...
	 ;; (set VARS)...
	 ;; BODY
         (let ((MV (make-label)))
           (comp-push producer)
           (push-code! loc (make-glil-mv-call 0 MV))
           (push-code! #f (make-glil-const 1))
           (push-label! MV)
           (push-code! #f (make-glil-mv-bind (map var->binding vars) rest))
           (for-each (lambda (var) (push-code! #f (make-glil-var 'set env var)))
                     (reverse vars)))
         (comp-tail body)
         (push-code! #f (make-glil-unbind)))

	((<ghil-lambda> env loc vars rest meta body)
	 (return-code! loc (codegen tree)))

	((<ghil-inline> env loc inline args)
	 ;; ARGS...
	 ;; (INST NARGS)
         (let ((tail-table '((call . goto/args)
                             (apply . goto/apply)
                             (call/cc . goto/cc))))
           (cond ((and tail (assq-ref tail-table inline))
                  => (lambda (tail-inst)
                       (push-call! loc tail-inst args)))
                 (else
                  (push-call! loc inline args)
                  (maybe-drop)
                  (maybe-return)))))

        ((<ghil-values> env loc values)
         (cond (tail ;; (lambda () (values 1 2))
                (push-call! loc 'return/values values))
               (drop ;; (lambda () (values 1 2) 3)
                (for-each comp-drop values))
               (else ;; (lambda () (list (values 10 12) 1))
                (push-code! #f (make-glil-const 'values))
                (push-code! #f (make-glil-call 'link-now 1))
                (push-code! #f (make-glil-call 'variable-ref 0))
                (push-call! loc 'call values))))
                
        ((<ghil-values*> env loc values)
         (cond (tail ;; (lambda () (apply values '(1 2)))
                (push-call! loc 'return/values* values))
               (drop ;; (lambda () (apply values '(1 2)) 3)
                (for-each comp-drop values))
               (else ;; (lambda () (list (apply values '(10 12)) 1))
                (push-code! #f (make-glil-const 'values))
                (push-code! #f (make-glil-call 'link-now 1))
                (push-code! #f (make-glil-call 'variable-ref 0))
                (push-call! loc 'apply values))))
                
	((<ghil-call> env loc proc args)
	 ;; PROC
	 ;; ARGS...
	 ;; ([tail-]call NARGS)
	 (comp-push proc)
         (let ((nargs (length args)))
           (cond ((< nargs 255)
                  (push-call! loc (if tail 'goto/args 'call) args))
                 (else
                  (push-call! loc 'mark '())
                  (for-each comp-push args)
                  (push-call! loc 'list-mark '())
                  (push-code! loc (make-glil-call (if tail 'goto/apply 'apply) 2)))))
	 (maybe-drop))

	((<ghil-mv-call> env loc producer consumer)
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
           (push-code! loc (make-glil-mv-call 0 MV))
           (push-code! loc (make-glil-call (if tail 'goto/args 'call) 1))
           (cond ((not tail)
                  (push-branch! #f 'br POST)))
           (push-label! MV)
           (push-code! loc (make-glil-call (if tail 'goto/nargs 'call/nargs) 0))
           (cond ((not tail)
                  (push-label! POST)
                  (maybe-drop)))))

        ((<ghil-reified-env> env loc)
         (return-object! loc (ghil-env-reify env)))))

    ;;
    ;; main
    (record-case ghil
      ((<ghil-lambda> env loc vars rest meta body)
       (let* ((evars (ghil-env-variables env))
	      (locs (pick (lambda (v) (eq? (ghil-var-kind v) 'local)) evars))
	      (exts (pick (lambda (v) (eq? (ghil-var-kind v) 'external)) evars))
              (nargs (allocate-indices-linearly! vars))
              (nlocs (allocate-locals! locs body))
              (nexts (allocate-indices-linearly! exts)))
	 ;; meta bindings
         (push-bindings! #f vars)
         ;; push on definition source location
         (if loc (set! stack (cons (make-glil-source loc) stack)))
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
                            (reverse! stack)))))))

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
