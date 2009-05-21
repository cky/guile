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
  #:use-module (ice-9 receive)
  #:use-module (language glil)
  #:use-module (language tree-il)
  #:use-module (language tree-il optimize)
  #:use-module (language tree-il analyze)
  #:export (compile-glil))

;;; TODO:
;;
;; call-with-values -> mv-bind
;; basic degenerate-case reduction

;; allocation:
;;  sym -> (local . index) | (heap level . index)
;;  lambda -> (nlocs . nexts)

(define *comp-module* (make-fluid))

(define (compile-glil x e opts)
  (let* ((x (make-lambda (tree-il-src x) '() '() '() x))
         (x (optimize! x e opts))
         (allocation (analyze-lexicals x)))
    (with-fluid* *comp-module* (or (and e (car e)) (current-module))
      (lambda ()
        (values (flatten-lambda x -1 allocation)
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
   ((@slot-set! . 3) . slot-set)))

(define (make-label) (gensym ":L"))

(define (vars->bind-list ids vars allocation)
  (map (lambda (id v)
         (let ((loc (hashq-ref allocation v)))
           (case (car loc)
             ((stack) (list id 'local (cdr loc)))
             ((heap)  (list id 'external (cddr loc)))
             (else (error "badness" id v loc)))))
       ids
       vars))

(define (emit-bindings src ids vars allocation emit-code)
  (if (pair? vars)
      (emit-code src (make-glil-bind
                      (vars->bind-list ids vars allocation)))))

(define (with-output-to-code proc)
  (let ((out '()))
    (define (emit-code src x)
      (set! out (cons x out))
      (if src
          (set! out (cons (make-glil-source src) out))))
    (proc emit-code)
    (reverse out)))

(define (flatten-lambda x level allocation)
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
    (let ((nlocs (car (hashq-ref allocation x)))
          (nexts (cdr (hashq-ref allocation x))))
      (make-glil-program
       nargs nrest nlocs nexts (lambda-meta x)
       (with-output-to-code
        (lambda (emit-code)
          ;; write bindings and source debugging info
          (emit-bindings #f ids vars allocation emit-code)
          (if (lambda-src x)
              (emit-code (make-glil-src (lambda-src x))))

          ;; copy args to the heap if necessary
          (let lp ((in vars) (n 0))
            (if (not (null? in))
                (let ((loc (hashq-ref allocation (car in))))
                  (case (car loc)
                    ((heap)
                     (emit-code #f (make-glil-local 'ref n))
                     (emit-code #f (make-glil-external 'set 0 (cddr loc)))))
                  (lp (cdr in) (1+ n)))))

          ;; and here, here, dear reader: we compile.
          (flatten (lambda-body x) (1+ level) allocation emit-code)))))))

(define (flatten x level allocation emit-code)
  (define (emit-label label)
    (emit-code #f (make-glil-label label)))
  (define (emit-branch src inst label)
    (emit-code src (make-glil-branch inst label)))

  (let comp ((x x) (context 'tail))
    (define (comp-tail tree) (comp tree context))
    (define (comp-push tree) (comp tree 'push))
    (define (comp-drop tree) (comp tree 'drop))

    (record-case x
      ((<void>)
       (case context
         ((push) (emit-code #f (make-glil-void)))
         ((tail)
          (emit-code #f (make-glil-void))
          (emit-code #f (make-glil-call 'return 1)))))

      ((<const> src exp)
       (case context
         ((push) (emit-code src (make-glil-const exp)))
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
              (>= (length args) 2))
         (let ((proc (car args))
               (args (cdr args)))
           (cond
            ((and (primitive-ref? proc) (eq? (primitive-ref-name proc) 'values)
                  (not (eq? context 'push)))
             ;; tail: (lambda () (apply values '(1 2)))
             ;; drop: (lambda () (apply values '(1 2)) 3)
             ;; push: (lambda () (list (apply values '(10 12)) 1))
             (case context
               ((drop) (for-each comp-drop args))
               ((tail)
                (for-each comp-push args)
                (emit-code src (make-glil-call 'return/values* (length args))))))

            (else
             (comp-push proc)
             (for-each comp-push args)
             (case context
               ((drop) (emit-code src (make-glil-call 'apply (1+ (length args))))
                       (emit-code src (make-glil-call 'drop 1)))
               ((tail) (emit-code src (make-glil-call 'goto/apply (1+ (length args)))))
               ((push) (emit-code src (make-glil-call 'apply (1+ (length args))))))))))

        ((and (primitive-ref? proc) (eq? (primitive-ref-name proc) 'values)
              (not (eq? context 'push)))
         ;; tail: (lambda () (values '(1 2)))
         ;; drop: (lambda () (values '(1 2)) 3)
         ;; push: (lambda () (list (values '(10 12)) 1))
         (case context
           ((drop) (for-each comp-drop args))
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
                         (emit-code #f (make-glil-call 'drop 1)))))))

        ((and (primitive-ref? proc)
              (eq? (primitive-ref-name proc) '@call-with-current-continuation)
              (= (length args) 1))
         (comp-push (car args))
         (case context
           ((tail) (emit-code src (make-glil-call 'goto/cc 1)))
           ((push) (emit-code src (make-glil-call 'call/cc 1)))
           ((drop) (emit-code src (make-glil-call 'call/cc 1))
                   (emit-code src (make-glil-call 'drop 1)))))

        ((and (primitive-ref? proc)
              (or (hash-ref *primcall-ops*
                            (cons (primitive-ref-name proc) (length args)))
                  (hash-ref *primcall-ops* (primitive-ref-name proc))))
         => (lambda (op)
              (for-each comp-push args)
              (emit-code src (make-glil-call op (length args)))
              (case context
                ((tail) (emit-code #f (make-glil-call 'return 1)))
                ((drop) (emit-code #f (make-glil-call 'drop 1))))))
        (else
         (comp-push proc)
         (for-each comp-push args)
         (let ((len (length args)))
           (case context
             ((tail) (emit-code src (make-glil-call 'goto/args len)))
             ((push) (emit-code src (make-glil-call 'call len)))
             ((drop)
              (let ((MV (make-label)))
                (emit-code src (make-glil-mv-call len MV))
                (emit-code #f (make-glil-const 1))
                (emit-label MV)
                (emit-code #f (make-glil-mv-bind '() #f))
                (emit-code #f (make-glil-unbind)))))))))

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
           ((push)
            (emit-code src (make-glil-toplevel 'ref name)))
           ((tail)
            (emit-code src (make-glil-toplevel 'ref name))
            (emit-code #f (make-glil-call 'return 1)))))
        (else
         (pk 'ew-the-badness x (current-module) (fluid-ref *comp-module*))
         (case context
           ((push)
            (emit-code src (make-glil-module 'ref '(guile) name #f)))
           ((tail)
            (emit-code src (make-glil-module 'ref '(guile) name #f))
            (emit-code #f (make-glil-call 'return 1)))))))

      ((<lexical-ref> src name gensym)
       (case context
         ((push tail)
          (let ((loc (hashq-ref allocation gensym)))
            (case (car loc)
              ((stack)
               (emit-code src (make-glil-local 'ref (cdr loc))))
              ((heap)
               (emit-code src (make-glil-external
                               'ref (- level (cadr loc)) (cddr loc))))
              (else (error "badness" x loc)))
            (if (eq? context 'tail)
                (emit-code #f (make-glil-call 'return 1)))))))

      ((<lexical-set> src name gensym exp)
       (comp-push exp)
       (let ((loc (hashq-ref allocation gensym)))
         (case (car loc)
           ((stack)
            (emit-code src (make-glil-local 'set (cdr loc))))
           ((heap)
            (emit-code src (make-glil-external
                            'set (- level (cadr loc)) (cddr loc))))
           (else (error "badness" x loc))))
       (case context
         ((push)
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
         ((push)
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
         ((push)
          (emit-code #f (make-glil-void)))
         ((tail) 
          (emit-code #f (make-glil-void))
          (emit-code #f (make-glil-call 'return 1)))))
      
      ((<toplevel-define> src name exp)
       (comp-push exp)
       (emit-code src (make-glil-toplevel 'define name))
       (case context
         ((push)
          (emit-code #f (make-glil-void)))
         ((tail) 
          (emit-code #f (make-glil-void))
          (emit-code #f (make-glil-call 'return 1)))))

      ((<lambda>)
       (case context
         ((push)
          (emit-code #f (flatten-lambda x level allocation)))
         ((tail)
          (emit-code #f (flatten-lambda x level allocation))
          (emit-code #f (make-glil-call 'return 1)))))

      ((<let> src names vars vals exp)
       (for-each comp-push vals)
       (emit-bindings src names vars allocation emit-code)
       (for-each (lambda (v)
                   (let ((loc (hashq-ref allocation v)))
                     (case (car loc)
                       ((stack)
                        (emit-code src (make-glil-local 'set (cdr loc))))
                       ((heap)
                        (emit-code src (make-glil-external 'set 0 (cddr loc))))
                       (else (error "badness" x loc)))))
                 (reverse vars))
       (comp-tail exp)
       (emit-code #f (make-glil-unbind)))

      ((<letrec> src names vars vals exp)
       (for-each comp-push vals)
       (emit-bindings src names vars allocation emit-code)
       (for-each (lambda (v)
                   (let ((loc (hashq-ref allocation v)))
                     (case (car loc)
                       ((stack)
                        (emit-code src (make-glil-local 'set (cdr loc))))
                       ((heap)
                        (emit-code src (make-glil-external 'set 0 (cddr loc))))
                       (else (error "badness" x loc)))))
                 (reverse vars))
       (comp-tail exp)
       (emit-code #f (make-glil-unbind))))))
