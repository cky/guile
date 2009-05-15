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

;; allocation:
;;  sym -> (local . index) | (heap level . index)
;;  lambda -> (nlocs . nexts)

(define (compile-glil x e opts)
  (let* ((x (make-lambda (tree-il-src x) '() '() x))
         (x (optimize! x e opts))
         (allocation (analyze-lexicals x)))
    (values (flatten-lambda x -1 allocation)
            (and e (cons (car e) (cddr e)))
            e)))



(define (make-label) (gensym ":L"))

(define (vars->bind-list vars allocation)
  (map (lambda (v)
         (let ((loc (hashq-ref allocation v)))
           (case (car loc)
             ((stack) (list v 'local (cdr loc)))
             ((heap)  (list v 'external (cddr loc)))
             (else (error "badness" v loc)))))
       vars))

(define (emit-bindings src vars allocation emit-code)
  (if (pair? vars)
      (emit-code src (make-glil-bind (vars->bind-list vars allocation)))))

(define (with-output-to-code proc)
  (let ((out '()))
    (define (emit-code src x)
      (set! out (cons x out))
      (if src
          (set! out (cons (make-glil-source src) out))))
    (proc emit-code)
    (reverse out)))

(define (flatten-lambda x level allocation)
  (receive (vars nargs nrest)
      (let lp ((vars (lambda-vars x)) (out '()) (n 0))
          (cond ((null? vars) (values (reverse out) n 0))
                ((pair? vars) (lp (cdr vars) (cons (car vars) out) (1+ n)))
                (else (values (reverse (cons vars out)) (1+ n) 1))))
    (let ((nlocs (car (hashq-ref allocation x)))
          (nexts (cdr (hashq-ref allocation x))))
      (make-glil-program
       nargs nrest nlocs nexts (lambda-meta x)
       (with-output-to-code
        (lambda (emit-code)
          ;; write bindings and source debugging info
          (emit-bindings #f vars allocation emit-code)
          (if (lambda-src x)
              (emit-code (make-glil-src (lambda-src x))))

          ;; copy args to the heap if necessary
          (let lp ((in vars) (n 0))
            (if (not (null? in))
                (let ((loc (hashq-ref allocation (car vars))))
                  (case (car loc)
                    ((heap)
                     (emit-code (make-glil-argument 'ref n))
                     (emit-code (make-glil-external 'set 0 (cddr loc)))))
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
       (comp-push proc)
       (for-each comp-push args)
       (emit-code src (make-glil-call (case context
                                        ((tail) 'goto/args)
                                        (else 'call))
                                      (length args))))

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
       (case context
         ((push)
          (emit-code src (make-glil-module 'ref '(guile) name #f)))
         ((tail)
          (emit-code src (make-glil-module 'ref '(guile) name #f))
          (emit-code #f (make-glil-call 'return 1)))))

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

      ((<let> src vars vals exp)
       (for-each comp-push vals)
       (emit-bindings src vars allocation emit-code)
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

      ((<letrec> src vars vals exp)
       (for-each comp-push vals)
       (emit-bindings src vars allocation emit-code)
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
