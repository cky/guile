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

(define-module (language tree-il analyze)
  #:use-module (system base syntax)
  #:use-module (language tree-il)
  #:export (analyze-lexicals))

;; allocation: the process of assigning a type and index to each var
;; a var is external if it is heaps; assigning index is easy
;; args are assigned in order
;; locals are indexed as their linear position in the binding path
;; (let (0 1)
;;   (let (2 3) ...)
;;   (let (2) ...))
;;   (let (2 3 4) ...))
;; etc.
;;
;; This algorithm has the problem that variables are only allocated
;; indices at the end of the binding path. If variables bound early in
;; the path are not used in later portions of the path, their indices
;; will not be recycled. This problem is particularly egregious in the
;; expansion of `or':
;;
;;  (or x y z)
;;    -> (let ((a x)) (if a a (let ((b y)) (if b b z))))
;;
;; As you can see, the `a' binding is only used in the ephemeral `then'
;; clause of the first `if', but its index would be reserved for the
;; whole of the `or' expansion. So we have a hack for this specific
;; case. A proper solution would be some sort of liveness analysis, and
;; not our linear allocation algorithm.
;;
;; allocation:
;;  sym -> (local . index) | (heap level . index)
;;  lambda -> (nlocs . nexts)

(define (analyze-lexicals x)
  ;; parents: lambda -> parent
  ;;  useful when we see a closed-over var, so we can calculate its
  ;;  coordinates (depth and index).
  ;; bindings: lambda -> (sym ...)
  ;;  useful for two reasons: one, so we know how much space to allocate
  ;;  when we go into a lambda; and two, so that we know when to stop,
  ;;  when looking for closed-over vars.
  ;; heaps: sym -> lambda
  ;;  allows us to heapify vars in an O(1) fashion
  ;; refcounts: sym -> count
  ;;  allows us to detect the or-expansion an O(1) time

  (define (find-heap sym parent)
    ;; fixme: check displaced lexicals here?
    (if (memq sym (hashq-ref bindings parent))
        parent
        (find-heap sym (hashq-ref parents parent))))

  (define (analyze! x parent level)
    (define (step y) (analyze! y parent level))
    (define (recur x parent) (analyze! x parent (1+ level)))
    (record-case x
      ((<application> proc args)
       (step proc) (for-each step args))

      ((<conditional> test then else)
       (step test) (step then) (step else))

      ((<lexical-ref> name gensym)
       (hashq-set! refcounts gensym (1+ (hashq-ref refcounts gensym 0)))
       (if (and (not (memq gensym (hashq-ref bindings parent)))
                (not (hashq-ref heaps gensym)))
           (hashq-set! heaps gensym (find-heap gensym parent))))
      
      ((<lexical-set> name gensym exp)
       (step exp)
       (if (not (hashq-ref heaps gensym))
           (hashq-set! heaps gensym (find-heap gensym parent))))
      
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
      (define (recur y) (allocate! y level n))
      (record-case x
        ((<application> proc args)
         (apply max (recur proc) (map recur args)))

        ((<conditional> test then else)
         (max (recur test) (recur then) (recur else)))

        ((<lexical-set> name gensym exp)
         (recur exp))
        
        ((<module-set> mod name public? exp)
         (recur exp))
        
        ((<toplevel-set> name exp)
         (recur exp))
        
        ((<toplevel-define> name exp)
         (recur exp))
        
        ((<sequence> exps)
         (apply max (map recur exps)))
        
        ((<lambda> vars meta body)
         (let lp ((vars vars) (n 0))
           (if (null? vars)
               (hashq-set! allocation x
                           (let ((nlocs (- (allocate! body (1+ level) n) n)))
                             (cons nlocs (1+ (hashq-ref heap-indexes x -1)))))
               (let ((v (if (pair? vars) (car vars) vars)))
                 (let ((binder (hashq-ref heaps v)))
                   (hashq-set!
                    allocation v
                    (if binder
                        (cons* 'heap (1+ level) (allocate-heap! binder))
                        (cons 'stack n))))
                 (lp (if (pair? vars) (cdr vars) '()) (1+ n)))))
         n)

        ((<let> vars vals exp)
         (let ((nmax (apply max (map recur vals))))
           (cond
            ;; the `or' hack
            ((and (conditional? exp)
                  (= (length vars) 1)
                  (let ((v (car vars)))
                    (and (not (hashq-ref heaps v))
                         (= (hashq-ref refcounts v 0) 2)
                         (lexical-ref? (conditional-test exp))
                         (eq? (lexical-ref-gensym (conditional-test exp)) v)
                         (lexical-ref? (conditional-then exp))
                         (eq? (lexical-ref-gensym (conditional-then exp)) v))))
             (hashq-set! allocation (car vars) (cons 'stack n))
             ;; the 1+ for this var
             (max nmax (1+ n) (allocate! (conditional-else exp) level n)))
            (else
             (let lp ((vars vars) (n n))
               (if (null? vars)
                   (max nmax (allocate! exp level n))
                   (let ((v (car vars)))
                     (let ((binder (hashq-ref heaps v)))
                       (hashq-set!
                        allocation v
                        (if binder
                            (cons* 'heap level (allocate-heap! binder))
                            (cons 'stack n)))
                       (lp (cdr vars) (if binder n (1+ n)))))))))))
        
        ((<letrec> vars vals exp)
         (let lp ((vars vars) (n n))
           (if (null? vars)
               (let ((nmax (apply max
                                  (map (lambda (x)
                                         (allocate! x level n))
                                       vals))))
                 (max nmax (allocate! exp level n)))
               (let ((v (car vars)))
                 (let ((binder (hashq-ref heaps v)))
                   (hashq-set!
                    allocation v
                    (if binder
                        (cons* 'heap level (allocate-heap! binder))
                        (cons 'stack n)))
                   (lp (cdr vars) (if binder n (1+ n))))))))

        (else n)))

  (define parents (make-hash-table))
  (define bindings (make-hash-table))
  (define heaps (make-hash-table))
  (define refcounts (make-hash-table))
  (define allocation (make-hash-table))
  (define heap-indexes (make-hash-table))

  (analyze! x #f -1)
  (allocate! x -1 0)

  allocation)
