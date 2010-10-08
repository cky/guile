;;; open-coding primitive procedures

;; Copyright (C) 2009, 2010 Free Software Foundation, Inc.

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

(define-module (language tree-il primitives)
  #:use-module (system base pmatch)
  #:use-module (rnrs bytevectors)
  #:use-module (system base syntax)
  #:use-module (language tree-il)
  #:use-module (srfi srfi-4)
  #:use-module (srfi srfi-16)
  #:export (resolve-primitives! add-interesting-primitive!
            expand-primitives!
            effect-free-primitive? effect+exception-free-primitive?))

(define *interesting-primitive-names* 
  '(apply @apply
    call-with-values @call-with-values
    call-with-current-continuation @call-with-current-continuation
    call/cc
    dynamic-wind
    @dynamic-wind
    values
    eq? eqv? equal?
    memq memv
    = < > <= >= zero?
    + * - / 1- 1+ quotient remainder modulo
    ash logand logior logxor
    not
    pair? null? list? symbol? vector? acons cons cons*

    list vector

    car cdr
    set-car! set-cdr!

    caar cadr cdar cddr

    caaar caadr cadar caddr cdaar cdadr cddar cdddr

    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr

    vector-ref vector-set!
    variable-ref variable-set!
    variable-bound?

    fluid-ref fluid-set!

    @prompt call-with-prompt @abort abort-to-prompt
    make-prompt-tag

    struct? struct-vtable make-struct struct-ref struct-set!

    bytevector-u8-ref bytevector-u8-set!
    bytevector-s8-ref bytevector-s8-set!
    u8vector-ref u8vector-set! s8vector-ref s8vector-set!
    
    bytevector-u16-ref bytevector-u16-set!
    bytevector-u16-native-ref bytevector-u16-native-set!
    bytevector-s16-ref bytevector-s16-set!
    bytevector-s16-native-ref bytevector-s16-native-set!
    u16vector-ref u16vector-set! s16vector-ref s16vector-set!
    
    bytevector-u32-ref bytevector-u32-set!
    bytevector-u32-native-ref bytevector-u32-native-set!
    bytevector-s32-ref bytevector-s32-set!
    bytevector-s32-native-ref bytevector-s32-native-set!
    u32vector-ref u32vector-set! s32vector-ref s32vector-set!
    
    bytevector-u64-ref bytevector-u64-set!
    bytevector-u64-native-ref bytevector-u64-native-set!
    bytevector-s64-ref bytevector-s64-set!
    bytevector-s64-native-ref bytevector-s64-native-set!
    u64vector-ref u64vector-set! s64vector-ref s64vector-set!
    
    bytevector-ieee-single-ref bytevector-ieee-single-set!
    bytevector-ieee-single-native-ref bytevector-ieee-single-native-set!
    bytevector-ieee-double-ref bytevector-ieee-double-set!
    bytevector-ieee-double-native-ref bytevector-ieee-double-native-set!
    f32vector-ref f32vector-set! f64vector-ref f64vector-set!))

(define (add-interesting-primitive! name)
  (hashq-set! *interesting-primitive-vars*
              (or (module-variable (current-module) name)
                  (error "unbound interesting primitive" name))
              name))

(define *interesting-primitive-vars* (make-hash-table))

(for-each add-interesting-primitive! *interesting-primitive-names*)

(define *effect-free-primitives*
  '(values
    eq? eqv? equal?
    = < > <= >= zero?
    + * - / 1- 1+ quotient remainder modulo
    not
    pair? null? list? symbol? vector? acons cons cons*
    list vector
    car cdr
    caar cadr cdar cddr
    caaar caadr cadar caddr cdaar cdadr cddar cdddr
    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
    vector-ref
    struct? struct-vtable make-struct make-struct/no-tail struct-ref
    bytevector-u8-ref bytevector-s8-ref
    bytevector-u16-ref bytevector-u16-native-ref
    bytevector-s16-ref bytevector-s16-native-ref
    bytevector-u32-ref bytevector-u32-native-ref
    bytevector-s32-ref bytevector-s32-native-ref
    bytevector-u64-ref bytevector-u64-native-ref
    bytevector-s64-ref bytevector-s64-native-ref
    bytevector-ieee-single-ref bytevector-ieee-single-native-ref
    bytevector-ieee-double-ref bytevector-ieee-double-native-ref))

;; Like *effect-free-primitives* above, but further restricted in that they
;; cannot raise exceptions.
(define *effect+exception-free-primitives*
  '(values
    eq? eqv? equal?
    not
    pair? null? list? symbol? vector? acons cons cons*
    list vector
    struct?))

(define *effect-free-primitive-table* (make-hash-table))
(define *effect+exceptions-free-primitive-table* (make-hash-table))

(for-each (lambda (x)
            (hashq-set! *effect-free-primitive-table* x #t))
          *effect-free-primitives*)
(for-each (lambda (x) 
            (hashq-set! *effect+exceptions-free-primitive-table* x #t))
          *effect+exception-free-primitives*)

(define (effect-free-primitive? prim)
  (hashq-ref *effect-free-primitive-table* prim))
(define (effect+exception-free-primitive? prim)
  (hashq-ref *effect+exceptions-free-primitive-table* prim))

(define (resolve-primitives! x mod)
  (post-order!
   (lambda (x)
     (record-case x
       ((<toplevel-ref> src name)
        (and=> (hashq-ref *interesting-primitive-vars*
                          (module-variable mod name))
               (lambda (name) (make-primitive-ref src name))))
       ((<module-ref> src mod name public?)
        ;; for the moment, we're disabling primitive resolution for
        ;; public refs because resolve-interface can raise errors.
        (let ((m (and (not public?) (resolve-module mod))))
          (and m 
               (and=> (hashq-ref *interesting-primitive-vars*
                                 (module-variable m name))
                      (lambda (name) (make-primitive-ref src name))))))
       (else #f)))
   x))



(define *primitive-expand-table* (make-hash-table))

(define (expand-primitives! x)
  (pre-order!
   (lambda (x)
     (record-case x
       ((<application> src proc args)
        (and (primitive-ref? proc)
             (let ((expand (hashq-ref *primitive-expand-table*
                                      (primitive-ref-name proc))))
               (and expand (apply expand src args)))))
       (else #f)))
   x))

;;; I actually did spend about 10 minutes trying to redo this with
;;; syntax-rules. Patches appreciated.
;;;
(define-macro (define-primitive-expander sym . clauses)
  (define (inline-args args)
    (let lp ((in args) (out '()))
      (cond ((null? in) `(list ,@(reverse out)))
            ((symbol? in) `(cons* ,@(reverse out) ,in))
            ((pair? (car in))
             (lp (cdr in)
                 (cons (if (eq? (caar in) 'quote)
                           `(make-const src ,@(cdar in))
                           `(make-application src (make-primitive-ref src ',(caar in))
                                              ,(inline-args (cdar in))))
                       out)))
            ((symbol? (car in))
             ;; assume it's locally bound
             (lp (cdr in) (cons (car in) out)))
            ((self-evaluating? (car in))
             (lp (cdr in) (cons `(make-const src ,(car in)) out)))
            (else
             (error "what what" (car in))))))
  (define (consequent exp)
    (cond
     ((pair? exp)
      (pmatch exp
        ((if ,test ,then ,else)
         `(if ,test
              ,(consequent then)
              ,(consequent else)))
        (else
         `(make-application src (make-primitive-ref src ',(car exp))
                            ,(inline-args (cdr exp))))))
     ((symbol? exp)
      ;; assume locally bound
      exp)
     ((number? exp)
      `(make-const src ,exp))
     ((not exp)
      ;; failed match
      #f)
     (else (error "bad consequent yall" exp))))
  `(hashq-set! *primitive-expand-table*
               ',sym
               (case-lambda
                ,@(let lp ((in clauses) (out '()))
                    (if (null? in)
                        (reverse (cons '(else #f) out))
                        (lp (cddr in)
                            (cons `((src . ,(car in))
                                    ,(consequent (cadr in))) out)))))))

(define-primitive-expander zero? (x)
  (= x 0))

(define-primitive-expander +
  () 0
  (x) x
  (x y) (if (and (const? y)
                 (let ((y (const-exp y)))
                   (and (number? y) (exact? y) (= y 1))))
            (1+ x)
            (if (and (const? y)
                     (let ((y (const-exp y)))
                       (and (number? y) (exact? y) (= y -1))))
                (1- x)
                (if (and (const? x)
                         (let ((x (const-exp x)))
                           (and (number? x) (exact? x) (= x 1))))
                    (1+ y)
                    (+ x y))))
  (x y z . rest) (+ x (+ y z . rest)))
  
(define-primitive-expander *
  () 1
  (x) x
  (x y z . rest) (* x (* y z . rest)))
  
(define-primitive-expander -
  (x) (- 0 x)
  (x y) (if (and (const? y)
                 (let ((y (const-exp y)))
                   (and (number? y) (exact? y) (= y 1))))
            (1- x)
            (- x y))
  (x y z . rest) (- x (+ y z . rest)))
  
(define-primitive-expander /
  (x) (/ 1 x)
  (x y z . rest) (/ x (* y z . rest)))
  
(define-primitive-expander caar (x) (car (car x)))
(define-primitive-expander cadr (x) (car (cdr x)))
(define-primitive-expander cdar (x) (cdr (car x)))
(define-primitive-expander cddr (x) (cdr (cdr x)))
(define-primitive-expander caaar (x) (car (car (car x))))
(define-primitive-expander caadr (x) (car (car (cdr x))))
(define-primitive-expander cadar (x) (car (cdr (car x))))
(define-primitive-expander caddr (x) (car (cdr (cdr x))))
(define-primitive-expander cdaar (x) (cdr (car (car x))))
(define-primitive-expander cdadr (x) (cdr (car (cdr x))))
(define-primitive-expander cddar (x) (cdr (cdr (car x))))
(define-primitive-expander cdddr (x) (cdr (cdr (cdr x))))
(define-primitive-expander caaaar (x) (car (car (car (car x)))))
(define-primitive-expander caaadr (x) (car (car (car (cdr x)))))
(define-primitive-expander caadar (x) (car (car (cdr (car x)))))
(define-primitive-expander caaddr (x) (car (car (cdr (cdr x)))))
(define-primitive-expander cadaar (x) (car (cdr (car (car x)))))
(define-primitive-expander cadadr (x) (car (cdr (car (cdr x)))))
(define-primitive-expander caddar (x) (car (cdr (cdr (car x)))))
(define-primitive-expander cadddr (x) (car (cdr (cdr (cdr x)))))
(define-primitive-expander cdaaar (x) (cdr (car (car (car x)))))
(define-primitive-expander cdaadr (x) (cdr (car (car (cdr x)))))
(define-primitive-expander cdadar (x) (cdr (car (cdr (car x)))))
(define-primitive-expander cdaddr (x) (cdr (car (cdr (cdr x)))))
(define-primitive-expander cddaar (x) (cdr (cdr (car (car x)))))
(define-primitive-expander cddadr (x) (cdr (cdr (car (cdr x)))))
(define-primitive-expander cdddar (x) (cdr (cdr (cdr (car x)))))
(define-primitive-expander cddddr (x) (cdr (cdr (cdr (cdr x)))))

(define-primitive-expander cons*
  (x) x
  (x y) (cons x y)
  (x y . rest) (cons x (cons* y . rest)))

(define-primitive-expander acons (x y z)
  (cons (cons x y) z))

(define-primitive-expander apply (f a0 . args)
  (@apply f a0 . args))

(define-primitive-expander call-with-values (producer consumer)
  (@call-with-values producer consumer))

(define-primitive-expander call-with-current-continuation (proc)
  (@call-with-current-continuation proc))

(define-primitive-expander call/cc (proc)
  (@call-with-current-continuation proc))

(define-primitive-expander values (x) x)

(define-primitive-expander make-struct (vtable tail-size . args)
  (if (and (const? tail-size)
           (let ((n (const-exp tail-size)))
             (and (number? n) (exact? n) (zero? n))))
      (make-struct/no-tail vtable . args)
      #f))

(define-primitive-expander u8vector-ref (vec i)
  (bytevector-u8-ref vec i))
(define-primitive-expander u8vector-set! (vec i x)
  (bytevector-u8-set! vec i x))
(define-primitive-expander s8vector-ref (vec i)
  (bytevector-s8-ref vec i))
(define-primitive-expander s8vector-set! (vec i x)
  (bytevector-s8-set! vec i x))

(define-primitive-expander u16vector-ref (vec i)
  (bytevector-u16-native-ref vec (* i 2)))
(define-primitive-expander u16vector-set! (vec i x)
  (bytevector-u16-native-set! vec (* i 2) x))
(define-primitive-expander s16vector-ref (vec i)
  (bytevector-s16-native-ref vec (* i 2)))
(define-primitive-expander s16vector-set! (vec i x)
  (bytevector-s16-native-set! vec (* i 2) x))

(define-primitive-expander u32vector-ref (vec i)
  (bytevector-u32-native-ref vec (* i 4)))
(define-primitive-expander u32vector-set! (vec i x)
  (bytevector-u32-native-set! vec (* i 4) x))
(define-primitive-expander s32vector-ref (vec i)
  (bytevector-s32-native-ref vec (* i 4)))
(define-primitive-expander s32vector-set! (vec i x)
  (bytevector-s32-native-set! vec (* i 4) x))

(define-primitive-expander u64vector-ref (vec i)
  (bytevector-u64-native-ref vec (* i 8)))
(define-primitive-expander u64vector-set! (vec i x)
  (bytevector-u64-native-set! vec (* i 8) x))
(define-primitive-expander s64vector-ref (vec i)
  (bytevector-s64-native-ref vec (* i 8)))
(define-primitive-expander s64vector-set! (vec i x)
  (bytevector-s64-native-set! vec (* i 8) x))

(define-primitive-expander f32vector-ref (vec i)
  (bytevector-ieee-single-native-ref vec (* i 4)))
(define-primitive-expander f32vector-set! (vec i x)
  (bytevector-ieee-single-native-set! vec (* i 4) x))
(define-primitive-expander f32vector-ref (vec i)
  (bytevector-ieee-single-native-ref vec (* i 4)))
(define-primitive-expander f32vector-set! (vec i x)
  (bytevector-ieee-single-native-set! vec (* i 4) x))

(define-primitive-expander f64vector-ref (vec i)
  (bytevector-ieee-double-native-ref vec (* i 8)))
(define-primitive-expander f64vector-set! (vec i x)
  (bytevector-ieee-double-native-set! vec (* i 8) x))
(define-primitive-expander f64vector-ref (vec i)
  (bytevector-ieee-double-native-ref vec (* i 8)))
(define-primitive-expander f64vector-set! (vec i x)
  (bytevector-ieee-double-native-set! vec (* i 8) x))

(hashq-set! *primitive-expand-table*
            'dynamic-wind
            (case-lambda
              ((src pre thunk post)
               ;; Here we will make concessions to the fact that our inliner is
               ;; lame, and add a hack.
               (cond
                ((lambda? thunk)
                 (let ((PRE (gensym " pre"))
                       (POST (gensym " post")))
                   (make-let
                    src
                    '(pre post)
                    (list PRE POST)
                    (list pre post)
                    (make-dynwind
                     src
                     (make-lexical-ref #f 'pre PRE)
                     (make-application #f thunk '())
                     (make-lexical-ref #f 'post POST)))))
                (else
                 (let ((PRE (gensym " pre"))
                       (THUNK (gensym " thunk"))
                       (POST (gensym " post")))
                   (make-let
                    src
                    '(pre thunk post)
                    (list PRE THUNK POST)
                    (list pre thunk post)
                    (make-dynwind
                     src
                     (make-lexical-ref #f 'pre PRE)
                     (make-application #f (make-lexical-ref #f 'thunk THUNK) '())
                     (make-lexical-ref #f 'post POST)))))))
              (else #f)))

(hashq-set! *primitive-expand-table*
            '@dynamic-wind
            (case-lambda
              ((src pre expr post)
               (let ((PRE (gensym " pre"))
                     (POST (gensym " post")))
                 (make-let
                  src
                  '(pre post)
                  (list PRE POST)
                  (list pre post)
                  (make-dynwind
                   src
                   (make-lexical-ref #f 'pre PRE)
                   expr
                   (make-lexical-ref #f 'post POST)))))))

(hashq-set! *primitive-expand-table*
            'fluid-ref
            (case-lambda
              ((src fluid) (make-dynref src fluid))
              (else #f)))

(hashq-set! *primitive-expand-table*
            'fluid-set!
            (case-lambda
              ((src fluid exp) (make-dynset src fluid exp))
              (else #f)))

(hashq-set! *primitive-expand-table*
            '@prompt
            (case-lambda
              ((src tag exp handler)
               (let ((args-sym (gensym)))
                 (make-prompt
                  src tag exp
                  ;; If handler itself is a lambda, the inliner can do some
                  ;; trickery here.
                  (make-lambda-case
                   (tree-il-src handler) '() #f 'args #f '() (list args-sym)
                   (make-application #f (make-primitive-ref #f 'apply)
                                     (list handler
                                           (make-lexical-ref #f 'args args-sym)))
                   #f))))
              (else #f)))

(hashq-set! *primitive-expand-table*
            'call-with-prompt
            (case-lambda
              ((src tag thunk handler)
               ;; Sigh. Until the inliner does its job, manually inline
               ;; (let ((h (lambda ...))) (prompt k x h))
               (cond
                ((lambda? handler)
                 (let ((args-sym (gensym)))
                   (make-prompt
                    src tag (make-application #f thunk '())
                    ;; If handler itself is a lambda, the inliner can do some
                    ;; trickery here.
                    (make-lambda-case
                     (tree-il-src handler) '() #f 'args #f '() (list args-sym)
                     (make-application #f (make-primitive-ref #f 'apply)
                                       (list handler
                                             (make-lexical-ref #f 'args args-sym)))
                     #f))))
                (else #f)))
              (else #f)))

(hashq-set! *primitive-expand-table*
            '@abort
            (case-lambda
              ((src tag tail-args)
               (make-abort src tag '() tail-args))
              (else #f)))
(hashq-set! *primitive-expand-table*
            'abort-to-prompt
            (case-lambda
              ((src tag . args)
               (make-abort src tag args (make-const #f '())))
              (else #f)))
