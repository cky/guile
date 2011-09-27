;;; Tree-il optimizer

;; Copyright (C) 2009, 2011 Free Software Foundation, Inc.

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

(define-module (language tree-il optimize)
  #:use-module (language tree-il)
  #:use-module (language tree-il primitives)
  #:use-module (language tree-il inline)
  #:use-module (language tree-il fix-letrec)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:export (optimize!))

(define (optimize! x env opts)
  (let ((peval (match (memq #:partial-eval? opts)
                 ((#:partial-eval? #f _ ...)
                  ;; Disable partial evaluation.
                  (lambda (x e) x))
                 (_ peval))))
   (inline!
    (fix-letrec!
     (peval (expand-primitives! (resolve-primitives! x env))
            env)))))


;;;
;;; Partial evaluation.
;;;

(define (fresh-gensyms syms)
  (map (lambda (x) (gensym (string-append (symbol->string x) " ")))
       syms))

(define (alpha-rename exp)
  "Alpha-rename EXP.  For any lambda in EXP, generate new symbols and
replace all lexical references to the former symbols with lexical
references to the new symbols."
  ;; XXX: This should be factorized somehow.
  (let loop ((exp     exp)
             (mapping vlist-null))             ; maps old to new gensyms
    (match exp
      (($ <lambda-case> src req opt rest kw inits gensyms body alt)
       ;; Create new symbols to replace GENSYMS and propagate them down
       ;; in BODY and ALT.
       (let* ((new     (fresh-gensyms
                        (append req
                                (or opt '())
                                (if rest (list rest) '())
                                (match kw
                                  ((aok? (_ name _) ...) name)
                                  (_ '())))))
              (mapping (fold vhash-consq mapping gensyms new)))
         (make-lambda-case src req opt rest
                           (match kw
                             ((aok? (kw name old) ...)
                              (cons aok? (map list
                                              kw
                                              name
                                              (take-right new (length old)))))
                             (_ #f))
                           (map (cut loop <> mapping) inits)
                           new
                           (loop body mapping)
                           (and alt (loop alt mapping)))))
      (($ <lexical-ref> src name gensym)
       ;; Possibly replace GENSYM by the new gensym defined in MAPPING.
       (let ((val (vhash-assq gensym mapping)))
         (if val
             (make-lexical-ref src name (cdr val))
             exp)))
      (($ <lexical-set> src name gensym exp)
       (let ((val (vhash-assq gensym mapping)))
         (make-lexical-set src name (if val (cdr val) gensym)
                           (loop exp mapping))))
      (($ <lambda> src meta body)
       (make-lambda src meta (loop body mapping)))
      (($ <let> src names gensyms vals body)
       ;; As for `lambda-case' rename GENSYMS to avoid any collision.
       (let* ((new     (fresh-gensyms names))
              (mapping (fold vhash-consq mapping gensyms new))
              (vals    (map (cut loop <> mapping) vals))
              (body    (loop body mapping)))
         (make-let src names new vals body)))
      (($ <letrec> src in-order? names gensyms vals body)
       ;; Likewise.
       (let* ((new     (fresh-gensyms names))
              (mapping (fold vhash-consq mapping gensyms new))
              (vals    (map (cut loop <> mapping) vals))
              (body    (loop body mapping)))
         (make-letrec src in-order? names new vals body)))
      (($ <fix> src names gensyms vals body)
       ;; Likewise.
       (let* ((new     (fresh-gensyms names))
              (mapping (fold vhash-consq mapping gensyms new))
              (vals    (map (cut loop <> mapping) vals))
              (body    (loop body mapping)))
         (make-fix src names new vals body)))
      (($ <let-values> src exp body)
       (make-let-values src (loop exp mapping) (loop body mapping)))
      (($ <const>)
       exp)
      (($ <void>)
       exp)
      (($ <toplevel-ref>)
       exp)
      (($ <module-ref>)
       exp)
      (($ <primitive-ref>)
       exp)
      (($ <toplevel-set> src name exp)
       (make-toplevel-set src name (loop exp mapping)))
      (($ <toplevel-define> src name exp)
       (make-toplevel-define src name (loop exp mapping)))
      (($ <module-set> src mod name public? exp)
       (make-module-set src mod name public? (loop exp mapping)))
      (($ <dynlet> src fluids vals body)
       (make-dynlet src
                    (map (cut loop <> mapping) fluids)
                    (map (cut loop <> mapping) vals)
                    (loop body mapping)))
      (($ <dynwind> src winder body unwinder)
       (make-dynwind src
                     (loop winder mapping)
                     (loop body mapping)
                     (loop unwinder mapping)))
      (($ <dynref> src fluid)
       (make-dynref src (loop fluid mapping)))
      (($ <dynset> src fluid exp)
       (make-dynset src (loop fluid mapping) (loop exp mapping)))
      (($ <conditional> src condition subsequent alternate)
       (make-conditional src
                         (loop condition mapping)
                         (loop subsequent mapping)
                         (loop alternate mapping)))
      (($ <application> src proc args)
       (make-application src (loop proc mapping)
                         (map (cut loop <> mapping) args)))
      (($ <sequence> src exps)
       (make-sequence src (map (cut loop <> mapping) exps)))
      (($ <prompt> src tag body handler)
       (make-prompt src (loop tag mapping) (loop body mapping)
                    (loop handler mapping)))
      (($ <abort> src tag args tail)
       (make-abort src (loop tag mapping) (map (cut loop <> mapping) args)
                   (loop tail mapping))))))

(define-syntax-rule (let/ec k e e* ...)
  (let ((tag (make-prompt-tag)))
    (call-with-prompt
     tag
     (lambda ()
       (let ((k (lambda args (apply abort-to-prompt tag args))))
         e e* ...))
     (lambda (_ res) res))))

(define (tree-il-any proc exp)
  (let/ec k
    (tree-il-fold (lambda (exp res)
                    (let ((res (proc exp)))
                      (if res (k res) #f)))
                  (lambda (exp res)
                    (let ((res (proc exp)))
                      (if res (k res) #f)))
                  (lambda (exp res) #f)
                  #f exp)))

(define (vlist-any proc vlist)
  (let ((len (vlist-length vlist)))
    (let lp ((i 0))
      (and (< i len)
           (or (proc (vlist-ref vlist i))
               (lp (1+ i)))))))

(define-record-type <var>
  (make-var name gensym refcount set?)
  var?
  (name var-name)
  (gensym var-gensym)
  (refcount var-refcount set-var-refcount!)
  (set? var-set? set-var-set?!))

(define* (build-var-table exp #:optional (table vlist-null))
  (tree-il-fold
   (lambda (exp res)
     (match exp
       (($ <lexical-ref> src name gensym)
        (let ((var (vhash-assq gensym res)))
          (if var
              (begin
                (set-var-refcount! (cdr var) (1+ (var-refcount (cdr var))))
                res)
              (vhash-consq gensym (make-var name gensym 1 #f) res))))
       (_ res)))
   (lambda (exp res)
     (match exp
       (($ <lexical-set> src name gensym exp)
        (let ((var (vhash-assq gensym res)))
          (if var
              (begin
                (set-var-set?! (cdr var) #t)
                res)
              (vhash-consq gensym (make-var name gensym 0 #t) res))))
       (_ res)))
   (lambda (exp res) res)
   table exp))

(define-record-type <counter>
  (%make-counter effort size continuation recursive? data prev)
  counter?
  (effort effort-counter)
  (size size-counter)
  (continuation counter-continuation)
  (recursive? counter-recursive?)
  (data counter-data)
  (prev counter-prev))

(define (abort-counter c)
  ((counter-continuation c)))

(define (record-effort! c)
  (let ((e (effort-counter c)))
    (if (zero? (variable-ref e))
        (abort-counter c)
        (variable-set! e (1- (variable-ref e))))))

(define (record-size! c)
  (let ((s (size-counter c)))
    (if (zero? (variable-ref s))
        (abort-counter c)
        (variable-set! s (1- (variable-ref s))))))

(define (find-counter data counter)
  (and counter
       (if (eq? data (counter-data counter))
           counter
           (find-counter data (counter-prev counter)))))

(define* (transfer! from to #:optional
                    (effort (variable-ref (effort-counter from)))
                    (size (variable-ref (size-counter from))))
  (define (transfer-counter! from-v to-v amount)
    (let* ((from-balance (variable-ref from-v))
           (to-balance (variable-ref to-v))
           (amount (min amount from-balance)))
      (variable-set! from-v (- from-balance amount))
      (variable-set! to-v (+ to-balance amount))))

  (transfer-counter! (effort-counter from) (effort-counter to) effort)
  (transfer-counter! (size-counter from) (size-counter to) size))

(define (make-top-counter effort-limit size-limit continuation data)
  (%make-counter (make-variable effort-limit)
                 (make-variable size-limit)
                 continuation
                 #t
                 data
                 #f))

(define (make-nested-counter continuation data current)
  (let ((c (%make-counter (make-variable 0)
                          (make-variable 0)
                          continuation
                          #f
                          data
                          current)))
    (transfer! current c)
    c))

(define (make-recursive-counter effort-limit size-limit orig current)
  (let ((c (%make-counter (make-variable 0)
                          (make-variable 0)
                          (counter-continuation orig)
                          #t
                          (counter-data orig)
                          current)))
    (transfer! current c effort-limit size-limit)
    c))

(define (types-check? primitive-name args)
  (case primitive-name
    ((values) #t)
    ((not pair? null? list? symbol? vector? struct?)
     (= (length args) 1))
    ((eq? eqv? equal?)
     (= (length args) 2))
    ;; FIXME: add more cases?
    (else #f)))

(define* (peval exp #:optional (cenv (current-module)) (env vlist-null)
                #:key
                (operator-size-limit 40)
                (operand-size-limit 20)
                (value-size-limit 10)
                (effort-limit 500)
                (recursive-effort-limit 100))
  "Partially evaluate EXP in compilation environment CENV, with
top-level bindings from ENV and return the resulting expression.  Since
it does not handle <fix> and <let-values>, it should be called before
`fix-letrec'."

  ;; This is a simple partial evaluator.  It effectively performs
  ;; constant folding, copy propagation, dead code elimination, and
  ;; inlining, but not across top-level bindings---there should be a way
  ;; to allow this (TODO).
  ;;
  ;; Unlike a full-blown partial evaluator, it does not emit definitions
  ;; of specialized versions of lambdas encountered on its way.  Also,
  ;; it's not yet complete: it bails out for `prompt', etc.

  (define local-toplevel-env
    ;; The top-level environment of the module being compiled.
    (match exp
      (($ <toplevel-define> _ name)
       (vhash-consq name #t env))
      (($ <sequence> _ exps)
       (fold (lambda (x r)
               (match x
                 (($ <toplevel-define> _ name)
                  (vhash-consq name #t r))
                 (_ r)))
             env
             exps))
      (_ env)))

  (define (local-toplevel? name)
    (vhash-assq name local-toplevel-env))

  (define store (build-var-table exp))

  (define (assigned-lexical? sym)
    (let ((v (vhash-assq sym store)))
      (and v (var-set? (cdr v)))))

  (define (lexical-refcount sym)
    (let ((v (vhash-assq sym store)))
      (if v (var-refcount (cdr v)) 0)))

  (define (record-source-expression! orig new)
    (set! store (vhash-consq new
                             (source-expression orig)
                             (build-var-table new store)))
    new)

  (define (source-expression new)
    (let ((x (vhash-assq new store)))
      (if x (cdr x) new)))

  (define residual-lexical-references (make-hash-table))

  (define (record-residual-lexical-reference! sym)
    (hashq-set! residual-lexical-references sym #t))

  (define (apply-primitive name args)
    ;; todo: further optimize commutative primitives
    (catch #t
      (lambda ()
        (call-with-values
            (lambda ()
              (apply (module-ref the-scm-module name) args))
          (lambda results
            (values #t results))))
      (lambda _
        (values #f '()))))

  (define (inline-values exp src names gensyms body)
    (let loop ((exp exp))
      (match exp
        ;; Some expression types are always singly-valued.
        ((or ($ <const>)
             ($ <void>)
             ($ <lambda>)
             ($ <lexical-ref>)
             ($ <toplevel-ref>)
             ($ <module-ref>)
             ($ <primitive-ref>)
             ($ <dynref>)
             ($ <lexical-set>)          ; FIXME: these set! expressions
             ($ <toplevel-set>)         ; could return zero values in
             ($ <toplevel-define>)      ; the future
             ($ <module-set>)           ;
             ($ <dynset>))              ; 
         (and (= (length names) 1)
              (make-let src names gensyms (list exp) body)))
        (($ <application> src
            ($ <primitive-ref> _ (? singly-valued-primitive? name)))
         (and (= (length names) 1)
              (make-let src names gensyms (list exp) body)))

        ;; Statically-known number of values.
        (($ <application> src ($ <primitive-ref> _ 'values) vals)
         (and (= (length names) (length vals))
              (make-let src names gensyms vals body)))

        ;; Not going to copy code into both branches.
        (($ <conditional>) #f)

        ;; Bail on other applications.
        (($ <application>) #f)

        ;; Bail on prompt and abort.
        (($ <prompt>) #f)
        (($ <abort>) #f)
        
        ;; Propagate to tail positions.
        (($ <let> src names gensyms vals body)
         (let ((body (loop body)))
           (and body
                (make-let src names gensyms vals body))))
        (($ <letrec> src in-order? names gensyms vals body)
         (let ((body (loop body)))
           (and body
                (make-letrec src in-order? names gensyms vals body))))
        (($ <fix> src names gensyms vals body)
         (let ((body (loop body)))
           (and body
                (make-fix src names gensyms vals body))))
        (($ <let-values> src exp
            ($ <lambda-case> src2 req opt rest kw inits gensyms body #f))
         (let ((body (loop body)))
           (and body
                (make-let-values src exp
                                 (make-lambda-case src2 req opt rest kw
                                                   inits gensyms body #f)))))
        (($ <dynwind> src winder body unwinder)
         (let ((body (loop body)))
           (and body
                (make-dynwind src winder body unwinder))))
        (($ <dynlet> src fluids vals body)
         (let ((body (loop body)))
           (and body
                (make-dynlet src fluids vals body))))
        (($ <sequence> src exps)
         (match exps
           ((head ... tail)
            (let ((tail (loop tail)))
              (and tail
                   (make-sequence src (append head (list tail)))))))))))

  (define (make-values src values)
    (match values
      ((single) single)                 ; 1 value
      ((_ ...)                          ; 0, or 2 or more values
       (make-application src (make-primitive-ref src 'values)
                         values))))

  (define (constant-expression? x)
    ;; Return true if X is constant---i.e., if it is known to have no
    ;; effects, does not allocate storage for a mutable object, and does
    ;; not access mutable data (like `car' or toplevel references).
    (let loop ((x x))
      (match x
        (($ <void>) #t)
        (($ <const>) #t)
        (($ <lambda>) #t)
        (($ <lambda-case> _ req opt rest kw inits _ body alternate)
         (and (every loop inits) (loop body) (loop alternate)))
        (($ <lexical-ref> _ _ gensym)
         (not (assigned-lexical? gensym)))
        (($ <primitive-ref>) #t)
        (($ <conditional> _ condition subsequent alternate)
         (and (loop condition) (loop subsequent) (loop alternate)))
        (($ <application> _ ($ <primitive-ref> _ name) args)
         (and (effect-free-primitive? name)
              (not (constructor-primitive? name))
              (types-check? name args)
              (every loop args)))
        (($ <application> _ ($ <lambda> _ _ body) args)
         (and (loop body) (every loop args)))
        (($ <sequence> _ exps)
         (every loop exps))
        (($ <let> _ _ _ vals body)
         (and (every loop vals) (loop body)))
        (($ <letrec> _ _ _ _ vals body)
         (and (every loop vals) (loop body)))
        (($ <fix> _ _ _ vals body)
         (and (every loop vals) (loop body)))
        (($ <let-values> _ exp body)
         (and (loop exp) (loop body)))
        (($ <prompt> _ tag body handler)
         (and (loop tag) (loop body) (loop handler)))
        (_ #f))))

  (define (small-expression? x limit)
    (let/ec k
      (tree-il-fold
       (lambda (x res)                  ; leaf
         (1+ res))
       (lambda (x res)                  ; down
         (1+ res))
       (lambda (x res)                  ; up
         (if (< res limit)
             res
             (k #f)))
       0 x)
      #t))
  
  (let loop ((exp   exp)
             (env   vlist-null)         ; static environment
             (counter #f)               ; inlined call stack
             (ctx 'value))   ; effect, value, test, operator, or operand
    (define (lookup var)
      (and=> (vhash-assq var env) cdr))

    (define (for-value exp)
      (loop exp env counter 'value))
    (define (for-operand exp)
      (loop exp env counter 'operand))
    (define (for-test exp)
      (loop exp env counter 'test))
    (define (for-effect exp)
      (loop exp env counter 'effect))
    (define (for-tail exp)
      (loop exp env counter ctx))

    (if counter
        (record-effort! counter))

    (match exp
      (($ <const>)
       (case ctx
         ((effect) (make-void #f))
         (else exp)))
      (($ <void>)
       (case ctx
         ((test) (make-const #f #t))
         (else exp)))
      (($ <lexical-ref> _ _ gensym)
       (case ctx
         ((effect) (make-void #f))
         (else
          (let ((val (lookup gensym)))
            (cond
             ((or (not val)
                  (assigned-lexical? gensym)
                  (not (constant-expression? val)))
              ;; Don't copy-propagate through assigned variables,
              ;; and don't reorder effects.
              (record-residual-lexical-reference! gensym)
              exp)
             ((lexical-ref? val)
              (for-tail val))
             ((or (const? val)
                  (void? val)
                  (primitive-ref? val))
              ;; Always propagate simple values that cannot lead to
              ;; code bloat.
              (for-tail val))
             ((= 1 (lexical-refcount gensym))
              ;; Always propagate values referenced only once.
              ;; There is no need to rename the bindings, as they
              ;; are only being moved, not copied.  However in
              ;; operator context we do rename it, as that
              ;; effectively clears out the residualized-lexical
              ;; flags that may have been set when this value was
              ;; visited previously as an operand.
              (case ctx
                ((test) (for-test val))
                ((operator) (record-source-expression! val (alpha-rename val)))
                (else val)))
             ;; FIXME: do demand-driven size accounting rather than
             ;; these heuristics.
             ((eq? ctx 'operator)
              ;; A pure expression in the operator position.  Inline
              ;; if it's a lambda that's small enough.
              (if (and (lambda? val)
                       (small-expression? val operator-size-limit))
                  (record-source-expression! val (alpha-rename val))
                  (begin
                    (record-residual-lexical-reference! gensym)
                    exp)))
             ((eq? ctx 'operand)
              ;; A pure expression in the operand position.  Inline
              ;; if it's small enough.
              (if (small-expression? val operand-size-limit)
                  (record-source-expression! val (alpha-rename val))
                  (begin
                    (record-residual-lexical-reference! gensym)
                    exp)))
             (else
              ;; A pure expression, processed for value.  Don't
              ;; inline lambdas, because they will probably won't
              ;; fold because we don't know the operator.
              (if (and (small-expression? val value-size-limit)
                       (not (tree-il-any lambda? val)))
                  (record-source-expression! val (alpha-rename val))
                  (begin
                    (record-residual-lexical-reference! gensym)
                    exp))))))))
      (($ <lexical-set> src name gensym exp)
       (if (zero? (lexical-refcount gensym))
           (let ((exp (for-effect exp)))
             (if (void? exp)
                 exp
                 (make-sequence src (list exp (make-void #f)))))
           (begin
             (record-residual-lexical-reference! gensym)
             (make-lexical-set src name gensym (for-value exp)))))
      (($ <let> src names gensyms vals body)
       (let* ((vals (map for-operand vals))
              (body (loop body
                      (fold vhash-consq env gensyms vals)
                      counter
                      ctx)))
         (cond
          ((const? body)
           (for-tail (make-sequence src (append vals (list body)))))
          ((and (lexical-ref? body)
                (memq (lexical-ref-gensym body) gensyms))
           (let ((sym (lexical-ref-gensym body))
                 (pairs (map cons gensyms vals)))
             ;; (let ((x foo) (y bar) ...) x) => (begin bar ... foo)
             (for-tail
              (make-sequence
               src
               (append (map cdr (alist-delete sym pairs eq?))
                       (list (assq-ref pairs sym)))))))
          (else
           ;; Only include bindings for which lexical references
           ;; have been residualized.
           (let*-values
               (((stripped) (remove
                             (lambda (x)
                               (and (not (hashq-ref
                                          residual-lexical-references
                                          (cadr x)))
                                    ;; FIXME: Here we can probably
                                    ;; strip pure expressions in
                                    ;; addition to constant
                                    ;; expressions.
                                    (constant-expression? (car x))))
                             (zip vals gensyms names)))
                ((vals gensyms names) (unzip3 stripped)))
             (if (null? stripped)
                 body
                 (make-let src names gensyms vals body)))))))
      (($ <letrec> src in-order? names gensyms vals body)
       ;; Things could be done more precisely when IN-ORDER? but
       ;; it's OK not to do it---at worst we lost an optimization
       ;; opportunity.
       (let* ((vals (map for-operand vals))
              (body (loop body
                      (fold vhash-consq env gensyms vals)
                      counter
                      ctx)))
         (if (and (const? body)
                  (every constant-expression? vals))
             body
             (let*-values
                 (((stripped) (remove
                               (lambda (x)
                                 (and (constant-expression? (car x))
                                      (not (hashq-ref
                                            residual-lexical-references
                                            (cadr x)))))
                               (zip vals gensyms names)))
                  ((vals gensyms names) (unzip3 stripped)))
               (if (null? stripped)
                   body
                   (make-letrec src in-order? names gensyms vals body))))))
      (($ <fix> src names gensyms vals body)
       (let* ((vals (map for-operand vals))
              (body (loop body
                      (fold vhash-consq env gensyms vals)
                      counter
                      ctx)))
         (if (const? body)
             body
             (make-fix src names gensyms vals body))))
      (($ <let-values> lv-src producer consumer)
       ;; Peval the producer, then try to inline the consumer into
       ;; the producer.  If that succeeds, peval again.  Otherwise
       ;; reconstruct the let-values, pevaling the consumer.
       (let ((producer (for-value producer)))
         (or (match consumer
               (($ <lambda-case> src req #f #f #f () gensyms body #f)
                (cond
                 ((inline-values producer src req gensyms body)
                  => for-tail)
                 (else #f)))
               (_ #f))
             (make-let-values lv-src producer (for-tail consumer)))))
      (($ <dynwind> src winder body unwinder)
       (make-dynwind src (for-value winder) (for-tail body)
                     (for-value unwinder)))
      (($ <dynlet> src fluids vals body)
       (make-dynlet src (map for-value fluids) (map for-value vals)
                    (for-tail body)))
      (($ <dynref> src fluid)
       (make-dynref src (for-value fluid)))
      (($ <dynset> src fluid exp)
       (make-dynset src (for-value fluid) (for-value exp)))
      (($ <toplevel-ref> src (? effect-free-primitive? name))
       (if (local-toplevel? name)
           exp
           (resolve-primitives! exp cenv)))
      (($ <toplevel-ref>)
       ;; todo: open private local bindings.
       exp)
      (($ <module-ref>)
       exp)
      (($ <module-set> src mod name public? exp)
       (make-module-set src mod name public? (for-value exp)))
      (($ <toplevel-define> src name exp)
       (make-toplevel-define src name (for-value exp)))
      (($ <toplevel-set> src name exp)
       (make-toplevel-set src name (for-value exp)))
      (($ <primitive-ref>)
       (case ctx
         ((effect) (make-void #f))
         ((test) (make-const #f #t))
         (else exp)))
      (($ <conditional> src condition subsequent alternate)
       (let ((condition (for-test condition)))
         (if (const? condition)
             (if (const-exp condition)
                 (for-tail subsequent)
                 (for-tail alternate))
             (make-conditional src condition
                               (for-tail subsequent)
                               (for-tail alternate)))))
      (($ <application> src
          ($ <primitive-ref> _ '@call-with-values)
          (producer
           ($ <lambda> _ _
              (and consumer
                   ;; No optional or kwargs.
                   ($ <lambda-case>
                      _ req #f rest #f () gensyms body #f)))))
       (for-tail (make-let-values src (make-application src producer '())
                                  consumer)))

      (($ <application> src orig-proc orig-args)
       ;; todo: augment the global env with specialized functions
       (let ((proc (loop orig-proc env counter 'operator)))
         (match proc
           (($ <primitive-ref> _ (? constructor-primitive? name))
            (case ctx
              ((effect test)
               (let ((res (if (eq? ctx 'effect)
                              (make-void #f)
                              (make-const #f #t))))
                 (match (for-value exp)
                   (($ <application> _ ($ <primitive-ref> _ 'cons) (x xs))
                    (for-tail
                     (make-sequence src (list x xs res))))
                   (($ <application> _ ($ <primitive-ref> _ 'list) elts)
                    (for-tail
                     (make-sequence src (append elts (list res)))))
                   (($ <application> _ ($ <primitive-ref> _ 'vector) elts)
                    (for-tail
                     (make-sequence src (append elts (list res)))))
                   (($ <application> _ ($ <primitive-ref> _ 'make-prompt-tag) ())
                    res)
                   (($ <application> _ ($ <primitive-ref> _ 'make-prompt-tag)
                       (($ <const> _ (? string?))))
                    res)
                   (exp exp))))
              (else
               (match (cons name (map for-value orig-args))
                 (('cons head tail)
                  (match tail
                    (($ <const> src ())
                     (make-application src (make-primitive-ref #f 'list)
                                       (list head)))
                    (($ <application> src ($ <primitive-ref> _ 'list) elts)
                     (make-application src (make-primitive-ref #f 'list)
                                       (cons head elts)))
                    (_ (make-application src proc
                                         (list head tail)))))

                 ;; FIXME: these for-tail recursions could take
                 ;; place outside an effort counter.
                 (('car ($ <application> src ($ <primitive-ref> _ 'cons) (head tail)))
                  (for-tail (make-sequence src (list tail head))))
                 (('cdr ($ <application> src ($ <primitive-ref> _ 'cons) (head tail)))
                  (for-tail (make-sequence src (list head tail))))
                 (('car ($ <application> src ($ <primitive-ref> _ 'list) (head . tail)))
                  (for-tail (make-sequence src (append tail (list head)))))
                 (('cdr ($ <application> src ($ <primitive-ref> _ 'list) (head . tail)))
                  (for-tail (make-sequence
                             src
                             (list head
                                   (make-application
                                    src (make-primitive-ref #f 'list) tail)))))
                  
                 (('car ($ <const> src (head . tail)))
                  (for-tail (make-const src head)))
                 (('cdr ($ <const> src (head . tail)))
                  (for-tail (make-const src tail)))

                 ((_ . args)
                  (make-application src proc args))))))
           (($ <primitive-ref> _ (? effect-free-primitive? name))
            (let ((args (map for-value orig-args)))
              (if (every const? args)   ; only simple constants
                  (let-values (((success? values)
                                (apply-primitive name
                                                 (map const-exp args))))
                    (if success?
                        (case ctx
                          ((effect) (make-void #f))
                          ((test)
                           ;; Values truncation: only take the first
                           ;; value.
                           (if (pair? values)
                               (make-const #f (car values))
                               (make-values src '())))
                          (else
                           (make-values src (map (cut make-const src <>)
                                                 values))))
                        (make-application src proc args)))
                  (cond
                   ((and (eq? ctx 'effect) (types-check? name args))
                    (make-void #f))
                   (else
                    (make-application src proc args))))))
           (($ <lambda> _ _
               ($ <lambda-case> _ req opt #f #f inits gensyms body #f))
            ;; Simple case: no rest, no keyword arguments.
            ;; todo: handle the more complex cases
            (let* ((nargs (length orig-args))
                   (nreq (length req))
                   (nopt (if opt (length opt) 0))
                   (key (source-expression proc)))
              (cond
               ((or (< nargs nreq) (> nargs (+ nreq nopt)))
                ;; An error, or effecting arguments.
                (make-application src (for-value orig-proc)
                                  (map for-value orig-args)))
               ((and=> (find-counter key counter) counter-recursive?)
                ;; A recursive call.  Process again in tail context.
                (loop (make-let src (append req (or opt '()))
                                gensyms
                                (append orig-args
                                        (drop inits (- nargs nreq)))
                                body)
                  env counter ctx))
               (else
                ;; An integration at the top-level, the first
                ;; recursion of a recursive procedure, or a nested
                ;; integration of a procedure that hasn't been seen
                ;; yet.
                (let/ec k
                  (define (abort)
                    (k (make-application src
                                         (for-value orig-proc)
                                         (map for-value orig-args))))
                  (define new-counter
                    (cond
                     ;; These first two cases will transfer effort
                     ;; from the current counter into the new
                     ;; counter.
                     ((find-counter key counter)
                      => (lambda (prev)
                           (make-recursive-counter recursive-effort-limit
                                                   operand-size-limit
                                                   prev counter)))
                     (counter
                      (make-nested-counter abort key counter))
                     ;; This case opens a new account, effectively
                     ;; printing money.  It should only do so once
                     ;; for each call site in the source program.
                     (else
                      (make-top-counter effort-limit operand-size-limit
                                        abort key))))
                  (define result
                    (loop (make-let src (append req (or opt '()))
                                    gensyms
                                    (append orig-args
                                            (drop inits (- nargs nreq)))
                                    body)
                      env new-counter ctx))
                      
                  (if counter
                      ;; The nested inlining attempt succeeded.
                      ;; Deposit the unspent effort and size back
                      ;; into the current counter.
                      (transfer! new-counter counter))

                  result)))))
           (_
            (make-application src proc
                              (map for-value orig-args))))))
      (($ <lambda> src meta body)
       (case ctx
         ((effect) (make-void #f))
         ((test) (make-const #f #t))
         ((operator) exp)
         (else
          (make-lambda src meta (for-value body)))))
      (($ <lambda-case> src req opt rest kw inits gensyms body alt)
       (make-lambda-case src req opt rest kw
                         (map for-value inits)
                         gensyms
                         (for-tail body)
                         (and alt (for-tail alt))))
      (($ <sequence> src exps)
       (let lp ((exps exps) (effects '()))
         (match exps
           ((last)
            (if (null? effects)
                (for-tail last)
                (make-sequence
                 src
                 (reverse (cons (for-tail last) effects)))))
           ((head . rest)
            (let ((head (for-effect head)))
              (cond
               ((sequence? head)
                (lp (append (sequence-exps head) rest) effects))
               ((void? head)
                (lp rest effects))
               (else
                (lp rest (cons head effects)))))))))
      (($ <prompt> src tag body handler)
       (define (singly-used-definition x)
         (cond
          ((and (lexical-ref? x)
                ;; Only fetch definitions with single uses.
                (= (lexical-refcount (lexical-ref-gensym x)) 1)
                (lookup (lexical-ref-gensym x)))
           => singly-used-definition)
          (else x)))
       (define (escape-only? handler)
         (match handler
           (($ <lambda-case> _ (_ . _) _ _ _ _ (cont . _) body #f)
            (tree-il-any (lambda (x)
                           (and (lexical-ref? x)
                                (eq? (lexical-ref-gensym x) cont)))
                         body))
           (else #f)))
       (define (thunk-application? x)
         (match x
           (($ <application> _
               ($ <lambda> _ _ ($ <lambda-case> _ () #f #f #f))
               ()) #t)
           (_ #f)))
       (define (make-thunk-application body)
         (define thunk
           (make-lambda #f '()
                        (make-lambda-case #f '() #f #f #f '() '() body #f)))
         (make-application #f thunk '()))

       (match (singly-used-definition tag)
         (($ <application> _ ($ <primitive-ref> _ 'make-prompt-tag)
             (or () ((? constant-expression?))))
          ;; There is no way that an <abort> could know the tag
          ;; for this <prompt>, so we can elide the <prompt>
          ;; entirely.
          (for-tail body))
         (_
          ;; It's a nasty, but this code has another job to do: to
          ;; ensure that either the handler is escape-only, or the
          ;; body is the application of a thunk.  Sad but true.
          (let ((tag (for-value tag))
                (body (for-value body))
                (handler (for-value handler)))
            (make-prompt src tag
                         (if (or (escape-only? handler)
                                 (thunk-application? body))
                             body
                             (make-thunk-application body))
                         handler)))))
      (($ <abort> src tag args tail)
       (make-abort src (for-value tag) (map for-value args)
                   (for-value tail))))))
