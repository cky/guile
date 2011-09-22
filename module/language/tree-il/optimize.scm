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
      (($ <conditional> src condition subsequent alternate)
       (make-conditional src
                         (loop condition mapping)
                         (loop subsequent mapping)
                         (loop alternate mapping)))
      (($ <application> src proc args)
       (make-application src (loop proc mapping)
                         (map (cut loop <> mapping) args)))
      (($ <sequence> src exps)
       (make-sequence src (map (cut loop <> mapping) exps))))))

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

(define (code-contains-calls? body proc lookup)
  "Return true if BODY contains calls to PROC.  Use LOOKUP to look up
lexical references."
  (tree-il-any
   (lambda (exp)
     (match exp
       (($ <application> _
           (and ref ($ <lexical-ref> _ _ gensym)) _)
        (or (equal? ref proc)
            (equal? (lookup gensym) proc)))
       (($ <application>
           (and proc* ($ <lambda>)))
        (equal? proc* proc))
       (_ #f)))
   body))

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

(define* (peval exp #:optional (cenv (current-module)) (env vlist-null))
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

  (define var-table (build-var-table exp))
  (define (record-lexical-bindings! x)
    (set! var-table (build-var-table x var-table))
    x)
  (define (assigned-lexical? sym)
    (let ((v (vhash-assq sym var-table)))
      (and v (var-set? (cdr v)))))
  (define (lexical-refcount sym)
    (let ((v (vhash-assq sym var-table)))
      (if v (var-refcount (cdr v)) 0)))

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
             ($ <module-set>))          ; 
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
      ((single) single)                           ; 1 value
      ((_ ...)                                    ; 0, or 2 or more values
       (make-application src (make-primitive-ref src 'values)
                         values))))

  (define (const*? x)
    (or (const? x) (lambda? x) (void? x)))

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
        (_ #f))))

  (define (mutable? exp)
    ;; Return #t if EXP is a mutable object.
    ;; todo: add an option to assume pairs are immutable
    (or (pair? exp)
        (vector? exp)
        (struct? exp)
        (string? exp)))

  (define (make-value-construction src exp)
    ;; Return an expression that builds a fresh copy of EXP at run-time,
    ;; or #f.
    (let loop ((exp exp))
      (match exp
        ((_ _ ...)                                 ; non-empty proper list
         (let ((args (map loop exp)))
           (and (every struct? args)
                (make-application src (make-primitive-ref src 'list)
                                  args))))
        ((h . (? (negate pair?) t))                ; simple pair
         (let ((h (loop h))
               (t (loop t)))
           (and h t
                (make-application src (make-primitive-ref src 'cons)
                                  (list h t)))))
        ((? vector?)                               ; vector
         (let ((args (map loop (vector->list exp))))
           (and (every struct? args)
                (make-application src (make-primitive-ref src 'vector)
                                  args))))
        ((? number?) (make-const src exp))
        ((? string?) (make-const src exp))
        ((? symbol?) (make-const src exp))
        ;((? bytevector?) (make-const src exp))
        (_ #f))))

  (define (maybe-unconst orig new)
    ;; If NEW is a constant, change it to a non-constant if need be.
    ;; Expressions that build a mutable object, such as `(list 1 2)',
    ;; must not be replaced by a constant; this procedure "undoes" the
    ;; change from `(list 1 2)' to `'(1 2)'.
    (match new
      (($ <const> src (? mutable? value))
       (if (equal? new orig)
           new
           (or (make-value-construction src value) orig)))
      (_ new)))

  (define (maybe-unlambda orig new env)
    ;; If NEW is a named lambda and ORIG is what it looked like before
    ;; partial evaluation, then attempt to replace NEW with a lexical
    ;; ref, to avoid code duplication.
    (match new
      (($ <lambda> src (= (cut assq-ref <> 'name) (? symbol? name))
          ($ <lambda-case> _ req opt rest kw inits gensyms body))
       ;; Look for NEW in the current environment, starting from the
       ;; outermost frame.
       (or (vlist-any (lambda (x)
                        (and (eq? (cdr x) new)
                             (begin
                               (record-residual-lexical-reference! (car x))
                               (make-lexical-ref src name (car x)))))
                      env)
           new))
      (($ <lambda> src ()
          (and lc ($ <lambda-case>)))
       ;; This is an anonymous lambda that we're going to inline.
       ;; Inlining creates new variable bindings, so we need to provide
       ;; the new code with fresh names.
       (make-lambda src '() (record-lexical-bindings! (alpha-rename lc))))
      (_ new)))

  (catch 'match-error
    (lambda ()
      (let loop ((exp   exp)
                 (env   vlist-null)  ; static environment
                 (calls '())         ; inlined call stack
                 (ctx 'value))       ; effect, value, test, or call
        (define (lookup var)
          (and=> (vhash-assq var env) cdr))

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
                 ((or (const? val)
                      (void? val)
                      (lexical-ref? val)
                      (toplevel-ref? val)
                      (primitive-ref? val))
                  ;; Always propagate simple values that cannot lead to
                  ;; code bloat.
                  (case ctx
                    ((test) (loop val env calls 'test))
                    (else val)))
                 ((= 1 (lexical-refcount gensym))
                  ;; Always propagate values referenced only once.
                  ;; There is no need to rename the bindings, as they
                  ;; are only being moved, not copied.
                  (case ctx
                    ((test) (loop val env calls 'test))
                    (else val)))
                 (else
                  ;; Always propagate constant expressions.  FIXME: leads to
                  ;; divergence!
                  (case ctx
                    ((test) (loop val env calls 'test))
                    (else val))))))))
          (($ <lexical-set> src name gensym exp)
           (if (zero? (lexical-refcount gensym))
               (let ((exp (loop exp env calls 'effect)))
                 (if (void? exp)
                     exp
                     (make-sequence src (list exp (make-void #f)))))
               (begin
                 (record-residual-lexical-reference! gensym)
                 (make-lexical-set src name gensym
                                   (maybe-unconst
                                    exp
                                    (loop exp env calls 'value))))))
          (($ <let> src names gensyms vals body)
           (let* ((vals* (map (cut loop <> env calls 'value) vals))
                  (vals  (map maybe-unconst vals vals*))
                  (body* (loop body
                               (fold vhash-consq env gensyms vals)
                               calls
                               ctx))
                  (body  (maybe-unconst body body*)))
             (if (const? body*)
                 body
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
                       (make-let src names gensyms vals body))))))
          (($ <letrec> src in-order? names gensyms vals body)
           ;; Things could be done more precisely when IN-ORDER? but
           ;; it's OK not to do it---at worst we lost an optimization
           ;; opportunity.
           (let* ((vals* (map (cut loop <> env calls 'value) vals))
                  (vals  (map maybe-unconst vals vals*))
                  (body* (loop body
                               (fold vhash-consq env gensyms vals)
                               calls
                               ctx))
                  (body  (maybe-unconst body body*)))
             (if (const? body*)
                 body
                 (make-letrec src in-order? names gensyms vals body))))
          (($ <fix> src names gensyms vals body)
           (let* ((vals (map (cut loop <> env calls 'value) vals))
                  (body* (loop body
                               (fold vhash-consq env gensyms vals)
                               calls
                               ctx))
                  (body  (maybe-unconst body body*)))
             (if (const? body*)
                 body
                 (make-fix src names gensyms vals body))))
          (($ <let-values> lv-src producer consumer)
           ;; Peval the producer, then try to inline the consumer into
           ;; the producer.  If that succeeds, peval again.  Otherwise
           ;; reconstruct the let-values, pevaling the consumer.
           (let ((producer (maybe-unconst producer
                                          (loop producer env calls 'value))))
             (or (match consumer
                   (($ <lambda-case> src req #f #f #f () gensyms body #f)
                    (cond
                     ((inline-values producer src req gensyms body)
                      => (cut loop <> env calls ctx))
                     (else #f)))
                   (_ #f))
                 (make-let-values lv-src producer
                                  (loop consumer env calls ctx)))))
          (($ <dynwind> src winder body unwinder)
           (make-dynwind src (loop winder env calls 'value)
                         (loop body env calls ctx)
                         (loop unwinder env calls 'value)))
          (($ <dynlet> src fluids vals body)
           (make-dynlet src
                        (map maybe-unconst fluids
                             (map (cut loop <> env calls 'value) fluids))
                        (map maybe-unconst vals
                             (map (cut loop <> env calls 'value) vals))
                        (maybe-unconst body (loop body env calls ctx))))
          (($ <dynref> src fluid)
           (make-dynref src
                        (maybe-unconst fluid (loop fluid env calls 'value))))
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
           (make-module-set src mod name public?
                            (maybe-unconst exp (loop exp env '() 'value))))
          (($ <toplevel-define> src name exp)
           (make-toplevel-define src name
                                 (maybe-unconst exp (loop exp env '() 'value))))
          (($ <toplevel-set> src name exp)
           (make-toplevel-set src name
                              (maybe-unconst exp (loop exp env '() 'value))))
          (($ <primitive-ref>)
           (case ctx
             ((effect) (make-void #f))
             ((test) (make-const #f #t))
             (else exp)))
          (($ <conditional> src condition subsequent alternate)
           (let ((condition (loop condition env calls 'test)))
             (if (const? condition)
                 (if (const-exp condition)
                     (loop subsequent env calls ctx)
                     (loop alternate env calls ctx))
                 (make-conditional src condition
                                   (loop subsequent env calls ctx)
                                   (loop alternate env calls ctx)))))
          (($ <application> src
              ($ <primitive-ref> _ '@call-with-values)
              (producer
               ($ <lambda> _ _
                  (and consumer
                       ;; No optional or kwargs.
                       ($ <lambda-case>
                          _ req #f rest #f () gensyms body #f)))))
           (loop (make-let-values src (make-application src producer '())
                                  consumer)
                 env calls ctx))

          (($ <application> src orig-proc orig-args)
           ;; todo: augment the global env with specialized functions
           (let* ((proc  (loop orig-proc env calls 'call))
                  (proc* (maybe-unlambda orig-proc proc env))
                  (args  (map (cut loop <> env calls 'value) orig-args))
                  (args* (map (cut maybe-unlambda <> <> env)
                              orig-args
                              (map maybe-unconst orig-args args)))
                  (app   (make-application src proc* args*)))
             ;; If at least one of ARGS is static (to avoid infinite
             ;; inlining) and this call hasn't already been expanded
             ;; before (to avoid infinite recursion), then expand it
             ;; (todo: emit an infinite recursion warning.)
             (if (and (or (null? args) (any const*? args))
                      (not (member (cons proc args) calls)))
                 (match proc
                   (($ <primitive-ref> _ (? effect-free-primitive? name))
                    (if (every const? args)  ; only simple constants
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
                              app))
                        app))
                   (($ <primitive-ref>)
                    ;; An effectful primitive.
                    app)
                   (($ <lambda> _ _
                       ($ <lambda-case> _ req opt #f #f inits gensyms body))
                    ;; Simple case: no rest, no keyword arguments.
                    ;; todo: handle the more complex cases
                    (let ((nargs  (length args))
                          (nreq   (length req))
                          (nopt   (if opt (length opt) 0)))
                      (if (and (>= nargs nreq) (<= nargs (+ nreq nopt))
                               (every constant-expression? args))
                          (let* ((params
                                  (append args
                                          (drop inits
                                                (max 0
                                                     (- nargs
                                                        (+ nreq nopt))))))
                                 (body
                                  (loop body
                                        (fold vhash-consq env gensyms params)
                                        (cons (cons proc args) calls)
                                        ctx)))
                            ;; If the residual code contains recursive
                            ;; calls, give up inlining.
                            (if (code-contains-calls? body proc lookup)
                                app
                                body))
                          app)))
                   (($ <lambda>)
                    app)
                   (($ <toplevel-ref>)
                    app)
                   
                   ;; In practice, this is the clause that stops peval:
                   ;; module-ref applications (produced by macros,
                   ;; typically) don't match, and so this throws,
                   ;; aborting peval for an entire expression.
                   )

                 app)))
          (($ <lambda> src meta body)
           (case ctx
             ((effect) (make-void #f))
             ((test) (make-const #f #t))
             (else
              (make-lambda src meta (loop body env calls 'value)))))
          (($ <lambda-case> src req opt rest kw inits gensyms body alt)
           (make-lambda-case src req opt rest kw
                             (map maybe-unconst inits
                                  (map (cut loop <> env calls 'value) inits))
                             gensyms
                             (maybe-unconst body (loop body env calls ctx))
                             alt))
          (($ <sequence> src exps)
           (let lp ((exps exps) (effects '()))
             (match exps
               ((last)
                (if (null? effects)
                    (loop last env calls ctx)
                    (make-sequence src (append (reverse effects)
                                               (list
                                                (maybe-unconst last
                                                               (loop last env calls ctx)))))))
               ((head . rest)
                (let ((head (loop head env calls 'effect)))
                  (cond
                   ((sequence? head)
                    (lp (append (sequence-exps head) rest) effects))
                   ((void? head)
                    (lp rest effects))
                   (else
                    (lp rest (cons head effects))))))))))))
    (lambda _
      ;; We encountered something we don't handle, like <abort> or
      ;; <prompt>, so bail out.
      exp)))
