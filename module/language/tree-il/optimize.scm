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
       (let* ((new     (map (compose gensym symbol->string) gensyms))
              (mapping (fold vhash-consq mapping gensyms new)))
         (make-lambda-case src req opt rest kw inits new
                           (loop body mapping)
                           (and alt (loop alt mapping)))))
      (($ <lexical-ref> src name gensym)
       ;; Possibly replace GENSYM by the new gensym defined in MAPPING.
       (let ((val (vhash-assq gensym mapping)))
         (if val
             (make-lexical-ref src name (cdr val))
             exp)))
      (($ <lambda> src meta body)
       (make-lambda src meta (loop body mapping)))
      (($ <let> src names gensyms vals body)
       ;; As for `lambda-case' rename GENSYMS to avoid any collision.
       (let* ((new     (map (compose gensym symbol->string) gensyms))
              (mapping (fold vhash-consq mapping gensyms new))
              (vals    (map (cut loop <> mapping) vals))
              (body    (loop body mapping)))
         (make-let src names new vals body)))
      (($ <letrec> src in-order? names gensyms vals body)
       ;; Likewise.
       (let* ((new     (map (compose gensym symbol->string) gensyms))
              (mapping (fold vhash-consq mapping gensyms new))
              (vals    (map (cut loop <> mapping) vals))
              (body    (loop body mapping)))
         (make-letrec src in-order? names new vals body)))
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
    (tree-il-fold (lambda (exp res) #f)
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
  ;; it's very conservative: it bails out if `set!', `prompt', etc. are
  ;; met.

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

  (define (make-values src values)
    (match values
      ((single) single)                           ; 1 value
      ((_ ...)                                    ; 0, or 2 or more values
       (make-application src (make-primitive-ref src 'values)
                         values))))

  (define (const*? x)
    (or (const? x) (lambda? x) (void? x)))

  (define (pure-expression? x)
    ;; Return true if X is pure---i.e., if it is known to have no
    ;; effects and does not allocate storage for a mutable object.
    ;; Note: <module-ref> is not "pure" because it loads a module as a
    ;; side-effect.
    (let loop ((x x))
      (match x
        (($ <void>) #t)
        (($ <const>) #t)
        (($ <lambda>) #t)
        (($ <lambda-case> _ req opt rest kw inits _ body alternate)
         (and (every loop inits) (loop body) (loop alternate)))
        (($ <lexical-ref>) #t)
        (($ <toplevel-ref>) #t)
        (($ <primitive-ref>) #t)
        (($ <conditional> _ condition subsequent alternate)
         (and (loop condition) (loop subsequent) (loop alternate)))
        (($ <application> _ ($ <primitive-ref> _ name) args)
         (and (effect-free-primitive? name)
              (not (constructor-primitive? name))
              (every loop args)))
        (($ <application> _ ($ <lambda> _ body) args)
         (and (loop body) (every loop args)))
        (($ <sequence> _ exps)
         (every loop exps))
        (($ <let> _ _ _ vals body)
         (and (every loop vals) (loop body)))
        (($ <letrec> _ _ _ _ vals body)
         (and (every loop vals) (loop body)))
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
                        (and (equal? (cdr x) new)
                             (make-lexical-ref src name (car x))))
                      env)
           new))
      (($ <lambda> src ()
          (and lc ($ <lambda-case>)))
       ;; This is an anonymous lambda that we're going to inline.  The
       ;; variable allocation process assumes globally unique gensyms, so
       ;; alpha-rename the lambda to avoid any collision with other
       ;; copies of it.
       (make-lambda src '() (alpha-rename lc)))
      (_ new)))

  (catch 'match-error
    (lambda ()
      (let loop ((exp   exp)
                 (env   vlist-null)  ; static environment
                 (calls '()))        ; inlined call stack
        (define (lookup var)
          (and=> (vhash-assq var env) cdr))

        (match exp
          (($ <const>)
           exp)
          (($ <void>)
           exp)
          (($ <lexical-ref> _ _ gensym)
           ;; Propagate only pure expressions.
           (let ((val (lookup gensym)))
             (or (and (pure-expression? val) val) exp)))
          (($ <let> src names gensyms vals body)
           (let* ((vals* (map (cut loop <> env calls) vals))
                  (vals  (map maybe-unconst vals vals*))
                  (body* (loop body
                               (fold vhash-consq env gensyms vals)
                               calls))
                  (body  (maybe-unconst body body*)))
             (if (const? body*)
                 body
                 (let*-values (((stripped) (remove (compose const? car)
                                                   (zip vals gensyms names)))
                               ((vals gensyms names) (unzip3 stripped)))
                   (if (null? stripped)
                       body
                       (make-let src names gensyms vals body))))))
          (($ <letrec> src in-order? names gensyms vals body)
           ;; Things could be done more precisely when IN-ORDER? but
           ;; it's OK not to do it---at worst we lost an optimization
           ;; opportunity.
           (let* ((vals* (map (cut loop <> env calls) vals))
                  (vals  (map maybe-unconst vals vals*))
                  (body* (loop body
                              (fold vhash-consq env gensyms vals)
                              calls))
                  (body  (maybe-unconst body body*)))
             (if (const? body*)
                 body
                 (make-letrec src in-order? names gensyms vals body))))
          (($ <toplevel-ref> src (? effect-free-primitive? name))
           (if (local-toplevel? name)
               exp
               (resolve-primitives! exp cenv)))
          (($ <toplevel-ref>)
           ;; todo: open private local bindings.
           exp)
          (($ <module-ref>)
           exp)
          (($ <toplevel-define> src name exp)
           (make-toplevel-define src name
                                 (maybe-unconst exp (loop exp env '()))))
          (($ <primitive-ref>)
           exp)
          (($ <conditional> src condition subsequent alternate)
           (let ((condition (loop condition env calls)))
             (if (const*? condition)
                 (if (or (lambda? condition) (void? condition)
                         (const-exp condition))
                     (loop subsequent env calls)
                     (loop alternate env calls))
                 (make-conditional src condition
                                   (loop subsequent env calls)
                                   (loop alternate env calls)))))
          (($ <application> src orig-proc orig-args)
           ;; todo: augment the global env with specialized functions
           (let* ((proc  (loop orig-proc env calls))
                  (proc* (maybe-unlambda orig-proc proc env))
                  (args  (map (cut loop <> env calls) orig-args))
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
                              (make-values src (map (cut make-const src <>)
                                                    values))
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
                               (every pure-expression? args))
                          (let* ((params
                                  (append args
                                          (drop inits
                                                (max 0
                                                     (- nargs
                                                        (+ nreq nopt))))))
                                 (body
                                  (loop body
                                        (fold vhash-consq env gensyms params)
                                        (cons (cons proc args) calls))))
                            ;; If the residual code contains recursive
                            ;; calls, give up inlining.
                            (if (code-contains-calls? body proc lookup)
                                app
                                body))
                          app)))
                   (($ <lambda>)
                    app)
                   (($ <toplevel-ref>)
                    app))

                 app)))
          (($ <lambda> src meta body)
           (make-lambda src meta (loop body env calls)))
          (($ <lambda-case> src req opt rest kw inits gensyms body alt)
           (make-lambda-case src req opt rest kw inits gensyms
                             (maybe-unconst body (loop body env calls))
                             alt))
          (($ <sequence> src exps)
           (let ((exps (map (cut loop <> env calls) exps)))
             (if (every pure-expression? exps)
                 (last exps)
                 (match (reverse exps)
                   ;; Remove all expressions but the last one.
                   ((keep rest ...)
                    (let ((rest (remove pure-expression? rest)))
                      (make-sequence src (reverse (cons keep rest))))))))))))
    (lambda _
      ;; We encountered something we don't handle, like `<lexical-set>',
      ;; <abort>, or some other effecting construct, so bail out.
      exp)))
