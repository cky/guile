(eval-when (compile) (set-current-module (resolve-module (quote (guile)))))
(if #f #f)

(letrec*
  ((make-void
     (lambda (src) (make-struct (vector-ref %expanded-vtables 0) 0 src)))
   (make-const
     (lambda (src exp)
       (make-struct (vector-ref %expanded-vtables 1) 0 src exp)))
   (make-primitive-ref
     (lambda (src name)
       (make-struct (vector-ref %expanded-vtables 2) 0 src name)))
   (make-lexical-ref
     (lambda (src name gensym)
       (make-struct (vector-ref %expanded-vtables 3) 0 src name gensym)))
   (make-lexical-set
     (lambda (src name gensym exp)
       (make-struct (vector-ref %expanded-vtables 4) 0 src name gensym exp)))
   (make-module-ref
     (lambda (src mod name public?)
       (make-struct (vector-ref %expanded-vtables 5) 0 src mod name public?)))
   (make-module-set
     (lambda (src mod name public? exp)
       (make-struct
         (vector-ref %expanded-vtables 6)
         0
         src
         mod
         name
         public?
         exp)))
   (make-toplevel-ref
     (lambda (src name)
       (make-struct (vector-ref %expanded-vtables 7) 0 src name)))
   (make-toplevel-set
     (lambda (src name exp)
       (make-struct (vector-ref %expanded-vtables 8) 0 src name exp)))
   (make-toplevel-define
     (lambda (src name exp)
       (make-struct (vector-ref %expanded-vtables 9) 0 src name exp)))
   (make-conditional
     (lambda (src test consequent alternate)
       (make-struct
         (vector-ref %expanded-vtables 10)
         0
         src
         test
         consequent
         alternate)))
   (make-application
     (lambda (src proc args)
       (make-struct (vector-ref %expanded-vtables 11) 0 src proc args)))
   (make-sequence
     (lambda (src exps)
       (make-struct (vector-ref %expanded-vtables 12) 0 src exps)))
   (make-lambda
     (lambda (src meta body)
       (make-struct (vector-ref %expanded-vtables 13) 0 src meta body)))
   (make-lambda-case
     (lambda (src req opt rest kw inits gensyms body alternate)
       (make-struct
         (vector-ref %expanded-vtables 14)
         0
         src
         req
         opt
         rest
         kw
         inits
         gensyms
         body
         alternate)))
   (make-let
     (lambda (src names gensyms vals body)
       (make-struct
         (vector-ref %expanded-vtables 15)
         0
         src
         names
         gensyms
         vals
         body)))
   (make-letrec
     (lambda (src in-order? names gensyms vals body)
       (make-struct
         (vector-ref %expanded-vtables 16)
         0
         src
         in-order?
         names
         gensyms
         vals
         body)))
   (make-dynlet
     (lambda (src fluids vals body)
       (make-struct
         (vector-ref %expanded-vtables 17)
         0
         src
         fluids
         vals
         body)))
   (lambda?
     (lambda (x)
       (and (struct? x)
            (eq? (struct-vtable x) (vector-ref %expanded-vtables 13)))))
   (lambda-meta (lambda (x) (struct-ref x 1)))
   (set-lambda-meta! (lambda (x v) (struct-set! x 1 v)))
   (top-level-eval-hook (lambda (x mod) (primitive-eval x)))
   (local-eval-hook (lambda (x mod) (primitive-eval x)))
   (session-id
     (let ((v (module-variable (current-module) 'syntax-session-id)))
       (lambda () ((variable-ref v)))))
   (put-global-definition-hook
     (lambda (symbol type val)
       (module-define!
         (current-module)
         symbol
         (make-syntax-transformer symbol type val))))
   (get-global-definition-hook
     (lambda (symbol module)
       (if (and (not module) (current-module))
         (warn "module system is booted, we should have a module" symbol))
       (let ((v (module-variable
                  (if module (resolve-module (cdr module)) (current-module))
                  symbol)))
         (and v
              (variable-bound? v)
              (let ((val (variable-ref v)))
                (and (macro? val)
                     (macro-type val)
                     (cons (macro-type val) (macro-binding val))))))))
   (decorate-source
     (lambda (e s)
       (if (and s (supports-source-properties? e))
         (set-source-properties! e s))
       e))
   (maybe-name-value!
     (lambda (name val)
       (if (lambda? val)
         (let ((meta (lambda-meta val)))
           (if (not (assq 'name meta))
             (set-lambda-meta! val (acons 'name name meta)))))))
   (build-void (lambda (source) (make-void source)))
   (build-application
     (lambda (source fun-exp arg-exps)
       (make-application source fun-exp arg-exps)))
   (build-conditional
     (lambda (source test-exp then-exp else-exp)
       (make-conditional source test-exp then-exp else-exp)))
   (build-dynlet
     (lambda (source fluids vals body)
       (make-dynlet source fluids vals body)))
   (build-lexical-reference
     (lambda (type source name var) (make-lexical-ref source name var)))
   (build-lexical-assignment
     (lambda (source name var exp)
       (maybe-name-value! name exp)
       (make-lexical-set source name var exp)))
   (analyze-variable
     (lambda (mod var modref-cont bare-cont)
       (if (not mod)
         (bare-cont var)
         (let ((kind (car mod)) (mod (cdr mod)))
           (let ((key kind))
             (cond ((memv key '(public)) (modref-cont mod var #t))
                   ((memv key '(private))
                    (if (not (equal? mod (module-name (current-module))))
                      (modref-cont mod var #f)
                      (bare-cont var)))
                   ((memv key '(bare)) (bare-cont var))
                   ((memv key '(hygiene))
                    (if (and (not (equal? mod (module-name (current-module))))
                             (module-variable (resolve-module mod) var))
                      (modref-cont mod var #f)
                      (bare-cont var)))
                   (else (syntax-violation #f "bad module kind" var mod))))))))
   (build-global-reference
     (lambda (source var mod)
       (analyze-variable
         mod
         var
         (lambda (mod var public?) (make-module-ref source mod var public?))
         (lambda (var) (make-toplevel-ref source var)))))
   (build-global-assignment
     (lambda (source var exp mod)
       (maybe-name-value! var exp)
       (analyze-variable
         mod
         var
         (lambda (mod var public?)
           (make-module-set source mod var public? exp))
         (lambda (var) (make-toplevel-set source var exp)))))
   (build-global-definition
     (lambda (source var exp)
       (maybe-name-value! var exp)
       (make-toplevel-define source var exp)))
   (build-simple-lambda
     (lambda (src req rest vars meta exp)
       (make-lambda
         src
         meta
         (make-lambda-case src req #f rest #f '() vars exp #f))))
   (build-case-lambda
     (lambda (src meta body) (make-lambda src meta body)))
   (build-lambda-case
     (lambda (src req opt rest kw inits vars body else-case)
       (make-lambda-case src req opt rest kw inits vars body else-case)))
   (build-primref
     (lambda (src name)
       (if (equal? (module-name (current-module)) '(guile))
         (make-toplevel-ref src name)
         (make-module-ref src '(guile) name #f))))
   (build-data (lambda (src exp) (make-const src exp)))
   (build-sequence
     (lambda (src exps)
       (if (null? (cdr exps)) (car exps) (make-sequence src exps))))
   (build-let
     (lambda (src ids vars val-exps body-exp)
       (for-each maybe-name-value! ids val-exps)
       (if (null? vars) body-exp (make-let src ids vars val-exps body-exp))))
   (build-named-let
     (lambda (src ids vars val-exps body-exp)
       (let ((f (car vars)) (f-name (car ids)) (vars (cdr vars)) (ids (cdr ids)))
         (let ((proc (build-simple-lambda src ids #f vars '() body-exp)))
           (maybe-name-value! f-name proc)
           (for-each maybe-name-value! ids val-exps)
           (make-letrec
             src
             #f
             (list f-name)
             (list f)
             (list proc)
             (build-application
               src
               (build-lexical-reference 'fun src f-name f)
               val-exps))))))
   (build-letrec
     (lambda (src in-order? ids vars val-exps body-exp)
       (if (null? vars)
         body-exp
         (begin
           (for-each maybe-name-value! ids val-exps)
           (make-letrec src in-order? ids vars val-exps body-exp)))))
   (make-syntax-object
     (lambda (expression wrap module)
       (vector 'syntax-object expression wrap module)))
   (syntax-object?
     (lambda (x)
       (and (vector? x)
            (= (vector-length x) 4)
            (eq? (vector-ref x 0) 'syntax-object))))
   (syntax-object-expression (lambda (x) (vector-ref x 1)))
   (syntax-object-wrap (lambda (x) (vector-ref x 2)))
   (syntax-object-module (lambda (x) (vector-ref x 3)))
   (set-syntax-object-expression!
     (lambda (x update) (vector-set! x 1 update)))
   (set-syntax-object-wrap!
     (lambda (x update) (vector-set! x 2 update)))
   (set-syntax-object-module!
     (lambda (x update) (vector-set! x 3 update)))
   (source-annotation
     (lambda (x)
       (let ((props (source-properties
                      (if (syntax-object? x) (syntax-object-expression x) x))))
         (and (pair? props) props))))
   (extend-env
     (lambda (labels bindings r)
       (if (null? labels)
         r
         (extend-env
           (cdr labels)
           (cdr bindings)
           (cons (cons (car labels) (car bindings)) r)))))
   (extend-var-env
     (lambda (labels vars r)
       (if (null? labels)
         r
         (extend-var-env
           (cdr labels)
           (cdr vars)
           (cons (cons (car labels) (cons 'lexical (car vars))) r)))))
   (macros-only-env
     (lambda (r)
       (if (null? r)
         '()
         (let ((a (car r)))
           (if (eq? (cadr a) 'macro)
             (cons a (macros-only-env (cdr r)))
             (macros-only-env (cdr r)))))))
   (lookup
     (lambda (x r mod)
       (let ((t (assq x r)))
         (cond (t (cdr t))
               ((symbol? x) (or (get-global-definition-hook x mod) '(global)))
               (else '(displaced-lexical))))))
   (global-extend
     (lambda (type sym val) (put-global-definition-hook sym type val)))
   (nonsymbol-id?
     (lambda (x)
       (and (syntax-object? x) (symbol? (syntax-object-expression x)))))
   (id? (lambda (x)
          (if (symbol? x)
            #t
            (and (syntax-object? x) (symbol? (syntax-object-expression x))))))
   (id-sym-name&marks
     (lambda (x w)
       (if (syntax-object? x)
         (values
           (syntax-object-expression x)
           (join-marks (car w) (car (syntax-object-wrap x))))
         (values x (car w)))))
   (gen-label
     (lambda ()
       (string-append "l-" (session-id) (symbol->string (gensym "-")))))
   (gen-labels
     (lambda (ls)
       (if (null? ls) '() (cons (gen-label) (gen-labels (cdr ls))))))
   (make-ribcage
     (lambda (symnames marks labels)
       (vector 'ribcage symnames marks labels)))
   (ribcage?
     (lambda (x)
       (and (vector? x)
            (= (vector-length x) 4)
            (eq? (vector-ref x 0) 'ribcage))))
   (ribcage-symnames (lambda (x) (vector-ref x 1)))
   (ribcage-marks (lambda (x) (vector-ref x 2)))
   (ribcage-labels (lambda (x) (vector-ref x 3)))
   (set-ribcage-symnames! (lambda (x update) (vector-set! x 1 update)))
   (set-ribcage-marks! (lambda (x update) (vector-set! x 2 update)))
   (set-ribcage-labels! (lambda (x update) (vector-set! x 3 update)))
   (anti-mark
     (lambda (w) (cons (cons #f (car w)) (cons 'shift (cdr w)))))
   (extend-ribcage!
     (lambda (ribcage id label)
       (set-ribcage-symnames!
         ribcage
         (cons (syntax-object-expression id) (ribcage-symnames ribcage)))
       (set-ribcage-marks!
         ribcage
         (cons (car (syntax-object-wrap id)) (ribcage-marks ribcage)))
       (set-ribcage-labels! ribcage (cons label (ribcage-labels ribcage)))))
   (make-binding-wrap
     (lambda (ids labels w)
       (if (null? ids)
         w
         (cons (car w)
               (cons (let* ((labelvec (list->vector labels)) (n (vector-length labelvec)))
                       (let ((symnamevec (make-vector n)) (marksvec (make-vector n)))
                         (let f ((ids ids) (i 0))
                           (if (not (null? ids))
                             (call-with-values
                               (lambda () (id-sym-name&marks (car ids) w))
                               (lambda (symname marks)
                                 (vector-set! symnamevec i symname)
                                 (vector-set! marksvec i marks)
                                 (f (cdr ids) (+ i 1))))))
                         (make-ribcage symnamevec marksvec labelvec)))
                     (cdr w))))))
   (smart-append (lambda (m1 m2) (if (null? m2) m1 (append m1 m2))))
   (join-wraps
     (lambda (w1 w2)
       (let ((m1 (car w1)) (s1 (cdr w1)))
         (if (null? m1)
           (if (null? s1) w2 (cons (car w2) (smart-append s1 (cdr w2))))
           (cons (smart-append m1 (car w2)) (smart-append s1 (cdr w2)))))))
   (join-marks (lambda (m1 m2) (smart-append m1 m2)))
   (same-marks?
     (lambda (x y)
       (or (eq? x y)
           (and (not (null? x))
                (not (null? y))
                (eq? (car x) (car y))
                (same-marks? (cdr x) (cdr y))))))
   (id-var-name
     (lambda (id w)
       (letrec*
         ((search
            (lambda (sym subst marks)
              (if (null? subst)
                (values #f marks)
                (let ((fst (car subst)))
                  (if (eq? fst 'shift)
                    (search sym (cdr subst) (cdr marks))
                    (let ((symnames (ribcage-symnames fst)))
                      (if (vector? symnames)
                        (search-vector-rib sym subst marks symnames fst)
                        (search-list-rib sym subst marks symnames fst))))))))
          (search-list-rib
            (lambda (sym subst marks symnames ribcage)
              (let f ((symnames symnames) (i 0))
                (cond ((null? symnames) (search sym (cdr subst) marks))
                      ((and (eq? (car symnames) sym)
                            (same-marks? marks (list-ref (ribcage-marks ribcage) i)))
                       (values (list-ref (ribcage-labels ribcage) i) marks))
                      (else (f (cdr symnames) (+ i 1)))))))
          (search-vector-rib
            (lambda (sym subst marks symnames ribcage)
              (let ((n (vector-length symnames)))
                (let f ((i 0))
                  (cond ((= i n) (search sym (cdr subst) marks))
                        ((and (eq? (vector-ref symnames i) sym)
                              (same-marks? marks (vector-ref (ribcage-marks ribcage) i)))
                         (values (vector-ref (ribcage-labels ribcage) i) marks))
                        (else (f (+ i 1)))))))))
         (cond ((symbol? id) (or (search id (cdr w) (car w)) id))
               ((syntax-object? id)
                (let ((id (syntax-object-expression id)) (w1 (syntax-object-wrap id)))
                  (let ((marks (join-marks (car w) (car w1))))
                    (call-with-values
                      (lambda () (search id (cdr w) marks))
                      (lambda (new-id marks) (or new-id (search id (cdr w1) marks) id))))))
               (else (syntax-violation 'id-var-name "invalid id" id))))))
   (locally-bound-identifiers
     (lambda (w mod)
       (letrec*
         ((scan (lambda (subst results)
                  (if (null? subst)
                    results
                    (let ((fst (car subst)))
                      (if (eq? fst 'shift)
                        (scan (cdr subst) results)
                        (let ((symnames (ribcage-symnames fst)) (marks (ribcage-marks fst)))
                          (if (vector? symnames)
                            (scan-vector-rib subst symnames marks results)
                            (scan-list-rib subst symnames marks results))))))))
          (scan-list-rib
            (lambda (subst symnames marks results)
              (let f ((symnames symnames) (marks marks) (results results))
                (if (null? symnames)
                  (scan (cdr subst) results)
                  (f (cdr symnames)
                     (cdr marks)
                     (cons (wrap (car symnames) (anti-mark (cons (car marks) subst)) mod)
                           results))))))
          (scan-vector-rib
            (lambda (subst symnames marks results)
              (let ((n (vector-length symnames)))
                (let f ((i 0) (results results))
                  (if (= i n)
                    (scan (cdr subst) results)
                    (f (+ i 1)
                       (cons (wrap (vector-ref symnames i)
                                   (anti-mark (cons (vector-ref marks i) subst))
                                   mod)
                             results))))))))
         (scan (cdr w) '()))))
   (resolve-identifier
     (lambda (id w r mod)
       (letrec*
         ((resolve-global
            (lambda (var mod)
              (let ((b (or (get-global-definition-hook var mod) '(global))))
                (if (eq? (car b) 'global)
                  (values 'global var mod)
                  (values (car b) (cdr b) mod)))))
          (resolve-lexical
            (lambda (label mod)
              (let ((b (or (assq-ref r label) '(displaced-lexical))))
                (values (car b) (cdr b) mod)))))
         (let ((n (id-var-name id w)))
           (cond ((symbol? n)
                  (resolve-global
                    n
                    (if (syntax-object? id) (syntax-object-module id) mod)))
                 ((string? n)
                  (resolve-lexical
                    n
                    (if (syntax-object? id) (syntax-object-module id) mod)))
                 (else (error "unexpected id-var-name" id w n)))))))
   (transformer-environment
     (make-fluid
       (lambda (k)
         (error "called outside the dynamic extent of a syntax transformer"))))
   (with-transformer-environment
     (lambda (k) ((fluid-ref transformer-environment) k)))
   (free-id=?
     (lambda (i j)
       (and (eq? (let ((x i)) (if (syntax-object? x) (syntax-object-expression x) x))
                 (let ((x j)) (if (syntax-object? x) (syntax-object-expression x) x)))
            (eq? (id-var-name i '(())) (id-var-name j '(()))))))
   (bound-id=?
     (lambda (i j)
       (if (and (syntax-object? i) (syntax-object? j))
         (and (eq? (syntax-object-expression i) (syntax-object-expression j))
              (same-marks?
                (car (syntax-object-wrap i))
                (car (syntax-object-wrap j))))
         (eq? i j))))
   (valid-bound-ids?
     (lambda (ids)
       (and (let all-ids? ((ids ids))
              (or (null? ids) (and (id? (car ids)) (all-ids? (cdr ids)))))
            (distinct-bound-ids? ids))))
   (distinct-bound-ids?
     (lambda (ids)
       (let distinct? ((ids ids))
         (or (null? ids)
             (and (not (bound-id-member? (car ids) (cdr ids)))
                  (distinct? (cdr ids)))))))
   (bound-id-member?
     (lambda (x list)
       (and (not (null? list))
            (or (bound-id=? x (car list)) (bound-id-member? x (cdr list))))))
   (wrap (lambda (x w defmod)
           (cond ((and (null? (car w)) (null? (cdr w))) x)
                 ((syntax-object? x)
                  (make-syntax-object
                    (syntax-object-expression x)
                    (join-wraps w (syntax-object-wrap x))
                    (syntax-object-module x)))
                 ((null? x) x)
                 (else (make-syntax-object x w defmod)))))
   (source-wrap
     (lambda (x w s defmod) (wrap (decorate-source x s) w defmod)))
   (expand-sequence
     (lambda (body r w s mod)
       (build-sequence
         s
         (let dobody ((body body) (r r) (w w) (mod mod))
           (if (null? body)
             '()
             (let ((first (expand (car body) r w mod)))
               (cons first (dobody (cdr body) r w mod))))))))
   (expand-top-sequence
     (lambda (body r w s m esew mod)
       (letrec*
         ((scan (lambda (body r w s m esew mod exps)
                  (if (null? body)
                    exps
                    (call-with-values
                      (lambda ()
                        (call-with-values
                          (lambda ()
                            (let ((e (car body)))
                              (syntax-type e r w (or (source-annotation e) s) #f mod #f)))
                          (lambda (type value form e w s mod)
                            (let ((key type))
                              (cond ((memv key '(begin-form))
                                     (let* ((tmp e) (tmp-1 ($sc-dispatch tmp '(_))))
                                       (if tmp-1
                                         (apply (lambda () exps) tmp-1)
                                         (let ((tmp-1 ($sc-dispatch tmp '(_ any . each-any))))
                                           (if tmp-1
                                             (apply (lambda (e1 e2) (scan (cons e1 e2) r w s m esew mod exps))
                                                    tmp-1)
                                             (syntax-violation
                                               #f
                                               "source expression failed to match any pattern"
                                               tmp))))))
                                    ((memv key '(local-syntax-form))
                                     (expand-local-syntax
                                       value
                                       e
                                       r
                                       w
                                       s
                                       mod
                                       (lambda (body r w s mod) (scan body r w s m esew mod exps))))
                                    ((memv key '(eval-when-form))
                                     (let* ((tmp-1 e) (tmp ($sc-dispatch tmp-1 '(_ each-any any . each-any))))
                                       (if tmp
                                         (apply (lambda (x e1 e2)
                                                  (let ((when-list (parse-when-list e x)) (body (cons e1 e2)))
                                                    (cond ((eq? m 'e)
                                                           (if (memq 'eval when-list)
                                                             (scan body
                                                                   r
                                                                   w
                                                                   s
                                                                   (if (memq 'expand when-list) 'c&e 'e)
                                                                   '(eval)
                                                                   mod
                                                                   exps)
                                                             (begin
                                                               (if (memq 'expand when-list)
                                                                 (top-level-eval-hook
                                                                   (expand-top-sequence body r w s 'e '(eval) mod)
                                                                   mod))
                                                               (values exps))))
                                                          ((memq 'load when-list)
                                                           (cond ((or (memq 'compile when-list)
                                                                      (memq 'expand when-list)
                                                                      (and (eq? m 'c&e) (memq 'eval when-list)))
                                                                  (scan body r w s 'c&e '(compile load) mod exps))
                                                                 ((memq m '(c c&e))
                                                                  (scan body r w s 'c '(load) mod exps))
                                                                 (else (values exps))))
                                                          ((or (memq 'compile when-list)
                                                               (memq 'expand when-list)
                                                               (and (eq? m 'c&e) (memq 'eval when-list)))
                                                           (top-level-eval-hook
                                                             (expand-top-sequence body r w s 'e '(eval) mod)
                                                             mod)
                                                           (values exps))
                                                          (else (values exps)))))
                                                tmp)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           tmp-1))))
                                    ((memv key '(define-syntax-form define-syntax-parameter-form))
                                     (let ((n (id-var-name value w)) (r (macros-only-env r)))
                                       (let ((key m))
                                         (cond ((memv key '(c))
                                                (cond ((memq 'compile esew)
                                                       (let ((e (expand-install-global n (expand e r w mod))))
                                                         (top-level-eval-hook e mod)
                                                         (if (memq 'load esew) (values (cons e exps)) (values exps))))
                                                      ((memq 'load esew)
                                                       (values
                                                         (cons (expand-install-global n (expand e r w mod)) exps)))
                                                      (else (values exps))))
                                               ((memv key '(c&e))
                                                (let ((e (expand-install-global n (expand e r w mod))))
                                                  (top-level-eval-hook e mod)
                                                  (values (cons e exps))))
                                               (else
                                                (if (memq 'eval esew)
                                                  (top-level-eval-hook
                                                    (expand-install-global n (expand e r w mod))
                                                    mod))
                                                (values exps))))))
                                    ((memv key '(define-form))
                                     (let* ((n (id-var-name value w)) (type (car (lookup n r mod))) (key type))
                                       (cond ((memv key '(global core macro module-ref))
                                              (if (and (memq m '(c c&e))
                                                       (not (module-local-variable (current-module) n))
                                                       (current-module))
                                                (let ((old (module-variable (current-module) n)))
                                                  (if (and (variable? old) (variable-bound? old))
                                                    (module-define! (current-module) n (variable-ref old))
                                                    (module-add! (current-module) n (make-undefined-variable)))))
                                              (values
                                                (cons (if (eq? m 'c&e)
                                                        (let ((x (build-global-definition s n (expand e r w mod))))
                                                          (top-level-eval-hook x mod)
                                                          x)
                                                        (lambda () (build-global-definition s n (expand e r w mod))))
                                                      exps)))
                                             ((memv key '(displaced-lexical))
                                              (syntax-violation
                                                #f
                                                "identifier out of context"
                                                (source-wrap form w s mod)
                                                (wrap value w mod)))
                                             (else
                                              (syntax-violation
                                                #f
                                                "cannot define keyword at top level"
                                                (source-wrap form w s mod)
                                                (wrap value w mod))))))
                                    (else
                                     (values
                                       (cons (if (eq? m 'c&e)
                                               (let ((x (expand-expr type value form e r w s mod)))
                                                 (top-level-eval-hook x mod)
                                                 x)
                                               (lambda () (expand-expr type value form e r w s mod)))
                                             exps))))))))
                      (lambda (exps) (scan (cdr body) r w s m esew mod exps)))))))
         (call-with-values
           (lambda () (scan body r w s m esew mod '()))
           (lambda (exps)
             (if (null? exps)
               (build-void s)
               (build-sequence
                 s
                 (let lp ((in exps) (out '()))
                   (if (null? in)
                     out
                     (let ((e (car in)))
                       (lp (cdr in) (cons (if (procedure? e) (e) e) out))))))))))))
   (expand-install-global
     (lambda (name e)
       (build-global-definition
         #f
         name
         (build-application
           #f
           (build-primref #f 'make-syntax-transformer)
           (list (build-data #f name) (build-data #f 'macro) e)))))
   (parse-when-list
     (lambda (e when-list)
       (let ((result (strip when-list '(()))))
         (let lp ((l result))
           (cond ((null? l) result)
                 ((memq (car l) '(compile load eval expand)) (lp (cdr l)))
                 (else (syntax-violation 'eval-when "invalid situation" e (car l))))))))
   (syntax-type
     (lambda (e r w s rib mod for-car?)
       (cond ((symbol? e)
              (let* ((n (id-var-name e w))
                     (b (lookup n r mod))
                     (type (car b))
                     (key type))
                (cond ((memv key '(lexical)) (values type (cdr b) e e w s mod))
                      ((memv key '(global)) (values type n e e w s mod))
                      ((memv key '(macro))
                       (if for-car?
                         (values type (cdr b) e e w s mod)
                         (syntax-type
                           (expand-macro (cdr b) e r w s rib mod)
                           r
                           '(())
                           s
                           rib
                           mod
                           #f)))
                      (else (values type (cdr b) e e w s mod)))))
             ((pair? e)
              (let ((first (car e)))
                (call-with-values
                  (lambda () (syntax-type first r w s rib mod #t))
                  (lambda (ftype fval fform fe fw fs fmod)
                    (let ((key ftype))
                      (cond ((memv key '(lexical)) (values 'lexical-call fval e e w s mod))
                            ((memv key '(global))
                             (values 'global-call (make-syntax-object fval w fmod) e e w s mod))
                            ((memv key '(macro))
                             (syntax-type
                               (expand-macro fval e r w s rib mod)
                               r
                               '(())
                               s
                               rib
                               mod
                               for-car?))
                            ((memv key '(module-ref))
                             (call-with-values
                               (lambda () (fval e r w))
                               (lambda (e r w s mod) (syntax-type e r w s rib mod for-car?))))
                            ((memv key '(core)) (values 'core-form fval e e w s mod))
                            ((memv key '(local-syntax))
                             (values 'local-syntax-form fval e e w s mod))
                            ((memv key '(begin)) (values 'begin-form #f e e w s mod))
                            ((memv key '(eval-when)) (values 'eval-when-form #f e e w s mod))
                            ((memv key '(define))
                             (let* ((tmp e) (tmp-1 ($sc-dispatch tmp '(_ any any))))
                               (if (and tmp-1 (apply (lambda (name val) (id? name)) tmp-1))
                                 (apply (lambda (name val) (values 'define-form name e val w s mod))
                                        tmp-1)
                                 (let ((tmp-1 ($sc-dispatch tmp '(_ (any . any) any . each-any))))
                                   (if (and tmp-1
                                            (apply (lambda (name args e1 e2)
                                                     (and (id? name) (valid-bound-ids? (lambda-var-list args))))
                                                   tmp-1))
                                     (apply (lambda (name args e1 e2)
                                              (values
                                                'define-form
                                                (wrap name w mod)
                                                (wrap e w mod)
                                                (decorate-source
                                                  (cons '#(syntax-object
                                                           lambda
                                                           ((top)
                                                            #(ribcage
                                                              #(name args e1 e2)
                                                              #((top) (top) (top) (top))
                                                              #("l-*-1902" "l-*-1903" "l-*-1904" "l-*-1905"))
                                                            #(ribcage () () ())
                                                            #(ribcage #(key) #((m-*-1867 top)) #("l-*-1868"))
                                                            #(ribcage () () ())
                                                            #(ribcage () () ())
                                                            #(ribcage
                                                              #(ftype fval fform fe fw fs fmod)
                                                              #((top) (top) (top) (top) (top) (top) (top))
                                                              #("l-*-1860"
                                                                "l-*-1861"
                                                                "l-*-1862"
                                                                "l-*-1863"
                                                                "l-*-1864"
                                                                "l-*-1865"
                                                                "l-*-1866"))
                                                            #(ribcage () () ())
                                                            #(ribcage #(first) #((top)) #("l-*-1851"))
                                                            #(ribcage () () ())
                                                            #(ribcage () () ())
                                                            #(ribcage
                                                              #(e r w s rib mod for-car?)
                                                              #((top) (top) (top) (top) (top) (top) (top))
                                                              #("l-*-1827"
                                                                "l-*-1828"
                                                                "l-*-1829"
                                                                "l-*-1830"
                                                                "l-*-1831"
                                                                "l-*-1832"
                                                                "l-*-1833"))
                                                            #(ribcage
                                                              (lambda-var-list
                                                                gen-var
                                                                strip
                                                                expand-lambda-case
                                                                lambda*-formals
                                                                expand-simple-lambda
                                                                lambda-formals
                                                                ellipsis?
                                                                expand-void
                                                                eval-local-transformer
                                                                expand-local-syntax
                                                                expand-body
                                                                expand-macro
                                                                expand-application
                                                                expand-expr
                                                                expand
                                                                syntax-type
                                                                parse-when-list
                                                                expand-install-global
                                                                expand-top-sequence
                                                                expand-sequence
                                                                source-wrap
                                                                wrap
                                                                bound-id-member?
                                                                distinct-bound-ids?
                                                                valid-bound-ids?
                                                                bound-id=?
                                                                free-id=?
                                                                with-transformer-environment
                                                                transformer-environment
                                                                resolve-identifier
                                                                locally-bound-identifiers
                                                                id-var-name
                                                                same-marks?
                                                                join-marks
                                                                join-wraps
                                                                smart-append
                                                                make-binding-wrap
                                                                extend-ribcage!
                                                                make-empty-ribcage
                                                                new-mark
                                                                anti-mark
                                                                the-anti-mark
                                                                top-marked?
                                                                top-wrap
                                                                empty-wrap
                                                                set-ribcage-labels!
                                                                set-ribcage-marks!
                                                                set-ribcage-symnames!
                                                                ribcage-labels
                                                                ribcage-marks
                                                                ribcage-symnames
                                                                ribcage?
                                                                make-ribcage
                                                                gen-labels
                                                                gen-label
                                                                make-rename
                                                                rename-marks
                                                                rename-new
                                                                rename-old
                                                                subst-rename?
                                                                wrap-subst
                                                                wrap-marks
                                                                make-wrap
                                                                id-sym-name&marks
                                                                id-sym-name
                                                                id?
                                                                nonsymbol-id?
                                                                global-extend
                                                                lookup
                                                                macros-only-env
                                                                extend-var-env
                                                                extend-env
                                                                null-env
                                                                binding-value
                                                                binding-type
                                                                make-binding
                                                                arg-check
                                                                source-annotation
                                                                no-source
                                                                set-syntax-object-module!
                                                                set-syntax-object-wrap!
                                                                set-syntax-object-expression!
                                                                syntax-object-module
                                                                syntax-object-wrap
                                                                syntax-object-expression
                                                                syntax-object?
                                                                make-syntax-object
                                                                build-lexical-var
                                                                build-letrec
                                                                build-named-let
                                                                build-let
                                                                build-sequence
                                                                build-data
                                                                build-primref
                                                                build-lambda-case
                                                                build-case-lambda
                                                                build-simple-lambda
                                                                build-global-definition
                                                                build-global-assignment
                                                                build-global-reference
                                                                analyze-variable
                                                                build-lexical-assignment
                                                                build-lexical-reference
                                                                build-dynlet
                                                                build-conditional
                                                                build-application
                                                                build-void
                                                                maybe-name-value!
                                                                decorate-source
                                                                get-global-definition-hook
                                                                put-global-definition-hook
                                                                session-id
                                                                local-eval-hook
                                                                top-level-eval-hook
                                                                fx<
                                                                fx=
                                                                fx-
                                                                fx+
                                                                set-lambda-meta!
                                                                lambda-meta
                                                                lambda?
                                                                make-dynlet
                                                                make-letrec
                                                                make-let
                                                                make-lambda-case
                                                                make-lambda
                                                                make-sequence
                                                                make-application
                                                                make-conditional
                                                                make-toplevel-define
                                                                make-toplevel-set
                                                                make-toplevel-ref
                                                                make-module-set
                                                                make-module-ref
                                                                make-lexical-set
                                                                make-lexical-ref
                                                                make-primitive-ref
                                                                make-const
                                                                make-void)
                                                              ((top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top))
                                                              ("l-*-476"
                                                               "l-*-474"
                                                               "l-*-472"
                                                               "l-*-470"
                                                               "l-*-468"
                                                               "l-*-466"
                                                               "l-*-464"
                                                               "l-*-462"
                                                               "l-*-460"
                                                               "l-*-458"
                                                               "l-*-456"
                                                               "l-*-454"
                                                               "l-*-452"
                                                               "l-*-450"
                                                               "l-*-448"
                                                               "l-*-446"
                                                               "l-*-444"
                                                               "l-*-442"
                                                               "l-*-440"
                                                               "l-*-438"
                                                               "l-*-436"
                                                               "l-*-434"
                                                               "l-*-432"
                                                               "l-*-430"
                                                               "l-*-428"
                                                               "l-*-426"
                                                               "l-*-424"
                                                               "l-*-422"
                                                               "l-*-420"
                                                               "l-*-418"
                                                               "l-*-416"
                                                               "l-*-414"
                                                               "l-*-412"
                                                               "l-*-410"
                                                               "l-*-408"
                                                               "l-*-406"
                                                               "l-*-404"
                                                               "l-*-402"
                                                               "l-*-400"
                                                               "l-*-399"
                                                               "l-*-397"
                                                               "l-*-394"
                                                               "l-*-393"
                                                               "l-*-392"
                                                               "l-*-390"
                                                               "l-*-389"
                                                               "l-*-387"
                                                               "l-*-385"
                                                               "l-*-383"
                                                               "l-*-381"
                                                               "l-*-379"
                                                               "l-*-377"
                                                               "l-*-375"
                                                               "l-*-373"
                                                               "l-*-370"
                                                               "l-*-368"
                                                               "l-*-367"
                                                               "l-*-365"
                                                               "l-*-363"
                                                               "l-*-361"
                                                               "l-*-359"
                                                               "l-*-358"
                                                               "l-*-357"
                                                               "l-*-356"
                                                               "l-*-354"
                                                               "l-*-353"
                                                               "l-*-350"
                                                               "l-*-348"
                                                               "l-*-346"
                                                               "l-*-344"
                                                               "l-*-342"
                                                               "l-*-340"
                                                               "l-*-338"
                                                               "l-*-337"
                                                               "l-*-336"
                                                               "l-*-334"
                                                               "l-*-332"
                                                               "l-*-331"
                                                               "l-*-328"
                                                               "l-*-327"
                                                               "l-*-325"
                                                               "l-*-323"
                                                               "l-*-321"
                                                               "l-*-319"
                                                               "l-*-317"
                                                               "l-*-315"
                                                               "l-*-313"
                                                               "l-*-311"
                                                               "l-*-309"
                                                               "l-*-306"
                                                               "l-*-304"
                                                               "l-*-302"
                                                               "l-*-300"
                                                               "l-*-298"
                                                               "l-*-296"
                                                               "l-*-294"
                                                               "l-*-292"
                                                               "l-*-290"
                                                               "l-*-288"
                                                               "l-*-286"
                                                               "l-*-284"
                                                               "l-*-282"
                                                               "l-*-280"
                                                               "l-*-278"
                                                               "l-*-276"
                                                               "l-*-274"
                                                               "l-*-272"
                                                               "l-*-270"
                                                               "l-*-268"
                                                               "l-*-266"
                                                               "l-*-264"
                                                               "l-*-262"
                                                               "l-*-260"
                                                               "l-*-258"
                                                               "l-*-256"
                                                               "l-*-255"
                                                               "l-*-254"
                                                               "l-*-253"
                                                               "l-*-252"
                                                               "l-*-250"
                                                               "l-*-248"
                                                               "l-*-246"
                                                               "l-*-243"
                                                               "l-*-241"
                                                               "l-*-239"
                                                               "l-*-237"
                                                               "l-*-235"
                                                               "l-*-233"
                                                               "l-*-231"
                                                               "l-*-229"
                                                               "l-*-227"
                                                               "l-*-225"
                                                               "l-*-223"
                                                               "l-*-221"
                                                               "l-*-219"
                                                               "l-*-217"
                                                               "l-*-215"
                                                               "l-*-213"
                                                               "l-*-211"
                                                               "l-*-209"))
                                                            #(ribcage
                                                              (define-structure
                                                                define-expansion-accessors
                                                                define-expansion-constructors)
                                                              ((top) (top) (top))
                                                              ("l-*-47" "l-*-46" "l-*-45")))
                                                           (hygiene guile))
                                                        (wrap (cons args (cons e1 e2)) w mod))
                                                  s)
                                                '(())
                                                s
                                                mod))
                                            tmp-1)
                                     (let ((tmp-1 ($sc-dispatch tmp '(_ any))))
                                       (if (and tmp-1 (apply (lambda (name) (id? name)) tmp-1))
                                         (apply (lambda (name)
                                                  (values
                                                    'define-form
                                                    (wrap name w mod)
                                                    (wrap e w mod)
                                                    '(#(syntax-object
                                                        if
                                                        ((top)
                                                         #(ribcage #(name) #((top)) #("l-*-1915"))
                                                         #(ribcage () () ())
                                                         #(ribcage #(key) #((m-*-1867 top)) #("l-*-1868"))
                                                         #(ribcage () () ())
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(ftype fval fform fe fw fs fmod)
                                                           #((top) (top) (top) (top) (top) (top) (top))
                                                           #("l-*-1860"
                                                             "l-*-1861"
                                                             "l-*-1862"
                                                             "l-*-1863"
                                                             "l-*-1864"
                                                             "l-*-1865"
                                                             "l-*-1866"))
                                                         #(ribcage () () ())
                                                         #(ribcage #(first) #((top)) #("l-*-1851"))
                                                         #(ribcage () () ())
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(e r w s rib mod for-car?)
                                                           #((top) (top) (top) (top) (top) (top) (top))
                                                           #("l-*-1827"
                                                             "l-*-1828"
                                                             "l-*-1829"
                                                             "l-*-1830"
                                                             "l-*-1831"
                                                             "l-*-1832"
                                                             "l-*-1833"))
                                                         #(ribcage
                                                           (lambda-var-list
                                                             gen-var
                                                             strip
                                                             expand-lambda-case
                                                             lambda*-formals
                                                             expand-simple-lambda
                                                             lambda-formals
                                                             ellipsis?
                                                             expand-void
                                                             eval-local-transformer
                                                             expand-local-syntax
                                                             expand-body
                                                             expand-macro
                                                             expand-application
                                                             expand-expr
                                                             expand
                                                             syntax-type
                                                             parse-when-list
                                                             expand-install-global
                                                             expand-top-sequence
                                                             expand-sequence
                                                             source-wrap
                                                             wrap
                                                             bound-id-member?
                                                             distinct-bound-ids?
                                                             valid-bound-ids?
                                                             bound-id=?
                                                             free-id=?
                                                             with-transformer-environment
                                                             transformer-environment
                                                             resolve-identifier
                                                             locally-bound-identifiers
                                                             id-var-name
                                                             same-marks?
                                                             join-marks
                                                             join-wraps
                                                             smart-append
                                                             make-binding-wrap
                                                             extend-ribcage!
                                                             make-empty-ribcage
                                                             new-mark
                                                             anti-mark
                                                             the-anti-mark
                                                             top-marked?
                                                             top-wrap
                                                             empty-wrap
                                                             set-ribcage-labels!
                                                             set-ribcage-marks!
                                                             set-ribcage-symnames!
                                                             ribcage-labels
                                                             ribcage-marks
                                                             ribcage-symnames
                                                             ribcage?
                                                             make-ribcage
                                                             gen-labels
                                                             gen-label
                                                             make-rename
                                                             rename-marks
                                                             rename-new
                                                             rename-old
                                                             subst-rename?
                                                             wrap-subst
                                                             wrap-marks
                                                             make-wrap
                                                             id-sym-name&marks
                                                             id-sym-name
                                                             id?
                                                             nonsymbol-id?
                                                             global-extend
                                                             lookup
                                                             macros-only-env
                                                             extend-var-env
                                                             extend-env
                                                             null-env
                                                             binding-value
                                                             binding-type
                                                             make-binding
                                                             arg-check
                                                             source-annotation
                                                             no-source
                                                             set-syntax-object-module!
                                                             set-syntax-object-wrap!
                                                             set-syntax-object-expression!
                                                             syntax-object-module
                                                             syntax-object-wrap
                                                             syntax-object-expression
                                                             syntax-object?
                                                             make-syntax-object
                                                             build-lexical-var
                                                             build-letrec
                                                             build-named-let
                                                             build-let
                                                             build-sequence
                                                             build-data
                                                             build-primref
                                                             build-lambda-case
                                                             build-case-lambda
                                                             build-simple-lambda
                                                             build-global-definition
                                                             build-global-assignment
                                                             build-global-reference
                                                             analyze-variable
                                                             build-lexical-assignment
                                                             build-lexical-reference
                                                             build-dynlet
                                                             build-conditional
                                                             build-application
                                                             build-void
                                                             maybe-name-value!
                                                             decorate-source
                                                             get-global-definition-hook
                                                             put-global-definition-hook
                                                             session-id
                                                             local-eval-hook
                                                             top-level-eval-hook
                                                             fx<
                                                             fx=
                                                             fx-
                                                             fx+
                                                             set-lambda-meta!
                                                             lambda-meta
                                                             lambda?
                                                             make-dynlet
                                                             make-letrec
                                                             make-let
                                                             make-lambda-case
                                                             make-lambda
                                                             make-sequence
                                                             make-application
                                                             make-conditional
                                                             make-toplevel-define
                                                             make-toplevel-set
                                                             make-toplevel-ref
                                                             make-module-set
                                                             make-module-ref
                                                             make-lexical-set
                                                             make-lexical-ref
                                                             make-primitive-ref
                                                             make-const
                                                             make-void)
                                                           ((top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top))
                                                           ("l-*-476"
                                                            "l-*-474"
                                                            "l-*-472"
                                                            "l-*-470"
                                                            "l-*-468"
                                                            "l-*-466"
                                                            "l-*-464"
                                                            "l-*-462"
                                                            "l-*-460"
                                                            "l-*-458"
                                                            "l-*-456"
                                                            "l-*-454"
                                                            "l-*-452"
                                                            "l-*-450"
                                                            "l-*-448"
                                                            "l-*-446"
                                                            "l-*-444"
                                                            "l-*-442"
                                                            "l-*-440"
                                                            "l-*-438"
                                                            "l-*-436"
                                                            "l-*-434"
                                                            "l-*-432"
                                                            "l-*-430"
                                                            "l-*-428"
                                                            "l-*-426"
                                                            "l-*-424"
                                                            "l-*-422"
                                                            "l-*-420"
                                                            "l-*-418"
                                                            "l-*-416"
                                                            "l-*-414"
                                                            "l-*-412"
                                                            "l-*-410"
                                                            "l-*-408"
                                                            "l-*-406"
                                                            "l-*-404"
                                                            "l-*-402"
                                                            "l-*-400"
                                                            "l-*-399"
                                                            "l-*-397"
                                                            "l-*-394"
                                                            "l-*-393"
                                                            "l-*-392"
                                                            "l-*-390"
                                                            "l-*-389"
                                                            "l-*-387"
                                                            "l-*-385"
                                                            "l-*-383"
                                                            "l-*-381"
                                                            "l-*-379"
                                                            "l-*-377"
                                                            "l-*-375"
                                                            "l-*-373"
                                                            "l-*-370"
                                                            "l-*-368"
                                                            "l-*-367"
                                                            "l-*-365"
                                                            "l-*-363"
                                                            "l-*-361"
                                                            "l-*-359"
                                                            "l-*-358"
                                                            "l-*-357"
                                                            "l-*-356"
                                                            "l-*-354"
                                                            "l-*-353"
                                                            "l-*-350"
                                                            "l-*-348"
                                                            "l-*-346"
                                                            "l-*-344"
                                                            "l-*-342"
                                                            "l-*-340"
                                                            "l-*-338"
                                                            "l-*-337"
                                                            "l-*-336"
                                                            "l-*-334"
                                                            "l-*-332"
                                                            "l-*-331"
                                                            "l-*-328"
                                                            "l-*-327"
                                                            "l-*-325"
                                                            "l-*-323"
                                                            "l-*-321"
                                                            "l-*-319"
                                                            "l-*-317"
                                                            "l-*-315"
                                                            "l-*-313"
                                                            "l-*-311"
                                                            "l-*-309"
                                                            "l-*-306"
                                                            "l-*-304"
                                                            "l-*-302"
                                                            "l-*-300"
                                                            "l-*-298"
                                                            "l-*-296"
                                                            "l-*-294"
                                                            "l-*-292"
                                                            "l-*-290"
                                                            "l-*-288"
                                                            "l-*-286"
                                                            "l-*-284"
                                                            "l-*-282"
                                                            "l-*-280"
                                                            "l-*-278"
                                                            "l-*-276"
                                                            "l-*-274"
                                                            "l-*-272"
                                                            "l-*-270"
                                                            "l-*-268"
                                                            "l-*-266"
                                                            "l-*-264"
                                                            "l-*-262"
                                                            "l-*-260"
                                                            "l-*-258"
                                                            "l-*-256"
                                                            "l-*-255"
                                                            "l-*-254"
                                                            "l-*-253"
                                                            "l-*-252"
                                                            "l-*-250"
                                                            "l-*-248"
                                                            "l-*-246"
                                                            "l-*-243"
                                                            "l-*-241"
                                                            "l-*-239"
                                                            "l-*-237"
                                                            "l-*-235"
                                                            "l-*-233"
                                                            "l-*-231"
                                                            "l-*-229"
                                                            "l-*-227"
                                                            "l-*-225"
                                                            "l-*-223"
                                                            "l-*-221"
                                                            "l-*-219"
                                                            "l-*-217"
                                                            "l-*-215"
                                                            "l-*-213"
                                                            "l-*-211"
                                                            "l-*-209"))
                                                         #(ribcage
                                                           (define-structure
                                                             define-expansion-accessors
                                                             define-expansion-constructors)
                                                           ((top) (top) (top))
                                                           ("l-*-47" "l-*-46" "l-*-45")))
                                                        (hygiene guile))
                                                      #(syntax-object
                                                        #f
                                                        ((top)
                                                         #(ribcage #(name) #((top)) #("l-*-1915"))
                                                         #(ribcage () () ())
                                                         #(ribcage #(key) #((m-*-1867 top)) #("l-*-1868"))
                                                         #(ribcage () () ())
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(ftype fval fform fe fw fs fmod)
                                                           #((top) (top) (top) (top) (top) (top) (top))
                                                           #("l-*-1860"
                                                             "l-*-1861"
                                                             "l-*-1862"
                                                             "l-*-1863"
                                                             "l-*-1864"
                                                             "l-*-1865"
                                                             "l-*-1866"))
                                                         #(ribcage () () ())
                                                         #(ribcage #(first) #((top)) #("l-*-1851"))
                                                         #(ribcage () () ())
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(e r w s rib mod for-car?)
                                                           #((top) (top) (top) (top) (top) (top) (top))
                                                           #("l-*-1827"
                                                             "l-*-1828"
                                                             "l-*-1829"
                                                             "l-*-1830"
                                                             "l-*-1831"
                                                             "l-*-1832"
                                                             "l-*-1833"))
                                                         #(ribcage
                                                           (lambda-var-list
                                                             gen-var
                                                             strip
                                                             expand-lambda-case
                                                             lambda*-formals
                                                             expand-simple-lambda
                                                             lambda-formals
                                                             ellipsis?
                                                             expand-void
                                                             eval-local-transformer
                                                             expand-local-syntax
                                                             expand-body
                                                             expand-macro
                                                             expand-application
                                                             expand-expr
                                                             expand
                                                             syntax-type
                                                             parse-when-list
                                                             expand-install-global
                                                             expand-top-sequence
                                                             expand-sequence
                                                             source-wrap
                                                             wrap
                                                             bound-id-member?
                                                             distinct-bound-ids?
                                                             valid-bound-ids?
                                                             bound-id=?
                                                             free-id=?
                                                             with-transformer-environment
                                                             transformer-environment
                                                             resolve-identifier
                                                             locally-bound-identifiers
                                                             id-var-name
                                                             same-marks?
                                                             join-marks
                                                             join-wraps
                                                             smart-append
                                                             make-binding-wrap
                                                             extend-ribcage!
                                                             make-empty-ribcage
                                                             new-mark
                                                             anti-mark
                                                             the-anti-mark
                                                             top-marked?
                                                             top-wrap
                                                             empty-wrap
                                                             set-ribcage-labels!
                                                             set-ribcage-marks!
                                                             set-ribcage-symnames!
                                                             ribcage-labels
                                                             ribcage-marks
                                                             ribcage-symnames
                                                             ribcage?
                                                             make-ribcage
                                                             gen-labels
                                                             gen-label
                                                             make-rename
                                                             rename-marks
                                                             rename-new
                                                             rename-old
                                                             subst-rename?
                                                             wrap-subst
                                                             wrap-marks
                                                             make-wrap
                                                             id-sym-name&marks
                                                             id-sym-name
                                                             id?
                                                             nonsymbol-id?
                                                             global-extend
                                                             lookup
                                                             macros-only-env
                                                             extend-var-env
                                                             extend-env
                                                             null-env
                                                             binding-value
                                                             binding-type
                                                             make-binding
                                                             arg-check
                                                             source-annotation
                                                             no-source
                                                             set-syntax-object-module!
                                                             set-syntax-object-wrap!
                                                             set-syntax-object-expression!
                                                             syntax-object-module
                                                             syntax-object-wrap
                                                             syntax-object-expression
                                                             syntax-object?
                                                             make-syntax-object
                                                             build-lexical-var
                                                             build-letrec
                                                             build-named-let
                                                             build-let
                                                             build-sequence
                                                             build-data
                                                             build-primref
                                                             build-lambda-case
                                                             build-case-lambda
                                                             build-simple-lambda
                                                             build-global-definition
                                                             build-global-assignment
                                                             build-global-reference
                                                             analyze-variable
                                                             build-lexical-assignment
                                                             build-lexical-reference
                                                             build-dynlet
                                                             build-conditional
                                                             build-application
                                                             build-void
                                                             maybe-name-value!
                                                             decorate-source
                                                             get-global-definition-hook
                                                             put-global-definition-hook
                                                             session-id
                                                             local-eval-hook
                                                             top-level-eval-hook
                                                             fx<
                                                             fx=
                                                             fx-
                                                             fx+
                                                             set-lambda-meta!
                                                             lambda-meta
                                                             lambda?
                                                             make-dynlet
                                                             make-letrec
                                                             make-let
                                                             make-lambda-case
                                                             make-lambda
                                                             make-sequence
                                                             make-application
                                                             make-conditional
                                                             make-toplevel-define
                                                             make-toplevel-set
                                                             make-toplevel-ref
                                                             make-module-set
                                                             make-module-ref
                                                             make-lexical-set
                                                             make-lexical-ref
                                                             make-primitive-ref
                                                             make-const
                                                             make-void)
                                                           ((top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top))
                                                           ("l-*-476"
                                                            "l-*-474"
                                                            "l-*-472"
                                                            "l-*-470"
                                                            "l-*-468"
                                                            "l-*-466"
                                                            "l-*-464"
                                                            "l-*-462"
                                                            "l-*-460"
                                                            "l-*-458"
                                                            "l-*-456"
                                                            "l-*-454"
                                                            "l-*-452"
                                                            "l-*-450"
                                                            "l-*-448"
                                                            "l-*-446"
                                                            "l-*-444"
                                                            "l-*-442"
                                                            "l-*-440"
                                                            "l-*-438"
                                                            "l-*-436"
                                                            "l-*-434"
                                                            "l-*-432"
                                                            "l-*-430"
                                                            "l-*-428"
                                                            "l-*-426"
                                                            "l-*-424"
                                                            "l-*-422"
                                                            "l-*-420"
                                                            "l-*-418"
                                                            "l-*-416"
                                                            "l-*-414"
                                                            "l-*-412"
                                                            "l-*-410"
                                                            "l-*-408"
                                                            "l-*-406"
                                                            "l-*-404"
                                                            "l-*-402"
                                                            "l-*-400"
                                                            "l-*-399"
                                                            "l-*-397"
                                                            "l-*-394"
                                                            "l-*-393"
                                                            "l-*-392"
                                                            "l-*-390"
                                                            "l-*-389"
                                                            "l-*-387"
                                                            "l-*-385"
                                                            "l-*-383"
                                                            "l-*-381"
                                                            "l-*-379"
                                                            "l-*-377"
                                                            "l-*-375"
                                                            "l-*-373"
                                                            "l-*-370"
                                                            "l-*-368"
                                                            "l-*-367"
                                                            "l-*-365"
                                                            "l-*-363"
                                                            "l-*-361"
                                                            "l-*-359"
                                                            "l-*-358"
                                                            "l-*-357"
                                                            "l-*-356"
                                                            "l-*-354"
                                                            "l-*-353"
                                                            "l-*-350"
                                                            "l-*-348"
                                                            "l-*-346"
                                                            "l-*-344"
                                                            "l-*-342"
                                                            "l-*-340"
                                                            "l-*-338"
                                                            "l-*-337"
                                                            "l-*-336"
                                                            "l-*-334"
                                                            "l-*-332"
                                                            "l-*-331"
                                                            "l-*-328"
                                                            "l-*-327"
                                                            "l-*-325"
                                                            "l-*-323"
                                                            "l-*-321"
                                                            "l-*-319"
                                                            "l-*-317"
                                                            "l-*-315"
                                                            "l-*-313"
                                                            "l-*-311"
                                                            "l-*-309"
                                                            "l-*-306"
                                                            "l-*-304"
                                                            "l-*-302"
                                                            "l-*-300"
                                                            "l-*-298"
                                                            "l-*-296"
                                                            "l-*-294"
                                                            "l-*-292"
                                                            "l-*-290"
                                                            "l-*-288"
                                                            "l-*-286"
                                                            "l-*-284"
                                                            "l-*-282"
                                                            "l-*-280"
                                                            "l-*-278"
                                                            "l-*-276"
                                                            "l-*-274"
                                                            "l-*-272"
                                                            "l-*-270"
                                                            "l-*-268"
                                                            "l-*-266"
                                                            "l-*-264"
                                                            "l-*-262"
                                                            "l-*-260"
                                                            "l-*-258"
                                                            "l-*-256"
                                                            "l-*-255"
                                                            "l-*-254"
                                                            "l-*-253"
                                                            "l-*-252"
                                                            "l-*-250"
                                                            "l-*-248"
                                                            "l-*-246"
                                                            "l-*-243"
                                                            "l-*-241"
                                                            "l-*-239"
                                                            "l-*-237"
                                                            "l-*-235"
                                                            "l-*-233"
                                                            "l-*-231"
                                                            "l-*-229"
                                                            "l-*-227"
                                                            "l-*-225"
                                                            "l-*-223"
                                                            "l-*-221"
                                                            "l-*-219"
                                                            "l-*-217"
                                                            "l-*-215"
                                                            "l-*-213"
                                                            "l-*-211"
                                                            "l-*-209"))
                                                         #(ribcage
                                                           (define-structure
                                                             define-expansion-accessors
                                                             define-expansion-constructors)
                                                           ((top) (top) (top))
                                                           ("l-*-47" "l-*-46" "l-*-45")))
                                                        (hygiene guile))
                                                      #(syntax-object
                                                        #f
                                                        ((top)
                                                         #(ribcage #(name) #((top)) #("l-*-1915"))
                                                         #(ribcage () () ())
                                                         #(ribcage #(key) #((m-*-1867 top)) #("l-*-1868"))
                                                         #(ribcage () () ())
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(ftype fval fform fe fw fs fmod)
                                                           #((top) (top) (top) (top) (top) (top) (top))
                                                           #("l-*-1860"
                                                             "l-*-1861"
                                                             "l-*-1862"
                                                             "l-*-1863"
                                                             "l-*-1864"
                                                             "l-*-1865"
                                                             "l-*-1866"))
                                                         #(ribcage () () ())
                                                         #(ribcage #(first) #((top)) #("l-*-1851"))
                                                         #(ribcage () () ())
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(e r w s rib mod for-car?)
                                                           #((top) (top) (top) (top) (top) (top) (top))
                                                           #("l-*-1827"
                                                             "l-*-1828"
                                                             "l-*-1829"
                                                             "l-*-1830"
                                                             "l-*-1831"
                                                             "l-*-1832"
                                                             "l-*-1833"))
                                                         #(ribcage
                                                           (lambda-var-list
                                                             gen-var
                                                             strip
                                                             expand-lambda-case
                                                             lambda*-formals
                                                             expand-simple-lambda
                                                             lambda-formals
                                                             ellipsis?
                                                             expand-void
                                                             eval-local-transformer
                                                             expand-local-syntax
                                                             expand-body
                                                             expand-macro
                                                             expand-application
                                                             expand-expr
                                                             expand
                                                             syntax-type
                                                             parse-when-list
                                                             expand-install-global
                                                             expand-top-sequence
                                                             expand-sequence
                                                             source-wrap
                                                             wrap
                                                             bound-id-member?
                                                             distinct-bound-ids?
                                                             valid-bound-ids?
                                                             bound-id=?
                                                             free-id=?
                                                             with-transformer-environment
                                                             transformer-environment
                                                             resolve-identifier
                                                             locally-bound-identifiers
                                                             id-var-name
                                                             same-marks?
                                                             join-marks
                                                             join-wraps
                                                             smart-append
                                                             make-binding-wrap
                                                             extend-ribcage!
                                                             make-empty-ribcage
                                                             new-mark
                                                             anti-mark
                                                             the-anti-mark
                                                             top-marked?
                                                             top-wrap
                                                             empty-wrap
                                                             set-ribcage-labels!
                                                             set-ribcage-marks!
                                                             set-ribcage-symnames!
                                                             ribcage-labels
                                                             ribcage-marks
                                                             ribcage-symnames
                                                             ribcage?
                                                             make-ribcage
                                                             gen-labels
                                                             gen-label
                                                             make-rename
                                                             rename-marks
                                                             rename-new
                                                             rename-old
                                                             subst-rename?
                                                             wrap-subst
                                                             wrap-marks
                                                             make-wrap
                                                             id-sym-name&marks
                                                             id-sym-name
                                                             id?
                                                             nonsymbol-id?
                                                             global-extend
                                                             lookup
                                                             macros-only-env
                                                             extend-var-env
                                                             extend-env
                                                             null-env
                                                             binding-value
                                                             binding-type
                                                             make-binding
                                                             arg-check
                                                             source-annotation
                                                             no-source
                                                             set-syntax-object-module!
                                                             set-syntax-object-wrap!
                                                             set-syntax-object-expression!
                                                             syntax-object-module
                                                             syntax-object-wrap
                                                             syntax-object-expression
                                                             syntax-object?
                                                             make-syntax-object
                                                             build-lexical-var
                                                             build-letrec
                                                             build-named-let
                                                             build-let
                                                             build-sequence
                                                             build-data
                                                             build-primref
                                                             build-lambda-case
                                                             build-case-lambda
                                                             build-simple-lambda
                                                             build-global-definition
                                                             build-global-assignment
                                                             build-global-reference
                                                             analyze-variable
                                                             build-lexical-assignment
                                                             build-lexical-reference
                                                             build-dynlet
                                                             build-conditional
                                                             build-application
                                                             build-void
                                                             maybe-name-value!
                                                             decorate-source
                                                             get-global-definition-hook
                                                             put-global-definition-hook
                                                             session-id
                                                             local-eval-hook
                                                             top-level-eval-hook
                                                             fx<
                                                             fx=
                                                             fx-
                                                             fx+
                                                             set-lambda-meta!
                                                             lambda-meta
                                                             lambda?
                                                             make-dynlet
                                                             make-letrec
                                                             make-let
                                                             make-lambda-case
                                                             make-lambda
                                                             make-sequence
                                                             make-application
                                                             make-conditional
                                                             make-toplevel-define
                                                             make-toplevel-set
                                                             make-toplevel-ref
                                                             make-module-set
                                                             make-module-ref
                                                             make-lexical-set
                                                             make-lexical-ref
                                                             make-primitive-ref
                                                             make-const
                                                             make-void)
                                                           ((top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top))
                                                           ("l-*-476"
                                                            "l-*-474"
                                                            "l-*-472"
                                                            "l-*-470"
                                                            "l-*-468"
                                                            "l-*-466"
                                                            "l-*-464"
                                                            "l-*-462"
                                                            "l-*-460"
                                                            "l-*-458"
                                                            "l-*-456"
                                                            "l-*-454"
                                                            "l-*-452"
                                                            "l-*-450"
                                                            "l-*-448"
                                                            "l-*-446"
                                                            "l-*-444"
                                                            "l-*-442"
                                                            "l-*-440"
                                                            "l-*-438"
                                                            "l-*-436"
                                                            "l-*-434"
                                                            "l-*-432"
                                                            "l-*-430"
                                                            "l-*-428"
                                                            "l-*-426"
                                                            "l-*-424"
                                                            "l-*-422"
                                                            "l-*-420"
                                                            "l-*-418"
                                                            "l-*-416"
                                                            "l-*-414"
                                                            "l-*-412"
                                                            "l-*-410"
                                                            "l-*-408"
                                                            "l-*-406"
                                                            "l-*-404"
                                                            "l-*-402"
                                                            "l-*-400"
                                                            "l-*-399"
                                                            "l-*-397"
                                                            "l-*-394"
                                                            "l-*-393"
                                                            "l-*-392"
                                                            "l-*-390"
                                                            "l-*-389"
                                                            "l-*-387"
                                                            "l-*-385"
                                                            "l-*-383"
                                                            "l-*-381"
                                                            "l-*-379"
                                                            "l-*-377"
                                                            "l-*-375"
                                                            "l-*-373"
                                                            "l-*-370"
                                                            "l-*-368"
                                                            "l-*-367"
                                                            "l-*-365"
                                                            "l-*-363"
                                                            "l-*-361"
                                                            "l-*-359"
                                                            "l-*-358"
                                                            "l-*-357"
                                                            "l-*-356"
                                                            "l-*-354"
                                                            "l-*-353"
                                                            "l-*-350"
                                                            "l-*-348"
                                                            "l-*-346"
                                                            "l-*-344"
                                                            "l-*-342"
                                                            "l-*-340"
                                                            "l-*-338"
                                                            "l-*-337"
                                                            "l-*-336"
                                                            "l-*-334"
                                                            "l-*-332"
                                                            "l-*-331"
                                                            "l-*-328"
                                                            "l-*-327"
                                                            "l-*-325"
                                                            "l-*-323"
                                                            "l-*-321"
                                                            "l-*-319"
                                                            "l-*-317"
                                                            "l-*-315"
                                                            "l-*-313"
                                                            "l-*-311"
                                                            "l-*-309"
                                                            "l-*-306"
                                                            "l-*-304"
                                                            "l-*-302"
                                                            "l-*-300"
                                                            "l-*-298"
                                                            "l-*-296"
                                                            "l-*-294"
                                                            "l-*-292"
                                                            "l-*-290"
                                                            "l-*-288"
                                                            "l-*-286"
                                                            "l-*-284"
                                                            "l-*-282"
                                                            "l-*-280"
                                                            "l-*-278"
                                                            "l-*-276"
                                                            "l-*-274"
                                                            "l-*-272"
                                                            "l-*-270"
                                                            "l-*-268"
                                                            "l-*-266"
                                                            "l-*-264"
                                                            "l-*-262"
                                                            "l-*-260"
                                                            "l-*-258"
                                                            "l-*-256"
                                                            "l-*-255"
                                                            "l-*-254"
                                                            "l-*-253"
                                                            "l-*-252"
                                                            "l-*-250"
                                                            "l-*-248"
                                                            "l-*-246"
                                                            "l-*-243"
                                                            "l-*-241"
                                                            "l-*-239"
                                                            "l-*-237"
                                                            "l-*-235"
                                                            "l-*-233"
                                                            "l-*-231"
                                                            "l-*-229"
                                                            "l-*-227"
                                                            "l-*-225"
                                                            "l-*-223"
                                                            "l-*-221"
                                                            "l-*-219"
                                                            "l-*-217"
                                                            "l-*-215"
                                                            "l-*-213"
                                                            "l-*-211"
                                                            "l-*-209"))
                                                         #(ribcage
                                                           (define-structure
                                                             define-expansion-accessors
                                                             define-expansion-constructors)
                                                           ((top) (top) (top))
                                                           ("l-*-47" "l-*-46" "l-*-45")))
                                                        (hygiene guile)))
                                                    '(())
                                                    s
                                                    mod))
                                                tmp-1)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           tmp))))))))
                            ((memv key '(define-syntax))
                             (let* ((tmp-1 e) (tmp ($sc-dispatch tmp-1 '(_ any any))))
                               (if (and tmp (apply (lambda (name val) (id? name)) tmp))
                                 (apply (lambda (name val) (values 'define-syntax-form name e val w s mod))
                                        tmp)
                                 (syntax-violation
                                   #f
                                   "source expression failed to match any pattern"
                                   tmp-1))))
                            ((memv key '(define-syntax-parameter))
                             (let* ((tmp-1 e) (tmp ($sc-dispatch tmp-1 '(_ any any))))
                               (if (and tmp (apply (lambda (name val) (id? name)) tmp))
                                 (apply (lambda (name val)
                                          (values 'define-syntax-parameter-form name e val w s mod))
                                        tmp)
                                 (syntax-violation
                                   #f
                                   "source expression failed to match any pattern"
                                   tmp-1))))
                            (else (values 'call #f e e w s mod))))))))
             ((syntax-object? e)
              (syntax-type
                (syntax-object-expression e)
                r
                (join-wraps w (syntax-object-wrap e))
                (or (source-annotation e) s)
                rib
                (or (syntax-object-module e) mod)
                for-car?))
             ((self-evaluating? e) (values 'constant #f e e w s mod))
             (else (values 'other #f e e w s mod)))))
   (expand
     (lambda (e r w mod)
       (call-with-values
         (lambda () (syntax-type e r w (source-annotation e) #f mod #f))
         (lambda (type value form e w s mod)
           (expand-expr type value form e r w s mod)))))
   (expand-expr
     (lambda (type value form e r w s mod)
       (let ((key type))
         (cond ((memv key '(lexical)) (build-lexical-reference 'value s e value))
               ((memv key '(core core-form)) (value e r w s mod))
               ((memv key '(module-ref))
                (call-with-values
                  (lambda () (value e r w))
                  (lambda (e r w s mod) (expand e r w mod))))
               ((memv key '(lexical-call))
                (expand-application
                  (let ((id (car e)))
                    (build-lexical-reference
                      'fun
                      (source-annotation id)
                      (if (syntax-object? id) (syntax->datum id) id)
                      value))
                  e
                  r
                  w
                  s
                  mod))
               ((memv key '(global-call))
                (expand-application
                  (build-global-reference
                    (source-annotation (car e))
                    (if (syntax-object? value) (syntax-object-expression value) value)
                    (if (syntax-object? value) (syntax-object-module value) mod))
                  e
                  r
                  w
                  s
                  mod))
               ((memv key '(constant))
                (build-data s (strip (source-wrap e w s mod) '(()))))
               ((memv key '(global)) (build-global-reference s value mod))
               ((memv key '(call))
                (expand-application (expand (car e) r w mod) e r w s mod))
               ((memv key '(begin-form))
                (let* ((tmp e) (tmp-1 ($sc-dispatch tmp '(_ any . each-any))))
                  (if tmp-1
                    (apply (lambda (e1 e2) (expand-sequence (cons e1 e2) r w s mod))
                           tmp-1)
                    (let ((tmp-1 ($sc-dispatch tmp '(_))))
                      (if tmp-1
                        (apply (lambda ()
                                 (if (include-deprecated-features)
                                   (begin
                                     (issue-deprecation-warning
                                       "Sequences of zero expressions are deprecated.  Use *unspecified*.")
                                     (expand-void))
                                   (syntax-violation
                                     #f
                                     "sequence of zero expressions"
                                     (source-wrap e w s mod))))
                               tmp-1)
                        (syntax-violation
                          #f
                          "source expression failed to match any pattern"
                          tmp))))))
               ((memv key '(local-syntax-form))
                (expand-local-syntax value e r w s mod expand-sequence))
               ((memv key '(eval-when-form))
                (let* ((tmp-1 e) (tmp ($sc-dispatch tmp-1 '(_ each-any any . each-any))))
                  (if tmp
                    (apply (lambda (x e1 e2)
                             (let ((when-list (parse-when-list e x)))
                               (if (memq 'eval when-list)
                                 (expand-sequence (cons e1 e2) r w s mod)
                                 (expand-void))))
                           tmp)
                    (syntax-violation
                      #f
                      "source expression failed to match any pattern"
                      tmp-1))))
               ((memv key
                      '(define-form define-syntax-form define-syntax-parameter-form))
                (syntax-violation
                  #f
                  "definition in expression context, where definitions are not allowed,"
                  (source-wrap form w s mod)))
               ((memv key '(syntax))
                (syntax-violation
                  #f
                  "reference to pattern variable outside syntax form"
                  (source-wrap e w s mod)))
               ((memv key '(displaced-lexical))
                (syntax-violation
                  #f
                  "reference to identifier outside its scope"
                  (source-wrap e w s mod)))
               (else
                (syntax-violation #f "unexpected syntax" (source-wrap e w s mod)))))))
   (expand-application
     (lambda (x e r w s mod)
       (let* ((tmp-1 e) (tmp ($sc-dispatch tmp-1 '(any . each-any))))
         (if tmp
           (apply (lambda (e0 e1)
                    (build-application s x (map (lambda (e) (expand e r w mod)) e1)))
                  tmp)
           (syntax-violation
             #f
             "source expression failed to match any pattern"
             tmp-1)))))
   (expand-macro
     (lambda (p e r w s rib mod)
       (letrec*
         ((rebuild-macro-output
            (lambda (x m)
              (cond ((pair? x)
                     (decorate-source
                       (cons (rebuild-macro-output (car x) m)
                             (rebuild-macro-output (cdr x) m))
                       s))
                    ((syntax-object? x)
                     (let ((w (syntax-object-wrap x)))
                       (let ((ms (car w)) (ss (cdr w)))
                         (if (and (pair? ms) (eq? (car ms) #f))
                           (make-syntax-object
                             (syntax-object-expression x)
                             (cons (cdr ms) (if rib (cons rib (cdr ss)) (cdr ss)))
                             (syntax-object-module x))
                           (make-syntax-object
                             (decorate-source (syntax-object-expression x) s)
                             (cons (cons m ms)
                                   (if rib (cons rib (cons 'shift ss)) (cons 'shift ss)))
                             (syntax-object-module x))))))
                    ((vector? x)
                     (let* ((n (vector-length x)) (v (decorate-source (make-vector n) s)))
                       (let loop ((i 0))
                         (if (= i n)
                           (begin (if #f #f) v)
                           (begin
                             (vector-set! v i (rebuild-macro-output (vector-ref x i) m))
                             (loop (+ i 1)))))))
                    ((symbol? x)
                     (syntax-violation
                       #f
                       "encountered raw symbol in macro output"
                       (source-wrap e w (cdr w) mod)
                       x))
                    (else (decorate-source x s))))))
         (with-fluids
           ((transformer-environment (lambda (k) (k e r w s rib mod))))
           (rebuild-macro-output
             (p (source-wrap e (anti-mark w) s mod))
             (gensym (string-append "m-" (session-id) "-")))))))
   (expand-body
     (lambda (body outer-form r w mod)
       (let* ((r (cons '("placeholder" placeholder) r))
              (ribcage (make-ribcage '() '() '()))
              (w (cons (car w) (cons ribcage (cdr w)))))
         (let parse ((body (map (lambda (x) (cons r (wrap x w mod))) body))
                     (ids '())
                     (labels '())
                     (var-ids '())
                     (vars '())
                     (vals '())
                     (bindings '()))
           (if (null? body)
             (syntax-violation #f "no expressions in body" outer-form)
             (let ((e (cdar body)) (er (caar body)))
               (call-with-values
                 (lambda ()
                   (syntax-type e er '(()) (source-annotation er) ribcage mod #f))
                 (lambda (type value form e w s mod)
                   (let ((key type))
                     (cond ((memv key '(define-form))
                            (let ((id (wrap value w mod)) (label (gen-label)))
                              (let ((var (gen-var id)))
                                (extend-ribcage! ribcage id label)
                                (parse (cdr body)
                                       (cons id ids)
                                       (cons label labels)
                                       (cons id var-ids)
                                       (cons var vars)
                                       (cons (cons er (wrap e w mod)) vals)
                                       (cons (cons 'lexical var) bindings)))))
                           ((memv key '(define-syntax-form define-syntax-parameter-form))
                            (let ((id (wrap value w mod)) (label (gen-label)))
                              (extend-ribcage! ribcage id label)
                              (parse (cdr body)
                                     (cons id ids)
                                     (cons label labels)
                                     var-ids
                                     vars
                                     vals
                                     (cons (cons 'macro (cons er (wrap e w mod))) bindings))))
                           ((memv key '(begin-form))
                            (let* ((tmp-1 e) (tmp ($sc-dispatch tmp-1 '(_ . each-any))))
                              (if tmp
                                (apply (lambda (e1)
                                         (parse (let f ((forms e1))
                                                  (if (null? forms)
                                                    (cdr body)
                                                    (cons (cons er (wrap (car forms) w mod)) (f (cdr forms)))))
                                                ids
                                                labels
                                                var-ids
                                                vars
                                                vals
                                                bindings))
                                       tmp)
                                (syntax-violation
                                  #f
                                  "source expression failed to match any pattern"
                                  tmp-1))))
                           ((memv key '(local-syntax-form))
                            (expand-local-syntax
                              value
                              e
                              er
                              w
                              s
                              mod
                              (lambda (forms er w s mod)
                                (parse (let f ((forms forms))
                                         (if (null? forms)
                                           (cdr body)
                                           (cons (cons er (wrap (car forms) w mod)) (f (cdr forms)))))
                                       ids
                                       labels
                                       var-ids
                                       vars
                                       vals
                                       bindings))))
                           ((null? ids)
                            (build-sequence
                              #f
                              (map (lambda (x) (expand (cdr x) (car x) '(()) mod))
                                   (cons (cons er (source-wrap e w s mod)) (cdr body)))))
                           (else
                            (if (not (valid-bound-ids? ids))
                              (syntax-violation
                                #f
                                "invalid or duplicate identifier in definition"
                                outer-form))
                            (let loop ((bs bindings) (er-cache #f) (r-cache #f))
                              (if (not (null? bs))
                                (let ((b (car bs)))
                                  (if (eq? (car b) 'macro)
                                    (let* ((er (cadr b))
                                           (r-cache (if (eq? er er-cache) r-cache (macros-only-env er))))
                                      (set-cdr!
                                        b
                                        (eval-local-transformer (expand (cddr b) r-cache '(()) mod) mod))
                                      (loop (cdr bs) er r-cache))
                                    (loop (cdr bs) er-cache r-cache)))))
                            (set-cdr! r (extend-env labels bindings (cdr r)))
                            (build-letrec
                              #f
                              #t
                              (reverse (map syntax->datum var-ids))
                              (reverse vars)
                              (map (lambda (x) (expand (cdr x) (car x) '(()) mod)) (reverse vals))
                              (build-sequence
                                #f
                                (map (lambda (x) (expand (cdr x) (car x) '(()) mod))
                                     (cons (cons er (source-wrap e w s mod)) (cdr body))))))))))))))))
   (expand-local-syntax
     (lambda (rec? e r w s mod k)
       (let* ((tmp e)
              (tmp ($sc-dispatch tmp '(_ #(each (any any)) any . each-any))))
         (if tmp
           (apply (lambda (id val e1 e2)
                    (let ((ids id))
                      (if (not (valid-bound-ids? ids))
                        (syntax-violation #f "duplicate bound keyword" e)
                        (let* ((labels (gen-labels ids)) (new-w (make-binding-wrap ids labels w)))
                          (k (cons e1 e2)
                             (extend-env
                               labels
                               (let ((w (if rec? new-w w)) (trans-r (macros-only-env r)))
                                 (map (lambda (x)
                                        (cons 'macro (eval-local-transformer (expand x trans-r w mod) mod)))
                                      val))
                               r)
                             new-w
                             s
                             mod)))))
                  tmp)
           (syntax-violation
             #f
             "bad local syntax definition"
             (source-wrap e w s mod))))))
   (eval-local-transformer
     (lambda (expanded mod)
       (let ((p (local-eval-hook expanded mod)))
         (if (procedure? p)
           p
           (syntax-violation #f "nonprocedure transformer" p)))))
   (expand-void (lambda () (build-void #f)))
   (ellipsis?
     (lambda (x)
       (and (nonsymbol-id? x)
            (free-id=?
              x
              '#(syntax-object
                 ...
                 ((top)
                  #(ribcage () () ())
                  #(ribcage () () ())
                  #(ribcage #(x) #((top)) #("l-*-2265"))
                  #(ribcage
                    (lambda-var-list
                      gen-var
                      strip
                      expand-lambda-case
                      lambda*-formals
                      expand-simple-lambda
                      lambda-formals
                      ellipsis?
                      expand-void
                      eval-local-transformer
                      expand-local-syntax
                      expand-body
                      expand-macro
                      expand-application
                      expand-expr
                      expand
                      syntax-type
                      parse-when-list
                      expand-install-global
                      expand-top-sequence
                      expand-sequence
                      source-wrap
                      wrap
                      bound-id-member?
                      distinct-bound-ids?
                      valid-bound-ids?
                      bound-id=?
                      free-id=?
                      with-transformer-environment
                      transformer-environment
                      resolve-identifier
                      locally-bound-identifiers
                      id-var-name
                      same-marks?
                      join-marks
                      join-wraps
                      smart-append
                      make-binding-wrap
                      extend-ribcage!
                      make-empty-ribcage
                      new-mark
                      anti-mark
                      the-anti-mark
                      top-marked?
                      top-wrap
                      empty-wrap
                      set-ribcage-labels!
                      set-ribcage-marks!
                      set-ribcage-symnames!
                      ribcage-labels
                      ribcage-marks
                      ribcage-symnames
                      ribcage?
                      make-ribcage
                      gen-labels
                      gen-label
                      make-rename
                      rename-marks
                      rename-new
                      rename-old
                      subst-rename?
                      wrap-subst
                      wrap-marks
                      make-wrap
                      id-sym-name&marks
                      id-sym-name
                      id?
                      nonsymbol-id?
                      global-extend
                      lookup
                      macros-only-env
                      extend-var-env
                      extend-env
                      null-env
                      binding-value
                      binding-type
                      make-binding
                      arg-check
                      source-annotation
                      no-source
                      set-syntax-object-module!
                      set-syntax-object-wrap!
                      set-syntax-object-expression!
                      syntax-object-module
                      syntax-object-wrap
                      syntax-object-expression
                      syntax-object?
                      make-syntax-object
                      build-lexical-var
                      build-letrec
                      build-named-let
                      build-let
                      build-sequence
                      build-data
                      build-primref
                      build-lambda-case
                      build-case-lambda
                      build-simple-lambda
                      build-global-definition
                      build-global-assignment
                      build-global-reference
                      analyze-variable
                      build-lexical-assignment
                      build-lexical-reference
                      build-dynlet
                      build-conditional
                      build-application
                      build-void
                      maybe-name-value!
                      decorate-source
                      get-global-definition-hook
                      put-global-definition-hook
                      session-id
                      local-eval-hook
                      top-level-eval-hook
                      fx<
                      fx=
                      fx-
                      fx+
                      set-lambda-meta!
                      lambda-meta
                      lambda?
                      make-dynlet
                      make-letrec
                      make-let
                      make-lambda-case
                      make-lambda
                      make-sequence
                      make-application
                      make-conditional
                      make-toplevel-define
                      make-toplevel-set
                      make-toplevel-ref
                      make-module-set
                      make-module-ref
                      make-lexical-set
                      make-lexical-ref
                      make-primitive-ref
                      make-const
                      make-void)
                    ((top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top)
                     (top))
                    ("l-*-476"
                     "l-*-474"
                     "l-*-472"
                     "l-*-470"
                     "l-*-468"
                     "l-*-466"
                     "l-*-464"
                     "l-*-462"
                     "l-*-460"
                     "l-*-458"
                     "l-*-456"
                     "l-*-454"
                     "l-*-452"
                     "l-*-450"
                     "l-*-448"
                     "l-*-446"
                     "l-*-444"
                     "l-*-442"
                     "l-*-440"
                     "l-*-438"
                     "l-*-436"
                     "l-*-434"
                     "l-*-432"
                     "l-*-430"
                     "l-*-428"
                     "l-*-426"
                     "l-*-424"
                     "l-*-422"
                     "l-*-420"
                     "l-*-418"
                     "l-*-416"
                     "l-*-414"
                     "l-*-412"
                     "l-*-410"
                     "l-*-408"
                     "l-*-406"
                     "l-*-404"
                     "l-*-402"
                     "l-*-400"
                     "l-*-399"
                     "l-*-397"
                     "l-*-394"
                     "l-*-393"
                     "l-*-392"
                     "l-*-390"
                     "l-*-389"
                     "l-*-387"
                     "l-*-385"
                     "l-*-383"
                     "l-*-381"
                     "l-*-379"
                     "l-*-377"
                     "l-*-375"
                     "l-*-373"
                     "l-*-370"
                     "l-*-368"
                     "l-*-367"
                     "l-*-365"
                     "l-*-363"
                     "l-*-361"
                     "l-*-359"
                     "l-*-358"
                     "l-*-357"
                     "l-*-356"
                     "l-*-354"
                     "l-*-353"
                     "l-*-350"
                     "l-*-348"
                     "l-*-346"
                     "l-*-344"
                     "l-*-342"
                     "l-*-340"
                     "l-*-338"
                     "l-*-337"
                     "l-*-336"
                     "l-*-334"
                     "l-*-332"
                     "l-*-331"
                     "l-*-328"
                     "l-*-327"
                     "l-*-325"
                     "l-*-323"
                     "l-*-321"
                     "l-*-319"
                     "l-*-317"
                     "l-*-315"
                     "l-*-313"
                     "l-*-311"
                     "l-*-309"
                     "l-*-306"
                     "l-*-304"
                     "l-*-302"
                     "l-*-300"
                     "l-*-298"
                     "l-*-296"
                     "l-*-294"
                     "l-*-292"
                     "l-*-290"
                     "l-*-288"
                     "l-*-286"
                     "l-*-284"
                     "l-*-282"
                     "l-*-280"
                     "l-*-278"
                     "l-*-276"
                     "l-*-274"
                     "l-*-272"
                     "l-*-270"
                     "l-*-268"
                     "l-*-266"
                     "l-*-264"
                     "l-*-262"
                     "l-*-260"
                     "l-*-258"
                     "l-*-256"
                     "l-*-255"
                     "l-*-254"
                     "l-*-253"
                     "l-*-252"
                     "l-*-250"
                     "l-*-248"
                     "l-*-246"
                     "l-*-243"
                     "l-*-241"
                     "l-*-239"
                     "l-*-237"
                     "l-*-235"
                     "l-*-233"
                     "l-*-231"
                     "l-*-229"
                     "l-*-227"
                     "l-*-225"
                     "l-*-223"
                     "l-*-221"
                     "l-*-219"
                     "l-*-217"
                     "l-*-215"
                     "l-*-213"
                     "l-*-211"
                     "l-*-209"))
                  #(ribcage
                    (define-structure
                      define-expansion-accessors
                      define-expansion-constructors)
                    ((top) (top) (top))
                    ("l-*-47" "l-*-46" "l-*-45")))
                 (hygiene guile))))))
   (lambda-formals
     (lambda (orig-args)
       (letrec*
         ((req (lambda (args rreq)
                 (let* ((tmp args) (tmp-1 ($sc-dispatch tmp '())))
                   (if tmp-1
                     (apply (lambda () (check (reverse rreq) #f)) tmp-1)
                     (let ((tmp-1 ($sc-dispatch tmp '(any . any))))
                       (if (and tmp-1 (apply (lambda (a b) (id? a)) tmp-1))
                         (apply (lambda (a b) (req b (cons a rreq))) tmp-1)
                         (let ((tmp-1 (list tmp)))
                           (if (and tmp-1 (apply (lambda (r) (id? r)) tmp-1))
                             (apply (lambda (r) (check (reverse rreq) r)) tmp-1)
                             (let ((else tmp))
                               (syntax-violation 'lambda "invalid argument list" orig-args args))))))))))
          (check (lambda (req rest)
                   (if (distinct-bound-ids? (if rest (cons rest req) req))
                     (values req #f rest #f)
                     (syntax-violation
                       'lambda
                       "duplicate identifier in argument list"
                       orig-args)))))
         (req orig-args '()))))
   (expand-simple-lambda
     (lambda (e r w s mod req rest meta body)
       (let* ((ids (if rest (append req (list rest)) req))
              (vars (map gen-var ids))
              (labels (gen-labels ids)))
         (build-simple-lambda
           s
           (map syntax->datum req)
           (and rest (syntax->datum rest))
           vars
           meta
           (expand-body
             body
             (source-wrap e w s mod)
             (extend-var-env labels vars r)
             (make-binding-wrap ids labels w)
             mod)))))
   (lambda*-formals
     (lambda (orig-args)
       (letrec*
         ((req (lambda (args rreq)
                 (let* ((tmp args) (tmp-1 ($sc-dispatch tmp '())))
                   (if tmp-1
                     (apply (lambda () (check (reverse rreq) '() #f '())) tmp-1)
                     (let ((tmp-1 ($sc-dispatch tmp '(any . any))))
                       (if (and tmp-1 (apply (lambda (a b) (id? a)) tmp-1))
                         (apply (lambda (a b) (req b (cons a rreq))) tmp-1)
                         (let ((tmp-1 ($sc-dispatch tmp '(any . any))))
                           (if (and tmp-1
                                    (apply (lambda (a b) (eq? (syntax->datum a) #:optional)) tmp-1))
                             (apply (lambda (a b) (opt b (reverse rreq) '())) tmp-1)
                             (let ((tmp-1 ($sc-dispatch tmp '(any . any))))
                               (if (and tmp-1
                                        (apply (lambda (a b) (eq? (syntax->datum a) #:key)) tmp-1))
                                 (apply (lambda (a b) (key b (reverse rreq) '() '())) tmp-1)
                                 (let ((tmp-1 ($sc-dispatch tmp '(any any))))
                                   (if (and tmp-1
                                            (apply (lambda (a b) (eq? (syntax->datum a) #:rest)) tmp-1))
                                     (apply (lambda (a b) (rest b (reverse rreq) '() '())) tmp-1)
                                     (let ((tmp-1 (list tmp)))
                                       (if (and tmp-1 (apply (lambda (r) (id? r)) tmp-1))
                                         (apply (lambda (r) (rest r (reverse rreq) '() '())) tmp-1)
                                         (let ((else tmp))
                                           (syntax-violation
                                             'lambda*
                                             "invalid argument list"
                                             orig-args
                                             args))))))))))))))))
          (opt (lambda (args req ropt)
                 (let* ((tmp args) (tmp-1 ($sc-dispatch tmp '())))
                   (if tmp-1
                     (apply (lambda () (check req (reverse ropt) #f '())) tmp-1)
                     (let ((tmp-1 ($sc-dispatch tmp '(any . any))))
                       (if (and tmp-1 (apply (lambda (a b) (id? a)) tmp-1))
                         (apply (lambda (a b)
                                  (opt b
                                       req
                                       (cons (cons a
                                                   '(#(syntax-object
                                                       #f
                                                       ((top)
                                                        #(ribcage #(a b) #((top) (top)) #("l-*-2402" "l-*-2403"))
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(args req ropt)
                                                          #((top) (top) (top))
                                                          #("l-*-2392" "l-*-2393" "l-*-2394"))
                                                        #(ribcage
                                                          (check rest key opt req)
                                                          ((top) (top) (top) (top) (top))
                                                          ("l-*-2338" "l-*-2336" "l-*-2334" "l-*-2332" "l-*-2330"))
                                                        #(ribcage #(orig-args) #((top)) #("l-*-2329"))
                                                        #(ribcage
                                                          (lambda-var-list
                                                            gen-var
                                                            strip
                                                            expand-lambda-case
                                                            lambda*-formals
                                                            expand-simple-lambda
                                                            lambda-formals
                                                            ellipsis?
                                                            expand-void
                                                            eval-local-transformer
                                                            expand-local-syntax
                                                            expand-body
                                                            expand-macro
                                                            expand-application
                                                            expand-expr
                                                            expand
                                                            syntax-type
                                                            parse-when-list
                                                            expand-install-global
                                                            expand-top-sequence
                                                            expand-sequence
                                                            source-wrap
                                                            wrap
                                                            bound-id-member?
                                                            distinct-bound-ids?
                                                            valid-bound-ids?
                                                            bound-id=?
                                                            free-id=?
                                                            with-transformer-environment
                                                            transformer-environment
                                                            resolve-identifier
                                                            locally-bound-identifiers
                                                            id-var-name
                                                            same-marks?
                                                            join-marks
                                                            join-wraps
                                                            smart-append
                                                            make-binding-wrap
                                                            extend-ribcage!
                                                            make-empty-ribcage
                                                            new-mark
                                                            anti-mark
                                                            the-anti-mark
                                                            top-marked?
                                                            top-wrap
                                                            empty-wrap
                                                            set-ribcage-labels!
                                                            set-ribcage-marks!
                                                            set-ribcage-symnames!
                                                            ribcage-labels
                                                            ribcage-marks
                                                            ribcage-symnames
                                                            ribcage?
                                                            make-ribcage
                                                            gen-labels
                                                            gen-label
                                                            make-rename
                                                            rename-marks
                                                            rename-new
                                                            rename-old
                                                            subst-rename?
                                                            wrap-subst
                                                            wrap-marks
                                                            make-wrap
                                                            id-sym-name&marks
                                                            id-sym-name
                                                            id?
                                                            nonsymbol-id?
                                                            global-extend
                                                            lookup
                                                            macros-only-env
                                                            extend-var-env
                                                            extend-env
                                                            null-env
                                                            binding-value
                                                            binding-type
                                                            make-binding
                                                            arg-check
                                                            source-annotation
                                                            no-source
                                                            set-syntax-object-module!
                                                            set-syntax-object-wrap!
                                                            set-syntax-object-expression!
                                                            syntax-object-module
                                                            syntax-object-wrap
                                                            syntax-object-expression
                                                            syntax-object?
                                                            make-syntax-object
                                                            build-lexical-var
                                                            build-letrec
                                                            build-named-let
                                                            build-let
                                                            build-sequence
                                                            build-data
                                                            build-primref
                                                            build-lambda-case
                                                            build-case-lambda
                                                            build-simple-lambda
                                                            build-global-definition
                                                            build-global-assignment
                                                            build-global-reference
                                                            analyze-variable
                                                            build-lexical-assignment
                                                            build-lexical-reference
                                                            build-dynlet
                                                            build-conditional
                                                            build-application
                                                            build-void
                                                            maybe-name-value!
                                                            decorate-source
                                                            get-global-definition-hook
                                                            put-global-definition-hook
                                                            session-id
                                                            local-eval-hook
                                                            top-level-eval-hook
                                                            fx<
                                                            fx=
                                                            fx-
                                                            fx+
                                                            set-lambda-meta!
                                                            lambda-meta
                                                            lambda?
                                                            make-dynlet
                                                            make-letrec
                                                            make-let
                                                            make-lambda-case
                                                            make-lambda
                                                            make-sequence
                                                            make-application
                                                            make-conditional
                                                            make-toplevel-define
                                                            make-toplevel-set
                                                            make-toplevel-ref
                                                            make-module-set
                                                            make-module-ref
                                                            make-lexical-set
                                                            make-lexical-ref
                                                            make-primitive-ref
                                                            make-const
                                                            make-void)
                                                          ((top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top))
                                                          ("l-*-476"
                                                           "l-*-474"
                                                           "l-*-472"
                                                           "l-*-470"
                                                           "l-*-468"
                                                           "l-*-466"
                                                           "l-*-464"
                                                           "l-*-462"
                                                           "l-*-460"
                                                           "l-*-458"
                                                           "l-*-456"
                                                           "l-*-454"
                                                           "l-*-452"
                                                           "l-*-450"
                                                           "l-*-448"
                                                           "l-*-446"
                                                           "l-*-444"
                                                           "l-*-442"
                                                           "l-*-440"
                                                           "l-*-438"
                                                           "l-*-436"
                                                           "l-*-434"
                                                           "l-*-432"
                                                           "l-*-430"
                                                           "l-*-428"
                                                           "l-*-426"
                                                           "l-*-424"
                                                           "l-*-422"
                                                           "l-*-420"
                                                           "l-*-418"
                                                           "l-*-416"
                                                           "l-*-414"
                                                           "l-*-412"
                                                           "l-*-410"
                                                           "l-*-408"
                                                           "l-*-406"
                                                           "l-*-404"
                                                           "l-*-402"
                                                           "l-*-400"
                                                           "l-*-399"
                                                           "l-*-397"
                                                           "l-*-394"
                                                           "l-*-393"
                                                           "l-*-392"
                                                           "l-*-390"
                                                           "l-*-389"
                                                           "l-*-387"
                                                           "l-*-385"
                                                           "l-*-383"
                                                           "l-*-381"
                                                           "l-*-379"
                                                           "l-*-377"
                                                           "l-*-375"
                                                           "l-*-373"
                                                           "l-*-370"
                                                           "l-*-368"
                                                           "l-*-367"
                                                           "l-*-365"
                                                           "l-*-363"
                                                           "l-*-361"
                                                           "l-*-359"
                                                           "l-*-358"
                                                           "l-*-357"
                                                           "l-*-356"
                                                           "l-*-354"
                                                           "l-*-353"
                                                           "l-*-350"
                                                           "l-*-348"
                                                           "l-*-346"
                                                           "l-*-344"
                                                           "l-*-342"
                                                           "l-*-340"
                                                           "l-*-338"
                                                           "l-*-337"
                                                           "l-*-336"
                                                           "l-*-334"
                                                           "l-*-332"
                                                           "l-*-331"
                                                           "l-*-328"
                                                           "l-*-327"
                                                           "l-*-325"
                                                           "l-*-323"
                                                           "l-*-321"
                                                           "l-*-319"
                                                           "l-*-317"
                                                           "l-*-315"
                                                           "l-*-313"
                                                           "l-*-311"
                                                           "l-*-309"
                                                           "l-*-306"
                                                           "l-*-304"
                                                           "l-*-302"
                                                           "l-*-300"
                                                           "l-*-298"
                                                           "l-*-296"
                                                           "l-*-294"
                                                           "l-*-292"
                                                           "l-*-290"
                                                           "l-*-288"
                                                           "l-*-286"
                                                           "l-*-284"
                                                           "l-*-282"
                                                           "l-*-280"
                                                           "l-*-278"
                                                           "l-*-276"
                                                           "l-*-274"
                                                           "l-*-272"
                                                           "l-*-270"
                                                           "l-*-268"
                                                           "l-*-266"
                                                           "l-*-264"
                                                           "l-*-262"
                                                           "l-*-260"
                                                           "l-*-258"
                                                           "l-*-256"
                                                           "l-*-255"
                                                           "l-*-254"
                                                           "l-*-253"
                                                           "l-*-252"
                                                           "l-*-250"
                                                           "l-*-248"
                                                           "l-*-246"
                                                           "l-*-243"
                                                           "l-*-241"
                                                           "l-*-239"
                                                           "l-*-237"
                                                           "l-*-235"
                                                           "l-*-233"
                                                           "l-*-231"
                                                           "l-*-229"
                                                           "l-*-227"
                                                           "l-*-225"
                                                           "l-*-223"
                                                           "l-*-221"
                                                           "l-*-219"
                                                           "l-*-217"
                                                           "l-*-215"
                                                           "l-*-213"
                                                           "l-*-211"
                                                           "l-*-209"))
                                                        #(ribcage
                                                          (define-structure
                                                            define-expansion-accessors
                                                            define-expansion-constructors)
                                                          ((top) (top) (top))
                                                          ("l-*-47" "l-*-46" "l-*-45")))
                                                       (hygiene guile))))
                                             ropt)))
                                tmp-1)
                         (let ((tmp-1 ($sc-dispatch tmp '((any any) . any))))
                           (if (and tmp-1 (apply (lambda (a init b) (id? a)) tmp-1))
                             (apply (lambda (a init b) (opt b req (cons (list a init) ropt)))
                                    tmp-1)
                             (let ((tmp-1 ($sc-dispatch tmp '(any . any))))
                               (if (and tmp-1
                                        (apply (lambda (a b) (eq? (syntax->datum a) #:key)) tmp-1))
                                 (apply (lambda (a b) (key b req (reverse ropt) '())) tmp-1)
                                 (let ((tmp-1 ($sc-dispatch tmp '(any any))))
                                   (if (and tmp-1
                                            (apply (lambda (a b) (eq? (syntax->datum a) #:rest)) tmp-1))
                                     (apply (lambda (a b) (rest b req (reverse ropt) '())) tmp-1)
                                     (let ((tmp-1 (list tmp)))
                                       (if (and tmp-1 (apply (lambda (r) (id? r)) tmp-1))
                                         (apply (lambda (r) (rest r req (reverse ropt) '())) tmp-1)
                                         (let ((else tmp))
                                           (syntax-violation
                                             'lambda*
                                             "invalid optional argument list"
                                             orig-args
                                             args))))))))))))))))
          (key (lambda (args req opt rkey)
                 (let* ((tmp args) (tmp-1 ($sc-dispatch tmp '())))
                   (if tmp-1
                     (apply (lambda () (check req opt #f (cons #f (reverse rkey)))) tmp-1)
                     (let ((tmp-1 ($sc-dispatch tmp '(any . any))))
                       (if (and tmp-1 (apply (lambda (a b) (id? a)) tmp-1))
                         (apply (lambda (a b)
                                  (let* ((tmp (symbol->keyword (syntax->datum a))) (k tmp))
                                    (key b
                                         req
                                         opt
                                         (cons (cons k
                                                     (cons a
                                                           '(#(syntax-object
                                                               #f
                                                               ((top)
                                                                #(ribcage () () ())
                                                                #(ribcage #(k) #((top)) #("l-*-2465"))
                                                                #(ribcage
                                                                  #(a b)
                                                                  #((top) (top))
                                                                  #("l-*-2459" "l-*-2460"))
                                                                #(ribcage () () ())
                                                                #(ribcage
                                                                  #(args req opt rkey)
                                                                  #((top) (top) (top) (top))
                                                                  #("l-*-2448" "l-*-2449" "l-*-2450" "l-*-2451"))
                                                                #(ribcage
                                                                  (check rest key opt req)
                                                                  ((top) (top) (top) (top) (top))
                                                                  ("l-*-2338"
                                                                   "l-*-2336"
                                                                   "l-*-2334"
                                                                   "l-*-2332"
                                                                   "l-*-2330"))
                                                                #(ribcage #(orig-args) #((top)) #("l-*-2329"))
                                                                #(ribcage
                                                                  (lambda-var-list
                                                                    gen-var
                                                                    strip
                                                                    expand-lambda-case
                                                                    lambda*-formals
                                                                    expand-simple-lambda
                                                                    lambda-formals
                                                                    ellipsis?
                                                                    expand-void
                                                                    eval-local-transformer
                                                                    expand-local-syntax
                                                                    expand-body
                                                                    expand-macro
                                                                    expand-application
                                                                    expand-expr
                                                                    expand
                                                                    syntax-type
                                                                    parse-when-list
                                                                    expand-install-global
                                                                    expand-top-sequence
                                                                    expand-sequence
                                                                    source-wrap
                                                                    wrap
                                                                    bound-id-member?
                                                                    distinct-bound-ids?
                                                                    valid-bound-ids?
                                                                    bound-id=?
                                                                    free-id=?
                                                                    with-transformer-environment
                                                                    transformer-environment
                                                                    resolve-identifier
                                                                    locally-bound-identifiers
                                                                    id-var-name
                                                                    same-marks?
                                                                    join-marks
                                                                    join-wraps
                                                                    smart-append
                                                                    make-binding-wrap
                                                                    extend-ribcage!
                                                                    make-empty-ribcage
                                                                    new-mark
                                                                    anti-mark
                                                                    the-anti-mark
                                                                    top-marked?
                                                                    top-wrap
                                                                    empty-wrap
                                                                    set-ribcage-labels!
                                                                    set-ribcage-marks!
                                                                    set-ribcage-symnames!
                                                                    ribcage-labels
                                                                    ribcage-marks
                                                                    ribcage-symnames
                                                                    ribcage?
                                                                    make-ribcage
                                                                    gen-labels
                                                                    gen-label
                                                                    make-rename
                                                                    rename-marks
                                                                    rename-new
                                                                    rename-old
                                                                    subst-rename?
                                                                    wrap-subst
                                                                    wrap-marks
                                                                    make-wrap
                                                                    id-sym-name&marks
                                                                    id-sym-name
                                                                    id?
                                                                    nonsymbol-id?
                                                                    global-extend
                                                                    lookup
                                                                    macros-only-env
                                                                    extend-var-env
                                                                    extend-env
                                                                    null-env
                                                                    binding-value
                                                                    binding-type
                                                                    make-binding
                                                                    arg-check
                                                                    source-annotation
                                                                    no-source
                                                                    set-syntax-object-module!
                                                                    set-syntax-object-wrap!
                                                                    set-syntax-object-expression!
                                                                    syntax-object-module
                                                                    syntax-object-wrap
                                                                    syntax-object-expression
                                                                    syntax-object?
                                                                    make-syntax-object
                                                                    build-lexical-var
                                                                    build-letrec
                                                                    build-named-let
                                                                    build-let
                                                                    build-sequence
                                                                    build-data
                                                                    build-primref
                                                                    build-lambda-case
                                                                    build-case-lambda
                                                                    build-simple-lambda
                                                                    build-global-definition
                                                                    build-global-assignment
                                                                    build-global-reference
                                                                    analyze-variable
                                                                    build-lexical-assignment
                                                                    build-lexical-reference
                                                                    build-dynlet
                                                                    build-conditional
                                                                    build-application
                                                                    build-void
                                                                    maybe-name-value!
                                                                    decorate-source
                                                                    get-global-definition-hook
                                                                    put-global-definition-hook
                                                                    session-id
                                                                    local-eval-hook
                                                                    top-level-eval-hook
                                                                    fx<
                                                                    fx=
                                                                    fx-
                                                                    fx+
                                                                    set-lambda-meta!
                                                                    lambda-meta
                                                                    lambda?
                                                                    make-dynlet
                                                                    make-letrec
                                                                    make-let
                                                                    make-lambda-case
                                                                    make-lambda
                                                                    make-sequence
                                                                    make-application
                                                                    make-conditional
                                                                    make-toplevel-define
                                                                    make-toplevel-set
                                                                    make-toplevel-ref
                                                                    make-module-set
                                                                    make-module-ref
                                                                    make-lexical-set
                                                                    make-lexical-ref
                                                                    make-primitive-ref
                                                                    make-const
                                                                    make-void)
                                                                  ((top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top))
                                                                  ("l-*-476"
                                                                   "l-*-474"
                                                                   "l-*-472"
                                                                   "l-*-470"
                                                                   "l-*-468"
                                                                   "l-*-466"
                                                                   "l-*-464"
                                                                   "l-*-462"
                                                                   "l-*-460"
                                                                   "l-*-458"
                                                                   "l-*-456"
                                                                   "l-*-454"
                                                                   "l-*-452"
                                                                   "l-*-450"
                                                                   "l-*-448"
                                                                   "l-*-446"
                                                                   "l-*-444"
                                                                   "l-*-442"
                                                                   "l-*-440"
                                                                   "l-*-438"
                                                                   "l-*-436"
                                                                   "l-*-434"
                                                                   "l-*-432"
                                                                   "l-*-430"
                                                                   "l-*-428"
                                                                   "l-*-426"
                                                                   "l-*-424"
                                                                   "l-*-422"
                                                                   "l-*-420"
                                                                   "l-*-418"
                                                                   "l-*-416"
                                                                   "l-*-414"
                                                                   "l-*-412"
                                                                   "l-*-410"
                                                                   "l-*-408"
                                                                   "l-*-406"
                                                                   "l-*-404"
                                                                   "l-*-402"
                                                                   "l-*-400"
                                                                   "l-*-399"
                                                                   "l-*-397"
                                                                   "l-*-394"
                                                                   "l-*-393"
                                                                   "l-*-392"
                                                                   "l-*-390"
                                                                   "l-*-389"
                                                                   "l-*-387"
                                                                   "l-*-385"
                                                                   "l-*-383"
                                                                   "l-*-381"
                                                                   "l-*-379"
                                                                   "l-*-377"
                                                                   "l-*-375"
                                                                   "l-*-373"
                                                                   "l-*-370"
                                                                   "l-*-368"
                                                                   "l-*-367"
                                                                   "l-*-365"
                                                                   "l-*-363"
                                                                   "l-*-361"
                                                                   "l-*-359"
                                                                   "l-*-358"
                                                                   "l-*-357"
                                                                   "l-*-356"
                                                                   "l-*-354"
                                                                   "l-*-353"
                                                                   "l-*-350"
                                                                   "l-*-348"
                                                                   "l-*-346"
                                                                   "l-*-344"
                                                                   "l-*-342"
                                                                   "l-*-340"
                                                                   "l-*-338"
                                                                   "l-*-337"
                                                                   "l-*-336"
                                                                   "l-*-334"
                                                                   "l-*-332"
                                                                   "l-*-331"
                                                                   "l-*-328"
                                                                   "l-*-327"
                                                                   "l-*-325"
                                                                   "l-*-323"
                                                                   "l-*-321"
                                                                   "l-*-319"
                                                                   "l-*-317"
                                                                   "l-*-315"
                                                                   "l-*-313"
                                                                   "l-*-311"
                                                                   "l-*-309"
                                                                   "l-*-306"
                                                                   "l-*-304"
                                                                   "l-*-302"
                                                                   "l-*-300"
                                                                   "l-*-298"
                                                                   "l-*-296"
                                                                   "l-*-294"
                                                                   "l-*-292"
                                                                   "l-*-290"
                                                                   "l-*-288"
                                                                   "l-*-286"
                                                                   "l-*-284"
                                                                   "l-*-282"
                                                                   "l-*-280"
                                                                   "l-*-278"
                                                                   "l-*-276"
                                                                   "l-*-274"
                                                                   "l-*-272"
                                                                   "l-*-270"
                                                                   "l-*-268"
                                                                   "l-*-266"
                                                                   "l-*-264"
                                                                   "l-*-262"
                                                                   "l-*-260"
                                                                   "l-*-258"
                                                                   "l-*-256"
                                                                   "l-*-255"
                                                                   "l-*-254"
                                                                   "l-*-253"
                                                                   "l-*-252"
                                                                   "l-*-250"
                                                                   "l-*-248"
                                                                   "l-*-246"
                                                                   "l-*-243"
                                                                   "l-*-241"
                                                                   "l-*-239"
                                                                   "l-*-237"
                                                                   "l-*-235"
                                                                   "l-*-233"
                                                                   "l-*-231"
                                                                   "l-*-229"
                                                                   "l-*-227"
                                                                   "l-*-225"
                                                                   "l-*-223"
                                                                   "l-*-221"
                                                                   "l-*-219"
                                                                   "l-*-217"
                                                                   "l-*-215"
                                                                   "l-*-213"
                                                                   "l-*-211"
                                                                   "l-*-209"))
                                                                #(ribcage
                                                                  (define-structure
                                                                    define-expansion-accessors
                                                                    define-expansion-constructors)
                                                                  ((top) (top) (top))
                                                                  ("l-*-47" "l-*-46" "l-*-45")))
                                                               (hygiene guile)))))
                                               rkey))))
                                tmp-1)
                         (let ((tmp-1 ($sc-dispatch tmp '((any any) . any))))
                           (if (and tmp-1 (apply (lambda (a init b) (id? a)) tmp-1))
                             (apply (lambda (a init b)
                                      (let* ((tmp (symbol->keyword (syntax->datum a))) (k tmp))
                                        (key b req opt (cons (list k a init) rkey))))
                                    tmp-1)
                             (let ((tmp-1 ($sc-dispatch tmp '((any any any) . any))))
                               (if (and tmp-1
                                        (apply (lambda (a init k b) (and (id? a) (keyword? (syntax->datum k))))
                                               tmp-1))
                                 (apply (lambda (a init k b) (key b req opt (cons (list k a init) rkey)))
                                        tmp-1)
                                 (let ((tmp-1 ($sc-dispatch tmp '(any))))
                                   (if (and tmp-1
                                            (apply (lambda (aok) (eq? (syntax->datum aok) #:allow-other-keys))
                                                   tmp-1))
                                     (apply (lambda (aok) (check req opt #f (cons #t (reverse rkey))))
                                            tmp-1)
                                     (let ((tmp-1 ($sc-dispatch tmp '(any any any))))
                                       (if (and tmp-1
                                                (apply (lambda (aok a b)
                                                         (and (eq? (syntax->datum aok) #:allow-other-keys)
                                                              (eq? (syntax->datum a) #:rest)))
                                                       tmp-1))
                                         (apply (lambda (aok a b) (rest b req opt (cons #t (reverse rkey))))
                                                tmp-1)
                                         (let ((tmp-1 ($sc-dispatch tmp '(any . any))))
                                           (if (and tmp-1
                                                    (apply (lambda (aok r)
                                                             (and (eq? (syntax->datum aok) #:allow-other-keys) (id? r)))
                                                           tmp-1))
                                             (apply (lambda (aok r) (rest r req opt (cons #t (reverse rkey))))
                                                    tmp-1)
                                             (let ((tmp-1 ($sc-dispatch tmp '(any any))))
                                               (if (and tmp-1
                                                        (apply (lambda (a b) (eq? (syntax->datum a) #:rest)) tmp-1))
                                                 (apply (lambda (a b) (rest b req opt (cons #f (reverse rkey))))
                                                        tmp-1)
                                                 (let ((tmp-1 (list tmp)))
                                                   (if (and tmp-1 (apply (lambda (r) (id? r)) tmp-1))
                                                     (apply (lambda (r) (rest r req opt (cons #f (reverse rkey))))
                                                            tmp-1)
                                                     (let ((else tmp))
                                                       (syntax-violation
                                                         'lambda*
                                                         "invalid keyword argument list"
                                                         orig-args
                                                         args))))))))))))))))))))))
          (rest (lambda (args req opt kw)
                  (let* ((tmp-1 args) (tmp (list tmp-1)))
                    (if (and tmp (apply (lambda (r) (id? r)) tmp))
                      (apply (lambda (r) (check req opt r kw)) tmp)
                      (let ((else tmp-1))
                        (syntax-violation 'lambda* "invalid rest argument" orig-args args))))))
          (check (lambda (req opt rest kw)
                   (if (distinct-bound-ids?
                         (append
                           req
                           (map car opt)
                           (if rest (list rest) '())
                           (if (pair? kw) (map cadr (cdr kw)) '())))
                     (values req opt rest kw)
                     (syntax-violation
                       'lambda*
                       "duplicate identifier in argument list"
                       orig-args)))))
         (req orig-args '()))))
   (expand-lambda-case
     (lambda (e r w s mod get-formals clauses)
       (letrec*
         ((parse-req
            (lambda (req opt rest kw body)
              (let ((vars (map gen-var req)) (labels (gen-labels req)))
                (let ((r* (extend-var-env labels vars r))
                      (w* (make-binding-wrap req labels w)))
                  (parse-opt
                    (map syntax->datum req)
                    opt
                    rest
                    kw
                    body
                    (reverse vars)
                    r*
                    w*
                    '()
                    '())))))
          (parse-opt
            (lambda (req opt rest kw body vars r* w* out inits)
              (cond ((pair? opt)
                     (let* ((tmp-1 (car opt)) (tmp ($sc-dispatch tmp-1 '(any any))))
                       (if tmp
                         (apply (lambda (id i)
                                  (let* ((v (gen-var id))
                                         (l (gen-labels (list v)))
                                         (r** (extend-var-env l (list v) r*))
                                         (w** (make-binding-wrap (list id) l w*)))
                                    (parse-opt
                                      req
                                      (cdr opt)
                                      rest
                                      kw
                                      body
                                      (cons v vars)
                                      r**
                                      w**
                                      (cons (syntax->datum id) out)
                                      (cons (expand i r* w* mod) inits))))
                                tmp)
                         (syntax-violation
                           #f
                           "source expression failed to match any pattern"
                           tmp-1))))
                    (rest
                     (let* ((v (gen-var rest))
                            (l (gen-labels (list v)))
                            (r* (extend-var-env l (list v) r*))
                            (w* (make-binding-wrap (list rest) l w*)))
                       (parse-kw
                         req
                         (and (pair? out) (reverse out))
                         (syntax->datum rest)
                         (if (pair? kw) (cdr kw) kw)
                         body
                         (cons v vars)
                         r*
                         w*
                         (and (pair? kw) (car kw))
                         '()
                         inits)))
                    (else
                     (parse-kw
                       req
                       (and (pair? out) (reverse out))
                       #f
                       (if (pair? kw) (cdr kw) kw)
                       body
                       vars
                       r*
                       w*
                       (and (pair? kw) (car kw))
                       '()
                       inits)))))
          (parse-kw
            (lambda (req opt rest kw body vars r* w* aok out inits)
              (if (pair? kw)
                (let* ((tmp-1 (car kw)) (tmp ($sc-dispatch tmp-1 '(any any any))))
                  (if tmp
                    (apply (lambda (k id i)
                             (let* ((v (gen-var id))
                                    (l (gen-labels (list v)))
                                    (r** (extend-var-env l (list v) r*))
                                    (w** (make-binding-wrap (list id) l w*)))
                               (parse-kw
                                 req
                                 opt
                                 rest
                                 (cdr kw)
                                 body
                                 (cons v vars)
                                 r**
                                 w**
                                 aok
                                 (cons (list (syntax->datum k) (syntax->datum id) v) out)
                                 (cons (expand i r* w* mod) inits))))
                           tmp)
                    (syntax-violation
                      #f
                      "source expression failed to match any pattern"
                      tmp-1)))
                (parse-body
                  req
                  opt
                  rest
                  (and (or aok (pair? out)) (cons aok (reverse out)))
                  body
                  (reverse vars)
                  r*
                  w*
                  (reverse inits)
                  '()))))
          (parse-body
            (lambda (req opt rest kw body vars r* w* inits meta)
              (let* ((tmp body) (tmp-1 ($sc-dispatch tmp '(any any . each-any))))
                (if (and tmp-1
                         (apply (lambda (docstring e1 e2) (string? (syntax->datum docstring)))
                                tmp-1))
                  (apply (lambda (docstring e1 e2)
                           (parse-body
                             req
                             opt
                             rest
                             kw
                             (cons e1 e2)
                             vars
                             r*
                             w*
                             inits
                             (append meta (list (cons 'documentation (syntax->datum docstring))))))
                         tmp-1)
                  (let ((tmp-1 ($sc-dispatch tmp '(#(vector #(each (any . any))) any . each-any))))
                    (if tmp-1
                      (apply (lambda (k v e1 e2)
                               (parse-body
                                 req
                                 opt
                                 rest
                                 kw
                                 (cons e1 e2)
                                 vars
                                 r*
                                 w*
                                 inits
                                 (append meta (syntax->datum (map cons k v)))))
                             tmp-1)
                      (let ((tmp-1 ($sc-dispatch tmp '(any . each-any))))
                        (if tmp-1
                          (apply (lambda (e1 e2)
                                   (values
                                     meta
                                     req
                                     opt
                                     rest
                                     kw
                                     inits
                                     vars
                                     (expand-body (cons e1 e2) (source-wrap e w s mod) r* w* mod)))
                                 tmp-1)
                          (syntax-violation
                            #f
                            "source expression failed to match any pattern"
                            tmp))))))))))
         (let* ((tmp clauses) (tmp-1 ($sc-dispatch tmp '())))
           (if tmp-1
             (apply (lambda () (values '() #f)) tmp-1)
             (let ((tmp-1 ($sc-dispatch
                            tmp
                            '((any any . each-any) . #(each (any any . each-any))))))
               (if tmp-1
                 (apply (lambda (args e1 e2 args* e1* e2*)
                          (call-with-values
                            (lambda () (get-formals args))
                            (lambda (req opt rest kw)
                              (call-with-values
                                (lambda () (parse-req req opt rest kw (cons e1 e2)))
                                (lambda (meta req opt rest kw inits vars body)
                                  (call-with-values
                                    (lambda ()
                                      (expand-lambda-case
                                        e
                                        r
                                        w
                                        s
                                        mod
                                        get-formals
                                        (map (lambda (tmp-2 tmp-1 tmp) (cons tmp (cons tmp-1 tmp-2)))
                                             e2*
                                             e1*
                                             args*)))
                                    (lambda (meta* else*)
                                      (values
                                        (append meta meta*)
                                        (build-lambda-case s req opt rest kw inits vars body else*)))))))))
                        tmp-1)
                 (syntax-violation
                   #f
                   "source expression failed to match any pattern"
                   tmp))))))))
   (strip (lambda (x w)
            (if (memq 'top (car w))
              x
              (let f ((x x))
                (cond ((syntax-object? x)
                       (strip (syntax-object-expression x) (syntax-object-wrap x)))
                      ((pair? x)
                       (let ((a (f (car x))) (d (f (cdr x))))
                         (if (and (eq? a (car x)) (eq? d (cdr x))) x (cons a d))))
                      ((vector? x)
                       (let* ((old (vector->list x)) (new (map f old)))
                         (let lp ((l1 old) (l2 new))
                           (cond ((null? l1) x)
                                 ((eq? (car l1) (car l2)) (lp (cdr l1) (cdr l2)))
                                 (else (list->vector new))))))
                      (else x))))))
   (gen-var
     (lambda (id)
       (let ((id (if (syntax-object? id) (syntax-object-expression id) id)))
         (gensym (string-append (symbol->string id) "-")))))
   (lambda-var-list
     (lambda (vars)
       (let lvl ((vars vars) (ls '()) (w '(())))
         (cond ((pair? vars) (lvl (cdr vars) (cons (wrap (car vars) w #f) ls) w))
               ((id? vars) (cons (wrap vars w #f) ls))
               ((null? vars) ls)
               ((syntax-object? vars)
                (lvl (syntax-object-expression vars)
                     ls
                     (join-wraps w (syntax-object-wrap vars))))
               (else (cons vars ls)))))))
  (global-extend 'local-syntax 'letrec-syntax #t)
  (global-extend 'local-syntax 'let-syntax #f)
  (global-extend
    'core
    'syntax-parameterize
    (lambda (e r w s mod)
      (let* ((tmp e)
             (tmp ($sc-dispatch tmp '(_ #(each (any any)) any . each-any))))
        (if (and tmp (apply (lambda (var val e1 e2) (valid-bound-ids? var)) tmp))
          (apply (lambda (var val e1 e2)
                   (let ((names (map (lambda (x) (id-var-name x w)) var)))
                     (for-each
                       (lambda (id n)
                         (let ((key (car (lookup n r mod))))
                           (if (memv key '(displaced-lexical))
                             (syntax-violation
                               'syntax-parameterize
                               "identifier out of context"
                               e
                               (source-wrap id w s mod)))))
                       var
                       names)
                     (expand-body
                       (cons e1 e2)
                       (source-wrap e w s mod)
                       (extend-env
                         names
                         (let ((trans-r (macros-only-env r)))
                           (map (lambda (x)
                                  (cons 'macro (eval-local-transformer (expand x trans-r w mod) mod)))
                                val))
                         r)
                       w
                       mod)))
                 tmp)
          (syntax-violation
            'syntax-parameterize
            "bad syntax"
            (source-wrap e w s mod))))))
  (global-extend
    'core
    'quote
    (lambda (e r w s mod)
      (let* ((tmp e) (tmp ($sc-dispatch tmp '(_ any))))
        (if tmp
          (apply (lambda (e) (build-data s (strip e w))) tmp)
          (syntax-violation 'quote "bad syntax" (source-wrap e w s mod))))))
  (global-extend
    'core
    'syntax
    (letrec*
      ((gen-syntax
         (lambda (src e r maps ellipsis? mod)
           (if (id? e)
             (let* ((label (id-var-name e '(()))) (b (lookup label r mod)))
               (cond ((eq? (car b) 'syntax)
                      (call-with-values
                        (lambda ()
                          (let ((var.lev (cdr b)))
                            (gen-ref src (car var.lev) (cdr var.lev) maps)))
                        (lambda (var maps) (values (list 'ref var) maps))))
                     ((ellipsis? e) (syntax-violation 'syntax "misplaced ellipsis" src))
                     (else (values (list 'quote e) maps))))
             (let* ((tmp e) (tmp-1 ($sc-dispatch tmp '(any any))))
               (if (and tmp-1 (apply (lambda (dots e) (ellipsis? dots)) tmp-1))
                 (apply (lambda (dots e) (gen-syntax src e r maps (lambda (x) #f) mod))
                        tmp-1)
                 (let ((tmp-1 ($sc-dispatch tmp '(any any . any))))
                   (if (and tmp-1 (apply (lambda (x dots y) (ellipsis? dots)) tmp-1))
                     (apply (lambda (x dots y)
                              (let f ((y y)
                                      (k (lambda (maps)
                                           (call-with-values
                                             (lambda () (gen-syntax src x r (cons '() maps) ellipsis? mod))
                                             (lambda (x maps)
                                               (if (null? (car maps))
                                                 (syntax-violation 'syntax "extra ellipsis" src)
                                                 (values (gen-map x (car maps)) (cdr maps))))))))
                                (let* ((tmp y) (tmp ($sc-dispatch tmp '(any . any))))
                                  (if (and tmp (apply (lambda (dots y) (ellipsis? dots)) tmp))
                                    (apply (lambda (dots y)
                                             (f y
                                                (lambda (maps)
                                                  (call-with-values
                                                    (lambda () (k (cons '() maps)))
                                                    (lambda (x maps)
                                                      (if (null? (car maps))
                                                        (syntax-violation 'syntax "extra ellipsis" src)
                                                        (values (gen-mappend x (car maps)) (cdr maps))))))))
                                           tmp)
                                    (call-with-values
                                      (lambda () (gen-syntax src y r maps ellipsis? mod))
                                      (lambda (y maps)
                                        (call-with-values
                                          (lambda () (k maps))
                                          (lambda (x maps) (values (gen-append x y) maps)))))))))
                            tmp-1)
                     (let ((tmp-1 ($sc-dispatch tmp '(any . any))))
                       (if tmp-1
                         (apply (lambda (x y)
                                  (call-with-values
                                    (lambda () (gen-syntax src x r maps ellipsis? mod))
                                    (lambda (x maps)
                                      (call-with-values
                                        (lambda () (gen-syntax src y r maps ellipsis? mod))
                                        (lambda (y maps) (values (gen-cons x y) maps))))))
                                tmp-1)
                         (let ((tmp ($sc-dispatch tmp '#(vector (any . each-any)))))
                           (if tmp
                             (apply (lambda (e1 e2)
                                      (call-with-values
                                        (lambda () (gen-syntax src (cons e1 e2) r maps ellipsis? mod))
                                        (lambda (e maps) (values (gen-vector e) maps))))
                                    tmp)
                             (values (list 'quote e) maps))))))))))))
       (gen-ref
         (lambda (src var level maps)
           (cond ((= level 0) (values var maps))
                 ((null? maps) (syntax-violation 'syntax "missing ellipsis" src))
                 (else
                  (call-with-values
                    (lambda () (gen-ref src var (- level 1) (cdr maps)))
                    (lambda (outer-var outer-maps)
                      (let ((b (assq outer-var (car maps))))
                        (if b
                          (values (cdr b) maps)
                          (let ((inner-var (gen-var 'tmp)))
                            (values
                              inner-var
                              (cons (cons (cons outer-var inner-var) (car maps)) outer-maps)))))))))))
       (gen-mappend
         (lambda (e map-env)
           (list 'apply '(primitive append) (gen-map e map-env))))
       (gen-map
         (lambda (e map-env)
           (let ((formals (map cdr map-env))
                 (actuals (map (lambda (x) (list 'ref (car x))) map-env)))
             (cond ((eq? (car e) 'ref) (car actuals))
                   ((and-map
                      (lambda (x) (and (eq? (car x) 'ref) (memq (cadr x) formals)))
                      (cdr e))
                    (cons 'map
                          (cons (list 'primitive (car e))
                                (map (let ((r (map cons formals actuals)))
                                       (lambda (x) (cdr (assq (cadr x) r))))
                                     (cdr e)))))
                   (else (cons 'map (cons (list 'lambda formals e) actuals)))))))
       (gen-cons
         (lambda (x y)
           (let ((key (car y)))
             (cond ((memv key '(quote))
                    (cond ((eq? (car x) 'quote) (list 'quote (cons (cadr x) (cadr y))))
                          ((eq? (cadr y) '()) (list 'list x))
                          (else (list 'cons x y))))
                   ((memv key '(list)) (cons 'list (cons x (cdr y))))
                   (else (list 'cons x y))))))
       (gen-append (lambda (x y) (if (equal? y ''()) x (list 'append x y))))
       (gen-vector
         (lambda (x)
           (cond ((eq? (car x) 'list) (cons 'vector (cdr x)))
                 ((eq? (car x) 'quote) (list 'quote (list->vector (cadr x))))
                 (else (list 'list->vector x)))))
       (regen (lambda (x)
                (let ((key (car x)))
                  (cond ((memv key '(ref))
                         (build-lexical-reference 'value #f (cadr x) (cadr x)))
                        ((memv key '(primitive)) (build-primref #f (cadr x)))
                        ((memv key '(quote)) (build-data #f (cadr x)))
                        ((memv key '(lambda))
                         (if (list? (cadr x))
                           (build-simple-lambda #f (cadr x) #f (cadr x) '() (regen (caddr x)))
                           (error "how did we get here" x)))
                        (else
                         (build-application #f (build-primref #f (car x)) (map regen (cdr x)))))))))
      (lambda (e r w s mod)
        (let* ((e (source-wrap e w s mod))
               (tmp e)
               (tmp ($sc-dispatch tmp '(_ any))))
          (if tmp
            (apply (lambda (x)
                     (call-with-values
                       (lambda () (gen-syntax e x r '() ellipsis? mod))
                       (lambda (e maps) (regen e))))
                   tmp)
            (syntax-violation 'syntax "bad `syntax' form" e))))))
  (global-extend
    'core
    'lambda
    (lambda (e r w s mod)
      (let* ((tmp e) (tmp ($sc-dispatch tmp '(_ any any . each-any))))
        (if tmp
          (apply (lambda (args e1 e2)
                   (call-with-values
                     (lambda () (lambda-formals args))
                     (lambda (req opt rest kw)
                       (let lp ((body (cons e1 e2)) (meta '()))
                         (let* ((tmp-1 body) (tmp ($sc-dispatch tmp-1 '(any any . each-any))))
                           (if (and tmp
                                    (apply (lambda (docstring e1 e2) (string? (syntax->datum docstring)))
                                           tmp))
                             (apply (lambda (docstring e1 e2)
                                      (lp (cons e1 e2)
                                          (append meta (list (cons 'documentation (syntax->datum docstring))))))
                                    tmp)
                             (let ((tmp ($sc-dispatch tmp-1 '(#(vector #(each (any . any))) any . each-any))))
                               (if tmp
                                 (apply (lambda (k v e1 e2)
                                          (lp (cons e1 e2) (append meta (syntax->datum (map cons k v)))))
                                        tmp)
                                 (expand-simple-lambda e r w s mod req rest meta body)))))))))
                 tmp)
          (syntax-violation 'lambda "bad lambda" e)))))
  (global-extend
    'core
    'lambda*
    (lambda (e r w s mod)
      (let* ((tmp e) (tmp ($sc-dispatch tmp '(_ any any . each-any))))
        (if tmp
          (apply (lambda (args e1 e2)
                   (call-with-values
                     (lambda ()
                       (expand-lambda-case
                         e
                         r
                         w
                         s
                         mod
                         lambda*-formals
                         (list (cons args (cons e1 e2)))))
                     (lambda (meta lcase) (build-case-lambda s meta lcase))))
                 tmp)
          (syntax-violation 'lambda "bad lambda*" e)))))
  (global-extend
    'core
    'case-lambda
    (lambda (e r w s mod)
      (let* ((tmp e)
             (tmp ($sc-dispatch
                    tmp
                    '(_ (any any . each-any) . #(each (any any . each-any))))))
        (if tmp
          (apply (lambda (args e1 e2 args* e1* e2*)
                   (call-with-values
                     (lambda ()
                       (expand-lambda-case
                         e
                         r
                         w
                         s
                         mod
                         lambda-formals
                         (cons (cons args (cons e1 e2))
                               (map (lambda (tmp-2 tmp-1 tmp) (cons tmp (cons tmp-1 tmp-2)))
                                    e2*
                                    e1*
                                    args*))))
                     (lambda (meta lcase) (build-case-lambda s meta lcase))))
                 tmp)
          (syntax-violation 'case-lambda "bad case-lambda" e)))))
  (global-extend
    'core
    'case-lambda*
    (lambda (e r w s mod)
      (let* ((tmp e)
             (tmp ($sc-dispatch
                    tmp
                    '(_ (any any . each-any) . #(each (any any . each-any))))))
        (if tmp
          (apply (lambda (args e1 e2 args* e1* e2*)
                   (call-with-values
                     (lambda ()
                       (expand-lambda-case
                         e
                         r
                         w
                         s
                         mod
                         lambda*-formals
                         (cons (cons args (cons e1 e2))
                               (map (lambda (tmp-2 tmp-1 tmp) (cons tmp (cons tmp-1 tmp-2)))
                                    e2*
                                    e1*
                                    args*))))
                     (lambda (meta lcase) (build-case-lambda s meta lcase))))
                 tmp)
          (syntax-violation 'case-lambda "bad case-lambda*" e)))))
  (global-extend
    'core
    'let
    (letrec*
      ((expand-let
         (lambda (e r w s mod constructor ids vals exps)
           (if (not (valid-bound-ids? ids))
             (syntax-violation 'let "duplicate bound variable" e)
             (let ((labels (gen-labels ids)) (new-vars (map gen-var ids)))
               (let ((nw (make-binding-wrap ids labels w))
                     (nr (extend-var-env labels new-vars r)))
                 (constructor
                   s
                   (map syntax->datum ids)
                   new-vars
                   (map (lambda (x) (expand x r w mod)) vals)
                   (expand-body exps (source-wrap e nw s mod) nr nw mod))))))))
      (lambda (e r w s mod)
        (let* ((tmp-1 e)
               (tmp ($sc-dispatch tmp-1 '(_ #(each (any any)) any . each-any))))
          (if (and tmp (apply (lambda (id val e1 e2) (and-map id? id)) tmp))
            (apply (lambda (id val e1 e2)
                     (expand-let e r w s mod build-let id val (cons e1 e2)))
                   tmp)
            (let ((tmp ($sc-dispatch tmp-1 '(_ any #(each (any any)) any . each-any))))
              (if (and tmp
                       (apply (lambda (f id val e1 e2) (and (id? f) (and-map id? id))) tmp))
                (apply (lambda (f id val e1 e2)
                         (expand-let e r w s mod build-named-let (cons f id) val (cons e1 e2)))
                       tmp)
                (syntax-violation 'let "bad let" (source-wrap e w s mod)))))))))
  (global-extend
    'core
    'letrec
    (lambda (e r w s mod)
      (let* ((tmp e)
             (tmp ($sc-dispatch tmp '(_ #(each (any any)) any . each-any))))
        (if (and tmp (apply (lambda (id val e1 e2) (and-map id? id)) tmp))
          (apply (lambda (id val e1 e2)
                   (let ((ids id))
                     (if (not (valid-bound-ids? ids))
                       (syntax-violation 'letrec "duplicate bound variable" e)
                       (let ((labels (gen-labels ids)) (new-vars (map gen-var ids)))
                         (let ((w (make-binding-wrap ids labels w))
                               (r (extend-var-env labels new-vars r)))
                           (build-letrec
                             s
                             #f
                             (map syntax->datum ids)
                             new-vars
                             (map (lambda (x) (expand x r w mod)) val)
                             (expand-body (cons e1 e2) (source-wrap e w s mod) r w mod)))))))
                 tmp)
          (syntax-violation 'letrec "bad letrec" (source-wrap e w s mod))))))
  (global-extend
    'core
    'letrec*
    (lambda (e r w s mod)
      (let* ((tmp e)
             (tmp ($sc-dispatch tmp '(_ #(each (any any)) any . each-any))))
        (if (and tmp (apply (lambda (id val e1 e2) (and-map id? id)) tmp))
          (apply (lambda (id val e1 e2)
                   (let ((ids id))
                     (if (not (valid-bound-ids? ids))
                       (syntax-violation 'letrec* "duplicate bound variable" e)
                       (let ((labels (gen-labels ids)) (new-vars (map gen-var ids)))
                         (let ((w (make-binding-wrap ids labels w))
                               (r (extend-var-env labels new-vars r)))
                           (build-letrec
                             s
                             #t
                             (map syntax->datum ids)
                             new-vars
                             (map (lambda (x) (expand x r w mod)) val)
                             (expand-body (cons e1 e2) (source-wrap e w s mod) r w mod)))))))
                 tmp)
          (syntax-violation 'letrec* "bad letrec*" (source-wrap e w s mod))))))
  (global-extend
    'core
    'set!
    (lambda (e r w s mod)
      (let* ((tmp-1 e) (tmp ($sc-dispatch tmp-1 '(_ any any))))
        (if (and tmp (apply (lambda (id val) (id? id)) tmp))
          (apply (lambda (id val)
                   (let ((n (id-var-name id w))
                         (id-mod (if (syntax-object? id) (syntax-object-module id) mod)))
                     (let* ((b (lookup n r id-mod)) (key (car b)))
                       (cond ((memv key '(lexical))
                              (build-lexical-assignment
                                s
                                (syntax->datum id)
                                (cdr b)
                                (expand val r w mod)))
                             ((memv key '(global))
                              (build-global-assignment s n (expand val r w mod) id-mod))
                             ((memv key '(macro))
                              (let ((p (cdr b)))
                                (if (procedure-property p 'variable-transformer)
                                  (expand (expand-macro p e r w s #f mod) r '(()) mod)
                                  (syntax-violation
                                    'set!
                                    "not a variable transformer"
                                    (wrap e w mod)
                                    (wrap id w id-mod)))))
                             ((memv key '(displaced-lexical))
                              (syntax-violation 'set! "identifier out of context" (wrap id w mod)))
                             (else (syntax-violation 'set! "bad set!" (source-wrap e w s mod)))))))
                 tmp)
          (let ((tmp ($sc-dispatch tmp-1 '(_ (any . each-any) any))))
            (if tmp
              (apply (lambda (head tail val)
                       (call-with-values
                         (lambda () (syntax-type head r '(()) #f #f mod #t))
                         (lambda (type value formform ee ww ss modmod)
                           (let ((key type))
                             (if (memv key '(module-ref))
                               (let ((val (expand val r w mod)))
                                 (call-with-values
                                   (lambda () (value (cons head tail) r w))
                                   (lambda (e r w s* mod)
                                     (let* ((tmp-1 e) (tmp (list tmp-1)))
                                       (if (and tmp (apply (lambda (e) (id? e)) tmp))
                                         (apply (lambda (e) (build-global-assignment s (syntax->datum e) val mod))
                                                tmp)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           tmp-1))))))
                               (build-application
                                 s
                                 (expand
                                   (list '#(syntax-object
                                            setter
                                            ((top)
                                             #(ribcage () () ())
                                             #(ribcage #(key) #((m-*-3526 top)) #("l-*-3527"))
                                             #(ribcage () () ())
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(type value formform ee ww ss modmod)
                                               #((top) (top) (top) (top) (top) (top) (top))
                                               #("l-*-3519"
                                                 "l-*-3520"
                                                 "l-*-3521"
                                                 "l-*-3522"
                                                 "l-*-3523"
                                                 "l-*-3524"
                                                 "l-*-3525"))
                                             #(ribcage
                                               #(head tail val)
                                               #((top) (top) (top))
                                               #("l-*-3504" "l-*-3505" "l-*-3506"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(e r w s mod)
                                               #((top) (top) (top) (top) (top))
                                               #("l-*-3473" "l-*-3474" "l-*-3475" "l-*-3476" "l-*-3477"))
                                             #(ribcage
                                               (lambda-var-list
                                                 gen-var
                                                 strip
                                                 expand-lambda-case
                                                 lambda*-formals
                                                 expand-simple-lambda
                                                 lambda-formals
                                                 ellipsis?
                                                 expand-void
                                                 eval-local-transformer
                                                 expand-local-syntax
                                                 expand-body
                                                 expand-macro
                                                 expand-application
                                                 expand-expr
                                                 expand
                                                 syntax-type
                                                 parse-when-list
                                                 expand-install-global
                                                 expand-top-sequence
                                                 expand-sequence
                                                 source-wrap
                                                 wrap
                                                 bound-id-member?
                                                 distinct-bound-ids?
                                                 valid-bound-ids?
                                                 bound-id=?
                                                 free-id=?
                                                 with-transformer-environment
                                                 transformer-environment
                                                 resolve-identifier
                                                 locally-bound-identifiers
                                                 id-var-name
                                                 same-marks?
                                                 join-marks
                                                 join-wraps
                                                 smart-append
                                                 make-binding-wrap
                                                 extend-ribcage!
                                                 make-empty-ribcage
                                                 new-mark
                                                 anti-mark
                                                 the-anti-mark
                                                 top-marked?
                                                 top-wrap
                                                 empty-wrap
                                                 set-ribcage-labels!
                                                 set-ribcage-marks!
                                                 set-ribcage-symnames!
                                                 ribcage-labels
                                                 ribcage-marks
                                                 ribcage-symnames
                                                 ribcage?
                                                 make-ribcage
                                                 gen-labels
                                                 gen-label
                                                 make-rename
                                                 rename-marks
                                                 rename-new
                                                 rename-old
                                                 subst-rename?
                                                 wrap-subst
                                                 wrap-marks
                                                 make-wrap
                                                 id-sym-name&marks
                                                 id-sym-name
                                                 id?
                                                 nonsymbol-id?
                                                 global-extend
                                                 lookup
                                                 macros-only-env
                                                 extend-var-env
                                                 extend-env
                                                 null-env
                                                 binding-value
                                                 binding-type
                                                 make-binding
                                                 arg-check
                                                 source-annotation
                                                 no-source
                                                 set-syntax-object-module!
                                                 set-syntax-object-wrap!
                                                 set-syntax-object-expression!
                                                 syntax-object-module
                                                 syntax-object-wrap
                                                 syntax-object-expression
                                                 syntax-object?
                                                 make-syntax-object
                                                 build-lexical-var
                                                 build-letrec
                                                 build-named-let
                                                 build-let
                                                 build-sequence
                                                 build-data
                                                 build-primref
                                                 build-lambda-case
                                                 build-case-lambda
                                                 build-simple-lambda
                                                 build-global-definition
                                                 build-global-assignment
                                                 build-global-reference
                                                 analyze-variable
                                                 build-lexical-assignment
                                                 build-lexical-reference
                                                 build-dynlet
                                                 build-conditional
                                                 build-application
                                                 build-void
                                                 maybe-name-value!
                                                 decorate-source
                                                 get-global-definition-hook
                                                 put-global-definition-hook
                                                 session-id
                                                 local-eval-hook
                                                 top-level-eval-hook
                                                 fx<
                                                 fx=
                                                 fx-
                                                 fx+
                                                 set-lambda-meta!
                                                 lambda-meta
                                                 lambda?
                                                 make-dynlet
                                                 make-letrec
                                                 make-let
                                                 make-lambda-case
                                                 make-lambda
                                                 make-sequence
                                                 make-application
                                                 make-conditional
                                                 make-toplevel-define
                                                 make-toplevel-set
                                                 make-toplevel-ref
                                                 make-module-set
                                                 make-module-ref
                                                 make-lexical-set
                                                 make-lexical-ref
                                                 make-primitive-ref
                                                 make-const
                                                 make-void)
                                               ((top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top))
                                               ("l-*-476"
                                                "l-*-474"
                                                "l-*-472"
                                                "l-*-470"
                                                "l-*-468"
                                                "l-*-466"
                                                "l-*-464"
                                                "l-*-462"
                                                "l-*-460"
                                                "l-*-458"
                                                "l-*-456"
                                                "l-*-454"
                                                "l-*-452"
                                                "l-*-450"
                                                "l-*-448"
                                                "l-*-446"
                                                "l-*-444"
                                                "l-*-442"
                                                "l-*-440"
                                                "l-*-438"
                                                "l-*-436"
                                                "l-*-434"
                                                "l-*-432"
                                                "l-*-430"
                                                "l-*-428"
                                                "l-*-426"
                                                "l-*-424"
                                                "l-*-422"
                                                "l-*-420"
                                                "l-*-418"
                                                "l-*-416"
                                                "l-*-414"
                                                "l-*-412"
                                                "l-*-410"
                                                "l-*-408"
                                                "l-*-406"
                                                "l-*-404"
                                                "l-*-402"
                                                "l-*-400"
                                                "l-*-399"
                                                "l-*-397"
                                                "l-*-394"
                                                "l-*-393"
                                                "l-*-392"
                                                "l-*-390"
                                                "l-*-389"
                                                "l-*-387"
                                                "l-*-385"
                                                "l-*-383"
                                                "l-*-381"
                                                "l-*-379"
                                                "l-*-377"
                                                "l-*-375"
                                                "l-*-373"
                                                "l-*-370"
                                                "l-*-368"
                                                "l-*-367"
                                                "l-*-365"
                                                "l-*-363"
                                                "l-*-361"
                                                "l-*-359"
                                                "l-*-358"
                                                "l-*-357"
                                                "l-*-356"
                                                "l-*-354"
                                                "l-*-353"
                                                "l-*-350"
                                                "l-*-348"
                                                "l-*-346"
                                                "l-*-344"
                                                "l-*-342"
                                                "l-*-340"
                                                "l-*-338"
                                                "l-*-337"
                                                "l-*-336"
                                                "l-*-334"
                                                "l-*-332"
                                                "l-*-331"
                                                "l-*-328"
                                                "l-*-327"
                                                "l-*-325"
                                                "l-*-323"
                                                "l-*-321"
                                                "l-*-319"
                                                "l-*-317"
                                                "l-*-315"
                                                "l-*-313"
                                                "l-*-311"
                                                "l-*-309"
                                                "l-*-306"
                                                "l-*-304"
                                                "l-*-302"
                                                "l-*-300"
                                                "l-*-298"
                                                "l-*-296"
                                                "l-*-294"
                                                "l-*-292"
                                                "l-*-290"
                                                "l-*-288"
                                                "l-*-286"
                                                "l-*-284"
                                                "l-*-282"
                                                "l-*-280"
                                                "l-*-278"
                                                "l-*-276"
                                                "l-*-274"
                                                "l-*-272"
                                                "l-*-270"
                                                "l-*-268"
                                                "l-*-266"
                                                "l-*-264"
                                                "l-*-262"
                                                "l-*-260"
                                                "l-*-258"
                                                "l-*-256"
                                                "l-*-255"
                                                "l-*-254"
                                                "l-*-253"
                                                "l-*-252"
                                                "l-*-250"
                                                "l-*-248"
                                                "l-*-246"
                                                "l-*-243"
                                                "l-*-241"
                                                "l-*-239"
                                                "l-*-237"
                                                "l-*-235"
                                                "l-*-233"
                                                "l-*-231"
                                                "l-*-229"
                                                "l-*-227"
                                                "l-*-225"
                                                "l-*-223"
                                                "l-*-221"
                                                "l-*-219"
                                                "l-*-217"
                                                "l-*-215"
                                                "l-*-213"
                                                "l-*-211"
                                                "l-*-209"))
                                             #(ribcage
                                               (define-structure
                                                 define-expansion-accessors
                                                 define-expansion-constructors)
                                               ((top) (top) (top))
                                               ("l-*-47" "l-*-46" "l-*-45")))
                                            (hygiene guile))
                                         head)
                                   r
                                   w
                                   mod)
                                 (map (lambda (e) (expand e r w mod)) (append tail (list val)))))))))
                     tmp)
              (syntax-violation 'set! "bad set!" (source-wrap e w s mod))))))))
  (global-extend
    'module-ref
    '@
    (lambda (e r w)
      (let* ((tmp-1 e) (tmp ($sc-dispatch tmp-1 '(_ each-any any))))
        (if (and tmp
                 (apply (lambda (mod id) (and (and-map id? mod) (id? id))) tmp))
          (apply (lambda (mod id)
                   (values
                     (syntax->datum id)
                     r
                     w
                     #f
                     (syntax->datum
                       (cons '#(syntax-object
                                public
                                ((top)
                                 #(ribcage #(mod id) #((top) (top)) #("l-*-3566" "l-*-3567"))
                                 #(ribcage () () ())
                                 #(ribcage
                                   #(e r w)
                                   #((top) (top) (top))
                                   #("l-*-3554" "l-*-3555" "l-*-3556"))
                                 #(ribcage
                                   (lambda-var-list
                                     gen-var
                                     strip
                                     expand-lambda-case
                                     lambda*-formals
                                     expand-simple-lambda
                                     lambda-formals
                                     ellipsis?
                                     expand-void
                                     eval-local-transformer
                                     expand-local-syntax
                                     expand-body
                                     expand-macro
                                     expand-application
                                     expand-expr
                                     expand
                                     syntax-type
                                     parse-when-list
                                     expand-install-global
                                     expand-top-sequence
                                     expand-sequence
                                     source-wrap
                                     wrap
                                     bound-id-member?
                                     distinct-bound-ids?
                                     valid-bound-ids?
                                     bound-id=?
                                     free-id=?
                                     with-transformer-environment
                                     transformer-environment
                                     resolve-identifier
                                     locally-bound-identifiers
                                     id-var-name
                                     same-marks?
                                     join-marks
                                     join-wraps
                                     smart-append
                                     make-binding-wrap
                                     extend-ribcage!
                                     make-empty-ribcage
                                     new-mark
                                     anti-mark
                                     the-anti-mark
                                     top-marked?
                                     top-wrap
                                     empty-wrap
                                     set-ribcage-labels!
                                     set-ribcage-marks!
                                     set-ribcage-symnames!
                                     ribcage-labels
                                     ribcage-marks
                                     ribcage-symnames
                                     ribcage?
                                     make-ribcage
                                     gen-labels
                                     gen-label
                                     make-rename
                                     rename-marks
                                     rename-new
                                     rename-old
                                     subst-rename?
                                     wrap-subst
                                     wrap-marks
                                     make-wrap
                                     id-sym-name&marks
                                     id-sym-name
                                     id?
                                     nonsymbol-id?
                                     global-extend
                                     lookup
                                     macros-only-env
                                     extend-var-env
                                     extend-env
                                     null-env
                                     binding-value
                                     binding-type
                                     make-binding
                                     arg-check
                                     source-annotation
                                     no-source
                                     set-syntax-object-module!
                                     set-syntax-object-wrap!
                                     set-syntax-object-expression!
                                     syntax-object-module
                                     syntax-object-wrap
                                     syntax-object-expression
                                     syntax-object?
                                     make-syntax-object
                                     build-lexical-var
                                     build-letrec
                                     build-named-let
                                     build-let
                                     build-sequence
                                     build-data
                                     build-primref
                                     build-lambda-case
                                     build-case-lambda
                                     build-simple-lambda
                                     build-global-definition
                                     build-global-assignment
                                     build-global-reference
                                     analyze-variable
                                     build-lexical-assignment
                                     build-lexical-reference
                                     build-dynlet
                                     build-conditional
                                     build-application
                                     build-void
                                     maybe-name-value!
                                     decorate-source
                                     get-global-definition-hook
                                     put-global-definition-hook
                                     session-id
                                     local-eval-hook
                                     top-level-eval-hook
                                     fx<
                                     fx=
                                     fx-
                                     fx+
                                     set-lambda-meta!
                                     lambda-meta
                                     lambda?
                                     make-dynlet
                                     make-letrec
                                     make-let
                                     make-lambda-case
                                     make-lambda
                                     make-sequence
                                     make-application
                                     make-conditional
                                     make-toplevel-define
                                     make-toplevel-set
                                     make-toplevel-ref
                                     make-module-set
                                     make-module-ref
                                     make-lexical-set
                                     make-lexical-ref
                                     make-primitive-ref
                                     make-const
                                     make-void)
                                   ((top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top)
                                    (top))
                                   ("l-*-476"
                                    "l-*-474"
                                    "l-*-472"
                                    "l-*-470"
                                    "l-*-468"
                                    "l-*-466"
                                    "l-*-464"
                                    "l-*-462"
                                    "l-*-460"
                                    "l-*-458"
                                    "l-*-456"
                                    "l-*-454"
                                    "l-*-452"
                                    "l-*-450"
                                    "l-*-448"
                                    "l-*-446"
                                    "l-*-444"
                                    "l-*-442"
                                    "l-*-440"
                                    "l-*-438"
                                    "l-*-436"
                                    "l-*-434"
                                    "l-*-432"
                                    "l-*-430"
                                    "l-*-428"
                                    "l-*-426"
                                    "l-*-424"
                                    "l-*-422"
                                    "l-*-420"
                                    "l-*-418"
                                    "l-*-416"
                                    "l-*-414"
                                    "l-*-412"
                                    "l-*-410"
                                    "l-*-408"
                                    "l-*-406"
                                    "l-*-404"
                                    "l-*-402"
                                    "l-*-400"
                                    "l-*-399"
                                    "l-*-397"
                                    "l-*-394"
                                    "l-*-393"
                                    "l-*-392"
                                    "l-*-390"
                                    "l-*-389"
                                    "l-*-387"
                                    "l-*-385"
                                    "l-*-383"
                                    "l-*-381"
                                    "l-*-379"
                                    "l-*-377"
                                    "l-*-375"
                                    "l-*-373"
                                    "l-*-370"
                                    "l-*-368"
                                    "l-*-367"
                                    "l-*-365"
                                    "l-*-363"
                                    "l-*-361"
                                    "l-*-359"
                                    "l-*-358"
                                    "l-*-357"
                                    "l-*-356"
                                    "l-*-354"
                                    "l-*-353"
                                    "l-*-350"
                                    "l-*-348"
                                    "l-*-346"
                                    "l-*-344"
                                    "l-*-342"
                                    "l-*-340"
                                    "l-*-338"
                                    "l-*-337"
                                    "l-*-336"
                                    "l-*-334"
                                    "l-*-332"
                                    "l-*-331"
                                    "l-*-328"
                                    "l-*-327"
                                    "l-*-325"
                                    "l-*-323"
                                    "l-*-321"
                                    "l-*-319"
                                    "l-*-317"
                                    "l-*-315"
                                    "l-*-313"
                                    "l-*-311"
                                    "l-*-309"
                                    "l-*-306"
                                    "l-*-304"
                                    "l-*-302"
                                    "l-*-300"
                                    "l-*-298"
                                    "l-*-296"
                                    "l-*-294"
                                    "l-*-292"
                                    "l-*-290"
                                    "l-*-288"
                                    "l-*-286"
                                    "l-*-284"
                                    "l-*-282"
                                    "l-*-280"
                                    "l-*-278"
                                    "l-*-276"
                                    "l-*-274"
                                    "l-*-272"
                                    "l-*-270"
                                    "l-*-268"
                                    "l-*-266"
                                    "l-*-264"
                                    "l-*-262"
                                    "l-*-260"
                                    "l-*-258"
                                    "l-*-256"
                                    "l-*-255"
                                    "l-*-254"
                                    "l-*-253"
                                    "l-*-252"
                                    "l-*-250"
                                    "l-*-248"
                                    "l-*-246"
                                    "l-*-243"
                                    "l-*-241"
                                    "l-*-239"
                                    "l-*-237"
                                    "l-*-235"
                                    "l-*-233"
                                    "l-*-231"
                                    "l-*-229"
                                    "l-*-227"
                                    "l-*-225"
                                    "l-*-223"
                                    "l-*-221"
                                    "l-*-219"
                                    "l-*-217"
                                    "l-*-215"
                                    "l-*-213"
                                    "l-*-211"
                                    "l-*-209"))
                                 #(ribcage
                                   (define-structure
                                     define-expansion-accessors
                                     define-expansion-constructors)
                                   ((top) (top) (top))
                                   ("l-*-47" "l-*-46" "l-*-45")))
                                (hygiene guile))
                             mod))))
                 tmp)
          (syntax-violation
            #f
            "source expression failed to match any pattern"
            tmp-1)))))
  (global-extend
    'module-ref
    '@@
    (lambda (e r w)
      (letrec*
        ((remodulate
           (lambda (x mod)
             (cond ((pair? x) (cons (remodulate (car x) mod) (remodulate (cdr x) mod)))
                   ((syntax-object? x)
                    (make-syntax-object
                      (remodulate (syntax-object-expression x) mod)
                      (syntax-object-wrap x)
                      mod))
                   ((vector? x)
                    (let* ((n (vector-length x)) (v (make-vector n)))
                      (let loop ((i 0))
                        (if (= i n)
                          (begin (if #f #f) v)
                          (begin
                            (vector-set! v i (remodulate (vector-ref x i) mod))
                            (loop (+ i 1)))))))
                   (else x)))))
        (let* ((tmp-1 e) (tmp ($sc-dispatch tmp-1 '(_ each-any any))))
          (if (and tmp (apply (lambda (mod exp) (and-map id? mod)) tmp))
            (apply (lambda (mod exp)
                     (let ((mod (syntax->datum
                                  (cons '#(syntax-object
                                           private
                                           ((top)
                                            #(ribcage #(mod exp) #((top) (top)) #("l-*-3604" "l-*-3605"))
                                            #(ribcage (remodulate) ((top)) ("l-*-3577"))
                                            #(ribcage
                                              #(e r w)
                                              #((top) (top) (top))
                                              #("l-*-3574" "l-*-3575" "l-*-3576"))
                                            #(ribcage
                                              (lambda-var-list
                                                gen-var
                                                strip
                                                expand-lambda-case
                                                lambda*-formals
                                                expand-simple-lambda
                                                lambda-formals
                                                ellipsis?
                                                expand-void
                                                eval-local-transformer
                                                expand-local-syntax
                                                expand-body
                                                expand-macro
                                                expand-application
                                                expand-expr
                                                expand
                                                syntax-type
                                                parse-when-list
                                                expand-install-global
                                                expand-top-sequence
                                                expand-sequence
                                                source-wrap
                                                wrap
                                                bound-id-member?
                                                distinct-bound-ids?
                                                valid-bound-ids?
                                                bound-id=?
                                                free-id=?
                                                with-transformer-environment
                                                transformer-environment
                                                resolve-identifier
                                                locally-bound-identifiers
                                                id-var-name
                                                same-marks?
                                                join-marks
                                                join-wraps
                                                smart-append
                                                make-binding-wrap
                                                extend-ribcage!
                                                make-empty-ribcage
                                                new-mark
                                                anti-mark
                                                the-anti-mark
                                                top-marked?
                                                top-wrap
                                                empty-wrap
                                                set-ribcage-labels!
                                                set-ribcage-marks!
                                                set-ribcage-symnames!
                                                ribcage-labels
                                                ribcage-marks
                                                ribcage-symnames
                                                ribcage?
                                                make-ribcage
                                                gen-labels
                                                gen-label
                                                make-rename
                                                rename-marks
                                                rename-new
                                                rename-old
                                                subst-rename?
                                                wrap-subst
                                                wrap-marks
                                                make-wrap
                                                id-sym-name&marks
                                                id-sym-name
                                                id?
                                                nonsymbol-id?
                                                global-extend
                                                lookup
                                                macros-only-env
                                                extend-var-env
                                                extend-env
                                                null-env
                                                binding-value
                                                binding-type
                                                make-binding
                                                arg-check
                                                source-annotation
                                                no-source
                                                set-syntax-object-module!
                                                set-syntax-object-wrap!
                                                set-syntax-object-expression!
                                                syntax-object-module
                                                syntax-object-wrap
                                                syntax-object-expression
                                                syntax-object?
                                                make-syntax-object
                                                build-lexical-var
                                                build-letrec
                                                build-named-let
                                                build-let
                                                build-sequence
                                                build-data
                                                build-primref
                                                build-lambda-case
                                                build-case-lambda
                                                build-simple-lambda
                                                build-global-definition
                                                build-global-assignment
                                                build-global-reference
                                                analyze-variable
                                                build-lexical-assignment
                                                build-lexical-reference
                                                build-dynlet
                                                build-conditional
                                                build-application
                                                build-void
                                                maybe-name-value!
                                                decorate-source
                                                get-global-definition-hook
                                                put-global-definition-hook
                                                session-id
                                                local-eval-hook
                                                top-level-eval-hook
                                                fx<
                                                fx=
                                                fx-
                                                fx+
                                                set-lambda-meta!
                                                lambda-meta
                                                lambda?
                                                make-dynlet
                                                make-letrec
                                                make-let
                                                make-lambda-case
                                                make-lambda
                                                make-sequence
                                                make-application
                                                make-conditional
                                                make-toplevel-define
                                                make-toplevel-set
                                                make-toplevel-ref
                                                make-module-set
                                                make-module-ref
                                                make-lexical-set
                                                make-lexical-ref
                                                make-primitive-ref
                                                make-const
                                                make-void)
                                              ((top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top))
                                              ("l-*-476"
                                               "l-*-474"
                                               "l-*-472"
                                               "l-*-470"
                                               "l-*-468"
                                               "l-*-466"
                                               "l-*-464"
                                               "l-*-462"
                                               "l-*-460"
                                               "l-*-458"
                                               "l-*-456"
                                               "l-*-454"
                                               "l-*-452"
                                               "l-*-450"
                                               "l-*-448"
                                               "l-*-446"
                                               "l-*-444"
                                               "l-*-442"
                                               "l-*-440"
                                               "l-*-438"
                                               "l-*-436"
                                               "l-*-434"
                                               "l-*-432"
                                               "l-*-430"
                                               "l-*-428"
                                               "l-*-426"
                                               "l-*-424"
                                               "l-*-422"
                                               "l-*-420"
                                               "l-*-418"
                                               "l-*-416"
                                               "l-*-414"
                                               "l-*-412"
                                               "l-*-410"
                                               "l-*-408"
                                               "l-*-406"
                                               "l-*-404"
                                               "l-*-402"
                                               "l-*-400"
                                               "l-*-399"
                                               "l-*-397"
                                               "l-*-394"
                                               "l-*-393"
                                               "l-*-392"
                                               "l-*-390"
                                               "l-*-389"
                                               "l-*-387"
                                               "l-*-385"
                                               "l-*-383"
                                               "l-*-381"
                                               "l-*-379"
                                               "l-*-377"
                                               "l-*-375"
                                               "l-*-373"
                                               "l-*-370"
                                               "l-*-368"
                                               "l-*-367"
                                               "l-*-365"
                                               "l-*-363"
                                               "l-*-361"
                                               "l-*-359"
                                               "l-*-358"
                                               "l-*-357"
                                               "l-*-356"
                                               "l-*-354"
                                               "l-*-353"
                                               "l-*-350"
                                               "l-*-348"
                                               "l-*-346"
                                               "l-*-344"
                                               "l-*-342"
                                               "l-*-340"
                                               "l-*-338"
                                               "l-*-337"
                                               "l-*-336"
                                               "l-*-334"
                                               "l-*-332"
                                               "l-*-331"
                                               "l-*-328"
                                               "l-*-327"
                                               "l-*-325"
                                               "l-*-323"
                                               "l-*-321"
                                               "l-*-319"
                                               "l-*-317"
                                               "l-*-315"
                                               "l-*-313"
                                               "l-*-311"
                                               "l-*-309"
                                               "l-*-306"
                                               "l-*-304"
                                               "l-*-302"
                                               "l-*-300"
                                               "l-*-298"
                                               "l-*-296"
                                               "l-*-294"
                                               "l-*-292"
                                               "l-*-290"
                                               "l-*-288"
                                               "l-*-286"
                                               "l-*-284"
                                               "l-*-282"
                                               "l-*-280"
                                               "l-*-278"
                                               "l-*-276"
                                               "l-*-274"
                                               "l-*-272"
                                               "l-*-270"
                                               "l-*-268"
                                               "l-*-266"
                                               "l-*-264"
                                               "l-*-262"
                                               "l-*-260"
                                               "l-*-258"
                                               "l-*-256"
                                               "l-*-255"
                                               "l-*-254"
                                               "l-*-253"
                                               "l-*-252"
                                               "l-*-250"
                                               "l-*-248"
                                               "l-*-246"
                                               "l-*-243"
                                               "l-*-241"
                                               "l-*-239"
                                               "l-*-237"
                                               "l-*-235"
                                               "l-*-233"
                                               "l-*-231"
                                               "l-*-229"
                                               "l-*-227"
                                               "l-*-225"
                                               "l-*-223"
                                               "l-*-221"
                                               "l-*-219"
                                               "l-*-217"
                                               "l-*-215"
                                               "l-*-213"
                                               "l-*-211"
                                               "l-*-209"))
                                            #(ribcage
                                              (define-structure
                                                define-expansion-accessors
                                                define-expansion-constructors)
                                              ((top) (top) (top))
                                              ("l-*-47" "l-*-46" "l-*-45")))
                                           (hygiene guile))
                                        mod))))
                       (values (remodulate exp mod) r w (source-annotation exp) mod)))
                   tmp)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              tmp-1))))))
  (global-extend
    'core
    'if
    (lambda (e r w s mod)
      (let* ((tmp e) (tmp-1 ($sc-dispatch tmp '(_ any any))))
        (if tmp-1
          (apply (lambda (test then)
                   (build-conditional
                     s
                     (expand test r w mod)
                     (expand then r w mod)
                     (build-void #f)))
                 tmp-1)
          (let ((tmp-1 ($sc-dispatch tmp '(_ any any any))))
            (if tmp-1
              (apply (lambda (test then else)
                       (build-conditional
                         s
                         (expand test r w mod)
                         (expand then r w mod)
                         (expand else r w mod)))
                     tmp-1)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                tmp)))))))
  (global-extend
    'core
    'with-fluids
    (lambda (e r w s mod)
      (let* ((tmp-1 e)
             (tmp ($sc-dispatch tmp-1 '(_ #(each (any any)) any . each-any))))
        (if tmp
          (apply (lambda (fluid val b b*)
                   (build-dynlet
                     s
                     (map (lambda (x) (expand x r w mod)) fluid)
                     (map (lambda (x) (expand x r w mod)) val)
                     (expand-body (cons b b*) (source-wrap e w s mod) r w mod)))
                 tmp)
          (syntax-violation
            #f
            "source expression failed to match any pattern"
            tmp-1)))))
  (global-extend 'begin 'begin '())
  (global-extend 'define 'define '())
  (global-extend 'define-syntax 'define-syntax '())
  (global-extend 'define-syntax-parameter 'define-syntax-parameter '())
  (global-extend 'eval-when 'eval-when '())
  (global-extend
    'core
    'syntax-case
    (letrec*
      ((convert-pattern
         (lambda (pattern keys)
           (letrec*
             ((cvt* (lambda (p* n ids)
                      (if (not (pair? p*))
                        (cvt p* n ids)
                        (call-with-values
                          (lambda () (cvt* (cdr p*) n ids))
                          (lambda (y ids)
                            (call-with-values
                              (lambda () (cvt (car p*) n ids))
                              (lambda (x ids) (values (cons x y) ids))))))))
              (v-reverse
                (lambda (x)
                  (let loop ((r '()) (x x))
                    (if (not (pair? x)) (values r x) (loop (cons (car x) r) (cdr x))))))
              (cvt (lambda (p n ids)
                     (if (id? p)
                       (cond ((bound-id-member? p keys) (values (vector 'free-id p) ids))
                             ((free-id=?
                                p
                                '#(syntax-object
                                   _
                                   ((top)
                                    #(ribcage () () ())
                                    #(ribcage
                                      #(p n ids)
                                      #((top) (top) (top))
                                      #("l-*-3705" "l-*-3706" "l-*-3707"))
                                    #(ribcage
                                      (cvt v-reverse cvt*)
                                      ((top) (top) (top))
                                      ("l-*-3678" "l-*-3676" "l-*-3674"))
                                    #(ribcage #(pattern keys) #((top) (top)) #("l-*-3672" "l-*-3673"))
                                    #(ribcage
                                      (gen-syntax-case gen-clause build-dispatch-call convert-pattern)
                                      ((top) (top) (top) (top))
                                      ("l-*-3668" "l-*-3666" "l-*-3664" "l-*-3662"))
                                    #(ribcage
                                      (lambda-var-list
                                        gen-var
                                        strip
                                        expand-lambda-case
                                        lambda*-formals
                                        expand-simple-lambda
                                        lambda-formals
                                        ellipsis?
                                        expand-void
                                        eval-local-transformer
                                        expand-local-syntax
                                        expand-body
                                        expand-macro
                                        expand-application
                                        expand-expr
                                        expand
                                        syntax-type
                                        parse-when-list
                                        expand-install-global
                                        expand-top-sequence
                                        expand-sequence
                                        source-wrap
                                        wrap
                                        bound-id-member?
                                        distinct-bound-ids?
                                        valid-bound-ids?
                                        bound-id=?
                                        free-id=?
                                        with-transformer-environment
                                        transformer-environment
                                        resolve-identifier
                                        locally-bound-identifiers
                                        id-var-name
                                        same-marks?
                                        join-marks
                                        join-wraps
                                        smart-append
                                        make-binding-wrap
                                        extend-ribcage!
                                        make-empty-ribcage
                                        new-mark
                                        anti-mark
                                        the-anti-mark
                                        top-marked?
                                        top-wrap
                                        empty-wrap
                                        set-ribcage-labels!
                                        set-ribcage-marks!
                                        set-ribcage-symnames!
                                        ribcage-labels
                                        ribcage-marks
                                        ribcage-symnames
                                        ribcage?
                                        make-ribcage
                                        gen-labels
                                        gen-label
                                        make-rename
                                        rename-marks
                                        rename-new
                                        rename-old
                                        subst-rename?
                                        wrap-subst
                                        wrap-marks
                                        make-wrap
                                        id-sym-name&marks
                                        id-sym-name
                                        id?
                                        nonsymbol-id?
                                        global-extend
                                        lookup
                                        macros-only-env
                                        extend-var-env
                                        extend-env
                                        null-env
                                        binding-value
                                        binding-type
                                        make-binding
                                        arg-check
                                        source-annotation
                                        no-source
                                        set-syntax-object-module!
                                        set-syntax-object-wrap!
                                        set-syntax-object-expression!
                                        syntax-object-module
                                        syntax-object-wrap
                                        syntax-object-expression
                                        syntax-object?
                                        make-syntax-object
                                        build-lexical-var
                                        build-letrec
                                        build-named-let
                                        build-let
                                        build-sequence
                                        build-data
                                        build-primref
                                        build-lambda-case
                                        build-case-lambda
                                        build-simple-lambda
                                        build-global-definition
                                        build-global-assignment
                                        build-global-reference
                                        analyze-variable
                                        build-lexical-assignment
                                        build-lexical-reference
                                        build-dynlet
                                        build-conditional
                                        build-application
                                        build-void
                                        maybe-name-value!
                                        decorate-source
                                        get-global-definition-hook
                                        put-global-definition-hook
                                        session-id
                                        local-eval-hook
                                        top-level-eval-hook
                                        fx<
                                        fx=
                                        fx-
                                        fx+
                                        set-lambda-meta!
                                        lambda-meta
                                        lambda?
                                        make-dynlet
                                        make-letrec
                                        make-let
                                        make-lambda-case
                                        make-lambda
                                        make-sequence
                                        make-application
                                        make-conditional
                                        make-toplevel-define
                                        make-toplevel-set
                                        make-toplevel-ref
                                        make-module-set
                                        make-module-ref
                                        make-lexical-set
                                        make-lexical-ref
                                        make-primitive-ref
                                        make-const
                                        make-void)
                                      ((top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top)
                                       (top))
                                      ("l-*-476"
                                       "l-*-474"
                                       "l-*-472"
                                       "l-*-470"
                                       "l-*-468"
                                       "l-*-466"
                                       "l-*-464"
                                       "l-*-462"
                                       "l-*-460"
                                       "l-*-458"
                                       "l-*-456"
                                       "l-*-454"
                                       "l-*-452"
                                       "l-*-450"
                                       "l-*-448"
                                       "l-*-446"
                                       "l-*-444"
                                       "l-*-442"
                                       "l-*-440"
                                       "l-*-438"
                                       "l-*-436"
                                       "l-*-434"
                                       "l-*-432"
                                       "l-*-430"
                                       "l-*-428"
                                       "l-*-426"
                                       "l-*-424"
                                       "l-*-422"
                                       "l-*-420"
                                       "l-*-418"
                                       "l-*-416"
                                       "l-*-414"
                                       "l-*-412"
                                       "l-*-410"
                                       "l-*-408"
                                       "l-*-406"
                                       "l-*-404"
                                       "l-*-402"
                                       "l-*-400"
                                       "l-*-399"
                                       "l-*-397"
                                       "l-*-394"
                                       "l-*-393"
                                       "l-*-392"
                                       "l-*-390"
                                       "l-*-389"
                                       "l-*-387"
                                       "l-*-385"
                                       "l-*-383"
                                       "l-*-381"
                                       "l-*-379"
                                       "l-*-377"
                                       "l-*-375"
                                       "l-*-373"
                                       "l-*-370"
                                       "l-*-368"
                                       "l-*-367"
                                       "l-*-365"
                                       "l-*-363"
                                       "l-*-361"
                                       "l-*-359"
                                       "l-*-358"
                                       "l-*-357"
                                       "l-*-356"
                                       "l-*-354"
                                       "l-*-353"
                                       "l-*-350"
                                       "l-*-348"
                                       "l-*-346"
                                       "l-*-344"
                                       "l-*-342"
                                       "l-*-340"
                                       "l-*-338"
                                       "l-*-337"
                                       "l-*-336"
                                       "l-*-334"
                                       "l-*-332"
                                       "l-*-331"
                                       "l-*-328"
                                       "l-*-327"
                                       "l-*-325"
                                       "l-*-323"
                                       "l-*-321"
                                       "l-*-319"
                                       "l-*-317"
                                       "l-*-315"
                                       "l-*-313"
                                       "l-*-311"
                                       "l-*-309"
                                       "l-*-306"
                                       "l-*-304"
                                       "l-*-302"
                                       "l-*-300"
                                       "l-*-298"
                                       "l-*-296"
                                       "l-*-294"
                                       "l-*-292"
                                       "l-*-290"
                                       "l-*-288"
                                       "l-*-286"
                                       "l-*-284"
                                       "l-*-282"
                                       "l-*-280"
                                       "l-*-278"
                                       "l-*-276"
                                       "l-*-274"
                                       "l-*-272"
                                       "l-*-270"
                                       "l-*-268"
                                       "l-*-266"
                                       "l-*-264"
                                       "l-*-262"
                                       "l-*-260"
                                       "l-*-258"
                                       "l-*-256"
                                       "l-*-255"
                                       "l-*-254"
                                       "l-*-253"
                                       "l-*-252"
                                       "l-*-250"
                                       "l-*-248"
                                       "l-*-246"
                                       "l-*-243"
                                       "l-*-241"
                                       "l-*-239"
                                       "l-*-237"
                                       "l-*-235"
                                       "l-*-233"
                                       "l-*-231"
                                       "l-*-229"
                                       "l-*-227"
                                       "l-*-225"
                                       "l-*-223"
                                       "l-*-221"
                                       "l-*-219"
                                       "l-*-217"
                                       "l-*-215"
                                       "l-*-213"
                                       "l-*-211"
                                       "l-*-209"))
                                    #(ribcage
                                      (define-structure
                                        define-expansion-accessors
                                        define-expansion-constructors)
                                      ((top) (top) (top))
                                      ("l-*-47" "l-*-46" "l-*-45")))
                                   (hygiene guile)))
                              (values '_ ids))
                             (else (values 'any (cons (cons p n) ids))))
                       (let* ((tmp p) (tmp-1 ($sc-dispatch tmp '(any any))))
                         (if (and tmp-1 (apply (lambda (x dots) (ellipsis? dots)) tmp-1))
                           (apply (lambda (x dots)
                                    (call-with-values
                                      (lambda () (cvt x (+ n 1) ids))
                                      (lambda (p ids)
                                        (values (if (eq? p 'any) 'each-any (vector 'each p)) ids))))
                                  tmp-1)
                           (let ((tmp-1 ($sc-dispatch tmp '(any any . any))))
                             (if (and tmp-1 (apply (lambda (x dots ys) (ellipsis? dots)) tmp-1))
                               (apply (lambda (x dots ys)
                                        (call-with-values
                                          (lambda () (cvt* ys n ids))
                                          (lambda (ys ids)
                                            (call-with-values
                                              (lambda () (cvt x (+ n 1) ids))
                                              (lambda (x ids)
                                                (call-with-values
                                                  (lambda () (v-reverse ys))
                                                  (lambda (ys e) (values (vector 'each+ x ys e) ids))))))))
                                      tmp-1)
                               (let ((tmp-1 ($sc-dispatch tmp '(any . any))))
                                 (if tmp-1
                                   (apply (lambda (x y)
                                            (call-with-values
                                              (lambda () (cvt y n ids))
                                              (lambda (y ids)
                                                (call-with-values
                                                  (lambda () (cvt x n ids))
                                                  (lambda (x ids) (values (cons x y) ids))))))
                                          tmp-1)
                                   (let ((tmp-1 ($sc-dispatch tmp '())))
                                     (if tmp-1
                                       (apply (lambda () (values '() ids)) tmp-1)
                                       (let ((tmp-1 ($sc-dispatch tmp '#(vector each-any))))
                                         (if tmp-1
                                           (apply (lambda (x)
                                                    (call-with-values
                                                      (lambda () (cvt x n ids))
                                                      (lambda (p ids) (values (vector 'vector p) ids))))
                                                  tmp-1)
                                           (let ((x tmp)) (values (vector 'atom (strip p '(()))) ids))))))))))))))))
             (cvt pattern 0 '()))))
       (build-dispatch-call
         (lambda (pvars exp y r mod)
           (let ((ids (map car pvars)) (levels (map cdr pvars)))
             (let ((labels (gen-labels ids)) (new-vars (map gen-var ids)))
               (build-application
                 #f
                 (build-primref #f 'apply)
                 (list (build-simple-lambda
                         #f
                         (map syntax->datum ids)
                         #f
                         new-vars
                         '()
                         (expand
                           exp
                           (extend-env
                             labels
                             (map (lambda (var level) (cons 'syntax (cons var level)))
                                  new-vars
                                  (map cdr pvars))
                             r)
                           (make-binding-wrap ids labels '(()))
                           mod))
                       y))))))
       (gen-clause
         (lambda (x keys clauses r pat fender exp mod)
           (call-with-values
             (lambda () (convert-pattern pat keys))
             (lambda (p pvars)
               (cond ((not (distinct-bound-ids? (map car pvars)))
                      (syntax-violation 'syntax-case "duplicate pattern variable" pat))
                     ((not (and-map (lambda (x) (not (ellipsis? (car x)))) pvars))
                      (syntax-violation 'syntax-case "misplaced ellipsis" pat))
                     (else
                      (let ((y (gen-var 'tmp)))
                        (build-application
                          #f
                          (build-simple-lambda
                            #f
                            (list 'tmp)
                            #f
                            (list y)
                            '()
                            (let ((y (build-lexical-reference 'value #f 'tmp y)))
                              (build-conditional
                                #f
                                (let* ((tmp fender) (tmp ($sc-dispatch tmp '#(atom #t))))
                                  (if tmp
                                    (apply (lambda () y) tmp)
                                    (build-conditional
                                      #f
                                      y
                                      (build-dispatch-call pvars fender y r mod)
                                      (build-data #f #f))))
                                (build-dispatch-call pvars exp y r mod)
                                (gen-syntax-case x keys clauses r mod))))
                          (list (if (eq? p 'any)
                                  (build-application #f (build-primref #f 'list) (list x))
                                  (build-application
                                    #f
                                    (build-primref #f '$sc-dispatch)
                                    (list x (build-data #f p)))))))))))))
       (gen-syntax-case
         (lambda (x keys clauses r mod)
           (if (null? clauses)
             (build-application
               #f
               (build-primref #f 'syntax-violation)
               (list (build-data #f #f)
                     (build-data #f "source expression failed to match any pattern")
                     x))
             (let* ((tmp-1 (car clauses)) (tmp ($sc-dispatch tmp-1 '(any any))))
               (if tmp
                 (apply (lambda (pat exp)
                          (if (and (id? pat)
                                   (and-map
                                     (lambda (x) (not (free-id=? pat x)))
                                     (cons '#(syntax-object
                                              ...
                                              ((top)
                                               #(ribcage #(pat exp) #((top) (top)) #("l-*-3859" "l-*-3860"))
                                               #(ribcage () () ())
                                               #(ribcage
                                                 #(x keys clauses r mod)
                                                 #((top) (top) (top) (top) (top))
                                                 #("l-*-3848" "l-*-3849" "l-*-3850" "l-*-3851" "l-*-3852"))
                                               #(ribcage
                                                 (gen-syntax-case gen-clause build-dispatch-call convert-pattern)
                                                 ((top) (top) (top) (top))
                                                 ("l-*-3668" "l-*-3666" "l-*-3664" "l-*-3662"))
                                               #(ribcage
                                                 (lambda-var-list
                                                   gen-var
                                                   strip
                                                   expand-lambda-case
                                                   lambda*-formals
                                                   expand-simple-lambda
                                                   lambda-formals
                                                   ellipsis?
                                                   expand-void
                                                   eval-local-transformer
                                                   expand-local-syntax
                                                   expand-body
                                                   expand-macro
                                                   expand-application
                                                   expand-expr
                                                   expand
                                                   syntax-type
                                                   parse-when-list
                                                   expand-install-global
                                                   expand-top-sequence
                                                   expand-sequence
                                                   source-wrap
                                                   wrap
                                                   bound-id-member?
                                                   distinct-bound-ids?
                                                   valid-bound-ids?
                                                   bound-id=?
                                                   free-id=?
                                                   with-transformer-environment
                                                   transformer-environment
                                                   resolve-identifier
                                                   locally-bound-identifiers
                                                   id-var-name
                                                   same-marks?
                                                   join-marks
                                                   join-wraps
                                                   smart-append
                                                   make-binding-wrap
                                                   extend-ribcage!
                                                   make-empty-ribcage
                                                   new-mark
                                                   anti-mark
                                                   the-anti-mark
                                                   top-marked?
                                                   top-wrap
                                                   empty-wrap
                                                   set-ribcage-labels!
                                                   set-ribcage-marks!
                                                   set-ribcage-symnames!
                                                   ribcage-labels
                                                   ribcage-marks
                                                   ribcage-symnames
                                                   ribcage?
                                                   make-ribcage
                                                   gen-labels
                                                   gen-label
                                                   make-rename
                                                   rename-marks
                                                   rename-new
                                                   rename-old
                                                   subst-rename?
                                                   wrap-subst
                                                   wrap-marks
                                                   make-wrap
                                                   id-sym-name&marks
                                                   id-sym-name
                                                   id?
                                                   nonsymbol-id?
                                                   global-extend
                                                   lookup
                                                   macros-only-env
                                                   extend-var-env
                                                   extend-env
                                                   null-env
                                                   binding-value
                                                   binding-type
                                                   make-binding
                                                   arg-check
                                                   source-annotation
                                                   no-source
                                                   set-syntax-object-module!
                                                   set-syntax-object-wrap!
                                                   set-syntax-object-expression!
                                                   syntax-object-module
                                                   syntax-object-wrap
                                                   syntax-object-expression
                                                   syntax-object?
                                                   make-syntax-object
                                                   build-lexical-var
                                                   build-letrec
                                                   build-named-let
                                                   build-let
                                                   build-sequence
                                                   build-data
                                                   build-primref
                                                   build-lambda-case
                                                   build-case-lambda
                                                   build-simple-lambda
                                                   build-global-definition
                                                   build-global-assignment
                                                   build-global-reference
                                                   analyze-variable
                                                   build-lexical-assignment
                                                   build-lexical-reference
                                                   build-dynlet
                                                   build-conditional
                                                   build-application
                                                   build-void
                                                   maybe-name-value!
                                                   decorate-source
                                                   get-global-definition-hook
                                                   put-global-definition-hook
                                                   session-id
                                                   local-eval-hook
                                                   top-level-eval-hook
                                                   fx<
                                                   fx=
                                                   fx-
                                                   fx+
                                                   set-lambda-meta!
                                                   lambda-meta
                                                   lambda?
                                                   make-dynlet
                                                   make-letrec
                                                   make-let
                                                   make-lambda-case
                                                   make-lambda
                                                   make-sequence
                                                   make-application
                                                   make-conditional
                                                   make-toplevel-define
                                                   make-toplevel-set
                                                   make-toplevel-ref
                                                   make-module-set
                                                   make-module-ref
                                                   make-lexical-set
                                                   make-lexical-ref
                                                   make-primitive-ref
                                                   make-const
                                                   make-void)
                                                 ((top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top))
                                                 ("l-*-476"
                                                  "l-*-474"
                                                  "l-*-472"
                                                  "l-*-470"
                                                  "l-*-468"
                                                  "l-*-466"
                                                  "l-*-464"
                                                  "l-*-462"
                                                  "l-*-460"
                                                  "l-*-458"
                                                  "l-*-456"
                                                  "l-*-454"
                                                  "l-*-452"
                                                  "l-*-450"
                                                  "l-*-448"
                                                  "l-*-446"
                                                  "l-*-444"
                                                  "l-*-442"
                                                  "l-*-440"
                                                  "l-*-438"
                                                  "l-*-436"
                                                  "l-*-434"
                                                  "l-*-432"
                                                  "l-*-430"
                                                  "l-*-428"
                                                  "l-*-426"
                                                  "l-*-424"
                                                  "l-*-422"
                                                  "l-*-420"
                                                  "l-*-418"
                                                  "l-*-416"
                                                  "l-*-414"
                                                  "l-*-412"
                                                  "l-*-410"
                                                  "l-*-408"
                                                  "l-*-406"
                                                  "l-*-404"
                                                  "l-*-402"
                                                  "l-*-400"
                                                  "l-*-399"
                                                  "l-*-397"
                                                  "l-*-394"
                                                  "l-*-393"
                                                  "l-*-392"
                                                  "l-*-390"
                                                  "l-*-389"
                                                  "l-*-387"
                                                  "l-*-385"
                                                  "l-*-383"
                                                  "l-*-381"
                                                  "l-*-379"
                                                  "l-*-377"
                                                  "l-*-375"
                                                  "l-*-373"
                                                  "l-*-370"
                                                  "l-*-368"
                                                  "l-*-367"
                                                  "l-*-365"
                                                  "l-*-363"
                                                  "l-*-361"
                                                  "l-*-359"
                                                  "l-*-358"
                                                  "l-*-357"
                                                  "l-*-356"
                                                  "l-*-354"
                                                  "l-*-353"
                                                  "l-*-350"
                                                  "l-*-348"
                                                  "l-*-346"
                                                  "l-*-344"
                                                  "l-*-342"
                                                  "l-*-340"
                                                  "l-*-338"
                                                  "l-*-337"
                                                  "l-*-336"
                                                  "l-*-334"
                                                  "l-*-332"
                                                  "l-*-331"
                                                  "l-*-328"
                                                  "l-*-327"
                                                  "l-*-325"
                                                  "l-*-323"
                                                  "l-*-321"
                                                  "l-*-319"
                                                  "l-*-317"
                                                  "l-*-315"
                                                  "l-*-313"
                                                  "l-*-311"
                                                  "l-*-309"
                                                  "l-*-306"
                                                  "l-*-304"
                                                  "l-*-302"
                                                  "l-*-300"
                                                  "l-*-298"
                                                  "l-*-296"
                                                  "l-*-294"
                                                  "l-*-292"
                                                  "l-*-290"
                                                  "l-*-288"
                                                  "l-*-286"
                                                  "l-*-284"
                                                  "l-*-282"
                                                  "l-*-280"
                                                  "l-*-278"
                                                  "l-*-276"
                                                  "l-*-274"
                                                  "l-*-272"
                                                  "l-*-270"
                                                  "l-*-268"
                                                  "l-*-266"
                                                  "l-*-264"
                                                  "l-*-262"
                                                  "l-*-260"
                                                  "l-*-258"
                                                  "l-*-256"
                                                  "l-*-255"
                                                  "l-*-254"
                                                  "l-*-253"
                                                  "l-*-252"
                                                  "l-*-250"
                                                  "l-*-248"
                                                  "l-*-246"
                                                  "l-*-243"
                                                  "l-*-241"
                                                  "l-*-239"
                                                  "l-*-237"
                                                  "l-*-235"
                                                  "l-*-233"
                                                  "l-*-231"
                                                  "l-*-229"
                                                  "l-*-227"
                                                  "l-*-225"
                                                  "l-*-223"
                                                  "l-*-221"
                                                  "l-*-219"
                                                  "l-*-217"
                                                  "l-*-215"
                                                  "l-*-213"
                                                  "l-*-211"
                                                  "l-*-209"))
                                               #(ribcage
                                                 (define-structure
                                                   define-expansion-accessors
                                                   define-expansion-constructors)
                                                 ((top) (top) (top))
                                                 ("l-*-47" "l-*-46" "l-*-45")))
                                              (hygiene guile))
                                           keys)))
                            (if (free-id=?
                                  pat
                                  '#(syntax-object
                                     _
                                     ((top)
                                      #(ribcage #(pat exp) #((top) (top)) #("l-*-3859" "l-*-3860"))
                                      #(ribcage () () ())
                                      #(ribcage
                                        #(x keys clauses r mod)
                                        #((top) (top) (top) (top) (top))
                                        #("l-*-3848" "l-*-3849" "l-*-3850" "l-*-3851" "l-*-3852"))
                                      #(ribcage
                                        (gen-syntax-case gen-clause build-dispatch-call convert-pattern)
                                        ((top) (top) (top) (top))
                                        ("l-*-3668" "l-*-3666" "l-*-3664" "l-*-3662"))
                                      #(ribcage
                                        (lambda-var-list
                                          gen-var
                                          strip
                                          expand-lambda-case
                                          lambda*-formals
                                          expand-simple-lambda
                                          lambda-formals
                                          ellipsis?
                                          expand-void
                                          eval-local-transformer
                                          expand-local-syntax
                                          expand-body
                                          expand-macro
                                          expand-application
                                          expand-expr
                                          expand
                                          syntax-type
                                          parse-when-list
                                          expand-install-global
                                          expand-top-sequence
                                          expand-sequence
                                          source-wrap
                                          wrap
                                          bound-id-member?
                                          distinct-bound-ids?
                                          valid-bound-ids?
                                          bound-id=?
                                          free-id=?
                                          with-transformer-environment
                                          transformer-environment
                                          resolve-identifier
                                          locally-bound-identifiers
                                          id-var-name
                                          same-marks?
                                          join-marks
                                          join-wraps
                                          smart-append
                                          make-binding-wrap
                                          extend-ribcage!
                                          make-empty-ribcage
                                          new-mark
                                          anti-mark
                                          the-anti-mark
                                          top-marked?
                                          top-wrap
                                          empty-wrap
                                          set-ribcage-labels!
                                          set-ribcage-marks!
                                          set-ribcage-symnames!
                                          ribcage-labels
                                          ribcage-marks
                                          ribcage-symnames
                                          ribcage?
                                          make-ribcage
                                          gen-labels
                                          gen-label
                                          make-rename
                                          rename-marks
                                          rename-new
                                          rename-old
                                          subst-rename?
                                          wrap-subst
                                          wrap-marks
                                          make-wrap
                                          id-sym-name&marks
                                          id-sym-name
                                          id?
                                          nonsymbol-id?
                                          global-extend
                                          lookup
                                          macros-only-env
                                          extend-var-env
                                          extend-env
                                          null-env
                                          binding-value
                                          binding-type
                                          make-binding
                                          arg-check
                                          source-annotation
                                          no-source
                                          set-syntax-object-module!
                                          set-syntax-object-wrap!
                                          set-syntax-object-expression!
                                          syntax-object-module
                                          syntax-object-wrap
                                          syntax-object-expression
                                          syntax-object?
                                          make-syntax-object
                                          build-lexical-var
                                          build-letrec
                                          build-named-let
                                          build-let
                                          build-sequence
                                          build-data
                                          build-primref
                                          build-lambda-case
                                          build-case-lambda
                                          build-simple-lambda
                                          build-global-definition
                                          build-global-assignment
                                          build-global-reference
                                          analyze-variable
                                          build-lexical-assignment
                                          build-lexical-reference
                                          build-dynlet
                                          build-conditional
                                          build-application
                                          build-void
                                          maybe-name-value!
                                          decorate-source
                                          get-global-definition-hook
                                          put-global-definition-hook
                                          session-id
                                          local-eval-hook
                                          top-level-eval-hook
                                          fx<
                                          fx=
                                          fx-
                                          fx+
                                          set-lambda-meta!
                                          lambda-meta
                                          lambda?
                                          make-dynlet
                                          make-letrec
                                          make-let
                                          make-lambda-case
                                          make-lambda
                                          make-sequence
                                          make-application
                                          make-conditional
                                          make-toplevel-define
                                          make-toplevel-set
                                          make-toplevel-ref
                                          make-module-set
                                          make-module-ref
                                          make-lexical-set
                                          make-lexical-ref
                                          make-primitive-ref
                                          make-const
                                          make-void)
                                        ((top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top)
                                         (top))
                                        ("l-*-476"
                                         "l-*-474"
                                         "l-*-472"
                                         "l-*-470"
                                         "l-*-468"
                                         "l-*-466"
                                         "l-*-464"
                                         "l-*-462"
                                         "l-*-460"
                                         "l-*-458"
                                         "l-*-456"
                                         "l-*-454"
                                         "l-*-452"
                                         "l-*-450"
                                         "l-*-448"
                                         "l-*-446"
                                         "l-*-444"
                                         "l-*-442"
                                         "l-*-440"
                                         "l-*-438"
                                         "l-*-436"
                                         "l-*-434"
                                         "l-*-432"
                                         "l-*-430"
                                         "l-*-428"
                                         "l-*-426"
                                         "l-*-424"
                                         "l-*-422"
                                         "l-*-420"
                                         "l-*-418"
                                         "l-*-416"
                                         "l-*-414"
                                         "l-*-412"
                                         "l-*-410"
                                         "l-*-408"
                                         "l-*-406"
                                         "l-*-404"
                                         "l-*-402"
                                         "l-*-400"
                                         "l-*-399"
                                         "l-*-397"
                                         "l-*-394"
                                         "l-*-393"
                                         "l-*-392"
                                         "l-*-390"
                                         "l-*-389"
                                         "l-*-387"
                                         "l-*-385"
                                         "l-*-383"
                                         "l-*-381"
                                         "l-*-379"
                                         "l-*-377"
                                         "l-*-375"
                                         "l-*-373"
                                         "l-*-370"
                                         "l-*-368"
                                         "l-*-367"
                                         "l-*-365"
                                         "l-*-363"
                                         "l-*-361"
                                         "l-*-359"
                                         "l-*-358"
                                         "l-*-357"
                                         "l-*-356"
                                         "l-*-354"
                                         "l-*-353"
                                         "l-*-350"
                                         "l-*-348"
                                         "l-*-346"
                                         "l-*-344"
                                         "l-*-342"
                                         "l-*-340"
                                         "l-*-338"
                                         "l-*-337"
                                         "l-*-336"
                                         "l-*-334"
                                         "l-*-332"
                                         "l-*-331"
                                         "l-*-328"
                                         "l-*-327"
                                         "l-*-325"
                                         "l-*-323"
                                         "l-*-321"
                                         "l-*-319"
                                         "l-*-317"
                                         "l-*-315"
                                         "l-*-313"
                                         "l-*-311"
                                         "l-*-309"
                                         "l-*-306"
                                         "l-*-304"
                                         "l-*-302"
                                         "l-*-300"
                                         "l-*-298"
                                         "l-*-296"
                                         "l-*-294"
                                         "l-*-292"
                                         "l-*-290"
                                         "l-*-288"
                                         "l-*-286"
                                         "l-*-284"
                                         "l-*-282"
                                         "l-*-280"
                                         "l-*-278"
                                         "l-*-276"
                                         "l-*-274"
                                         "l-*-272"
                                         "l-*-270"
                                         "l-*-268"
                                         "l-*-266"
                                         "l-*-264"
                                         "l-*-262"
                                         "l-*-260"
                                         "l-*-258"
                                         "l-*-256"
                                         "l-*-255"
                                         "l-*-254"
                                         "l-*-253"
                                         "l-*-252"
                                         "l-*-250"
                                         "l-*-248"
                                         "l-*-246"
                                         "l-*-243"
                                         "l-*-241"
                                         "l-*-239"
                                         "l-*-237"
                                         "l-*-235"
                                         "l-*-233"
                                         "l-*-231"
                                         "l-*-229"
                                         "l-*-227"
                                         "l-*-225"
                                         "l-*-223"
                                         "l-*-221"
                                         "l-*-219"
                                         "l-*-217"
                                         "l-*-215"
                                         "l-*-213"
                                         "l-*-211"
                                         "l-*-209"))
                                      #(ribcage
                                        (define-structure
                                          define-expansion-accessors
                                          define-expansion-constructors)
                                        ((top) (top) (top))
                                        ("l-*-47" "l-*-46" "l-*-45")))
                                     (hygiene guile)))
                              (expand exp r '(()) mod)
                              (let ((labels (list (gen-label))) (var (gen-var pat)))
                                (build-application
                                  #f
                                  (build-simple-lambda
                                    #f
                                    (list (syntax->datum pat))
                                    #f
                                    (list var)
                                    '()
                                    (expand
                                      exp
                                      (extend-env labels (list (cons 'syntax (cons var 0))) r)
                                      (make-binding-wrap (list pat) labels '(()))
                                      mod))
                                  (list x))))
                            (gen-clause x keys (cdr clauses) r pat #t exp mod)))
                        tmp)
                 (let ((tmp ($sc-dispatch tmp-1 '(any any any))))
                   (if tmp
                     (apply (lambda (pat fender exp)
                              (gen-clause x keys (cdr clauses) r pat fender exp mod))
                            tmp)
                     (syntax-violation 'syntax-case "invalid clause" (car clauses))))))))))
      (lambda (e r w s mod)
        (let* ((e (source-wrap e w s mod))
               (tmp-1 e)
               (tmp ($sc-dispatch tmp-1 '(_ any each-any . each-any))))
          (if tmp
            (apply (lambda (val key m)
                     (if (and-map (lambda (x) (and (id? x) (not (ellipsis? x)))) key)
                       (let ((x (gen-var 'tmp)))
                         (build-application
                           s
                           (build-simple-lambda
                             #f
                             (list 'tmp)
                             #f
                             (list x)
                             '()
                             (gen-syntax-case
                               (build-lexical-reference 'value #f 'tmp x)
                               key
                               m
                               r
                               mod))
                           (list (expand val r '(()) mod))))
                       (syntax-violation 'syntax-case "invalid literals list" e)))
                   tmp)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              tmp-1))))))
  (set! macroexpand
    (lambda* (x #:optional (m 'e) (esew '(eval)))
      (expand-top-sequence
        (list x)
        '()
        '((top))
        #f
        m
        esew
        (cons 'hygiene (module-name (current-module))))))
  (set! identifier? (lambda (x) (nonsymbol-id? x)))
  (set! datum->syntax
    (lambda (id datum)
      (make-syntax-object
        datum
        (syntax-object-wrap id)
        (syntax-object-module id))))
  (set! syntax->datum (lambda (x) (strip x '(()))))
  (set! syntax-source (lambda (x) (source-annotation x)))
  (set! generate-temporaries
    (lambda (ls)
      (let ((x ls))
        (if (not (list? x))
          (syntax-violation 'generate-temporaries "invalid argument" x)))
      (let ((mod (cons 'hygiene (module-name (current-module)))))
        (map (lambda (x) (wrap (gensym "t-") '((top)) mod)) ls))))
  (set! free-identifier=?
    (lambda (x y)
      (let ((x x))
        (if (not (nonsymbol-id? x))
          (syntax-violation 'free-identifier=? "invalid argument" x)))
      (let ((x y))
        (if (not (nonsymbol-id? x))
          (syntax-violation 'free-identifier=? "invalid argument" x)))
      (free-id=? x y)))
  (set! bound-identifier=?
    (lambda (x y)
      (let ((x x))
        (if (not (nonsymbol-id? x))
          (syntax-violation 'bound-identifier=? "invalid argument" x)))
      (let ((x y))
        (if (not (nonsymbol-id? x))
          (syntax-violation 'bound-identifier=? "invalid argument" x)))
      (bound-id=? x y)))
  (set! syntax-violation
    (lambda* (who message form #:optional (subform #f))
      (let ((x who))
        (if (not (let ((x x)) (or (not x) (string? x) (symbol? x))))
          (syntax-violation 'syntax-violation "invalid argument" x)))
      (let ((x message))
        (if (not (string? x))
          (syntax-violation 'syntax-violation "invalid argument" x)))
      (throw 'syntax-error
             who
             message
             (or (source-annotation subform) (source-annotation form))
             (strip form '(()))
             (and subform (strip subform '(()))))))
  (letrec*
    ((syntax-module
       (lambda (id)
         (let ((x id))
           (if (not (nonsymbol-id? x))
             (syntax-violation 'syntax-module "invalid argument" x)))
         (cdr (syntax-object-module id))))
     (syntax-local-binding
       (lambda (id)
         (let ((x id))
           (if (not (nonsymbol-id? x))
             (syntax-violation 'syntax-local-binding "invalid argument" x)))
         (with-transformer-environment
           (lambda (e r w s rib mod)
             (letrec*
               ((strip-anti-mark
                  (lambda (w)
                    (let ((ms (car w)) (s (cdr w)))
                      (if (and (pair? ms) (eq? (car ms) #f))
                        (cons (cdr ms) (if rib (cons rib (cdr s)) (cdr s)))
                        (cons ms (if rib (cons rib s) s)))))))
               (call-with-values
                 (lambda ()
                   (resolve-identifier
                     (syntax-object-expression id)
                     (strip-anti-mark (syntax-object-wrap id))
                     r
                     (syntax-object-module id)))
                 (lambda (type value mod)
                   (let ((key type))
                     (cond ((memv key '(lexical)) (values 'lexical value))
                           ((memv key '(macro)) (values 'macro value))
                           ((memv key '(syntax)) (values 'pattern-variable value))
                           ((memv key '(displaced-lexical)) (values 'displaced-lexical #f))
                           ((memv key '(global)) (values 'global (cons value (cdr mod))))
                           (else (values 'other #f)))))))))))
     (syntax-locally-bound-identifiers
       (lambda (id)
         (let ((x id))
           (if (not (nonsymbol-id? x))
             (syntax-violation
               'syntax-locally-bound-identifiers
               "invalid argument"
               x)))
         (locally-bound-identifiers
           (syntax-object-wrap id)
           (syntax-object-module id)))))
    (define! 'syntax-module syntax-module)
    (define! 'syntax-local-binding syntax-local-binding)
    (define!
      'syntax-locally-bound-identifiers
      syntax-locally-bound-identifiers))
  (letrec*
    ((match-each
       (lambda (e p w mod)
         (cond ((pair? e)
                (let ((first (match (car e) p w '() mod)))
                  (and first
                       (let ((rest (match-each (cdr e) p w mod)))
                         (and rest (cons first rest))))))
               ((null? e) '())
               ((syntax-object? e)
                (match-each
                  (syntax-object-expression e)
                  p
                  (join-wraps w (syntax-object-wrap e))
                  (syntax-object-module e)))
               (else #f))))
     (match-each+
       (lambda (e x-pat y-pat z-pat w r mod)
         (let f ((e e) (w w))
           (cond ((pair? e)
                  (call-with-values
                    (lambda () (f (cdr e) w))
                    (lambda (xr* y-pat r)
                      (if r
                        (if (null? y-pat)
                          (let ((xr (match (car e) x-pat w '() mod)))
                            (if xr (values (cons xr xr*) y-pat r) (values #f #f #f)))
                          (values '() (cdr y-pat) (match (car e) (car y-pat) w r mod)))
                        (values #f #f #f)))))
                 ((syntax-object? e)
                  (f (syntax-object-expression e) (join-wraps w e)))
                 (else (values '() y-pat (match e z-pat w r mod)))))))
     (match-each-any
       (lambda (e w mod)
         (cond ((pair? e)
                (let ((l (match-each-any (cdr e) w mod)))
                  (and l (cons (wrap (car e) w mod) l))))
               ((null? e) '())
               ((syntax-object? e)
                (match-each-any
                  (syntax-object-expression e)
                  (join-wraps w (syntax-object-wrap e))
                  mod))
               (else #f))))
     (match-empty
       (lambda (p r)
         (cond ((null? p) r)
               ((eq? p '_) r)
               ((eq? p 'any) (cons '() r))
               ((pair? p) (match-empty (car p) (match-empty (cdr p) r)))
               ((eq? p 'each-any) (cons '() r))
               (else
                (let ((key (vector-ref p 0)))
                  (cond ((memv key '(each)) (match-empty (vector-ref p 1) r))
                        ((memv key '(each+))
                         (match-empty
                           (vector-ref p 1)
                           (match-empty
                             (reverse (vector-ref p 2))
                             (match-empty (vector-ref p 3) r))))
                        ((memv key '(free-id atom)) r)
                        ((memv key '(vector)) (match-empty (vector-ref p 1) r))))))))
     (combine
       (lambda (r* r)
         (if (null? (car r*)) r (cons (map car r*) (combine (map cdr r*) r)))))
     (match*
       (lambda (e p w r mod)
         (cond ((null? p) (and (null? e) r))
               ((pair? p)
                (and (pair? e)
                     (match (car e) (car p) w (match (cdr e) (cdr p) w r mod) mod)))
               ((eq? p 'each-any)
                (let ((l (match-each-any e w mod))) (and l (cons l r))))
               (else
                (let ((key (vector-ref p 0)))
                  (cond ((memv key '(each))
                         (if (null? e)
                           (match-empty (vector-ref p 1) r)
                           (let ((l (match-each e (vector-ref p 1) w mod)))
                             (and l
                                  (let collect ((l l))
                                    (if (null? (car l)) r (cons (map car l) (collect (map cdr l)))))))))
                        ((memv key '(each+))
                         (call-with-values
                           (lambda ()
                             (match-each+
                               e
                               (vector-ref p 1)
                               (vector-ref p 2)
                               (vector-ref p 3)
                               w
                               r
                               mod))
                           (lambda (xr* y-pat r)
                             (and r
                                  (null? y-pat)
                                  (if (null? xr*) (match-empty (vector-ref p 1) r) (combine xr* r))))))
                        ((memv key '(free-id))
                         (and (id? e) (free-id=? (wrap e w mod) (vector-ref p 1)) r))
                        ((memv key '(atom)) (and (equal? (vector-ref p 1) (strip e w)) r))
                        ((memv key '(vector))
                         (and (vector? e) (match (vector->list e) (vector-ref p 1) w r mod)))))))))
     (match (lambda (e p w r mod)
              (cond ((not r) #f)
                    ((eq? p '_) r)
                    ((eq? p 'any) (cons (wrap e w mod) r))
                    ((syntax-object? e)
                     (match*
                       (syntax-object-expression e)
                       p
                       (join-wraps w (syntax-object-wrap e))
                       r
                       (syntax-object-module e)))
                    (else (match* e p w r mod))))))
    (set! $sc-dispatch
      (lambda (e p)
        (cond ((eq? p 'any) (list e))
              ((eq? p '_) '())
              ((syntax-object? e)
               (match*
                 (syntax-object-expression e)
                 p
                 (syntax-object-wrap e)
                 '()
                 (syntax-object-module e)))
              (else (match* e p '(()) '() #f)))))))

(define with-syntax
  (make-syntax-transformer
    'with-syntax
    'macro
    (lambda (x)
      (let ((tmp x))
        (let ((tmp-1 ($sc-dispatch tmp '(_ () any . each-any))))
          (if tmp-1
            (apply (lambda (e1 e2)
                     (cons '#(syntax-object
                              let
                              ((top)
                               #(ribcage #(e1 e2) #((top) (top)) #("l-*-4203" "l-*-4204"))
                               #(ribcage () () ())
                               #(ribcage #(x) #((top)) #("l-*-4200")))
                              (hygiene guile))
                           (cons '() (cons e1 e2))))
                   tmp-1)
            (let ((tmp-1 ($sc-dispatch tmp '(_ ((any any)) any . each-any))))
              (if tmp-1
                (apply (lambda (out in e1 e2)
                         (list '#(syntax-object
                                  syntax-case
                                  ((top)
                                   #(ribcage
                                     #(out in e1 e2)
                                     #((top) (top) (top) (top))
                                     #("l-*-4209" "l-*-4210" "l-*-4211" "l-*-4212"))
                                   #(ribcage () () ())
                                   #(ribcage #(x) #((top)) #("l-*-4200")))
                                  (hygiene guile))
                               in
                               '()
                               (list out
                                     (cons '#(syntax-object
                                              let
                                              ((top)
                                               #(ribcage
                                                 #(out in e1 e2)
                                                 #((top) (top) (top) (top))
                                                 #("l-*-4209" "l-*-4210" "l-*-4211" "l-*-4212"))
                                               #(ribcage () () ())
                                               #(ribcage #(x) #((top)) #("l-*-4200")))
                                              (hygiene guile))
                                           (cons '() (cons e1 e2))))))
                       tmp-1)
                (let ((tmp-1 ($sc-dispatch tmp '(_ #(each (any any)) any . each-any))))
                  (if tmp-1
                    (apply (lambda (out in e1 e2)
                             (list '#(syntax-object
                                      syntax-case
                                      ((top)
                                       #(ribcage
                                         #(out in e1 e2)
                                         #((top) (top) (top) (top))
                                         #("l-*-4219" "l-*-4220" "l-*-4221" "l-*-4222"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("l-*-4200")))
                                      (hygiene guile))
                                   (cons '#(syntax-object
                                            list
                                            ((top)
                                             #(ribcage
                                               #(out in e1 e2)
                                               #((top) (top) (top) (top))
                                               #("l-*-4219" "l-*-4220" "l-*-4221" "l-*-4222"))
                                             #(ribcage () () ())
                                             #(ribcage #(x) #((top)) #("l-*-4200")))
                                            (hygiene guile))
                                         in)
                                   '()
                                   (list out
                                         (cons '#(syntax-object
                                                  let
                                                  ((top)
                                                   #(ribcage
                                                     #(out in e1 e2)
                                                     #((top) (top) (top) (top))
                                                     #("l-*-4219" "l-*-4220" "l-*-4221" "l-*-4222"))
                                                   #(ribcage () () ())
                                                   #(ribcage #(x) #((top)) #("l-*-4200")))
                                                  (hygiene guile))
                                               (cons '() (cons e1 e2))))))
                           tmp-1)
                    (syntax-violation
                      #f
                      "source expression failed to match any pattern"
                      tmp)))))))))))

(define syntax-rules
  (make-syntax-transformer
    'syntax-rules
    'macro
    (lambda (x)
      (let ((tmp-1 x))
        (let ((tmp ($sc-dispatch tmp-1 '(_ each-any . #(each ((any . any) any))))))
          (if tmp
            (apply (lambda (k keyword pattern template)
                     (list '#(syntax-object
                              lambda
                              ((top)
                               #(ribcage
                                 #(k keyword pattern template)
                                 #((top) (top) (top) (top))
                                 #("l-*-4234" "l-*-4235" "l-*-4236" "l-*-4237"))
                               #(ribcage () () ())
                               #(ribcage #(x) #((top)) #("l-*-4231")))
                              (hygiene guile))
                           '(#(syntax-object
                               x
                               ((top)
                                #(ribcage
                                  #(k keyword pattern template)
                                  #((top) (top) (top) (top))
                                  #("l-*-4234" "l-*-4235" "l-*-4236" "l-*-4237"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("l-*-4231")))
                               (hygiene guile)))
                           (vector
                             '(#(syntax-object
                                 macro-type
                                 ((top)
                                  #(ribcage
                                    #(k keyword pattern template)
                                    #((top) (top) (top) (top))
                                    #("l-*-4234" "l-*-4235" "l-*-4236" "l-*-4237"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("l-*-4231")))
                                 (hygiene guile))
                               .
                               #(syntax-object
                                 syntax-rules
                                 ((top)
                                  #(ribcage
                                    #(k keyword pattern template)
                                    #((top) (top) (top) (top))
                                    #("l-*-4234" "l-*-4235" "l-*-4236" "l-*-4237"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("l-*-4231")))
                                 (hygiene guile)))
                             (cons '#(syntax-object
                                      patterns
                                      ((top)
                                       #(ribcage
                                         #(k keyword pattern template)
                                         #((top) (top) (top) (top))
                                         #("l-*-4234" "l-*-4235" "l-*-4236" "l-*-4237"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("l-*-4231")))
                                      (hygiene guile))
                                   pattern))
                           (cons '#(syntax-object
                                    syntax-case
                                    ((top)
                                     #(ribcage
                                       #(k keyword pattern template)
                                       #((top) (top) (top) (top))
                                       #("l-*-4234" "l-*-4235" "l-*-4236" "l-*-4237"))
                                     #(ribcage () () ())
                                     #(ribcage #(x) #((top)) #("l-*-4231")))
                                    (hygiene guile))
                                 (cons '#(syntax-object
                                          x
                                          ((top)
                                           #(ribcage
                                             #(k keyword pattern template)
                                             #((top) (top) (top) (top))
                                             #("l-*-4234" "l-*-4235" "l-*-4236" "l-*-4237"))
                                           #(ribcage () () ())
                                           #(ribcage #(x) #((top)) #("l-*-4231")))
                                          (hygiene guile))
                                       (cons k
                                             (map (lambda (tmp-1 tmp)
                                                    (list (cons '#(syntax-object
                                                                   dummy
                                                                   ((top)
                                                                    #(ribcage
                                                                      #(k keyword pattern template)
                                                                      #((top) (top) (top) (top))
                                                                      #("l-*-4234" "l-*-4235" "l-*-4236" "l-*-4237"))
                                                                    #(ribcage () () ())
                                                                    #(ribcage #(x) #((top)) #("l-*-4231")))
                                                                   (hygiene guile))
                                                                tmp)
                                                          (list '#(syntax-object
                                                                   syntax
                                                                   ((top)
                                                                    #(ribcage
                                                                      #(k keyword pattern template)
                                                                      #((top) (top) (top) (top))
                                                                      #("l-*-4234" "l-*-4235" "l-*-4236" "l-*-4237"))
                                                                    #(ribcage () () ())
                                                                    #(ribcage #(x) #((top)) #("l-*-4231")))
                                                                   (hygiene guile))
                                                                tmp-1)))
                                                  template
                                                  pattern))))))
                   tmp)
            (let ((tmp ($sc-dispatch tmp-1 '(_ each-any any . #(each ((any . any) any))))))
              (if (if tmp
                    (apply (lambda (k docstring keyword pattern template)
                             (string? (syntax->datum docstring)))
                           tmp)
                    #f)
                (apply (lambda (k docstring keyword pattern template)
                         (list '#(syntax-object
                                  lambda
                                  ((top)
                                   #(ribcage
                                     #(k docstring keyword pattern template)
                                     #((top) (top) (top) (top) (top))
                                     #("l-*-4257" "l-*-4258" "l-*-4259" "l-*-4260" "l-*-4261"))
                                   #(ribcage () () ())
                                   #(ribcage #(x) #((top)) #("l-*-4231")))
                                  (hygiene guile))
                               '(#(syntax-object
                                   x
                                   ((top)
                                    #(ribcage
                                      #(k docstring keyword pattern template)
                                      #((top) (top) (top) (top) (top))
                                      #("l-*-4257" "l-*-4258" "l-*-4259" "l-*-4260" "l-*-4261"))
                                    #(ribcage () () ())
                                    #(ribcage #(x) #((top)) #("l-*-4231")))
                                   (hygiene guile)))
                               docstring
                               (vector
                                 '(#(syntax-object
                                     macro-type
                                     ((top)
                                      #(ribcage
                                        #(k docstring keyword pattern template)
                                        #((top) (top) (top) (top) (top))
                                        #("l-*-4257" "l-*-4258" "l-*-4259" "l-*-4260" "l-*-4261"))
                                      #(ribcage () () ())
                                      #(ribcage #(x) #((top)) #("l-*-4231")))
                                     (hygiene guile))
                                   .
                                   #(syntax-object
                                     syntax-rules
                                     ((top)
                                      #(ribcage
                                        #(k docstring keyword pattern template)
                                        #((top) (top) (top) (top) (top))
                                        #("l-*-4257" "l-*-4258" "l-*-4259" "l-*-4260" "l-*-4261"))
                                      #(ribcage () () ())
                                      #(ribcage #(x) #((top)) #("l-*-4231")))
                                     (hygiene guile)))
                                 (cons '#(syntax-object
                                          patterns
                                          ((top)
                                           #(ribcage
                                             #(k docstring keyword pattern template)
                                             #((top) (top) (top) (top) (top))
                                             #("l-*-4257" "l-*-4258" "l-*-4259" "l-*-4260" "l-*-4261"))
                                           #(ribcage () () ())
                                           #(ribcage #(x) #((top)) #("l-*-4231")))
                                          (hygiene guile))
                                       pattern))
                               (cons '#(syntax-object
                                        syntax-case
                                        ((top)
                                         #(ribcage
                                           #(k docstring keyword pattern template)
                                           #((top) (top) (top) (top) (top))
                                           #("l-*-4257" "l-*-4258" "l-*-4259" "l-*-4260" "l-*-4261"))
                                         #(ribcage () () ())
                                         #(ribcage #(x) #((top)) #("l-*-4231")))
                                        (hygiene guile))
                                     (cons '#(syntax-object
                                              x
                                              ((top)
                                               #(ribcage
                                                 #(k docstring keyword pattern template)
                                                 #((top) (top) (top) (top) (top))
                                                 #("l-*-4257" "l-*-4258" "l-*-4259" "l-*-4260" "l-*-4261"))
                                               #(ribcage () () ())
                                               #(ribcage #(x) #((top)) #("l-*-4231")))
                                              (hygiene guile))
                                           (cons k
                                                 (map (lambda (tmp-1 tmp)
                                                        (list (cons '#(syntax-object
                                                                       dummy
                                                                       ((top)
                                                                        #(ribcage
                                                                          #(k docstring keyword pattern template)
                                                                          #((top) (top) (top) (top) (top))
                                                                          #("l-*-4257"
                                                                            "l-*-4258"
                                                                            "l-*-4259"
                                                                            "l-*-4260"
                                                                            "l-*-4261"))
                                                                        #(ribcage () () ())
                                                                        #(ribcage #(x) #((top)) #("l-*-4231")))
                                                                       (hygiene guile))
                                                                    tmp)
                                                              (list '#(syntax-object
                                                                       syntax
                                                                       ((top)
                                                                        #(ribcage
                                                                          #(k docstring keyword pattern template)
                                                                          #((top) (top) (top) (top) (top))
                                                                          #("l-*-4257"
                                                                            "l-*-4258"
                                                                            "l-*-4259"
                                                                            "l-*-4260"
                                                                            "l-*-4261"))
                                                                        #(ribcage () () ())
                                                                        #(ribcage #(x) #((top)) #("l-*-4231")))
                                                                       (hygiene guile))
                                                                    tmp-1)))
                                                      template
                                                      pattern))))))
                       tmp)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  tmp-1)))))))))

(define define-syntax-rule
  (make-syntax-transformer
    'define-syntax-rule
    'macro
    (lambda (x)
      (let ((tmp-1 x))
        (let ((tmp ($sc-dispatch tmp-1 '(_ (any . any) any))))
          (if tmp
            (apply (lambda (name pattern template)
                     (list '#(syntax-object
                              define-syntax
                              ((top)
                               #(ribcage
                                 #(name pattern template)
                                 #((top) (top) (top))
                                 #("l-*-4275" "l-*-4276" "l-*-4277"))
                               #(ribcage () () ())
                               #(ribcage #(x) #((top)) #("l-*-4272")))
                              (hygiene guile))
                           name
                           (list '#(syntax-object
                                    syntax-rules
                                    ((top)
                                     #(ribcage
                                       #(name pattern template)
                                       #((top) (top) (top))
                                       #("l-*-4275" "l-*-4276" "l-*-4277"))
                                     #(ribcage () () ())
                                     #(ribcage #(x) #((top)) #("l-*-4272")))
                                    (hygiene guile))
                                 '()
                                 (list (cons '#(syntax-object
                                                _
                                                ((top)
                                                 #(ribcage
                                                   #(name pattern template)
                                                   #((top) (top) (top))
                                                   #("l-*-4275" "l-*-4276" "l-*-4277"))
                                                 #(ribcage () () ())
                                                 #(ribcage #(x) #((top)) #("l-*-4272")))
                                                (hygiene guile))
                                             pattern)
                                       template))))
                   tmp)
            (let ((tmp ($sc-dispatch tmp-1 '(_ (any . any) any any))))
              (if (if tmp
                    (apply (lambda (name pattern docstring template)
                             (string? (syntax->datum docstring)))
                           tmp)
                    #f)
                (apply (lambda (name pattern docstring template)
                         (list '#(syntax-object
                                  define-syntax
                                  ((top)
                                   #(ribcage
                                     #(name pattern docstring template)
                                     #((top) (top) (top) (top))
                                     #("l-*-4290" "l-*-4291" "l-*-4292" "l-*-4293"))
                                   #(ribcage () () ())
                                   #(ribcage #(x) #((top)) #("l-*-4272")))
                                  (hygiene guile))
                               name
                               (list '#(syntax-object
                                        syntax-rules
                                        ((top)
                                         #(ribcage
                                           #(name pattern docstring template)
                                           #((top) (top) (top) (top))
                                           #("l-*-4290" "l-*-4291" "l-*-4292" "l-*-4293"))
                                         #(ribcage () () ())
                                         #(ribcage #(x) #((top)) #("l-*-4272")))
                                        (hygiene guile))
                                     '()
                                     docstring
                                     (list (cons '#(syntax-object
                                                    _
                                                    ((top)
                                                     #(ribcage
                                                       #(name pattern docstring template)
                                                       #((top) (top) (top) (top))
                                                       #("l-*-4290" "l-*-4291" "l-*-4292" "l-*-4293"))
                                                     #(ribcage () () ())
                                                     #(ribcage #(x) #((top)) #("l-*-4272")))
                                                    (hygiene guile))
                                                 pattern)
                                           template))))
                       tmp)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  tmp-1)))))))))

(define let*
  (make-syntax-transformer
    'let*
    'macro
    (lambda (x)
      (let ((tmp-1 x))
        (let ((tmp ($sc-dispatch tmp-1 '(any #(each (any any)) any . each-any))))
          (if (if tmp
                (apply (lambda (let* x v e1 e2) (and-map identifier? x)) tmp)
                #f)
            (apply (lambda (let* x v e1 e2)
                     (let f ((bindings (map list x v)))
                       (if (null? bindings)
                         (cons '#(syntax-object
                                  let
                                  ((top)
                                   #(ribcage () () ())
                                   #(ribcage #(f bindings) #((top) (top)) #("l-*-4323" "l-*-4324"))
                                   #(ribcage
                                     #(let* x v e1 e2)
                                     #((top) (top) (top) (top) (top))
                                     #("l-*-4313" "l-*-4314" "l-*-4315" "l-*-4316" "l-*-4317"))
                                   #(ribcage () () ())
                                   #(ribcage #(x) #((top)) #("l-*-4299")))
                                  (hygiene guile))
                               (cons '() (cons e1 e2)))
                         (let ((tmp-1 (list (f (cdr bindings)) (car bindings))))
                           (let ((tmp ($sc-dispatch tmp-1 '(any any))))
                             (if tmp
                               (apply (lambda (body binding)
                                        (list '#(syntax-object
                                                 let
                                                 ((top)
                                                  #(ribcage () () ())
                                                  #(ribcage #(body binding) #((top) (top)) #("l-*-4333" "l-*-4334"))
                                                  #(ribcage () () ())
                                                  #(ribcage #(f bindings) #((top) (top)) #("l-*-4323" "l-*-4324"))
                                                  #(ribcage
                                                    #(let* x v e1 e2)
                                                    #((top) (top) (top) (top) (top))
                                                    #("l-*-4313" "l-*-4314" "l-*-4315" "l-*-4316" "l-*-4317"))
                                                  #(ribcage () () ())
                                                  #(ribcage #(x) #((top)) #("l-*-4299")))
                                                 (hygiene guile))
                                              (list binding)
                                              body))
                                      tmp)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 tmp-1)))))))
                   tmp)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              tmp-1)))))))

(define do
  (make-syntax-transformer
    'do
    'macro
    (lambda (orig-x)
      (let ((tmp-1 orig-x))
        (let ((tmp ($sc-dispatch
                     tmp-1
                     '(_ #(each (any any . any)) (any . each-any) . each-any))))
          (if tmp
            (apply (lambda (var init step e0 e1 c)
                     (let ((tmp-1 (map (lambda (v s)
                                         (let ((tmp-1 s))
                                           (let ((tmp ($sc-dispatch tmp-1 '())))
                                             (if tmp
                                               (apply (lambda () v) tmp)
                                               (let ((tmp ($sc-dispatch tmp-1 '(any))))
                                                 (if tmp
                                                   (apply (lambda (e) e) tmp)
                                                   (syntax-violation 'do "bad step expression" orig-x s)))))))
                                       var
                                       step)))
                       (let ((tmp ($sc-dispatch tmp-1 'each-any)))
                         (if tmp
                           (apply (lambda (step)
                                    (let ((tmp e1))
                                      (let ((tmp-1 ($sc-dispatch tmp '())))
                                        (if tmp-1
                                          (apply (lambda ()
                                                   (list '#(syntax-object
                                                            let
                                                            ((top)
                                                             #(ribcage () () ())
                                                             #(ribcage #(step) #((top)) #("l-*-4356"))
                                                             #(ribcage
                                                               #(var init step e0 e1 c)
                                                               #((top) (top) (top) (top) (top) (top))
                                                               #("l-*-4341"
                                                                 "l-*-4342"
                                                                 "l-*-4343"
                                                                 "l-*-4344"
                                                                 "l-*-4345"
                                                                 "l-*-4346"))
                                                             #(ribcage () () ())
                                                             #(ribcage #(orig-x) #((top)) #("l-*-4338")))
                                                            (hygiene guile))
                                                         '#(syntax-object
                                                            doloop
                                                            ((top)
                                                             #(ribcage () () ())
                                                             #(ribcage #(step) #((top)) #("l-*-4356"))
                                                             #(ribcage
                                                               #(var init step e0 e1 c)
                                                               #((top) (top) (top) (top) (top) (top))
                                                               #("l-*-4341"
                                                                 "l-*-4342"
                                                                 "l-*-4343"
                                                                 "l-*-4344"
                                                                 "l-*-4345"
                                                                 "l-*-4346"))
                                                             #(ribcage () () ())
                                                             #(ribcage #(orig-x) #((top)) #("l-*-4338")))
                                                            (hygiene guile))
                                                         (map list var init)
                                                         (list '#(syntax-object
                                                                  if
                                                                  ((top)
                                                                   #(ribcage () () ())
                                                                   #(ribcage #(step) #((top)) #("l-*-4356"))
                                                                   #(ribcage
                                                                     #(var init step e0 e1 c)
                                                                     #((top) (top) (top) (top) (top) (top))
                                                                     #("l-*-4341"
                                                                       "l-*-4342"
                                                                       "l-*-4343"
                                                                       "l-*-4344"
                                                                       "l-*-4345"
                                                                       "l-*-4346"))
                                                                   #(ribcage () () ())
                                                                   #(ribcage #(orig-x) #((top)) #("l-*-4338")))
                                                                  (hygiene guile))
                                                               (list '#(syntax-object
                                                                        not
                                                                        ((top)
                                                                         #(ribcage () () ())
                                                                         #(ribcage #(step) #((top)) #("l-*-4356"))
                                                                         #(ribcage
                                                                           #(var init step e0 e1 c)
                                                                           #((top) (top) (top) (top) (top) (top))
                                                                           #("l-*-4341"
                                                                             "l-*-4342"
                                                                             "l-*-4343"
                                                                             "l-*-4344"
                                                                             "l-*-4345"
                                                                             "l-*-4346"))
                                                                         #(ribcage () () ())
                                                                         #(ribcage #(orig-x) #((top)) #("l-*-4338")))
                                                                        (hygiene guile))
                                                                     e0)
                                                               (cons '#(syntax-object
                                                                        begin
                                                                        ((top)
                                                                         #(ribcage () () ())
                                                                         #(ribcage #(step) #((top)) #("l-*-4356"))
                                                                         #(ribcage
                                                                           #(var init step e0 e1 c)
                                                                           #((top) (top) (top) (top) (top) (top))
                                                                           #("l-*-4341"
                                                                             "l-*-4342"
                                                                             "l-*-4343"
                                                                             "l-*-4344"
                                                                             "l-*-4345"
                                                                             "l-*-4346"))
                                                                         #(ribcage () () ())
                                                                         #(ribcage #(orig-x) #((top)) #("l-*-4338")))
                                                                        (hygiene guile))
                                                                     (append
                                                                       c
                                                                       (list (cons '#(syntax-object
                                                                                      doloop
                                                                                      ((top)
                                                                                       #(ribcage () () ())
                                                                                       #(ribcage
                                                                                         #(step)
                                                                                         #((top))
                                                                                         #("l-*-4356"))
                                                                                       #(ribcage
                                                                                         #(var init step e0 e1 c)
                                                                                         #((top)
                                                                                           (top)
                                                                                           (top)
                                                                                           (top)
                                                                                           (top)
                                                                                           (top))
                                                                                         #("l-*-4341"
                                                                                           "l-*-4342"
                                                                                           "l-*-4343"
                                                                                           "l-*-4344"
                                                                                           "l-*-4345"
                                                                                           "l-*-4346"))
                                                                                       #(ribcage () () ())
                                                                                       #(ribcage
                                                                                         #(orig-x)
                                                                                         #((top))
                                                                                         #("l-*-4338")))
                                                                                      (hygiene guile))
                                                                                   step)))))))
                                                 tmp-1)
                                          (let ((tmp-1 ($sc-dispatch tmp '(any . each-any))))
                                            (if tmp-1
                                              (apply (lambda (e1 e2)
                                                       (list '#(syntax-object
                                                                let
                                                                ((top)
                                                                 #(ribcage
                                                                   #(e1 e2)
                                                                   #((top) (top))
                                                                   #("l-*-4365" "l-*-4366"))
                                                                 #(ribcage () () ())
                                                                 #(ribcage #(step) #((top)) #("l-*-4356"))
                                                                 #(ribcage
                                                                   #(var init step e0 e1 c)
                                                                   #((top) (top) (top) (top) (top) (top))
                                                                   #("l-*-4341"
                                                                     "l-*-4342"
                                                                     "l-*-4343"
                                                                     "l-*-4344"
                                                                     "l-*-4345"
                                                                     "l-*-4346"))
                                                                 #(ribcage () () ())
                                                                 #(ribcage #(orig-x) #((top)) #("l-*-4338")))
                                                                (hygiene guile))
                                                             '#(syntax-object
                                                                doloop
                                                                ((top)
                                                                 #(ribcage
                                                                   #(e1 e2)
                                                                   #((top) (top))
                                                                   #("l-*-4365" "l-*-4366"))
                                                                 #(ribcage () () ())
                                                                 #(ribcage #(step) #((top)) #("l-*-4356"))
                                                                 #(ribcage
                                                                   #(var init step e0 e1 c)
                                                                   #((top) (top) (top) (top) (top) (top))
                                                                   #("l-*-4341"
                                                                     "l-*-4342"
                                                                     "l-*-4343"
                                                                     "l-*-4344"
                                                                     "l-*-4345"
                                                                     "l-*-4346"))
                                                                 #(ribcage () () ())
                                                                 #(ribcage #(orig-x) #((top)) #("l-*-4338")))
                                                                (hygiene guile))
                                                             (map list var init)
                                                             (list '#(syntax-object
                                                                      if
                                                                      ((top)
                                                                       #(ribcage
                                                                         #(e1 e2)
                                                                         #((top) (top))
                                                                         #("l-*-4365" "l-*-4366"))
                                                                       #(ribcage () () ())
                                                                       #(ribcage #(step) #((top)) #("l-*-4356"))
                                                                       #(ribcage
                                                                         #(var init step e0 e1 c)
                                                                         #((top) (top) (top) (top) (top) (top))
                                                                         #("l-*-4341"
                                                                           "l-*-4342"
                                                                           "l-*-4343"
                                                                           "l-*-4344"
                                                                           "l-*-4345"
                                                                           "l-*-4346"))
                                                                       #(ribcage () () ())
                                                                       #(ribcage #(orig-x) #((top)) #("l-*-4338")))
                                                                      (hygiene guile))
                                                                   e0
                                                                   (cons '#(syntax-object
                                                                            begin
                                                                            ((top)
                                                                             #(ribcage
                                                                               #(e1 e2)
                                                                               #((top) (top))
                                                                               #("l-*-4365" "l-*-4366"))
                                                                             #(ribcage () () ())
                                                                             #(ribcage #(step) #((top)) #("l-*-4356"))
                                                                             #(ribcage
                                                                               #(var init step e0 e1 c)
                                                                               #((top) (top) (top) (top) (top) (top))
                                                                               #("l-*-4341"
                                                                                 "l-*-4342"
                                                                                 "l-*-4343"
                                                                                 "l-*-4344"
                                                                                 "l-*-4345"
                                                                                 "l-*-4346"))
                                                                             #(ribcage () () ())
                                                                             #(ribcage
                                                                               #(orig-x)
                                                                               #((top))
                                                                               #("l-*-4338")))
                                                                            (hygiene guile))
                                                                         (cons e1 e2))
                                                                   (cons '#(syntax-object
                                                                            begin
                                                                            ((top)
                                                                             #(ribcage
                                                                               #(e1 e2)
                                                                               #((top) (top))
                                                                               #("l-*-4365" "l-*-4366"))
                                                                             #(ribcage () () ())
                                                                             #(ribcage #(step) #((top)) #("l-*-4356"))
                                                                             #(ribcage
                                                                               #(var init step e0 e1 c)
                                                                               #((top) (top) (top) (top) (top) (top))
                                                                               #("l-*-4341"
                                                                                 "l-*-4342"
                                                                                 "l-*-4343"
                                                                                 "l-*-4344"
                                                                                 "l-*-4345"
                                                                                 "l-*-4346"))
                                                                             #(ribcage () () ())
                                                                             #(ribcage
                                                                               #(orig-x)
                                                                               #((top))
                                                                               #("l-*-4338")))
                                                                            (hygiene guile))
                                                                         (append
                                                                           c
                                                                           (list (cons '#(syntax-object
                                                                                          doloop
                                                                                          ((top)
                                                                                           #(ribcage
                                                                                             #(e1 e2)
                                                                                             #((top) (top))
                                                                                             #("l-*-4365" "l-*-4366"))
                                                                                           #(ribcage () () ())
                                                                                           #(ribcage
                                                                                             #(step)
                                                                                             #((top))
                                                                                             #("l-*-4356"))
                                                                                           #(ribcage
                                                                                             #(var init step e0 e1 c)
                                                                                             #((top)
                                                                                               (top)
                                                                                               (top)
                                                                                               (top)
                                                                                               (top)
                                                                                               (top))
                                                                                             #("l-*-4341"
                                                                                               "l-*-4342"
                                                                                               "l-*-4343"
                                                                                               "l-*-4344"
                                                                                               "l-*-4345"
                                                                                               "l-*-4346"))
                                                                                           #(ribcage () () ())
                                                                                           #(ribcage
                                                                                             #(orig-x)
                                                                                             #((top))
                                                                                             #("l-*-4338")))
                                                                                          (hygiene guile))
                                                                                       step)))))))
                                                     tmp-1)
                                              (syntax-violation
                                                #f
                                                "source expression failed to match any pattern"
                                                tmp)))))))
                                  tmp)
                           (syntax-violation
                             #f
                             "source expression failed to match any pattern"
                             tmp-1)))))
                   tmp)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              tmp-1)))))))

(define quasiquote
  (make-syntax-transformer
    'quasiquote
    'macro
    (letrec*
      ((quasi (lambda (p lev)
                (let ((tmp p))
                  (let ((tmp-1 ($sc-dispatch
                                 tmp
                                 '(#(free-id
                                     #(syntax-object
                                       unquote
                                       ((top)
                                        #(ribcage () () ())
                                        #(ribcage #(p lev) #((top) (top)) #("l-*-4402" "l-*-4403"))
                                        #(ribcage
                                          (emit quasivector quasilist* quasiappend quasicons vquasi quasi)
                                          ((top) (top) (top) (top) (top) (top) (top))
                                          ("l-*-4398"
                                           "l-*-4396"
                                           "l-*-4394"
                                           "l-*-4392"
                                           "l-*-4390"
                                           "l-*-4388"
                                           "l-*-4386")))
                                       (hygiene guile)))
                                   any))))
                    (if tmp-1
                      (apply (lambda (p)
                               (if (= lev 0)
                                 (list '#(syntax-object
                                          "value"
                                          ((top)
                                           #(ribcage #(p) #((top)) #("l-*-4406"))
                                           #(ribcage () () ())
                                           #(ribcage #(p lev) #((top) (top)) #("l-*-4402" "l-*-4403"))
                                           #(ribcage
                                             (emit quasivector quasilist* quasiappend quasicons vquasi quasi)
                                             ((top) (top) (top) (top) (top) (top) (top))
                                             ("l-*-4398"
                                              "l-*-4396"
                                              "l-*-4394"
                                              "l-*-4392"
                                              "l-*-4390"
                                              "l-*-4388"
                                              "l-*-4386")))
                                          (hygiene guile))
                                       p)
                                 (quasicons
                                   '(#(syntax-object
                                       "quote"
                                       ((top)
                                        #(ribcage #(p) #((top)) #("l-*-4406"))
                                        #(ribcage () () ())
                                        #(ribcage #(p lev) #((top) (top)) #("l-*-4402" "l-*-4403"))
                                        #(ribcage
                                          (emit quasivector quasilist* quasiappend quasicons vquasi quasi)
                                          ((top) (top) (top) (top) (top) (top) (top))
                                          ("l-*-4398"
                                           "l-*-4396"
                                           "l-*-4394"
                                           "l-*-4392"
                                           "l-*-4390"
                                           "l-*-4388"
                                           "l-*-4386")))
                                       (hygiene guile))
                                     #(syntax-object
                                       unquote
                                       ((top)
                                        #(ribcage #(p) #((top)) #("l-*-4406"))
                                        #(ribcage () () ())
                                        #(ribcage #(p lev) #((top) (top)) #("l-*-4402" "l-*-4403"))
                                        #(ribcage
                                          (emit quasivector quasilist* quasiappend quasicons vquasi quasi)
                                          ((top) (top) (top) (top) (top) (top) (top))
                                          ("l-*-4398"
                                           "l-*-4396"
                                           "l-*-4394"
                                           "l-*-4392"
                                           "l-*-4390"
                                           "l-*-4388"
                                           "l-*-4386")))
                                       (hygiene guile)))
                                   (quasi (list p) (- lev 1)))))
                             tmp-1)
                      (let ((tmp-1 ($sc-dispatch
                                     tmp
                                     '(#(free-id
                                         #(syntax-object
                                           quasiquote
                                           ((top)
                                            #(ribcage () () ())
                                            #(ribcage #(p lev) #((top) (top)) #("l-*-4402" "l-*-4403"))
                                            #(ribcage
                                              (emit quasivector quasilist* quasiappend quasicons vquasi quasi)
                                              ((top) (top) (top) (top) (top) (top) (top))
                                              ("l-*-4398"
                                               "l-*-4396"
                                               "l-*-4394"
                                               "l-*-4392"
                                               "l-*-4390"
                                               "l-*-4388"
                                               "l-*-4386")))
                                           (hygiene guile)))
                                       any))))
                        (if tmp-1
                          (apply (lambda (p)
                                   (quasicons
                                     '(#(syntax-object
                                         "quote"
                                         ((top)
                                          #(ribcage #(p) #((top)) #("l-*-4409"))
                                          #(ribcage () () ())
                                          #(ribcage #(p lev) #((top) (top)) #("l-*-4402" "l-*-4403"))
                                          #(ribcage
                                            (emit quasivector quasilist* quasiappend quasicons vquasi quasi)
                                            ((top) (top) (top) (top) (top) (top) (top))
                                            ("l-*-4398"
                                             "l-*-4396"
                                             "l-*-4394"
                                             "l-*-4392"
                                             "l-*-4390"
                                             "l-*-4388"
                                             "l-*-4386")))
                                         (hygiene guile))
                                       #(syntax-object
                                         quasiquote
                                         ((top)
                                          #(ribcage #(p) #((top)) #("l-*-4409"))
                                          #(ribcage () () ())
                                          #(ribcage #(p lev) #((top) (top)) #("l-*-4402" "l-*-4403"))
                                          #(ribcage
                                            (emit quasivector quasilist* quasiappend quasicons vquasi quasi)
                                            ((top) (top) (top) (top) (top) (top) (top))
                                            ("l-*-4398"
                                             "l-*-4396"
                                             "l-*-4394"
                                             "l-*-4392"
                                             "l-*-4390"
                                             "l-*-4388"
                                             "l-*-4386")))
                                         (hygiene guile)))
                                     (quasi (list p) (+ lev 1))))
                                 tmp-1)
                          (let ((tmp-1 ($sc-dispatch tmp '(any . any))))
                            (if tmp-1
                              (apply (lambda (p q)
                                       (let ((tmp-1 p))
                                         (let ((tmp ($sc-dispatch
                                                      tmp-1
                                                      '(#(free-id
                                                          #(syntax-object
                                                            unquote
                                                            ((top)
                                                             #(ribcage #(p q) #((top) (top)) #("l-*-4412" "l-*-4413"))
                                                             #(ribcage () () ())
                                                             #(ribcage #(p lev) #((top) (top)) #("l-*-4402" "l-*-4403"))
                                                             #(ribcage
                                                               (emit quasivector
                                                                     quasilist*
                                                                     quasiappend
                                                                     quasicons
                                                                     vquasi
                                                                     quasi)
                                                               ((top) (top) (top) (top) (top) (top) (top))
                                                               ("l-*-4398"
                                                                "l-*-4396"
                                                                "l-*-4394"
                                                                "l-*-4392"
                                                                "l-*-4390"
                                                                "l-*-4388"
                                                                "l-*-4386")))
                                                            (hygiene guile)))
                                                        .
                                                        each-any))))
                                           (if tmp
                                             (apply (lambda (p)
                                                      (if (= lev 0)
                                                        (quasilist*
                                                          (map (lambda (tmp)
                                                                 (list '#(syntax-object
                                                                          "value"
                                                                          ((top)
                                                                           #(ribcage #(p) #((top)) #("l-*-4418"))
                                                                           #(ribcage
                                                                             #(p q)
                                                                             #((top) (top))
                                                                             #("l-*-4412" "l-*-4413"))
                                                                           #(ribcage () () ())
                                                                           #(ribcage
                                                                             #(p lev)
                                                                             #((top) (top))
                                                                             #("l-*-4402" "l-*-4403"))
                                                                           #(ribcage
                                                                             (emit quasivector
                                                                                   quasilist*
                                                                                   quasiappend
                                                                                   quasicons
                                                                                   vquasi
                                                                                   quasi)
                                                                             ((top) (top) (top) (top) (top) (top) (top))
                                                                             ("l-*-4398"
                                                                              "l-*-4396"
                                                                              "l-*-4394"
                                                                              "l-*-4392"
                                                                              "l-*-4390"
                                                                              "l-*-4388"
                                                                              "l-*-4386")))
                                                                          (hygiene guile))
                                                                       tmp))
                                                               p)
                                                          (quasi q lev))
                                                        (quasicons
                                                          (quasicons
                                                            '(#(syntax-object
                                                                "quote"
                                                                ((top)
                                                                 #(ribcage #(p) #((top)) #("l-*-4418"))
                                                                 #(ribcage
                                                                   #(p q)
                                                                   #((top) (top))
                                                                   #("l-*-4412" "l-*-4413"))
                                                                 #(ribcage () () ())
                                                                 #(ribcage
                                                                   #(p lev)
                                                                   #((top) (top))
                                                                   #("l-*-4402" "l-*-4403"))
                                                                 #(ribcage
                                                                   (emit quasivector
                                                                         quasilist*
                                                                         quasiappend
                                                                         quasicons
                                                                         vquasi
                                                                         quasi)
                                                                   ((top) (top) (top) (top) (top) (top) (top))
                                                                   ("l-*-4398"
                                                                    "l-*-4396"
                                                                    "l-*-4394"
                                                                    "l-*-4392"
                                                                    "l-*-4390"
                                                                    "l-*-4388"
                                                                    "l-*-4386")))
                                                                (hygiene guile))
                                                              #(syntax-object
                                                                unquote
                                                                ((top)
                                                                 #(ribcage #(p) #((top)) #("l-*-4418"))
                                                                 #(ribcage
                                                                   #(p q)
                                                                   #((top) (top))
                                                                   #("l-*-4412" "l-*-4413"))
                                                                 #(ribcage () () ())
                                                                 #(ribcage
                                                                   #(p lev)
                                                                   #((top) (top))
                                                                   #("l-*-4402" "l-*-4403"))
                                                                 #(ribcage
                                                                   (emit quasivector
                                                                         quasilist*
                                                                         quasiappend
                                                                         quasicons
                                                                         vquasi
                                                                         quasi)
                                                                   ((top) (top) (top) (top) (top) (top) (top))
                                                                   ("l-*-4398"
                                                                    "l-*-4396"
                                                                    "l-*-4394"
                                                                    "l-*-4392"
                                                                    "l-*-4390"
                                                                    "l-*-4388"
                                                                    "l-*-4386")))
                                                                (hygiene guile)))
                                                            (quasi p (- lev 1)))
                                                          (quasi q lev))))
                                                    tmp)
                                             (let ((tmp ($sc-dispatch
                                                          tmp-1
                                                          '(#(free-id
                                                              #(syntax-object
                                                                unquote-splicing
                                                                ((top)
                                                                 #(ribcage
                                                                   #(p q)
                                                                   #((top) (top))
                                                                   #("l-*-4412" "l-*-4413"))
                                                                 #(ribcage () () ())
                                                                 #(ribcage
                                                                   #(p lev)
                                                                   #((top) (top))
                                                                   #("l-*-4402" "l-*-4403"))
                                                                 #(ribcage
                                                                   (emit quasivector
                                                                         quasilist*
                                                                         quasiappend
                                                                         quasicons
                                                                         vquasi
                                                                         quasi)
                                                                   ((top) (top) (top) (top) (top) (top) (top))
                                                                   ("l-*-4398"
                                                                    "l-*-4396"
                                                                    "l-*-4394"
                                                                    "l-*-4392"
                                                                    "l-*-4390"
                                                                    "l-*-4388"
                                                                    "l-*-4386")))
                                                                (hygiene guile)))
                                                            .
                                                            each-any))))
                                               (if tmp
                                                 (apply (lambda (p)
                                                          (if (= lev 0)
                                                            (quasiappend
                                                              (map (lambda (tmp)
                                                                     (list '#(syntax-object
                                                                              "value"
                                                                              ((top)
                                                                               #(ribcage #(p) #((top)) #("l-*-4423"))
                                                                               #(ribcage
                                                                                 #(p q)
                                                                                 #((top) (top))
                                                                                 #("l-*-4412" "l-*-4413"))
                                                                               #(ribcage () () ())
                                                                               #(ribcage
                                                                                 #(p lev)
                                                                                 #((top) (top))
                                                                                 #("l-*-4402" "l-*-4403"))
                                                                               #(ribcage
                                                                                 (emit quasivector
                                                                                       quasilist*
                                                                                       quasiappend
                                                                                       quasicons
                                                                                       vquasi
                                                                                       quasi)
                                                                                 ((top)
                                                                                  (top)
                                                                                  (top)
                                                                                  (top)
                                                                                  (top)
                                                                                  (top)
                                                                                  (top))
                                                                                 ("l-*-4398"
                                                                                  "l-*-4396"
                                                                                  "l-*-4394"
                                                                                  "l-*-4392"
                                                                                  "l-*-4390"
                                                                                  "l-*-4388"
                                                                                  "l-*-4386")))
                                                                              (hygiene guile))
                                                                           tmp))
                                                                   p)
                                                              (quasi q lev))
                                                            (quasicons
                                                              (quasicons
                                                                '(#(syntax-object
                                                                    "quote"
                                                                    ((top)
                                                                     #(ribcage #(p) #((top)) #("l-*-4423"))
                                                                     #(ribcage
                                                                       #(p q)
                                                                       #((top) (top))
                                                                       #("l-*-4412" "l-*-4413"))
                                                                     #(ribcage () () ())
                                                                     #(ribcage
                                                                       #(p lev)
                                                                       #((top) (top))
                                                                       #("l-*-4402" "l-*-4403"))
                                                                     #(ribcage
                                                                       (emit quasivector
                                                                             quasilist*
                                                                             quasiappend
                                                                             quasicons
                                                                             vquasi
                                                                             quasi)
                                                                       ((top) (top) (top) (top) (top) (top) (top))
                                                                       ("l-*-4398"
                                                                        "l-*-4396"
                                                                        "l-*-4394"
                                                                        "l-*-4392"
                                                                        "l-*-4390"
                                                                        "l-*-4388"
                                                                        "l-*-4386")))
                                                                    (hygiene guile))
                                                                  #(syntax-object
                                                                    unquote-splicing
                                                                    ((top)
                                                                     #(ribcage #(p) #((top)) #("l-*-4423"))
                                                                     #(ribcage
                                                                       #(p q)
                                                                       #((top) (top))
                                                                       #("l-*-4412" "l-*-4413"))
                                                                     #(ribcage () () ())
                                                                     #(ribcage
                                                                       #(p lev)
                                                                       #((top) (top))
                                                                       #("l-*-4402" "l-*-4403"))
                                                                     #(ribcage
                                                                       (emit quasivector
                                                                             quasilist*
                                                                             quasiappend
                                                                             quasicons
                                                                             vquasi
                                                                             quasi)
                                                                       ((top) (top) (top) (top) (top) (top) (top))
                                                                       ("l-*-4398"
                                                                        "l-*-4396"
                                                                        "l-*-4394"
                                                                        "l-*-4392"
                                                                        "l-*-4390"
                                                                        "l-*-4388"
                                                                        "l-*-4386")))
                                                                    (hygiene guile)))
                                                                (quasi p (- lev 1)))
                                                              (quasi q lev))))
                                                        tmp)
                                                 (quasicons (quasi p lev) (quasi q lev))))))))
                                     tmp-1)
                              (let ((tmp-1 ($sc-dispatch tmp '#(vector each-any))))
                                (if tmp-1
                                  (apply (lambda (x) (quasivector (vquasi x lev))) tmp-1)
                                  (let ((p tmp))
                                    (list '#(syntax-object
                                             "quote"
                                             ((top)
                                              #(ribcage #(p) #((top)) #("l-*-4431"))
                                              #(ribcage () () ())
                                              #(ribcage #(p lev) #((top) (top)) #("l-*-4402" "l-*-4403"))
                                              #(ribcage
                                                (emit quasivector quasilist* quasiappend quasicons vquasi quasi)
                                                ((top) (top) (top) (top) (top) (top) (top))
                                                ("l-*-4398"
                                                 "l-*-4396"
                                                 "l-*-4394"
                                                 "l-*-4392"
                                                 "l-*-4390"
                                                 "l-*-4388"
                                                 "l-*-4386")))
                                             (hygiene guile))
                                          p)))))))))))))
       (vquasi
         (lambda (p lev)
           (let ((tmp p))
             (let ((tmp-1 ($sc-dispatch tmp '(any . any))))
               (if tmp-1
                 (apply (lambda (p q)
                          (let ((tmp-1 p))
                            (let ((tmp ($sc-dispatch
                                         tmp-1
                                         '(#(free-id
                                             #(syntax-object
                                               unquote
                                               ((top)
                                                #(ribcage #(p q) #((top) (top)) #("l-*-4439" "l-*-4440"))
                                                #(ribcage () () ())
                                                #(ribcage #(p lev) #((top) (top)) #("l-*-4435" "l-*-4436"))
                                                #(ribcage
                                                  (emit quasivector quasilist* quasiappend quasicons vquasi quasi)
                                                  ((top) (top) (top) (top) (top) (top) (top))
                                                  ("l-*-4398"
                                                   "l-*-4396"
                                                   "l-*-4394"
                                                   "l-*-4392"
                                                   "l-*-4390"
                                                   "l-*-4388"
                                                   "l-*-4386")))
                                               (hygiene guile)))
                                           .
                                           each-any))))
                              (if tmp
                                (apply (lambda (p)
                                         (if (= lev 0)
                                           (quasilist*
                                             (map (lambda (tmp)
                                                    (list '#(syntax-object
                                                             "value"
                                                             ((top)
                                                              #(ribcage #(p) #((top)) #("l-*-4445"))
                                                              #(ribcage #(p q) #((top) (top)) #("l-*-4439" "l-*-4440"))
                                                              #(ribcage () () ())
                                                              #(ribcage
                                                                #(p lev)
                                                                #((top) (top))
                                                                #("l-*-4435" "l-*-4436"))
                                                              #(ribcage
                                                                (emit quasivector
                                                                      quasilist*
                                                                      quasiappend
                                                                      quasicons
                                                                      vquasi
                                                                      quasi)
                                                                ((top) (top) (top) (top) (top) (top) (top))
                                                                ("l-*-4398"
                                                                 "l-*-4396"
                                                                 "l-*-4394"
                                                                 "l-*-4392"
                                                                 "l-*-4390"
                                                                 "l-*-4388"
                                                                 "l-*-4386")))
                                                             (hygiene guile))
                                                          tmp))
                                                  p)
                                             (vquasi q lev))
                                           (quasicons
                                             (quasicons
                                               '(#(syntax-object
                                                   "quote"
                                                   ((top)
                                                    #(ribcage #(p) #((top)) #("l-*-4445"))
                                                    #(ribcage #(p q) #((top) (top)) #("l-*-4439" "l-*-4440"))
                                                    #(ribcage () () ())
                                                    #(ribcage #(p lev) #((top) (top)) #("l-*-4435" "l-*-4436"))
                                                    #(ribcage
                                                      (emit quasivector quasilist* quasiappend quasicons vquasi quasi)
                                                      ((top) (top) (top) (top) (top) (top) (top))
                                                      ("l-*-4398"
                                                       "l-*-4396"
                                                       "l-*-4394"
                                                       "l-*-4392"
                                                       "l-*-4390"
                                                       "l-*-4388"
                                                       "l-*-4386")))
                                                   (hygiene guile))
                                                 #(syntax-object
                                                   unquote
                                                   ((top)
                                                    #(ribcage #(p) #((top)) #("l-*-4445"))
                                                    #(ribcage #(p q) #((top) (top)) #("l-*-4439" "l-*-4440"))
                                                    #(ribcage () () ())
                                                    #(ribcage #(p lev) #((top) (top)) #("l-*-4435" "l-*-4436"))
                                                    #(ribcage
                                                      (emit quasivector quasilist* quasiappend quasicons vquasi quasi)
                                                      ((top) (top) (top) (top) (top) (top) (top))
                                                      ("l-*-4398"
                                                       "l-*-4396"
                                                       "l-*-4394"
                                                       "l-*-4392"
                                                       "l-*-4390"
                                                       "l-*-4388"
                                                       "l-*-4386")))
                                                   (hygiene guile)))
                                               (quasi p (- lev 1)))
                                             (vquasi q lev))))
                                       tmp)
                                (let ((tmp ($sc-dispatch
                                             tmp-1
                                             '(#(free-id
                                                 #(syntax-object
                                                   unquote-splicing
                                                   ((top)
                                                    #(ribcage #(p q) #((top) (top)) #("l-*-4439" "l-*-4440"))
                                                    #(ribcage () () ())
                                                    #(ribcage #(p lev) #((top) (top)) #("l-*-4435" "l-*-4436"))
                                                    #(ribcage
                                                      (emit quasivector quasilist* quasiappend quasicons vquasi quasi)
                                                      ((top) (top) (top) (top) (top) (top) (top))
                                                      ("l-*-4398"
                                                       "l-*-4396"
                                                       "l-*-4394"
                                                       "l-*-4392"
                                                       "l-*-4390"
                                                       "l-*-4388"
                                                       "l-*-4386")))
                                                   (hygiene guile)))
                                               .
                                               each-any))))
                                  (if tmp
                                    (apply (lambda (p)
                                             (if (= lev 0)
                                               (quasiappend
                                                 (map (lambda (tmp)
                                                        (list '#(syntax-object
                                                                 "value"
                                                                 ((top)
                                                                  #(ribcage #(p) #((top)) #("l-*-4450"))
                                                                  #(ribcage
                                                                    #(p q)
                                                                    #((top) (top))
                                                                    #("l-*-4439" "l-*-4440"))
                                                                  #(ribcage () () ())
                                                                  #(ribcage
                                                                    #(p lev)
                                                                    #((top) (top))
                                                                    #("l-*-4435" "l-*-4436"))
                                                                  #(ribcage
                                                                    (emit quasivector
                                                                          quasilist*
                                                                          quasiappend
                                                                          quasicons
                                                                          vquasi
                                                                          quasi)
                                                                    ((top) (top) (top) (top) (top) (top) (top))
                                                                    ("l-*-4398"
                                                                     "l-*-4396"
                                                                     "l-*-4394"
                                                                     "l-*-4392"
                                                                     "l-*-4390"
                                                                     "l-*-4388"
                                                                     "l-*-4386")))
                                                                 (hygiene guile))
                                                              tmp))
                                                      p)
                                                 (vquasi q lev))
                                               (quasicons
                                                 (quasicons
                                                   '(#(syntax-object
                                                       "quote"
                                                       ((top)
                                                        #(ribcage #(p) #((top)) #("l-*-4450"))
                                                        #(ribcage #(p q) #((top) (top)) #("l-*-4439" "l-*-4440"))
                                                        #(ribcage () () ())
                                                        #(ribcage #(p lev) #((top) (top)) #("l-*-4435" "l-*-4436"))
                                                        #(ribcage
                                                          (emit quasivector
                                                                quasilist*
                                                                quasiappend
                                                                quasicons
                                                                vquasi
                                                                quasi)
                                                          ((top) (top) (top) (top) (top) (top) (top))
                                                          ("l-*-4398"
                                                           "l-*-4396"
                                                           "l-*-4394"
                                                           "l-*-4392"
                                                           "l-*-4390"
                                                           "l-*-4388"
                                                           "l-*-4386")))
                                                       (hygiene guile))
                                                     #(syntax-object
                                                       unquote-splicing
                                                       ((top)
                                                        #(ribcage #(p) #((top)) #("l-*-4450"))
                                                        #(ribcage #(p q) #((top) (top)) #("l-*-4439" "l-*-4440"))
                                                        #(ribcage () () ())
                                                        #(ribcage #(p lev) #((top) (top)) #("l-*-4435" "l-*-4436"))
                                                        #(ribcage
                                                          (emit quasivector
                                                                quasilist*
                                                                quasiappend
                                                                quasicons
                                                                vquasi
                                                                quasi)
                                                          ((top) (top) (top) (top) (top) (top) (top))
                                                          ("l-*-4398"
                                                           "l-*-4396"
                                                           "l-*-4394"
                                                           "l-*-4392"
                                                           "l-*-4390"
                                                           "l-*-4388"
                                                           "l-*-4386")))
                                                       (hygiene guile)))
                                                   (quasi p (- lev 1)))
                                                 (vquasi q lev))))
                                           tmp)
                                    (quasicons (quasi p lev) (vquasi q lev))))))))
                        tmp-1)
                 (let ((tmp-1 ($sc-dispatch tmp '())))
                   (if tmp-1
                     (apply (lambda ()
                              '(#(syntax-object
                                  "quote"
                                  ((top)
                                   #(ribcage () () ())
                                   #(ribcage #(p lev) #((top) (top)) #("l-*-4435" "l-*-4436"))
                                   #(ribcage
                                     (emit quasivector quasilist* quasiappend quasicons vquasi quasi)
                                     ((top) (top) (top) (top) (top) (top) (top))
                                     ("l-*-4398"
                                      "l-*-4396"
                                      "l-*-4394"
                                      "l-*-4392"
                                      "l-*-4390"
                                      "l-*-4388"
                                      "l-*-4386")))
                                  (hygiene guile))
                                ()))
                            tmp-1)
                     (syntax-violation
                       #f
                       "source expression failed to match any pattern"
                       tmp))))))))
       (quasicons
         (lambda (x y)
           (let ((tmp-1 (list x y)))
             (let ((tmp ($sc-dispatch tmp-1 '(any any))))
               (if tmp
                 (apply (lambda (x y)
                          (let ((tmp y))
                            (let ((tmp-1 ($sc-dispatch tmp '(#(atom "quote") any))))
                              (if tmp-1
                                (apply (lambda (dy)
                                         (let ((tmp x))
                                           (let ((tmp ($sc-dispatch tmp '(#(atom "quote") any))))
                                             (if tmp
                                               (apply (lambda (dx)
                                                        (list '#(syntax-object
                                                                 "quote"
                                                                 ((top)
                                                                  #(ribcage #(dx) #((top)) #("l-*-4472"))
                                                                  #(ribcage #(dy) #((top)) #("l-*-4468"))
                                                                  #(ribcage () () ())
                                                                  #(ribcage
                                                                    #(x y)
                                                                    #((top) (top))
                                                                    #("l-*-4462" "l-*-4463"))
                                                                  #(ribcage () () ())
                                                                  #(ribcage () () ())
                                                                  #(ribcage
                                                                    #(x y)
                                                                    #((top) (top))
                                                                    #("l-*-4457" "l-*-4458"))
                                                                  #(ribcage
                                                                    (emit quasivector
                                                                          quasilist*
                                                                          quasiappend
                                                                          quasicons
                                                                          vquasi
                                                                          quasi)
                                                                    ((top) (top) (top) (top) (top) (top) (top))
                                                                    ("l-*-4398"
                                                                     "l-*-4396"
                                                                     "l-*-4394"
                                                                     "l-*-4392"
                                                                     "l-*-4390"
                                                                     "l-*-4388"
                                                                     "l-*-4386")))
                                                                 (hygiene guile))
                                                              (cons dx dy)))
                                                      tmp)
                                               (if (null? dy)
                                                 (list '#(syntax-object
                                                          "list"
                                                          ((top)
                                                           #(ribcage #(dy) #((top)) #("l-*-4468"))
                                                           #(ribcage () () ())
                                                           #(ribcage #(x y) #((top) (top)) #("l-*-4462" "l-*-4463"))
                                                           #(ribcage () () ())
                                                           #(ribcage () () ())
                                                           #(ribcage #(x y) #((top) (top)) #("l-*-4457" "l-*-4458"))
                                                           #(ribcage
                                                             (emit quasivector
                                                                   quasilist*
                                                                   quasiappend
                                                                   quasicons
                                                                   vquasi
                                                                   quasi)
                                                             ((top) (top) (top) (top) (top) (top) (top))
                                                             ("l-*-4398"
                                                              "l-*-4396"
                                                              "l-*-4394"
                                                              "l-*-4392"
                                                              "l-*-4390"
                                                              "l-*-4388"
                                                              "l-*-4386")))
                                                          (hygiene guile))
                                                       x)
                                                 (list '#(syntax-object
                                                          "list*"
                                                          ((top)
                                                           #(ribcage #(dy) #((top)) #("l-*-4468"))
                                                           #(ribcage () () ())
                                                           #(ribcage #(x y) #((top) (top)) #("l-*-4462" "l-*-4463"))
                                                           #(ribcage () () ())
                                                           #(ribcage () () ())
                                                           #(ribcage #(x y) #((top) (top)) #("l-*-4457" "l-*-4458"))
                                                           #(ribcage
                                                             (emit quasivector
                                                                   quasilist*
                                                                   quasiappend
                                                                   quasicons
                                                                   vquasi
                                                                   quasi)
                                                             ((top) (top) (top) (top) (top) (top) (top))
                                                             ("l-*-4398"
                                                              "l-*-4396"
                                                              "l-*-4394"
                                                              "l-*-4392"
                                                              "l-*-4390"
                                                              "l-*-4388"
                                                              "l-*-4386")))
                                                          (hygiene guile))
                                                       x
                                                       y))))))
                                       tmp-1)
                                (let ((tmp-1 ($sc-dispatch tmp '(#(atom "list") . any))))
                                  (if tmp-1
                                    (apply (lambda (stuff)
                                             (cons '#(syntax-object
                                                      "list"
                                                      ((top)
                                                       #(ribcage #(stuff) #((top)) #("l-*-4475"))
                                                       #(ribcage () () ())
                                                       #(ribcage #(x y) #((top) (top)) #("l-*-4462" "l-*-4463"))
                                                       #(ribcage () () ())
                                                       #(ribcage () () ())
                                                       #(ribcage #(x y) #((top) (top)) #("l-*-4457" "l-*-4458"))
                                                       #(ribcage
                                                         (emit quasivector
                                                               quasilist*
                                                               quasiappend
                                                               quasicons
                                                               vquasi
                                                               quasi)
                                                         ((top) (top) (top) (top) (top) (top) (top))
                                                         ("l-*-4398"
                                                          "l-*-4396"
                                                          "l-*-4394"
                                                          "l-*-4392"
                                                          "l-*-4390"
                                                          "l-*-4388"
                                                          "l-*-4386")))
                                                      (hygiene guile))
                                                   (cons x stuff)))
                                           tmp-1)
                                    (let ((tmp ($sc-dispatch tmp '(#(atom "list*") . any))))
                                      (if tmp
                                        (apply (lambda (stuff)
                                                 (cons '#(syntax-object
                                                          "list*"
                                                          ((top)
                                                           #(ribcage #(stuff) #((top)) #("l-*-4478"))
                                                           #(ribcage () () ())
                                                           #(ribcage #(x y) #((top) (top)) #("l-*-4462" "l-*-4463"))
                                                           #(ribcage () () ())
                                                           #(ribcage () () ())
                                                           #(ribcage #(x y) #((top) (top)) #("l-*-4457" "l-*-4458"))
                                                           #(ribcage
                                                             (emit quasivector
                                                                   quasilist*
                                                                   quasiappend
                                                                   quasicons
                                                                   vquasi
                                                                   quasi)
                                                             ((top) (top) (top) (top) (top) (top) (top))
                                                             ("l-*-4398"
                                                              "l-*-4396"
                                                              "l-*-4394"
                                                              "l-*-4392"
                                                              "l-*-4390"
                                                              "l-*-4388"
                                                              "l-*-4386")))
                                                          (hygiene guile))
                                                       (cons x stuff)))
                                               tmp)
                                        (list '#(syntax-object
                                                 "list*"
                                                 ((top)
                                                  #(ribcage () () ())
                                                  #(ribcage #(x y) #((top) (top)) #("l-*-4462" "l-*-4463"))
                                                  #(ribcage () () ())
                                                  #(ribcage () () ())
                                                  #(ribcage #(x y) #((top) (top)) #("l-*-4457" "l-*-4458"))
                                                  #(ribcage
                                                    (emit quasivector quasilist* quasiappend quasicons vquasi quasi)
                                                    ((top) (top) (top) (top) (top) (top) (top))
                                                    ("l-*-4398"
                                                     "l-*-4396"
                                                     "l-*-4394"
                                                     "l-*-4392"
                                                     "l-*-4390"
                                                     "l-*-4388"
                                                     "l-*-4386")))
                                                 (hygiene guile))
                                              x
                                              y)))))))))
                        tmp)
                 (syntax-violation
                   #f
                   "source expression failed to match any pattern"
                   tmp-1))))))
       (quasiappend
         (lambda (x y)
           (let ((tmp y))
             (let ((tmp ($sc-dispatch tmp '(#(atom "quote") ()))))
               (if tmp
                 (apply (lambda ()
                          (if (null? x)
                            '(#(syntax-object
                                "quote"
                                ((top)
                                 #(ribcage () () ())
                                 #(ribcage #(x y) #((top) (top)) #("l-*-4482" "l-*-4483"))
                                 #(ribcage
                                   (emit quasivector quasilist* quasiappend quasicons vquasi quasi)
                                   ((top) (top) (top) (top) (top) (top) (top))
                                   ("l-*-4398"
                                    "l-*-4396"
                                    "l-*-4394"
                                    "l-*-4392"
                                    "l-*-4390"
                                    "l-*-4388"
                                    "l-*-4386")))
                                (hygiene guile))
                              ())
                            (if (null? (cdr x))
                              (car x)
                              (let ((tmp-1 x))
                                (let ((tmp ($sc-dispatch tmp-1 'each-any)))
                                  (if tmp
                                    (apply (lambda (p)
                                             (cons '#(syntax-object
                                                      "append"
                                                      ((top)
                                                       #(ribcage () () ())
                                                       #(ribcage #(p) #((top)) #("l-*-4490"))
                                                       #(ribcage () () ())
                                                       #(ribcage #(x y) #((top) (top)) #("l-*-4482" "l-*-4483"))
                                                       #(ribcage
                                                         (emit quasivector
                                                               quasilist*
                                                               quasiappend
                                                               quasicons
                                                               vquasi
                                                               quasi)
                                                         ((top) (top) (top) (top) (top) (top) (top))
                                                         ("l-*-4398"
                                                          "l-*-4396"
                                                          "l-*-4394"
                                                          "l-*-4392"
                                                          "l-*-4390"
                                                          "l-*-4388"
                                                          "l-*-4386")))
                                                      (hygiene guile))
                                                   p))
                                           tmp)
                                    (syntax-violation
                                      #f
                                      "source expression failed to match any pattern"
                                      tmp-1)))))))
                        tmp)
                 (if (null? x)
                   y
                   (let ((tmp-1 (list x y)))
                     (let ((tmp ($sc-dispatch tmp-1 '(each-any any))))
                       (if tmp
                         (apply (lambda (p y)
                                  (cons '#(syntax-object
                                           "append"
                                           ((top)
                                            #(ribcage () () ())
                                            #(ribcage #(p y) #((top) (top)) #("l-*-4497" "l-*-4498"))
                                            #(ribcage () () ())
                                            #(ribcage #(x y) #((top) (top)) #("l-*-4482" "l-*-4483"))
                                            #(ribcage
                                              (emit quasivector quasilist* quasiappend quasicons vquasi quasi)
                                              ((top) (top) (top) (top) (top) (top) (top))
                                              ("l-*-4398"
                                               "l-*-4396"
                                               "l-*-4394"
                                               "l-*-4392"
                                               "l-*-4390"
                                               "l-*-4388"
                                               "l-*-4386")))
                                           (hygiene guile))
                                        (append p (list y))))
                                tmp)
                         (syntax-violation
                           #f
                           "source expression failed to match any pattern"
                           tmp-1))))))))))
       (quasilist*
         (lambda (x y)
           (let f ((x x)) (if (null? x) y (quasicons (car x) (f (cdr x)))))))
       (quasivector
         (lambda (x)
           (let ((tmp x))
             (let ((tmp ($sc-dispatch tmp '(#(atom "quote") each-any))))
               (if tmp
                 (apply (lambda (x)
                          (list '#(syntax-object
                                   "quote"
                                   ((top)
                                    #(ribcage #(x) #((top)) #("l-*-4514"))
                                    #(ribcage () () ())
                                    #(ribcage #(x) #((top)) #("l-*-4511"))
                                    #(ribcage
                                      (emit quasivector quasilist* quasiappend quasicons vquasi quasi)
                                      ((top) (top) (top) (top) (top) (top) (top))
                                      ("l-*-4398"
                                       "l-*-4396"
                                       "l-*-4394"
                                       "l-*-4392"
                                       "l-*-4390"
                                       "l-*-4388"
                                       "l-*-4386")))
                                   (hygiene guile))
                                (list->vector x)))
                        tmp)
                 (let f ((y x)
                         (k (lambda (ls)
                              (let ((tmp-1 ls))
                                (let ((tmp ($sc-dispatch tmp-1 'each-any)))
                                  (if tmp
                                    (apply (lambda (t)
                                             (cons '#(syntax-object
                                                      "vector"
                                                      ((top)
                                                       #(ribcage () () ())
                                                       #(ribcage #(t-4525) #((m-*-4526 top)) #("l-*-4530"))
                                                       #(ribcage () () ())
                                                       #(ribcage () () ())
                                                       #(ribcage () () ())
                                                       #(ribcage #(ls) #((top)) #("l-*-4524"))
                                                       #(ribcage () () ())
                                                       #(ribcage #(x) #((top)) #("l-*-4511"))
                                                       #(ribcage
                                                         (emit quasivector
                                                               quasilist*
                                                               quasiappend
                                                               quasicons
                                                               vquasi
                                                               quasi)
                                                         ((top) (top) (top) (top) (top) (top) (top))
                                                         ("l-*-4398"
                                                          "l-*-4396"
                                                          "l-*-4394"
                                                          "l-*-4392"
                                                          "l-*-4390"
                                                          "l-*-4388"
                                                          "l-*-4386")))
                                                      (hygiene guile))
                                                   t))
                                           tmp)
                                    (syntax-violation
                                      #f
                                      "source expression failed to match any pattern"
                                      tmp-1)))))))
                   (let ((tmp y))
                     (let ((tmp-1 ($sc-dispatch tmp '(#(atom "quote") each-any))))
                       (if tmp-1
                         (apply (lambda (y)
                                  (k (map (lambda (tmp)
                                            (list '#(syntax-object
                                                     "quote"
                                                     ((top)
                                                      #(ribcage #(y) #((top)) #("l-*-4535"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(f y k)
                                                        #((top) (top) (top))
                                                        #("l-*-4517" "l-*-4518" "l-*-4519"))
                                                      #(ribcage () () ())
                                                      #(ribcage #(x) #((top)) #("l-*-4511"))
                                                      #(ribcage
                                                        (emit quasivector quasilist* quasiappend quasicons vquasi quasi)
                                                        ((top) (top) (top) (top) (top) (top) (top))
                                                        ("l-*-4398"
                                                         "l-*-4396"
                                                         "l-*-4394"
                                                         "l-*-4392"
                                                         "l-*-4390"
                                                         "l-*-4388"
                                                         "l-*-4386")))
                                                     (hygiene guile))
                                                  tmp))
                                          y)))
                                tmp-1)
                         (let ((tmp-1 ($sc-dispatch tmp '(#(atom "list") . each-any))))
                           (if tmp-1
                             (apply (lambda (y) (k y)) tmp-1)
                             (let ((tmp-1 ($sc-dispatch tmp '(#(atom "list*") . #(each+ any (any) ())))))
                               (if tmp-1
                                 (apply (lambda (y z) (f z (lambda (ls) (k (append y ls))))) tmp-1)
                                 (let ((else tmp))
                                   (let ((tmp x))
                                     (let ((t tmp))
                                       (list '#(syntax-object
                                                "list->vector"
                                                ((top)
                                                 #(ribcage () () ())
                                                 #(ribcage #(t-4552) #((m-*-4553 top)) #("l-*-4556"))
                                                 #(ribcage #(else) #((top)) #("l-*-4550"))
                                                 #(ribcage () () ())
                                                 #(ribcage
                                                   #(f y k)
                                                   #((top) (top) (top))
                                                   #("l-*-4517" "l-*-4518" "l-*-4519"))
                                                 #(ribcage () () ())
                                                 #(ribcage #(x) #((top)) #("l-*-4511"))
                                                 #(ribcage
                                                   (emit quasivector quasilist* quasiappend quasicons vquasi quasi)
                                                   ((top) (top) (top) (top) (top) (top) (top))
                                                   ("l-*-4398"
                                                    "l-*-4396"
                                                    "l-*-4394"
                                                    "l-*-4392"
                                                    "l-*-4390"
                                                    "l-*-4388"
                                                    "l-*-4386")))
                                                (hygiene guile))
                                             t)))))))))))))))))
       (emit (lambda (x)
               (let ((tmp x))
                 (let ((tmp-1 ($sc-dispatch tmp '(#(atom "quote") any))))
                   (if tmp-1
                     (apply (lambda (x)
                              (list '#(syntax-object
                                       quote
                                       ((top)
                                        #(ribcage #(x) #((top)) #("l-*-4562"))
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("l-*-4559"))
                                        #(ribcage
                                          (emit quasivector quasilist* quasiappend quasicons vquasi quasi)
                                          ((top) (top) (top) (top) (top) (top) (top))
                                          ("l-*-4398"
                                           "l-*-4396"
                                           "l-*-4394"
                                           "l-*-4392"
                                           "l-*-4390"
                                           "l-*-4388"
                                           "l-*-4386")))
                                       (hygiene guile))
                                    x))
                            tmp-1)
                     (let ((tmp-1 ($sc-dispatch tmp '(#(atom "list") . each-any))))
                       (if tmp-1
                         (apply (lambda (x)
                                  (let ((tmp-1 (map emit x)))
                                    (let ((tmp ($sc-dispatch tmp-1 'each-any)))
                                      (if tmp
                                        (apply (lambda (t)
                                                 (cons '#(syntax-object
                                                          list
                                                          ((top)
                                                           #(ribcage () () ())
                                                           #(ribcage #(t-4567) #((m-*-4568 top)) #("l-*-4572"))
                                                           #(ribcage #(x) #((top)) #("l-*-4565"))
                                                           #(ribcage () () ())
                                                           #(ribcage #(x) #((top)) #("l-*-4559"))
                                                           #(ribcage
                                                             (emit quasivector
                                                                   quasilist*
                                                                   quasiappend
                                                                   quasicons
                                                                   vquasi
                                                                   quasi)
                                                             ((top) (top) (top) (top) (top) (top) (top))
                                                             ("l-*-4398"
                                                              "l-*-4396"
                                                              "l-*-4394"
                                                              "l-*-4392"
                                                              "l-*-4390"
                                                              "l-*-4388"
                                                              "l-*-4386")))
                                                          (hygiene guile))
                                                       t))
                                               tmp)
                                        (syntax-violation
                                          #f
                                          "source expression failed to match any pattern"
                                          tmp-1)))))
                                tmp-1)
                         (let ((tmp-1 ($sc-dispatch tmp '(#(atom "list*") . #(each+ any (any) ())))))
                           (if tmp-1
                             (apply (lambda (x y)
                                      (let f ((x* x))
                                        (if (null? x*)
                                          (emit y)
                                          (let ((tmp-1 (list (emit (car x*)) (f (cdr x*)))))
                                            (let ((tmp ($sc-dispatch tmp-1 '(any any))))
                                              (if tmp
                                                (apply (lambda (t-1 t)
                                                         (list '#(syntax-object
                                                                  cons
                                                                  ((top)
                                                                   #(ribcage () () ())
                                                                   #(ribcage
                                                                     #(t-4587 t-4586)
                                                                     #((m-*-4588 top) (m-*-4588 top))
                                                                     #("l-*-4592" "l-*-4593"))
                                                                   #(ribcage () () ())
                                                                   #(ribcage
                                                                     #(f x*)
                                                                     #((top) (top))
                                                                     #("l-*-4581" "l-*-4582"))
                                                                   #(ribcage
                                                                     #(x y)
                                                                     #((top) (top))
                                                                     #("l-*-4577" "l-*-4578"))
                                                                   #(ribcage () () ())
                                                                   #(ribcage #(x) #((top)) #("l-*-4559"))
                                                                   #(ribcage
                                                                     (emit quasivector
                                                                           quasilist*
                                                                           quasiappend
                                                                           quasicons
                                                                           vquasi
                                                                           quasi)
                                                                     ((top) (top) (top) (top) (top) (top) (top))
                                                                     ("l-*-4398"
                                                                      "l-*-4396"
                                                                      "l-*-4394"
                                                                      "l-*-4392"
                                                                      "l-*-4390"
                                                                      "l-*-4388"
                                                                      "l-*-4386")))
                                                                  (hygiene guile))
                                                               t-1
                                                               t))
                                                       tmp)
                                                (syntax-violation
                                                  #f
                                                  "source expression failed to match any pattern"
                                                  tmp-1)))))))
                                    tmp-1)
                             (let ((tmp-1 ($sc-dispatch tmp '(#(atom "append") . each-any))))
                               (if tmp-1
                                 (apply (lambda (x)
                                          (let ((tmp-1 (map emit x)))
                                            (let ((tmp ($sc-dispatch tmp-1 'each-any)))
                                              (if tmp
                                                (apply (lambda (t)
                                                         (cons '#(syntax-object
                                                                  append
                                                                  ((top)
                                                                   #(ribcage () () ())
                                                                   #(ribcage #(t-4599) #((m-*-4600 top)) #("l-*-4604"))
                                                                   #(ribcage #(x) #((top)) #("l-*-4597"))
                                                                   #(ribcage () () ())
                                                                   #(ribcage #(x) #((top)) #("l-*-4559"))
                                                                   #(ribcage
                                                                     (emit quasivector
                                                                           quasilist*
                                                                           quasiappend
                                                                           quasicons
                                                                           vquasi
                                                                           quasi)
                                                                     ((top) (top) (top) (top) (top) (top) (top))
                                                                     ("l-*-4398"
                                                                      "l-*-4396"
                                                                      "l-*-4394"
                                                                      "l-*-4392"
                                                                      "l-*-4390"
                                                                      "l-*-4388"
                                                                      "l-*-4386")))
                                                                  (hygiene guile))
                                                               t))
                                                       tmp)
                                                (syntax-violation
                                                  #f
                                                  "source expression failed to match any pattern"
                                                  tmp-1)))))
                                        tmp-1)
                                 (let ((tmp-1 ($sc-dispatch tmp '(#(atom "vector") . each-any))))
                                   (if tmp-1
                                     (apply (lambda (x)
                                              (let ((tmp-1 (map emit x)))
                                                (let ((tmp ($sc-dispatch tmp-1 'each-any)))
                                                  (if tmp
                                                    (apply (lambda (t)
                                                             (cons '#(syntax-object
                                                                      vector
                                                                      ((top)
                                                                       #(ribcage () () ())
                                                                       #(ribcage
                                                                         #(t-4611)
                                                                         #((m-*-4612 top))
                                                                         #("l-*-4616"))
                                                                       #(ribcage #(x) #((top)) #("l-*-4609"))
                                                                       #(ribcage () () ())
                                                                       #(ribcage #(x) #((top)) #("l-*-4559"))
                                                                       #(ribcage
                                                                         (emit quasivector
                                                                               quasilist*
                                                                               quasiappend
                                                                               quasicons
                                                                               vquasi
                                                                               quasi)
                                                                         ((top) (top) (top) (top) (top) (top) (top))
                                                                         ("l-*-4398"
                                                                          "l-*-4396"
                                                                          "l-*-4394"
                                                                          "l-*-4392"
                                                                          "l-*-4390"
                                                                          "l-*-4388"
                                                                          "l-*-4386")))
                                                                      (hygiene guile))
                                                                   t))
                                                           tmp)
                                                    (syntax-violation
                                                      #f
                                                      "source expression failed to match any pattern"
                                                      tmp-1)))))
                                            tmp-1)
                                     (let ((tmp-1 ($sc-dispatch tmp '(#(atom "list->vector") any))))
                                       (if tmp-1
                                         (apply (lambda (x)
                                                  (let ((tmp (emit x)))
                                                    (let ((t tmp))
                                                      (list '#(syntax-object
                                                               list->vector
                                                               ((top)
                                                                #(ribcage () () ())
                                                                #(ribcage #(t-4623) #((m-*-4624 top)) #("l-*-4627"))
                                                                #(ribcage #(x) #((top)) #("l-*-4621"))
                                                                #(ribcage () () ())
                                                                #(ribcage #(x) #((top)) #("l-*-4559"))
                                                                #(ribcage
                                                                  (emit quasivector
                                                                        quasilist*
                                                                        quasiappend
                                                                        quasicons
                                                                        vquasi
                                                                        quasi)
                                                                  ((top) (top) (top) (top) (top) (top) (top))
                                                                  ("l-*-4398"
                                                                   "l-*-4396"
                                                                   "l-*-4394"
                                                                   "l-*-4392"
                                                                   "l-*-4390"
                                                                   "l-*-4388"
                                                                   "l-*-4386")))
                                                               (hygiene guile))
                                                            t))))
                                                tmp-1)
                                         (let ((tmp-1 ($sc-dispatch tmp '(#(atom "value") any))))
                                           (if tmp-1
                                             (apply (lambda (x) x) tmp-1)
                                             (syntax-violation
                                               #f
                                               "source expression failed to match any pattern"
                                               tmp)))))))))))))))))))
      (lambda (x)
        (let ((tmp-1 x))
          (let ((tmp ($sc-dispatch tmp-1 '(_ any))))
            (if tmp
              (apply (lambda (e) (emit (quasi e 0))) tmp)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                tmp-1))))))))

(define include
  (make-syntax-transformer
    'include
    'macro
    (lambda (x)
      (letrec*
        ((read-file
           (lambda (fn k)
             (let ((p (open-input-file fn)))
               (let f ((x (read p)) (result '()))
                 (if (eof-object? x)
                   (begin (close-input-port p) (reverse result))
                   (f (read p) (cons (datum->syntax k x) result))))))))
        (let ((tmp-1 x))
          (let ((tmp ($sc-dispatch tmp-1 '(any any))))
            (if tmp
              (apply (lambda (k filename)
                       (let ((fn (syntax->datum filename)))
                         (let ((tmp-1 (read-file fn filename)))
                           (let ((tmp ($sc-dispatch tmp-1 'each-any)))
                             (if tmp
                               (apply (lambda (exp)
                                        (cons '#(syntax-object
                                                 begin
                                                 ((top)
                                                  #(ribcage () () ())
                                                  #(ribcage #(exp) #((top)) #("l-*-4665"))
                                                  #(ribcage () () ())
                                                  #(ribcage () () ())
                                                  #(ribcage #(fn) #((top)) #("l-*-4660"))
                                                  #(ribcage #(k filename) #((top) (top)) #("l-*-4656" "l-*-4657"))
                                                  #(ribcage (read-file) ((top)) ("l-*-4640"))
                                                  #(ribcage #(x) #((top)) #("l-*-4639")))
                                                 (hygiene guile))
                                              exp))
                                      tmp)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 tmp-1))))))
                     tmp)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                tmp-1))))))))

(define include-from-path
  (make-syntax-transformer
    'include-from-path
    'macro
    (lambda (x)
      (let ((tmp-1 x))
        (let ((tmp ($sc-dispatch tmp-1 '(any any))))
          (if tmp
            (apply (lambda (k filename)
                     (let ((fn (syntax->datum filename)))
                       (let ((tmp (datum->syntax
                                    filename
                                    (let ((t (%search-load-path fn)))
                                      (if t
                                        t
                                        (syntax-violation
                                          'include-from-path
                                          "file not found in path"
                                          x
                                          filename))))))
                         (let ((fn tmp))
                           (list '#(syntax-object
                                    include
                                    ((top)
                                     #(ribcage () () ())
                                     #(ribcage #(fn) #((top)) #("l-*-4680"))
                                     #(ribcage () () ())
                                     #(ribcage () () ())
                                     #(ribcage #(fn) #((top)) #("l-*-4676"))
                                     #(ribcage #(k filename) #((top) (top)) #("l-*-4672" "l-*-4673"))
                                     #(ribcage () () ())
                                     #(ribcage #(x) #((top)) #("l-*-4669")))
                                    (hygiene guile))
                                 fn)))))
                   tmp)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              tmp-1)))))))

(define unquote
  (make-syntax-transformer
    'unquote
    'macro
    (lambda (x)
      (syntax-violation
        'unquote
        "expression not valid outside of quasiquote"
        x))))

(define unquote-splicing
  (make-syntax-transformer
    'unquote-splicing
    'macro
    (lambda (x)
      (syntax-violation
        'unquote-splicing
        "expression not valid outside of quasiquote"
        x))))

(define case
  (make-syntax-transformer
    'case
    'macro
    (lambda (x)
      (let ((tmp-1 x))
        (let ((tmp ($sc-dispatch tmp-1 '(_ any any . each-any))))
          (if tmp
            (apply (lambda (e m1 m2)
                     (let ((tmp (let f ((clause m1) (clauses m2))
                                  (if (null? clauses)
                                    (let ((tmp-1 clause))
                                      (let ((tmp ($sc-dispatch
                                                   tmp-1
                                                   '(#(free-id
                                                       #(syntax-object
                                                         else
                                                         ((top)
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(f clause clauses)
                                                            #((top) (top) (top))
                                                            #("l-*-4704" "l-*-4705" "l-*-4706"))
                                                          #(ribcage
                                                            #(e m1 m2)
                                                            #((top) (top) (top))
                                                            #("l-*-4694" "l-*-4695" "l-*-4696"))
                                                          #(ribcage () () ())
                                                          #(ribcage #(x) #((top)) #("l-*-4691")))
                                                         (hygiene guile)))
                                                     any
                                                     .
                                                     each-any))))
                                        (if tmp
                                          (apply (lambda (e1 e2)
                                                   (cons '#(syntax-object
                                                            begin
                                                            ((top)
                                                             #(ribcage #(e1 e2) #((top) (top)) #("l-*-4713" "l-*-4714"))
                                                             #(ribcage () () ())
                                                             #(ribcage
                                                               #(f clause clauses)
                                                               #((top) (top) (top))
                                                               #("l-*-4704" "l-*-4705" "l-*-4706"))
                                                             #(ribcage
                                                               #(e m1 m2)
                                                               #((top) (top) (top))
                                                               #("l-*-4694" "l-*-4695" "l-*-4696"))
                                                             #(ribcage () () ())
                                                             #(ribcage #(x) #((top)) #("l-*-4691")))
                                                            (hygiene guile))
                                                         (cons e1 e2)))
                                                 tmp)
                                          (let ((tmp ($sc-dispatch tmp-1 '(each-any any . each-any))))
                                            (if tmp
                                              (apply (lambda (k e1 e2)
                                                       (list '#(syntax-object
                                                                if
                                                                ((top)
                                                                 #(ribcage
                                                                   #(k e1 e2)
                                                                   #((top) (top) (top))
                                                                   #("l-*-4719" "l-*-4720" "l-*-4721"))
                                                                 #(ribcage () () ())
                                                                 #(ribcage
                                                                   #(f clause clauses)
                                                                   #((top) (top) (top))
                                                                   #("l-*-4704" "l-*-4705" "l-*-4706"))
                                                                 #(ribcage
                                                                   #(e m1 m2)
                                                                   #((top) (top) (top))
                                                                   #("l-*-4694" "l-*-4695" "l-*-4696"))
                                                                 #(ribcage () () ())
                                                                 #(ribcage #(x) #((top)) #("l-*-4691")))
                                                                (hygiene guile))
                                                             (list '#(syntax-object
                                                                      memv
                                                                      ((top)
                                                                       #(ribcage
                                                                         #(k e1 e2)
                                                                         #((top) (top) (top))
                                                                         #("l-*-4719" "l-*-4720" "l-*-4721"))
                                                                       #(ribcage () () ())
                                                                       #(ribcage
                                                                         #(f clause clauses)
                                                                         #((top) (top) (top))
                                                                         #("l-*-4704" "l-*-4705" "l-*-4706"))
                                                                       #(ribcage
                                                                         #(e m1 m2)
                                                                         #((top) (top) (top))
                                                                         #("l-*-4694" "l-*-4695" "l-*-4696"))
                                                                       #(ribcage () () ())
                                                                       #(ribcage #(x) #((top)) #("l-*-4691")))
                                                                      (hygiene guile))
                                                                   '#(syntax-object
                                                                      t
                                                                      ((top)
                                                                       #(ribcage
                                                                         #(k e1 e2)
                                                                         #((top) (top) (top))
                                                                         #("l-*-4719" "l-*-4720" "l-*-4721"))
                                                                       #(ribcage () () ())
                                                                       #(ribcage
                                                                         #(f clause clauses)
                                                                         #((top) (top) (top))
                                                                         #("l-*-4704" "l-*-4705" "l-*-4706"))
                                                                       #(ribcage
                                                                         #(e m1 m2)
                                                                         #((top) (top) (top))
                                                                         #("l-*-4694" "l-*-4695" "l-*-4696"))
                                                                       #(ribcage () () ())
                                                                       #(ribcage #(x) #((top)) #("l-*-4691")))
                                                                      (hygiene guile))
                                                                   (list '#(syntax-object
                                                                            quote
                                                                            ((top)
                                                                             #(ribcage
                                                                               #(k e1 e2)
                                                                               #((top) (top) (top))
                                                                               #("l-*-4719" "l-*-4720" "l-*-4721"))
                                                                             #(ribcage () () ())
                                                                             #(ribcage
                                                                               #(f clause clauses)
                                                                               #((top) (top) (top))
                                                                               #("l-*-4704" "l-*-4705" "l-*-4706"))
                                                                             #(ribcage
                                                                               #(e m1 m2)
                                                                               #((top) (top) (top))
                                                                               #("l-*-4694" "l-*-4695" "l-*-4696"))
                                                                             #(ribcage () () ())
                                                                             #(ribcage #(x) #((top)) #("l-*-4691")))
                                                                            (hygiene guile))
                                                                         k))
                                                             (cons '#(syntax-object
                                                                      begin
                                                                      ((top)
                                                                       #(ribcage
                                                                         #(k e1 e2)
                                                                         #((top) (top) (top))
                                                                         #("l-*-4719" "l-*-4720" "l-*-4721"))
                                                                       #(ribcage () () ())
                                                                       #(ribcage
                                                                         #(f clause clauses)
                                                                         #((top) (top) (top))
                                                                         #("l-*-4704" "l-*-4705" "l-*-4706"))
                                                                       #(ribcage
                                                                         #(e m1 m2)
                                                                         #((top) (top) (top))
                                                                         #("l-*-4694" "l-*-4695" "l-*-4696"))
                                                                       #(ribcage () () ())
                                                                       #(ribcage #(x) #((top)) #("l-*-4691")))
                                                                      (hygiene guile))
                                                                   (cons e1 e2))))
                                                     tmp)
                                              (syntax-violation 'case "bad clause" x clause))))))
                                    (let ((tmp (f (car clauses) (cdr clauses))))
                                      (let ((rest tmp))
                                        (let ((tmp clause))
                                          (let ((tmp ($sc-dispatch tmp '(each-any any . each-any))))
                                            (if tmp
                                              (apply (lambda (k e1 e2)
                                                       (list '#(syntax-object
                                                                if
                                                                ((top)
                                                                 #(ribcage
                                                                   #(k e1 e2)
                                                                   #((top) (top) (top))
                                                                   #("l-*-4733" "l-*-4734" "l-*-4735"))
                                                                 #(ribcage () () ())
                                                                 #(ribcage #(rest) #((top)) #("l-*-4729"))
                                                                 #(ribcage () () ())
                                                                 #(ribcage
                                                                   #(f clause clauses)
                                                                   #((top) (top) (top))
                                                                   #("l-*-4704" "l-*-4705" "l-*-4706"))
                                                                 #(ribcage
                                                                   #(e m1 m2)
                                                                   #((top) (top) (top))
                                                                   #("l-*-4694" "l-*-4695" "l-*-4696"))
                                                                 #(ribcage () () ())
                                                                 #(ribcage #(x) #((top)) #("l-*-4691")))
                                                                (hygiene guile))
                                                             (list '#(syntax-object
                                                                      memv
                                                                      ((top)
                                                                       #(ribcage
                                                                         #(k e1 e2)
                                                                         #((top) (top) (top))
                                                                         #("l-*-4733" "l-*-4734" "l-*-4735"))
                                                                       #(ribcage () () ())
                                                                       #(ribcage #(rest) #((top)) #("l-*-4729"))
                                                                       #(ribcage () () ())
                                                                       #(ribcage
                                                                         #(f clause clauses)
                                                                         #((top) (top) (top))
                                                                         #("l-*-4704" "l-*-4705" "l-*-4706"))
                                                                       #(ribcage
                                                                         #(e m1 m2)
                                                                         #((top) (top) (top))
                                                                         #("l-*-4694" "l-*-4695" "l-*-4696"))
                                                                       #(ribcage () () ())
                                                                       #(ribcage #(x) #((top)) #("l-*-4691")))
                                                                      (hygiene guile))
                                                                   '#(syntax-object
                                                                      t
                                                                      ((top)
                                                                       #(ribcage
                                                                         #(k e1 e2)
                                                                         #((top) (top) (top))
                                                                         #("l-*-4733" "l-*-4734" "l-*-4735"))
                                                                       #(ribcage () () ())
                                                                       #(ribcage #(rest) #((top)) #("l-*-4729"))
                                                                       #(ribcage () () ())
                                                                       #(ribcage
                                                                         #(f clause clauses)
                                                                         #((top) (top) (top))
                                                                         #("l-*-4704" "l-*-4705" "l-*-4706"))
                                                                       #(ribcage
                                                                         #(e m1 m2)
                                                                         #((top) (top) (top))
                                                                         #("l-*-4694" "l-*-4695" "l-*-4696"))
                                                                       #(ribcage () () ())
                                                                       #(ribcage #(x) #((top)) #("l-*-4691")))
                                                                      (hygiene guile))
                                                                   (list '#(syntax-object
                                                                            quote
                                                                            ((top)
                                                                             #(ribcage
                                                                               #(k e1 e2)
                                                                               #((top) (top) (top))
                                                                               #("l-*-4733" "l-*-4734" "l-*-4735"))
                                                                             #(ribcage () () ())
                                                                             #(ribcage #(rest) #((top)) #("l-*-4729"))
                                                                             #(ribcage () () ())
                                                                             #(ribcage
                                                                               #(f clause clauses)
                                                                               #((top) (top) (top))
                                                                               #("l-*-4704" "l-*-4705" "l-*-4706"))
                                                                             #(ribcage
                                                                               #(e m1 m2)
                                                                               #((top) (top) (top))
                                                                               #("l-*-4694" "l-*-4695" "l-*-4696"))
                                                                             #(ribcage () () ())
                                                                             #(ribcage #(x) #((top)) #("l-*-4691")))
                                                                            (hygiene guile))
                                                                         k))
                                                             (cons '#(syntax-object
                                                                      begin
                                                                      ((top)
                                                                       #(ribcage
                                                                         #(k e1 e2)
                                                                         #((top) (top) (top))
                                                                         #("l-*-4733" "l-*-4734" "l-*-4735"))
                                                                       #(ribcage () () ())
                                                                       #(ribcage #(rest) #((top)) #("l-*-4729"))
                                                                       #(ribcage () () ())
                                                                       #(ribcage
                                                                         #(f clause clauses)
                                                                         #((top) (top) (top))
                                                                         #("l-*-4704" "l-*-4705" "l-*-4706"))
                                                                       #(ribcage
                                                                         #(e m1 m2)
                                                                         #((top) (top) (top))
                                                                         #("l-*-4694" "l-*-4695" "l-*-4696"))
                                                                       #(ribcage () () ())
                                                                       #(ribcage #(x) #((top)) #("l-*-4691")))
                                                                      (hygiene guile))
                                                                   (cons e1 e2))
                                                             rest))
                                                     tmp)
                                              (syntax-violation 'case "bad clause" x clause))))))))))
                       (let ((body tmp))
                         (list '#(syntax-object
                                  let
                                  ((top)
                                   #(ribcage () () ())
                                   #(ribcage #(body) #((top)) #("l-*-4702"))
                                   #(ribcage
                                     #(e m1 m2)
                                     #((top) (top) (top))
                                     #("l-*-4694" "l-*-4695" "l-*-4696"))
                                   #(ribcage () () ())
                                   #(ribcage #(x) #((top)) #("l-*-4691")))
                                  (hygiene guile))
                               (list (list '#(syntax-object
                                              t
                                              ((top)
                                               #(ribcage () () ())
                                               #(ribcage #(body) #((top)) #("l-*-4702"))
                                               #(ribcage
                                                 #(e m1 m2)
                                                 #((top) (top) (top))
                                                 #("l-*-4694" "l-*-4695" "l-*-4696"))
                                               #(ribcage () () ())
                                               #(ribcage #(x) #((top)) #("l-*-4691")))
                                              (hygiene guile))
                                           e))
                               body))))
                   tmp)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              tmp-1)))))))

(define make-variable-transformer
  (lambda (proc)
    (if (procedure? proc)
      (let ((trans (lambda (x) (proc x))))
        (set-procedure-property! trans 'variable-transformer #t)
        trans)
      (error "variable transformer not a procedure" proc))))

(define identifier-syntax
  (make-syntax-transformer
    'identifier-syntax
    'macro
    (lambda (x)
      (let ((tmp-1 x))
        (let ((tmp ($sc-dispatch tmp-1 '(_ any))))
          (if tmp
            (apply (lambda (e)
                     (list '#(syntax-object
                              lambda
                              ((top)
                               #(ribcage #(e) #((top)) #("l-*-4751"))
                               #(ribcage () () ())
                               #(ribcage #(x) #((top)) #("l-*-4748")))
                              (hygiene guile))
                           '(#(syntax-object
                               x
                               ((top)
                                #(ribcage #(e) #((top)) #("l-*-4751"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("l-*-4748")))
                               (hygiene guile)))
                           '#((#(syntax-object
                                 macro-type
                                 ((top)
                                  #(ribcage #(e) #((top)) #("l-*-4751"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("l-*-4748")))
                                 (hygiene guile))
                               .
                               #(syntax-object
                                 identifier-syntax
                                 ((top)
                                  #(ribcage #(e) #((top)) #("l-*-4751"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("l-*-4748")))
                                 (hygiene guile))))
                           (list '#(syntax-object
                                    syntax-case
                                    ((top)
                                     #(ribcage #(e) #((top)) #("l-*-4751"))
                                     #(ribcage () () ())
                                     #(ribcage #(x) #((top)) #("l-*-4748")))
                                    (hygiene guile))
                                 '#(syntax-object
                                    x
                                    ((top)
                                     #(ribcage #(e) #((top)) #("l-*-4751"))
                                     #(ribcage () () ())
                                     #(ribcage #(x) #((top)) #("l-*-4748")))
                                    (hygiene guile))
                                 '()
                                 (list '#(syntax-object
                                          id
                                          ((top)
                                           #(ribcage #(e) #((top)) #("l-*-4751"))
                                           #(ribcage () () ())
                                           #(ribcage #(x) #((top)) #("l-*-4748")))
                                          (hygiene guile))
                                       '(#(syntax-object
                                           identifier?
                                           ((top)
                                            #(ribcage #(e) #((top)) #("l-*-4751"))
                                            #(ribcage () () ())
                                            #(ribcage #(x) #((top)) #("l-*-4748")))
                                           (hygiene guile))
                                         (#(syntax-object
                                            syntax
                                            ((top)
                                             #(ribcage #(e) #((top)) #("l-*-4751"))
                                             #(ribcage () () ())
                                             #(ribcage #(x) #((top)) #("l-*-4748")))
                                            (hygiene guile))
                                          #(syntax-object
                                            id
                                            ((top)
                                             #(ribcage #(e) #((top)) #("l-*-4751"))
                                             #(ribcage () () ())
                                             #(ribcage #(x) #((top)) #("l-*-4748")))
                                            (hygiene guile))))
                                       (list '#(syntax-object
                                                syntax
                                                ((top)
                                                 #(ribcage #(e) #((top)) #("l-*-4751"))
                                                 #(ribcage () () ())
                                                 #(ribcage #(x) #((top)) #("l-*-4748")))
                                                (hygiene guile))
                                             e))
                                 (list '(#(syntax-object
                                           _
                                           ((top)
                                            #(ribcage #(e) #((top)) #("l-*-4751"))
                                            #(ribcage () () ())
                                            #(ribcage #(x) #((top)) #("l-*-4748")))
                                           (hygiene guile))
                                         #(syntax-object
                                           x
                                           ((top)
                                            #(ribcage #(e) #((top)) #("l-*-4751"))
                                            #(ribcage () () ())
                                            #(ribcage #(x) #((top)) #("l-*-4748")))
                                           (hygiene guile))
                                         #(syntax-object
                                           ...
                                           ((top)
                                            #(ribcage #(e) #((top)) #("l-*-4751"))
                                            #(ribcage () () ())
                                            #(ribcage #(x) #((top)) #("l-*-4748")))
                                           (hygiene guile)))
                                       (list '#(syntax-object
                                                syntax
                                                ((top)
                                                 #(ribcage #(e) #((top)) #("l-*-4751"))
                                                 #(ribcage () () ())
                                                 #(ribcage #(x) #((top)) #("l-*-4748")))
                                                (hygiene guile))
                                             (cons e
                                                   '(#(syntax-object
                                                       x
                                                       ((top)
                                                        #(ribcage #(e) #((top)) #("l-*-4751"))
                                                        #(ribcage () () ())
                                                        #(ribcage #(x) #((top)) #("l-*-4748")))
                                                       (hygiene guile))
                                                     #(syntax-object
                                                       ...
                                                       ((top)
                                                        #(ribcage #(e) #((top)) #("l-*-4751"))
                                                        #(ribcage () () ())
                                                        #(ribcage #(x) #((top)) #("l-*-4748")))
                                                       (hygiene guile)))))))))
                   tmp)
            (let ((tmp ($sc-dispatch
                         tmp-1
                         '(_ (any any)
                             ((#(free-id
                                 #(syntax-object
                                   set!
                                   ((top) #(ribcage () () ()) #(ribcage #(x) #((top)) #("l-*-4748")))
                                   (hygiene guile)))
                               any
                               any)
                              any)))))
              (if (if tmp
                    (apply (lambda (id exp1 var val exp2)
                             (if (identifier? id) (identifier? var) #f))
                           tmp)
                    #f)
                (apply (lambda (id exp1 var val exp2)
                         (list '#(syntax-object
                                  make-variable-transformer
                                  ((top)
                                   #(ribcage
                                     #(id exp1 var val exp2)
                                     #((top) (top) (top) (top) (top))
                                     #("l-*-4766" "l-*-4767" "l-*-4768" "l-*-4769" "l-*-4770"))
                                   #(ribcage () () ())
                                   #(ribcage #(x) #((top)) #("l-*-4748")))
                                  (hygiene guile))
                               (list '#(syntax-object
                                        lambda
                                        ((top)
                                         #(ribcage
                                           #(id exp1 var val exp2)
                                           #((top) (top) (top) (top) (top))
                                           #("l-*-4766" "l-*-4767" "l-*-4768" "l-*-4769" "l-*-4770"))
                                         #(ribcage () () ())
                                         #(ribcage #(x) #((top)) #("l-*-4748")))
                                        (hygiene guile))
                                     '(#(syntax-object
                                         x
                                         ((top)
                                          #(ribcage
                                            #(id exp1 var val exp2)
                                            #((top) (top) (top) (top) (top))
                                            #("l-*-4766" "l-*-4767" "l-*-4768" "l-*-4769" "l-*-4770"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("l-*-4748")))
                                         (hygiene guile)))
                                     '#((#(syntax-object
                                           macro-type
                                           ((top)
                                            #(ribcage
                                              #(id exp1 var val exp2)
                                              #((top) (top) (top) (top) (top))
                                              #("l-*-4766" "l-*-4767" "l-*-4768" "l-*-4769" "l-*-4770"))
                                            #(ribcage () () ())
                                            #(ribcage #(x) #((top)) #("l-*-4748")))
                                           (hygiene guile))
                                         .
                                         #(syntax-object
                                           variable-transformer
                                           ((top)
                                            #(ribcage
                                              #(id exp1 var val exp2)
                                              #((top) (top) (top) (top) (top))
                                              #("l-*-4766" "l-*-4767" "l-*-4768" "l-*-4769" "l-*-4770"))
                                            #(ribcage () () ())
                                            #(ribcage #(x) #((top)) #("l-*-4748")))
                                           (hygiene guile))))
                                     (list '#(syntax-object
                                              syntax-case
                                              ((top)
                                               #(ribcage
                                                 #(id exp1 var val exp2)
                                                 #((top) (top) (top) (top) (top))
                                                 #("l-*-4766" "l-*-4767" "l-*-4768" "l-*-4769" "l-*-4770"))
                                               #(ribcage () () ())
                                               #(ribcage #(x) #((top)) #("l-*-4748")))
                                              (hygiene guile))
                                           '#(syntax-object
                                              x
                                              ((top)
                                               #(ribcage
                                                 #(id exp1 var val exp2)
                                                 #((top) (top) (top) (top) (top))
                                                 #("l-*-4766" "l-*-4767" "l-*-4768" "l-*-4769" "l-*-4770"))
                                               #(ribcage () () ())
                                               #(ribcage #(x) #((top)) #("l-*-4748")))
                                              (hygiene guile))
                                           '(#(syntax-object
                                               set!
                                               ((top)
                                                #(ribcage
                                                  #(id exp1 var val exp2)
                                                  #((top) (top) (top) (top) (top))
                                                  #("l-*-4766" "l-*-4767" "l-*-4768" "l-*-4769" "l-*-4770"))
                                                #(ribcage () () ())
                                                #(ribcage #(x) #((top)) #("l-*-4748")))
                                               (hygiene guile)))
                                           (list (list '#(syntax-object
                                                          set!
                                                          ((top)
                                                           #(ribcage
                                                             #(id exp1 var val exp2)
                                                             #((top) (top) (top) (top) (top))
                                                             #("l-*-4766" "l-*-4767" "l-*-4768" "l-*-4769" "l-*-4770"))
                                                           #(ribcage () () ())
                                                           #(ribcage #(x) #((top)) #("l-*-4748")))
                                                          (hygiene guile))
                                                       var
                                                       val)
                                                 (list '#(syntax-object
                                                          syntax
                                                          ((top)
                                                           #(ribcage
                                                             #(id exp1 var val exp2)
                                                             #((top) (top) (top) (top) (top))
                                                             #("l-*-4766" "l-*-4767" "l-*-4768" "l-*-4769" "l-*-4770"))
                                                           #(ribcage () () ())
                                                           #(ribcage #(x) #((top)) #("l-*-4748")))
                                                          (hygiene guile))
                                                       exp2))
                                           (list (cons id
                                                       '(#(syntax-object
                                                           x
                                                           ((top)
                                                            #(ribcage
                                                              #(id exp1 var val exp2)
                                                              #((top) (top) (top) (top) (top))
                                                              #("l-*-4766" "l-*-4767" "l-*-4768" "l-*-4769" "l-*-4770"))
                                                            #(ribcage () () ())
                                                            #(ribcage #(x) #((top)) #("l-*-4748")))
                                                           (hygiene guile))
                                                         #(syntax-object
                                                           ...
                                                           ((top)
                                                            #(ribcage
                                                              #(id exp1 var val exp2)
                                                              #((top) (top) (top) (top) (top))
                                                              #("l-*-4766" "l-*-4767" "l-*-4768" "l-*-4769" "l-*-4770"))
                                                            #(ribcage () () ())
                                                            #(ribcage #(x) #((top)) #("l-*-4748")))
                                                           (hygiene guile))))
                                                 (list '#(syntax-object
                                                          syntax
                                                          ((top)
                                                           #(ribcage
                                                             #(id exp1 var val exp2)
                                                             #((top) (top) (top) (top) (top))
                                                             #("l-*-4766" "l-*-4767" "l-*-4768" "l-*-4769" "l-*-4770"))
                                                           #(ribcage () () ())
                                                           #(ribcage #(x) #((top)) #("l-*-4748")))
                                                          (hygiene guile))
                                                       (cons exp1
                                                             '(#(syntax-object
                                                                 x
                                                                 ((top)
                                                                  #(ribcage
                                                                    #(id exp1 var val exp2)
                                                                    #((top) (top) (top) (top) (top))
                                                                    #("l-*-4766"
                                                                      "l-*-4767"
                                                                      "l-*-4768"
                                                                      "l-*-4769"
                                                                      "l-*-4770"))
                                                                  #(ribcage () () ())
                                                                  #(ribcage #(x) #((top)) #("l-*-4748")))
                                                                 (hygiene guile))
                                                               #(syntax-object
                                                                 ...
                                                                 ((top)
                                                                  #(ribcage
                                                                    #(id exp1 var val exp2)
                                                                    #((top) (top) (top) (top) (top))
                                                                    #("l-*-4766"
                                                                      "l-*-4767"
                                                                      "l-*-4768"
                                                                      "l-*-4769"
                                                                      "l-*-4770"))
                                                                  #(ribcage () () ())
                                                                  #(ribcage #(x) #((top)) #("l-*-4748")))
                                                                 (hygiene guile))))))
                                           (list id
                                                 (list '#(syntax-object
                                                          identifier?
                                                          ((top)
                                                           #(ribcage
                                                             #(id exp1 var val exp2)
                                                             #((top) (top) (top) (top) (top))
                                                             #("l-*-4766" "l-*-4767" "l-*-4768" "l-*-4769" "l-*-4770"))
                                                           #(ribcage () () ())
                                                           #(ribcage #(x) #((top)) #("l-*-4748")))
                                                          (hygiene guile))
                                                       (list '#(syntax-object
                                                                syntax
                                                                ((top)
                                                                 #(ribcage
                                                                   #(id exp1 var val exp2)
                                                                   #((top) (top) (top) (top) (top))
                                                                   #("l-*-4766"
                                                                     "l-*-4767"
                                                                     "l-*-4768"
                                                                     "l-*-4769"
                                                                     "l-*-4770"))
                                                                 #(ribcage () () ())
                                                                 #(ribcage #(x) #((top)) #("l-*-4748")))
                                                                (hygiene guile))
                                                             id))
                                                 (list '#(syntax-object
                                                          syntax
                                                          ((top)
                                                           #(ribcage
                                                             #(id exp1 var val exp2)
                                                             #((top) (top) (top) (top) (top))
                                                             #("l-*-4766" "l-*-4767" "l-*-4768" "l-*-4769" "l-*-4770"))
                                                           #(ribcage () () ())
                                                           #(ribcage #(x) #((top)) #("l-*-4748")))
                                                          (hygiene guile))
                                                       exp1))))))
                       tmp)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  tmp-1)))))))))

(define define*
  (make-syntax-transformer
    'define*
    'macro
    (lambda (x)
      (let ((tmp-1 x))
        (let ((tmp ($sc-dispatch tmp-1 '(_ (any . any) any . each-any))))
          (if tmp
            (apply (lambda (id args b0 b1)
                     (list '#(syntax-object
                              define
                              ((top)
                               #(ribcage
                                 #(id args b0 b1)
                                 #((top) (top) (top) (top))
                                 #("l-*-4780" "l-*-4781" "l-*-4782" "l-*-4783"))
                               #(ribcage () () ())
                               #(ribcage #(x) #((top)) #("l-*-4777")))
                              (hygiene guile))
                           id
                           (cons '#(syntax-object
                                    lambda*
                                    ((top)
                                     #(ribcage
                                       #(id args b0 b1)
                                       #((top) (top) (top) (top))
                                       #("l-*-4780" "l-*-4781" "l-*-4782" "l-*-4783"))
                                     #(ribcage () () ())
                                     #(ribcage #(x) #((top)) #("l-*-4777")))
                                    (hygiene guile))
                                 (cons args (cons b0 b1)))))
                   tmp)
            (let ((tmp ($sc-dispatch tmp-1 '(_ any any))))
              (if (if tmp (apply (lambda (id val) (identifier? id)) tmp) #f)
                (apply (lambda (id val)
                         (list '#(syntax-object
                                  define
                                  ((top)
                                   #(ribcage #(id val) #((top) (top)) #("l-*-4794" "l-*-4795"))
                                   #(ribcage () () ())
                                   #(ribcage #(x) #((top)) #("l-*-4777")))
                                  (hygiene guile))
                               id
                               val))
                       tmp)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  tmp-1)))))))))

