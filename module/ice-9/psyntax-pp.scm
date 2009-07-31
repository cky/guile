(eval-when (compile) (set-current-module (resolve-module (quote (guile)))))
(if #f #f)

(letrec ((and-map*17
           (lambda (f57 first56 . rest55)
             (let ((t58 (null? first56)))
               (if t58
                 t58
                 (if (null? rest55)
                   (letrec ((andmap59
                              (lambda (first60)
                                (let ((x61 (car first60))
                                      (first62 (cdr first60)))
                                  (if (null? first62)
                                    (f57 x61)
                                    (if (f57 x61) (andmap59 first62) #f))))))
                     (andmap59 first56))
                   (letrec ((andmap63
                              (lambda (first64 rest65)
                                (let ((x66 (car first64))
                                      (xr67 (map car rest65))
                                      (first68 (cdr first64))
                                      (rest69 (map cdr rest65)))
                                  (if (null? first68)
                                    (apply f57 (cons x66 xr67))
                                    (if (apply f57 (cons x66 xr67))
                                      (andmap63 first68 rest69)
                                      #f))))))
                     (andmap63 first56 rest55))))))))
  (letrec ((lambda-var-list163
             (lambda (vars287)
               (letrec ((lvl288
                          (lambda (vars289 ls290 w291)
                            (if (pair? vars289)
                              (lvl288
                                (cdr vars289)
                                (cons (wrap143 (car vars289) w291 #f) ls290)
                                w291)
                              (if (id?115 vars289)
                                (cons (wrap143 vars289 w291 #f) ls290)
                                (if (null? vars289)
                                  ls290
                                  (if (syntax-object?99 vars289)
                                    (lvl288
                                      (syntax-object-expression100 vars289)
                                      ls290
                                      (join-wraps134
                                        w291
                                        (syntax-object-wrap101 vars289)))
                                    (cons vars289 ls290))))))))
                 (lvl288 vars287 (quote ()) (quote (()))))))
           (gen-var162
             (lambda (id292)
               (let ((id293 (if (syntax-object?99 id292)
                              (syntax-object-expression100 id292)
                              id292)))
                 (gensym
                   (string-append (symbol->string id293) " ")))))
           (strip161
             (lambda (x294 w295)
               (if (memq (quote top) (wrap-marks118 w295))
                 x294
                 (letrec ((f296 (lambda (x297)
                                  (if (syntax-object?99 x297)
                                    (strip161
                                      (syntax-object-expression100 x297)
                                      (syntax-object-wrap101 x297))
                                    (if (pair? x297)
                                      (let ((a298 (f296 (car x297)))
                                            (d299 (f296 (cdr x297))))
                                        (if (if (eq? a298 (car x297))
                                              (eq? d299 (cdr x297))
                                              #f)
                                          x297
                                          (cons a298 d299)))
                                      (if (vector? x297)
                                        (let ((old300 (vector->list x297)))
                                          (let ((new301 (map f296 old300)))
                                            (if (and-map*17 eq? old300 new301)
                                              x297
                                              (list->vector new301))))
                                        x297))))))
                   (f296 x294)))))
           (ellipsis?160
             (lambda (x302)
               (if (nonsymbol-id?114 x302)
                 (free-id=?138
                   x302
                   '#(syntax-object
                      ...
                      ((top)
                       #(ribcage () () ())
                       #(ribcage () () ())
                       #(ribcage #(x) #((top)) #("i"))
                       #(ribcage
                         (lambda-var-list
                           gen-var
                           strip
                           ellipsis?
                           chi-void
                           eval-local-transformer
                           chi-local-syntax
                           chi-lambda-clause
                           chi-body
                           chi-macro
                           chi-application
                           chi-expr
                           chi
                           chi-top
                           syntax-type
                           chi-when-list
                           chi-install-global
                           chi-top-sequence
                           chi-sequence
                           source-wrap
                           wrap
                           bound-id-member?
                           distinct-bound-ids?
                           valid-bound-ids?
                           bound-id=?
                           free-id=?
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
                           build-lambda
                           build-global-definition
                           maybe-name-value!
                           build-global-assignment
                           build-global-reference
                           analyze-variable
                           build-lexical-assignment
                           build-lexical-reference
                           build-conditional
                           build-application
                           build-void
                           decorate-source
                           get-global-definition-hook
                           put-global-definition-hook
                           gensym-hook
                           local-eval-hook
                           top-level-eval-hook
                           fx<
                           fx=
                           fx-
                           fx+
                           *mode*
                           noexpand)
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
                          (top))
                         ("i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"
                          "i"))
                       #(ribcage
                         (define-structure and-map*)
                         ((top) (top))
                         ("i" "i")))
                      (hygiene guile)))
                 #f)))
           (chi-void159 (lambda () (build-void81 #f)))
           (eval-local-transformer158
             (lambda (expanded303 mod304)
               (let ((p305 (local-eval-hook77 expanded303 mod304)))
                 (if (procedure? p305)
                   p305
                   (syntax-violation
                     #f
                     "nonprocedure transformer"
                     p305)))))
           (chi-local-syntax157
             (lambda (rec?306 e307 r308 w309 s310 mod311 k312)
               ((lambda (tmp313)
                  ((lambda (tmp314)
                     (if tmp314
                       (apply (lambda (_315 id316 val317 e1318 e2319)
                                (let ((ids320 id316))
                                  (if (not (valid-bound-ids?140 ids320))
                                    (syntax-violation
                                      #f
                                      "duplicate bound keyword"
                                      e307)
                                    (let ((labels322 (gen-labels121 ids320)))
                                      (let ((new-w323
                                              (make-binding-wrap132
                                                ids320
                                                labels322
                                                w309)))
                                        (k312 (cons e1318 e2319)
                                              (extend-env109
                                                labels322
                                                (let ((w325 (if rec?306
                                                              new-w323
                                                              w309))
                                                      (trans-r326
                                                        (macros-only-env111
                                                          r308)))
                                                  (map (lambda (x327)
                                                         (cons 'macro
                                                               (eval-local-transformer158
                                                                 (chi151
                                                                   x327
                                                                   trans-r326
                                                                   w325
                                                                   mod311)
                                                                 mod311)))
                                                       val317))
                                                r308)
                                              new-w323
                                              s310
                                              mod311))))))
                              tmp314)
                       ((lambda (_329)
                          (syntax-violation
                            #f
                            "bad local syntax definition"
                            (source-wrap144 e307 w309 s310 mod311)))
                        tmp313)))
                   ($sc-dispatch
                     tmp313
                     '(any #(each (any any)) any . each-any))))
                e307)))
           (chi-lambda-clause156
             (lambda (e330 docstring331 c332 r333 w334 mod335 k336)
               ((lambda (tmp337)
                  ((lambda (tmp338)
                     (if (if tmp338
                           (apply (lambda (args339 doc340 e1341 e2342)
                                    (if (string? (syntax->datum doc340))
                                      (not docstring331)
                                      #f))
                                  tmp338)
                           #f)
                       (apply (lambda (args343 doc344 e1345 e2346)
                                (chi-lambda-clause156
                                  e330
                                  doc344
                                  (cons args343 (cons e1345 e2346))
                                  r333
                                  w334
                                  mod335
                                  k336))
                              tmp338)
                       ((lambda (tmp348)
                          (if tmp348
                            (apply (lambda (id349 e1350 e2351)
                                     (let ((ids352 id349))
                                       (if (not (valid-bound-ids?140 ids352))
                                         (syntax-violation
                                           'lambda
                                           "invalid parameter list"
                                           e330)
                                         (let ((labels354
                                                 (gen-labels121 ids352))
                                               (new-vars355
                                                 (map gen-var162 ids352)))
                                           (k336 (map syntax->datum ids352)
                                                 new-vars355
                                                 (if docstring331
                                                   (syntax->datum docstring331)
                                                   #f)
                                                 (chi-body155
                                                   (cons e1350 e2351)
                                                   e330
                                                   (extend-var-env110
                                                     labels354
                                                     new-vars355
                                                     r333)
                                                   (make-binding-wrap132
                                                     ids352
                                                     labels354
                                                     w334)
                                                   mod335))))))
                                   tmp348)
                            ((lambda (tmp357)
                               (if tmp357
                                 (apply (lambda (ids358 e1359 e2360)
                                          (let ((old-ids361
                                                  (lambda-var-list163 ids358)))
                                            (if (not (valid-bound-ids?140
                                                       old-ids361))
                                              (syntax-violation
                                                'lambda
                                                "invalid parameter list"
                                                e330)
                                              (let ((labels362
                                                      (gen-labels121
                                                        old-ids361))
                                                    (new-vars363
                                                      (map gen-var162
                                                           old-ids361)))
                                                (k336 (letrec ((f364 (lambda (ls1365
                                                                              ls2366)
                                                                       (if (null? ls1365)
                                                                         (syntax->datum
                                                                           ls2366)
                                                                         (f364 (cdr ls1365)
                                                                               (cons (syntax->datum
                                                                                       (car ls1365))
                                                                                     ls2366))))))
                                                        (f364 (cdr old-ids361)
                                                              (car old-ids361)))
                                                      (letrec ((f367 (lambda (ls1368
                                                                              ls2369)
                                                                       (if (null? ls1368)
                                                                         ls2369
                                                                         (f367 (cdr ls1368)
                                                                               (cons (car ls1368)
                                                                                     ls2369))))))
                                                        (f367 (cdr new-vars363)
                                                              (car new-vars363)))
                                                      (if docstring331
                                                        (syntax->datum
                                                          docstring331)
                                                        #f)
                                                      (chi-body155
                                                        (cons e1359 e2360)
                                                        e330
                                                        (extend-var-env110
                                                          labels362
                                                          new-vars363
                                                          r333)
                                                        (make-binding-wrap132
                                                          old-ids361
                                                          labels362
                                                          w334)
                                                        mod335))))))
                                        tmp357)
                                 ((lambda (_371)
                                    (syntax-violation
                                      'lambda
                                      "bad lambda"
                                      e330))
                                  tmp337)))
                             ($sc-dispatch
                               tmp337
                               '(any any . each-any)))))
                        ($sc-dispatch
                          tmp337
                          '(each-any any . each-any)))))
                   ($sc-dispatch
                     tmp337
                     '(any any any . each-any))))
                c332)))
           (chi-body155
             (lambda (body372 outer-form373 r374 w375 mod376)
               (let ((r377 (cons (quote ("placeholder" placeholder)) r374)))
                 (let ((ribcage378
                         (make-ribcage122
                           '()
                           '()
                           '())))
                   (let ((w379 (make-wrap117
                                 (wrap-marks118 w375)
                                 (cons ribcage378 (wrap-subst119 w375)))))
                     (letrec ((parse380
                                (lambda (body381
                                         ids382
                                         labels383
                                         var-ids384
                                         vars385
                                         vals386
                                         bindings387)
                                  (if (null? body381)
                                    (syntax-violation
                                      #f
                                      "no expressions in body"
                                      outer-form373)
                                    (let ((e389 (cdar body381))
                                          (er390 (caar body381)))
                                      (call-with-values
                                        (lambda ()
                                          (syntax-type149
                                            e389
                                            er390
                                            '(())
                                            (source-annotation106 er390)
                                            ribcage378
                                            mod376
                                            #f))
                                        (lambda (type391
                                                 value392
                                                 e393
                                                 w394
                                                 s395
                                                 mod396)
                                          (if (memv type391
                                                    '(define-form))
                                            (let ((id397 (wrap143
                                                           value392
                                                           w394
                                                           mod396))
                                                  (label398 (gen-label120)))
                                              (let ((var399
                                                      (gen-var162 id397)))
                                                (begin
                                                  (extend-ribcage!131
                                                    ribcage378
                                                    id397
                                                    label398)
                                                  (parse380
                                                    (cdr body381)
                                                    (cons id397 ids382)
                                                    (cons label398 labels383)
                                                    (cons id397 var-ids384)
                                                    (cons var399 vars385)
                                                    (cons (cons er390
                                                                (wrap143
                                                                  e393
                                                                  w394
                                                                  mod396))
                                                          vals386)
                                                    (cons (cons 'lexical
                                                                var399)
                                                          bindings387)))))
                                            (if (memv type391
                                                      '(define-syntax-form))
                                              (let ((id400 (wrap143
                                                             value392
                                                             w394
                                                             mod396))
                                                    (label401 (gen-label120)))
                                                (begin
                                                  (extend-ribcage!131
                                                    ribcage378
                                                    id400
                                                    label401)
                                                  (parse380
                                                    (cdr body381)
                                                    (cons id400 ids382)
                                                    (cons label401 labels383)
                                                    var-ids384
                                                    vars385
                                                    vals386
                                                    (cons (cons 'macro
                                                                (cons er390
                                                                      (wrap143
                                                                        e393
                                                                        w394
                                                                        mod396)))
                                                          bindings387))))
                                              (if (memv type391
                                                        '(begin-form))
                                                ((lambda (tmp402)
                                                   ((lambda (tmp403)
                                                      (if tmp403
                                                        (apply (lambda (_404
                                                                        e1405)
                                                                 (parse380
                                                                   (letrec ((f406 (lambda (forms407)
                                                                                    (if (null? forms407)
                                                                                      (cdr body381)
                                                                                      (cons (cons er390
                                                                                                  (wrap143
                                                                                                    (car forms407)
                                                                                                    w394
                                                                                                    mod396))
                                                                                            (f406 (cdr forms407)))))))
                                                                     (f406 e1405))
                                                                   ids382
                                                                   labels383
                                                                   var-ids384
                                                                   vars385
                                                                   vals386
                                                                   bindings387))
                                                               tmp403)
                                                        (syntax-violation
                                                          #f
                                                          "source expression failed to match any pattern"
                                                          tmp402)))
                                                    ($sc-dispatch
                                                      tmp402
                                                      '(any . each-any))))
                                                 e393)
                                                (if (memv type391
                                                          '(local-syntax-form))
                                                  (chi-local-syntax157
                                                    value392
                                                    e393
                                                    er390
                                                    w394
                                                    s395
                                                    mod396
                                                    (lambda (forms409
                                                             er410
                                                             w411
                                                             s412
                                                             mod413)
                                                      (parse380
                                                        (letrec ((f414 (lambda (forms415)
                                                                         (if (null? forms415)
                                                                           (cdr body381)
                                                                           (cons (cons er410
                                                                                       (wrap143
                                                                                         (car forms415)
                                                                                         w411
                                                                                         mod413))
                                                                                 (f414 (cdr forms415)))))))
                                                          (f414 forms409))
                                                        ids382
                                                        labels383
                                                        var-ids384
                                                        vars385
                                                        vals386
                                                        bindings387)))
                                                  (if (null? ids382)
                                                    (build-sequence94
                                                      #f
                                                      (map (lambda (x416)
                                                             (chi151
                                                               (cdr x416)
                                                               (car x416)
                                                               '(())
                                                               mod396))
                                                           (cons (cons er390
                                                                       (source-wrap144
                                                                         e393
                                                                         w394
                                                                         s395
                                                                         mod396))
                                                                 (cdr body381))))
                                                    (begin
                                                      (if (not (valid-bound-ids?140
                                                                 ids382))
                                                        (syntax-violation
                                                          #f
                                                          "invalid or duplicate identifier in definition"
                                                          outer-form373))
                                                      (letrec ((loop417
                                                                 (lambda (bs418
                                                                          er-cache419
                                                                          r-cache420)
                                                                   (if (not (null? bs418))
                                                                     (let ((b421 (car bs418)))
                                                                       (if (eq? (car b421)
                                                                                'macro)
                                                                         (let ((er422 (cadr b421)))
                                                                           (let ((r-cache423
                                                                                   (if (eq? er422
                                                                                            er-cache419)
                                                                                     r-cache420
                                                                                     (macros-only-env111
                                                                                       er422))))
                                                                             (begin
                                                                               (set-cdr!
                                                                                 b421
                                                                                 (eval-local-transformer158
                                                                                   (chi151
                                                                                     (cddr b421)
                                                                                     r-cache423
                                                                                     '(())
                                                                                     mod396)
                                                                                   mod396))
                                                                               (loop417
                                                                                 (cdr bs418)
                                                                                 er422
                                                                                 r-cache423))))
                                                                         (loop417
                                                                           (cdr bs418)
                                                                           er-cache419
                                                                           r-cache420)))))))
                                                        (loop417
                                                          bindings387
                                                          #f
                                                          #f))
                                                      (set-cdr!
                                                        r377
                                                        (extend-env109
                                                          labels383
                                                          bindings387
                                                          (cdr r377)))
                                                      (build-letrec97
                                                        #f
                                                        (map syntax->datum
                                                             var-ids384)
                                                        vars385
                                                        (map (lambda (x424)
                                                               (chi151
                                                                 (cdr x424)
                                                                 (car x424)
                                                                 '(())
                                                                 mod396))
                                                             vals386)
                                                        (build-sequence94
                                                          #f
                                                          (map (lambda (x425)
                                                                 (chi151
                                                                   (cdr x425)
                                                                   (car x425)
                                                                   '(())
                                                                   mod396))
                                                               (cons (cons er390
                                                                           (source-wrap144
                                                                             e393
                                                                             w394
                                                                             s395
                                                                             mod396))
                                                                     (cdr body381))))))))))))))))))
                       (parse380
                         (map (lambda (x388)
                                (cons r377 (wrap143 x388 w379 mod376)))
                              body372)
                         '()
                         '()
                         '()
                         '()
                         '()
                         '())))))))
           (chi-macro154
             (lambda (p426 e427 r428 w429 rib430 mod431)
               (letrec ((rebuild-macro-output432
                          (lambda (x433 m434)
                            (if (pair? x433)
                              (cons (rebuild-macro-output432 (car x433) m434)
                                    (rebuild-macro-output432 (cdr x433) m434))
                              (if (syntax-object?99 x433)
                                (let ((w435 (syntax-object-wrap101 x433)))
                                  (let ((ms436 (wrap-marks118 w435))
                                        (s437 (wrap-subst119 w435)))
                                    (if (if (pair? ms436)
                                          (eq? (car ms436) #f)
                                          #f)
                                      (make-syntax-object98
                                        (syntax-object-expression100 x433)
                                        (make-wrap117
                                          (cdr ms436)
                                          (if rib430
                                            (cons rib430 (cdr s437))
                                            (cdr s437)))
                                        (syntax-object-module102 x433))
                                      (make-syntax-object98
                                        (syntax-object-expression100 x433)
                                        (make-wrap117
                                          (cons m434 ms436)
                                          (if rib430
                                            (cons rib430
                                                  (cons (quote shift) s437))
                                            (cons (quote shift) s437)))
                                        (let ((pmod438
                                                (procedure-module p426)))
                                          (if pmod438
                                            (cons 'hygiene
                                                  (module-name pmod438))
                                            '(hygiene guile)))))))
                                (if (vector? x433)
                                  (let ((n439 (vector-length x433)))
                                    (let ((v440 (make-vector n439)))
                                      (letrec ((loop441
                                                 (lambda (i442)
                                                   (if (fx=74 i442 n439)
                                                     (begin (if #f #f) v440)
                                                     (begin
                                                       (vector-set!
                                                         v440
                                                         i442
                                                         (rebuild-macro-output432
                                                           (vector-ref
                                                             x433
                                                             i442)
                                                           m434))
                                                       (loop441
                                                         (fx+72 i442 1)))))))
                                        (loop441 0))))
                                  (if (symbol? x433)
                                    (syntax-violation
                                      #f
                                      "encountered raw symbol in macro output"
                                      (source-wrap144 e427 w429 s mod431)
                                      x433)
                                    x433)))))))
                 (rebuild-macro-output432
                   (p426 (wrap143 e427 (anti-mark130 w429) mod431))
                   (string #\m)))))
           (chi-application153
             (lambda (x443 e444 r445 w446 s447 mod448)
               ((lambda (tmp449)
                  ((lambda (tmp450)
                     (if tmp450
                       (apply (lambda (e0451 e1452)
                                (build-application82
                                  s447
                                  x443
                                  (map (lambda (e453)
                                         (chi151 e453 r445 w446 mod448))
                                       e1452)))
                              tmp450)
                       (syntax-violation
                         #f
                         "source expression failed to match any pattern"
                         tmp449)))
                   ($sc-dispatch tmp449 (quote (any . each-any)))))
                e444)))
           (chi-expr152
             (lambda (type455 value456 e457 r458 w459 s460 mod461)
               (if (memv type455 (quote (lexical)))
                 (build-lexical-reference84
                   'value
                   s460
                   e457
                   value456)
                 (if (memv type455 (quote (core core-form)))
                   (value456 e457 r458 w459 s460 mod461)
                   (if (memv type455 (quote (module-ref)))
                     (call-with-values
                       (lambda () (value456 e457))
                       (lambda (id462 mod463)
                         (build-global-reference87 s460 id462 mod463)))
                     (if (memv type455 (quote (lexical-call)))
                       (chi-application153
                         (build-lexical-reference84
                           'fun
                           (source-annotation106 (car e457))
                           (car e457)
                           value456)
                         e457
                         r458
                         w459
                         s460
                         mod461)
                       (if (memv type455 (quote (global-call)))
                         (chi-application153
                           (build-global-reference87
                             (source-annotation106 (car e457))
                             (if (syntax-object?99 value456)
                               (syntax-object-expression100 value456)
                               value456)
                             (if (syntax-object?99 value456)
                               (syntax-object-module102 value456)
                               mod461))
                           e457
                           r458
                           w459
                           s460
                           mod461)
                         (if (memv type455 (quote (constant)))
                           (build-data93
                             s460
                             (strip161
                               (source-wrap144 e457 w459 s460 mod461)
                               '(())))
                           (if (memv type455 (quote (global)))
                             (build-global-reference87 s460 value456 mod461)
                             (if (memv type455 (quote (call)))
                               (chi-application153
                                 (chi151 (car e457) r458 w459 mod461)
                                 e457
                                 r458
                                 w459
                                 s460
                                 mod461)
                               (if (memv type455 (quote (begin-form)))
                                 ((lambda (tmp464)
                                    ((lambda (tmp465)
                                       (if tmp465
                                         (apply (lambda (_466 e1467 e2468)
                                                  (chi-sequence145
                                                    (cons e1467 e2468)
                                                    r458
                                                    w459
                                                    s460
                                                    mod461))
                                                tmp465)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           tmp464)))
                                     ($sc-dispatch
                                       tmp464
                                       '(any any . each-any))))
                                  e457)
                                 (if (memv type455 (quote (local-syntax-form)))
                                   (chi-local-syntax157
                                     value456
                                     e457
                                     r458
                                     w459
                                     s460
                                     mod461
                                     chi-sequence145)
                                   (if (memv type455 (quote (eval-when-form)))
                                     ((lambda (tmp470)
                                        ((lambda (tmp471)
                                           (if tmp471
                                             (apply (lambda (_472
                                                             x473
                                                             e1474
                                                             e2475)
                                                      (let ((when-list476
                                                              (chi-when-list148
                                                                e457
                                                                x473
                                                                w459)))
                                                        (if (memq 'eval
                                                                  when-list476)
                                                          (chi-sequence145
                                                            (cons e1474 e2475)
                                                            r458
                                                            w459
                                                            s460
                                                            mod461)
                                                          (chi-void159))))
                                                    tmp471)
                                             (syntax-violation
                                               #f
                                               "source expression failed to match any pattern"
                                               tmp470)))
                                         ($sc-dispatch
                                           tmp470
                                           '(any each-any any . each-any))))
                                      e457)
                                     (if (memv type455
                                               '(define-form
                                                  define-syntax-form))
                                       (syntax-violation
                                         #f
                                         "definition in expression context"
                                         e457
                                         (wrap143 value456 w459 mod461))
                                       (if (memv type455 (quote (syntax)))
                                         (syntax-violation
                                           #f
                                           "reference to pattern variable outside syntax form"
                                           (source-wrap144
                                             e457
                                             w459
                                             s460
                                             mod461))
                                         (if (memv type455
                                                   '(displaced-lexical))
                                           (syntax-violation
                                             #f
                                             "reference to identifier outside its scope"
                                             (source-wrap144
                                               e457
                                               w459
                                               s460
                                               mod461))
                                           (syntax-violation
                                             #f
                                             "unexpected syntax"
                                             (source-wrap144
                                               e457
                                               w459
                                               s460
                                               mod461))))))))))))))))))
           (chi151
             (lambda (e479 r480 w481 mod482)
               (call-with-values
                 (lambda ()
                   (syntax-type149
                     e479
                     r480
                     w481
                     (source-annotation106 e479)
                     #f
                     mod482
                     #f))
                 (lambda (type483 value484 e485 w486 s487 mod488)
                   (chi-expr152
                     type483
                     value484
                     e485
                     r480
                     w486
                     s487
                     mod488)))))
           (chi-top150
             (lambda (e489 r490 w491 m492 esew493 mod494)
               (call-with-values
                 (lambda ()
                   (syntax-type149
                     e489
                     r490
                     w491
                     (source-annotation106 e489)
                     #f
                     mod494
                     #f))
                 (lambda (type502 value503 e504 w505 s506 mod507)
                   (if (memv type502 (quote (begin-form)))
                     ((lambda (tmp508)
                        ((lambda (tmp509)
                           (if tmp509
                             (apply (lambda (_510) (chi-void159)) tmp509)
                             ((lambda (tmp511)
                                (if tmp511
                                  (apply (lambda (_512 e1513 e2514)
                                           (chi-top-sequence146
                                             (cons e1513 e2514)
                                             r490
                                             w505
                                             s506
                                             m492
                                             esew493
                                             mod507))
                                         tmp511)
                                  (syntax-violation
                                    #f
                                    "source expression failed to match any pattern"
                                    tmp508)))
                              ($sc-dispatch
                                tmp508
                                '(any any . each-any)))))
                         ($sc-dispatch tmp508 (quote (any)))))
                      e504)
                     (if (memv type502 (quote (local-syntax-form)))
                       (chi-local-syntax157
                         value503
                         e504
                         r490
                         w505
                         s506
                         mod507
                         (lambda (body516 r517 w518 s519 mod520)
                           (chi-top-sequence146
                             body516
                             r517
                             w518
                             s519
                             m492
                             esew493
                             mod520)))
                       (if (memv type502 (quote (eval-when-form)))
                         ((lambda (tmp521)
                            ((lambda (tmp522)
                               (if tmp522
                                 (apply (lambda (_523 x524 e1525 e2526)
                                          (let ((when-list527
                                                  (chi-when-list148
                                                    e504
                                                    x524
                                                    w505))
                                                (body528 (cons e1525 e2526)))
                                            (if (eq? m492 (quote e))
                                              (if (memq 'eval
                                                        when-list527)
                                                (chi-top-sequence146
                                                  body528
                                                  r490
                                                  w505
                                                  s506
                                                  'e
                                                  '(eval)
                                                  mod507)
                                                (chi-void159))
                                              (if (memq 'load
                                                        when-list527)
                                                (if (let ((t531 (memq 'compile
                                                                      when-list527)))
                                                      (if t531
                                                        t531
                                                        (if (eq? m492
                                                                 'c&e)
                                                          (memq 'eval
                                                                when-list527)
                                                          #f)))
                                                  (chi-top-sequence146
                                                    body528
                                                    r490
                                                    w505
                                                    s506
                                                    'c&e
                                                    '(compile load)
                                                    mod507)
                                                  (if (memq m492
                                                            '(c c&e))
                                                    (chi-top-sequence146
                                                      body528
                                                      r490
                                                      w505
                                                      s506
                                                      'c
                                                      '(load)
                                                      mod507)
                                                    (chi-void159)))
                                                (if (let ((t532 (memq 'compile
                                                                      when-list527)))
                                                      (if t532
                                                        t532
                                                        (if (eq? m492
                                                                 'c&e)
                                                          (memq 'eval
                                                                when-list527)
                                                          #f)))
                                                  (begin
                                                    (top-level-eval-hook76
                                                      (chi-top-sequence146
                                                        body528
                                                        r490
                                                        w505
                                                        s506
                                                        'e
                                                        '(eval)
                                                        mod507)
                                                      mod507)
                                                    (chi-void159))
                                                  (chi-void159))))))
                                        tmp522)
                                 (syntax-violation
                                   #f
                                   "source expression failed to match any pattern"
                                   tmp521)))
                             ($sc-dispatch
                               tmp521
                               '(any each-any any . each-any))))
                          e504)
                         (if (memv type502 (quote (define-syntax-form)))
                           (let ((n533 (id-var-name137 value503 w505))
                                 (r534 (macros-only-env111 r490)))
                             (if (memv m492 (quote (c)))
                               (if (memq (quote compile) esew493)
                                 (let ((e535 (chi-install-global147
                                               n533
                                               (chi151
                                                 e504
                                                 r534
                                                 w505
                                                 mod507))))
                                   (begin
                                     (top-level-eval-hook76 e535 mod507)
                                     (if (memq (quote load) esew493)
                                       e535
                                       (chi-void159))))
                                 (if (memq (quote load) esew493)
                                   (chi-install-global147
                                     n533
                                     (chi151 e504 r534 w505 mod507))
                                   (chi-void159)))
                               (if (memv m492 (quote (c&e)))
                                 (let ((e536 (chi-install-global147
                                               n533
                                               (chi151
                                                 e504
                                                 r534
                                                 w505
                                                 mod507))))
                                   (begin
                                     (top-level-eval-hook76 e536 mod507)
                                     e536))
                                 (begin
                                   (if (memq (quote eval) esew493)
                                     (top-level-eval-hook76
                                       (chi-install-global147
                                         n533
                                         (chi151 e504 r534 w505 mod507))
                                       mod507))
                                   (chi-void159)))))
                           (if (memv type502 (quote (define-form)))
                             (let ((n537 (id-var-name137 value503 w505)))
                               (let ((type538
                                       (binding-type107
                                         (lookup112 n537 r490 mod507))))
                                 (if (memv type538
                                           '(global core macro module-ref))
                                   (begin
                                     (if (if (not (module-local-variable
                                                    (current-module)
                                                    n537))
                                           (current-module)
                                           #f)
                                       (module-define!
                                         (current-module)
                                         n537
                                         #f))
                                     (let ((x539 (build-global-definition90
                                                   s506
                                                   n537
                                                   (chi151
                                                     e504
                                                     r490
                                                     w505
                                                     mod507))))
                                       (begin
                                         (if (eq? m492 (quote c&e))
                                           (top-level-eval-hook76 x539 mod507))
                                         x539)))
                                   (if (memv type538
                                             '(displaced-lexical))
                                     (syntax-violation
                                       #f
                                       "identifier out of context"
                                       e504
                                       (wrap143 value503 w505 mod507))
                                     (syntax-violation
                                       #f
                                       "cannot define keyword at top level"
                                       e504
                                       (wrap143 value503 w505 mod507))))))
                             (let ((x540 (chi-expr152
                                           type502
                                           value503
                                           e504
                                           r490
                                           w505
                                           s506
                                           mod507)))
                               (begin
                                 (if (eq? m492 (quote c&e))
                                   (top-level-eval-hook76 x540 mod507))
                                 x540)))))))))))
           (syntax-type149
             (lambda (e541 r542 w543 s544 rib545 mod546 for-car?547)
               (if (symbol? e541)
                 (let ((n548 (id-var-name137 e541 w543)))
                   (let ((b549 (lookup112 n548 r542 mod546)))
                     (let ((type550 (binding-type107 b549)))
                       (if (memv type550 (quote (lexical)))
                         (values
                           type550
                           (binding-value108 b549)
                           e541
                           w543
                           s544
                           mod546)
                         (if (memv type550 (quote (global)))
                           (values type550 n548 e541 w543 s544 mod546)
                           (if (memv type550 (quote (macro)))
                             (if for-car?547
                               (values
                                 type550
                                 (binding-value108 b549)
                                 e541
                                 w543
                                 s544
                                 mod546)
                               (syntax-type149
                                 (chi-macro154
                                   (binding-value108 b549)
                                   e541
                                   r542
                                   w543
                                   rib545
                                   mod546)
                                 r542
                                 '(())
                                 s544
                                 rib545
                                 mod546
                                 #f))
                             (values
                               type550
                               (binding-value108 b549)
                               e541
                               w543
                               s544
                               mod546)))))))
                 (if (pair? e541)
                   (let ((first551 (car e541)))
                     (call-with-values
                       (lambda ()
                         (syntax-type149
                           first551
                           r542
                           w543
                           s544
                           rib545
                           mod546
                           #t))
                       (lambda (ftype552 fval553 fe554 fw555 fs556 fmod557)
                         (if (memv ftype552 (quote (lexical)))
                           (values
                             'lexical-call
                             fval553
                             e541
                             w543
                             s544
                             mod546)
                           (if (memv ftype552 (quote (global)))
                             (values
                               'global-call
                               (make-syntax-object98 fval553 w543 fmod557)
                               e541
                               w543
                               s544
                               mod546)
                             (if (memv ftype552 (quote (macro)))
                               (syntax-type149
                                 (chi-macro154
                                   fval553
                                   e541
                                   r542
                                   w543
                                   rib545
                                   mod546)
                                 r542
                                 '(())
                                 s544
                                 rib545
                                 mod546
                                 for-car?547)
                               (if (memv ftype552 (quote (module-ref)))
                                 (call-with-values
                                   (lambda () (fval553 e541))
                                   (lambda (sym558 mod559)
                                     (syntax-type149
                                       sym558
                                       r542
                                       w543
                                       s544
                                       rib545
                                       mod559
                                       for-car?547)))
                                 (if (memv ftype552 (quote (core)))
                                   (values
                                     'core-form
                                     fval553
                                     e541
                                     w543
                                     s544
                                     mod546)
                                   (if (memv ftype552 (quote (local-syntax)))
                                     (values
                                       'local-syntax-form
                                       fval553
                                       e541
                                       w543
                                       s544
                                       mod546)
                                     (if (memv ftype552 (quote (begin)))
                                       (values
                                         'begin-form
                                         #f
                                         e541
                                         w543
                                         s544
                                         mod546)
                                       (if (memv ftype552 (quote (eval-when)))
                                         (values
                                           'eval-when-form
                                           #f
                                           e541
                                           w543
                                           s544
                                           mod546)
                                         (if (memv ftype552 (quote (define)))
                                           ((lambda (tmp560)
                                              ((lambda (tmp561)
                                                 (if (if tmp561
                                                       (apply (lambda (_562
                                                                       name563
                                                                       val564)
                                                                (id?115
                                                                  name563))
                                                              tmp561)
                                                       #f)
                                                   (apply (lambda (_565
                                                                   name566
                                                                   val567)
                                                            (values
                                                              'define-form
                                                              name566
                                                              val567
                                                              w543
                                                              s544
                                                              mod546))
                                                          tmp561)
                                                   ((lambda (tmp568)
                                                      (if (if tmp568
                                                            (apply (lambda (_569
                                                                            name570
                                                                            args571
                                                                            e1572
                                                                            e2573)
                                                                     (if (id?115
                                                                           name570)
                                                                       (valid-bound-ids?140
                                                                         (lambda-var-list163
                                                                           args571))
                                                                       #f))
                                                                   tmp568)
                                                            #f)
                                                        (apply (lambda (_574
                                                                        name575
                                                                        args576
                                                                        e1577
                                                                        e2578)
                                                                 (values
                                                                   'define-form
                                                                   (wrap143
                                                                     name575
                                                                     w543
                                                                     mod546)
                                                                   (decorate-source80
                                                                     (cons '#(syntax-object
                                                                              lambda
                                                                              ((top)
                                                                               #(ribcage
                                                                                 #(_
                                                                                   name
                                                                                   args
                                                                                   e1
                                                                                   e2)
                                                                                 #((top)
                                                                                   (top)
                                                                                   (top)
                                                                                   (top)
                                                                                   (top))
                                                                                 #("i"
                                                                                   "i"
                                                                                   "i"
                                                                                   "i"
                                                                                   "i"))
                                                                               #(ribcage
                                                                                 ()
                                                                                 ()
                                                                                 ())
                                                                               #(ribcage
                                                                                 ()
                                                                                 ()
                                                                                 ())
                                                                               #(ribcage
                                                                                 #(ftype
                                                                                   fval
                                                                                   fe
                                                                                   fw
                                                                                   fs
                                                                                   fmod)
                                                                                 #((top)
                                                                                   (top)
                                                                                   (top)
                                                                                   (top)
                                                                                   (top)
                                                                                   (top))
                                                                                 #("i"
                                                                                   "i"
                                                                                   "i"
                                                                                   "i"
                                                                                   "i"
                                                                                   "i"))
                                                                               #(ribcage
                                                                                 ()
                                                                                 ()
                                                                                 ())
                                                                               #(ribcage
                                                                                 #(first)
                                                                                 #((top))
                                                                                 #("i"))
                                                                               #(ribcage
                                                                                 ()
                                                                                 ()
                                                                                 ())
                                                                               #(ribcage
                                                                                 ()
                                                                                 ()
                                                                                 ())
                                                                               #(ribcage
                                                                                 ()
                                                                                 ()
                                                                                 ())
                                                                               #(ribcage
                                                                                 #(e
                                                                                   r
                                                                                   w
                                                                                   s
                                                                                   rib
                                                                                   mod
                                                                                   for-car?)
                                                                                 #((top)
                                                                                   (top)
                                                                                   (top)
                                                                                   (top)
                                                                                   (top)
                                                                                   (top)
                                                                                   (top))
                                                                                 #("i"
                                                                                   "i"
                                                                                   "i"
                                                                                   "i"
                                                                                   "i"
                                                                                   "i"
                                                                                   "i"))
                                                                               #(ribcage
                                                                                 (lambda-var-list
                                                                                   gen-var
                                                                                   strip
                                                                                   ellipsis?
                                                                                   chi-void
                                                                                   eval-local-transformer
                                                                                   chi-local-syntax
                                                                                   chi-lambda-clause
                                                                                   chi-body
                                                                                   chi-macro
                                                                                   chi-application
                                                                                   chi-expr
                                                                                   chi
                                                                                   chi-top
                                                                                   syntax-type
                                                                                   chi-when-list
                                                                                   chi-install-global
                                                                                   chi-top-sequence
                                                                                   chi-sequence
                                                                                   source-wrap
                                                                                   wrap
                                                                                   bound-id-member?
                                                                                   distinct-bound-ids?
                                                                                   valid-bound-ids?
                                                                                   bound-id=?
                                                                                   free-id=?
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
                                                                                   build-lambda
                                                                                   build-global-definition
                                                                                   maybe-name-value!
                                                                                   build-global-assignment
                                                                                   build-global-reference
                                                                                   analyze-variable
                                                                                   build-lexical-assignment
                                                                                   build-lexical-reference
                                                                                   build-conditional
                                                                                   build-application
                                                                                   build-void
                                                                                   decorate-source
                                                                                   get-global-definition-hook
                                                                                   put-global-definition-hook
                                                                                   gensym-hook
                                                                                   local-eval-hook
                                                                                   top-level-eval-hook
                                                                                   fx<
                                                                                   fx=
                                                                                   fx-
                                                                                   fx+
                                                                                   *mode*
                                                                                   noexpand)
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
                                                                                  (top))
                                                                                 ("i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"))
                                                                               #(ribcage
                                                                                 (define-structure
                                                                                   and-map*)
                                                                                 ((top)
                                                                                  (top))
                                                                                 ("i"
                                                                                  "i")))
                                                                              (hygiene
                                                                                guile))
                                                                           (wrap143
                                                                             (cons args576
                                                                                   (cons e1577
                                                                                         e2578))
                                                                             w543
                                                                             mod546))
                                                                     s544)
                                                                   '(())
                                                                   s544
                                                                   mod546))
                                                               tmp568)
                                                        ((lambda (tmp580)
                                                           (if (if tmp580
                                                                 (apply (lambda (_581
                                                                                 name582)
                                                                          (id?115
                                                                            name582))
                                                                        tmp580)
                                                                 #f)
                                                             (apply (lambda (_583
                                                                             name584)
                                                                      (values
                                                                        'define-form
                                                                        (wrap143
                                                                          name584
                                                                          w543
                                                                          mod546)
                                                                        '(#(syntax-object
                                                                            if
                                                                            ((top)
                                                                             #(ribcage
                                                                               #(_
                                                                                 name)
                                                                               #((top)
                                                                                 (top))
                                                                               #("i"
                                                                                 "i"))
                                                                             #(ribcage
                                                                               ()
                                                                               ()
                                                                               ())
                                                                             #(ribcage
                                                                               ()
                                                                               ()
                                                                               ())
                                                                             #(ribcage
                                                                               #(ftype
                                                                                 fval
                                                                                 fe
                                                                                 fw
                                                                                 fs
                                                                                 fmod)
                                                                               #((top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top))
                                                                               #("i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"))
                                                                             #(ribcage
                                                                               ()
                                                                               ()
                                                                               ())
                                                                             #(ribcage
                                                                               #(first)
                                                                               #((top))
                                                                               #("i"))
                                                                             #(ribcage
                                                                               ()
                                                                               ()
                                                                               ())
                                                                             #(ribcage
                                                                               ()
                                                                               ()
                                                                               ())
                                                                             #(ribcage
                                                                               ()
                                                                               ()
                                                                               ())
                                                                             #(ribcage
                                                                               #(e
                                                                                 r
                                                                                 w
                                                                                 s
                                                                                 rib
                                                                                 mod
                                                                                 for-car?)
                                                                               #((top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top))
                                                                               #("i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"))
                                                                             #(ribcage
                                                                               (lambda-var-list
                                                                                 gen-var
                                                                                 strip
                                                                                 ellipsis?
                                                                                 chi-void
                                                                                 eval-local-transformer
                                                                                 chi-local-syntax
                                                                                 chi-lambda-clause
                                                                                 chi-body
                                                                                 chi-macro
                                                                                 chi-application
                                                                                 chi-expr
                                                                                 chi
                                                                                 chi-top
                                                                                 syntax-type
                                                                                 chi-when-list
                                                                                 chi-install-global
                                                                                 chi-top-sequence
                                                                                 chi-sequence
                                                                                 source-wrap
                                                                                 wrap
                                                                                 bound-id-member?
                                                                                 distinct-bound-ids?
                                                                                 valid-bound-ids?
                                                                                 bound-id=?
                                                                                 free-id=?
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
                                                                                 build-lambda
                                                                                 build-global-definition
                                                                                 maybe-name-value!
                                                                                 build-global-assignment
                                                                                 build-global-reference
                                                                                 analyze-variable
                                                                                 build-lexical-assignment
                                                                                 build-lexical-reference
                                                                                 build-conditional
                                                                                 build-application
                                                                                 build-void
                                                                                 decorate-source
                                                                                 get-global-definition-hook
                                                                                 put-global-definition-hook
                                                                                 gensym-hook
                                                                                 local-eval-hook
                                                                                 top-level-eval-hook
                                                                                 fx<
                                                                                 fx=
                                                                                 fx-
                                                                                 fx+
                                                                                 *mode*
                                                                                 noexpand)
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
                                                                                (top))
                                                                               ("i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"))
                                                                             #(ribcage
                                                                               (define-structure
                                                                                 and-map*)
                                                                               ((top)
                                                                                (top))
                                                                               ("i"
                                                                                "i")))
                                                                            (hygiene
                                                                              guile))
                                                                          #(syntax-object
                                                                            #f
                                                                            ((top)
                                                                             #(ribcage
                                                                               #(_
                                                                                 name)
                                                                               #((top)
                                                                                 (top))
                                                                               #("i"
                                                                                 "i"))
                                                                             #(ribcage
                                                                               ()
                                                                               ()
                                                                               ())
                                                                             #(ribcage
                                                                               ()
                                                                               ()
                                                                               ())
                                                                             #(ribcage
                                                                               #(ftype
                                                                                 fval
                                                                                 fe
                                                                                 fw
                                                                                 fs
                                                                                 fmod)
                                                                               #((top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top))
                                                                               #("i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"))
                                                                             #(ribcage
                                                                               ()
                                                                               ()
                                                                               ())
                                                                             #(ribcage
                                                                               #(first)
                                                                               #((top))
                                                                               #("i"))
                                                                             #(ribcage
                                                                               ()
                                                                               ()
                                                                               ())
                                                                             #(ribcage
                                                                               ()
                                                                               ()
                                                                               ())
                                                                             #(ribcage
                                                                               ()
                                                                               ()
                                                                               ())
                                                                             #(ribcage
                                                                               #(e
                                                                                 r
                                                                                 w
                                                                                 s
                                                                                 rib
                                                                                 mod
                                                                                 for-car?)
                                                                               #((top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top))
                                                                               #("i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"))
                                                                             #(ribcage
                                                                               (lambda-var-list
                                                                                 gen-var
                                                                                 strip
                                                                                 ellipsis?
                                                                                 chi-void
                                                                                 eval-local-transformer
                                                                                 chi-local-syntax
                                                                                 chi-lambda-clause
                                                                                 chi-body
                                                                                 chi-macro
                                                                                 chi-application
                                                                                 chi-expr
                                                                                 chi
                                                                                 chi-top
                                                                                 syntax-type
                                                                                 chi-when-list
                                                                                 chi-install-global
                                                                                 chi-top-sequence
                                                                                 chi-sequence
                                                                                 source-wrap
                                                                                 wrap
                                                                                 bound-id-member?
                                                                                 distinct-bound-ids?
                                                                                 valid-bound-ids?
                                                                                 bound-id=?
                                                                                 free-id=?
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
                                                                                 build-lambda
                                                                                 build-global-definition
                                                                                 maybe-name-value!
                                                                                 build-global-assignment
                                                                                 build-global-reference
                                                                                 analyze-variable
                                                                                 build-lexical-assignment
                                                                                 build-lexical-reference
                                                                                 build-conditional
                                                                                 build-application
                                                                                 build-void
                                                                                 decorate-source
                                                                                 get-global-definition-hook
                                                                                 put-global-definition-hook
                                                                                 gensym-hook
                                                                                 local-eval-hook
                                                                                 top-level-eval-hook
                                                                                 fx<
                                                                                 fx=
                                                                                 fx-
                                                                                 fx+
                                                                                 *mode*
                                                                                 noexpand)
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
                                                                                (top))
                                                                               ("i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"))
                                                                             #(ribcage
                                                                               (define-structure
                                                                                 and-map*)
                                                                               ((top)
                                                                                (top))
                                                                               ("i"
                                                                                "i")))
                                                                            (hygiene
                                                                              guile))
                                                                          #(syntax-object
                                                                            #f
                                                                            ((top)
                                                                             #(ribcage
                                                                               #(_
                                                                                 name)
                                                                               #((top)
                                                                                 (top))
                                                                               #("i"
                                                                                 "i"))
                                                                             #(ribcage
                                                                               ()
                                                                               ()
                                                                               ())
                                                                             #(ribcage
                                                                               ()
                                                                               ()
                                                                               ())
                                                                             #(ribcage
                                                                               #(ftype
                                                                                 fval
                                                                                 fe
                                                                                 fw
                                                                                 fs
                                                                                 fmod)
                                                                               #((top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top))
                                                                               #("i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"))
                                                                             #(ribcage
                                                                               ()
                                                                               ()
                                                                               ())
                                                                             #(ribcage
                                                                               #(first)
                                                                               #((top))
                                                                               #("i"))
                                                                             #(ribcage
                                                                               ()
                                                                               ()
                                                                               ())
                                                                             #(ribcage
                                                                               ()
                                                                               ()
                                                                               ())
                                                                             #(ribcage
                                                                               ()
                                                                               ()
                                                                               ())
                                                                             #(ribcage
                                                                               #(e
                                                                                 r
                                                                                 w
                                                                                 s
                                                                                 rib
                                                                                 mod
                                                                                 for-car?)
                                                                               #((top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top))
                                                                               #("i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"))
                                                                             #(ribcage
                                                                               (lambda-var-list
                                                                                 gen-var
                                                                                 strip
                                                                                 ellipsis?
                                                                                 chi-void
                                                                                 eval-local-transformer
                                                                                 chi-local-syntax
                                                                                 chi-lambda-clause
                                                                                 chi-body
                                                                                 chi-macro
                                                                                 chi-application
                                                                                 chi-expr
                                                                                 chi
                                                                                 chi-top
                                                                                 syntax-type
                                                                                 chi-when-list
                                                                                 chi-install-global
                                                                                 chi-top-sequence
                                                                                 chi-sequence
                                                                                 source-wrap
                                                                                 wrap
                                                                                 bound-id-member?
                                                                                 distinct-bound-ids?
                                                                                 valid-bound-ids?
                                                                                 bound-id=?
                                                                                 free-id=?
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
                                                                                 build-lambda
                                                                                 build-global-definition
                                                                                 maybe-name-value!
                                                                                 build-global-assignment
                                                                                 build-global-reference
                                                                                 analyze-variable
                                                                                 build-lexical-assignment
                                                                                 build-lexical-reference
                                                                                 build-conditional
                                                                                 build-application
                                                                                 build-void
                                                                                 decorate-source
                                                                                 get-global-definition-hook
                                                                                 put-global-definition-hook
                                                                                 gensym-hook
                                                                                 local-eval-hook
                                                                                 top-level-eval-hook
                                                                                 fx<
                                                                                 fx=
                                                                                 fx-
                                                                                 fx+
                                                                                 *mode*
                                                                                 noexpand)
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
                                                                                (top))
                                                                               ("i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"))
                                                                             #(ribcage
                                                                               (define-structure
                                                                                 and-map*)
                                                                               ((top)
                                                                                (top))
                                                                               ("i"
                                                                                "i")))
                                                                            (hygiene
                                                                              guile)))
                                                                        '(())
                                                                        s544
                                                                        mod546))
                                                                    tmp580)
                                                             (syntax-violation
                                                               #f
                                                               "source expression failed to match any pattern"
                                                               tmp560)))
                                                         ($sc-dispatch
                                                           tmp560
                                                           '(any any)))))
                                                    ($sc-dispatch
                                                      tmp560
                                                      '(any (any . any)
                                                            any
                                                            .
                                                            each-any)))))
                                               ($sc-dispatch
                                                 tmp560
                                                 '(any any any))))
                                            e541)
                                           (if (memv ftype552
                                                     '(define-syntax))
                                             ((lambda (tmp585)
                                                ((lambda (tmp586)
                                                   (if (if tmp586
                                                         (apply (lambda (_587
                                                                         name588
                                                                         val589)
                                                                  (id?115
                                                                    name588))
                                                                tmp586)
                                                         #f)
                                                     (apply (lambda (_590
                                                                     name591
                                                                     val592)
                                                              (values
                                                                'define-syntax-form
                                                                name591
                                                                val592
                                                                w543
                                                                s544
                                                                mod546))
                                                            tmp586)
                                                     (syntax-violation
                                                       #f
                                                       "source expression failed to match any pattern"
                                                       tmp585)))
                                                 ($sc-dispatch
                                                   tmp585
                                                   '(any any any))))
                                              e541)
                                             (values
                                               'call
                                               #f
                                               e541
                                               w543
                                               s544
                                               mod546))))))))))))))
                   (if (syntax-object?99 e541)
                     (syntax-type149
                       (syntax-object-expression100 e541)
                       r542
                       (join-wraps134 w543 (syntax-object-wrap101 e541))
                       s544
                       rib545
                       (let ((t593 (syntax-object-module102 e541)))
                         (if t593 t593 mod546))
                       for-car?547)
                     (if (self-evaluating? e541)
                       (values
                         'constant
                         #f
                         e541
                         w543
                         s544
                         mod546)
                       (values (quote other) #f e541 w543 s544 mod546)))))))
           (chi-when-list148
             (lambda (e594 when-list595 w596)
               (letrec ((f597 (lambda (when-list598 situations599)
                                (if (null? when-list598)
                                  situations599
                                  (f597 (cdr when-list598)
                                        (cons (let ((x600 (car when-list598)))
                                                (if (free-id=?138
                                                      x600
                                                      '#(syntax-object
                                                         compile
                                                         ((top)
                                                          #(ribcage () () ())
                                                          #(ribcage () () ())
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(x)
                                                            #((top))
                                                            #("i"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(f
                                                              when-list
                                                              situations)
                                                            #((top)
                                                              (top)
                                                              (top))
                                                            #("i" "i" "i"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(e when-list w)
                                                            #((top)
                                                              (top)
                                                              (top))
                                                            #("i" "i" "i"))
                                                          #(ribcage
                                                            (lambda-var-list
                                                              gen-var
                                                              strip
                                                              ellipsis?
                                                              chi-void
                                                              eval-local-transformer
                                                              chi-local-syntax
                                                              chi-lambda-clause
                                                              chi-body
                                                              chi-macro
                                                              chi-application
                                                              chi-expr
                                                              chi
                                                              chi-top
                                                              syntax-type
                                                              chi-when-list
                                                              chi-install-global
                                                              chi-top-sequence
                                                              chi-sequence
                                                              source-wrap
                                                              wrap
                                                              bound-id-member?
                                                              distinct-bound-ids?
                                                              valid-bound-ids?
                                                              bound-id=?
                                                              free-id=?
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
                                                              build-lambda
                                                              build-global-definition
                                                              maybe-name-value!
                                                              build-global-assignment
                                                              build-global-reference
                                                              analyze-variable
                                                              build-lexical-assignment
                                                              build-lexical-reference
                                                              build-conditional
                                                              build-application
                                                              build-void
                                                              decorate-source
                                                              get-global-definition-hook
                                                              put-global-definition-hook
                                                              gensym-hook
                                                              local-eval-hook
                                                              top-level-eval-hook
                                                              fx<
                                                              fx=
                                                              fx-
                                                              fx+
                                                              *mode*
                                                              noexpand)
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
                                                             (top))
                                                            ("i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"))
                                                          #(ribcage
                                                            (define-structure
                                                              and-map*)
                                                            ((top) (top))
                                                            ("i" "i")))
                                                         (hygiene guile)))
                                                  'compile
                                                  (if (free-id=?138
                                                        x600
                                                        '#(syntax-object
                                                           load
                                                           ((top)
                                                            #(ribcage () () ())
                                                            #(ribcage () () ())
                                                            #(ribcage () () ())
                                                            #(ribcage
                                                              #(x)
                                                              #((top))
                                                              #("i"))
                                                            #(ribcage () () ())
                                                            #(ribcage
                                                              #(f
                                                                when-list
                                                                situations)
                                                              #((top)
                                                                (top)
                                                                (top))
                                                              #("i" "i" "i"))
                                                            #(ribcage () () ())
                                                            #(ribcage
                                                              #(e when-list w)
                                                              #((top)
                                                                (top)
                                                                (top))
                                                              #("i" "i" "i"))
                                                            #(ribcage
                                                              (lambda-var-list
                                                                gen-var
                                                                strip
                                                                ellipsis?
                                                                chi-void
                                                                eval-local-transformer
                                                                chi-local-syntax
                                                                chi-lambda-clause
                                                                chi-body
                                                                chi-macro
                                                                chi-application
                                                                chi-expr
                                                                chi
                                                                chi-top
                                                                syntax-type
                                                                chi-when-list
                                                                chi-install-global
                                                                chi-top-sequence
                                                                chi-sequence
                                                                source-wrap
                                                                wrap
                                                                bound-id-member?
                                                                distinct-bound-ids?
                                                                valid-bound-ids?
                                                                bound-id=?
                                                                free-id=?
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
                                                                build-lambda
                                                                build-global-definition
                                                                maybe-name-value!
                                                                build-global-assignment
                                                                build-global-reference
                                                                analyze-variable
                                                                build-lexical-assignment
                                                                build-lexical-reference
                                                                build-conditional
                                                                build-application
                                                                build-void
                                                                decorate-source
                                                                get-global-definition-hook
                                                                put-global-definition-hook
                                                                gensym-hook
                                                                local-eval-hook
                                                                top-level-eval-hook
                                                                fx<
                                                                fx=
                                                                fx-
                                                                fx+
                                                                *mode*
                                                                noexpand)
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
                                                               (top))
                                                              ("i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"
                                                               "i"))
                                                            #(ribcage
                                                              (define-structure
                                                                and-map*)
                                                              ((top) (top))
                                                              ("i" "i")))
                                                           (hygiene guile)))
                                                    'load
                                                    (if (free-id=?138
                                                          x600
                                                          '#(syntax-object
                                                             eval
                                                             ((top)
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                #(x)
                                                                #((top))
                                                                #("i"))
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                #(f
                                                                  when-list
                                                                  situations)
                                                                #((top)
                                                                  (top)
                                                                  (top))
                                                                #("i" "i" "i"))
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                #(e
                                                                  when-list
                                                                  w)
                                                                #((top)
                                                                  (top)
                                                                  (top))
                                                                #("i" "i" "i"))
                                                              #(ribcage
                                                                (lambda-var-list
                                                                  gen-var
                                                                  strip
                                                                  ellipsis?
                                                                  chi-void
                                                                  eval-local-transformer
                                                                  chi-local-syntax
                                                                  chi-lambda-clause
                                                                  chi-body
                                                                  chi-macro
                                                                  chi-application
                                                                  chi-expr
                                                                  chi
                                                                  chi-top
                                                                  syntax-type
                                                                  chi-when-list
                                                                  chi-install-global
                                                                  chi-top-sequence
                                                                  chi-sequence
                                                                  source-wrap
                                                                  wrap
                                                                  bound-id-member?
                                                                  distinct-bound-ids?
                                                                  valid-bound-ids?
                                                                  bound-id=?
                                                                  free-id=?
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
                                                                  build-lambda
                                                                  build-global-definition
                                                                  maybe-name-value!
                                                                  build-global-assignment
                                                                  build-global-reference
                                                                  analyze-variable
                                                                  build-lexical-assignment
                                                                  build-lexical-reference
                                                                  build-conditional
                                                                  build-application
                                                                  build-void
                                                                  decorate-source
                                                                  get-global-definition-hook
                                                                  put-global-definition-hook
                                                                  gensym-hook
                                                                  local-eval-hook
                                                                  top-level-eval-hook
                                                                  fx<
                                                                  fx=
                                                                  fx-
                                                                  fx+
                                                                  *mode*
                                                                  noexpand)
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
                                                                 (top))
                                                                ("i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"
                                                                 "i"))
                                                              #(ribcage
                                                                (define-structure
                                                                  and-map*)
                                                                ((top) (top))
                                                                ("i" "i")))
                                                             (hygiene guile)))
                                                      'eval
                                                      (syntax-violation
                                                        'eval-when
                                                        "invalid situation"
                                                        e594
                                                        (wrap143
                                                          x600
                                                          w596
                                                          #f))))))
                                              situations599))))))
                 (f597 when-list595 (quote ())))))
           (chi-install-global147
             (lambda (name601 e602)
               (build-global-definition90
                 #f
                 name601
                 (if (let ((v603 (module-variable (current-module) name601)))
                       (if v603
                         (if (variable-bound? v603)
                           (if (macro? (variable-ref v603))
                             (not (eq? (macro-type (variable-ref v603))
                                       'syncase-macro))
                             #f)
                           #f)
                         #f))
                   (build-application82
                     #f
                     (build-primref92
                       #f
                       'make-extended-syncase-macro)
                     (list (build-application82
                             #f
                             (build-primref92 #f (quote module-ref))
                             (list (build-application82
                                     #f
                                     (build-primref92
                                       #f
                                       'current-module)
                                     '())
                                   (build-data93 #f name601)))
                           (build-data93 #f (quote macro))
                           e602))
                   (build-application82
                     #f
                     (build-primref92 #f (quote make-syncase-macro))
                     (list (build-data93 #f (quote macro)) e602))))))
           (chi-top-sequence146
             (lambda (body604 r605 w606 s607 m608 esew609 mod610)
               (build-sequence94
                 s607
                 (letrec ((dobody611
                            (lambda (body612 r613 w614 m615 esew616 mod617)
                              (if (null? body612)
                                '()
                                (let ((first618
                                        (chi-top150
                                          (car body612)
                                          r613
                                          w614
                                          m615
                                          esew616
                                          mod617)))
                                  (cons first618
                                        (dobody611
                                          (cdr body612)
                                          r613
                                          w614
                                          m615
                                          esew616
                                          mod617)))))))
                   (dobody611 body604 r605 w606 m608 esew609 mod610)))))
           (chi-sequence145
             (lambda (body619 r620 w621 s622 mod623)
               (build-sequence94
                 s622
                 (letrec ((dobody624
                            (lambda (body625 r626 w627 mod628)
                              (if (null? body625)
                                '()
                                (let ((first629
                                        (chi151
                                          (car body625)
                                          r626
                                          w627
                                          mod628)))
                                  (cons first629
                                        (dobody624
                                          (cdr body625)
                                          r626
                                          w627
                                          mod628)))))))
                   (dobody624 body619 r620 w621 mod623)))))
           (source-wrap144
             (lambda (x630 w631 s632 defmod633)
               (wrap143
                 (decorate-source80 x630 s632)
                 w631
                 defmod633)))
           (wrap143
             (lambda (x634 w635 defmod636)
               (if (if (null? (wrap-marks118 w635))
                     (null? (wrap-subst119 w635))
                     #f)
                 x634
                 (if (syntax-object?99 x634)
                   (make-syntax-object98
                     (syntax-object-expression100 x634)
                     (join-wraps134 w635 (syntax-object-wrap101 x634))
                     (syntax-object-module102 x634))
                   (if (null? x634)
                     x634
                     (make-syntax-object98 x634 w635 defmod636))))))
           (bound-id-member?142
             (lambda (x637 list638)
               (if (not (null? list638))
                 (let ((t639 (bound-id=?139 x637 (car list638))))
                   (if t639
                     t639
                     (bound-id-member?142 x637 (cdr list638))))
                 #f)))
           (distinct-bound-ids?141
             (lambda (ids640)
               (letrec ((distinct?641
                          (lambda (ids642)
                            (let ((t643 (null? ids642)))
                              (if t643
                                t643
                                (if (not (bound-id-member?142
                                           (car ids642)
                                           (cdr ids642)))
                                  (distinct?641 (cdr ids642))
                                  #f))))))
                 (distinct?641 ids640))))
           (valid-bound-ids?140
             (lambda (ids644)
               (if (letrec ((all-ids?645
                              (lambda (ids646)
                                (let ((t647 (null? ids646)))
                                  (if t647
                                    t647
                                    (if (id?115 (car ids646))
                                      (all-ids?645 (cdr ids646))
                                      #f))))))
                     (all-ids?645 ids644))
                 (distinct-bound-ids?141 ids644)
                 #f)))
           (bound-id=?139
             (lambda (i648 j649)
               (if (if (syntax-object?99 i648)
                     (syntax-object?99 j649)
                     #f)
                 (if (eq? (syntax-object-expression100 i648)
                          (syntax-object-expression100 j649))
                   (same-marks?136
                     (wrap-marks118 (syntax-object-wrap101 i648))
                     (wrap-marks118 (syntax-object-wrap101 j649)))
                   #f)
                 (eq? i648 j649))))
           (free-id=?138
             (lambda (i650 j651)
               (if (eq? (let ((x652 i650))
                          (if (syntax-object?99 x652)
                            (syntax-object-expression100 x652)
                            x652))
                        (let ((x653 j651))
                          (if (syntax-object?99 x653)
                            (syntax-object-expression100 x653)
                            x653)))
                 (eq? (id-var-name137 i650 (quote (())))
                      (id-var-name137 j651 (quote (()))))
                 #f)))
           (id-var-name137
             (lambda (id654 w655)
               (letrec ((search-vector-rib658
                          (lambda (sym664
                                   subst665
                                   marks666
                                   symnames667
                                   ribcage668)
                            (let ((n669 (vector-length symnames667)))
                              (letrec ((f670 (lambda (i671)
                                               (if (fx=74 i671 n669)
                                                 (search656
                                                   sym664
                                                   (cdr subst665)
                                                   marks666)
                                                 (if (if (eq? (vector-ref
                                                                symnames667
                                                                i671)
                                                              sym664)
                                                       (same-marks?136
                                                         marks666
                                                         (vector-ref
                                                           (ribcage-marks125
                                                             ribcage668)
                                                           i671))
                                                       #f)
                                                   (values
                                                     (vector-ref
                                                       (ribcage-labels126
                                                         ribcage668)
                                                       i671)
                                                     marks666)
                                                   (f670 (fx+72 i671 1)))))))
                                (f670 0)))))
                        (search-list-rib657
                          (lambda (sym672
                                   subst673
                                   marks674
                                   symnames675
                                   ribcage676)
                            (letrec ((f677 (lambda (symnames678 i679)
                                             (if (null? symnames678)
                                               (search656
                                                 sym672
                                                 (cdr subst673)
                                                 marks674)
                                               (if (if (eq? (car symnames678)
                                                            sym672)
                                                     (same-marks?136
                                                       marks674
                                                       (list-ref
                                                         (ribcage-marks125
                                                           ribcage676)
                                                         i679))
                                                     #f)
                                                 (values
                                                   (list-ref
                                                     (ribcage-labels126
                                                       ribcage676)
                                                     i679)
                                                   marks674)
                                                 (f677 (cdr symnames678)
                                                       (fx+72 i679 1)))))))
                              (f677 symnames675 0))))
                        (search656
                          (lambda (sym680 subst681 marks682)
                            (if (null? subst681)
                              (values #f marks682)
                              (let ((fst683 (car subst681)))
                                (if (eq? fst683 (quote shift))
                                  (search656
                                    sym680
                                    (cdr subst681)
                                    (cdr marks682))
                                  (let ((symnames684
                                          (ribcage-symnames124 fst683)))
                                    (if (vector? symnames684)
                                      (search-vector-rib658
                                        sym680
                                        subst681
                                        marks682
                                        symnames684
                                        fst683)
                                      (search-list-rib657
                                        sym680
                                        subst681
                                        marks682
                                        symnames684
                                        fst683)))))))))
                 (if (symbol? id654)
                   (let ((t685 (call-with-values
                                 (lambda ()
                                   (search656
                                     id654
                                     (wrap-subst119 w655)
                                     (wrap-marks118 w655)))
                                 (lambda (x687 . ignore686) x687))))
                     (if t685 t685 id654))
                   (if (syntax-object?99 id654)
                     (let ((id688 (syntax-object-expression100 id654))
                           (w1689 (syntax-object-wrap101 id654)))
                       (let ((marks690
                               (join-marks135
                                 (wrap-marks118 w655)
                                 (wrap-marks118 w1689))))
                         (call-with-values
                           (lambda ()
                             (search656 id688 (wrap-subst119 w655) marks690))
                           (lambda (new-id691 marks692)
                             (let ((t693 new-id691))
                               (if t693
                                 t693
                                 (let ((t694 (call-with-values
                                               (lambda ()
                                                 (search656
                                                   id688
                                                   (wrap-subst119 w1689)
                                                   marks692))
                                               (lambda (x696 . ignore695)
                                                 x696))))
                                   (if t694 t694 id688))))))))
                     (syntax-violation
                       'id-var-name
                       "invalid id"
                       id654))))))
           (same-marks?136
             (lambda (x697 y698)
               (let ((t699 (eq? x697 y698)))
                 (if t699
                   t699
                   (if (not (null? x697))
                     (if (not (null? y698))
                       (if (eq? (car x697) (car y698))
                         (same-marks?136 (cdr x697) (cdr y698))
                         #f)
                       #f)
                     #f)))))
           (join-marks135
             (lambda (m1700 m2701)
               (smart-append133 m1700 m2701)))
           (join-wraps134
             (lambda (w1702 w2703)
               (let ((m1704 (wrap-marks118 w1702))
                     (s1705 (wrap-subst119 w1702)))
                 (if (null? m1704)
                   (if (null? s1705)
                     w2703
                     (make-wrap117
                       (wrap-marks118 w2703)
                       (smart-append133 s1705 (wrap-subst119 w2703))))
                   (make-wrap117
                     (smart-append133 m1704 (wrap-marks118 w2703))
                     (smart-append133 s1705 (wrap-subst119 w2703)))))))
           (smart-append133
             (lambda (m1706 m2707)
               (if (null? m2707) m1706 (append m1706 m2707))))
           (make-binding-wrap132
             (lambda (ids708 labels709 w710)
               (if (null? ids708)
                 w710
                 (make-wrap117
                   (wrap-marks118 w710)
                   (cons (let ((labelvec711 (list->vector labels709)))
                           (let ((n712 (vector-length labelvec711)))
                             (let ((symnamevec713 (make-vector n712))
                                   (marksvec714 (make-vector n712)))
                               (begin
                                 (letrec ((f715 (lambda (ids716 i717)
                                                  (if (not (null? ids716))
                                                    (call-with-values
                                                      (lambda ()
                                                        (id-sym-name&marks116
                                                          (car ids716)
                                                          w710))
                                                      (lambda (symname718
                                                               marks719)
                                                        (begin
                                                          (vector-set!
                                                            symnamevec713
                                                            i717
                                                            symname718)
                                                          (vector-set!
                                                            marksvec714
                                                            i717
                                                            marks719)
                                                          (f715 (cdr ids716)
                                                                (fx+72 i717
                                                                       1)))))))))
                                   (f715 ids708 0))
                                 (make-ribcage122
                                   symnamevec713
                                   marksvec714
                                   labelvec711)))))
                         (wrap-subst119 w710))))))
           (extend-ribcage!131
             (lambda (ribcage720 id721 label722)
               (begin
                 (set-ribcage-symnames!127
                   ribcage720
                   (cons (syntax-object-expression100 id721)
                         (ribcage-symnames124 ribcage720)))
                 (set-ribcage-marks!128
                   ribcage720
                   (cons (wrap-marks118 (syntax-object-wrap101 id721))
                         (ribcage-marks125 ribcage720)))
                 (set-ribcage-labels!129
                   ribcage720
                   (cons label722 (ribcage-labels126 ribcage720))))))
           (anti-mark130
             (lambda (w723)
               (make-wrap117
                 (cons #f (wrap-marks118 w723))
                 (cons (quote shift) (wrap-subst119 w723)))))
           (set-ribcage-labels!129
             (lambda (x724 update725)
               (vector-set! x724 3 update725)))
           (set-ribcage-marks!128
             (lambda (x726 update727)
               (vector-set! x726 2 update727)))
           (set-ribcage-symnames!127
             (lambda (x728 update729)
               (vector-set! x728 1 update729)))
           (ribcage-labels126
             (lambda (x730) (vector-ref x730 3)))
           (ribcage-marks125
             (lambda (x731) (vector-ref x731 2)))
           (ribcage-symnames124
             (lambda (x732) (vector-ref x732 1)))
           (ribcage?123
             (lambda (x733)
               (if (vector? x733)
                 (if (= (vector-length x733) 4)
                   (eq? (vector-ref x733 0) (quote ribcage))
                   #f)
                 #f)))
           (make-ribcage122
             (lambda (symnames734 marks735 labels736)
               (vector
                 'ribcage
                 symnames734
                 marks735
                 labels736)))
           (gen-labels121
             (lambda (ls737)
               (if (null? ls737)
                 '()
                 (cons (gen-label120) (gen-labels121 (cdr ls737))))))
           (gen-label120 (lambda () (string #\i)))
           (wrap-subst119 cdr)
           (wrap-marks118 car)
           (make-wrap117 cons)
           (id-sym-name&marks116
             (lambda (x738 w739)
               (if (syntax-object?99 x738)
                 (values
                   (syntax-object-expression100 x738)
                   (join-marks135
                     (wrap-marks118 w739)
                     (wrap-marks118 (syntax-object-wrap101 x738))))
                 (values x738 (wrap-marks118 w739)))))
           (id?115
             (lambda (x740)
               (if (symbol? x740)
                 #t
                 (if (syntax-object?99 x740)
                   (symbol? (syntax-object-expression100 x740))
                   #f))))
           (nonsymbol-id?114
             (lambda (x741)
               (if (syntax-object?99 x741)
                 (symbol? (syntax-object-expression100 x741))
                 #f)))
           (global-extend113
             (lambda (type742 sym743 val744)
               (put-global-definition-hook78
                 sym743
                 type742
                 val744)))
           (lookup112
             (lambda (x745 r746 mod747)
               (let ((t748 (assq x745 r746)))
                 (if t748
                   (cdr t748)
                   (if (symbol? x745)
                     (let ((t749 (get-global-definition-hook79 x745 mod747)))
                       (if t749 t749 (quote (global))))
                     '(displaced-lexical))))))
           (macros-only-env111
             (lambda (r750)
               (if (null? r750)
                 '()
                 (let ((a751 (car r750)))
                   (if (eq? (cadr a751) (quote macro))
                     (cons a751 (macros-only-env111 (cdr r750)))
                     (macros-only-env111 (cdr r750)))))))
           (extend-var-env110
             (lambda (labels752 vars753 r754)
               (if (null? labels752)
                 r754
                 (extend-var-env110
                   (cdr labels752)
                   (cdr vars753)
                   (cons (cons (car labels752)
                               (cons (quote lexical) (car vars753)))
                         r754)))))
           (extend-env109
             (lambda (labels755 bindings756 r757)
               (if (null? labels755)
                 r757
                 (extend-env109
                   (cdr labels755)
                   (cdr bindings756)
                   (cons (cons (car labels755) (car bindings756))
                         r757)))))
           (binding-value108 cdr)
           (binding-type107 car)
           (source-annotation106
             (lambda (x758)
               (if (syntax-object?99 x758)
                 (source-annotation106
                   (syntax-object-expression100 x758))
                 (if (pair? x758)
                   (let ((props759 (source-properties x758)))
                     (if (pair? props759) props759 #f))
                   #f))))
           (set-syntax-object-module!105
             (lambda (x760 update761)
               (vector-set! x760 3 update761)))
           (set-syntax-object-wrap!104
             (lambda (x762 update763)
               (vector-set! x762 2 update763)))
           (set-syntax-object-expression!103
             (lambda (x764 update765)
               (vector-set! x764 1 update765)))
           (syntax-object-module102
             (lambda (x766) (vector-ref x766 3)))
           (syntax-object-wrap101
             (lambda (x767) (vector-ref x767 2)))
           (syntax-object-expression100
             (lambda (x768) (vector-ref x768 1)))
           (syntax-object?99
             (lambda (x769)
               (if (vector? x769)
                 (if (= (vector-length x769) 4)
                   (eq? (vector-ref x769 0) (quote syntax-object))
                   #f)
                 #f)))
           (make-syntax-object98
             (lambda (expression770 wrap771 module772)
               (vector
                 'syntax-object
                 expression770
                 wrap771
                 module772)))
           (build-letrec97
             (lambda (src773 ids774 vars775 val-exps776 body-exp777)
               (if (null? vars775)
                 body-exp777
                 (let ((atom-key778 (fluid-ref *mode*71)))
                   (if (memv atom-key778 (quote (c)))
                     (begin
                       (for-each maybe-name-value!89 ids774 val-exps776)
                       ((@ (language tree-il) make-letrec)
                        src773
                        ids774
                        vars775
                        val-exps776
                        body-exp777))
                     (decorate-source80
                       (list 'letrec
                             (map list vars775 val-exps776)
                             body-exp777)
                       src773))))))
           (build-named-let96
             (lambda (src779 ids780 vars781 val-exps782 body-exp783)
               (let ((f784 (car vars781))
                     (f-name785 (car ids780))
                     (vars786 (cdr vars781))
                     (ids787 (cdr ids780)))
                 (let ((atom-key788 (fluid-ref *mode*71)))
                   (if (memv atom-key788 (quote (c)))
                     (let ((proc789
                             (build-lambda91
                               src779
                               ids787
                               vars786
                               #f
                               body-exp783)))
                       (begin
                         (maybe-name-value!89 f-name785 proc789)
                         (for-each maybe-name-value!89 ids787 val-exps782)
                         ((@ (language tree-il) make-letrec)
                          src779
                          (list f-name785)
                          (list f784)
                          (list proc789)
                          (build-application82
                            src779
                            (build-lexical-reference84
                              'fun
                              src779
                              f-name785
                              f784)
                            val-exps782))))
                     (decorate-source80
                       (list 'let
                             f784
                             (map list vars786 val-exps782)
                             body-exp783)
                       src779))))))
           (build-let95
             (lambda (src790 ids791 vars792 val-exps793 body-exp794)
               (if (null? vars792)
                 body-exp794
                 (let ((atom-key795 (fluid-ref *mode*71)))
                   (if (memv atom-key795 (quote (c)))
                     (begin
                       (for-each maybe-name-value!89 ids791 val-exps793)
                       ((@ (language tree-il) make-let)
                        src790
                        ids791
                        vars792
                        val-exps793
                        body-exp794))
                     (decorate-source80
                       (list 'let
                             (map list vars792 val-exps793)
                             body-exp794)
                       src790))))))
           (build-sequence94
             (lambda (src796 exps797)
               (if (null? (cdr exps797))
                 (car exps797)
                 (let ((atom-key798 (fluid-ref *mode*71)))
                   (if (memv atom-key798 (quote (c)))
                     ((@ (language tree-il) make-sequence)
                      src796
                      exps797)
                     (decorate-source80
                       (cons (quote begin) exps797)
                       src796))))))
           (build-data93
             (lambda (src799 exp800)
               (let ((atom-key801 (fluid-ref *mode*71)))
                 (if (memv atom-key801 (quote (c)))
                   ((@ (language tree-il) make-const) src799 exp800)
                   (decorate-source80
                     (if (if (self-evaluating? exp800)
                           (not (vector? exp800))
                           #f)
                       exp800
                       (list (quote quote) exp800))
                     src799)))))
           (build-primref92
             (lambda (src802 name803)
               (if (equal?
                     (module-name (current-module))
                     '(guile))
                 (let ((atom-key804 (fluid-ref *mode*71)))
                   (if (memv atom-key804 (quote (c)))
                     ((@ (language tree-il) make-toplevel-ref)
                      src802
                      name803)
                     (decorate-source80 name803 src802)))
                 (let ((atom-key805 (fluid-ref *mode*71)))
                   (if (memv atom-key805 (quote (c)))
                     ((@ (language tree-il) make-module-ref)
                      src802
                      '(guile)
                      name803
                      #f)
                     (decorate-source80
                       (list (quote @@) (quote (guile)) name803)
                       src802))))))
           (build-lambda91
             (lambda (src806 ids807 vars808 docstring809 exp810)
               (let ((atom-key811 (fluid-ref *mode*71)))
                 (if (memv atom-key811 (quote (c)))
                   ((@ (language tree-il) make-lambda)
                    src806
                    ids807
                    vars808
                    (if docstring809
                      (list (cons (quote documentation) docstring809))
                      '())
                    exp810)
                   (decorate-source80
                     (cons 'lambda
                           (cons vars808
                                 (append
                                   (if docstring809
                                     (list docstring809)
                                     '())
                                   (list exp810))))
                     src806)))))
           (build-global-definition90
             (lambda (source812 var813 exp814)
               (let ((atom-key815 (fluid-ref *mode*71)))
                 (if (memv atom-key815 (quote (c)))
                   (begin
                     (maybe-name-value!89 var813 exp814)
                     ((@ (language tree-il) make-toplevel-define)
                      source812
                      var813
                      exp814))
                   (decorate-source80
                     (list (quote define) var813 exp814)
                     source812)))))
           (maybe-name-value!89
             (lambda (name816 val817)
               (if ((@ (language tree-il) lambda?) val817)
                 (let ((meta818
                         ((@ (language tree-il) lambda-meta) val817)))
                   (if (not (assq (quote name) meta818))
                     ((setter (@ (language tree-il) lambda-meta))
                      val817
                      (acons (quote name) name816 meta818)))))))
           (build-global-assignment88
             (lambda (source819 var820 exp821 mod822)
               (analyze-variable86
                 mod822
                 var820
                 (lambda (mod823 var824 public?825)
                   (let ((atom-key826 (fluid-ref *mode*71)))
                     (if (memv atom-key826 (quote (c)))
                       ((@ (language tree-il) make-module-set)
                        source819
                        mod823
                        var824
                        public?825
                        exp821)
                       (decorate-source80
                         (list 'set!
                               (list (if public?825 (quote @) (quote @@))
                                     mod823
                                     var824)
                               exp821)
                         source819))))
                 (lambda (var827)
                   (let ((atom-key828 (fluid-ref *mode*71)))
                     (if (memv atom-key828 (quote (c)))
                       ((@ (language tree-il) make-toplevel-set)
                        source819
                        var827
                        exp821)
                       (decorate-source80
                         (list (quote set!) var827 exp821)
                         source819)))))))
           (build-global-reference87
             (lambda (source829 var830 mod831)
               (analyze-variable86
                 mod831
                 var830
                 (lambda (mod832 var833 public?834)
                   (let ((atom-key835 (fluid-ref *mode*71)))
                     (if (memv atom-key835 (quote (c)))
                       ((@ (language tree-il) make-module-ref)
                        source829
                        mod832
                        var833
                        public?834)
                       (decorate-source80
                         (list (if public?834 (quote @) (quote @@))
                               mod832
                               var833)
                         source829))))
                 (lambda (var836)
                   (let ((atom-key837 (fluid-ref *mode*71)))
                     (if (memv atom-key837 (quote (c)))
                       ((@ (language tree-il) make-toplevel-ref)
                        source829
                        var836)
                       (decorate-source80 var836 source829)))))))
           (analyze-variable86
             (lambda (mod838 var839 modref-cont840 bare-cont841)
               (if (not mod838)
                 (bare-cont841 var839)
                 (let ((kind842 (car mod838)) (mod843 (cdr mod838)))
                   (if (memv kind842 (quote (public)))
                     (modref-cont840 mod843 var839 #t)
                     (if (memv kind842 (quote (private)))
                       (if (not (equal? mod843 (module-name (current-module))))
                         (modref-cont840 mod843 var839 #f)
                         (bare-cont841 var839))
                       (if (memv kind842 (quote (bare)))
                         (bare-cont841 var839)
                         (if (memv kind842 (quote (hygiene)))
                           (if (if (not (equal?
                                          mod843
                                          (module-name (current-module))))
                                 (module-variable
                                   (resolve-module mod843)
                                   var839)
                                 #f)
                             (modref-cont840 mod843 var839 #f)
                             (bare-cont841 var839))
                           (syntax-violation
                             #f
                             "bad module kind"
                             var839
                             mod843)))))))))
           (build-lexical-assignment85
             (lambda (source844 name845 var846 exp847)
               (let ((atom-key848 (fluid-ref *mode*71)))
                 (if (memv atom-key848 (quote (c)))
                   ((@ (language tree-il) make-lexical-set)
                    source844
                    name845
                    var846
                    exp847)
                   (decorate-source80
                     (list (quote set!) var846 exp847)
                     source844)))))
           (build-lexical-reference84
             (lambda (type849 source850 name851 var852)
               (let ((atom-key853 (fluid-ref *mode*71)))
                 (if (memv atom-key853 (quote (c)))
                   ((@ (language tree-il) make-lexical-ref)
                    source850
                    name851
                    var852)
                   (decorate-source80 var852 source850)))))
           (build-conditional83
             (lambda (source854 test-exp855 then-exp856 else-exp857)
               (let ((atom-key858 (fluid-ref *mode*71)))
                 (if (memv atom-key858 (quote (c)))
                   ((@ (language tree-il) make-conditional)
                    source854
                    test-exp855
                    then-exp856
                    else-exp857)
                   (decorate-source80
                     (if (equal? else-exp857 (quote (if #f #f)))
                       (list (quote if) test-exp855 then-exp856)
                       (list 'if
                             test-exp855
                             then-exp856
                             else-exp857))
                     source854)))))
           (build-application82
             (lambda (source859 fun-exp860 arg-exps861)
               (let ((atom-key862 (fluid-ref *mode*71)))
                 (if (memv atom-key862 (quote (c)))
                   ((@ (language tree-il) make-application)
                    source859
                    fun-exp860
                    arg-exps861)
                   (decorate-source80
                     (cons fun-exp860 arg-exps861)
                     source859)))))
           (build-void81
             (lambda (source863)
               (let ((atom-key864 (fluid-ref *mode*71)))
                 (if (memv atom-key864 (quote (c)))
                   ((@ (language tree-il) make-void) source863)
                   (decorate-source80 (quote (if #f #f)) source863)))))
           (decorate-source80
             (lambda (e865 s866)
               (begin
                 (if (if (pair? e865) s866 #f)
                   (set-source-properties! e865 s866))
                 e865)))
           (get-global-definition-hook79
             (lambda (symbol867 module868)
               (begin
                 (if (if (not module868) (current-module) #f)
                   (warn "module system is booted, we should have a module"
                         symbol867))
                 (let ((v869 (module-variable
                               (if module868
                                 (resolve-module (cdr module868))
                                 (current-module))
                               symbol867)))
                   (if v869
                     (if (variable-bound? v869)
                       (let ((val870 (variable-ref v869)))
                         (if (macro? val870)
                           (if (syncase-macro-type val870)
                             (cons (syncase-macro-type val870)
                                   (syncase-macro-binding val870))
                             #f)
                           #f))
                       #f)
                     #f)))))
           (put-global-definition-hook78
             (lambda (symbol871 type872 val873)
               (let ((existing874
                       (let ((v875 (module-variable
                                     (current-module)
                                     symbol871)))
                         (if v875
                           (if (variable-bound? v875)
                             (let ((val876 (variable-ref v875)))
                               (if (macro? val876)
                                 (if (not (syncase-macro-type val876))
                                   val876
                                   #f)
                                 #f))
                             #f)
                           #f))))
                 (module-define!
                   (current-module)
                   symbol871
                   (if existing874
                     (make-extended-syncase-macro
                       existing874
                       type872
                       val873)
                     (make-syncase-macro type872 val873))))))
           (local-eval-hook77
             (lambda (x877 mod878)
               (primitive-eval
                 (list noexpand70
                       (let ((atom-key879 (fluid-ref *mode*71)))
                         (if (memv atom-key879 (quote (c)))
                           ((@ (language tree-il) tree-il->scheme) x877)
                           x877))))))
           (top-level-eval-hook76
             (lambda (x880 mod881)
               (primitive-eval
                 (list noexpand70
                       (let ((atom-key882 (fluid-ref *mode*71)))
                         (if (memv atom-key882 (quote (c)))
                           ((@ (language tree-il) tree-il->scheme) x880)
                           x880))))))
           (fx<75 <)
           (fx=74 =)
           (fx-73 -)
           (fx+72 +)
           (*mode*71 (make-fluid))
           (noexpand70 "noexpand"))
    (begin
      (global-extend113
        'local-syntax
        'letrec-syntax
        #t)
      (global-extend113
        'local-syntax
        'let-syntax
        #f)
      (global-extend113
        'core
        'fluid-let-syntax
        (lambda (e883 r884 w885 s886 mod887)
          ((lambda (tmp888)
             ((lambda (tmp889)
                (if (if tmp889
                      (apply (lambda (_890 var891 val892 e1893 e2894)
                               (valid-bound-ids?140 var891))
                             tmp889)
                      #f)
                  (apply (lambda (_896 var897 val898 e1899 e2900)
                           (let ((names901
                                   (map (lambda (x902)
                                          (id-var-name137 x902 w885))
                                        var897)))
                             (begin
                               (for-each
                                 (lambda (id904 n905)
                                   (let ((atom-key906
                                           (binding-type107
                                             (lookup112 n905 r884 mod887))))
                                     (if (memv atom-key906
                                               '(displaced-lexical))
                                       (syntax-violation
                                         'fluid-let-syntax
                                         "identifier out of context"
                                         e883
                                         (source-wrap144
                                           id904
                                           w885
                                           s886
                                           mod887)))))
                                 var897
                                 names901)
                               (chi-body155
                                 (cons e1899 e2900)
                                 (source-wrap144 e883 w885 s886 mod887)
                                 (extend-env109
                                   names901
                                   (let ((trans-r909
                                           (macros-only-env111 r884)))
                                     (map (lambda (x910)
                                            (cons 'macro
                                                  (eval-local-transformer158
                                                    (chi151
                                                      x910
                                                      trans-r909
                                                      w885
                                                      mod887)
                                                    mod887)))
                                          val898))
                                   r884)
                                 w885
                                 mod887))))
                         tmp889)
                  ((lambda (_912)
                     (syntax-violation
                       'fluid-let-syntax
                       "bad syntax"
                       (source-wrap144 e883 w885 s886 mod887)))
                   tmp888)))
              ($sc-dispatch
                tmp888
                '(any #(each (any any)) any . each-any))))
           e883)))
      (global-extend113
        'core
        'quote
        (lambda (e913 r914 w915 s916 mod917)
          ((lambda (tmp918)
             ((lambda (tmp919)
                (if tmp919
                  (apply (lambda (_920 e921)
                           (build-data93 s916 (strip161 e921 w915)))
                         tmp919)
                  ((lambda (_922)
                     (syntax-violation
                       'quote
                       "bad syntax"
                       (source-wrap144 e913 w915 s916 mod917)))
                   tmp918)))
              ($sc-dispatch tmp918 (quote (any any)))))
           e913)))
      (global-extend113
        'core
        'syntax
        (letrec ((regen930
                   (lambda (x931)
                     (let ((atom-key932 (car x931)))
                       (if (memv atom-key932 (quote (ref)))
                         (build-lexical-reference84
                           'value
                           #f
                           (cadr x931)
                           (cadr x931))
                         (if (memv atom-key932 (quote (primitive)))
                           (build-primref92 #f (cadr x931))
                           (if (memv atom-key932 (quote (quote)))
                             (build-data93 #f (cadr x931))
                             (if (memv atom-key932 (quote (lambda)))
                               (build-lambda91
                                 #f
                                 (cadr x931)
                                 (cadr x931)
                                 #f
                                 (regen930 (caddr x931)))
                               (build-application82
                                 #f
                                 (build-primref92 #f (car x931))
                                 (map regen930 (cdr x931))))))))))
                 (gen-vector929
                   (lambda (x933)
                     (if (eq? (car x933) (quote list))
                       (cons (quote vector) (cdr x933))
                       (if (eq? (car x933) (quote quote))
                         (list (quote quote) (list->vector (cadr x933)))
                         (list (quote list->vector) x933)))))
                 (gen-append928
                   (lambda (x934 y935)
                     (if (equal? y935 (quote (quote ())))
                       x934
                       (list (quote append) x934 y935))))
                 (gen-cons927
                   (lambda (x936 y937)
                     (let ((atom-key938 (car y937)))
                       (if (memv atom-key938 (quote (quote)))
                         (if (eq? (car x936) (quote quote))
                           (list 'quote
                                 (cons (cadr x936) (cadr y937)))
                           (if (eq? (cadr y937) (quote ()))
                             (list (quote list) x936)
                             (list (quote cons) x936 y937)))
                         (if (memv atom-key938 (quote (list)))
                           (cons (quote list) (cons x936 (cdr y937)))
                           (list (quote cons) x936 y937))))))
                 (gen-map926
                   (lambda (e939 map-env940)
                     (let ((formals941 (map cdr map-env940))
                           (actuals942
                             (map (lambda (x943) (list (quote ref) (car x943)))
                                  map-env940)))
                       (if (eq? (car e939) (quote ref))
                         (car actuals942)
                         (if (and-map
                               (lambda (x944)
                                 (if (eq? (car x944) (quote ref))
                                   (memq (cadr x944) formals941)
                                   #f))
                               (cdr e939))
                           (cons 'map
                                 (cons (list (quote primitive) (car e939))
                                       (map (let ((r945 (map cons
                                                             formals941
                                                             actuals942)))
                                              (lambda (x946)
                                                (cdr (assq (cadr x946) r945))))
                                            (cdr e939))))
                           (cons 'map
                                 (cons (list (quote lambda) formals941 e939)
                                       actuals942)))))))
                 (gen-mappend925
                   (lambda (e947 map-env948)
                     (list 'apply
                           '(primitive append)
                           (gen-map926 e947 map-env948))))
                 (gen-ref924
                   (lambda (src949 var950 level951 maps952)
                     (if (fx=74 level951 0)
                       (values var950 maps952)
                       (if (null? maps952)
                         (syntax-violation
                           'syntax
                           "missing ellipsis"
                           src949)
                         (call-with-values
                           (lambda ()
                             (gen-ref924
                               src949
                               var950
                               (fx-73 level951 1)
                               (cdr maps952)))
                           (lambda (outer-var953 outer-maps954)
                             (let ((b955 (assq outer-var953 (car maps952))))
                               (if b955
                                 (values (cdr b955) maps952)
                                 (let ((inner-var956 (gen-var162 (quote tmp))))
                                   (values
                                     inner-var956
                                     (cons (cons (cons outer-var953
                                                       inner-var956)
                                                 (car maps952))
                                           outer-maps954)))))))))))
                 (gen-syntax923
                   (lambda (src957 e958 r959 maps960 ellipsis?961 mod962)
                     (if (id?115 e958)
                       (let ((label963 (id-var-name137 e958 (quote (())))))
                         (let ((b964 (lookup112 label963 r959 mod962)))
                           (if (eq? (binding-type107 b964) (quote syntax))
                             (call-with-values
                               (lambda ()
                                 (let ((var.lev965 (binding-value108 b964)))
                                   (gen-ref924
                                     src957
                                     (car var.lev965)
                                     (cdr var.lev965)
                                     maps960)))
                               (lambda (var966 maps967)
                                 (values (list (quote ref) var966) maps967)))
                             (if (ellipsis?961 e958)
                               (syntax-violation
                                 'syntax
                                 "misplaced ellipsis"
                                 src957)
                               (values (list (quote quote) e958) maps960)))))
                       ((lambda (tmp968)
                          ((lambda (tmp969)
                             (if (if tmp969
                                   (apply (lambda (dots970 e971)
                                            (ellipsis?961 dots970))
                                          tmp969)
                                   #f)
                               (apply (lambda (dots972 e973)
                                        (gen-syntax923
                                          src957
                                          e973
                                          r959
                                          maps960
                                          (lambda (x974) #f)
                                          mod962))
                                      tmp969)
                               ((lambda (tmp975)
                                  (if (if tmp975
                                        (apply (lambda (x976 dots977 y978)
                                                 (ellipsis?961 dots977))
                                               tmp975)
                                        #f)
                                    (apply (lambda (x979 dots980 y981)
                                             (letrec ((f982 (lambda (y983 k984)
                                                              ((lambda (tmp988)
                                                                 ((lambda (tmp989)
                                                                    (if (if tmp989
                                                                          (apply (lambda (dots990
                                                                                          y991)
                                                                                   (ellipsis?961
                                                                                     dots990))
                                                                                 tmp989)
                                                                          #f)
                                                                      (apply (lambda (dots992
                                                                                      y993)
                                                                               (f982 y993
                                                                                     (lambda (maps994)
                                                                                       (call-with-values
                                                                                         (lambda ()
                                                                                           (k984 (cons '()
                                                                                                       maps994)))
                                                                                         (lambda (x995
                                                                                                  maps996)
                                                                                           (if (null? (car maps996))
                                                                                             (syntax-violation
                                                                                               'syntax
                                                                                               "extra ellipsis"
                                                                                               src957)
                                                                                             (values
                                                                                               (gen-mappend925
                                                                                                 x995
                                                                                                 (car maps996))
                                                                                               (cdr maps996))))))))
                                                                             tmp989)
                                                                      ((lambda (_997)
                                                                         (call-with-values
                                                                           (lambda ()
                                                                             (gen-syntax923
                                                                               src957
                                                                               y983
                                                                               r959
                                                                               maps960
                                                                               ellipsis?961
                                                                               mod962))
                                                                           (lambda (y998
                                                                                    maps999)
                                                                             (call-with-values
                                                                               (lambda ()
                                                                                 (k984 maps999))
                                                                               (lambda (x1000
                                                                                        maps1001)
                                                                                 (values
                                                                                   (gen-append928
                                                                                     x1000
                                                                                     y998)
                                                                                   maps1001))))))
                                                                       tmp988)))
                                                                  ($sc-dispatch
                                                                    tmp988
                                                                    '(any .
                                                                          any))))
                                                               y983))))
                                               (f982 y981
                                                     (lambda (maps985)
                                                       (call-with-values
                                                         (lambda ()
                                                           (gen-syntax923
                                                             src957
                                                             x979
                                                             r959
                                                             (cons '()
                                                                   maps985)
                                                             ellipsis?961
                                                             mod962))
                                                         (lambda (x986 maps987)
                                                           (if (null? (car maps987))
                                                             (syntax-violation
                                                               'syntax
                                                               "extra ellipsis"
                                                               src957)
                                                             (values
                                                               (gen-map926
                                                                 x986
                                                                 (car maps987))
                                                               (cdr maps987)))))))))
                                           tmp975)
                                    ((lambda (tmp1002)
                                       (if tmp1002
                                         (apply (lambda (x1003 y1004)
                                                  (call-with-values
                                                    (lambda ()
                                                      (gen-syntax923
                                                        src957
                                                        x1003
                                                        r959
                                                        maps960
                                                        ellipsis?961
                                                        mod962))
                                                    (lambda (x1005 maps1006)
                                                      (call-with-values
                                                        (lambda ()
                                                          (gen-syntax923
                                                            src957
                                                            y1004
                                                            r959
                                                            maps1006
                                                            ellipsis?961
                                                            mod962))
                                                        (lambda (y1007
                                                                 maps1008)
                                                          (values
                                                            (gen-cons927
                                                              x1005
                                                              y1007)
                                                            maps1008))))))
                                                tmp1002)
                                         ((lambda (tmp1009)
                                            (if tmp1009
                                              (apply (lambda (e11010 e21011)
                                                       (call-with-values
                                                         (lambda ()
                                                           (gen-syntax923
                                                             src957
                                                             (cons e11010
                                                                   e21011)
                                                             r959
                                                             maps960
                                                             ellipsis?961
                                                             mod962))
                                                         (lambda (e1013
                                                                  maps1014)
                                                           (values
                                                             (gen-vector929
                                                               e1013)
                                                             maps1014))))
                                                     tmp1009)
                                              ((lambda (_1015)
                                                 (values
                                                   (list (quote quote) e958)
                                                   maps960))
                                               tmp968)))
                                          ($sc-dispatch
                                            tmp968
                                            '#(vector (any . each-any))))))
                                     ($sc-dispatch
                                       tmp968
                                       '(any . any)))))
                                ($sc-dispatch
                                  tmp968
                                  '(any any . any)))))
                           ($sc-dispatch tmp968 (quote (any any)))))
                        e958)))))
          (lambda (e1016 r1017 w1018 s1019 mod1020)
            (let ((e1021 (source-wrap144 e1016 w1018 s1019 mod1020)))
              ((lambda (tmp1022)
                 ((lambda (tmp1023)
                    (if tmp1023
                      (apply (lambda (_1024 x1025)
                               (call-with-values
                                 (lambda ()
                                   (gen-syntax923
                                     e1021
                                     x1025
                                     r1017
                                     '()
                                     ellipsis?160
                                     mod1020))
                                 (lambda (e1026 maps1027) (regen930 e1026))))
                             tmp1023)
                      ((lambda (_1028)
                         (syntax-violation
                           'syntax
                           "bad `syntax' form"
                           e1021))
                       tmp1022)))
                  ($sc-dispatch tmp1022 (quote (any any)))))
               e1021)))))
      (global-extend113
        'core
        'lambda
        (lambda (e1029 r1030 w1031 s1032 mod1033)
          ((lambda (tmp1034)
             ((lambda (tmp1035)
                (if tmp1035
                  (apply (lambda (_1036 c1037)
                           (chi-lambda-clause156
                             (source-wrap144 e1029 w1031 s1032 mod1033)
                             #f
                             c1037
                             r1030
                             w1031
                             mod1033
                             (lambda (names1038
                                      vars1039
                                      docstring1040
                                      body1041)
                               (build-lambda91
                                 s1032
                                 names1038
                                 vars1039
                                 docstring1040
                                 body1041))))
                         tmp1035)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    tmp1034)))
              ($sc-dispatch tmp1034 (quote (any . any)))))
           e1029)))
      (global-extend113
        'core
        'let
        (letrec ((chi-let1042
                   (lambda (e1043
                            r1044
                            w1045
                            s1046
                            mod1047
                            constructor1048
                            ids1049
                            vals1050
                            exps1051)
                     (if (not (valid-bound-ids?140 ids1049))
                       (syntax-violation
                         'let
                         "duplicate bound variable"
                         e1043)
                       (let ((labels1052 (gen-labels121 ids1049))
                             (new-vars1053 (map gen-var162 ids1049)))
                         (let ((nw1054
                                 (make-binding-wrap132
                                   ids1049
                                   labels1052
                                   w1045))
                               (nr1055
                                 (extend-var-env110
                                   labels1052
                                   new-vars1053
                                   r1044)))
                           (constructor1048
                             s1046
                             (map syntax->datum ids1049)
                             new-vars1053
                             (map (lambda (x1056)
                                    (chi151 x1056 r1044 w1045 mod1047))
                                  vals1050)
                             (chi-body155
                               exps1051
                               (source-wrap144 e1043 nw1054 s1046 mod1047)
                               nr1055
                               nw1054
                               mod1047))))))))
          (lambda (e1057 r1058 w1059 s1060 mod1061)
            ((lambda (tmp1062)
               ((lambda (tmp1063)
                  (if (if tmp1063
                        (apply (lambda (_1064 id1065 val1066 e11067 e21068)
                                 (and-map id?115 id1065))
                               tmp1063)
                        #f)
                    (apply (lambda (_1070 id1071 val1072 e11073 e21074)
                             (chi-let1042
                               e1057
                               r1058
                               w1059
                               s1060
                               mod1061
                               build-let95
                               id1071
                               val1072
                               (cons e11073 e21074)))
                           tmp1063)
                    ((lambda (tmp1078)
                       (if (if tmp1078
                             (apply (lambda (_1079
                                             f1080
                                             id1081
                                             val1082
                                             e11083
                                             e21084)
                                      (if (id?115 f1080)
                                        (and-map id?115 id1081)
                                        #f))
                                    tmp1078)
                             #f)
                         (apply (lambda (_1086
                                         f1087
                                         id1088
                                         val1089
                                         e11090
                                         e21091)
                                  (chi-let1042
                                    e1057
                                    r1058
                                    w1059
                                    s1060
                                    mod1061
                                    build-named-let96
                                    (cons f1087 id1088)
                                    val1089
                                    (cons e11090 e21091)))
                                tmp1078)
                         ((lambda (_1095)
                            (syntax-violation
                              'let
                              "bad let"
                              (source-wrap144 e1057 w1059 s1060 mod1061)))
                          tmp1062)))
                     ($sc-dispatch
                       tmp1062
                       '(any any #(each (any any)) any . each-any)))))
                ($sc-dispatch
                  tmp1062
                  '(any #(each (any any)) any . each-any))))
             e1057))))
      (global-extend113
        'core
        'letrec
        (lambda (e1096 r1097 w1098 s1099 mod1100)
          ((lambda (tmp1101)
             ((lambda (tmp1102)
                (if (if tmp1102
                      (apply (lambda (_1103 id1104 val1105 e11106 e21107)
                               (and-map id?115 id1104))
                             tmp1102)
                      #f)
                  (apply (lambda (_1109 id1110 val1111 e11112 e21113)
                           (let ((ids1114 id1110))
                             (if (not (valid-bound-ids?140 ids1114))
                               (syntax-violation
                                 'letrec
                                 "duplicate bound variable"
                                 e1096)
                               (let ((labels1116 (gen-labels121 ids1114))
                                     (new-vars1117 (map gen-var162 ids1114)))
                                 (let ((w1118 (make-binding-wrap132
                                                ids1114
                                                labels1116
                                                w1098))
                                       (r1119 (extend-var-env110
                                                labels1116
                                                new-vars1117
                                                r1097)))
                                   (build-letrec97
                                     s1099
                                     (map syntax->datum ids1114)
                                     new-vars1117
                                     (map (lambda (x1120)
                                            (chi151 x1120 r1119 w1118 mod1100))
                                          val1111)
                                     (chi-body155
                                       (cons e11112 e21113)
                                       (source-wrap144
                                         e1096
                                         w1118
                                         s1099
                                         mod1100)
                                       r1119
                                       w1118
                                       mod1100)))))))
                         tmp1102)
                  ((lambda (_1123)
                     (syntax-violation
                       'letrec
                       "bad letrec"
                       (source-wrap144 e1096 w1098 s1099 mod1100)))
                   tmp1101)))
              ($sc-dispatch
                tmp1101
                '(any #(each (any any)) any . each-any))))
           e1096)))
      (global-extend113
        'core
        'set!
        (lambda (e1124 r1125 w1126 s1127 mod1128)
          ((lambda (tmp1129)
             ((lambda (tmp1130)
                (if (if tmp1130
                      (apply (lambda (_1131 id1132 val1133) (id?115 id1132))
                             tmp1130)
                      #f)
                  (apply (lambda (_1134 id1135 val1136)
                           (let ((val1137 (chi151 val1136 r1125 w1126 mod1128))
                                 (n1138 (id-var-name137 id1135 w1126)))
                             (let ((b1139 (lookup112 n1138 r1125 mod1128)))
                               (let ((atom-key1140 (binding-type107 b1139)))
                                 (if (memv atom-key1140 (quote (lexical)))
                                   (build-lexical-assignment85
                                     s1127
                                     (syntax->datum id1135)
                                     (binding-value108 b1139)
                                     val1137)
                                   (if (memv atom-key1140 (quote (global)))
                                     (build-global-assignment88
                                       s1127
                                       n1138
                                       val1137
                                       mod1128)
                                     (if (memv atom-key1140
                                               '(displaced-lexical))
                                       (syntax-violation
                                         'set!
                                         "identifier out of context"
                                         (wrap143 id1135 w1126 mod1128))
                                       (syntax-violation
                                         'set!
                                         "bad set!"
                                         (source-wrap144
                                           e1124
                                           w1126
                                           s1127
                                           mod1128)))))))))
                         tmp1130)
                  ((lambda (tmp1141)
                     (if tmp1141
                       (apply (lambda (_1142 head1143 tail1144 val1145)
                                (call-with-values
                                  (lambda ()
                                    (syntax-type149
                                      head1143
                                      r1125
                                      '(())
                                      #f
                                      #f
                                      mod1128
                                      #t))
                                  (lambda (type1146
                                           value1147
                                           ee1148
                                           ww1149
                                           ss1150
                                           modmod1151)
                                    (if (memv type1146 (quote (module-ref)))
                                      (let ((val1152
                                              (chi151
                                                val1145
                                                r1125
                                                w1126
                                                mod1128)))
                                        (call-with-values
                                          (lambda ()
                                            (value1147
                                              (cons head1143 tail1144)))
                                          (lambda (id1154 mod1155)
                                            (build-global-assignment88
                                              s1127
                                              id1154
                                              val1152
                                              mod1155))))
                                      (build-application82
                                        s1127
                                        (chi151
                                          (list '#(syntax-object
                                                   setter
                                                   ((top)
                                                    #(ribcage () () ())
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(type
                                                        value
                                                        ee
                                                        ww
                                                        ss
                                                        modmod)
                                                      #((top)
                                                        (top)
                                                        (top)
                                                        (top)
                                                        (top)
                                                        (top))
                                                      #("i"
                                                        "i"
                                                        "i"
                                                        "i"
                                                        "i"
                                                        "i"))
                                                    #(ribcage
                                                      #(_ head tail val)
                                                      #((top)
                                                        (top)
                                                        (top)
                                                        (top))
                                                      #("i" "i" "i" "i"))
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(e r w s mod)
                                                      #((top)
                                                        (top)
                                                        (top)
                                                        (top)
                                                        (top))
                                                      #("i" "i" "i" "i" "i"))
                                                    #(ribcage
                                                      (lambda-var-list
                                                        gen-var
                                                        strip
                                                        ellipsis?
                                                        chi-void
                                                        eval-local-transformer
                                                        chi-local-syntax
                                                        chi-lambda-clause
                                                        chi-body
                                                        chi-macro
                                                        chi-application
                                                        chi-expr
                                                        chi
                                                        chi-top
                                                        syntax-type
                                                        chi-when-list
                                                        chi-install-global
                                                        chi-top-sequence
                                                        chi-sequence
                                                        source-wrap
                                                        wrap
                                                        bound-id-member?
                                                        distinct-bound-ids?
                                                        valid-bound-ids?
                                                        bound-id=?
                                                        free-id=?
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
                                                        build-lambda
                                                        build-global-definition
                                                        maybe-name-value!
                                                        build-global-assignment
                                                        build-global-reference
                                                        analyze-variable
                                                        build-lexical-assignment
                                                        build-lexical-reference
                                                        build-conditional
                                                        build-application
                                                        build-void
                                                        decorate-source
                                                        get-global-definition-hook
                                                        put-global-definition-hook
                                                        gensym-hook
                                                        local-eval-hook
                                                        top-level-eval-hook
                                                        fx<
                                                        fx=
                                                        fx-
                                                        fx+
                                                        *mode*
                                                        noexpand)
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
                                                       (top))
                                                      ("i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"
                                                       "i"))
                                                    #(ribcage
                                                      (define-structure
                                                        and-map*)
                                                      ((top) (top))
                                                      ("i" "i")))
                                                   (hygiene guile))
                                                head1143)
                                          r1125
                                          w1126
                                          mod1128)
                                        (map (lambda (e1156)
                                               (chi151
                                                 e1156
                                                 r1125
                                                 w1126
                                                 mod1128))
                                             (append
                                               tail1144
                                               (list val1145))))))))
                              tmp1141)
                       ((lambda (_1158)
                          (syntax-violation
                            'set!
                            "bad set!"
                            (source-wrap144 e1124 w1126 s1127 mod1128)))
                        tmp1129)))
                   ($sc-dispatch
                     tmp1129
                     '(any (any . each-any) any)))))
              ($sc-dispatch tmp1129 (quote (any any any)))))
           e1124)))
      (global-extend113
        'module-ref
        '@
        (lambda (e1159)
          ((lambda (tmp1160)
             ((lambda (tmp1161)
                (if (if tmp1161
                      (apply (lambda (_1162 mod1163 id1164)
                               (if (and-map id?115 mod1163)
                                 (id?115 id1164)
                                 #f))
                             tmp1161)
                      #f)
                  (apply (lambda (_1166 mod1167 id1168)
                           (values
                             (syntax->datum id1168)
                             (syntax->datum
                               (cons '#(syntax-object
                                        public
                                        ((top)
                                         #(ribcage
                                           #(_ mod id)
                                           #((top) (top) (top))
                                           #("i" "i" "i"))
                                         #(ribcage () () ())
                                         #(ribcage #(e) #((top)) #("i"))
                                         #(ribcage
                                           (lambda-var-list
                                             gen-var
                                             strip
                                             ellipsis?
                                             chi-void
                                             eval-local-transformer
                                             chi-local-syntax
                                             chi-lambda-clause
                                             chi-body
                                             chi-macro
                                             chi-application
                                             chi-expr
                                             chi
                                             chi-top
                                             syntax-type
                                             chi-when-list
                                             chi-install-global
                                             chi-top-sequence
                                             chi-sequence
                                             source-wrap
                                             wrap
                                             bound-id-member?
                                             distinct-bound-ids?
                                             valid-bound-ids?
                                             bound-id=?
                                             free-id=?
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
                                             build-lambda
                                             build-global-definition
                                             maybe-name-value!
                                             build-global-assignment
                                             build-global-reference
                                             analyze-variable
                                             build-lexical-assignment
                                             build-lexical-reference
                                             build-conditional
                                             build-application
                                             build-void
                                             decorate-source
                                             get-global-definition-hook
                                             put-global-definition-hook
                                             gensym-hook
                                             local-eval-hook
                                             top-level-eval-hook
                                             fx<
                                             fx=
                                             fx-
                                             fx+
                                             *mode*
                                             noexpand)
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
                                            (top))
                                           ("i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"))
                                         #(ribcage
                                           (define-structure and-map*)
                                           ((top) (top))
                                           ("i" "i")))
                                        (hygiene guile))
                                     mod1167))))
                         tmp1161)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    tmp1160)))
              ($sc-dispatch tmp1160 (quote (any each-any any)))))
           e1159)))
      (global-extend113
        'module-ref
        '@@
        (lambda (e1170)
          ((lambda (tmp1171)
             ((lambda (tmp1172)
                (if (if tmp1172
                      (apply (lambda (_1173 mod1174 id1175)
                               (if (and-map id?115 mod1174)
                                 (id?115 id1175)
                                 #f))
                             tmp1172)
                      #f)
                  (apply (lambda (_1177 mod1178 id1179)
                           (values
                             (syntax->datum id1179)
                             (syntax->datum
                               (cons '#(syntax-object
                                        private
                                        ((top)
                                         #(ribcage
                                           #(_ mod id)
                                           #((top) (top) (top))
                                           #("i" "i" "i"))
                                         #(ribcage () () ())
                                         #(ribcage #(e) #((top)) #("i"))
                                         #(ribcage
                                           (lambda-var-list
                                             gen-var
                                             strip
                                             ellipsis?
                                             chi-void
                                             eval-local-transformer
                                             chi-local-syntax
                                             chi-lambda-clause
                                             chi-body
                                             chi-macro
                                             chi-application
                                             chi-expr
                                             chi
                                             chi-top
                                             syntax-type
                                             chi-when-list
                                             chi-install-global
                                             chi-top-sequence
                                             chi-sequence
                                             source-wrap
                                             wrap
                                             bound-id-member?
                                             distinct-bound-ids?
                                             valid-bound-ids?
                                             bound-id=?
                                             free-id=?
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
                                             build-lambda
                                             build-global-definition
                                             maybe-name-value!
                                             build-global-assignment
                                             build-global-reference
                                             analyze-variable
                                             build-lexical-assignment
                                             build-lexical-reference
                                             build-conditional
                                             build-application
                                             build-void
                                             decorate-source
                                             get-global-definition-hook
                                             put-global-definition-hook
                                             gensym-hook
                                             local-eval-hook
                                             top-level-eval-hook
                                             fx<
                                             fx=
                                             fx-
                                             fx+
                                             *mode*
                                             noexpand)
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
                                            (top))
                                           ("i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"
                                            "i"))
                                         #(ribcage
                                           (define-structure and-map*)
                                           ((top) (top))
                                           ("i" "i")))
                                        (hygiene guile))
                                     mod1178))))
                         tmp1172)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    tmp1171)))
              ($sc-dispatch tmp1171 (quote (any each-any any)))))
           e1170)))
      (global-extend113
        'core
        'if
        (lambda (e1181 r1182 w1183 s1184 mod1185)
          ((lambda (tmp1186)
             ((lambda (tmp1187)
                (if tmp1187
                  (apply (lambda (_1188 test1189 then1190)
                           (build-conditional83
                             s1184
                             (chi151 test1189 r1182 w1183 mod1185)
                             (chi151 then1190 r1182 w1183 mod1185)
                             (build-void81 #f)))
                         tmp1187)
                  ((lambda (tmp1191)
                     (if tmp1191
                       (apply (lambda (_1192 test1193 then1194 else1195)
                                (build-conditional83
                                  s1184
                                  (chi151 test1193 r1182 w1183 mod1185)
                                  (chi151 then1194 r1182 w1183 mod1185)
                                  (chi151 else1195 r1182 w1183 mod1185)))
                              tmp1191)
                       (syntax-violation
                         #f
                         "source expression failed to match any pattern"
                         tmp1186)))
                   ($sc-dispatch tmp1186 (quote (any any any any))))))
              ($sc-dispatch tmp1186 (quote (any any any)))))
           e1181)))
      (global-extend113
        'begin
        'begin
        '())
      (global-extend113
        'define
        'define
        '())
      (global-extend113
        'define-syntax
        'define-syntax
        '())
      (global-extend113
        'eval-when
        'eval-when
        '())
      (global-extend113
        'core
        'syntax-case
        (letrec ((gen-syntax-case1199
                   (lambda (x1200 keys1201 clauses1202 r1203 mod1204)
                     (if (null? clauses1202)
                       (build-application82
                         #f
                         (build-primref92 #f (quote syntax-violation))
                         (list (build-data93 #f #f)
                               (build-data93
                                 #f
                                 "source expression failed to match any pattern")
                               x1200))
                       ((lambda (tmp1205)
                          ((lambda (tmp1206)
                             (if tmp1206
                               (apply (lambda (pat1207 exp1208)
                                        (if (if (id?115 pat1207)
                                              (and-map
                                                (lambda (x1209)
                                                  (not (free-id=?138
                                                         pat1207
                                                         x1209)))
                                                (cons '#(syntax-object
                                                         ...
                                                         ((top)
                                                          #(ribcage
                                                            #(pat exp)
                                                            #((top) (top))
                                                            #("i" "i"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(x
                                                              keys
                                                              clauses
                                                              r
                                                              mod)
                                                            #((top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top))
                                                            #("i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"))
                                                          #(ribcage
                                                            (gen-syntax-case
                                                              gen-clause
                                                              build-dispatch-call
                                                              convert-pattern)
                                                            ((top)
                                                             (top)
                                                             (top)
                                                             (top))
                                                            ("i" "i" "i" "i"))
                                                          #(ribcage
                                                            (lambda-var-list
                                                              gen-var
                                                              strip
                                                              ellipsis?
                                                              chi-void
                                                              eval-local-transformer
                                                              chi-local-syntax
                                                              chi-lambda-clause
                                                              chi-body
                                                              chi-macro
                                                              chi-application
                                                              chi-expr
                                                              chi
                                                              chi-top
                                                              syntax-type
                                                              chi-when-list
                                                              chi-install-global
                                                              chi-top-sequence
                                                              chi-sequence
                                                              source-wrap
                                                              wrap
                                                              bound-id-member?
                                                              distinct-bound-ids?
                                                              valid-bound-ids?
                                                              bound-id=?
                                                              free-id=?
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
                                                              build-lambda
                                                              build-global-definition
                                                              maybe-name-value!
                                                              build-global-assignment
                                                              build-global-reference
                                                              analyze-variable
                                                              build-lexical-assignment
                                                              build-lexical-reference
                                                              build-conditional
                                                              build-application
                                                              build-void
                                                              decorate-source
                                                              get-global-definition-hook
                                                              put-global-definition-hook
                                                              gensym-hook
                                                              local-eval-hook
                                                              top-level-eval-hook
                                                              fx<
                                                              fx=
                                                              fx-
                                                              fx+
                                                              *mode*
                                                              noexpand)
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
                                                             (top))
                                                            ("i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"
                                                             "i"))
                                                          #(ribcage
                                                            (define-structure
                                                              and-map*)
                                                            ((top) (top))
                                                            ("i" "i")))
                                                         (hygiene guile))
                                                      keys1201))
                                              #f)
                                          (let ((labels1210
                                                  (list (gen-label120)))
                                                (var1211 (gen-var162 pat1207)))
                                            (build-application82
                                              #f
                                              (build-lambda91
                                                #f
                                                (list (syntax->datum pat1207))
                                                (list var1211)
                                                #f
                                                (chi151
                                                  exp1208
                                                  (extend-env109
                                                    labels1210
                                                    (list (cons 'syntax
                                                                (cons var1211
                                                                      0)))
                                                    r1203)
                                                  (make-binding-wrap132
                                                    (list pat1207)
                                                    labels1210
                                                    '(()))
                                                  mod1204))
                                              (list x1200)))
                                          (gen-clause1198
                                            x1200
                                            keys1201
                                            (cdr clauses1202)
                                            r1203
                                            pat1207
                                            #t
                                            exp1208
                                            mod1204)))
                                      tmp1206)
                               ((lambda (tmp1212)
                                  (if tmp1212
                                    (apply (lambda (pat1213 fender1214 exp1215)
                                             (gen-clause1198
                                               x1200
                                               keys1201
                                               (cdr clauses1202)
                                               r1203
                                               pat1213
                                               fender1214
                                               exp1215
                                               mod1204))
                                           tmp1212)
                                    ((lambda (_1216)
                                       (syntax-violation
                                         'syntax-case
                                         "invalid clause"
                                         (car clauses1202)))
                                     tmp1205)))
                                ($sc-dispatch tmp1205 (quote (any any any))))))
                           ($sc-dispatch tmp1205 (quote (any any)))))
                        (car clauses1202)))))
                 (gen-clause1198
                   (lambda (x1217
                            keys1218
                            clauses1219
                            r1220
                            pat1221
                            fender1222
                            exp1223
                            mod1224)
                     (call-with-values
                       (lambda ()
                         (convert-pattern1196 pat1221 keys1218))
                       (lambda (p1225 pvars1226)
                         (if (not (distinct-bound-ids?141 (map car pvars1226)))
                           (syntax-violation
                             'syntax-case
                             "duplicate pattern variable"
                             pat1221)
                           (if (not (and-map
                                      (lambda (x1227)
                                        (not (ellipsis?160 (car x1227))))
                                      pvars1226))
                             (syntax-violation
                               'syntax-case
                               "misplaced ellipsis"
                               pat1221)
                             (let ((y1228 (gen-var162 (quote tmp))))
                               (build-application82
                                 #f
                                 (build-lambda91
                                   #f
                                   (list (quote tmp))
                                   (list y1228)
                                   #f
                                   (let ((y1229 (build-lexical-reference84
                                                  'value
                                                  #f
                                                  'tmp
                                                  y1228)))
                                     (build-conditional83
                                       #f
                                       ((lambda (tmp1230)
                                          ((lambda (tmp1231)
                                             (if tmp1231
                                               (apply (lambda () y1229)
                                                      tmp1231)
                                               ((lambda (_1232)
                                                  (build-conditional83
                                                    #f
                                                    y1229
                                                    (build-dispatch-call1197
                                                      pvars1226
                                                      fender1222
                                                      y1229
                                                      r1220
                                                      mod1224)
                                                    (build-data93 #f #f)))
                                                tmp1230)))
                                           ($sc-dispatch
                                             tmp1230
                                             '#(atom #t))))
                                        fender1222)
                                       (build-dispatch-call1197
                                         pvars1226
                                         exp1223
                                         y1229
                                         r1220
                                         mod1224)
                                       (gen-syntax-case1199
                                         x1217
                                         keys1218
                                         clauses1219
                                         r1220
                                         mod1224))))
                                 (list (if (eq? p1225 (quote any))
                                         (build-application82
                                           #f
                                           (build-primref92 #f (quote list))
                                           (list x1217))
                                         (build-application82
                                           #f
                                           (build-primref92
                                             #f
                                             '$sc-dispatch)
                                           (list x1217
                                                 (build-data93
                                                   #f
                                                   p1225)))))))))))))
                 (build-dispatch-call1197
                   (lambda (pvars1233 exp1234 y1235 r1236 mod1237)
                     (let ((ids1238 (map car pvars1233))
                           (levels1239 (map cdr pvars1233)))
                       (let ((labels1240 (gen-labels121 ids1238))
                             (new-vars1241 (map gen-var162 ids1238)))
                         (build-application82
                           #f
                           (build-primref92 #f (quote apply))
                           (list (build-lambda91
                                   #f
                                   (map syntax->datum ids1238)
                                   new-vars1241
                                   #f
                                   (chi151
                                     exp1234
                                     (extend-env109
                                       labels1240
                                       (map (lambda (var1242 level1243)
                                              (cons 'syntax
                                                    (cons var1242 level1243)))
                                            new-vars1241
                                            (map cdr pvars1233))
                                       r1236)
                                     (make-binding-wrap132
                                       ids1238
                                       labels1240
                                       '(()))
                                     mod1237))
                                 y1235))))))
                 (convert-pattern1196
                   (lambda (pattern1244 keys1245)
                     (letrec ((cvt1246
                                (lambda (p1247 n1248 ids1249)
                                  (if (id?115 p1247)
                                    (if (bound-id-member?142 p1247 keys1245)
                                      (values
                                        (vector (quote free-id) p1247)
                                        ids1249)
                                      (values
                                        'any
                                        (cons (cons p1247 n1248) ids1249)))
                                    ((lambda (tmp1250)
                                       ((lambda (tmp1251)
                                          (if (if tmp1251
                                                (apply (lambda (x1252 dots1253)
                                                         (ellipsis?160
                                                           dots1253))
                                                       tmp1251)
                                                #f)
                                            (apply (lambda (x1254 dots1255)
                                                     (call-with-values
                                                       (lambda ()
                                                         (cvt1246
                                                           x1254
                                                           (fx+72 n1248 1)
                                                           ids1249))
                                                       (lambda (p1256 ids1257)
                                                         (values
                                                           (if (eq? p1256
                                                                    'any)
                                                             'each-any
                                                             (vector
                                                               'each
                                                               p1256))
                                                           ids1257))))
                                                   tmp1251)
                                            ((lambda (tmp1258)
                                               (if tmp1258
                                                 (apply (lambda (x1259 y1260)
                                                          (call-with-values
                                                            (lambda ()
                                                              (cvt1246
                                                                y1260
                                                                n1248
                                                                ids1249))
                                                            (lambda (y1261
                                                                     ids1262)
                                                              (call-with-values
                                                                (lambda ()
                                                                  (cvt1246
                                                                    x1259
                                                                    n1248
                                                                    ids1262))
                                                                (lambda (x1263
                                                                         ids1264)
                                                                  (values
                                                                    (cons x1263
                                                                          y1261)
                                                                    ids1264))))))
                                                        tmp1258)
                                                 ((lambda (tmp1265)
                                                    (if tmp1265
                                                      (apply (lambda ()
                                                               (values
                                                                 '()
                                                                 ids1249))
                                                             tmp1265)
                                                      ((lambda (tmp1266)
                                                         (if tmp1266
                                                           (apply (lambda (x1267)
                                                                    (call-with-values
                                                                      (lambda ()
                                                                        (cvt1246
                                                                          x1267
                                                                          n1248
                                                                          ids1249))
                                                                      (lambda (p1269
                                                                               ids1270)
                                                                        (values
                                                                          (vector
                                                                            'vector
                                                                            p1269)
                                                                          ids1270))))
                                                                  tmp1266)
                                                           ((lambda (x1271)
                                                              (values
                                                                (vector
                                                                  'atom
                                                                  (strip161
                                                                    p1247
                                                                    '(())))
                                                                ids1249))
                                                            tmp1250)))
                                                       ($sc-dispatch
                                                         tmp1250
                                                         '#(vector
                                                            each-any)))))
                                                  ($sc-dispatch
                                                    tmp1250
                                                    '()))))
                                             ($sc-dispatch
                                               tmp1250
                                               '(any . any)))))
                                        ($sc-dispatch
                                          tmp1250
                                          '(any any))))
                                     p1247)))))
                       (cvt1246 pattern1244 0 (quote ()))))))
          (lambda (e1272 r1273 w1274 s1275 mod1276)
            (let ((e1277 (source-wrap144 e1272 w1274 s1275 mod1276)))
              ((lambda (tmp1278)
                 ((lambda (tmp1279)
                    (if tmp1279
                      (apply (lambda (_1280 val1281 key1282 m1283)
                               (if (and-map
                                     (lambda (x1284)
                                       (if (id?115 x1284)
                                         (not (ellipsis?160 x1284))
                                         #f))
                                     key1282)
                                 (let ((x1286 (gen-var162 (quote tmp))))
                                   (build-application82
                                     s1275
                                     (build-lambda91
                                       #f
                                       (list (quote tmp))
                                       (list x1286)
                                       #f
                                       (gen-syntax-case1199
                                         (build-lexical-reference84
                                           'value
                                           #f
                                           'tmp
                                           x1286)
                                         key1282
                                         m1283
                                         r1273
                                         mod1276))
                                     (list (chi151
                                             val1281
                                             r1273
                                             '(())
                                             mod1276))))
                                 (syntax-violation
                                   'syntax-case
                                   "invalid literals list"
                                   e1277)))
                             tmp1279)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        tmp1278)))
                  ($sc-dispatch
                    tmp1278
                    '(any any each-any . each-any))))
               e1277)))))
      (set! sc-expand
        (lambda (x1290 . rest1289)
          (if (if (pair? x1290)
                (equal? (car x1290) noexpand70)
                #f)
            (cadr x1290)
            (let ((m1291 (if (null? rest1289) (quote e) (car rest1289)))
                  (esew1292
                    (if (let ((t1293 (null? rest1289)))
                          (if t1293 t1293 (null? (cdr rest1289))))
                      '(eval)
                      (cadr rest1289))))
              (with-fluid*
                *mode*71
                m1291
                (lambda ()
                  (chi-top150
                    x1290
                    '()
                    '((top))
                    m1291
                    esew1292
                    (cons 'hygiene
                          (module-name (current-module))))))))))
      (set! identifier?
        (lambda (x1294) (nonsymbol-id?114 x1294)))
      (set! datum->syntax
        (lambda (id1295 datum1296)
          (make-syntax-object98
            datum1296
            (syntax-object-wrap101 id1295)
            #f)))
      (set! syntax->datum
        (lambda (x1297) (strip161 x1297 (quote (())))))
      (set! generate-temporaries
        (lambda (ls1298)
          (begin
            (let ((x1299 ls1298))
              (if (not (list? x1299))
                (syntax-violation
                  'generate-temporaries
                  "invalid argument"
                  x1299)))
            (map (lambda (x1300)
                   (wrap143 (gensym) (quote ((top))) #f))
                 ls1298))))
      (set! free-identifier=?
        (lambda (x1301 y1302)
          (begin
            (let ((x1303 x1301))
              (if (not (nonsymbol-id?114 x1303))
                (syntax-violation
                  'free-identifier=?
                  "invalid argument"
                  x1303)))
            (let ((x1304 y1302))
              (if (not (nonsymbol-id?114 x1304))
                (syntax-violation
                  'free-identifier=?
                  "invalid argument"
                  x1304)))
            (free-id=?138 x1301 y1302))))
      (set! bound-identifier=?
        (lambda (x1305 y1306)
          (begin
            (let ((x1307 x1305))
              (if (not (nonsymbol-id?114 x1307))
                (syntax-violation
                  'bound-identifier=?
                  "invalid argument"
                  x1307)))
            (let ((x1308 y1306))
              (if (not (nonsymbol-id?114 x1308))
                (syntax-violation
                  'bound-identifier=?
                  "invalid argument"
                  x1308)))
            (bound-id=?139 x1305 y1306))))
      (set! syntax-violation
        (lambda (who1312 message1311 form1310 . subform1309)
          (begin
            (let ((x1313 who1312))
              (if (not ((lambda (x1314)
                          (let ((t1315 (not x1314)))
                            (if t1315
                              t1315
                              (let ((t1316 (string? x1314)))
                                (if t1316 t1316 (symbol? x1314))))))
                        x1313))
                (syntax-violation
                  'syntax-violation
                  "invalid argument"
                  x1313)))
            (let ((x1317 message1311))
              (if (not (string? x1317))
                (syntax-violation
                  'syntax-violation
                  "invalid argument"
                  x1317)))
            (scm-error
              'syntax-error
              'sc-expand
              (string-append
                (if who1312 "~a: " "")
                "~a "
                (if (null? subform1309)
                  "in ~a"
                  "in subform `~s' of `~s'"))
              (let ((tail1318
                      (cons message1311
                            (map (lambda (x1319) (strip161 x1319 (quote (()))))
                                 (append subform1309 (list form1310))))))
                (if who1312 (cons who1312 tail1318) tail1318))
              #f))))
      (letrec ((match1324
                 (lambda (e1325 p1326 w1327 r1328 mod1329)
                   (if (not r1328)
                     #f
                     (if (eq? p1326 (quote any))
                       (cons (wrap143 e1325 w1327 mod1329) r1328)
                       (if (syntax-object?99 e1325)
                         (match*1323
                           (syntax-object-expression100 e1325)
                           p1326
                           (join-wraps134
                             w1327
                             (syntax-object-wrap101 e1325))
                           r1328
                           (syntax-object-module102 e1325))
                         (match*1323 e1325 p1326 w1327 r1328 mod1329))))))
               (match*1323
                 (lambda (e1330 p1331 w1332 r1333 mod1334)
                   (if (null? p1331)
                     (if (null? e1330) r1333 #f)
                     (if (pair? p1331)
                       (if (pair? e1330)
                         (match1324
                           (car e1330)
                           (car p1331)
                           w1332
                           (match1324
                             (cdr e1330)
                             (cdr p1331)
                             w1332
                             r1333
                             mod1334)
                           mod1334)
                         #f)
                       (if (eq? p1331 (quote each-any))
                         (let ((l1335 (match-each-any1321
                                        e1330
                                        w1332
                                        mod1334)))
                           (if l1335 (cons l1335 r1333) #f))
                         (let ((atom-key1336 (vector-ref p1331 0)))
                           (if (memv atom-key1336 (quote (each)))
                             (if (null? e1330)
                               (match-empty1322 (vector-ref p1331 1) r1333)
                               (let ((l1337 (match-each1320
                                              e1330
                                              (vector-ref p1331 1)
                                              w1332
                                              mod1334)))
                                 (if l1337
                                   (letrec ((collect1338
                                              (lambda (l1339)
                                                (if (null? (car l1339))
                                                  r1333
                                                  (cons (map car l1339)
                                                        (collect1338
                                                          (map cdr l1339)))))))
                                     (collect1338 l1337))
                                   #f)))
                             (if (memv atom-key1336 (quote (free-id)))
                               (if (id?115 e1330)
                                 (if (free-id=?138
                                       (wrap143 e1330 w1332 mod1334)
                                       (vector-ref p1331 1))
                                   r1333
                                   #f)
                                 #f)
                               (if (memv atom-key1336 (quote (atom)))
                                 (if (equal?
                                       (vector-ref p1331 1)
                                       (strip161 e1330 w1332))
                                   r1333
                                   #f)
                                 (if (memv atom-key1336 (quote (vector)))
                                   (if (vector? e1330)
                                     (match1324
                                       (vector->list e1330)
                                       (vector-ref p1331 1)
                                       w1332
                                       r1333
                                       mod1334)
                                     #f)))))))))))
               (match-empty1322
                 (lambda (p1340 r1341)
                   (if (null? p1340)
                     r1341
                     (if (eq? p1340 (quote any))
                       (cons (quote ()) r1341)
                       (if (pair? p1340)
                         (match-empty1322
                           (car p1340)
                           (match-empty1322 (cdr p1340) r1341))
                         (if (eq? p1340 (quote each-any))
                           (cons (quote ()) r1341)
                           (let ((atom-key1342 (vector-ref p1340 0)))
                             (if (memv atom-key1342 (quote (each)))
                               (match-empty1322 (vector-ref p1340 1) r1341)
                               (if (memv atom-key1342 (quote (free-id atom)))
                                 r1341
                                 (if (memv atom-key1342 (quote (vector)))
                                   (match-empty1322
                                     (vector-ref p1340 1)
                                     r1341)))))))))))
               (match-each-any1321
                 (lambda (e1343 w1344 mod1345)
                   (if (pair? e1343)
                     (let ((l1346 (match-each-any1321
                                    (cdr e1343)
                                    w1344
                                    mod1345)))
                       (if l1346
                         (cons (wrap143 (car e1343) w1344 mod1345) l1346)
                         #f))
                     (if (null? e1343)
                       '()
                       (if (syntax-object?99 e1343)
                         (match-each-any1321
                           (syntax-object-expression100 e1343)
                           (join-wraps134
                             w1344
                             (syntax-object-wrap101 e1343))
                           mod1345)
                         #f)))))
               (match-each1320
                 (lambda (e1347 p1348 w1349 mod1350)
                   (if (pair? e1347)
                     (let ((first1351
                             (match1324
                               (car e1347)
                               p1348
                               w1349
                               '()
                               mod1350)))
                       (if first1351
                         (let ((rest1352
                                 (match-each1320
                                   (cdr e1347)
                                   p1348
                                   w1349
                                   mod1350)))
                           (if rest1352 (cons first1351 rest1352) #f))
                         #f))
                     (if (null? e1347)
                       '()
                       (if (syntax-object?99 e1347)
                         (match-each1320
                           (syntax-object-expression100 e1347)
                           p1348
                           (join-wraps134
                             w1349
                             (syntax-object-wrap101 e1347))
                           (syntax-object-module102 e1347))
                         #f))))))
        (set! $sc-dispatch
          (lambda (e1353 p1354)
            (if (eq? p1354 (quote any))
              (list e1353)
              (if (syntax-object?99 e1353)
                (match*1323
                  (syntax-object-expression100 e1353)
                  p1354
                  (syntax-object-wrap101 e1353)
                  '()
                  (syntax-object-module102 e1353))
                (match*1323
                  e1353
                  p1354
                  '(())
                  '()
                  #f)))))))))

(define with-syntax
  (make-syncase-macro
    'macro
    (lambda (x1355)
      ((lambda (tmp1356)
         ((lambda (tmp1357)
            (if tmp1357
              (apply (lambda (_1358 e11359 e21360)
                       (cons '#(syntax-object
                                begin
                                ((top)
                                 #(ribcage
                                   #(_ e1 e2)
                                   #((top) (top) (top))
                                   #("i" "i" "i"))
                                 #(ribcage () () ())
                                 #(ribcage #(x) #((top)) #("i")))
                                (hygiene guile))
                             (cons e11359 e21360)))
                     tmp1357)
              ((lambda (tmp1362)
                 (if tmp1362
                   (apply (lambda (_1363 out1364 in1365 e11366 e21367)
                            (list '#(syntax-object
                                     syntax-case
                                     ((top)
                                      #(ribcage
                                        #(_ out in e1 e2)
                                        #((top) (top) (top) (top) (top))
                                        #("i" "i" "i" "i" "i"))
                                      #(ribcage () () ())
                                      #(ribcage #(x) #((top)) #("i")))
                                     (hygiene guile))
                                  in1365
                                  '()
                                  (list out1364
                                        (cons '#(syntax-object
                                                 begin
                                                 ((top)
                                                  #(ribcage
                                                    #(_ out in e1 e2)
                                                    #((top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top))
                                                    #("i" "i" "i" "i" "i"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(x)
                                                    #((top))
                                                    #("i")))
                                                 (hygiene guile))
                                              (cons e11366 e21367)))))
                          tmp1362)
                   ((lambda (tmp1369)
                      (if tmp1369
                        (apply (lambda (_1370 out1371 in1372 e11373 e21374)
                                 (list '#(syntax-object
                                          syntax-case
                                          ((top)
                                           #(ribcage
                                             #(_ out in e1 e2)
                                             #((top) (top) (top) (top) (top))
                                             #("i" "i" "i" "i" "i"))
                                           #(ribcage () () ())
                                           #(ribcage #(x) #((top)) #("i")))
                                          (hygiene guile))
                                       (cons '#(syntax-object
                                                list
                                                ((top)
                                                 #(ribcage
                                                   #(_ out in e1 e2)
                                                   #((top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top))
                                                   #("i" "i" "i" "i" "i"))
                                                 #(ribcage () () ())
                                                 #(ribcage
                                                   #(x)
                                                   #((top))
                                                   #("i")))
                                                (hygiene guile))
                                             in1372)
                                       '()
                                       (list out1371
                                             (cons '#(syntax-object
                                                      begin
                                                      ((top)
                                                       #(ribcage
                                                         #(_ out in e1 e2)
                                                         #((top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top))
                                                         #("i"
                                                           "i"
                                                           "i"
                                                           "i"
                                                           "i"))
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(x)
                                                         #((top))
                                                         #("i")))
                                                      (hygiene guile))
                                                   (cons e11373 e21374)))))
                               tmp1369)
                        (syntax-violation
                          #f
                          "source expression failed to match any pattern"
                          tmp1356)))
                    ($sc-dispatch
                      tmp1356
                      '(any #(each (any any)) any . each-any)))))
               ($sc-dispatch
                 tmp1356
                 '(any ((any any)) any . each-any)))))
          ($sc-dispatch
            tmp1356
            '(any () any . each-any))))
       x1355))))

(define syntax-rules
  (make-syncase-macro
    'macro
    (lambda (x1378)
      ((lambda (tmp1379)
         ((lambda (tmp1380)
            (if tmp1380
              (apply (lambda (_1381
                              k1382
                              keyword1383
                              pattern1384
                              template1385)
                       (list '#(syntax-object
                                lambda
                                ((top)
                                 #(ribcage
                                   #(_ k keyword pattern template)
                                   #((top) (top) (top) (top) (top))
                                   #("i" "i" "i" "i" "i"))
                                 #(ribcage () () ())
                                 #(ribcage #(x) #((top)) #("i")))
                                (hygiene guile))
                             '(#(syntax-object
                                 x
                                 ((top)
                                  #(ribcage
                                    #(_ k keyword pattern template)
                                    #((top) (top) (top) (top) (top))
                                    #("i" "i" "i" "i" "i"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i")))
                                 (hygiene guile)))
                             (cons '#(syntax-object
                                      syntax-case
                                      ((top)
                                       #(ribcage
                                         #(_ k keyword pattern template)
                                         #((top) (top) (top) (top) (top))
                                         #("i" "i" "i" "i" "i"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i")))
                                      (hygiene guile))
                                   (cons '#(syntax-object
                                            x
                                            ((top)
                                             #(ribcage
                                               #(_ k keyword pattern template)
                                               #((top) (top) (top) (top) (top))
                                               #("i" "i" "i" "i" "i"))
                                             #(ribcage () () ())
                                             #(ribcage #(x) #((top)) #("i")))
                                            (hygiene guile))
                                         (cons k1382
                                               (map (lambda (tmp1388 tmp1387)
                                                      (list (cons '#(syntax-object
                                                                     dummy
                                                                     ((top)
                                                                      #(ribcage
                                                                        #(_
                                                                          k
                                                                          keyword
                                                                          pattern
                                                                          template)
                                                                        #((top)
                                                                          (top)
                                                                          (top)
                                                                          (top)
                                                                          (top))
                                                                        #("i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"))
                                                                      #(ribcage
                                                                        ()
                                                                        ()
                                                                        ())
                                                                      #(ribcage
                                                                        #(x)
                                                                        #((top))
                                                                        #("i")))
                                                                     (hygiene
                                                                       guile))
                                                                  tmp1387)
                                                            (list '#(syntax-object
                                                                     syntax
                                                                     ((top)
                                                                      #(ribcage
                                                                        #(_
                                                                          k
                                                                          keyword
                                                                          pattern
                                                                          template)
                                                                        #((top)
                                                                          (top)
                                                                          (top)
                                                                          (top)
                                                                          (top))
                                                                        #("i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"))
                                                                      #(ribcage
                                                                        ()
                                                                        ()
                                                                        ())
                                                                      #(ribcage
                                                                        #(x)
                                                                        #((top))
                                                                        #("i")))
                                                                     (hygiene
                                                                       guile))
                                                                  tmp1388)))
                                                    template1385
                                                    pattern1384))))))
                     tmp1380)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                tmp1379)))
          ($sc-dispatch
            tmp1379
            '(any each-any . #(each ((any . any) any))))))
       x1378))))

(define let*
  (make-extended-syncase-macro
    (module-ref (current-module) (quote let*))
    'macro
    (lambda (x1389)
      ((lambda (tmp1390)
         ((lambda (tmp1391)
            (if (if tmp1391
                  (apply (lambda (let*1392 x1393 v1394 e11395 e21396)
                           (and-map identifier? x1393))
                         tmp1391)
                  #f)
              (apply (lambda (let*1398 x1399 v1400 e11401 e21402)
                       (letrec ((f1403 (lambda (bindings1404)
                                         (if (null? bindings1404)
                                           (cons '#(syntax-object
                                                    let
                                                    ((top)
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(f bindings)
                                                       #((top) (top))
                                                       #("i" "i"))
                                                     #(ribcage
                                                       #(let* x v e1 e2)
                                                       #((top)
                                                         (top)
                                                         (top)
                                                         (top)
                                                         (top))
                                                       #("i" "i" "i" "i" "i"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(x)
                                                       #((top))
                                                       #("i")))
                                                    (hygiene guile))
                                                 (cons '()
                                                       (cons e11401 e21402)))
                                           ((lambda (tmp1408)
                                              ((lambda (tmp1409)
                                                 (if tmp1409
                                                   (apply (lambda (body1410
                                                                   binding1411)
                                                            (list '#(syntax-object
                                                                     let
                                                                     ((top)
                                                                      #(ribcage
                                                                        #(body
                                                                          binding)
                                                                        #((top)
                                                                          (top))
                                                                        #("i"
                                                                          "i"))
                                                                      #(ribcage
                                                                        ()
                                                                        ()
                                                                        ())
                                                                      #(ribcage
                                                                        #(f
                                                                          bindings)
                                                                        #((top)
                                                                          (top))
                                                                        #("i"
                                                                          "i"))
                                                                      #(ribcage
                                                                        #(let*
                                                                          x
                                                                          v
                                                                          e1
                                                                          e2)
                                                                        #((top)
                                                                          (top)
                                                                          (top)
                                                                          (top)
                                                                          (top))
                                                                        #("i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"))
                                                                      #(ribcage
                                                                        ()
                                                                        ()
                                                                        ())
                                                                      #(ribcage
                                                                        #(x)
                                                                        #((top))
                                                                        #("i")))
                                                                     (hygiene
                                                                       guile))
                                                                  (list binding1411)
                                                                  body1410))
                                                          tmp1409)
                                                   (syntax-violation
                                                     #f
                                                     "source expression failed to match any pattern"
                                                     tmp1408)))
                                               ($sc-dispatch
                                                 tmp1408
                                                 '(any any))))
                                            (list (f1403 (cdr bindings1404))
                                                  (car bindings1404)))))))
                         (f1403 (map list x1399 v1400))))
                     tmp1391)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                tmp1390)))
          ($sc-dispatch
            tmp1390
            '(any #(each (any any)) any . each-any))))
       x1389))))

(define do
  (make-extended-syncase-macro
    (module-ref (current-module) (quote do))
    'macro
    (lambda (orig-x1412)
      ((lambda (tmp1413)
         ((lambda (tmp1414)
            (if tmp1414
              (apply (lambda (_1415
                              var1416
                              init1417
                              step1418
                              e01419
                              e11420
                              c1421)
                       ((lambda (tmp1422)
                          ((lambda (tmp1423)
                             (if tmp1423
                               (apply (lambda (step1424)
                                        ((lambda (tmp1425)
                                           ((lambda (tmp1426)
                                              (if tmp1426
                                                (apply (lambda ()
                                                         (list '#(syntax-object
                                                                  let
                                                                  ((top)
                                                                   #(ribcage
                                                                     #(step)
                                                                     #((top))
                                                                     #("i"))
                                                                   #(ribcage
                                                                     #(_
                                                                       var
                                                                       init
                                                                       step
                                                                       e0
                                                                       e1
                                                                       c)
                                                                     #((top)
                                                                       (top)
                                                                       (top)
                                                                       (top)
                                                                       (top)
                                                                       (top)
                                                                       (top))
                                                                     #("i"
                                                                       "i"
                                                                       "i"
                                                                       "i"
                                                                       "i"
                                                                       "i"
                                                                       "i"))
                                                                   #(ribcage
                                                                     ()
                                                                     ()
                                                                     ())
                                                                   #(ribcage
                                                                     #(orig-x)
                                                                     #((top))
                                                                     #("i")))
                                                                  (hygiene
                                                                    guile))
                                                               '#(syntax-object
                                                                  doloop
                                                                  ((top)
                                                                   #(ribcage
                                                                     #(step)
                                                                     #((top))
                                                                     #("i"))
                                                                   #(ribcage
                                                                     #(_
                                                                       var
                                                                       init
                                                                       step
                                                                       e0
                                                                       e1
                                                                       c)
                                                                     #((top)
                                                                       (top)
                                                                       (top)
                                                                       (top)
                                                                       (top)
                                                                       (top)
                                                                       (top))
                                                                     #("i"
                                                                       "i"
                                                                       "i"
                                                                       "i"
                                                                       "i"
                                                                       "i"
                                                                       "i"))
                                                                   #(ribcage
                                                                     ()
                                                                     ()
                                                                     ())
                                                                   #(ribcage
                                                                     #(orig-x)
                                                                     #((top))
                                                                     #("i")))
                                                                  (hygiene
                                                                    guile))
                                                               (map list
                                                                    var1416
                                                                    init1417)
                                                               (list '#(syntax-object
                                                                        if
                                                                        ((top)
                                                                         #(ribcage
                                                                           #(step)
                                                                           #((top))
                                                                           #("i"))
                                                                         #(ribcage
                                                                           #(_
                                                                             var
                                                                             init
                                                                             step
                                                                             e0
                                                                             e1
                                                                             c)
                                                                           #((top)
                                                                             (top)
                                                                             (top)
                                                                             (top)
                                                                             (top)
                                                                             (top)
                                                                             (top))
                                                                           #("i"
                                                                             "i"
                                                                             "i"
                                                                             "i"
                                                                             "i"
                                                                             "i"
                                                                             "i"))
                                                                         #(ribcage
                                                                           ()
                                                                           ()
                                                                           ())
                                                                         #(ribcage
                                                                           #(orig-x)
                                                                           #((top))
                                                                           #("i")))
                                                                        (hygiene
                                                                          guile))
                                                                     (list '#(syntax-object
                                                                              not
                                                                              ((top)
                                                                               #(ribcage
                                                                                 #(step)
                                                                                 #((top))
                                                                                 #("i"))
                                                                               #(ribcage
                                                                                 #(_
                                                                                   var
                                                                                   init
                                                                                   step
                                                                                   e0
                                                                                   e1
                                                                                   c)
                                                                                 #((top)
                                                                                   (top)
                                                                                   (top)
                                                                                   (top)
                                                                                   (top)
                                                                                   (top)
                                                                                   (top))
                                                                                 #("i"
                                                                                   "i"
                                                                                   "i"
                                                                                   "i"
                                                                                   "i"
                                                                                   "i"
                                                                                   "i"))
                                                                               #(ribcage
                                                                                 ()
                                                                                 ()
                                                                                 ())
                                                                               #(ribcage
                                                                                 #(orig-x)
                                                                                 #((top))
                                                                                 #("i")))
                                                                              (hygiene
                                                                                guile))
                                                                           e01419)
                                                                     (cons '#(syntax-object
                                                                              begin
                                                                              ((top)
                                                                               #(ribcage
                                                                                 #(step)
                                                                                 #((top))
                                                                                 #("i"))
                                                                               #(ribcage
                                                                                 #(_
                                                                                   var
                                                                                   init
                                                                                   step
                                                                                   e0
                                                                                   e1
                                                                                   c)
                                                                                 #((top)
                                                                                   (top)
                                                                                   (top)
                                                                                   (top)
                                                                                   (top)
                                                                                   (top)
                                                                                   (top))
                                                                                 #("i"
                                                                                   "i"
                                                                                   "i"
                                                                                   "i"
                                                                                   "i"
                                                                                   "i"
                                                                                   "i"))
                                                                               #(ribcage
                                                                                 ()
                                                                                 ()
                                                                                 ())
                                                                               #(ribcage
                                                                                 #(orig-x)
                                                                                 #((top))
                                                                                 #("i")))
                                                                              (hygiene
                                                                                guile))
                                                                           (append
                                                                             c1421
                                                                             (list (cons '#(syntax-object
                                                                                            doloop
                                                                                            ((top)
                                                                                             #(ribcage
                                                                                               #(step)
                                                                                               #((top))
                                                                                               #("i"))
                                                                                             #(ribcage
                                                                                               #(_
                                                                                                 var
                                                                                                 init
                                                                                                 step
                                                                                                 e0
                                                                                                 e1
                                                                                                 c)
                                                                                               #((top)
                                                                                                 (top)
                                                                                                 (top)
                                                                                                 (top)
                                                                                                 (top)
                                                                                                 (top)
                                                                                                 (top))
                                                                                               #("i"
                                                                                                 "i"
                                                                                                 "i"
                                                                                                 "i"
                                                                                                 "i"
                                                                                                 "i"
                                                                                                 "i"))
                                                                                             #(ribcage
                                                                                               ()
                                                                                               ()
                                                                                               ())
                                                                                             #(ribcage
                                                                                               #(orig-x)
                                                                                               #((top))
                                                                                               #("i")))
                                                                                            (hygiene
                                                                                              guile))
                                                                                         step1424)))))))
                                                       tmp1426)
                                                ((lambda (tmp1431)
                                                   (if tmp1431
                                                     (apply (lambda (e11432
                                                                     e21433)
                                                              (list '#(syntax-object
                                                                       let
                                                                       ((top)
                                                                        #(ribcage
                                                                          #(e1
                                                                            e2)
                                                                          #((top)
                                                                            (top))
                                                                          #("i"
                                                                            "i"))
                                                                        #(ribcage
                                                                          #(step)
                                                                          #((top))
                                                                          #("i"))
                                                                        #(ribcage
                                                                          #(_
                                                                            var
                                                                            init
                                                                            step
                                                                            e0
                                                                            e1
                                                                            c)
                                                                          #((top)
                                                                            (top)
                                                                            (top)
                                                                            (top)
                                                                            (top)
                                                                            (top)
                                                                            (top))
                                                                          #("i"
                                                                            "i"
                                                                            "i"
                                                                            "i"
                                                                            "i"
                                                                            "i"
                                                                            "i"))
                                                                        #(ribcage
                                                                          ()
                                                                          ()
                                                                          ())
                                                                        #(ribcage
                                                                          #(orig-x)
                                                                          #((top))
                                                                          #("i")))
                                                                       (hygiene
                                                                         guile))
                                                                    '#(syntax-object
                                                                       doloop
                                                                       ((top)
                                                                        #(ribcage
                                                                          #(e1
                                                                            e2)
                                                                          #((top)
                                                                            (top))
                                                                          #("i"
                                                                            "i"))
                                                                        #(ribcage
                                                                          #(step)
                                                                          #((top))
                                                                          #("i"))
                                                                        #(ribcage
                                                                          #(_
                                                                            var
                                                                            init
                                                                            step
                                                                            e0
                                                                            e1
                                                                            c)
                                                                          #((top)
                                                                            (top)
                                                                            (top)
                                                                            (top)
                                                                            (top)
                                                                            (top)
                                                                            (top))
                                                                          #("i"
                                                                            "i"
                                                                            "i"
                                                                            "i"
                                                                            "i"
                                                                            "i"
                                                                            "i"))
                                                                        #(ribcage
                                                                          ()
                                                                          ()
                                                                          ())
                                                                        #(ribcage
                                                                          #(orig-x)
                                                                          #((top))
                                                                          #("i")))
                                                                       (hygiene
                                                                         guile))
                                                                    (map list
                                                                         var1416
                                                                         init1417)
                                                                    (list '#(syntax-object
                                                                             if
                                                                             ((top)
                                                                              #(ribcage
                                                                                #(e1
                                                                                  e2)
                                                                                #((top)
                                                                                  (top))
                                                                                #("i"
                                                                                  "i"))
                                                                              #(ribcage
                                                                                #(step)
                                                                                #((top))
                                                                                #("i"))
                                                                              #(ribcage
                                                                                #(_
                                                                                  var
                                                                                  init
                                                                                  step
                                                                                  e0
                                                                                  e1
                                                                                  c)
                                                                                #((top)
                                                                                  (top)
                                                                                  (top)
                                                                                  (top)
                                                                                  (top)
                                                                                  (top)
                                                                                  (top))
                                                                                #("i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i"))
                                                                              #(ribcage
                                                                                ()
                                                                                ()
                                                                                ())
                                                                              #(ribcage
                                                                                #(orig-x)
                                                                                #((top))
                                                                                #("i")))
                                                                             (hygiene
                                                                               guile))
                                                                          e01419
                                                                          (cons '#(syntax-object
                                                                                   begin
                                                                                   ((top)
                                                                                    #(ribcage
                                                                                      #(e1
                                                                                        e2)
                                                                                      #((top)
                                                                                        (top))
                                                                                      #("i"
                                                                                        "i"))
                                                                                    #(ribcage
                                                                                      #(step)
                                                                                      #((top))
                                                                                      #("i"))
                                                                                    #(ribcage
                                                                                      #(_
                                                                                        var
                                                                                        init
                                                                                        step
                                                                                        e0
                                                                                        e1
                                                                                        c)
                                                                                      #((top)
                                                                                        (top)
                                                                                        (top)
                                                                                        (top)
                                                                                        (top)
                                                                                        (top)
                                                                                        (top))
                                                                                      #("i"
                                                                                        "i"
                                                                                        "i"
                                                                                        "i"
                                                                                        "i"
                                                                                        "i"
                                                                                        "i"))
                                                                                    #(ribcage
                                                                                      ()
                                                                                      ()
                                                                                      ())
                                                                                    #(ribcage
                                                                                      #(orig-x)
                                                                                      #((top))
                                                                                      #("i")))
                                                                                   (hygiene
                                                                                     guile))
                                                                                (cons e11432
                                                                                      e21433))
                                                                          (cons '#(syntax-object
                                                                                   begin
                                                                                   ((top)
                                                                                    #(ribcage
                                                                                      #(e1
                                                                                        e2)
                                                                                      #((top)
                                                                                        (top))
                                                                                      #("i"
                                                                                        "i"))
                                                                                    #(ribcage
                                                                                      #(step)
                                                                                      #((top))
                                                                                      #("i"))
                                                                                    #(ribcage
                                                                                      #(_
                                                                                        var
                                                                                        init
                                                                                        step
                                                                                        e0
                                                                                        e1
                                                                                        c)
                                                                                      #((top)
                                                                                        (top)
                                                                                        (top)
                                                                                        (top)
                                                                                        (top)
                                                                                        (top)
                                                                                        (top))
                                                                                      #("i"
                                                                                        "i"
                                                                                        "i"
                                                                                        "i"
                                                                                        "i"
                                                                                        "i"
                                                                                        "i"))
                                                                                    #(ribcage
                                                                                      ()
                                                                                      ()
                                                                                      ())
                                                                                    #(ribcage
                                                                                      #(orig-x)
                                                                                      #((top))
                                                                                      #("i")))
                                                                                   (hygiene
                                                                                     guile))
                                                                                (append
                                                                                  c1421
                                                                                  (list (cons '#(syntax-object
                                                                                                 doloop
                                                                                                 ((top)
                                                                                                  #(ribcage
                                                                                                    #(e1
                                                                                                      e2)
                                                                                                    #((top)
                                                                                                      (top))
                                                                                                    #("i"
                                                                                                      "i"))
                                                                                                  #(ribcage
                                                                                                    #(step)
                                                                                                    #((top))
                                                                                                    #("i"))
                                                                                                  #(ribcage
                                                                                                    #(_
                                                                                                      var
                                                                                                      init
                                                                                                      step
                                                                                                      e0
                                                                                                      e1
                                                                                                      c)
                                                                                                    #((top)
                                                                                                      (top)
                                                                                                      (top)
                                                                                                      (top)
                                                                                                      (top)
                                                                                                      (top)
                                                                                                      (top))
                                                                                                    #("i"
                                                                                                      "i"
                                                                                                      "i"
                                                                                                      "i"
                                                                                                      "i"
                                                                                                      "i"
                                                                                                      "i"))
                                                                                                  #(ribcage
                                                                                                    ()
                                                                                                    ()
                                                                                                    ())
                                                                                                  #(ribcage
                                                                                                    #(orig-x)
                                                                                                    #((top))
                                                                                                    #("i")))
                                                                                                 (hygiene
                                                                                                   guile))
                                                                                              step1424)))))))
                                                            tmp1431)
                                                     (syntax-violation
                                                       #f
                                                       "source expression failed to match any pattern"
                                                       tmp1425)))
                                                 ($sc-dispatch
                                                   tmp1425
                                                   '(any . each-any)))))
                                            ($sc-dispatch tmp1425 (quote ()))))
                                         e11420))
                                      tmp1423)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 tmp1422)))
                           ($sc-dispatch tmp1422 (quote each-any))))
                        (map (lambda (v1440 s1441)
                               ((lambda (tmp1442)
                                  ((lambda (tmp1443)
                                     (if tmp1443
                                       (apply (lambda () v1440) tmp1443)
                                       ((lambda (tmp1444)
                                          (if tmp1444
                                            (apply (lambda (e1445) e1445)
                                                   tmp1444)
                                            ((lambda (_1446)
                                               (syntax-violation
                                                 'do
                                                 "bad step expression"
                                                 orig-x1412
                                                 s1441))
                                             tmp1442)))
                                        ($sc-dispatch tmp1442 (quote (any))))))
                                   ($sc-dispatch tmp1442 (quote ()))))
                                s1441))
                             var1416
                             step1418)))
                     tmp1414)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                tmp1413)))
          ($sc-dispatch
            tmp1413
            '(any #(each (any any . any))
                  (any . each-any)
                  .
                  each-any))))
       orig-x1412))))

(define quasiquote
  (make-extended-syncase-macro
    (module-ref (current-module) (quote quasiquote))
    'macro
    (letrec ((quasicons1449
               (lambda (x1453 y1454)
                 ((lambda (tmp1455)
                    ((lambda (tmp1456)
                       (if tmp1456
                         (apply (lambda (x1457 y1458)
                                  ((lambda (tmp1459)
                                     ((lambda (tmp1460)
                                        (if tmp1460
                                          (apply (lambda (dy1461)
                                                   ((lambda (tmp1462)
                                                      ((lambda (tmp1463)
                                                         (if tmp1463
                                                           (apply (lambda (dx1464)
                                                                    (list '#(syntax-object
                                                                             quote
                                                                             ((top)
                                                                              #(ribcage
                                                                                #(dx)
                                                                                #((top))
                                                                                #("i"))
                                                                              #(ribcage
                                                                                #(dy)
                                                                                #((top))
                                                                                #("i"))
                                                                              #(ribcage
                                                                                #(x
                                                                                  y)
                                                                                #((top)
                                                                                  (top))
                                                                                #("i"
                                                                                  "i"))
                                                                              #(ribcage
                                                                                ()
                                                                                ()
                                                                                ())
                                                                              #(ribcage
                                                                                ()
                                                                                ()
                                                                                ())
                                                                              #(ribcage
                                                                                #(x
                                                                                  y)
                                                                                #((top)
                                                                                  (top))
                                                                                #("i"
                                                                                  "i"))
                                                                              #(ribcage
                                                                                #(quasicons
                                                                                  quasiappend
                                                                                  quasivector
                                                                                  quasi)
                                                                                #((top)
                                                                                  (top)
                                                                                  (top)
                                                                                  (top))
                                                                                #("i"
                                                                                  "i"
                                                                                  "i"
                                                                                  "i")))
                                                                             (hygiene
                                                                               guile))
                                                                          (cons dx1464
                                                                                dy1461)))
                                                                  tmp1463)
                                                           ((lambda (_1465)
                                                              (if (null? dy1461)
                                                                (list '#(syntax-object
                                                                         list
                                                                         ((top)
                                                                          #(ribcage
                                                                            #(_)
                                                                            #((top))
                                                                            #("i"))
                                                                          #(ribcage
                                                                            #(dy)
                                                                            #((top))
                                                                            #("i"))
                                                                          #(ribcage
                                                                            #(x
                                                                              y)
                                                                            #((top)
                                                                              (top))
                                                                            #("i"
                                                                              "i"))
                                                                          #(ribcage
                                                                            ()
                                                                            ()
                                                                            ())
                                                                          #(ribcage
                                                                            ()
                                                                            ()
                                                                            ())
                                                                          #(ribcage
                                                                            #(x
                                                                              y)
                                                                            #((top)
                                                                              (top))
                                                                            #("i"
                                                                              "i"))
                                                                          #(ribcage
                                                                            #(quasicons
                                                                              quasiappend
                                                                              quasivector
                                                                              quasi)
                                                                            #((top)
                                                                              (top)
                                                                              (top)
                                                                              (top))
                                                                            #("i"
                                                                              "i"
                                                                              "i"
                                                                              "i")))
                                                                         (hygiene
                                                                           guile))
                                                                      x1457)
                                                                (list '#(syntax-object
                                                                         cons
                                                                         ((top)
                                                                          #(ribcage
                                                                            #(_)
                                                                            #((top))
                                                                            #("i"))
                                                                          #(ribcage
                                                                            #(dy)
                                                                            #((top))
                                                                            #("i"))
                                                                          #(ribcage
                                                                            #(x
                                                                              y)
                                                                            #((top)
                                                                              (top))
                                                                            #("i"
                                                                              "i"))
                                                                          #(ribcage
                                                                            ()
                                                                            ()
                                                                            ())
                                                                          #(ribcage
                                                                            ()
                                                                            ()
                                                                            ())
                                                                          #(ribcage
                                                                            #(x
                                                                              y)
                                                                            #((top)
                                                                              (top))
                                                                            #("i"
                                                                              "i"))
                                                                          #(ribcage
                                                                            #(quasicons
                                                                              quasiappend
                                                                              quasivector
                                                                              quasi)
                                                                            #((top)
                                                                              (top)
                                                                              (top)
                                                                              (top))
                                                                            #("i"
                                                                              "i"
                                                                              "i"
                                                                              "i")))
                                                                         (hygiene
                                                                           guile))
                                                                      x1457
                                                                      y1458)))
                                                            tmp1462)))
                                                       ($sc-dispatch
                                                         tmp1462
                                                         '(#(free-id
                                                             #(syntax-object
                                                               quote
                                                               ((top)
                                                                #(ribcage
                                                                  #(dy)
                                                                  #((top))
                                                                  #("i"))
                                                                #(ribcage
                                                                  #(x y)
                                                                  #((top)
                                                                    (top))
                                                                  #("i" "i"))
                                                                #(ribcage
                                                                  ()
                                                                  ()
                                                                  ())
                                                                #(ribcage
                                                                  ()
                                                                  ()
                                                                  ())
                                                                #(ribcage
                                                                  #(x y)
                                                                  #((top)
                                                                    (top))
                                                                  #("i" "i"))
                                                                #(ribcage
                                                                  #(quasicons
                                                                    quasiappend
                                                                    quasivector
                                                                    quasi)
                                                                  #((top)
                                                                    (top)
                                                                    (top)
                                                                    (top))
                                                                  #("i"
                                                                    "i"
                                                                    "i"
                                                                    "i")))
                                                               (hygiene
                                                                 guile)))
                                                           any))))
                                                    x1457))
                                                 tmp1460)
                                          ((lambda (tmp1466)
                                             (if tmp1466
                                               (apply (lambda (stuff1467)
                                                        (cons '#(syntax-object
                                                                 list
                                                                 ((top)
                                                                  #(ribcage
                                                                    #(stuff)
                                                                    #((top))
                                                                    #("i"))
                                                                  #(ribcage
                                                                    #(x y)
                                                                    #((top)
                                                                      (top))
                                                                    #("i" "i"))
                                                                  #(ribcage
                                                                    ()
                                                                    ()
                                                                    ())
                                                                  #(ribcage
                                                                    ()
                                                                    ()
                                                                    ())
                                                                  #(ribcage
                                                                    #(x y)
                                                                    #((top)
                                                                      (top))
                                                                    #("i" "i"))
                                                                  #(ribcage
                                                                    #(quasicons
                                                                      quasiappend
                                                                      quasivector
                                                                      quasi)
                                                                    #((top)
                                                                      (top)
                                                                      (top)
                                                                      (top))
                                                                    #("i"
                                                                      "i"
                                                                      "i"
                                                                      "i")))
                                                                 (hygiene
                                                                   guile))
                                                              (cons x1457
                                                                    stuff1467)))
                                                      tmp1466)
                                               ((lambda (else1468)
                                                  (list '#(syntax-object
                                                           cons
                                                           ((top)
                                                            #(ribcage
                                                              #(else)
                                                              #((top))
                                                              #("i"))
                                                            #(ribcage
                                                              #(x y)
                                                              #((top) (top))
                                                              #("i" "i"))
                                                            #(ribcage () () ())
                                                            #(ribcage () () ())
                                                            #(ribcage
                                                              #(x y)
                                                              #((top) (top))
                                                              #("i" "i"))
                                                            #(ribcage
                                                              #(quasicons
                                                                quasiappend
                                                                quasivector
                                                                quasi)
                                                              #((top)
                                                                (top)
                                                                (top)
                                                                (top))
                                                              #("i"
                                                                "i"
                                                                "i"
                                                                "i")))
                                                           (hygiene guile))
                                                        x1457
                                                        y1458))
                                                tmp1459)))
                                           ($sc-dispatch
                                             tmp1459
                                             '(#(free-id
                                                 #(syntax-object
                                                   list
                                                   ((top)
                                                    #(ribcage
                                                      #(x y)
                                                      #((top) (top))
                                                      #("i" "i"))
                                                    #(ribcage () () ())
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(x y)
                                                      #((top) (top))
                                                      #("i" "i"))
                                                    #(ribcage
                                                      #(quasicons
                                                        quasiappend
                                                        quasivector
                                                        quasi)
                                                      #((top)
                                                        (top)
                                                        (top)
                                                        (top))
                                                      #("i" "i" "i" "i")))
                                                   (hygiene guile)))
                                               .
                                               any)))))
                                      ($sc-dispatch
                                        tmp1459
                                        '(#(free-id
                                            #(syntax-object
                                              quote
                                              ((top)
                                               #(ribcage
                                                 #(x y)
                                                 #((top) (top))
                                                 #("i" "i"))
                                               #(ribcage () () ())
                                               #(ribcage () () ())
                                               #(ribcage
                                                 #(x y)
                                                 #((top) (top))
                                                 #("i" "i"))
                                               #(ribcage
                                                 #(quasicons
                                                   quasiappend
                                                   quasivector
                                                   quasi)
                                                 #((top) (top) (top) (top))
                                                 #("i" "i" "i" "i")))
                                              (hygiene guile)))
                                          any))))
                                   y1458))
                                tmp1456)
                         (syntax-violation
                           #f
                           "source expression failed to match any pattern"
                           tmp1455)))
                     ($sc-dispatch tmp1455 (quote (any any)))))
                  (list x1453 y1454))))
             (quasiappend1450
               (lambda (x1469 y1470)
                 ((lambda (tmp1471)
                    ((lambda (tmp1472)
                       (if tmp1472
                         (apply (lambda (x1473 y1474)
                                  ((lambda (tmp1475)
                                     ((lambda (tmp1476)
                                        (if tmp1476
                                          (apply (lambda () x1473) tmp1476)
                                          ((lambda (_1477)
                                             (list '#(syntax-object
                                                      append
                                                      ((top)
                                                       #(ribcage
                                                         #(_)
                                                         #((top))
                                                         #("i"))
                                                       #(ribcage
                                                         #(x y)
                                                         #((top) (top))
                                                         #("i" "i"))
                                                       #(ribcage () () ())
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(x y)
                                                         #((top) (top))
                                                         #("i" "i"))
                                                       #(ribcage
                                                         #(quasicons
                                                           quasiappend
                                                           quasivector
                                                           quasi)
                                                         #((top)
                                                           (top)
                                                           (top)
                                                           (top))
                                                         #("i" "i" "i" "i")))
                                                      (hygiene guile))
                                                   x1473
                                                   y1474))
                                           tmp1475)))
                                      ($sc-dispatch
                                        tmp1475
                                        '(#(free-id
                                            #(syntax-object
                                              quote
                                              ((top)
                                               #(ribcage
                                                 #(x y)
                                                 #((top) (top))
                                                 #("i" "i"))
                                               #(ribcage () () ())
                                               #(ribcage () () ())
                                               #(ribcage
                                                 #(x y)
                                                 #((top) (top))
                                                 #("i" "i"))
                                               #(ribcage
                                                 #(quasicons
                                                   quasiappend
                                                   quasivector
                                                   quasi)
                                                 #((top) (top) (top) (top))
                                                 #("i" "i" "i" "i")))
                                              (hygiene guile)))
                                          ()))))
                                   y1474))
                                tmp1472)
                         (syntax-violation
                           #f
                           "source expression failed to match any pattern"
                           tmp1471)))
                     ($sc-dispatch tmp1471 (quote (any any)))))
                  (list x1469 y1470))))
             (quasivector1451
               (lambda (x1478)
                 ((lambda (tmp1479)
                    ((lambda (x1480)
                       ((lambda (tmp1481)
                          ((lambda (tmp1482)
                             (if tmp1482
                               (apply (lambda (x1483)
                                        (list '#(syntax-object
                                                 quote
                                                 ((top)
                                                  #(ribcage
                                                    #(x)
                                                    #((top))
                                                    #("i"))
                                                  #(ribcage
                                                    #(x)
                                                    #((top))
                                                    #("i"))
                                                  #(ribcage () () ())
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(x)
                                                    #((top))
                                                    #("i"))
                                                  #(ribcage
                                                    #(quasicons
                                                      quasiappend
                                                      quasivector
                                                      quasi)
                                                    #((top) (top) (top) (top))
                                                    #("i" "i" "i" "i")))
                                                 (hygiene guile))
                                              (list->vector x1483)))
                                      tmp1482)
                               ((lambda (tmp1485)
                                  (if tmp1485
                                    (apply (lambda (x1486)
                                             (cons '#(syntax-object
                                                      vector
                                                      ((top)
                                                       #(ribcage
                                                         #(x)
                                                         #((top))
                                                         #("i"))
                                                       #(ribcage
                                                         #(x)
                                                         #((top))
                                                         #("i"))
                                                       #(ribcage () () ())
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(x)
                                                         #((top))
                                                         #("i"))
                                                       #(ribcage
                                                         #(quasicons
                                                           quasiappend
                                                           quasivector
                                                           quasi)
                                                         #((top)
                                                           (top)
                                                           (top)
                                                           (top))
                                                         #("i" "i" "i" "i")))
                                                      (hygiene guile))
                                                   x1486))
                                           tmp1485)
                                    ((lambda (_1488)
                                       (list '#(syntax-object
                                                list->vector
                                                ((top)
                                                 #(ribcage
                                                   #(_)
                                                   #((top))
                                                   #("i"))
                                                 #(ribcage
                                                   #(x)
                                                   #((top))
                                                   #("i"))
                                                 #(ribcage () () ())
                                                 #(ribcage () () ())
                                                 #(ribcage
                                                   #(x)
                                                   #((top))
                                                   #("i"))
                                                 #(ribcage
                                                   #(quasicons
                                                     quasiappend
                                                     quasivector
                                                     quasi)
                                                   #((top) (top) (top) (top))
                                                   #("i" "i" "i" "i")))
                                                (hygiene guile))
                                             x1480))
                                     tmp1481)))
                                ($sc-dispatch
                                  tmp1481
                                  '(#(free-id
                                      #(syntax-object
                                        list
                                        ((top)
                                         #(ribcage #(x) #((top)) #("i"))
                                         #(ribcage () () ())
                                         #(ribcage () () ())
                                         #(ribcage #(x) #((top)) #("i"))
                                         #(ribcage
                                           #(quasicons
                                             quasiappend
                                             quasivector
                                             quasi)
                                           #((top) (top) (top) (top))
                                           #("i" "i" "i" "i")))
                                        (hygiene guile)))
                                    .
                                    each-any)))))
                           ($sc-dispatch
                             tmp1481
                             '(#(free-id
                                 #(syntax-object
                                   quote
                                   ((top)
                                    #(ribcage #(x) #((top)) #("i"))
                                    #(ribcage () () ())
                                    #(ribcage () () ())
                                    #(ribcage #(x) #((top)) #("i"))
                                    #(ribcage
                                      #(quasicons
                                        quasiappend
                                        quasivector
                                        quasi)
                                      #((top) (top) (top) (top))
                                      #("i" "i" "i" "i")))
                                   (hygiene guile)))
                               each-any))))
                        x1480))
                     tmp1479))
                  x1478)))
             (quasi1452
               (lambda (p1489 lev1490)
                 ((lambda (tmp1491)
                    ((lambda (tmp1492)
                       (if tmp1492
                         (apply (lambda (p1493)
                                  (if (= lev1490 0)
                                    p1493
                                    (quasicons1449
                                      '(#(syntax-object
                                          quote
                                          ((top)
                                           #(ribcage #(p) #((top)) #("i"))
                                           #(ribcage () () ())
                                           #(ribcage
                                             #(p lev)
                                             #((top) (top))
                                             #("i" "i"))
                                           #(ribcage
                                             #(quasicons
                                               quasiappend
                                               quasivector
                                               quasi)
                                             #((top) (top) (top) (top))
                                             #("i" "i" "i" "i")))
                                          (hygiene guile))
                                        #(syntax-object
                                          unquote
                                          ((top)
                                           #(ribcage #(p) #((top)) #("i"))
                                           #(ribcage () () ())
                                           #(ribcage
                                             #(p lev)
                                             #((top) (top))
                                             #("i" "i"))
                                           #(ribcage
                                             #(quasicons
                                               quasiappend
                                               quasivector
                                               quasi)
                                             #((top) (top) (top) (top))
                                             #("i" "i" "i" "i")))
                                          (hygiene guile)))
                                      (quasi1452 (list p1493) (- lev1490 1)))))
                                tmp1492)
                         ((lambda (tmp1494)
                            (if (if tmp1494
                                  (apply (lambda (args1495) (= lev1490 0))
                                         tmp1494)
                                  #f)
                              (apply (lambda (args1496)
                                       (syntax-violation
                                         'unquote
                                         "unquote takes exactly one argument"
                                         p1489
                                         (cons '#(syntax-object
                                                  unquote
                                                  ((top)
                                                   #(ribcage
                                                     #(args)
                                                     #((top))
                                                     #("i"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(p lev)
                                                     #((top) (top))
                                                     #("i" "i"))
                                                   #(ribcage
                                                     #(quasicons
                                                       quasiappend
                                                       quasivector
                                                       quasi)
                                                     #((top) (top) (top) (top))
                                                     #("i" "i" "i" "i")))
                                                  (hygiene guile))
                                               args1496)))
                                     tmp1494)
                              ((lambda (tmp1497)
                                 (if tmp1497
                                   (apply (lambda (p1498 q1499)
                                            (if (= lev1490 0)
                                              (quasiappend1450
                                                p1498
                                                (quasi1452 q1499 lev1490))
                                              (quasicons1449
                                                (quasicons1449
                                                  '(#(syntax-object
                                                      quote
                                                      ((top)
                                                       #(ribcage
                                                         #(p q)
                                                         #((top) (top))
                                                         #("i" "i"))
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(p lev)
                                                         #((top) (top))
                                                         #("i" "i"))
                                                       #(ribcage
                                                         #(quasicons
                                                           quasiappend
                                                           quasivector
                                                           quasi)
                                                         #((top)
                                                           (top)
                                                           (top)
                                                           (top))
                                                         #("i" "i" "i" "i")))
                                                      (hygiene guile))
                                                    #(syntax-object
                                                      unquote-splicing
                                                      ((top)
                                                       #(ribcage
                                                         #(p q)
                                                         #((top) (top))
                                                         #("i" "i"))
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(p lev)
                                                         #((top) (top))
                                                         #("i" "i"))
                                                       #(ribcage
                                                         #(quasicons
                                                           quasiappend
                                                           quasivector
                                                           quasi)
                                                         #((top)
                                                           (top)
                                                           (top)
                                                           (top))
                                                         #("i" "i" "i" "i")))
                                                      (hygiene guile)))
                                                  (quasi1452
                                                    (list p1498)
                                                    (- lev1490 1)))
                                                (quasi1452 q1499 lev1490))))
                                          tmp1497)
                                   ((lambda (tmp1500)
                                      (if (if tmp1500
                                            (apply (lambda (args1501 q1502)
                                                     (= lev1490 0))
                                                   tmp1500)
                                            #f)
                                        (apply (lambda (args1503 q1504)
                                                 (syntax-violation
                                                   'unquote-splicing
                                                   "unquote-splicing takes exactly one argument"
                                                   p1489
                                                   (cons '#(syntax-object
                                                            unquote-splicing
                                                            ((top)
                                                             #(ribcage
                                                               #(args q)
                                                               #((top) (top))
                                                               #("i" "i"))
                                                             #(ribcage
                                                               ()
                                                               ()
                                                               ())
                                                             #(ribcage
                                                               #(p lev)
                                                               #((top) (top))
                                                               #("i" "i"))
                                                             #(ribcage
                                                               #(quasicons
                                                                 quasiappend
                                                                 quasivector
                                                                 quasi)
                                                               #((top)
                                                                 (top)
                                                                 (top)
                                                                 (top))
                                                               #("i"
                                                                 "i"
                                                                 "i"
                                                                 "i")))
                                                            (hygiene guile))
                                                         args1503)))
                                               tmp1500)
                                        ((lambda (tmp1505)
                                           (if tmp1505
                                             (apply (lambda (p1506)
                                                      (quasicons1449
                                                        '(#(syntax-object
                                                            quote
                                                            ((top)
                                                             #(ribcage
                                                               #(p)
                                                               #((top))
                                                               #("i"))
                                                             #(ribcage
                                                               ()
                                                               ()
                                                               ())
                                                             #(ribcage
                                                               #(p lev)
                                                               #((top) (top))
                                                               #("i" "i"))
                                                             #(ribcage
                                                               #(quasicons
                                                                 quasiappend
                                                                 quasivector
                                                                 quasi)
                                                               #((top)
                                                                 (top)
                                                                 (top)
                                                                 (top))
                                                               #("i"
                                                                 "i"
                                                                 "i"
                                                                 "i")))
                                                            (hygiene guile))
                                                          #(syntax-object
                                                            quasiquote
                                                            ((top)
                                                             #(ribcage
                                                               #(p)
                                                               #((top))
                                                               #("i"))
                                                             #(ribcage
                                                               ()
                                                               ()
                                                               ())
                                                             #(ribcage
                                                               #(p lev)
                                                               #((top) (top))
                                                               #("i" "i"))
                                                             #(ribcage
                                                               #(quasicons
                                                                 quasiappend
                                                                 quasivector
                                                                 quasi)
                                                               #((top)
                                                                 (top)
                                                                 (top)
                                                                 (top))
                                                               #("i"
                                                                 "i"
                                                                 "i"
                                                                 "i")))
                                                            (hygiene guile)))
                                                        (quasi1452
                                                          (list p1506)
                                                          (+ lev1490 1))))
                                                    tmp1505)
                                             ((lambda (tmp1507)
                                                (if tmp1507
                                                  (apply (lambda (p1508 q1509)
                                                           (quasicons1449
                                                             (quasi1452
                                                               p1508
                                                               lev1490)
                                                             (quasi1452
                                                               q1509
                                                               lev1490)))
                                                         tmp1507)
                                                  ((lambda (tmp1510)
                                                     (if tmp1510
                                                       (apply (lambda (x1511)
                                                                (quasivector1451
                                                                  (quasi1452
                                                                    x1511
                                                                    lev1490)))
                                                              tmp1510)
                                                       ((lambda (p1513)
                                                          (list '#(syntax-object
                                                                   quote
                                                                   ((top)
                                                                    #(ribcage
                                                                      #(p)
                                                                      #((top))
                                                                      #("i"))
                                                                    #(ribcage
                                                                      ()
                                                                      ()
                                                                      ())
                                                                    #(ribcage
                                                                      #(p lev)
                                                                      #((top)
                                                                        (top))
                                                                      #("i"
                                                                        "i"))
                                                                    #(ribcage
                                                                      #(quasicons
                                                                        quasiappend
                                                                        quasivector
                                                                        quasi)
                                                                      #((top)
                                                                        (top)
                                                                        (top)
                                                                        (top))
                                                                      #("i"
                                                                        "i"
                                                                        "i"
                                                                        "i")))
                                                                   (hygiene
                                                                     guile))
                                                                p1513))
                                                        tmp1491)))
                                                   ($sc-dispatch
                                                     tmp1491
                                                     '#(vector each-any)))))
                                              ($sc-dispatch
                                                tmp1491
                                                '(any . any)))))
                                         ($sc-dispatch
                                           tmp1491
                                           '(#(free-id
                                               #(syntax-object
                                                 quasiquote
                                                 ((top)
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(p lev)
                                                    #((top) (top))
                                                    #("i" "i"))
                                                  #(ribcage
                                                    #(quasicons
                                                      quasiappend
                                                      quasivector
                                                      quasi)
                                                    #((top) (top) (top) (top))
                                                    #("i" "i" "i" "i")))
                                                 (hygiene guile)))
                                             any)))))
                                    ($sc-dispatch
                                      tmp1491
                                      '((#(free-id
                                           #(syntax-object
                                             unquote-splicing
                                             ((top)
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(p lev)
                                                #((top) (top))
                                                #("i" "i"))
                                              #(ribcage
                                                #(quasicons
                                                  quasiappend
                                                  quasivector
                                                  quasi)
                                                #((top) (top) (top) (top))
                                                #("i" "i" "i" "i")))
                                             (hygiene guile)))
                                         .
                                         any)
                                        .
                                        any)))))
                               ($sc-dispatch
                                 tmp1491
                                 '((#(free-id
                                      #(syntax-object
                                        unquote-splicing
                                        ((top)
                                         #(ribcage () () ())
                                         #(ribcage
                                           #(p lev)
                                           #((top) (top))
                                           #("i" "i"))
                                         #(ribcage
                                           #(quasicons
                                             quasiappend
                                             quasivector
                                             quasi)
                                           #((top) (top) (top) (top))
                                           #("i" "i" "i" "i")))
                                        (hygiene guile)))
                                    any)
                                   .
                                   any)))))
                          ($sc-dispatch
                            tmp1491
                            '(#(free-id
                                #(syntax-object
                                  unquote
                                  ((top)
                                   #(ribcage () () ())
                                   #(ribcage
                                     #(p lev)
                                     #((top) (top))
                                     #("i" "i"))
                                   #(ribcage
                                     #(quasicons quasiappend quasivector quasi)
                                     #((top) (top) (top) (top))
                                     #("i" "i" "i" "i")))
                                  (hygiene guile)))
                              .
                              any)))))
                     ($sc-dispatch
                       tmp1491
                       '(#(free-id
                           #(syntax-object
                             unquote
                             ((top)
                              #(ribcage () () ())
                              #(ribcage #(p lev) #((top) (top)) #("i" "i"))
                              #(ribcage
                                #(quasicons quasiappend quasivector quasi)
                                #((top) (top) (top) (top))
                                #("i" "i" "i" "i")))
                             (hygiene guile)))
                         any))))
                  p1489))))
      (lambda (x1514)
        ((lambda (tmp1515)
           ((lambda (tmp1516)
              (if tmp1516
                (apply (lambda (_1517 e1518) (quasi1452 e1518 0))
                       tmp1516)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  tmp1515)))
            ($sc-dispatch tmp1515 (quote (any any)))))
         x1514)))))

(define include
  (make-syncase-macro
    'macro
    (lambda (x1519)
      (letrec ((read-file1520
                 (lambda (fn1521 k1522)
                   (let ((p1523 (open-input-file fn1521)))
                     (letrec ((f1524 (lambda (x1525)
                                       (if (eof-object? x1525)
                                         (begin
                                           (close-input-port p1523)
                                           '())
                                         (cons (datum->syntax k1522 x1525)
                                               (f1524 (read p1523)))))))
                       (f1524 (read p1523)))))))
        ((lambda (tmp1526)
           ((lambda (tmp1527)
              (if tmp1527
                (apply (lambda (k1528 filename1529)
                         (let ((fn1530 (syntax->datum filename1529)))
                           ((lambda (tmp1531)
                              ((lambda (tmp1532)
                                 (if tmp1532
                                   (apply (lambda (exp1533)
                                            (cons '#(syntax-object
                                                     begin
                                                     ((top)
                                                      #(ribcage
                                                        #(exp)
                                                        #((top))
                                                        #("i"))
                                                      #(ribcage () () ())
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(fn)
                                                        #((top))
                                                        #("i"))
                                                      #(ribcage
                                                        #(k filename)
                                                        #((top) (top))
                                                        #("i" "i"))
                                                      #(ribcage
                                                        (read-file)
                                                        ((top))
                                                        ("i"))
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i")))
                                                     (hygiene guile))
                                                  exp1533))
                                          tmp1532)
                                   (syntax-violation
                                     #f
                                     "source expression failed to match any pattern"
                                     tmp1531)))
                               ($sc-dispatch tmp1531 (quote each-any))))
                            (read-file1520 fn1530 k1528))))
                       tmp1527)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  tmp1526)))
            ($sc-dispatch tmp1526 (quote (any any)))))
         x1519)))))

(define unquote
  (make-syncase-macro
    'macro
    (lambda (x1535)
      ((lambda (tmp1536)
         ((lambda (tmp1537)
            (if tmp1537
              (apply (lambda (_1538 e1539)
                       (syntax-violation
                         'unquote
                         "expression not valid outside of quasiquote"
                         x1535))
                     tmp1537)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                tmp1536)))
          ($sc-dispatch tmp1536 (quote (any any)))))
       x1535))))

(define unquote-splicing
  (make-syncase-macro
    'macro
    (lambda (x1540)
      ((lambda (tmp1541)
         ((lambda (tmp1542)
            (if tmp1542
              (apply (lambda (_1543 e1544)
                       (syntax-violation
                         'unquote-splicing
                         "expression not valid outside of quasiquote"
                         x1540))
                     tmp1542)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                tmp1541)))
          ($sc-dispatch tmp1541 (quote (any any)))))
       x1540))))

(define case
  (make-extended-syncase-macro
    (module-ref (current-module) (quote case))
    'macro
    (lambda (x1545)
      ((lambda (tmp1546)
         ((lambda (tmp1547)
            (if tmp1547
              (apply (lambda (_1548 e1549 m11550 m21551)
                       ((lambda (tmp1552)
                          ((lambda (body1553)
                             (list '#(syntax-object
                                      let
                                      ((top)
                                       #(ribcage #(body) #((top)) #("i"))
                                       #(ribcage
                                         #(_ e m1 m2)
                                         #((top) (top) (top) (top))
                                         #("i" "i" "i" "i"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i")))
                                      (hygiene guile))
                                   (list (list '#(syntax-object
                                                  t
                                                  ((top)
                                                   #(ribcage
                                                     #(body)
                                                     #((top))
                                                     #("i"))
                                                   #(ribcage
                                                     #(_ e m1 m2)
                                                     #((top) (top) (top) (top))
                                                     #("i" "i" "i" "i"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x)
                                                     #((top))
                                                     #("i")))
                                                  (hygiene guile))
                                               e1549))
                                   body1553))
                           tmp1552))
                        (letrec ((f1554 (lambda (clause1555 clauses1556)
                                          (if (null? clauses1556)
                                            ((lambda (tmp1558)
                                               ((lambda (tmp1559)
                                                  (if tmp1559
                                                    (apply (lambda (e11560
                                                                    e21561)
                                                             (cons '#(syntax-object
                                                                      begin
                                                                      ((top)
                                                                       #(ribcage
                                                                         #(e1
                                                                           e2)
                                                                         #((top)
                                                                           (top))
                                                                         #("i"
                                                                           "i"))
                                                                       #(ribcage
                                                                         ()
                                                                         ()
                                                                         ())
                                                                       #(ribcage
                                                                         #(f
                                                                           clause
                                                                           clauses)
                                                                         #((top)
                                                                           (top)
                                                                           (top))
                                                                         #("i"
                                                                           "i"
                                                                           "i"))
                                                                       #(ribcage
                                                                         #(_
                                                                           e
                                                                           m1
                                                                           m2)
                                                                         #((top)
                                                                           (top)
                                                                           (top)
                                                                           (top))
                                                                         #("i"
                                                                           "i"
                                                                           "i"
                                                                           "i"))
                                                                       #(ribcage
                                                                         ()
                                                                         ()
                                                                         ())
                                                                       #(ribcage
                                                                         #(x)
                                                                         #((top))
                                                                         #("i")))
                                                                      (hygiene
                                                                        guile))
                                                                   (cons e11560
                                                                         e21561)))
                                                           tmp1559)
                                                    ((lambda (tmp1563)
                                                       (if tmp1563
                                                         (apply (lambda (k1564
                                                                         e11565
                                                                         e21566)
                                                                  (list '#(syntax-object
                                                                           if
                                                                           ((top)
                                                                            #(ribcage
                                                                              #(k
                                                                                e1
                                                                                e2)
                                                                              #((top)
                                                                                (top)
                                                                                (top))
                                                                              #("i"
                                                                                "i"
                                                                                "i"))
                                                                            #(ribcage
                                                                              ()
                                                                              ()
                                                                              ())
                                                                            #(ribcage
                                                                              #(f
                                                                                clause
                                                                                clauses)
                                                                              #((top)
                                                                                (top)
                                                                                (top))
                                                                              #("i"
                                                                                "i"
                                                                                "i"))
                                                                            #(ribcage
                                                                              #(_
                                                                                e
                                                                                m1
                                                                                m2)
                                                                              #((top)
                                                                                (top)
                                                                                (top)
                                                                                (top))
                                                                              #("i"
                                                                                "i"
                                                                                "i"
                                                                                "i"))
                                                                            #(ribcage
                                                                              ()
                                                                              ()
                                                                              ())
                                                                            #(ribcage
                                                                              #(x)
                                                                              #((top))
                                                                              #("i")))
                                                                           (hygiene
                                                                             guile))
                                                                        (list '#(syntax-object
                                                                                 memv
                                                                                 ((top)
                                                                                  #(ribcage
                                                                                    #(k
                                                                                      e1
                                                                                      e2)
                                                                                    #((top)
                                                                                      (top)
                                                                                      (top))
                                                                                    #("i"
                                                                                      "i"
                                                                                      "i"))
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    #(f
                                                                                      clause
                                                                                      clauses)
                                                                                    #((top)
                                                                                      (top)
                                                                                      (top))
                                                                                    #("i"
                                                                                      "i"
                                                                                      "i"))
                                                                                  #(ribcage
                                                                                    #(_
                                                                                      e
                                                                                      m1
                                                                                      m2)
                                                                                    #((top)
                                                                                      (top)
                                                                                      (top)
                                                                                      (top))
                                                                                    #("i"
                                                                                      "i"
                                                                                      "i"
                                                                                      "i"))
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    #(x)
                                                                                    #((top))
                                                                                    #("i")))
                                                                                 (hygiene
                                                                                   guile))
                                                                              '#(syntax-object
                                                                                 t
                                                                                 ((top)
                                                                                  #(ribcage
                                                                                    #(k
                                                                                      e1
                                                                                      e2)
                                                                                    #((top)
                                                                                      (top)
                                                                                      (top))
                                                                                    #("i"
                                                                                      "i"
                                                                                      "i"))
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    #(f
                                                                                      clause
                                                                                      clauses)
                                                                                    #((top)
                                                                                      (top)
                                                                                      (top))
                                                                                    #("i"
                                                                                      "i"
                                                                                      "i"))
                                                                                  #(ribcage
                                                                                    #(_
                                                                                      e
                                                                                      m1
                                                                                      m2)
                                                                                    #((top)
                                                                                      (top)
                                                                                      (top)
                                                                                      (top))
                                                                                    #("i"
                                                                                      "i"
                                                                                      "i"
                                                                                      "i"))
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    #(x)
                                                                                    #((top))
                                                                                    #("i")))
                                                                                 (hygiene
                                                                                   guile))
                                                                              (list '#(syntax-object
                                                                                       quote
                                                                                       ((top)
                                                                                        #(ribcage
                                                                                          #(k
                                                                                            e1
                                                                                            e2)
                                                                                          #((top)
                                                                                            (top)
                                                                                            (top))
                                                                                          #("i"
                                                                                            "i"
                                                                                            "i"))
                                                                                        #(ribcage
                                                                                          ()
                                                                                          ()
                                                                                          ())
                                                                                        #(ribcage
                                                                                          #(f
                                                                                            clause
                                                                                            clauses)
                                                                                          #((top)
                                                                                            (top)
                                                                                            (top))
                                                                                          #("i"
                                                                                            "i"
                                                                                            "i"))
                                                                                        #(ribcage
                                                                                          #(_
                                                                                            e
                                                                                            m1
                                                                                            m2)
                                                                                          #((top)
                                                                                            (top)
                                                                                            (top)
                                                                                            (top))
                                                                                          #("i"
                                                                                            "i"
                                                                                            "i"
                                                                                            "i"))
                                                                                        #(ribcage
                                                                                          ()
                                                                                          ()
                                                                                          ())
                                                                                        #(ribcage
                                                                                          #(x)
                                                                                          #((top))
                                                                                          #("i")))
                                                                                       (hygiene
                                                                                         guile))
                                                                                    k1564))
                                                                        (cons '#(syntax-object
                                                                                 begin
                                                                                 ((top)
                                                                                  #(ribcage
                                                                                    #(k
                                                                                      e1
                                                                                      e2)
                                                                                    #((top)
                                                                                      (top)
                                                                                      (top))
                                                                                    #("i"
                                                                                      "i"
                                                                                      "i"))
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    #(f
                                                                                      clause
                                                                                      clauses)
                                                                                    #((top)
                                                                                      (top)
                                                                                      (top))
                                                                                    #("i"
                                                                                      "i"
                                                                                      "i"))
                                                                                  #(ribcage
                                                                                    #(_
                                                                                      e
                                                                                      m1
                                                                                      m2)
                                                                                    #((top)
                                                                                      (top)
                                                                                      (top)
                                                                                      (top))
                                                                                    #("i"
                                                                                      "i"
                                                                                      "i"
                                                                                      "i"))
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    #(x)
                                                                                    #((top))
                                                                                    #("i")))
                                                                                 (hygiene
                                                                                   guile))
                                                                              (cons e11565
                                                                                    e21566))))
                                                                tmp1563)
                                                         ((lambda (_1569)
                                                            (syntax-violation
                                                              'case
                                                              "bad clause"
                                                              x1545
                                                              clause1555))
                                                          tmp1558)))
                                                     ($sc-dispatch
                                                       tmp1558
                                                       '(each-any
                                                          any
                                                          .
                                                          each-any)))))
                                                ($sc-dispatch
                                                  tmp1558
                                                  '(#(free-id
                                                      #(syntax-object
                                                        else
                                                        ((top)
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(f clause clauses)
                                                           #((top) (top) (top))
                                                           #("i" "i" "i"))
                                                         #(ribcage
                                                           #(_ e m1 m2)
                                                           #((top)
                                                             (top)
                                                             (top)
                                                             (top))
                                                           #("i" "i" "i" "i"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(x)
                                                           #((top))
                                                           #("i")))
                                                        (hygiene guile)))
                                                    any
                                                    .
                                                    each-any))))
                                             clause1555)
                                            ((lambda (tmp1570)
                                               ((lambda (rest1571)
                                                  ((lambda (tmp1572)
                                                     ((lambda (tmp1573)
                                                        (if tmp1573
                                                          (apply (lambda (k1574
                                                                          e11575
                                                                          e21576)
                                                                   (list '#(syntax-object
                                                                            if
                                                                            ((top)
                                                                             #(ribcage
                                                                               #(k
                                                                                 e1
                                                                                 e2)
                                                                               #((top)
                                                                                 (top)
                                                                                 (top))
                                                                               #("i"
                                                                                 "i"
                                                                                 "i"))
                                                                             #(ribcage
                                                                               #(rest)
                                                                               #((top))
                                                                               #("i"))
                                                                             #(ribcage
                                                                               ()
                                                                               ()
                                                                               ())
                                                                             #(ribcage
                                                                               #(f
                                                                                 clause
                                                                                 clauses)
                                                                               #((top)
                                                                                 (top)
                                                                                 (top))
                                                                               #("i"
                                                                                 "i"
                                                                                 "i"))
                                                                             #(ribcage
                                                                               #(_
                                                                                 e
                                                                                 m1
                                                                                 m2)
                                                                               #((top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top))
                                                                               #("i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"))
                                                                             #(ribcage
                                                                               ()
                                                                               ()
                                                                               ())
                                                                             #(ribcage
                                                                               #(x)
                                                                               #((top))
                                                                               #("i")))
                                                                            (hygiene
                                                                              guile))
                                                                         (list '#(syntax-object
                                                                                  memv
                                                                                  ((top)
                                                                                   #(ribcage
                                                                                     #(k
                                                                                       e1
                                                                                       e2)
                                                                                     #((top)
                                                                                       (top)
                                                                                       (top))
                                                                                     #("i"
                                                                                       "i"
                                                                                       "i"))
                                                                                   #(ribcage
                                                                                     #(rest)
                                                                                     #((top))
                                                                                     #("i"))
                                                                                   #(ribcage
                                                                                     ()
                                                                                     ()
                                                                                     ())
                                                                                   #(ribcage
                                                                                     #(f
                                                                                       clause
                                                                                       clauses)
                                                                                     #((top)
                                                                                       (top)
                                                                                       (top))
                                                                                     #("i"
                                                                                       "i"
                                                                                       "i"))
                                                                                   #(ribcage
                                                                                     #(_
                                                                                       e
                                                                                       m1
                                                                                       m2)
                                                                                     #((top)
                                                                                       (top)
                                                                                       (top)
                                                                                       (top))
                                                                                     #("i"
                                                                                       "i"
                                                                                       "i"
                                                                                       "i"))
                                                                                   #(ribcage
                                                                                     ()
                                                                                     ()
                                                                                     ())
                                                                                   #(ribcage
                                                                                     #(x)
                                                                                     #((top))
                                                                                     #("i")))
                                                                                  (hygiene
                                                                                    guile))
                                                                               '#(syntax-object
                                                                                  t
                                                                                  ((top)
                                                                                   #(ribcage
                                                                                     #(k
                                                                                       e1
                                                                                       e2)
                                                                                     #((top)
                                                                                       (top)
                                                                                       (top))
                                                                                     #("i"
                                                                                       "i"
                                                                                       "i"))
                                                                                   #(ribcage
                                                                                     #(rest)
                                                                                     #((top))
                                                                                     #("i"))
                                                                                   #(ribcage
                                                                                     ()
                                                                                     ()
                                                                                     ())
                                                                                   #(ribcage
                                                                                     #(f
                                                                                       clause
                                                                                       clauses)
                                                                                     #((top)
                                                                                       (top)
                                                                                       (top))
                                                                                     #("i"
                                                                                       "i"
                                                                                       "i"))
                                                                                   #(ribcage
                                                                                     #(_
                                                                                       e
                                                                                       m1
                                                                                       m2)
                                                                                     #((top)
                                                                                       (top)
                                                                                       (top)
                                                                                       (top))
                                                                                     #("i"
                                                                                       "i"
                                                                                       "i"
                                                                                       "i"))
                                                                                   #(ribcage
                                                                                     ()
                                                                                     ()
                                                                                     ())
                                                                                   #(ribcage
                                                                                     #(x)
                                                                                     #((top))
                                                                                     #("i")))
                                                                                  (hygiene
                                                                                    guile))
                                                                               (list '#(syntax-object
                                                                                        quote
                                                                                        ((top)
                                                                                         #(ribcage
                                                                                           #(k
                                                                                             e1
                                                                                             e2)
                                                                                           #((top)
                                                                                             (top)
                                                                                             (top))
                                                                                           #("i"
                                                                                             "i"
                                                                                             "i"))
                                                                                         #(ribcage
                                                                                           #(rest)
                                                                                           #((top))
                                                                                           #("i"))
                                                                                         #(ribcage
                                                                                           ()
                                                                                           ()
                                                                                           ())
                                                                                         #(ribcage
                                                                                           #(f
                                                                                             clause
                                                                                             clauses)
                                                                                           #((top)
                                                                                             (top)
                                                                                             (top))
                                                                                           #("i"
                                                                                             "i"
                                                                                             "i"))
                                                                                         #(ribcage
                                                                                           #(_
                                                                                             e
                                                                                             m1
                                                                                             m2)
                                                                                           #((top)
                                                                                             (top)
                                                                                             (top)
                                                                                             (top))
                                                                                           #("i"
                                                                                             "i"
                                                                                             "i"
                                                                                             "i"))
                                                                                         #(ribcage
                                                                                           ()
                                                                                           ()
                                                                                           ())
                                                                                         #(ribcage
                                                                                           #(x)
                                                                                           #((top))
                                                                                           #("i")))
                                                                                        (hygiene
                                                                                          guile))
                                                                                     k1574))
                                                                         (cons '#(syntax-object
                                                                                  begin
                                                                                  ((top)
                                                                                   #(ribcage
                                                                                     #(k
                                                                                       e1
                                                                                       e2)
                                                                                     #((top)
                                                                                       (top)
                                                                                       (top))
                                                                                     #("i"
                                                                                       "i"
                                                                                       "i"))
                                                                                   #(ribcage
                                                                                     #(rest)
                                                                                     #((top))
                                                                                     #("i"))
                                                                                   #(ribcage
                                                                                     ()
                                                                                     ()
                                                                                     ())
                                                                                   #(ribcage
                                                                                     #(f
                                                                                       clause
                                                                                       clauses)
                                                                                     #((top)
                                                                                       (top)
                                                                                       (top))
                                                                                     #("i"
                                                                                       "i"
                                                                                       "i"))
                                                                                   #(ribcage
                                                                                     #(_
                                                                                       e
                                                                                       m1
                                                                                       m2)
                                                                                     #((top)
                                                                                       (top)
                                                                                       (top)
                                                                                       (top))
                                                                                     #("i"
                                                                                       "i"
                                                                                       "i"
                                                                                       "i"))
                                                                                   #(ribcage
                                                                                     ()
                                                                                     ()
                                                                                     ())
                                                                                   #(ribcage
                                                                                     #(x)
                                                                                     #((top))
                                                                                     #("i")))
                                                                                  (hygiene
                                                                                    guile))
                                                                               (cons e11575
                                                                                     e21576))
                                                                         rest1571))
                                                                 tmp1573)
                                                          ((lambda (_1579)
                                                             (syntax-violation
                                                               'case
                                                               "bad clause"
                                                               x1545
                                                               clause1555))
                                                           tmp1572)))
                                                      ($sc-dispatch
                                                        tmp1572
                                                        '(each-any
                                                           any
                                                           .
                                                           each-any))))
                                                   clause1555))
                                                tmp1570))
                                             (f1554 (car clauses1556)
                                                    (cdr clauses1556)))))))
                          (f1554 m11550 m21551))))
                     tmp1547)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                tmp1546)))
          ($sc-dispatch
            tmp1546
            '(any any any . each-any))))
       x1545))))

(define identifier-syntax
  (make-syncase-macro
    'macro
    (lambda (x1580)
      ((lambda (tmp1581)
         ((lambda (tmp1582)
            (if tmp1582
              (apply (lambda (_1583 e1584)
                       (list '#(syntax-object
                                lambda
                                ((top)
                                 #(ribcage #(_ e) #((top) (top)) #("i" "i"))
                                 #(ribcage () () ())
                                 #(ribcage #(x) #((top)) #("i")))
                                (hygiene guile))
                             '(#(syntax-object
                                 x
                                 ((top)
                                  #(ribcage #(_ e) #((top) (top)) #("i" "i"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i")))
                                 (hygiene guile)))
                             (list '#(syntax-object
                                      syntax-case
                                      ((top)
                                       #(ribcage
                                         #(_ e)
                                         #((top) (top))
                                         #("i" "i"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i")))
                                      (hygiene guile))
                                   '#(syntax-object
                                      x
                                      ((top)
                                       #(ribcage
                                         #(_ e)
                                         #((top) (top))
                                         #("i" "i"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i")))
                                      (hygiene guile))
                                   '()
                                   (list '#(syntax-object
                                            id
                                            ((top)
                                             #(ribcage
                                               #(_ e)
                                               #((top) (top))
                                               #("i" "i"))
                                             #(ribcage () () ())
                                             #(ribcage #(x) #((top)) #("i")))
                                            (hygiene guile))
                                         '(#(syntax-object
                                             identifier?
                                             ((top)
                                              #(ribcage
                                                #(_ e)
                                                #((top) (top))
                                                #("i" "i"))
                                              #(ribcage () () ())
                                              #(ribcage #(x) #((top)) #("i")))
                                             (hygiene guile))
                                           (#(syntax-object
                                              syntax
                                              ((top)
                                               #(ribcage
                                                 #(_ e)
                                                 #((top) (top))
                                                 #("i" "i"))
                                               #(ribcage () () ())
                                               #(ribcage #(x) #((top)) #("i")))
                                              (hygiene guile))
                                            #(syntax-object
                                              id
                                              ((top)
                                               #(ribcage
                                                 #(_ e)
                                                 #((top) (top))
                                                 #("i" "i"))
                                               #(ribcage () () ())
                                               #(ribcage #(x) #((top)) #("i")))
                                              (hygiene guile))))
                                         (list '#(syntax-object
                                                  syntax
                                                  ((top)
                                                   #(ribcage
                                                     #(_ e)
                                                     #((top) (top))
                                                     #("i" "i"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x)
                                                     #((top))
                                                     #("i")))
                                                  (hygiene guile))
                                               e1584))
                                   (list (cons _1583
                                               '(#(syntax-object
                                                   x
                                                   ((top)
                                                    #(ribcage
                                                      #(_ e)
                                                      #((top) (top))
                                                      #("i" "i"))
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(x)
                                                      #((top))
                                                      #("i")))
                                                   (hygiene guile))
                                                 #(syntax-object
                                                   ...
                                                   ((top)
                                                    #(ribcage
                                                      #(_ e)
                                                      #((top) (top))
                                                      #("i" "i"))
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(x)
                                                      #((top))
                                                      #("i")))
                                                   (hygiene guile))))
                                         (list '#(syntax-object
                                                  syntax
                                                  ((top)
                                                   #(ribcage
                                                     #(_ e)
                                                     #((top) (top))
                                                     #("i" "i"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x)
                                                     #((top))
                                                     #("i")))
                                                  (hygiene guile))
                                               (cons e1584
                                                     '(#(syntax-object
                                                         x
                                                         ((top)
                                                          #(ribcage
                                                            #(_ e)
                                                            #((top) (top))
                                                            #("i" "i"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(x)
                                                            #((top))
                                                            #("i")))
                                                         (hygiene guile))
                                                       #(syntax-object
                                                         ...
                                                         ((top)
                                                          #(ribcage
                                                            #(_ e)
                                                            #((top) (top))
                                                            #("i" "i"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(x)
                                                            #((top))
                                                            #("i")))
                                                         (hygiene
                                                           guile)))))))))
                     tmp1582)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                tmp1581)))
          ($sc-dispatch tmp1581 (quote (any any)))))
       x1580))))

