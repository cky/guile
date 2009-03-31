(letrec ((syntmp-lambda-var-list-168
           (lambda (syntmp-vars-559)
             (let syntmp-lvl-560 ((syntmp-vars-561 syntmp-vars-559)
                                  (syntmp-ls-562 (quote ()))
                                  (syntmp-w-563 (quote (()))))
               (cond ((pair? syntmp-vars-561)
                      (syntmp-lvl-560
                        (cdr syntmp-vars-561)
                        (cons (syntmp-wrap-147
                                (car syntmp-vars-561)
                                syntmp-w-563
                                #f)
                              syntmp-ls-562)
                        syntmp-w-563))
                     ((syntmp-id?-119 syntmp-vars-561)
                      (cons (syntmp-wrap-147 syntmp-vars-561 syntmp-w-563 #f)
                            syntmp-ls-562))
                     ((null? syntmp-vars-561) syntmp-ls-562)
                     ((syntmp-syntax-object?-103 syntmp-vars-561)
                      (syntmp-lvl-560
                        (syntmp-syntax-object-expression-104
                          syntmp-vars-561)
                        syntmp-ls-562
                        (syntmp-join-wraps-138
                          syntmp-w-563
                          (syntmp-syntax-object-wrap-105 syntmp-vars-561))))
                     ((annotation? syntmp-vars-561)
                      (syntmp-lvl-560
                        (annotation-expression syntmp-vars-561)
                        syntmp-ls-562
                        syntmp-w-563))
                     (else (cons syntmp-vars-561 syntmp-ls-562))))))
         (syntmp-gen-var-167
           (lambda (syntmp-id-564)
             (let ((syntmp-id-565
                     (if (syntmp-syntax-object?-103 syntmp-id-564)
                       (syntmp-syntax-object-expression-104
                         syntmp-id-564)
                       syntmp-id-564)))
               (if (annotation? syntmp-id-565)
                 (syntmp-build-annotated-96
                   (annotation-source syntmp-id-565)
                   (gensym
                     (symbol->string
                       (annotation-expression syntmp-id-565))))
                 (syntmp-build-annotated-96
                   #f
                   (gensym (symbol->string syntmp-id-565)))))))
         (syntmp-strip-166
           (lambda (syntmp-x-566 syntmp-w-567)
             (if (memq 'top
                       (syntmp-wrap-marks-122 syntmp-w-567))
               (if (or (annotation? syntmp-x-566)
                       (and (pair? syntmp-x-566)
                            (annotation? (car syntmp-x-566))))
                 (syntmp-strip-annotation-165 syntmp-x-566 #f)
                 syntmp-x-566)
               (let syntmp-f-568 ((syntmp-x-569 syntmp-x-566))
                 (cond ((syntmp-syntax-object?-103 syntmp-x-569)
                        (syntmp-strip-166
                          (syntmp-syntax-object-expression-104
                            syntmp-x-569)
                          (syntmp-syntax-object-wrap-105 syntmp-x-569)))
                       ((pair? syntmp-x-569)
                        (let ((syntmp-a-570 (syntmp-f-568 (car syntmp-x-569)))
                              (syntmp-d-571 (syntmp-f-568 (cdr syntmp-x-569))))
                          (if (and (eq? syntmp-a-570 (car syntmp-x-569))
                                   (eq? syntmp-d-571 (cdr syntmp-x-569)))
                            syntmp-x-569
                            (cons syntmp-a-570 syntmp-d-571))))
                       ((vector? syntmp-x-569)
                        (let ((syntmp-old-572 (vector->list syntmp-x-569)))
                          (let ((syntmp-new-573
                                  (map syntmp-f-568 syntmp-old-572)))
                            (if (andmap eq? syntmp-old-572 syntmp-new-573)
                              syntmp-x-569
                              (list->vector syntmp-new-573)))))
                       (else syntmp-x-569))))))
         (syntmp-strip-annotation-165
           (lambda (syntmp-x-574 syntmp-parent-575)
             (cond ((pair? syntmp-x-574)
                    (let ((syntmp-new-576 (cons #f #f)))
                      (begin
                        (if syntmp-parent-575
                          (set-annotation-stripped!
                            syntmp-parent-575
                            syntmp-new-576))
                        (set-car!
                          syntmp-new-576
                          (syntmp-strip-annotation-165
                            (car syntmp-x-574)
                            #f))
                        (set-cdr!
                          syntmp-new-576
                          (syntmp-strip-annotation-165
                            (cdr syntmp-x-574)
                            #f))
                        syntmp-new-576)))
                   ((annotation? syntmp-x-574)
                    (or (annotation-stripped syntmp-x-574)
                        (syntmp-strip-annotation-165
                          (annotation-expression syntmp-x-574)
                          syntmp-x-574)))
                   ((vector? syntmp-x-574)
                    (let ((syntmp-new-577
                            (make-vector (vector-length syntmp-x-574))))
                      (begin
                        (if syntmp-parent-575
                          (set-annotation-stripped!
                            syntmp-parent-575
                            syntmp-new-577))
                        (let syntmp-loop-578 ((syntmp-i-579
                                                (- (vector-length syntmp-x-574)
                                                   1)))
                          (unless
                            (syntmp-fx<-90 syntmp-i-579 0)
                            (vector-set!
                              syntmp-new-577
                              syntmp-i-579
                              (syntmp-strip-annotation-165
                                (vector-ref syntmp-x-574 syntmp-i-579)
                                #f))
                            (syntmp-loop-578 (syntmp-fx--88 syntmp-i-579 1))))
                        syntmp-new-577)))
                   (else syntmp-x-574))))
         (syntmp-ellipsis?-164
           (lambda (syntmp-x-580)
             (and (syntmp-nonsymbol-id?-118 syntmp-x-580)
                  (syntmp-free-id=?-142
                    syntmp-x-580
                    '#(syntax-object
                       ...
                       ((top)
                        #(ribcage () () ())
                        #(ribcage #(x) #((top)) #("i"))
                        #(ribcage
                          (lambda-var-list
                            gen-var
                            strip
                            strip-annotation
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
                            unannotate
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
                            build-global-assignment
                            build-global-reference
                            build-lexical-assignment
                            build-lexical-reference
                            build-conditional
                            build-application
                            build-annotated
                            get-global-definition-hook
                            put-global-definition-hook
                            gensym-hook
                            error-hook
                            local-eval-hook
                            top-level-eval-hook
                            fx<
                            fx=
                            fx-
                            fx+
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
                           "i"))
                        #(ribcage (define-structure) ((top)) ("i")))
                       (ice-9 syncase))))))
         (syntmp-chi-void-163
           (lambda ()
             (syntmp-build-annotated-96
               #f
               (list (syntmp-build-annotated-96 #f (quote void))))))
         (syntmp-eval-local-transformer-162
           (lambda (syntmp-expanded-581 syntmp-mod-582)
             (let ((syntmp-p-583
                     (syntmp-local-eval-hook-92
                       syntmp-expanded-581
                       syntmp-mod-582)))
               (if (procedure? syntmp-p-583)
                 syntmp-p-583
                 (syntax-error
                   syntmp-p-583
                   "nonprocedure transformer")))))
         (syntmp-chi-local-syntax-161
           (lambda (syntmp-rec?-584
                    syntmp-e-585
                    syntmp-r-586
                    syntmp-w-587
                    syntmp-s-588
                    syntmp-mod-589
                    syntmp-k-590)
             ((lambda (syntmp-tmp-591)
                ((lambda (syntmp-tmp-592)
                   (if syntmp-tmp-592
                     (apply (lambda (syntmp-_-593
                                     syntmp-id-594
                                     syntmp-val-595
                                     syntmp-e1-596
                                     syntmp-e2-597)
                              (let ((syntmp-ids-598 syntmp-id-594))
                                (if (not (syntmp-valid-bound-ids?-144
                                           syntmp-ids-598))
                                  (syntax-error
                                    syntmp-e-585
                                    "duplicate bound keyword in")
                                  (let ((syntmp-labels-600
                                          (syntmp-gen-labels-125
                                            syntmp-ids-598)))
                                    (let ((syntmp-new-w-601
                                            (syntmp-make-binding-wrap-136
                                              syntmp-ids-598
                                              syntmp-labels-600
                                              syntmp-w-587)))
                                      (syntmp-k-590
                                        (cons syntmp-e1-596 syntmp-e2-597)
                                        (syntmp-extend-env-113
                                          syntmp-labels-600
                                          (let ((syntmp-w-603
                                                  (if syntmp-rec?-584
                                                    syntmp-new-w-601
                                                    syntmp-w-587))
                                                (syntmp-trans-r-604
                                                  (syntmp-macros-only-env-115
                                                    syntmp-r-586)))
                                            (map (lambda (syntmp-x-605)
                                                   (cons 'macro
                                                         (syntmp-eval-local-transformer-162
                                                           (syntmp-chi-155
                                                             syntmp-x-605
                                                             syntmp-trans-r-604
                                                             syntmp-w-603
                                                             syntmp-mod-589)
                                                           syntmp-mod-589)))
                                                 syntmp-val-595))
                                          syntmp-r-586)
                                        syntmp-new-w-601
                                        syntmp-s-588
                                        syntmp-mod-589))))))
                            syntmp-tmp-592)
                     ((lambda (syntmp-_-607)
                        (syntax-error
                          (syntmp-source-wrap-148
                            syntmp-e-585
                            syntmp-w-587
                            syntmp-s-588
                            syntmp-mod-589)))
                      syntmp-tmp-591)))
                 (syntax-dispatch
                   syntmp-tmp-591
                   '(any #(each (any any)) any . each-any))))
              syntmp-e-585)))
         (syntmp-chi-lambda-clause-160
           (lambda (syntmp-e-608
                    syntmp-c-609
                    syntmp-r-610
                    syntmp-w-611
                    syntmp-mod-612
                    syntmp-k-613)
             ((lambda (syntmp-tmp-614)
                ((lambda (syntmp-tmp-615)
                   (if syntmp-tmp-615
                     (apply (lambda (syntmp-id-616 syntmp-e1-617 syntmp-e2-618)
                              (let ((syntmp-ids-619 syntmp-id-616))
                                (if (not (syntmp-valid-bound-ids?-144
                                           syntmp-ids-619))
                                  (syntax-error
                                    syntmp-e-608
                                    "invalid parameter list in")
                                  (let ((syntmp-labels-621
                                          (syntmp-gen-labels-125
                                            syntmp-ids-619))
                                        (syntmp-new-vars-622
                                          (map syntmp-gen-var-167
                                               syntmp-ids-619)))
                                    (syntmp-k-613
                                      syntmp-new-vars-622
                                      (syntmp-chi-body-159
                                        (cons syntmp-e1-617 syntmp-e2-618)
                                        syntmp-e-608
                                        (syntmp-extend-var-env-114
                                          syntmp-labels-621
                                          syntmp-new-vars-622
                                          syntmp-r-610)
                                        (syntmp-make-binding-wrap-136
                                          syntmp-ids-619
                                          syntmp-labels-621
                                          syntmp-w-611)
                                        syntmp-mod-612))))))
                            syntmp-tmp-615)
                     ((lambda (syntmp-tmp-624)
                        (if syntmp-tmp-624
                          (apply (lambda (syntmp-ids-625
                                          syntmp-e1-626
                                          syntmp-e2-627)
                                   (let ((syntmp-old-ids-628
                                           (syntmp-lambda-var-list-168
                                             syntmp-ids-625)))
                                     (if (not (syntmp-valid-bound-ids?-144
                                                syntmp-old-ids-628))
                                       (syntax-error
                                         syntmp-e-608
                                         "invalid parameter list in")
                                       (let ((syntmp-labels-629
                                               (syntmp-gen-labels-125
                                                 syntmp-old-ids-628))
                                             (syntmp-new-vars-630
                                               (map syntmp-gen-var-167
                                                    syntmp-old-ids-628)))
                                         (syntmp-k-613
                                           (let syntmp-f-631 ((syntmp-ls1-632
                                                                (cdr syntmp-new-vars-630))
                                                              (syntmp-ls2-633
                                                                (car syntmp-new-vars-630)))
                                             (if (null? syntmp-ls1-632)
                                               syntmp-ls2-633
                                               (syntmp-f-631
                                                 (cdr syntmp-ls1-632)
                                                 (cons (car syntmp-ls1-632)
                                                       syntmp-ls2-633))))
                                           (syntmp-chi-body-159
                                             (cons syntmp-e1-626 syntmp-e2-627)
                                             syntmp-e-608
                                             (syntmp-extend-var-env-114
                                               syntmp-labels-629
                                               syntmp-new-vars-630
                                               syntmp-r-610)
                                             (syntmp-make-binding-wrap-136
                                               syntmp-old-ids-628
                                               syntmp-labels-629
                                               syntmp-w-611)
                                             syntmp-mod-612))))))
                                 syntmp-tmp-624)
                          ((lambda (syntmp-_-635)
                             (syntax-error syntmp-e-608))
                           syntmp-tmp-614)))
                      (syntax-dispatch
                        syntmp-tmp-614
                        '(any any . each-any)))))
                 (syntax-dispatch
                   syntmp-tmp-614
                   '(each-any any . each-any))))
              syntmp-c-609)))
         (syntmp-chi-body-159
           (lambda (syntmp-body-636
                    syntmp-outer-form-637
                    syntmp-r-638
                    syntmp-w-639
                    syntmp-mod-640)
             (let ((syntmp-r-641
                     (cons '("placeholder" placeholder)
                           syntmp-r-638)))
               (let ((syntmp-ribcage-642
                       (syntmp-make-ribcage-126
                         '()
                         '()
                         '())))
                 (let ((syntmp-w-643
                         (syntmp-make-wrap-121
                           (syntmp-wrap-marks-122 syntmp-w-639)
                           (cons syntmp-ribcage-642
                                 (syntmp-wrap-subst-123 syntmp-w-639)))))
                   (let syntmp-parse-644 ((syntmp-body-645
                                            (map (lambda (syntmp-x-651)
                                                   (cons syntmp-r-641
                                                         (syntmp-wrap-147
                                                           syntmp-x-651
                                                           syntmp-w-643
                                                           syntmp-mod-640)))
                                                 syntmp-body-636))
                                          (syntmp-ids-646 (quote ()))
                                          (syntmp-labels-647 (quote ()))
                                          (syntmp-vars-648 (quote ()))
                                          (syntmp-vals-649 (quote ()))
                                          (syntmp-bindings-650 (quote ())))
                     (if (null? syntmp-body-645)
                       (syntax-error
                         syntmp-outer-form-637
                         "no expressions in body")
                       (let ((syntmp-e-652 (cdar syntmp-body-645))
                             (syntmp-er-653 (caar syntmp-body-645)))
                         (call-with-values
                           (lambda ()
                             (syntmp-syntax-type-153
                               syntmp-e-652
                               syntmp-er-653
                               '(())
                               #f
                               syntmp-ribcage-642
                               syntmp-mod-640))
                           (lambda (syntmp-type-654
                                    syntmp-value-655
                                    syntmp-e-656
                                    syntmp-w-657
                                    syntmp-s-658
                                    syntmp-mod-659)
                             (let ((syntmp-t-660 syntmp-type-654))
                               (if (memv syntmp-t-660 (quote (define-form)))
                                 (let ((syntmp-id-661
                                         (syntmp-wrap-147
                                           syntmp-value-655
                                           syntmp-w-657
                                           syntmp-mod-659))
                                       (syntmp-label-662
                                         (syntmp-gen-label-124)))
                                   (let ((syntmp-var-663
                                           (syntmp-gen-var-167 syntmp-id-661)))
                                     (begin
                                       (syntmp-extend-ribcage!-135
                                         syntmp-ribcage-642
                                         syntmp-id-661
                                         syntmp-label-662)
                                       (syntmp-parse-644
                                         (cdr syntmp-body-645)
                                         (cons syntmp-id-661 syntmp-ids-646)
                                         (cons syntmp-label-662
                                               syntmp-labels-647)
                                         (cons syntmp-var-663 syntmp-vars-648)
                                         (cons (cons syntmp-er-653
                                                     (syntmp-wrap-147
                                                       syntmp-e-656
                                                       syntmp-w-657
                                                       syntmp-mod-659))
                                               syntmp-vals-649)
                                         (cons (cons 'lexical
                                                     syntmp-var-663)
                                               syntmp-bindings-650)))))
                                 (if (memv syntmp-t-660
                                           '(define-syntax-form))
                                   (let ((syntmp-id-664
                                           (syntmp-wrap-147
                                             syntmp-value-655
                                             syntmp-w-657
                                             syntmp-mod-659))
                                         (syntmp-label-665
                                           (syntmp-gen-label-124)))
                                     (begin
                                       (syntmp-extend-ribcage!-135
                                         syntmp-ribcage-642
                                         syntmp-id-664
                                         syntmp-label-665)
                                       (syntmp-parse-644
                                         (cdr syntmp-body-645)
                                         (cons syntmp-id-664 syntmp-ids-646)
                                         (cons syntmp-label-665
                                               syntmp-labels-647)
                                         syntmp-vars-648
                                         syntmp-vals-649
                                         (cons (cons 'macro
                                                     (cons syntmp-er-653
                                                           (syntmp-wrap-147
                                                             syntmp-e-656
                                                             syntmp-w-657
                                                             syntmp-mod-659)))
                                               syntmp-bindings-650))))
                                   (if (memv syntmp-t-660 (quote (begin-form)))
                                     ((lambda (syntmp-tmp-666)
                                        ((lambda (syntmp-tmp-667)
                                           (if syntmp-tmp-667
                                             (apply (lambda (syntmp-_-668
                                                             syntmp-e1-669)
                                                      (syntmp-parse-644
                                                        (let syntmp-f-670 ((syntmp-forms-671
                                                                             syntmp-e1-669))
                                                          (if (null? syntmp-forms-671)
                                                            (cdr syntmp-body-645)
                                                            (cons (cons syntmp-er-653
                                                                        (syntmp-wrap-147
                                                                          (car syntmp-forms-671)
                                                                          syntmp-w-657
                                                                          syntmp-mod-659))
                                                                  (syntmp-f-670
                                                                    (cdr syntmp-forms-671)))))
                                                        syntmp-ids-646
                                                        syntmp-labels-647
                                                        syntmp-vars-648
                                                        syntmp-vals-649
                                                        syntmp-bindings-650))
                                                    syntmp-tmp-667)
                                             (syntax-error syntmp-tmp-666)))
                                         (syntax-dispatch
                                           syntmp-tmp-666
                                           '(any . each-any))))
                                      syntmp-e-656)
                                     (if (memv syntmp-t-660
                                               '(local-syntax-form))
                                       (syntmp-chi-local-syntax-161
                                         syntmp-value-655
                                         syntmp-e-656
                                         syntmp-er-653
                                         syntmp-w-657
                                         syntmp-s-658
                                         syntmp-mod-659
                                         (lambda (syntmp-forms-673
                                                  syntmp-er-674
                                                  syntmp-w-675
                                                  syntmp-s-676
                                                  syntmp-mod-677)
                                           (syntmp-parse-644
                                             (let syntmp-f-678 ((syntmp-forms-679
                                                                  syntmp-forms-673))
                                               (if (null? syntmp-forms-679)
                                                 (cdr syntmp-body-645)
                                                 (cons (cons syntmp-er-674
                                                             (syntmp-wrap-147
                                                               (car syntmp-forms-679)
                                                               syntmp-w-675
                                                               syntmp-mod-677))
                                                       (syntmp-f-678
                                                         (cdr syntmp-forms-679)))))
                                             syntmp-ids-646
                                             syntmp-labels-647
                                             syntmp-vars-648
                                             syntmp-vals-649
                                             syntmp-bindings-650)))
                                       (if (null? syntmp-ids-646)
                                         (syntmp-build-sequence-98
                                           #f
                                           (map (lambda (syntmp-x-680)
                                                  (syntmp-chi-155
                                                    (cdr syntmp-x-680)
                                                    (car syntmp-x-680)
                                                    '(())
                                                    syntmp-mod-659))
                                                (cons (cons syntmp-er-653
                                                            (syntmp-source-wrap-148
                                                              syntmp-e-656
                                                              syntmp-w-657
                                                              syntmp-s-658
                                                              syntmp-mod-659))
                                                      (cdr syntmp-body-645))))
                                         (begin
                                           (if (not (syntmp-valid-bound-ids?-144
                                                      syntmp-ids-646))
                                             (syntax-error
                                               syntmp-outer-form-637
                                               "invalid or duplicate identifier in definition"))
                                           (let syntmp-loop-681 ((syntmp-bs-682
                                                                   syntmp-bindings-650)
                                                                 (syntmp-er-cache-683
                                                                   #f)
                                                                 (syntmp-r-cache-684
                                                                   #f))
                                             (if (not (null? syntmp-bs-682))
                                               (let ((syntmp-b-685
                                                       (car syntmp-bs-682)))
                                                 (if (eq? (car syntmp-b-685)
                                                          'macro)
                                                   (let ((syntmp-er-686
                                                           (cadr syntmp-b-685)))
                                                     (let ((syntmp-r-cache-687
                                                             (if (eq? syntmp-er-686
                                                                      syntmp-er-cache-683)
                                                               syntmp-r-cache-684
                                                               (syntmp-macros-only-env-115
                                                                 syntmp-er-686))))
                                                       (begin
                                                         (set-cdr!
                                                           syntmp-b-685
                                                           (syntmp-eval-local-transformer-162
                                                             (syntmp-chi-155
                                                               (cddr syntmp-b-685)
                                                               syntmp-r-cache-687
                                                               '(())
                                                               syntmp-mod-659)
                                                             syntmp-mod-659))
                                                         (syntmp-loop-681
                                                           (cdr syntmp-bs-682)
                                                           syntmp-er-686
                                                           syntmp-r-cache-687))))
                                                   (syntmp-loop-681
                                                     (cdr syntmp-bs-682)
                                                     syntmp-er-cache-683
                                                     syntmp-r-cache-684)))))
                                           (set-cdr!
                                             syntmp-r-641
                                             (syntmp-extend-env-113
                                               syntmp-labels-647
                                               syntmp-bindings-650
                                               (cdr syntmp-r-641)))
                                           (syntmp-build-letrec-101
                                             #f
                                             syntmp-vars-648
                                             (map (lambda (syntmp-x-688)
                                                    (syntmp-chi-155
                                                      (cdr syntmp-x-688)
                                                      (car syntmp-x-688)
                                                      '(())
                                                      syntmp-mod-659))
                                                  syntmp-vals-649)
                                             (syntmp-build-sequence-98
                                               #f
                                               (map (lambda (syntmp-x-689)
                                                      (syntmp-chi-155
                                                        (cdr syntmp-x-689)
                                                        (car syntmp-x-689)
                                                        '(())
                                                        syntmp-mod-659))
                                                    (cons (cons syntmp-er-653
                                                                (syntmp-source-wrap-148
                                                                  syntmp-e-656
                                                                  syntmp-w-657
                                                                  syntmp-s-658
                                                                  syntmp-mod-659))
                                                          (cdr syntmp-body-645))))))))))))))))))))))
         (syntmp-chi-macro-158
           (lambda (syntmp-p-690
                    syntmp-e-691
                    syntmp-r-692
                    syntmp-w-693
                    syntmp-rib-694
                    syntmp-mod-695)
             (letrec ((syntmp-rebuild-macro-output-696
                        (lambda (syntmp-x-697 syntmp-m-698)
                          (cond ((pair? syntmp-x-697)
                                 (cons (syntmp-rebuild-macro-output-696
                                         (car syntmp-x-697)
                                         syntmp-m-698)
                                       (syntmp-rebuild-macro-output-696
                                         (cdr syntmp-x-697)
                                         syntmp-m-698)))
                                ((syntmp-syntax-object?-103 syntmp-x-697)
                                 (let ((syntmp-w-699
                                         (syntmp-syntax-object-wrap-105
                                           syntmp-x-697)))
                                   (let ((syntmp-ms-700
                                           (syntmp-wrap-marks-122
                                             syntmp-w-699))
                                         (syntmp-s-701
                                           (syntmp-wrap-subst-123
                                             syntmp-w-699)))
                                     (if (and (pair? syntmp-ms-700)
                                              (eq? (car syntmp-ms-700) #f))
                                       (syntmp-make-syntax-object-102
                                         (syntmp-syntax-object-expression-104
                                           syntmp-x-697)
                                         (syntmp-make-wrap-121
                                           (cdr syntmp-ms-700)
                                           (if syntmp-rib-694
                                             (cons syntmp-rib-694
                                                   (cdr syntmp-s-701))
                                             (cdr syntmp-s-701)))
                                         (syntmp-syntax-object-module-106
                                           syntmp-x-697))
                                       (syntmp-make-syntax-object-102
                                         (syntmp-syntax-object-expression-104
                                           syntmp-x-697)
                                         (syntmp-make-wrap-121
                                           (cons syntmp-m-698 syntmp-ms-700)
                                           (if syntmp-rib-694
                                             (cons syntmp-rib-694
                                                   (cons 'shift
                                                         syntmp-s-701))
                                             (cons 'shift
                                                   syntmp-s-701)))
                                         (module-name
                                           (procedure-module
                                             syntmp-p-690)))))))
                                ((vector? syntmp-x-697)
                                 (let ((syntmp-n-702
                                         (vector-length syntmp-x-697)))
                                   (let ((syntmp-v-703
                                           (make-vector syntmp-n-702)))
                                     (let syntmp-doloop-704 ((syntmp-i-705 0))
                                       (if (syntmp-fx=-89
                                             syntmp-i-705
                                             syntmp-n-702)
                                         syntmp-v-703
                                         (begin
                                           (vector-set!
                                             syntmp-v-703
                                             syntmp-i-705
                                             (syntmp-rebuild-macro-output-696
                                               (vector-ref
                                                 syntmp-x-697
                                                 syntmp-i-705)
                                               syntmp-m-698))
                                           (syntmp-doloop-704
                                             (syntmp-fx+-87
                                               syntmp-i-705
                                               1))))))))
                                ((symbol? syntmp-x-697)
                                 (syntax-error
                                   syntmp-x-697
                                   "encountered raw symbol in macro output"))
                                (else syntmp-x-697)))))
               (syntmp-rebuild-macro-output-696
                 (syntmp-p-690
                   (syntmp-wrap-147
                     syntmp-e-691
                     (syntmp-anti-mark-134 syntmp-w-693)
                     syntmp-mod-695))
                 (string #\m)))))
         (syntmp-chi-application-157
           (lambda (syntmp-x-706
                    syntmp-e-707
                    syntmp-r-708
                    syntmp-w-709
                    syntmp-s-710
                    syntmp-mod-711)
             ((lambda (syntmp-tmp-712)
                ((lambda (syntmp-tmp-713)
                   (if syntmp-tmp-713
                     (apply (lambda (syntmp-e0-714 syntmp-e1-715)
                              (syntmp-build-annotated-96
                                syntmp-s-710
                                (cons syntmp-x-706
                                      (map (lambda (syntmp-e-716)
                                             (syntmp-chi-155
                                               syntmp-e-716
                                               syntmp-r-708
                                               syntmp-w-709
                                               syntmp-mod-711))
                                           syntmp-e1-715))))
                            syntmp-tmp-713)
                     (syntax-error syntmp-tmp-712)))
                 (syntax-dispatch
                   syntmp-tmp-712
                   '(any . each-any))))
              syntmp-e-707)))
         (syntmp-chi-expr-156
           (lambda (syntmp-type-718
                    syntmp-value-719
                    syntmp-e-720
                    syntmp-r-721
                    syntmp-w-722
                    syntmp-s-723
                    syntmp-mod-724)
             (let ((syntmp-t-725 syntmp-type-718))
               (if (memv syntmp-t-725 (quote (lexical)))
                 (syntmp-build-annotated-96
                   syntmp-s-723
                   syntmp-value-719)
                 (if (memv syntmp-t-725 (quote (core external-macro)))
                   (syntmp-value-719
                     syntmp-e-720
                     syntmp-r-721
                     syntmp-w-722
                     syntmp-s-723
                     syntmp-mod-724)
                   (if (memv syntmp-t-725 (quote (lexical-call)))
                     (syntmp-chi-application-157
                       (syntmp-build-annotated-96
                         (syntmp-source-annotation-110 (car syntmp-e-720))
                         syntmp-value-719)
                       syntmp-e-720
                       syntmp-r-721
                       syntmp-w-722
                       syntmp-s-723
                       syntmp-mod-724)
                     (if (memv syntmp-t-725 (quote (global-call)))
                       (syntmp-chi-application-157
                         (syntmp-build-annotated-96
                           (syntmp-source-annotation-110 (car syntmp-e-720))
                           (make-module-ref
                             (if (syntmp-syntax-object?-103 (car syntmp-e-720))
                               (syntmp-syntax-object-module-106
                                 (car syntmp-e-720))
                               syntmp-mod-724)
                             syntmp-value-719
                             #f))
                         syntmp-e-720
                         syntmp-r-721
                         syntmp-w-722
                         syntmp-s-723
                         syntmp-mod-724)
                       (if (memv syntmp-t-725 (quote (constant)))
                         (syntmp-build-data-97
                           syntmp-s-723
                           (syntmp-strip-166
                             (syntmp-source-wrap-148
                               syntmp-e-720
                               syntmp-w-722
                               syntmp-s-723
                               syntmp-mod-724)
                             '(())))
                         (if (memv syntmp-t-725 (quote (global)))
                           (syntmp-build-annotated-96
                             syntmp-s-723
                             (make-module-ref
                               syntmp-mod-724
                               syntmp-value-719
                               #f))
                           (if (memv syntmp-t-725 (quote (call)))
                             (syntmp-chi-application-157
                               (syntmp-chi-155
                                 (car syntmp-e-720)
                                 syntmp-r-721
                                 syntmp-w-722
                                 syntmp-mod-724)
                               syntmp-e-720
                               syntmp-r-721
                               syntmp-w-722
                               syntmp-s-723
                               syntmp-mod-724)
                             (if (memv syntmp-t-725 (quote (begin-form)))
                               ((lambda (syntmp-tmp-726)
                                  ((lambda (syntmp-tmp-727)
                                     (if syntmp-tmp-727
                                       (apply (lambda (syntmp-_-728
                                                       syntmp-e1-729
                                                       syntmp-e2-730)
                                                (syntmp-chi-sequence-149
                                                  (cons syntmp-e1-729
                                                        syntmp-e2-730)
                                                  syntmp-r-721
                                                  syntmp-w-722
                                                  syntmp-s-723
                                                  syntmp-mod-724))
                                              syntmp-tmp-727)
                                       (syntax-error syntmp-tmp-726)))
                                   (syntax-dispatch
                                     syntmp-tmp-726
                                     '(any any . each-any))))
                                syntmp-e-720)
                               (if (memv syntmp-t-725
                                         '(local-syntax-form))
                                 (syntmp-chi-local-syntax-161
                                   syntmp-value-719
                                   syntmp-e-720
                                   syntmp-r-721
                                   syntmp-w-722
                                   syntmp-s-723
                                   syntmp-mod-724
                                   syntmp-chi-sequence-149)
                                 (if (memv syntmp-t-725
                                           '(eval-when-form))
                                   ((lambda (syntmp-tmp-732)
                                      ((lambda (syntmp-tmp-733)
                                         (if syntmp-tmp-733
                                           (apply (lambda (syntmp-_-734
                                                           syntmp-x-735
                                                           syntmp-e1-736
                                                           syntmp-e2-737)
                                                    (let ((syntmp-when-list-738
                                                            (syntmp-chi-when-list-152
                                                              syntmp-e-720
                                                              syntmp-x-735
                                                              syntmp-w-722)))
                                                      (if (memq 'eval
                                                                syntmp-when-list-738)
                                                        (syntmp-chi-sequence-149
                                                          (cons syntmp-e1-736
                                                                syntmp-e2-737)
                                                          syntmp-r-721
                                                          syntmp-w-722
                                                          syntmp-s-723
                                                          syntmp-mod-724)
                                                        (syntmp-chi-void-163))))
                                                  syntmp-tmp-733)
                                           (syntax-error syntmp-tmp-732)))
                                       (syntax-dispatch
                                         syntmp-tmp-732
                                         '(any each-any any . each-any))))
                                    syntmp-e-720)
                                   (if (memv syntmp-t-725
                                             '(define-form define-syntax-form))
                                     (syntax-error
                                       (syntmp-wrap-147
                                         syntmp-value-719
                                         syntmp-w-722
                                         syntmp-mod-724)
                                       "invalid context for definition of")
                                     (if (memv syntmp-t-725 (quote (syntax)))
                                       (syntax-error
                                         (syntmp-source-wrap-148
                                           syntmp-e-720
                                           syntmp-w-722
                                           syntmp-s-723
                                           syntmp-mod-724)
                                         "reference to pattern variable outside syntax form")
                                       (if (memv syntmp-t-725
                                                 '(displaced-lexical))
                                         (syntax-error
                                           (syntmp-source-wrap-148
                                             syntmp-e-720
                                             syntmp-w-722
                                             syntmp-s-723
                                             syntmp-mod-724)
                                           "reference to identifier outside its scope")
                                         (syntax-error
                                           (syntmp-source-wrap-148
                                             syntmp-e-720
                                             syntmp-w-722
                                             syntmp-s-723
                                             syntmp-mod-724))))))))))))))))))
         (syntmp-chi-155
           (lambda (syntmp-e-741
                    syntmp-r-742
                    syntmp-w-743
                    syntmp-mod-744)
             (call-with-values
               (lambda ()
                 (syntmp-syntax-type-153
                   syntmp-e-741
                   syntmp-r-742
                   syntmp-w-743
                   #f
                   #f
                   syntmp-mod-744))
               (lambda (syntmp-type-745
                        syntmp-value-746
                        syntmp-e-747
                        syntmp-w-748
                        syntmp-s-749
                        syntmp-mod-750)
                 (syntmp-chi-expr-156
                   syntmp-type-745
                   syntmp-value-746
                   syntmp-e-747
                   syntmp-r-742
                   syntmp-w-748
                   syntmp-s-749
                   syntmp-mod-750)))))
         (syntmp-chi-top-154
           (lambda (syntmp-e-751
                    syntmp-r-752
                    syntmp-w-753
                    syntmp-m-754
                    syntmp-esew-755
                    syntmp-mod-756)
             (call-with-values
               (lambda ()
                 (syntmp-syntax-type-153
                   syntmp-e-751
                   syntmp-r-752
                   syntmp-w-753
                   #f
                   #f
                   syntmp-mod-756))
               (lambda (syntmp-type-771
                        syntmp-value-772
                        syntmp-e-773
                        syntmp-w-774
                        syntmp-s-775
                        syntmp-mod-776)
                 (let ((syntmp-t-777 syntmp-type-771))
                   (if (memv syntmp-t-777 (quote (begin-form)))
                     ((lambda (syntmp-tmp-778)
                        ((lambda (syntmp-tmp-779)
                           (if syntmp-tmp-779
                             (apply (lambda (syntmp-_-780)
                                      (syntmp-chi-void-163))
                                    syntmp-tmp-779)
                             ((lambda (syntmp-tmp-781)
                                (if syntmp-tmp-781
                                  (apply (lambda (syntmp-_-782
                                                  syntmp-e1-783
                                                  syntmp-e2-784)
                                           (syntmp-chi-top-sequence-150
                                             (cons syntmp-e1-783 syntmp-e2-784)
                                             syntmp-r-752
                                             syntmp-w-774
                                             syntmp-s-775
                                             syntmp-m-754
                                             syntmp-esew-755
                                             syntmp-mod-776))
                                         syntmp-tmp-781)
                                  (syntax-error syntmp-tmp-778)))
                              (syntax-dispatch
                                syntmp-tmp-778
                                '(any any . each-any)))))
                         (syntax-dispatch syntmp-tmp-778 (quote (any)))))
                      syntmp-e-773)
                     (if (memv syntmp-t-777 (quote (local-syntax-form)))
                       (syntmp-chi-local-syntax-161
                         syntmp-value-772
                         syntmp-e-773
                         syntmp-r-752
                         syntmp-w-774
                         syntmp-s-775
                         syntmp-mod-776
                         (lambda (syntmp-body-786
                                  syntmp-r-787
                                  syntmp-w-788
                                  syntmp-s-789
                                  syntmp-mod-790)
                           (syntmp-chi-top-sequence-150
                             syntmp-body-786
                             syntmp-r-787
                             syntmp-w-788
                             syntmp-s-789
                             syntmp-m-754
                             syntmp-esew-755
                             syntmp-mod-790)))
                       (if (memv syntmp-t-777 (quote (eval-when-form)))
                         ((lambda (syntmp-tmp-791)
                            ((lambda (syntmp-tmp-792)
                               (if syntmp-tmp-792
                                 (apply (lambda (syntmp-_-793
                                                 syntmp-x-794
                                                 syntmp-e1-795
                                                 syntmp-e2-796)
                                          (let ((syntmp-when-list-797
                                                  (syntmp-chi-when-list-152
                                                    syntmp-e-773
                                                    syntmp-x-794
                                                    syntmp-w-774))
                                                (syntmp-body-798
                                                  (cons syntmp-e1-795
                                                        syntmp-e2-796)))
                                            (cond ((eq? syntmp-m-754 (quote e))
                                                   (if (memq 'eval
                                                             syntmp-when-list-797)
                                                     (syntmp-chi-top-sequence-150
                                                       syntmp-body-798
                                                       syntmp-r-752
                                                       syntmp-w-774
                                                       syntmp-s-775
                                                       'e
                                                       '(eval)
                                                       syntmp-mod-776)
                                                     (syntmp-chi-void-163)))
                                                  ((memq 'load
                                                         syntmp-when-list-797)
                                                   (if (or (memq 'compile
                                                                 syntmp-when-list-797)
                                                           (and (eq? syntmp-m-754
                                                                     'c&e)
                                                                (memq 'eval
                                                                      syntmp-when-list-797)))
                                                     (syntmp-chi-top-sequence-150
                                                       syntmp-body-798
                                                       syntmp-r-752
                                                       syntmp-w-774
                                                       syntmp-s-775
                                                       'c&e
                                                       '(compile load)
                                                       syntmp-mod-776)
                                                     (if (memq syntmp-m-754
                                                               '(c c&e))
                                                       (syntmp-chi-top-sequence-150
                                                         syntmp-body-798
                                                         syntmp-r-752
                                                         syntmp-w-774
                                                         syntmp-s-775
                                                         'c
                                                         '(load)
                                                         syntmp-mod-776)
                                                       (syntmp-chi-void-163))))
                                                  ((or (memq 'compile
                                                             syntmp-when-list-797)
                                                       (and (eq? syntmp-m-754
                                                                 'c&e)
                                                            (memq 'eval
                                                                  syntmp-when-list-797)))
                                                   (syntmp-top-level-eval-hook-91
                                                     (syntmp-chi-top-sequence-150
                                                       syntmp-body-798
                                                       syntmp-r-752
                                                       syntmp-w-774
                                                       syntmp-s-775
                                                       'e
                                                       '(eval)
                                                       syntmp-mod-776)
                                                     syntmp-mod-776)
                                                   (syntmp-chi-void-163))
                                                  (else
                                                   (syntmp-chi-void-163)))))
                                        syntmp-tmp-792)
                                 (syntax-error syntmp-tmp-791)))
                             (syntax-dispatch
                               syntmp-tmp-791
                               '(any each-any any . each-any))))
                          syntmp-e-773)
                         (if (memv syntmp-t-777 (quote (define-syntax-form)))
                           (let ((syntmp-n-801
                                   (syntmp-id-var-name-141
                                     syntmp-value-772
                                     syntmp-w-774))
                                 (syntmp-r-802
                                   (syntmp-macros-only-env-115 syntmp-r-752)))
                             (let ((syntmp-t-803 syntmp-m-754))
                               (if (memv syntmp-t-803 (quote (c)))
                                 (if (memq (quote compile) syntmp-esew-755)
                                   (let ((syntmp-e-804
                                           (syntmp-chi-install-global-151
                                             syntmp-n-801
                                             (syntmp-chi-155
                                               syntmp-e-773
                                               syntmp-r-802
                                               syntmp-w-774
                                               syntmp-mod-776))))
                                     (begin
                                       (syntmp-top-level-eval-hook-91
                                         syntmp-e-804
                                         syntmp-mod-776)
                                       (if (memq (quote load) syntmp-esew-755)
                                         syntmp-e-804
                                         (syntmp-chi-void-163))))
                                   (if (memq (quote load) syntmp-esew-755)
                                     (syntmp-chi-install-global-151
                                       syntmp-n-801
                                       (syntmp-chi-155
                                         syntmp-e-773
                                         syntmp-r-802
                                         syntmp-w-774
                                         syntmp-mod-776))
                                     (syntmp-chi-void-163)))
                                 (if (memv syntmp-t-803 (quote (c&e)))
                                   (let ((syntmp-e-805
                                           (syntmp-chi-install-global-151
                                             syntmp-n-801
                                             (syntmp-chi-155
                                               syntmp-e-773
                                               syntmp-r-802
                                               syntmp-w-774
                                               syntmp-mod-776))))
                                     (begin
                                       (syntmp-top-level-eval-hook-91
                                         syntmp-e-805
                                         syntmp-mod-776)
                                       syntmp-e-805))
                                   (begin
                                     (if (memq (quote eval) syntmp-esew-755)
                                       (syntmp-top-level-eval-hook-91
                                         (syntmp-chi-install-global-151
                                           syntmp-n-801
                                           (syntmp-chi-155
                                             syntmp-e-773
                                             syntmp-r-802
                                             syntmp-w-774
                                             syntmp-mod-776))
                                         syntmp-mod-776))
                                     (syntmp-chi-void-163))))))
                           (if (memv syntmp-t-777 (quote (define-form)))
                             (let ((syntmp-n-806
                                     (syntmp-id-var-name-141
                                       syntmp-value-772
                                       syntmp-w-774)))
                               (let ((syntmp-type-807
                                       (syntmp-binding-type-111
                                         (syntmp-lookup-116
                                           syntmp-n-806
                                           syntmp-r-752
                                           syntmp-mod-776))))
                                 (let ((syntmp-t-808 syntmp-type-807))
                                   (if (memv syntmp-t-808 (quote (global)))
                                     (let ((syntmp-x-809
                                             (syntmp-build-annotated-96
                                               syntmp-s-775
                                               (list 'define
                                                     syntmp-n-806
                                                     (syntmp-chi-155
                                                       syntmp-e-773
                                                       syntmp-r-752
                                                       syntmp-w-774
                                                       syntmp-mod-776)))))
                                       (begin
                                         (if (eq? syntmp-m-754 (quote c&e))
                                           (syntmp-top-level-eval-hook-91
                                             syntmp-x-809
                                             syntmp-mod-776))
                                         syntmp-x-809))
                                     (if (memv syntmp-t-808
                                               '(displaced-lexical))
                                       (syntax-error
                                         (syntmp-wrap-147
                                           syntmp-value-772
                                           syntmp-w-774
                                           syntmp-mod-776)
                                         "identifier out of context")
                                       (if (eq? syntmp-type-807
                                                'external-macro)
                                         (let ((syntmp-x-810
                                                 (syntmp-build-annotated-96
                                                   syntmp-s-775
                                                   (list 'define
                                                         syntmp-n-806
                                                         (syntmp-chi-155
                                                           syntmp-e-773
                                                           syntmp-r-752
                                                           syntmp-w-774
                                                           syntmp-mod-776)))))
                                           (begin
                                             (if (eq? syntmp-m-754 (quote c&e))
                                               (syntmp-top-level-eval-hook-91
                                                 syntmp-x-810
                                                 syntmp-mod-776))
                                             syntmp-x-810))
                                         (syntax-error
                                           (syntmp-wrap-147
                                             syntmp-value-772
                                             syntmp-w-774
                                             syntmp-mod-776)
                                           "cannot define keyword at top level")))))))
                             (let ((syntmp-x-811
                                     (syntmp-chi-expr-156
                                       syntmp-type-771
                                       syntmp-value-772
                                       syntmp-e-773
                                       syntmp-r-752
                                       syntmp-w-774
                                       syntmp-s-775
                                       syntmp-mod-776)))
                               (begin
                                 (if (eq? syntmp-m-754 (quote c&e))
                                   (syntmp-top-level-eval-hook-91
                                     syntmp-x-811
                                     syntmp-mod-776))
                                 syntmp-x-811))))))))))))
         (syntmp-syntax-type-153
           (lambda (syntmp-e-812
                    syntmp-r-813
                    syntmp-w-814
                    syntmp-s-815
                    syntmp-rib-816
                    syntmp-mod-817)
             (cond ((symbol? syntmp-e-812)
                    (let ((syntmp-n-818
                            (syntmp-id-var-name-141
                              syntmp-e-812
                              syntmp-w-814)))
                      (let ((syntmp-b-819
                              (syntmp-lookup-116
                                syntmp-n-818
                                syntmp-r-813
                                syntmp-mod-817)))
                        (let ((syntmp-type-820
                                (syntmp-binding-type-111 syntmp-b-819)))
                          (let ((syntmp-t-821 syntmp-type-820))
                            (if (memv syntmp-t-821 (quote (lexical)))
                              (values
                                syntmp-type-820
                                (syntmp-binding-value-112 syntmp-b-819)
                                syntmp-e-812
                                syntmp-w-814
                                syntmp-s-815
                                syntmp-mod-817)
                              (if (memv syntmp-t-821 (quote (global)))
                                (values
                                  syntmp-type-820
                                  syntmp-n-818
                                  syntmp-e-812
                                  syntmp-w-814
                                  syntmp-s-815
                                  syntmp-mod-817)
                                (if (memv syntmp-t-821 (quote (macro)))
                                  (syntmp-syntax-type-153
                                    (syntmp-chi-macro-158
                                      (syntmp-binding-value-112 syntmp-b-819)
                                      syntmp-e-812
                                      syntmp-r-813
                                      syntmp-w-814
                                      syntmp-rib-816
                                      syntmp-mod-817)
                                    syntmp-r-813
                                    '(())
                                    syntmp-s-815
                                    syntmp-rib-816
                                    syntmp-mod-817)
                                  (values
                                    syntmp-type-820
                                    (syntmp-binding-value-112 syntmp-b-819)
                                    syntmp-e-812
                                    syntmp-w-814
                                    syntmp-s-815
                                    syntmp-mod-817)))))))))
                   ((pair? syntmp-e-812)
                    (let ((syntmp-first-822 (car syntmp-e-812)))
                      (if (syntmp-id?-119 syntmp-first-822)
                        (let ((syntmp-n-823
                                (syntmp-id-var-name-141
                                  syntmp-first-822
                                  syntmp-w-814)))
                          (let ((syntmp-b-824
                                  (syntmp-lookup-116
                                    syntmp-n-823
                                    syntmp-r-813
                                    syntmp-mod-817)))
                            (let ((syntmp-type-825
                                    (syntmp-binding-type-111 syntmp-b-824)))
                              (let ((syntmp-t-826 syntmp-type-825))
                                (if (memv syntmp-t-826 (quote (lexical)))
                                  (values
                                    'lexical-call
                                    (syntmp-binding-value-112 syntmp-b-824)
                                    syntmp-e-812
                                    syntmp-w-814
                                    syntmp-s-815
                                    syntmp-mod-817)
                                  (if (memv syntmp-t-826 (quote (global)))
                                    (values
                                      'global-call
                                      syntmp-n-823
                                      syntmp-e-812
                                      syntmp-w-814
                                      syntmp-s-815
                                      syntmp-mod-817)
                                    (if (memv syntmp-t-826 (quote (macro)))
                                      (syntmp-syntax-type-153
                                        (syntmp-chi-macro-158
                                          (syntmp-binding-value-112
                                            syntmp-b-824)
                                          syntmp-e-812
                                          syntmp-r-813
                                          syntmp-w-814
                                          syntmp-rib-816
                                          syntmp-mod-817)
                                        syntmp-r-813
                                        '(())
                                        syntmp-s-815
                                        syntmp-rib-816
                                        syntmp-mod-817)
                                      (if (memv syntmp-t-826
                                                '(core external-macro))
                                        (values
                                          syntmp-type-825
                                          (syntmp-binding-value-112
                                            syntmp-b-824)
                                          syntmp-e-812
                                          syntmp-w-814
                                          syntmp-s-815
                                          syntmp-mod-817)
                                        (if (memv syntmp-t-826
                                                  '(local-syntax))
                                          (values
                                            'local-syntax-form
                                            (syntmp-binding-value-112
                                              syntmp-b-824)
                                            syntmp-e-812
                                            syntmp-w-814
                                            syntmp-s-815
                                            syntmp-mod-817)
                                          (if (memv syntmp-t-826
                                                    '(begin))
                                            (values
                                              'begin-form
                                              #f
                                              syntmp-e-812
                                              syntmp-w-814
                                              syntmp-s-815
                                              syntmp-mod-817)
                                            (if (memv syntmp-t-826
                                                      '(eval-when))
                                              (values
                                                'eval-when-form
                                                #f
                                                syntmp-e-812
                                                syntmp-w-814
                                                syntmp-s-815
                                                syntmp-mod-817)
                                              (if (memv syntmp-t-826
                                                        '(define))
                                                ((lambda (syntmp-tmp-827)
                                                   ((lambda (syntmp-tmp-828)
                                                      (if (if syntmp-tmp-828
                                                            (apply (lambda (syntmp-_-829
                                                                            syntmp-name-830
                                                                            syntmp-val-831)
                                                                     (syntmp-id?-119
                                                                       syntmp-name-830))
                                                                   syntmp-tmp-828)
                                                            #f)
                                                        (apply (lambda (syntmp-_-832
                                                                        syntmp-name-833
                                                                        syntmp-val-834)
                                                                 (values
                                                                   'define-form
                                                                   syntmp-name-833
                                                                   syntmp-val-834
                                                                   syntmp-w-814
                                                                   syntmp-s-815
                                                                   syntmp-mod-817))
                                                               syntmp-tmp-828)
                                                        ((lambda (syntmp-tmp-835)
                                                           (if (if syntmp-tmp-835
                                                                 (apply (lambda (syntmp-_-836
                                                                                 syntmp-name-837
                                                                                 syntmp-args-838
                                                                                 syntmp-e1-839
                                                                                 syntmp-e2-840)
                                                                          (and (syntmp-id?-119
                                                                                 syntmp-name-837)
                                                                               (syntmp-valid-bound-ids?-144
                                                                                 (syntmp-lambda-var-list-168
                                                                                   syntmp-args-838))))
                                                                        syntmp-tmp-835)
                                                                 #f)
                                                             (apply (lambda (syntmp-_-841
                                                                             syntmp-name-842
                                                                             syntmp-args-843
                                                                             syntmp-e1-844
                                                                             syntmp-e2-845)
                                                                      (values
                                                                        'define-form
                                                                        (syntmp-wrap-147
                                                                          syntmp-name-842
                                                                          syntmp-w-814
                                                                          syntmp-mod-817)
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
                                                                                    #(t)
                                                                                    #(("m"
                                                                                       top))
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
                                                                                    #(type)
                                                                                    #((top))
                                                                                    #("i"))
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    #(b)
                                                                                    #((top))
                                                                                    #("i"))
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    #(n)
                                                                                    #((top))
                                                                                    #("i"))
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
                                                                                    #(e
                                                                                      r
                                                                                      w
                                                                                      s
                                                                                      rib
                                                                                      mod)
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
                                                                                    (lambda-var-list
                                                                                      gen-var
                                                                                      strip
                                                                                      strip-annotation
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
                                                                                      unannotate
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
                                                                                      build-global-assignment
                                                                                      build-global-reference
                                                                                      build-lexical-assignment
                                                                                      build-lexical-reference
                                                                                      build-conditional
                                                                                      build-application
                                                                                      build-annotated
                                                                                      get-global-definition-hook
                                                                                      put-global-definition-hook
                                                                                      gensym-hook
                                                                                      error-hook
                                                                                      local-eval-hook
                                                                                      top-level-eval-hook
                                                                                      fx<
                                                                                      fx=
                                                                                      fx-
                                                                                      fx+
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
                                                                                     "i"))
                                                                                  #(ribcage
                                                                                    (define-structure)
                                                                                    ((top))
                                                                                    ("i")))
                                                                                 (ice-9 syncase))
                                                                              (syntmp-wrap-147
                                                                                (cons syntmp-args-843
                                                                                      (cons syntmp-e1-844
                                                                                            syntmp-e2-845))
                                                                                syntmp-w-814
                                                                                syntmp-mod-817))
                                                                        '(())
                                                                        syntmp-s-815
                                                                        syntmp-mod-817))
                                                                    syntmp-tmp-835)
                                                             ((lambda (syntmp-tmp-847)
                                                                (if (if syntmp-tmp-847
                                                                      (apply (lambda (syntmp-_-848
                                                                                      syntmp-name-849)
                                                                               (syntmp-id?-119
                                                                                 syntmp-name-849))
                                                                             syntmp-tmp-847)
                                                                      #f)
                                                                  (apply (lambda (syntmp-_-850
                                                                                  syntmp-name-851)
                                                                           (values
                                                                             'define-form
                                                                             (syntmp-wrap-147
                                                                               syntmp-name-851
                                                                               syntmp-w-814
                                                                               syntmp-mod-817)
                                                                             '(#(syntax-object
                                                                                 void
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
                                                                                    #(t)
                                                                                    #(("m"
                                                                                       top))
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
                                                                                    #(type)
                                                                                    #((top))
                                                                                    #("i"))
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    #(b)
                                                                                    #((top))
                                                                                    #("i"))
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    #(n)
                                                                                    #((top))
                                                                                    #("i"))
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
                                                                                    #(e
                                                                                      r
                                                                                      w
                                                                                      s
                                                                                      rib
                                                                                      mod)
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
                                                                                    (lambda-var-list
                                                                                      gen-var
                                                                                      strip
                                                                                      strip-annotation
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
                                                                                      unannotate
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
                                                                                      build-global-assignment
                                                                                      build-global-reference
                                                                                      build-lexical-assignment
                                                                                      build-lexical-reference
                                                                                      build-conditional
                                                                                      build-application
                                                                                      build-annotated
                                                                                      get-global-definition-hook
                                                                                      put-global-definition-hook
                                                                                      gensym-hook
                                                                                      error-hook
                                                                                      local-eval-hook
                                                                                      top-level-eval-hook
                                                                                      fx<
                                                                                      fx=
                                                                                      fx-
                                                                                      fx+
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
                                                                                     "i"))
                                                                                  #(ribcage
                                                                                    (define-structure)
                                                                                    ((top))
                                                                                    ("i")))
                                                                                 (ice-9 syncase)))
                                                                             '(())
                                                                             syntmp-s-815
                                                                             syntmp-mod-817))
                                                                         syntmp-tmp-847)
                                                                  (syntax-error
                                                                    syntmp-tmp-827)))
                                                              (syntax-dispatch
                                                                syntmp-tmp-827
                                                                '(any any)))))
                                                         (syntax-dispatch
                                                           syntmp-tmp-827
                                                           '(any (any . any)
                                                                 any
                                                                 .
                                                                 each-any)))))
                                                    (syntax-dispatch
                                                      syntmp-tmp-827
                                                      '(any any any))))
                                                 syntmp-e-812)
                                                (if (memv syntmp-t-826
                                                          '(define-syntax))
                                                  ((lambda (syntmp-tmp-852)
                                                     ((lambda (syntmp-tmp-853)
                                                        (if (if syntmp-tmp-853
                                                              (apply (lambda (syntmp-_-854
                                                                              syntmp-name-855
                                                                              syntmp-val-856)
                                                                       (syntmp-id?-119
                                                                         syntmp-name-855))
                                                                     syntmp-tmp-853)
                                                              #f)
                                                          (apply (lambda (syntmp-_-857
                                                                          syntmp-name-858
                                                                          syntmp-val-859)
                                                                   (values
                                                                     'define-syntax-form
                                                                     syntmp-name-858
                                                                     syntmp-val-859
                                                                     syntmp-w-814
                                                                     syntmp-s-815
                                                                     syntmp-mod-817))
                                                                 syntmp-tmp-853)
                                                          (syntax-error
                                                            syntmp-tmp-852)))
                                                      (syntax-dispatch
                                                        syntmp-tmp-852
                                                        '(any any any))))
                                                   syntmp-e-812)
                                                  (values
                                                    'call
                                                    #f
                                                    syntmp-e-812
                                                    syntmp-w-814
                                                    syntmp-s-815
                                                    syntmp-mod-817))))))))))))))
                        (values
                          'call
                          #f
                          syntmp-e-812
                          syntmp-w-814
                          syntmp-s-815
                          syntmp-mod-817))))
                   ((syntmp-syntax-object?-103 syntmp-e-812)
                    (syntmp-syntax-type-153
                      (syntmp-syntax-object-expression-104
                        syntmp-e-812)
                      syntmp-r-813
                      (syntmp-join-wraps-138
                        syntmp-w-814
                        (syntmp-syntax-object-wrap-105 syntmp-e-812))
                      #f
                      syntmp-rib-816
                      (or (syntmp-syntax-object-module-106 syntmp-e-812)
                          syntmp-mod-817)))
                   ((annotation? syntmp-e-812)
                    (syntmp-syntax-type-153
                      (annotation-expression syntmp-e-812)
                      syntmp-r-813
                      syntmp-w-814
                      (annotation-source syntmp-e-812)
                      syntmp-rib-816
                      syntmp-mod-817))
                   ((self-evaluating? syntmp-e-812)
                    (values
                      'constant
                      #f
                      syntmp-e-812
                      syntmp-w-814
                      syntmp-s-815
                      syntmp-mod-817))
                   (else
                    (values
                      'other
                      #f
                      syntmp-e-812
                      syntmp-w-814
                      syntmp-s-815
                      syntmp-mod-817)))))
         (syntmp-chi-when-list-152
           (lambda (syntmp-e-860 syntmp-when-list-861 syntmp-w-862)
             (let syntmp-f-863 ((syntmp-when-list-864 syntmp-when-list-861)
                                (syntmp-situations-865 (quote ())))
               (if (null? syntmp-when-list-864)
                 syntmp-situations-865
                 (syntmp-f-863
                   (cdr syntmp-when-list-864)
                   (cons (let ((syntmp-x-866 (car syntmp-when-list-864)))
                           (cond ((syntmp-free-id=?-142
                                    syntmp-x-866
                                    '#(syntax-object
                                       compile
                                       ((top)
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i"))
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(f when-list situations)
                                          #((top) (top) (top))
                                          #("i" "i" "i"))
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(e when-list w)
                                          #((top) (top) (top))
                                          #("i" "i" "i"))
                                        #(ribcage
                                          (lambda-var-list
                                            gen-var
                                            strip
                                            strip-annotation
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
                                            unannotate
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
                                            build-global-assignment
                                            build-global-reference
                                            build-lexical-assignment
                                            build-lexical-reference
                                            build-conditional
                                            build-application
                                            build-annotated
                                            get-global-definition-hook
                                            put-global-definition-hook
                                            gensym-hook
                                            error-hook
                                            local-eval-hook
                                            top-level-eval-hook
                                            fx<
                                            fx=
                                            fx-
                                            fx+
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
                                           "i"))
                                        #(ribcage
                                          (define-structure)
                                          ((top))
                                          ("i")))
                                       (ice-9 syncase)))
                                  'compile)
                                 ((syntmp-free-id=?-142
                                    syntmp-x-866
                                    '#(syntax-object
                                       load
                                       ((top)
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i"))
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(f when-list situations)
                                          #((top) (top) (top))
                                          #("i" "i" "i"))
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(e when-list w)
                                          #((top) (top) (top))
                                          #("i" "i" "i"))
                                        #(ribcage
                                          (lambda-var-list
                                            gen-var
                                            strip
                                            strip-annotation
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
                                            unannotate
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
                                            build-global-assignment
                                            build-global-reference
                                            build-lexical-assignment
                                            build-lexical-reference
                                            build-conditional
                                            build-application
                                            build-annotated
                                            get-global-definition-hook
                                            put-global-definition-hook
                                            gensym-hook
                                            error-hook
                                            local-eval-hook
                                            top-level-eval-hook
                                            fx<
                                            fx=
                                            fx-
                                            fx+
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
                                           "i"))
                                        #(ribcage
                                          (define-structure)
                                          ((top))
                                          ("i")))
                                       (ice-9 syncase)))
                                  'load)
                                 ((syntmp-free-id=?-142
                                    syntmp-x-866
                                    '#(syntax-object
                                       eval
                                       ((top)
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i"))
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(f when-list situations)
                                          #((top) (top) (top))
                                          #("i" "i" "i"))
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(e when-list w)
                                          #((top) (top) (top))
                                          #("i" "i" "i"))
                                        #(ribcage
                                          (lambda-var-list
                                            gen-var
                                            strip
                                            strip-annotation
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
                                            unannotate
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
                                            build-global-assignment
                                            build-global-reference
                                            build-lexical-assignment
                                            build-lexical-reference
                                            build-conditional
                                            build-application
                                            build-annotated
                                            get-global-definition-hook
                                            put-global-definition-hook
                                            gensym-hook
                                            error-hook
                                            local-eval-hook
                                            top-level-eval-hook
                                            fx<
                                            fx=
                                            fx-
                                            fx+
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
                                           "i"))
                                        #(ribcage
                                          (define-structure)
                                          ((top))
                                          ("i")))
                                       (ice-9 syncase)))
                                  'eval)
                                 (else
                                  (syntax-error
                                    (syntmp-wrap-147
                                      syntmp-x-866
                                      syntmp-w-862
                                      #f)
                                    "invalid eval-when situation"))))
                         syntmp-situations-865))))))
         (syntmp-chi-install-global-151
           (lambda (syntmp-name-877 syntmp-e-878)
             (syntmp-build-annotated-96
               #f
               (list (syntmp-build-annotated-96
                       #f
                       'install-global-transformer)
                     (syntmp-build-data-97 #f syntmp-name-877)
                     syntmp-e-878))))
         (syntmp-chi-top-sequence-150
           (lambda (syntmp-body-879
                    syntmp-r-880
                    syntmp-w-881
                    syntmp-s-882
                    syntmp-m-883
                    syntmp-esew-884
                    syntmp-mod-885)
             (syntmp-build-sequence-98
               syntmp-s-882
               (let syntmp-dobody-886 ((syntmp-body-887 syntmp-body-879)
                                       (syntmp-r-888 syntmp-r-880)
                                       (syntmp-w-889 syntmp-w-881)
                                       (syntmp-m-890 syntmp-m-883)
                                       (syntmp-esew-891 syntmp-esew-884)
                                       (syntmp-mod-892 syntmp-mod-885))
                 (if (null? syntmp-body-887)
                   '()
                   (let ((syntmp-first-893
                           (syntmp-chi-top-154
                             (car syntmp-body-887)
                             syntmp-r-888
                             syntmp-w-889
                             syntmp-m-890
                             syntmp-esew-891
                             syntmp-mod-892)))
                     (cons syntmp-first-893
                           (syntmp-dobody-886
                             (cdr syntmp-body-887)
                             syntmp-r-888
                             syntmp-w-889
                             syntmp-m-890
                             syntmp-esew-891
                             syntmp-mod-892))))))))
         (syntmp-chi-sequence-149
           (lambda (syntmp-body-894
                    syntmp-r-895
                    syntmp-w-896
                    syntmp-s-897
                    syntmp-mod-898)
             (syntmp-build-sequence-98
               syntmp-s-897
               (let syntmp-dobody-899 ((syntmp-body-900 syntmp-body-894)
                                       (syntmp-r-901 syntmp-r-895)
                                       (syntmp-w-902 syntmp-w-896)
                                       (syntmp-mod-903 syntmp-mod-898))
                 (if (null? syntmp-body-900)
                   '()
                   (let ((syntmp-first-904
                           (syntmp-chi-155
                             (car syntmp-body-900)
                             syntmp-r-901
                             syntmp-w-902
                             syntmp-mod-903)))
                     (cons syntmp-first-904
                           (syntmp-dobody-899
                             (cdr syntmp-body-900)
                             syntmp-r-901
                             syntmp-w-902
                             syntmp-mod-903))))))))
         (syntmp-source-wrap-148
           (lambda (syntmp-x-905
                    syntmp-w-906
                    syntmp-s-907
                    syntmp-defmod-908)
             (syntmp-wrap-147
               (if syntmp-s-907
                 (make-annotation syntmp-x-905 syntmp-s-907 #f)
                 syntmp-x-905)
               syntmp-w-906
               syntmp-defmod-908)))
         (syntmp-wrap-147
           (lambda (syntmp-x-909 syntmp-w-910 syntmp-defmod-911)
             (cond ((and (null? (syntmp-wrap-marks-122 syntmp-w-910))
                         (null? (syntmp-wrap-subst-123 syntmp-w-910)))
                    syntmp-x-909)
                   ((syntmp-syntax-object?-103 syntmp-x-909)
                    (syntmp-make-syntax-object-102
                      (syntmp-syntax-object-expression-104
                        syntmp-x-909)
                      (syntmp-join-wraps-138
                        syntmp-w-910
                        (syntmp-syntax-object-wrap-105 syntmp-x-909))
                      (syntmp-syntax-object-module-106 syntmp-x-909)))
                   ((null? syntmp-x-909) syntmp-x-909)
                   (else
                    (syntmp-make-syntax-object-102
                      syntmp-x-909
                      syntmp-w-910
                      syntmp-defmod-911)))))
         (syntmp-bound-id-member?-146
           (lambda (syntmp-x-912 syntmp-list-913)
             (and (not (null? syntmp-list-913))
                  (or (syntmp-bound-id=?-143
                        syntmp-x-912
                        (car syntmp-list-913))
                      (syntmp-bound-id-member?-146
                        syntmp-x-912
                        (cdr syntmp-list-913))))))
         (syntmp-distinct-bound-ids?-145
           (lambda (syntmp-ids-914)
             (let syntmp-distinct?-915 ((syntmp-ids-916 syntmp-ids-914))
               (or (null? syntmp-ids-916)
                   (and (not (syntmp-bound-id-member?-146
                               (car syntmp-ids-916)
                               (cdr syntmp-ids-916)))
                        (syntmp-distinct?-915 (cdr syntmp-ids-916)))))))
         (syntmp-valid-bound-ids?-144
           (lambda (syntmp-ids-917)
             (and (let syntmp-all-ids?-918 ((syntmp-ids-919 syntmp-ids-917))
                    (or (null? syntmp-ids-919)
                        (and (syntmp-id?-119 (car syntmp-ids-919))
                             (syntmp-all-ids?-918 (cdr syntmp-ids-919)))))
                  (syntmp-distinct-bound-ids?-145 syntmp-ids-917))))
         (syntmp-bound-id=?-143
           (lambda (syntmp-i-920 syntmp-j-921)
             (if (and (syntmp-syntax-object?-103 syntmp-i-920)
                      (syntmp-syntax-object?-103 syntmp-j-921))
               (and (eq? (let ((syntmp-e-922
                                 (syntmp-syntax-object-expression-104
                                   syntmp-i-920)))
                           (if (annotation? syntmp-e-922)
                             (annotation-expression syntmp-e-922)
                             syntmp-e-922))
                         (let ((syntmp-e-923
                                 (syntmp-syntax-object-expression-104
                                   syntmp-j-921)))
                           (if (annotation? syntmp-e-923)
                             (annotation-expression syntmp-e-923)
                             syntmp-e-923)))
                    (syntmp-same-marks?-140
                      (syntmp-wrap-marks-122
                        (syntmp-syntax-object-wrap-105 syntmp-i-920))
                      (syntmp-wrap-marks-122
                        (syntmp-syntax-object-wrap-105 syntmp-j-921))))
               (eq? (let ((syntmp-e-924 syntmp-i-920))
                      (if (annotation? syntmp-e-924)
                        (annotation-expression syntmp-e-924)
                        syntmp-e-924))
                    (let ((syntmp-e-925 syntmp-j-921))
                      (if (annotation? syntmp-e-925)
                        (annotation-expression syntmp-e-925)
                        syntmp-e-925))))))
         (syntmp-free-id=?-142
           (lambda (syntmp-i-926 syntmp-j-927)
             (and (eq? (let ((syntmp-x-928 syntmp-i-926))
                         (let ((syntmp-e-929
                                 (if (syntmp-syntax-object?-103 syntmp-x-928)
                                   (syntmp-syntax-object-expression-104
                                     syntmp-x-928)
                                   syntmp-x-928)))
                           (if (annotation? syntmp-e-929)
                             (annotation-expression syntmp-e-929)
                             syntmp-e-929)))
                       (let ((syntmp-x-930 syntmp-j-927))
                         (let ((syntmp-e-931
                                 (if (syntmp-syntax-object?-103 syntmp-x-930)
                                   (syntmp-syntax-object-expression-104
                                     syntmp-x-930)
                                   syntmp-x-930)))
                           (if (annotation? syntmp-e-931)
                             (annotation-expression syntmp-e-931)
                             syntmp-e-931))))
                  (eq? (syntmp-id-var-name-141
                         syntmp-i-926
                         '(()))
                       (syntmp-id-var-name-141
                         syntmp-j-927
                         '(()))))))
         (syntmp-id-var-name-141
           (lambda (syntmp-id-932 syntmp-w-933)
             (letrec ((syntmp-search-vector-rib-936
                        (lambda (syntmp-sym-947
                                 syntmp-subst-948
                                 syntmp-marks-949
                                 syntmp-symnames-950
                                 syntmp-ribcage-951)
                          (let ((syntmp-n-952
                                  (vector-length syntmp-symnames-950)))
                            (let syntmp-f-953 ((syntmp-i-954 0))
                              (cond ((syntmp-fx=-89 syntmp-i-954 syntmp-n-952)
                                     (syntmp-search-934
                                       syntmp-sym-947
                                       (cdr syntmp-subst-948)
                                       syntmp-marks-949))
                                    ((and (eq? (vector-ref
                                                 syntmp-symnames-950
                                                 syntmp-i-954)
                                               syntmp-sym-947)
                                          (syntmp-same-marks?-140
                                            syntmp-marks-949
                                            (vector-ref
                                              (syntmp-ribcage-marks-129
                                                syntmp-ribcage-951)
                                              syntmp-i-954)))
                                     (values
                                       (vector-ref
                                         (syntmp-ribcage-labels-130
                                           syntmp-ribcage-951)
                                         syntmp-i-954)
                                       syntmp-marks-949))
                                    (else
                                     (syntmp-f-953
                                       (syntmp-fx+-87 syntmp-i-954 1))))))))
                      (syntmp-search-list-rib-935
                        (lambda (syntmp-sym-955
                                 syntmp-subst-956
                                 syntmp-marks-957
                                 syntmp-symnames-958
                                 syntmp-ribcage-959)
                          (let syntmp-f-960 ((syntmp-symnames-961
                                               syntmp-symnames-958)
                                             (syntmp-i-962 0))
                            (cond ((null? syntmp-symnames-961)
                                   (syntmp-search-934
                                     syntmp-sym-955
                                     (cdr syntmp-subst-956)
                                     syntmp-marks-957))
                                  ((and (eq? (car syntmp-symnames-961)
                                             syntmp-sym-955)
                                        (syntmp-same-marks?-140
                                          syntmp-marks-957
                                          (list-ref
                                            (syntmp-ribcage-marks-129
                                              syntmp-ribcage-959)
                                            syntmp-i-962)))
                                   (values
                                     (list-ref
                                       (syntmp-ribcage-labels-130
                                         syntmp-ribcage-959)
                                       syntmp-i-962)
                                     syntmp-marks-957))
                                  (else
                                   (syntmp-f-960
                                     (cdr syntmp-symnames-961)
                                     (syntmp-fx+-87 syntmp-i-962 1)))))))
                      (syntmp-search-934
                        (lambda (syntmp-sym-963
                                 syntmp-subst-964
                                 syntmp-marks-965)
                          (if (null? syntmp-subst-964)
                            (values #f syntmp-marks-965)
                            (let ((syntmp-fst-966 (car syntmp-subst-964)))
                              (if (eq? syntmp-fst-966 (quote shift))
                                (syntmp-search-934
                                  syntmp-sym-963
                                  (cdr syntmp-subst-964)
                                  (cdr syntmp-marks-965))
                                (let ((syntmp-symnames-967
                                        (syntmp-ribcage-symnames-128
                                          syntmp-fst-966)))
                                  (if (vector? syntmp-symnames-967)
                                    (syntmp-search-vector-rib-936
                                      syntmp-sym-963
                                      syntmp-subst-964
                                      syntmp-marks-965
                                      syntmp-symnames-967
                                      syntmp-fst-966)
                                    (syntmp-search-list-rib-935
                                      syntmp-sym-963
                                      syntmp-subst-964
                                      syntmp-marks-965
                                      syntmp-symnames-967
                                      syntmp-fst-966)))))))))
               (cond ((symbol? syntmp-id-932)
                      (or (call-with-values
                            (lambda ()
                              (syntmp-search-934
                                syntmp-id-932
                                (syntmp-wrap-subst-123 syntmp-w-933)
                                (syntmp-wrap-marks-122 syntmp-w-933)))
                            (lambda (syntmp-x-969 . syntmp-ignore-968)
                              syntmp-x-969))
                          syntmp-id-932))
                     ((syntmp-syntax-object?-103 syntmp-id-932)
                      (let ((syntmp-id-970
                              (let ((syntmp-e-972
                                      (syntmp-syntax-object-expression-104
                                        syntmp-id-932)))
                                (if (annotation? syntmp-e-972)
                                  (annotation-expression syntmp-e-972)
                                  syntmp-e-972)))
                            (syntmp-w1-971
                              (syntmp-syntax-object-wrap-105 syntmp-id-932)))
                        (let ((syntmp-marks-973
                                (syntmp-join-marks-139
                                  (syntmp-wrap-marks-122 syntmp-w-933)
                                  (syntmp-wrap-marks-122 syntmp-w1-971))))
                          (call-with-values
                            (lambda ()
                              (syntmp-search-934
                                syntmp-id-970
                                (syntmp-wrap-subst-123 syntmp-w-933)
                                syntmp-marks-973))
                            (lambda (syntmp-new-id-974 syntmp-marks-975)
                              (or syntmp-new-id-974
                                  (call-with-values
                                    (lambda ()
                                      (syntmp-search-934
                                        syntmp-id-970
                                        (syntmp-wrap-subst-123 syntmp-w1-971)
                                        syntmp-marks-975))
                                    (lambda (syntmp-x-977 . syntmp-ignore-976)
                                      syntmp-x-977))
                                  syntmp-id-970))))))
                     ((annotation? syntmp-id-932)
                      (let ((syntmp-id-978
                              (let ((syntmp-e-979 syntmp-id-932))
                                (if (annotation? syntmp-e-979)
                                  (annotation-expression syntmp-e-979)
                                  syntmp-e-979))))
                        (or (call-with-values
                              (lambda ()
                                (syntmp-search-934
                                  syntmp-id-978
                                  (syntmp-wrap-subst-123 syntmp-w-933)
                                  (syntmp-wrap-marks-122 syntmp-w-933)))
                              (lambda (syntmp-x-981 . syntmp-ignore-980)
                                syntmp-x-981))
                            syntmp-id-978)))
                     (else
                      (syntmp-error-hook-93
                        'id-var-name
                        "invalid id"
                        syntmp-id-932))))))
         (syntmp-same-marks?-140
           (lambda (syntmp-x-982 syntmp-y-983)
             (or (eq? syntmp-x-982 syntmp-y-983)
                 (and (not (null? syntmp-x-982))
                      (not (null? syntmp-y-983))
                      (eq? (car syntmp-x-982) (car syntmp-y-983))
                      (syntmp-same-marks?-140
                        (cdr syntmp-x-982)
                        (cdr syntmp-y-983))))))
         (syntmp-join-marks-139
           (lambda (syntmp-m1-984 syntmp-m2-985)
             (syntmp-smart-append-137
               syntmp-m1-984
               syntmp-m2-985)))
         (syntmp-join-wraps-138
           (lambda (syntmp-w1-986 syntmp-w2-987)
             (let ((syntmp-m1-988
                     (syntmp-wrap-marks-122 syntmp-w1-986))
                   (syntmp-s1-989
                     (syntmp-wrap-subst-123 syntmp-w1-986)))
               (if (null? syntmp-m1-988)
                 (if (null? syntmp-s1-989)
                   syntmp-w2-987
                   (syntmp-make-wrap-121
                     (syntmp-wrap-marks-122 syntmp-w2-987)
                     (syntmp-smart-append-137
                       syntmp-s1-989
                       (syntmp-wrap-subst-123 syntmp-w2-987))))
                 (syntmp-make-wrap-121
                   (syntmp-smart-append-137
                     syntmp-m1-988
                     (syntmp-wrap-marks-122 syntmp-w2-987))
                   (syntmp-smart-append-137
                     syntmp-s1-989
                     (syntmp-wrap-subst-123 syntmp-w2-987)))))))
         (syntmp-smart-append-137
           (lambda (syntmp-m1-990 syntmp-m2-991)
             (if (null? syntmp-m2-991)
               syntmp-m1-990
               (append syntmp-m1-990 syntmp-m2-991))))
         (syntmp-make-binding-wrap-136
           (lambda (syntmp-ids-992 syntmp-labels-993 syntmp-w-994)
             (if (null? syntmp-ids-992)
               syntmp-w-994
               (syntmp-make-wrap-121
                 (syntmp-wrap-marks-122 syntmp-w-994)
                 (cons (let ((syntmp-labelvec-995
                               (list->vector syntmp-labels-993)))
                         (let ((syntmp-n-996
                                 (vector-length syntmp-labelvec-995)))
                           (let ((syntmp-symnamevec-997
                                   (make-vector syntmp-n-996))
                                 (syntmp-marksvec-998
                                   (make-vector syntmp-n-996)))
                             (begin
                               (let syntmp-f-999 ((syntmp-ids-1000
                                                    syntmp-ids-992)
                                                  (syntmp-i-1001 0))
                                 (if (not (null? syntmp-ids-1000))
                                   (call-with-values
                                     (lambda ()
                                       (syntmp-id-sym-name&marks-120
                                         (car syntmp-ids-1000)
                                         syntmp-w-994))
                                     (lambda (syntmp-symname-1002
                                              syntmp-marks-1003)
                                       (begin
                                         (vector-set!
                                           syntmp-symnamevec-997
                                           syntmp-i-1001
                                           syntmp-symname-1002)
                                         (vector-set!
                                           syntmp-marksvec-998
                                           syntmp-i-1001
                                           syntmp-marks-1003)
                                         (syntmp-f-999
                                           (cdr syntmp-ids-1000)
                                           (syntmp-fx+-87
                                             syntmp-i-1001
                                             1)))))))
                               (syntmp-make-ribcage-126
                                 syntmp-symnamevec-997
                                 syntmp-marksvec-998
                                 syntmp-labelvec-995)))))
                       (syntmp-wrap-subst-123 syntmp-w-994))))))
         (syntmp-extend-ribcage!-135
           (lambda (syntmp-ribcage-1004
                    syntmp-id-1005
                    syntmp-label-1006)
             (begin
               (syntmp-set-ribcage-symnames!-131
                 syntmp-ribcage-1004
                 (cons (let ((syntmp-e-1007
                               (syntmp-syntax-object-expression-104
                                 syntmp-id-1005)))
                         (if (annotation? syntmp-e-1007)
                           (annotation-expression syntmp-e-1007)
                           syntmp-e-1007))
                       (syntmp-ribcage-symnames-128 syntmp-ribcage-1004)))
               (syntmp-set-ribcage-marks!-132
                 syntmp-ribcage-1004
                 (cons (syntmp-wrap-marks-122
                         (syntmp-syntax-object-wrap-105 syntmp-id-1005))
                       (syntmp-ribcage-marks-129 syntmp-ribcage-1004)))
               (syntmp-set-ribcage-labels!-133
                 syntmp-ribcage-1004
                 (cons syntmp-label-1006
                       (syntmp-ribcage-labels-130 syntmp-ribcage-1004))))))
         (syntmp-anti-mark-134
           (lambda (syntmp-w-1008)
             (syntmp-make-wrap-121
               (cons #f (syntmp-wrap-marks-122 syntmp-w-1008))
               (cons 'shift
                     (syntmp-wrap-subst-123 syntmp-w-1008)))))
         (syntmp-set-ribcage-labels!-133
           (lambda (syntmp-x-1009 syntmp-update-1010)
             (vector-set! syntmp-x-1009 3 syntmp-update-1010)))
         (syntmp-set-ribcage-marks!-132
           (lambda (syntmp-x-1011 syntmp-update-1012)
             (vector-set! syntmp-x-1011 2 syntmp-update-1012)))
         (syntmp-set-ribcage-symnames!-131
           (lambda (syntmp-x-1013 syntmp-update-1014)
             (vector-set! syntmp-x-1013 1 syntmp-update-1014)))
         (syntmp-ribcage-labels-130
           (lambda (syntmp-x-1015)
             (vector-ref syntmp-x-1015 3)))
         (syntmp-ribcage-marks-129
           (lambda (syntmp-x-1016)
             (vector-ref syntmp-x-1016 2)))
         (syntmp-ribcage-symnames-128
           (lambda (syntmp-x-1017)
             (vector-ref syntmp-x-1017 1)))
         (syntmp-ribcage?-127
           (lambda (syntmp-x-1018)
             (and (vector? syntmp-x-1018)
                  (= (vector-length syntmp-x-1018) 4)
                  (eq? (vector-ref syntmp-x-1018 0)
                       'ribcage))))
         (syntmp-make-ribcage-126
           (lambda (syntmp-symnames-1019
                    syntmp-marks-1020
                    syntmp-labels-1021)
             (vector
               'ribcage
               syntmp-symnames-1019
               syntmp-marks-1020
               syntmp-labels-1021)))
         (syntmp-gen-labels-125
           (lambda (syntmp-ls-1022)
             (if (null? syntmp-ls-1022)
               '()
               (cons (syntmp-gen-label-124)
                     (syntmp-gen-labels-125 (cdr syntmp-ls-1022))))))
         (syntmp-gen-label-124 (lambda () (string #\i)))
         (syntmp-wrap-subst-123 cdr)
         (syntmp-wrap-marks-122 car)
         (syntmp-make-wrap-121 cons)
         (syntmp-id-sym-name&marks-120
           (lambda (syntmp-x-1023 syntmp-w-1024)
             (if (syntmp-syntax-object?-103 syntmp-x-1023)
               (values
                 (let ((syntmp-e-1025
                         (syntmp-syntax-object-expression-104
                           syntmp-x-1023)))
                   (if (annotation? syntmp-e-1025)
                     (annotation-expression syntmp-e-1025)
                     syntmp-e-1025))
                 (syntmp-join-marks-139
                   (syntmp-wrap-marks-122 syntmp-w-1024)
                   (syntmp-wrap-marks-122
                     (syntmp-syntax-object-wrap-105 syntmp-x-1023))))
               (values
                 (let ((syntmp-e-1026 syntmp-x-1023))
                   (if (annotation? syntmp-e-1026)
                     (annotation-expression syntmp-e-1026)
                     syntmp-e-1026))
                 (syntmp-wrap-marks-122 syntmp-w-1024)))))
         (syntmp-id?-119
           (lambda (syntmp-x-1027)
             (cond ((symbol? syntmp-x-1027) #t)
                   ((syntmp-syntax-object?-103 syntmp-x-1027)
                    (symbol?
                      (let ((syntmp-e-1028
                              (syntmp-syntax-object-expression-104
                                syntmp-x-1027)))
                        (if (annotation? syntmp-e-1028)
                          (annotation-expression syntmp-e-1028)
                          syntmp-e-1028))))
                   ((annotation? syntmp-x-1027)
                    (symbol? (annotation-expression syntmp-x-1027)))
                   (else #f))))
         (syntmp-nonsymbol-id?-118
           (lambda (syntmp-x-1029)
             (and (syntmp-syntax-object?-103 syntmp-x-1029)
                  (symbol?
                    (let ((syntmp-e-1030
                            (syntmp-syntax-object-expression-104
                              syntmp-x-1029)))
                      (if (annotation? syntmp-e-1030)
                        (annotation-expression syntmp-e-1030)
                        syntmp-e-1030))))))
         (syntmp-global-extend-117
           (lambda (syntmp-type-1031
                    syntmp-sym-1032
                    syntmp-val-1033)
             (syntmp-put-global-definition-hook-94
               syntmp-sym-1032
               (cons syntmp-type-1031 syntmp-val-1033)
               (module-name (current-module)))))
         (syntmp-lookup-116
           (lambda (syntmp-x-1034 syntmp-r-1035 syntmp-mod-1036)
             (cond ((assq syntmp-x-1034 syntmp-r-1035) => cdr)
                   ((symbol? syntmp-x-1034)
                    (or (syntmp-get-global-definition-hook-95
                          syntmp-x-1034
                          syntmp-mod-1036)
                        '(global)))
                   (else (quote (displaced-lexical))))))
         (syntmp-macros-only-env-115
           (lambda (syntmp-r-1037)
             (if (null? syntmp-r-1037)
               '()
               (let ((syntmp-a-1038 (car syntmp-r-1037)))
                 (if (eq? (cadr syntmp-a-1038) (quote macro))
                   (cons syntmp-a-1038
                         (syntmp-macros-only-env-115 (cdr syntmp-r-1037)))
                   (syntmp-macros-only-env-115 (cdr syntmp-r-1037)))))))
         (syntmp-extend-var-env-114
           (lambda (syntmp-labels-1039
                    syntmp-vars-1040
                    syntmp-r-1041)
             (if (null? syntmp-labels-1039)
               syntmp-r-1041
               (syntmp-extend-var-env-114
                 (cdr syntmp-labels-1039)
                 (cdr syntmp-vars-1040)
                 (cons (cons (car syntmp-labels-1039)
                             (cons (quote lexical) (car syntmp-vars-1040)))
                       syntmp-r-1041)))))
         (syntmp-extend-env-113
           (lambda (syntmp-labels-1042
                    syntmp-bindings-1043
                    syntmp-r-1044)
             (if (null? syntmp-labels-1042)
               syntmp-r-1044
               (syntmp-extend-env-113
                 (cdr syntmp-labels-1042)
                 (cdr syntmp-bindings-1043)
                 (cons (cons (car syntmp-labels-1042)
                             (car syntmp-bindings-1043))
                       syntmp-r-1044)))))
         (syntmp-binding-value-112 cdr)
         (syntmp-binding-type-111 car)
         (syntmp-source-annotation-110
           (lambda (syntmp-x-1045)
             (cond ((annotation? syntmp-x-1045)
                    (annotation-source syntmp-x-1045))
                   ((syntmp-syntax-object?-103 syntmp-x-1045)
                    (syntmp-source-annotation-110
                      (syntmp-syntax-object-expression-104
                        syntmp-x-1045)))
                   (else #f))))
         (syntmp-set-syntax-object-module!-109
           (lambda (syntmp-x-1046 syntmp-update-1047)
             (vector-set! syntmp-x-1046 3 syntmp-update-1047)))
         (syntmp-set-syntax-object-wrap!-108
           (lambda (syntmp-x-1048 syntmp-update-1049)
             (vector-set! syntmp-x-1048 2 syntmp-update-1049)))
         (syntmp-set-syntax-object-expression!-107
           (lambda (syntmp-x-1050 syntmp-update-1051)
             (vector-set! syntmp-x-1050 1 syntmp-update-1051)))
         (syntmp-syntax-object-module-106
           (lambda (syntmp-x-1052)
             (vector-ref syntmp-x-1052 3)))
         (syntmp-syntax-object-wrap-105
           (lambda (syntmp-x-1053)
             (vector-ref syntmp-x-1053 2)))
         (syntmp-syntax-object-expression-104
           (lambda (syntmp-x-1054)
             (vector-ref syntmp-x-1054 1)))
         (syntmp-syntax-object?-103
           (lambda (syntmp-x-1055)
             (and (vector? syntmp-x-1055)
                  (= (vector-length syntmp-x-1055) 4)
                  (eq? (vector-ref syntmp-x-1055 0)
                       'syntax-object))))
         (syntmp-make-syntax-object-102
           (lambda (syntmp-expression-1056
                    syntmp-wrap-1057
                    syntmp-module-1058)
             (vector
               'syntax-object
               syntmp-expression-1056
               syntmp-wrap-1057
               syntmp-module-1058)))
         (syntmp-build-letrec-101
           (lambda (syntmp-src-1059
                    syntmp-vars-1060
                    syntmp-val-exps-1061
                    syntmp-body-exp-1062)
             (if (null? syntmp-vars-1060)
               (syntmp-build-annotated-96
                 syntmp-src-1059
                 syntmp-body-exp-1062)
               (syntmp-build-annotated-96
                 syntmp-src-1059
                 (list 'letrec
                       (map list syntmp-vars-1060 syntmp-val-exps-1061)
                       syntmp-body-exp-1062)))))
         (syntmp-build-named-let-100
           (lambda (syntmp-src-1063
                    syntmp-vars-1064
                    syntmp-val-exps-1065
                    syntmp-body-exp-1066)
             (if (null? syntmp-vars-1064)
               (syntmp-build-annotated-96
                 syntmp-src-1063
                 syntmp-body-exp-1066)
               (syntmp-build-annotated-96
                 syntmp-src-1063
                 (list 'let
                       (car syntmp-vars-1064)
                       (map list
                            (cdr syntmp-vars-1064)
                            syntmp-val-exps-1065)
                       syntmp-body-exp-1066)))))
         (syntmp-build-let-99
           (lambda (syntmp-src-1067
                    syntmp-vars-1068
                    syntmp-val-exps-1069
                    syntmp-body-exp-1070)
             (if (null? syntmp-vars-1068)
               (syntmp-build-annotated-96
                 syntmp-src-1067
                 syntmp-body-exp-1070)
               (syntmp-build-annotated-96
                 syntmp-src-1067
                 (list 'let
                       (map list syntmp-vars-1068 syntmp-val-exps-1069)
                       syntmp-body-exp-1070)))))
         (syntmp-build-sequence-98
           (lambda (syntmp-src-1071 syntmp-exps-1072)
             (if (null? (cdr syntmp-exps-1072))
               (syntmp-build-annotated-96
                 syntmp-src-1071
                 (car syntmp-exps-1072))
               (syntmp-build-annotated-96
                 syntmp-src-1071
                 (cons (quote begin) syntmp-exps-1072)))))
         (syntmp-build-data-97
           (lambda (syntmp-src-1073 syntmp-exp-1074)
             (if (and (self-evaluating? syntmp-exp-1074)
                      (not (vector? syntmp-exp-1074)))
               (syntmp-build-annotated-96
                 syntmp-src-1073
                 syntmp-exp-1074)
               (syntmp-build-annotated-96
                 syntmp-src-1073
                 (list (quote quote) syntmp-exp-1074)))))
         (syntmp-build-annotated-96
           (lambda (syntmp-src-1075 syntmp-exp-1076)
             (if (and syntmp-src-1075
                      (not (annotation? syntmp-exp-1076)))
               (make-annotation
                 syntmp-exp-1076
                 syntmp-src-1075
                 #t)
               syntmp-exp-1076)))
         (syntmp-get-global-definition-hook-95
           (lambda (syntmp-symbol-1077 syntmp-module-1078)
             (let ((syntmp-module-1079
                     (if syntmp-module-1078
                       (resolve-module syntmp-module-1078)
                       (warn "wha" syntmp-symbol-1077 (current-module)))))
               (let ((syntmp-v-1080
                       (module-variable
                         syntmp-module-1079
                         syntmp-symbol-1077)))
                 (and syntmp-v-1080
                      (or (object-property
                            syntmp-v-1080
                            '*sc-expander*)
                          (and (variable-bound? syntmp-v-1080)
                               (macro? (variable-ref syntmp-v-1080))
                               (macro-transformer (variable-ref syntmp-v-1080))
                               guile-macro)))))))
         (syntmp-put-global-definition-hook-94
           (lambda (syntmp-symbol-1081
                    syntmp-binding-1082
                    syntmp-module-1083)
             (let ((syntmp-module-1084
                     (if syntmp-module-1083
                       (resolve-module syntmp-module-1083)
                       (warn "wha" syntmp-symbol-1081 (current-module)))))
               (let ((syntmp-v-1085
                       (or (module-variable
                             syntmp-module-1084
                             syntmp-symbol-1081)
                           (let ((syntmp-v-1086 (make-variable sc-macro)))
                             (begin
                               (module-add!
                                 syntmp-module-1084
                                 syntmp-symbol-1081
                                 syntmp-v-1086)
                               syntmp-v-1086)))))
                 (begin
                   (if (not (and (symbol-property
                                   syntmp-symbol-1081
                                   'primitive-syntax)
                                 (eq? syntmp-module-1084 the-syncase-module)))
                     (variable-set! syntmp-v-1085 sc-macro))
                   (set-object-property!
                     syntmp-v-1085
                     '*sc-expander*
                     syntmp-binding-1082))))))
         (syntmp-error-hook-93
           (lambda (syntmp-who-1087
                    syntmp-why-1088
                    syntmp-what-1089)
             (error syntmp-who-1087
                    "~a ~s"
                    syntmp-why-1088
                    syntmp-what-1089)))
         (syntmp-local-eval-hook-92
           (lambda (syntmp-x-1090 syntmp-mod-1091)
             (eval (list syntmp-noexpand-86 syntmp-x-1090)
                   (if syntmp-mod-1091
                     (resolve-module syntmp-mod-1091)
                     (interaction-environment)))))
         (syntmp-top-level-eval-hook-91
           (lambda (syntmp-x-1092 syntmp-mod-1093)
             (eval (list syntmp-noexpand-86 syntmp-x-1092)
                   (if syntmp-mod-1093
                     (resolve-module syntmp-mod-1093)
                     (interaction-environment)))))
         (syntmp-fx<-90 <)
         (syntmp-fx=-89 =)
         (syntmp-fx--88 -)
         (syntmp-fx+-87 +)
         (syntmp-noexpand-86 "noexpand"))
  (begin
    (syntmp-global-extend-117
      'local-syntax
      'letrec-syntax
      #t)
    (syntmp-global-extend-117
      'local-syntax
      'let-syntax
      #f)
    (syntmp-global-extend-117
      'core
      'fluid-let-syntax
      (lambda (syntmp-e-1094
               syntmp-r-1095
               syntmp-w-1096
               syntmp-s-1097
               syntmp-mod-1098)
        ((lambda (syntmp-tmp-1099)
           ((lambda (syntmp-tmp-1100)
              (if (if syntmp-tmp-1100
                    (apply (lambda (syntmp-_-1101
                                    syntmp-var-1102
                                    syntmp-val-1103
                                    syntmp-e1-1104
                                    syntmp-e2-1105)
                             (syntmp-valid-bound-ids?-144 syntmp-var-1102))
                           syntmp-tmp-1100)
                    #f)
                (apply (lambda (syntmp-_-1107
                                syntmp-var-1108
                                syntmp-val-1109
                                syntmp-e1-1110
                                syntmp-e2-1111)
                         (let ((syntmp-names-1112
                                 (map (lambda (syntmp-x-1113)
                                        (syntmp-id-var-name-141
                                          syntmp-x-1113
                                          syntmp-w-1096))
                                      syntmp-var-1108)))
                           (begin
                             (for-each
                               (lambda (syntmp-id-1115 syntmp-n-1116)
                                 (let ((syntmp-t-1117
                                         (syntmp-binding-type-111
                                           (syntmp-lookup-116
                                             syntmp-n-1116
                                             syntmp-r-1095
                                             syntmp-mod-1098))))
                                   (if (memv syntmp-t-1117
                                             '(displaced-lexical))
                                     (syntax-error
                                       (syntmp-source-wrap-148
                                         syntmp-id-1115
                                         syntmp-w-1096
                                         syntmp-s-1097
                                         syntmp-mod-1098)
                                       "identifier out of context"))))
                               syntmp-var-1108
                               syntmp-names-1112)
                             (syntmp-chi-body-159
                               (cons syntmp-e1-1110 syntmp-e2-1111)
                               (syntmp-source-wrap-148
                                 syntmp-e-1094
                                 syntmp-w-1096
                                 syntmp-s-1097
                                 syntmp-mod-1098)
                               (syntmp-extend-env-113
                                 syntmp-names-1112
                                 (let ((syntmp-trans-r-1120
                                         (syntmp-macros-only-env-115
                                           syntmp-r-1095)))
                                   (map (lambda (syntmp-x-1121)
                                          (cons 'macro
                                                (syntmp-eval-local-transformer-162
                                                  (syntmp-chi-155
                                                    syntmp-x-1121
                                                    syntmp-trans-r-1120
                                                    syntmp-w-1096
                                                    syntmp-mod-1098)
                                                  syntmp-mod-1098)))
                                        syntmp-val-1109))
                                 syntmp-r-1095)
                               syntmp-w-1096
                               syntmp-mod-1098))))
                       syntmp-tmp-1100)
                ((lambda (syntmp-_-1123)
                   (syntax-error
                     (syntmp-source-wrap-148
                       syntmp-e-1094
                       syntmp-w-1096
                       syntmp-s-1097
                       syntmp-mod-1098)))
                 syntmp-tmp-1099)))
            (syntax-dispatch
              syntmp-tmp-1099
              '(any #(each (any any)) any . each-any))))
         syntmp-e-1094)))
    (syntmp-global-extend-117
      'core
      'quote
      (lambda (syntmp-e-1124
               syntmp-r-1125
               syntmp-w-1126
               syntmp-s-1127
               syntmp-mod-1128)
        ((lambda (syntmp-tmp-1129)
           ((lambda (syntmp-tmp-1130)
              (if syntmp-tmp-1130
                (apply (lambda (syntmp-_-1131 syntmp-e-1132)
                         (syntmp-build-data-97
                           syntmp-s-1127
                           (syntmp-strip-166 syntmp-e-1132 syntmp-w-1126)))
                       syntmp-tmp-1130)
                ((lambda (syntmp-_-1133)
                   (syntax-error
                     (syntmp-source-wrap-148
                       syntmp-e-1124
                       syntmp-w-1126
                       syntmp-s-1127
                       syntmp-mod-1128)))
                 syntmp-tmp-1129)))
            (syntax-dispatch
              syntmp-tmp-1129
              '(any any))))
         syntmp-e-1124)))
    (syntmp-global-extend-117
      'core
      'syntax
      (letrec ((syntmp-regen-1141
                 (lambda (syntmp-x-1142)
                   (let ((syntmp-t-1143 (car syntmp-x-1142)))
                     (if (memv syntmp-t-1143 (quote (ref)))
                       (syntmp-build-annotated-96
                         #f
                         (cadr syntmp-x-1142))
                       (if (memv syntmp-t-1143 (quote (primitive)))
                         (syntmp-build-annotated-96
                           #f
                           (cadr syntmp-x-1142))
                         (if (memv syntmp-t-1143 (quote (quote)))
                           (syntmp-build-data-97 #f (cadr syntmp-x-1142))
                           (if (memv syntmp-t-1143 (quote (lambda)))
                             (syntmp-build-annotated-96
                               #f
                               (list 'lambda
                                     (cadr syntmp-x-1142)
                                     (syntmp-regen-1141
                                       (caddr syntmp-x-1142))))
                             (if (memv syntmp-t-1143 (quote (map)))
                               (let ((syntmp-ls-1144
                                       (map syntmp-regen-1141
                                            (cdr syntmp-x-1142))))
                                 (syntmp-build-annotated-96
                                   #f
                                   (cons (if (syntmp-fx=-89
                                               (length syntmp-ls-1144)
                                               2)
                                           (syntmp-build-annotated-96
                                             #f
                                             'map)
                                           (syntmp-build-annotated-96
                                             #f
                                             'map))
                                         syntmp-ls-1144)))
                               (syntmp-build-annotated-96
                                 #f
                                 (cons (syntmp-build-annotated-96
                                         #f
                                         (car syntmp-x-1142))
                                       (map syntmp-regen-1141
                                            (cdr syntmp-x-1142))))))))))))
               (syntmp-gen-vector-1140
                 (lambda (syntmp-x-1145)
                   (cond ((eq? (car syntmp-x-1145) (quote list))
                          (cons (quote vector) (cdr syntmp-x-1145)))
                         ((eq? (car syntmp-x-1145) (quote quote))
                          (list 'quote
                                (list->vector (cadr syntmp-x-1145))))
                         (else (list (quote list->vector) syntmp-x-1145)))))
               (syntmp-gen-append-1139
                 (lambda (syntmp-x-1146 syntmp-y-1147)
                   (if (equal? syntmp-y-1147 (quote (quote ())))
                     syntmp-x-1146
                     (list (quote append) syntmp-x-1146 syntmp-y-1147))))
               (syntmp-gen-cons-1138
                 (lambda (syntmp-x-1148 syntmp-y-1149)
                   (let ((syntmp-t-1150 (car syntmp-y-1149)))
                     (if (memv syntmp-t-1150 (quote (quote)))
                       (if (eq? (car syntmp-x-1148) (quote quote))
                         (list 'quote
                               (cons (cadr syntmp-x-1148)
                                     (cadr syntmp-y-1149)))
                         (if (eq? (cadr syntmp-y-1149) (quote ()))
                           (list (quote list) syntmp-x-1148)
                           (list (quote cons) syntmp-x-1148 syntmp-y-1149)))
                       (if (memv syntmp-t-1150 (quote (list)))
                         (cons 'list
                               (cons syntmp-x-1148 (cdr syntmp-y-1149)))
                         (list (quote cons) syntmp-x-1148 syntmp-y-1149))))))
               (syntmp-gen-map-1137
                 (lambda (syntmp-e-1151 syntmp-map-env-1152)
                   (let ((syntmp-formals-1153
                           (map cdr syntmp-map-env-1152))
                         (syntmp-actuals-1154
                           (map (lambda (syntmp-x-1155)
                                  (list (quote ref) (car syntmp-x-1155)))
                                syntmp-map-env-1152)))
                     (cond ((eq? (car syntmp-e-1151) (quote ref))
                            (car syntmp-actuals-1154))
                           ((andmap
                              (lambda (syntmp-x-1156)
                                (and (eq? (car syntmp-x-1156) (quote ref))
                                     (memq (cadr syntmp-x-1156)
                                           syntmp-formals-1153)))
                              (cdr syntmp-e-1151))
                            (cons 'map
                                  (cons (list 'primitive
                                              (car syntmp-e-1151))
                                        (map (let ((syntmp-r-1157
                                                     (map cons
                                                          syntmp-formals-1153
                                                          syntmp-actuals-1154)))
                                               (lambda (syntmp-x-1158)
                                                 (cdr (assq (cadr syntmp-x-1158)
                                                            syntmp-r-1157))))
                                             (cdr syntmp-e-1151)))))
                           (else
                            (cons 'map
                                  (cons (list 'lambda
                                              syntmp-formals-1153
                                              syntmp-e-1151)
                                        syntmp-actuals-1154)))))))
               (syntmp-gen-mappend-1136
                 (lambda (syntmp-e-1159 syntmp-map-env-1160)
                   (list 'apply
                         '(primitive append)
                         (syntmp-gen-map-1137
                           syntmp-e-1159
                           syntmp-map-env-1160))))
               (syntmp-gen-ref-1135
                 (lambda (syntmp-src-1161
                          syntmp-var-1162
                          syntmp-level-1163
                          syntmp-maps-1164)
                   (if (syntmp-fx=-89 syntmp-level-1163 0)
                     (values syntmp-var-1162 syntmp-maps-1164)
                     (if (null? syntmp-maps-1164)
                       (syntax-error
                         syntmp-src-1161
                         "missing ellipsis in syntax form")
                       (call-with-values
                         (lambda ()
                           (syntmp-gen-ref-1135
                             syntmp-src-1161
                             syntmp-var-1162
                             (syntmp-fx--88 syntmp-level-1163 1)
                             (cdr syntmp-maps-1164)))
                         (lambda (syntmp-outer-var-1165 syntmp-outer-maps-1166)
                           (let ((syntmp-b-1167
                                   (assq syntmp-outer-var-1165
                                         (car syntmp-maps-1164))))
                             (if syntmp-b-1167
                               (values (cdr syntmp-b-1167) syntmp-maps-1164)
                               (let ((syntmp-inner-var-1168
                                       (syntmp-gen-var-167 (quote tmp))))
                                 (values
                                   syntmp-inner-var-1168
                                   (cons (cons (cons syntmp-outer-var-1165
                                                     syntmp-inner-var-1168)
                                               (car syntmp-maps-1164))
                                         syntmp-outer-maps-1166)))))))))))
               (syntmp-gen-syntax-1134
                 (lambda (syntmp-src-1169
                          syntmp-e-1170
                          syntmp-r-1171
                          syntmp-maps-1172
                          syntmp-ellipsis?-1173
                          syntmp-mod-1174)
                   (if (syntmp-id?-119 syntmp-e-1170)
                     (let ((syntmp-label-1175
                             (syntmp-id-var-name-141
                               syntmp-e-1170
                               '(()))))
                       (let ((syntmp-b-1176
                               (syntmp-lookup-116
                                 syntmp-label-1175
                                 syntmp-r-1171
                                 syntmp-mod-1174)))
                         (if (eq? (syntmp-binding-type-111 syntmp-b-1176)
                                  'syntax)
                           (call-with-values
                             (lambda ()
                               (let ((syntmp-var.lev-1177
                                       (syntmp-binding-value-112
                                         syntmp-b-1176)))
                                 (syntmp-gen-ref-1135
                                   syntmp-src-1169
                                   (car syntmp-var.lev-1177)
                                   (cdr syntmp-var.lev-1177)
                                   syntmp-maps-1172)))
                             (lambda (syntmp-var-1178 syntmp-maps-1179)
                               (values
                                 (list (quote ref) syntmp-var-1178)
                                 syntmp-maps-1179)))
                           (if (syntmp-ellipsis?-1173 syntmp-e-1170)
                             (syntax-error
                               syntmp-src-1169
                               "misplaced ellipsis in syntax form")
                             (values
                               (list (quote quote) syntmp-e-1170)
                               syntmp-maps-1172)))))
                     ((lambda (syntmp-tmp-1180)
                        ((lambda (syntmp-tmp-1181)
                           (if (if syntmp-tmp-1181
                                 (apply (lambda (syntmp-dots-1182
                                                 syntmp-e-1183)
                                          (syntmp-ellipsis?-1173
                                            syntmp-dots-1182))
                                        syntmp-tmp-1181)
                                 #f)
                             (apply (lambda (syntmp-dots-1184 syntmp-e-1185)
                                      (syntmp-gen-syntax-1134
                                        syntmp-src-1169
                                        syntmp-e-1185
                                        syntmp-r-1171
                                        syntmp-maps-1172
                                        (lambda (syntmp-x-1186) #f)
                                        syntmp-mod-1174))
                                    syntmp-tmp-1181)
                             ((lambda (syntmp-tmp-1187)
                                (if (if syntmp-tmp-1187
                                      (apply (lambda (syntmp-x-1188
                                                      syntmp-dots-1189
                                                      syntmp-y-1190)
                                               (syntmp-ellipsis?-1173
                                                 syntmp-dots-1189))
                                             syntmp-tmp-1187)
                                      #f)
                                  (apply (lambda (syntmp-x-1191
                                                  syntmp-dots-1192
                                                  syntmp-y-1193)
                                           (let syntmp-f-1194 ((syntmp-y-1195
                                                                 syntmp-y-1193)
                                                               (syntmp-k-1196
                                                                 (lambda (syntmp-maps-1197)
                                                                   (call-with-values
                                                                     (lambda ()
                                                                       (syntmp-gen-syntax-1134
                                                                         syntmp-src-1169
                                                                         syntmp-x-1191
                                                                         syntmp-r-1171
                                                                         (cons '()
                                                                               syntmp-maps-1197)
                                                                         syntmp-ellipsis?-1173
                                                                         syntmp-mod-1174))
                                                                     (lambda (syntmp-x-1198
                                                                              syntmp-maps-1199)
                                                                       (if (null? (car syntmp-maps-1199))
                                                                         (syntax-error
                                                                           syntmp-src-1169
                                                                           "extra ellipsis in syntax form")
                                                                         (values
                                                                           (syntmp-gen-map-1137
                                                                             syntmp-x-1198
                                                                             (car syntmp-maps-1199))
                                                                           (cdr syntmp-maps-1199))))))))
                                             ((lambda (syntmp-tmp-1200)
                                                ((lambda (syntmp-tmp-1201)
                                                   (if (if syntmp-tmp-1201
                                                         (apply (lambda (syntmp-dots-1202
                                                                         syntmp-y-1203)
                                                                  (syntmp-ellipsis?-1173
                                                                    syntmp-dots-1202))
                                                                syntmp-tmp-1201)
                                                         #f)
                                                     (apply (lambda (syntmp-dots-1204
                                                                     syntmp-y-1205)
                                                              (syntmp-f-1194
                                                                syntmp-y-1205
                                                                (lambda (syntmp-maps-1206)
                                                                  (call-with-values
                                                                    (lambda ()
                                                                      (syntmp-k-1196
                                                                        (cons '()
                                                                              syntmp-maps-1206)))
                                                                    (lambda (syntmp-x-1207
                                                                             syntmp-maps-1208)
                                                                      (if (null? (car syntmp-maps-1208))
                                                                        (syntax-error
                                                                          syntmp-src-1169
                                                                          "extra ellipsis in syntax form")
                                                                        (values
                                                                          (syntmp-gen-mappend-1136
                                                                            syntmp-x-1207
                                                                            (car syntmp-maps-1208))
                                                                          (cdr syntmp-maps-1208))))))))
                                                            syntmp-tmp-1201)
                                                     ((lambda (syntmp-_-1209)
                                                        (call-with-values
                                                          (lambda ()
                                                            (syntmp-gen-syntax-1134
                                                              syntmp-src-1169
                                                              syntmp-y-1195
                                                              syntmp-r-1171
                                                              syntmp-maps-1172
                                                              syntmp-ellipsis?-1173
                                                              syntmp-mod-1174))
                                                          (lambda (syntmp-y-1210
                                                                   syntmp-maps-1211)
                                                            (call-with-values
                                                              (lambda ()
                                                                (syntmp-k-1196
                                                                  syntmp-maps-1211))
                                                              (lambda (syntmp-x-1212
                                                                       syntmp-maps-1213)
                                                                (values
                                                                  (syntmp-gen-append-1139
                                                                    syntmp-x-1212
                                                                    syntmp-y-1210)
                                                                  syntmp-maps-1213))))))
                                                      syntmp-tmp-1200)))
                                                 (syntax-dispatch
                                                   syntmp-tmp-1200
                                                   '(any . any))))
                                              syntmp-y-1195)))
                                         syntmp-tmp-1187)
                                  ((lambda (syntmp-tmp-1214)
                                     (if syntmp-tmp-1214
                                       (apply (lambda (syntmp-x-1215
                                                       syntmp-y-1216)
                                                (call-with-values
                                                  (lambda ()
                                                    (syntmp-gen-syntax-1134
                                                      syntmp-src-1169
                                                      syntmp-x-1215
                                                      syntmp-r-1171
                                                      syntmp-maps-1172
                                                      syntmp-ellipsis?-1173
                                                      syntmp-mod-1174))
                                                  (lambda (syntmp-x-1217
                                                           syntmp-maps-1218)
                                                    (call-with-values
                                                      (lambda ()
                                                        (syntmp-gen-syntax-1134
                                                          syntmp-src-1169
                                                          syntmp-y-1216
                                                          syntmp-r-1171
                                                          syntmp-maps-1218
                                                          syntmp-ellipsis?-1173
                                                          syntmp-mod-1174))
                                                      (lambda (syntmp-y-1219
                                                               syntmp-maps-1220)
                                                        (values
                                                          (syntmp-gen-cons-1138
                                                            syntmp-x-1217
                                                            syntmp-y-1219)
                                                          syntmp-maps-1220))))))
                                              syntmp-tmp-1214)
                                       ((lambda (syntmp-tmp-1221)
                                          (if syntmp-tmp-1221
                                            (apply (lambda (syntmp-e1-1222
                                                            syntmp-e2-1223)
                                                     (call-with-values
                                                       (lambda ()
                                                         (syntmp-gen-syntax-1134
                                                           syntmp-src-1169
                                                           (cons syntmp-e1-1222
                                                                 syntmp-e2-1223)
                                                           syntmp-r-1171
                                                           syntmp-maps-1172
                                                           syntmp-ellipsis?-1173
                                                           syntmp-mod-1174))
                                                       (lambda (syntmp-e-1225
                                                                syntmp-maps-1226)
                                                         (values
                                                           (syntmp-gen-vector-1140
                                                             syntmp-e-1225)
                                                           syntmp-maps-1226))))
                                                   syntmp-tmp-1221)
                                            ((lambda (syntmp-_-1227)
                                               (values
                                                 (list 'quote
                                                       syntmp-e-1170)
                                                 syntmp-maps-1172))
                                             syntmp-tmp-1180)))
                                        (syntax-dispatch
                                          syntmp-tmp-1180
                                          '#(vector (any . each-any))))))
                                   (syntax-dispatch
                                     syntmp-tmp-1180
                                     '(any . any)))))
                              (syntax-dispatch
                                syntmp-tmp-1180
                                '(any any . any)))))
                         (syntax-dispatch
                           syntmp-tmp-1180
                           '(any any))))
                      syntmp-e-1170)))))
        (lambda (syntmp-e-1228
                 syntmp-r-1229
                 syntmp-w-1230
                 syntmp-s-1231
                 syntmp-mod-1232)
          (let ((syntmp-e-1233
                  (syntmp-source-wrap-148
                    syntmp-e-1228
                    syntmp-w-1230
                    syntmp-s-1231
                    syntmp-mod-1232)))
            ((lambda (syntmp-tmp-1234)
               ((lambda (syntmp-tmp-1235)
                  (if syntmp-tmp-1235
                    (apply (lambda (syntmp-_-1236 syntmp-x-1237)
                             (call-with-values
                               (lambda ()
                                 (syntmp-gen-syntax-1134
                                   syntmp-e-1233
                                   syntmp-x-1237
                                   syntmp-r-1229
                                   '()
                                   syntmp-ellipsis?-164
                                   syntmp-mod-1232))
                               (lambda (syntmp-e-1238 syntmp-maps-1239)
                                 (syntmp-regen-1141 syntmp-e-1238))))
                           syntmp-tmp-1235)
                    ((lambda (syntmp-_-1240)
                       (syntax-error syntmp-e-1233))
                     syntmp-tmp-1234)))
                (syntax-dispatch
                  syntmp-tmp-1234
                  '(any any))))
             syntmp-e-1233)))))
    (syntmp-global-extend-117
      'core
      'lambda
      (lambda (syntmp-e-1241
               syntmp-r-1242
               syntmp-w-1243
               syntmp-s-1244
               syntmp-mod-1245)
        ((lambda (syntmp-tmp-1246)
           ((lambda (syntmp-tmp-1247)
              (if syntmp-tmp-1247
                (apply (lambda (syntmp-_-1248 syntmp-c-1249)
                         (syntmp-chi-lambda-clause-160
                           (syntmp-source-wrap-148
                             syntmp-e-1241
                             syntmp-w-1243
                             syntmp-s-1244
                             syntmp-mod-1245)
                           syntmp-c-1249
                           syntmp-r-1242
                           syntmp-w-1243
                           syntmp-mod-1245
                           (lambda (syntmp-vars-1250 syntmp-body-1251)
                             (syntmp-build-annotated-96
                               syntmp-s-1244
                               (list 'lambda
                                     syntmp-vars-1250
                                     syntmp-body-1251)))))
                       syntmp-tmp-1247)
                (syntax-error syntmp-tmp-1246)))
            (syntax-dispatch
              syntmp-tmp-1246
              '(any . any))))
         syntmp-e-1241)))
    (syntmp-global-extend-117
      'core
      'let
      (letrec ((syntmp-chi-let-1252
                 (lambda (syntmp-e-1253
                          syntmp-r-1254
                          syntmp-w-1255
                          syntmp-s-1256
                          syntmp-mod-1257
                          syntmp-constructor-1258
                          syntmp-ids-1259
                          syntmp-vals-1260
                          syntmp-exps-1261)
                   (if (not (syntmp-valid-bound-ids?-144 syntmp-ids-1259))
                     (syntax-error
                       syntmp-e-1253
                       "duplicate bound variable in")
                     (let ((syntmp-labels-1262
                             (syntmp-gen-labels-125 syntmp-ids-1259))
                           (syntmp-new-vars-1263
                             (map syntmp-gen-var-167 syntmp-ids-1259)))
                       (let ((syntmp-nw-1264
                               (syntmp-make-binding-wrap-136
                                 syntmp-ids-1259
                                 syntmp-labels-1262
                                 syntmp-w-1255))
                             (syntmp-nr-1265
                               (syntmp-extend-var-env-114
                                 syntmp-labels-1262
                                 syntmp-new-vars-1263
                                 syntmp-r-1254)))
                         (syntmp-constructor-1258
                           syntmp-s-1256
                           syntmp-new-vars-1263
                           (map (lambda (syntmp-x-1266)
                                  (syntmp-chi-155
                                    syntmp-x-1266
                                    syntmp-r-1254
                                    syntmp-w-1255
                                    syntmp-mod-1257))
                                syntmp-vals-1260)
                           (syntmp-chi-body-159
                             syntmp-exps-1261
                             (syntmp-source-wrap-148
                               syntmp-e-1253
                               syntmp-nw-1264
                               syntmp-s-1256
                               syntmp-mod-1257)
                             syntmp-nr-1265
                             syntmp-nw-1264
                             syntmp-mod-1257))))))))
        (lambda (syntmp-e-1267
                 syntmp-r-1268
                 syntmp-w-1269
                 syntmp-s-1270
                 syntmp-mod-1271)
          ((lambda (syntmp-tmp-1272)
             ((lambda (syntmp-tmp-1273)
                (if syntmp-tmp-1273
                  (apply (lambda (syntmp-_-1274
                                  syntmp-id-1275
                                  syntmp-val-1276
                                  syntmp-e1-1277
                                  syntmp-e2-1278)
                           (syntmp-chi-let-1252
                             syntmp-e-1267
                             syntmp-r-1268
                             syntmp-w-1269
                             syntmp-s-1270
                             syntmp-mod-1271
                             syntmp-build-let-99
                             syntmp-id-1275
                             syntmp-val-1276
                             (cons syntmp-e1-1277 syntmp-e2-1278)))
                         syntmp-tmp-1273)
                  ((lambda (syntmp-tmp-1282)
                     (if (if syntmp-tmp-1282
                           (apply (lambda (syntmp-_-1283
                                           syntmp-f-1284
                                           syntmp-id-1285
                                           syntmp-val-1286
                                           syntmp-e1-1287
                                           syntmp-e2-1288)
                                    (syntmp-id?-119 syntmp-f-1284))
                                  syntmp-tmp-1282)
                           #f)
                       (apply (lambda (syntmp-_-1289
                                       syntmp-f-1290
                                       syntmp-id-1291
                                       syntmp-val-1292
                                       syntmp-e1-1293
                                       syntmp-e2-1294)
                                (syntmp-chi-let-1252
                                  syntmp-e-1267
                                  syntmp-r-1268
                                  syntmp-w-1269
                                  syntmp-s-1270
                                  syntmp-mod-1271
                                  syntmp-build-named-let-100
                                  (cons syntmp-f-1290 syntmp-id-1291)
                                  syntmp-val-1292
                                  (cons syntmp-e1-1293 syntmp-e2-1294)))
                              syntmp-tmp-1282)
                       ((lambda (syntmp-_-1298)
                          (syntax-error
                            (syntmp-source-wrap-148
                              syntmp-e-1267
                              syntmp-w-1269
                              syntmp-s-1270
                              syntmp-mod-1271)))
                        syntmp-tmp-1272)))
                   (syntax-dispatch
                     syntmp-tmp-1272
                     '(any any #(each (any any)) any . each-any)))))
              (syntax-dispatch
                syntmp-tmp-1272
                '(any #(each (any any)) any . each-any))))
           syntmp-e-1267))))
    (syntmp-global-extend-117
      'core
      'letrec
      (lambda (syntmp-e-1299
               syntmp-r-1300
               syntmp-w-1301
               syntmp-s-1302
               syntmp-mod-1303)
        ((lambda (syntmp-tmp-1304)
           ((lambda (syntmp-tmp-1305)
              (if syntmp-tmp-1305
                (apply (lambda (syntmp-_-1306
                                syntmp-id-1307
                                syntmp-val-1308
                                syntmp-e1-1309
                                syntmp-e2-1310)
                         (let ((syntmp-ids-1311 syntmp-id-1307))
                           (if (not (syntmp-valid-bound-ids?-144
                                      syntmp-ids-1311))
                             (syntax-error
                               syntmp-e-1299
                               "duplicate bound variable in")
                             (let ((syntmp-labels-1313
                                     (syntmp-gen-labels-125 syntmp-ids-1311))
                                   (syntmp-new-vars-1314
                                     (map syntmp-gen-var-167 syntmp-ids-1311)))
                               (let ((syntmp-w-1315
                                       (syntmp-make-binding-wrap-136
                                         syntmp-ids-1311
                                         syntmp-labels-1313
                                         syntmp-w-1301))
                                     (syntmp-r-1316
                                       (syntmp-extend-var-env-114
                                         syntmp-labels-1313
                                         syntmp-new-vars-1314
                                         syntmp-r-1300)))
                                 (syntmp-build-letrec-101
                                   syntmp-s-1302
                                   syntmp-new-vars-1314
                                   (map (lambda (syntmp-x-1317)
                                          (syntmp-chi-155
                                            syntmp-x-1317
                                            syntmp-r-1316
                                            syntmp-w-1315
                                            syntmp-mod-1303))
                                        syntmp-val-1308)
                                   (syntmp-chi-body-159
                                     (cons syntmp-e1-1309 syntmp-e2-1310)
                                     (syntmp-source-wrap-148
                                       syntmp-e-1299
                                       syntmp-w-1315
                                       syntmp-s-1302
                                       syntmp-mod-1303)
                                     syntmp-r-1316
                                     syntmp-w-1315
                                     syntmp-mod-1303)))))))
                       syntmp-tmp-1305)
                ((lambda (syntmp-_-1320)
                   (syntax-error
                     (syntmp-source-wrap-148
                       syntmp-e-1299
                       syntmp-w-1301
                       syntmp-s-1302
                       syntmp-mod-1303)))
                 syntmp-tmp-1304)))
            (syntax-dispatch
              syntmp-tmp-1304
              '(any #(each (any any)) any . each-any))))
         syntmp-e-1299)))
    (syntmp-global-extend-117
      'core
      'set!
      (lambda (syntmp-e-1321
               syntmp-r-1322
               syntmp-w-1323
               syntmp-s-1324
               syntmp-mod-1325)
        ((lambda (syntmp-tmp-1326)
           ((lambda (syntmp-tmp-1327)
              (if (if syntmp-tmp-1327
                    (apply (lambda (syntmp-_-1328
                                    syntmp-id-1329
                                    syntmp-val-1330)
                             (syntmp-id?-119 syntmp-id-1329))
                           syntmp-tmp-1327)
                    #f)
                (apply (lambda (syntmp-_-1331 syntmp-id-1332 syntmp-val-1333)
                         (let ((syntmp-val-1334
                                 (syntmp-chi-155
                                   syntmp-val-1333
                                   syntmp-r-1322
                                   syntmp-w-1323
                                   syntmp-mod-1325))
                               (syntmp-n-1335
                                 (syntmp-id-var-name-141
                                   syntmp-id-1332
                                   syntmp-w-1323)))
                           (let ((syntmp-b-1336
                                   (syntmp-lookup-116
                                     syntmp-n-1335
                                     syntmp-r-1322
                                     syntmp-mod-1325)))
                             (let ((syntmp-t-1337
                                     (syntmp-binding-type-111 syntmp-b-1336)))
                               (if (memv syntmp-t-1337 (quote (lexical)))
                                 (syntmp-build-annotated-96
                                   syntmp-s-1324
                                   (list 'set!
                                         (syntmp-binding-value-112
                                           syntmp-b-1336)
                                         syntmp-val-1334))
                                 (if (memv syntmp-t-1337 (quote (global)))
                                   (syntmp-build-annotated-96
                                     syntmp-s-1324
                                     (list 'set!
                                           (make-module-ref
                                             syntmp-mod-1325
                                             syntmp-n-1335
                                             #f)
                                           syntmp-val-1334))
                                   (if (memv syntmp-t-1337
                                             '(displaced-lexical))
                                     (syntax-error
                                       (syntmp-wrap-147
                                         syntmp-id-1332
                                         syntmp-w-1323
                                         syntmp-mod-1325)
                                       "identifier out of context")
                                     (syntax-error
                                       (syntmp-source-wrap-148
                                         syntmp-e-1321
                                         syntmp-w-1323
                                         syntmp-s-1324
                                         syntmp-mod-1325)))))))))
                       syntmp-tmp-1327)
                ((lambda (syntmp-tmp-1338)
                   (if syntmp-tmp-1338
                     (apply (lambda (syntmp-_-1339
                                     syntmp-getter-1340
                                     syntmp-arg-1341
                                     syntmp-val-1342)
                              (syntmp-build-annotated-96
                                syntmp-s-1324
                                (cons (syntmp-chi-155
                                        (list '#(syntax-object
                                                 setter
                                                 ((top)
                                                  #(ribcage
                                                    #(_ getter arg val)
                                                    #((top) (top) (top) (top))
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
                                                      strip-annotation
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
                                                      unannotate
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
                                                      build-global-assignment
                                                      build-global-reference
                                                      build-lexical-assignment
                                                      build-lexical-reference
                                                      build-conditional
                                                      build-application
                                                      build-annotated
                                                      get-global-definition-hook
                                                      put-global-definition-hook
                                                      gensym-hook
                                                      error-hook
                                                      local-eval-hook
                                                      top-level-eval-hook
                                                      fx<
                                                      fx=
                                                      fx-
                                                      fx+
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
                                                     "i"))
                                                  #(ribcage
                                                    (define-structure)
                                                    ((top))
                                                    ("i")))
                                                 (ice-9 syncase))
                                              syntmp-getter-1340)
                                        syntmp-r-1322
                                        syntmp-w-1323
                                        syntmp-mod-1325)
                                      (map (lambda (syntmp-e-1343)
                                             (syntmp-chi-155
                                               syntmp-e-1343
                                               syntmp-r-1322
                                               syntmp-w-1323
                                               syntmp-mod-1325))
                                           (append
                                             syntmp-arg-1341
                                             (list syntmp-val-1342))))))
                            syntmp-tmp-1338)
                     ((lambda (syntmp-_-1345)
                        (syntax-error
                          (syntmp-source-wrap-148
                            syntmp-e-1321
                            syntmp-w-1323
                            syntmp-s-1324
                            syntmp-mod-1325)))
                      syntmp-tmp-1326)))
                 (syntax-dispatch
                   syntmp-tmp-1326
                   '(any (any . each-any) any)))))
            (syntax-dispatch
              syntmp-tmp-1326
              '(any any any))))
         syntmp-e-1321)))
    (syntmp-global-extend-117
      'begin
      'begin
      '())
    (syntmp-global-extend-117
      'define
      'define
      '())
    (syntmp-global-extend-117
      'define-syntax
      'define-syntax
      '())
    (syntmp-global-extend-117
      'eval-when
      'eval-when
      '())
    (syntmp-global-extend-117
      'core
      'syntax-case
      (letrec ((syntmp-gen-syntax-case-1349
                 (lambda (syntmp-x-1350
                          syntmp-keys-1351
                          syntmp-clauses-1352
                          syntmp-r-1353
                          syntmp-mod-1354)
                   (if (null? syntmp-clauses-1352)
                     (syntmp-build-annotated-96
                       #f
                       (list (syntmp-build-annotated-96
                               #f
                               'syntax-error)
                             syntmp-x-1350))
                     ((lambda (syntmp-tmp-1355)
                        ((lambda (syntmp-tmp-1356)
                           (if syntmp-tmp-1356
                             (apply (lambda (syntmp-pat-1357 syntmp-exp-1358)
                                      (if (and (syntmp-id?-119 syntmp-pat-1357)
                                               (andmap
                                                 (lambda (syntmp-x-1359)
                                                   (not (syntmp-free-id=?-142
                                                          syntmp-pat-1357
                                                          syntmp-x-1359)))
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
                                                               strip-annotation
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
                                                               unannotate
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
                                                               build-global-assignment
                                                               build-global-reference
                                                               build-lexical-assignment
                                                               build-lexical-reference
                                                               build-conditional
                                                               build-application
                                                               build-annotated
                                                               get-global-definition-hook
                                                               put-global-definition-hook
                                                               gensym-hook
                                                               error-hook
                                                               local-eval-hook
                                                               top-level-eval-hook
                                                               fx<
                                                               fx=
                                                               fx-
                                                               fx+
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
                                                              "i"))
                                                           #(ribcage
                                                             (define-structure)
                                                             ((top))
                                                             ("i")))
                                                          (ice-9 syncase))
                                                       syntmp-keys-1351)))
                                        (let ((syntmp-labels-1360
                                                (list (syntmp-gen-label-124)))
                                              (syntmp-var-1361
                                                (syntmp-gen-var-167
                                                  syntmp-pat-1357)))
                                          (syntmp-build-annotated-96
                                            #f
                                            (list (syntmp-build-annotated-96
                                                    #f
                                                    (list 'lambda
                                                          (list syntmp-var-1361)
                                                          (syntmp-chi-155
                                                            syntmp-exp-1358
                                                            (syntmp-extend-env-113
                                                              syntmp-labels-1360
                                                              (list (cons 'syntax
                                                                          (cons syntmp-var-1361
                                                                                0)))
                                                              syntmp-r-1353)
                                                            (syntmp-make-binding-wrap-136
                                                              (list syntmp-pat-1357)
                                                              syntmp-labels-1360
                                                              '(()))
                                                            syntmp-mod-1354)))
                                                  syntmp-x-1350)))
                                        (syntmp-gen-clause-1348
                                          syntmp-x-1350
                                          syntmp-keys-1351
                                          (cdr syntmp-clauses-1352)
                                          syntmp-r-1353
                                          syntmp-pat-1357
                                          #t
                                          syntmp-exp-1358
                                          syntmp-mod-1354)))
                                    syntmp-tmp-1356)
                             ((lambda (syntmp-tmp-1362)
                                (if syntmp-tmp-1362
                                  (apply (lambda (syntmp-pat-1363
                                                  syntmp-fender-1364
                                                  syntmp-exp-1365)
                                           (syntmp-gen-clause-1348
                                             syntmp-x-1350
                                             syntmp-keys-1351
                                             (cdr syntmp-clauses-1352)
                                             syntmp-r-1353
                                             syntmp-pat-1363
                                             syntmp-fender-1364
                                             syntmp-exp-1365
                                             syntmp-mod-1354))
                                         syntmp-tmp-1362)
                                  ((lambda (syntmp-_-1366)
                                     (syntax-error
                                       (car syntmp-clauses-1352)
                                       "invalid syntax-case clause"))
                                   syntmp-tmp-1355)))
                              (syntax-dispatch
                                syntmp-tmp-1355
                                '(any any any)))))
                         (syntax-dispatch
                           syntmp-tmp-1355
                           '(any any))))
                      (car syntmp-clauses-1352)))))
               (syntmp-gen-clause-1348
                 (lambda (syntmp-x-1367
                          syntmp-keys-1368
                          syntmp-clauses-1369
                          syntmp-r-1370
                          syntmp-pat-1371
                          syntmp-fender-1372
                          syntmp-exp-1373
                          syntmp-mod-1374)
                   (call-with-values
                     (lambda ()
                       (syntmp-convert-pattern-1346
                         syntmp-pat-1371
                         syntmp-keys-1368))
                     (lambda (syntmp-p-1375 syntmp-pvars-1376)
                       (cond ((not (syntmp-distinct-bound-ids?-145
                                     (map car syntmp-pvars-1376)))
                              (syntax-error
                                syntmp-pat-1371
                                "duplicate pattern variable in syntax-case pattern"))
                             ((not (andmap
                                     (lambda (syntmp-x-1377)
                                       (not (syntmp-ellipsis?-164
                                              (car syntmp-x-1377))))
                                     syntmp-pvars-1376))
                              (syntax-error
                                syntmp-pat-1371
                                "misplaced ellipsis in syntax-case pattern"))
                             (else
                              (let ((syntmp-y-1378
                                      (syntmp-gen-var-167 (quote tmp))))
                                (syntmp-build-annotated-96
                                  #f
                                  (list (syntmp-build-annotated-96
                                          #f
                                          (list 'lambda
                                                (list syntmp-y-1378)
                                                (let ((syntmp-y-1379
                                                        (syntmp-build-annotated-96
                                                          #f
                                                          syntmp-y-1378)))
                                                  (syntmp-build-annotated-96
                                                    #f
                                                    (list 'if
                                                          ((lambda (syntmp-tmp-1380)
                                                             ((lambda (syntmp-tmp-1381)
                                                                (if syntmp-tmp-1381
                                                                  (apply (lambda ()
                                                                           syntmp-y-1379)
                                                                         syntmp-tmp-1381)
                                                                  ((lambda (syntmp-_-1382)
                                                                     (syntmp-build-annotated-96
                                                                       #f
                                                                       (list 'if
                                                                             syntmp-y-1379
                                                                             (syntmp-build-dispatch-call-1347
                                                                               syntmp-pvars-1376
                                                                               syntmp-fender-1372
                                                                               syntmp-y-1379
                                                                               syntmp-r-1370
                                                                               syntmp-mod-1374)
                                                                             (syntmp-build-data-97
                                                                               #f
                                                                               #f))))
                                                                   syntmp-tmp-1380)))
                                                              (syntax-dispatch
                                                                syntmp-tmp-1380
                                                                '#(atom #t))))
                                                           syntmp-fender-1372)
                                                          (syntmp-build-dispatch-call-1347
                                                            syntmp-pvars-1376
                                                            syntmp-exp-1373
                                                            syntmp-y-1379
                                                            syntmp-r-1370
                                                            syntmp-mod-1374)
                                                          (syntmp-gen-syntax-case-1349
                                                            syntmp-x-1367
                                                            syntmp-keys-1368
                                                            syntmp-clauses-1369
                                                            syntmp-r-1370
                                                            syntmp-mod-1374))))))
                                        (if (eq? syntmp-p-1375 (quote any))
                                          (syntmp-build-annotated-96
                                            #f
                                            (list (syntmp-build-annotated-96
                                                    #f
                                                    'list)
                                                  syntmp-x-1367))
                                          (syntmp-build-annotated-96
                                            #f
                                            (list (syntmp-build-annotated-96
                                                    #f
                                                    'syntax-dispatch)
                                                  syntmp-x-1367
                                                  (syntmp-build-data-97
                                                    #f
                                                    syntmp-p-1375)))))))))))))
               (syntmp-build-dispatch-call-1347
                 (lambda (syntmp-pvars-1383
                          syntmp-exp-1384
                          syntmp-y-1385
                          syntmp-r-1386
                          syntmp-mod-1387)
                   (let ((syntmp-ids-1388 (map car syntmp-pvars-1383))
                         (syntmp-levels-1389 (map cdr syntmp-pvars-1383)))
                     (let ((syntmp-labels-1390
                             (syntmp-gen-labels-125 syntmp-ids-1388))
                           (syntmp-new-vars-1391
                             (map syntmp-gen-var-167 syntmp-ids-1388)))
                       (syntmp-build-annotated-96
                         #f
                         (list (syntmp-build-annotated-96 #f (quote apply))
                               (syntmp-build-annotated-96
                                 #f
                                 (list 'lambda
                                       syntmp-new-vars-1391
                                       (syntmp-chi-155
                                         syntmp-exp-1384
                                         (syntmp-extend-env-113
                                           syntmp-labels-1390
                                           (map (lambda (syntmp-var-1392
                                                         syntmp-level-1393)
                                                  (cons 'syntax
                                                        (cons syntmp-var-1392
                                                              syntmp-level-1393)))
                                                syntmp-new-vars-1391
                                                (map cdr syntmp-pvars-1383))
                                           syntmp-r-1386)
                                         (syntmp-make-binding-wrap-136
                                           syntmp-ids-1388
                                           syntmp-labels-1390
                                           '(()))
                                         syntmp-mod-1387)))
                               syntmp-y-1385))))))
               (syntmp-convert-pattern-1346
                 (lambda (syntmp-pattern-1394 syntmp-keys-1395)
                   (let syntmp-cvt-1396 ((syntmp-p-1397 syntmp-pattern-1394)
                                         (syntmp-n-1398 0)
                                         (syntmp-ids-1399 (quote ())))
                     (if (syntmp-id?-119 syntmp-p-1397)
                       (if (syntmp-bound-id-member?-146
                             syntmp-p-1397
                             syntmp-keys-1395)
                         (values
                           (vector (quote free-id) syntmp-p-1397)
                           syntmp-ids-1399)
                         (values
                           'any
                           (cons (cons syntmp-p-1397 syntmp-n-1398)
                                 syntmp-ids-1399)))
                       ((lambda (syntmp-tmp-1400)
                          ((lambda (syntmp-tmp-1401)
                             (if (if syntmp-tmp-1401
                                   (apply (lambda (syntmp-x-1402
                                                   syntmp-dots-1403)
                                            (syntmp-ellipsis?-164
                                              syntmp-dots-1403))
                                          syntmp-tmp-1401)
                                   #f)
                               (apply (lambda (syntmp-x-1404 syntmp-dots-1405)
                                        (call-with-values
                                          (lambda ()
                                            (syntmp-cvt-1396
                                              syntmp-x-1404
                                              (syntmp-fx+-87 syntmp-n-1398 1)
                                              syntmp-ids-1399))
                                          (lambda (syntmp-p-1406
                                                   syntmp-ids-1407)
                                            (values
                                              (if (eq? syntmp-p-1406
                                                       'any)
                                                'each-any
                                                (vector
                                                  'each
                                                  syntmp-p-1406))
                                              syntmp-ids-1407))))
                                      syntmp-tmp-1401)
                               ((lambda (syntmp-tmp-1408)
                                  (if syntmp-tmp-1408
                                    (apply (lambda (syntmp-x-1409
                                                    syntmp-y-1410)
                                             (call-with-values
                                               (lambda ()
                                                 (syntmp-cvt-1396
                                                   syntmp-y-1410
                                                   syntmp-n-1398
                                                   syntmp-ids-1399))
                                               (lambda (syntmp-y-1411
                                                        syntmp-ids-1412)
                                                 (call-with-values
                                                   (lambda ()
                                                     (syntmp-cvt-1396
                                                       syntmp-x-1409
                                                       syntmp-n-1398
                                                       syntmp-ids-1412))
                                                   (lambda (syntmp-x-1413
                                                            syntmp-ids-1414)
                                                     (values
                                                       (cons syntmp-x-1413
                                                             syntmp-y-1411)
                                                       syntmp-ids-1414))))))
                                           syntmp-tmp-1408)
                                    ((lambda (syntmp-tmp-1415)
                                       (if syntmp-tmp-1415
                                         (apply (lambda ()
                                                  (values
                                                    '()
                                                    syntmp-ids-1399))
                                                syntmp-tmp-1415)
                                         ((lambda (syntmp-tmp-1416)
                                            (if syntmp-tmp-1416
                                              (apply (lambda (syntmp-x-1417)
                                                       (call-with-values
                                                         (lambda ()
                                                           (syntmp-cvt-1396
                                                             syntmp-x-1417
                                                             syntmp-n-1398
                                                             syntmp-ids-1399))
                                                         (lambda (syntmp-p-1419
                                                                  syntmp-ids-1420)
                                                           (values
                                                             (vector
                                                               'vector
                                                               syntmp-p-1419)
                                                             syntmp-ids-1420))))
                                                     syntmp-tmp-1416)
                                              ((lambda (syntmp-x-1421)
                                                 (values
                                                   (vector
                                                     'atom
                                                     (syntmp-strip-166
                                                       syntmp-p-1397
                                                       '(())))
                                                   syntmp-ids-1399))
                                               syntmp-tmp-1400)))
                                          (syntax-dispatch
                                            syntmp-tmp-1400
                                            '#(vector each-any)))))
                                     (syntax-dispatch
                                       syntmp-tmp-1400
                                       '()))))
                                (syntax-dispatch
                                  syntmp-tmp-1400
                                  '(any . any)))))
                           (syntax-dispatch
                             syntmp-tmp-1400
                             '(any any))))
                        syntmp-p-1397))))))
        (lambda (syntmp-e-1422
                 syntmp-r-1423
                 syntmp-w-1424
                 syntmp-s-1425
                 syntmp-mod-1426)
          (let ((syntmp-e-1427
                  (syntmp-source-wrap-148
                    syntmp-e-1422
                    syntmp-w-1424
                    syntmp-s-1425
                    syntmp-mod-1426)))
            ((lambda (syntmp-tmp-1428)
               ((lambda (syntmp-tmp-1429)
                  (if syntmp-tmp-1429
                    (apply (lambda (syntmp-_-1430
                                    syntmp-val-1431
                                    syntmp-key-1432
                                    syntmp-m-1433)
                             (if (andmap
                                   (lambda (syntmp-x-1434)
                                     (and (syntmp-id?-119 syntmp-x-1434)
                                          (not (syntmp-ellipsis?-164
                                                 syntmp-x-1434))))
                                   syntmp-key-1432)
                               (let ((syntmp-x-1436
                                       (syntmp-gen-var-167 (quote tmp))))
                                 (syntmp-build-annotated-96
                                   syntmp-s-1425
                                   (list (syntmp-build-annotated-96
                                           #f
                                           (list 'lambda
                                                 (list syntmp-x-1436)
                                                 (syntmp-gen-syntax-case-1349
                                                   (syntmp-build-annotated-96
                                                     #f
                                                     syntmp-x-1436)
                                                   syntmp-key-1432
                                                   syntmp-m-1433
                                                   syntmp-r-1423
                                                   syntmp-mod-1426)))
                                         (syntmp-chi-155
                                           syntmp-val-1431
                                           syntmp-r-1423
                                           '(())
                                           syntmp-mod-1426))))
                               (syntax-error
                                 syntmp-e-1427
                                 "invalid literals list in")))
                           syntmp-tmp-1429)
                    (syntax-error syntmp-tmp-1428)))
                (syntax-dispatch
                  syntmp-tmp-1428
                  '(any any each-any . each-any))))
             syntmp-e-1427)))))
    (set! sc-expand
      (let ((syntmp-m-1439 (quote e))
            (syntmp-esew-1440 (quote (eval))))
        (lambda (syntmp-x-1441)
          (if (and (pair? syntmp-x-1441)
                   (equal? (car syntmp-x-1441) syntmp-noexpand-86))
            (cadr syntmp-x-1441)
            (syntmp-chi-top-154
              syntmp-x-1441
              '()
              '((top))
              syntmp-m-1439
              syntmp-esew-1440
              (module-name (current-module)))))))
    (set! sc-expand3
      (let ((syntmp-m-1442 (quote e))
            (syntmp-esew-1443 (quote (eval))))
        (lambda (syntmp-x-1445 . syntmp-rest-1444)
          (if (and (pair? syntmp-x-1445)
                   (equal? (car syntmp-x-1445) syntmp-noexpand-86))
            (cadr syntmp-x-1445)
            (syntmp-chi-top-154
              syntmp-x-1445
              '()
              '((top))
              (if (null? syntmp-rest-1444)
                syntmp-m-1442
                (car syntmp-rest-1444))
              (if (or (null? syntmp-rest-1444)
                      (null? (cdr syntmp-rest-1444)))
                syntmp-esew-1443
                (cadr syntmp-rest-1444))
              (module-name (current-module)))))))
    (set! identifier?
      (lambda (syntmp-x-1446)
        (syntmp-nonsymbol-id?-118 syntmp-x-1446)))
    (set! datum->syntax-object
      (lambda (syntmp-id-1447 syntmp-datum-1448)
        (syntmp-make-syntax-object-102
          syntmp-datum-1448
          (syntmp-syntax-object-wrap-105 syntmp-id-1447)
          #f)))
    (set! syntax-object->datum
      (lambda (syntmp-x-1449)
        (syntmp-strip-166 syntmp-x-1449 (quote (())))))
    (set! generate-temporaries
      (lambda (syntmp-ls-1450)
        (begin
          (let ((syntmp-x-1451 syntmp-ls-1450))
            (if (not (list? syntmp-x-1451))
              (syntmp-error-hook-93
                'generate-temporaries
                "invalid argument"
                syntmp-x-1451)))
          (map (lambda (syntmp-x-1452)
                 (syntmp-wrap-147 (gensym) (quote ((top))) #f))
               syntmp-ls-1450))))
    (set! free-identifier=?
      (lambda (syntmp-x-1453 syntmp-y-1454)
        (begin
          (let ((syntmp-x-1455 syntmp-x-1453))
            (if (not (syntmp-nonsymbol-id?-118 syntmp-x-1455))
              (syntmp-error-hook-93
                'free-identifier=?
                "invalid argument"
                syntmp-x-1455)))
          (let ((syntmp-x-1456 syntmp-y-1454))
            (if (not (syntmp-nonsymbol-id?-118 syntmp-x-1456))
              (syntmp-error-hook-93
                'free-identifier=?
                "invalid argument"
                syntmp-x-1456)))
          (syntmp-free-id=?-142
            syntmp-x-1453
            syntmp-y-1454))))
    (set! bound-identifier=?
      (lambda (syntmp-x-1457 syntmp-y-1458)
        (begin
          (let ((syntmp-x-1459 syntmp-x-1457))
            (if (not (syntmp-nonsymbol-id?-118 syntmp-x-1459))
              (syntmp-error-hook-93
                'bound-identifier=?
                "invalid argument"
                syntmp-x-1459)))
          (let ((syntmp-x-1460 syntmp-y-1458))
            (if (not (syntmp-nonsymbol-id?-118 syntmp-x-1460))
              (syntmp-error-hook-93
                'bound-identifier=?
                "invalid argument"
                syntmp-x-1460)))
          (syntmp-bound-id=?-143
            syntmp-x-1457
            syntmp-y-1458))))
    (set! syntax-error
      (lambda (syntmp-object-1462 . syntmp-messages-1461)
        (begin
          (for-each
            (lambda (syntmp-x-1463)
              (let ((syntmp-x-1464 syntmp-x-1463))
                (if (not (string? syntmp-x-1464))
                  (syntmp-error-hook-93
                    'syntax-error
                    "invalid argument"
                    syntmp-x-1464))))
            syntmp-messages-1461)
          (let ((syntmp-message-1465
                  (if (null? syntmp-messages-1461)
                    "invalid syntax"
                    (apply string-append syntmp-messages-1461))))
            (syntmp-error-hook-93
              #f
              syntmp-message-1465
              (syntmp-strip-166
                syntmp-object-1462
                '(())))))))
    (set! install-global-transformer
      (lambda (syntmp-sym-1466 syntmp-v-1467)
        (begin
          (let ((syntmp-x-1468 syntmp-sym-1466))
            (if (not (symbol? syntmp-x-1468))
              (syntmp-error-hook-93
                'define-syntax
                "invalid argument"
                syntmp-x-1468)))
          (let ((syntmp-x-1469 syntmp-v-1467))
            (if (not (procedure? syntmp-x-1469))
              (syntmp-error-hook-93
                'define-syntax
                "invalid argument"
                syntmp-x-1469)))
          (syntmp-global-extend-117
            'macro
            syntmp-sym-1466
            syntmp-v-1467))))
    (letrec ((syntmp-match-1474
               (lambda (syntmp-e-1475
                        syntmp-p-1476
                        syntmp-w-1477
                        syntmp-r-1478
                        syntmp-mod-1479)
                 (cond ((not syntmp-r-1478) #f)
                       ((eq? syntmp-p-1476 (quote any))
                        (cons (syntmp-wrap-147
                                syntmp-e-1475
                                syntmp-w-1477
                                syntmp-mod-1479)
                              syntmp-r-1478))
                       ((syntmp-syntax-object?-103 syntmp-e-1475)
                        (syntmp-match*-1473
                          (let ((syntmp-e-1480
                                  (syntmp-syntax-object-expression-104
                                    syntmp-e-1475)))
                            (if (annotation? syntmp-e-1480)
                              (annotation-expression syntmp-e-1480)
                              syntmp-e-1480))
                          syntmp-p-1476
                          (syntmp-join-wraps-138
                            syntmp-w-1477
                            (syntmp-syntax-object-wrap-105 syntmp-e-1475))
                          syntmp-r-1478
                          (syntmp-syntax-object-module-106 syntmp-e-1475)))
                       (else
                        (syntmp-match*-1473
                          (let ((syntmp-e-1481 syntmp-e-1475))
                            (if (annotation? syntmp-e-1481)
                              (annotation-expression syntmp-e-1481)
                              syntmp-e-1481))
                          syntmp-p-1476
                          syntmp-w-1477
                          syntmp-r-1478
                          syntmp-mod-1479)))))
             (syntmp-match*-1473
               (lambda (syntmp-e-1482
                        syntmp-p-1483
                        syntmp-w-1484
                        syntmp-r-1485
                        syntmp-mod-1486)
                 (cond ((null? syntmp-p-1483)
                        (and (null? syntmp-e-1482) syntmp-r-1485))
                       ((pair? syntmp-p-1483)
                        (and (pair? syntmp-e-1482)
                             (syntmp-match-1474
                               (car syntmp-e-1482)
                               (car syntmp-p-1483)
                               syntmp-w-1484
                               (syntmp-match-1474
                                 (cdr syntmp-e-1482)
                                 (cdr syntmp-p-1483)
                                 syntmp-w-1484
                                 syntmp-r-1485
                                 syntmp-mod-1486)
                               syntmp-mod-1486)))
                       ((eq? syntmp-p-1483 (quote each-any))
                        (let ((syntmp-l-1487
                                (syntmp-match-each-any-1471
                                  syntmp-e-1482
                                  syntmp-w-1484
                                  syntmp-mod-1486)))
                          (and syntmp-l-1487
                               (cons syntmp-l-1487 syntmp-r-1485))))
                       (else
                        (let ((syntmp-t-1488 (vector-ref syntmp-p-1483 0)))
                          (if (memv syntmp-t-1488 (quote (each)))
                            (if (null? syntmp-e-1482)
                              (syntmp-match-empty-1472
                                (vector-ref syntmp-p-1483 1)
                                syntmp-r-1485)
                              (let ((syntmp-l-1489
                                      (syntmp-match-each-1470
                                        syntmp-e-1482
                                        (vector-ref syntmp-p-1483 1)
                                        syntmp-w-1484
                                        syntmp-mod-1486)))
                                (and syntmp-l-1489
                                     (let syntmp-collect-1490 ((syntmp-l-1491
                                                                 syntmp-l-1489))
                                       (if (null? (car syntmp-l-1491))
                                         syntmp-r-1485
                                         (cons (map car syntmp-l-1491)
                                               (syntmp-collect-1490
                                                 (map cdr syntmp-l-1491))))))))
                            (if (memv syntmp-t-1488 (quote (free-id)))
                              (and (syntmp-id?-119 syntmp-e-1482)
                                   (syntmp-free-id=?-142
                                     (syntmp-wrap-147
                                       syntmp-e-1482
                                       syntmp-w-1484
                                       syntmp-mod-1486)
                                     (vector-ref syntmp-p-1483 1))
                                   syntmp-r-1485)
                              (if (memv syntmp-t-1488 (quote (atom)))
                                (and (equal?
                                       (vector-ref syntmp-p-1483 1)
                                       (syntmp-strip-166
                                         syntmp-e-1482
                                         syntmp-w-1484))
                                     syntmp-r-1485)
                                (if (memv syntmp-t-1488 (quote (vector)))
                                  (and (vector? syntmp-e-1482)
                                       (syntmp-match-1474
                                         (vector->list syntmp-e-1482)
                                         (vector-ref syntmp-p-1483 1)
                                         syntmp-w-1484
                                         syntmp-r-1485
                                         syntmp-mod-1486)))))))))))
             (syntmp-match-empty-1472
               (lambda (syntmp-p-1492 syntmp-r-1493)
                 (cond ((null? syntmp-p-1492) syntmp-r-1493)
                       ((eq? syntmp-p-1492 (quote any))
                        (cons (quote ()) syntmp-r-1493))
                       ((pair? syntmp-p-1492)
                        (syntmp-match-empty-1472
                          (car syntmp-p-1492)
                          (syntmp-match-empty-1472
                            (cdr syntmp-p-1492)
                            syntmp-r-1493)))
                       ((eq? syntmp-p-1492 (quote each-any))
                        (cons (quote ()) syntmp-r-1493))
                       (else
                        (let ((syntmp-t-1494 (vector-ref syntmp-p-1492 0)))
                          (if (memv syntmp-t-1494 (quote (each)))
                            (syntmp-match-empty-1472
                              (vector-ref syntmp-p-1492 1)
                              syntmp-r-1493)
                            (if (memv syntmp-t-1494 (quote (free-id atom)))
                              syntmp-r-1493
                              (if (memv syntmp-t-1494 (quote (vector)))
                                (syntmp-match-empty-1472
                                  (vector-ref syntmp-p-1492 1)
                                  syntmp-r-1493)))))))))
             (syntmp-match-each-any-1471
               (lambda (syntmp-e-1495 syntmp-w-1496 syntmp-mod-1497)
                 (cond ((annotation? syntmp-e-1495)
                        (syntmp-match-each-any-1471
                          (annotation-expression syntmp-e-1495)
                          syntmp-w-1496
                          syntmp-mod-1497))
                       ((pair? syntmp-e-1495)
                        (let ((syntmp-l-1498
                                (syntmp-match-each-any-1471
                                  (cdr syntmp-e-1495)
                                  syntmp-w-1496
                                  syntmp-mod-1497)))
                          (and syntmp-l-1498
                               (cons (syntmp-wrap-147
                                       (car syntmp-e-1495)
                                       syntmp-w-1496
                                       syntmp-mod-1497)
                                     syntmp-l-1498))))
                       ((null? syntmp-e-1495) (quote ()))
                       ((syntmp-syntax-object?-103 syntmp-e-1495)
                        (syntmp-match-each-any-1471
                          (syntmp-syntax-object-expression-104
                            syntmp-e-1495)
                          (syntmp-join-wraps-138
                            syntmp-w-1496
                            (syntmp-syntax-object-wrap-105 syntmp-e-1495))
                          syntmp-mod-1497))
                       (else #f))))
             (syntmp-match-each-1470
               (lambda (syntmp-e-1499
                        syntmp-p-1500
                        syntmp-w-1501
                        syntmp-mod-1502)
                 (cond ((annotation? syntmp-e-1499)
                        (syntmp-match-each-1470
                          (annotation-expression syntmp-e-1499)
                          syntmp-p-1500
                          syntmp-w-1501
                          syntmp-mod-1502))
                       ((pair? syntmp-e-1499)
                        (let ((syntmp-first-1503
                                (syntmp-match-1474
                                  (car syntmp-e-1499)
                                  syntmp-p-1500
                                  syntmp-w-1501
                                  '()
                                  syntmp-mod-1502)))
                          (and syntmp-first-1503
                               (let ((syntmp-rest-1504
                                       (syntmp-match-each-1470
                                         (cdr syntmp-e-1499)
                                         syntmp-p-1500
                                         syntmp-w-1501
                                         syntmp-mod-1502)))
                                 (and syntmp-rest-1504
                                      (cons syntmp-first-1503
                                            syntmp-rest-1504))))))
                       ((null? syntmp-e-1499) (quote ()))
                       ((syntmp-syntax-object?-103 syntmp-e-1499)
                        (syntmp-match-each-1470
                          (syntmp-syntax-object-expression-104
                            syntmp-e-1499)
                          syntmp-p-1500
                          (syntmp-join-wraps-138
                            syntmp-w-1501
                            (syntmp-syntax-object-wrap-105 syntmp-e-1499))
                          (syntmp-syntax-object-module-106 syntmp-e-1499)))
                       (else #f)))))
      (begin
        (set! syntax-dispatch
          (lambda (syntmp-e-1505 syntmp-p-1506)
            (cond ((eq? syntmp-p-1506 (quote any))
                   (list syntmp-e-1505))
                  ((syntmp-syntax-object?-103 syntmp-e-1505)
                   (syntmp-match*-1473
                     (let ((syntmp-e-1507
                             (syntmp-syntax-object-expression-104
                               syntmp-e-1505)))
                       (if (annotation? syntmp-e-1507)
                         (annotation-expression syntmp-e-1507)
                         syntmp-e-1507))
                     syntmp-p-1506
                     (syntmp-syntax-object-wrap-105 syntmp-e-1505)
                     '()
                     (syntmp-syntax-object-module-106 syntmp-e-1505)))
                  (else
                   (syntmp-match*-1473
                     (let ((syntmp-e-1508 syntmp-e-1505))
                       (if (annotation? syntmp-e-1508)
                         (annotation-expression syntmp-e-1508)
                         syntmp-e-1508))
                     syntmp-p-1506
                     '(())
                     '()
                     #f)))))
        (set! sc-chi syntmp-chi-155)))))
(install-global-transformer
  'with-syntax
  (lambda (syntmp-x-1509)
    ((lambda (syntmp-tmp-1510)
       ((lambda (syntmp-tmp-1511)
          (if syntmp-tmp-1511
            (apply (lambda (syntmp-_-1512 syntmp-e1-1513 syntmp-e2-1514)
                     (cons '#(syntax-object
                              begin
                              ((top)
                               #(ribcage
                                 #(_ e1 e2)
                                 #((top) (top) (top))
                                 #("i" "i" "i"))
                               #(ribcage () () ())
                               #(ribcage #(x) #((top)) #("i")))
                              (ice-9 syncase))
                           (cons syntmp-e1-1513 syntmp-e2-1514)))
                   syntmp-tmp-1511)
            ((lambda (syntmp-tmp-1516)
               (if syntmp-tmp-1516
                 (apply (lambda (syntmp-_-1517
                                 syntmp-out-1518
                                 syntmp-in-1519
                                 syntmp-e1-1520
                                 syntmp-e2-1521)
                          (list '#(syntax-object
                                   syntax-case
                                   ((top)
                                    #(ribcage
                                      #(_ out in e1 e2)
                                      #((top) (top) (top) (top) (top))
                                      #("i" "i" "i" "i" "i"))
                                    #(ribcage () () ())
                                    #(ribcage #(x) #((top)) #("i")))
                                   (ice-9 syncase))
                                syntmp-in-1519
                                '()
                                (list syntmp-out-1518
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
                                               (ice-9 syncase))
                                            (cons syntmp-e1-1520
                                                  syntmp-e2-1521)))))
                        syntmp-tmp-1516)
                 ((lambda (syntmp-tmp-1523)
                    (if syntmp-tmp-1523
                      (apply (lambda (syntmp-_-1524
                                      syntmp-out-1525
                                      syntmp-in-1526
                                      syntmp-e1-1527
                                      syntmp-e2-1528)
                               (list '#(syntax-object
                                        syntax-case
                                        ((top)
                                         #(ribcage
                                           #(_ out in e1 e2)
                                           #((top) (top) (top) (top) (top))
                                           #("i" "i" "i" "i" "i"))
                                         #(ribcage () () ())
                                         #(ribcage #(x) #((top)) #("i")))
                                        (ice-9 syncase))
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
                                               #(ribcage #(x) #((top)) #("i")))
                                              (ice-9 syncase))
                                           syntmp-in-1526)
                                     '()
                                     (list syntmp-out-1525
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
                                                    (ice-9 syncase))
                                                 (cons syntmp-e1-1527
                                                       syntmp-e2-1528)))))
                             syntmp-tmp-1523)
                      (syntax-error syntmp-tmp-1510)))
                  (syntax-dispatch
                    syntmp-tmp-1510
                    '(any #(each (any any)) any . each-any)))))
             (syntax-dispatch
               syntmp-tmp-1510
               '(any ((any any)) any . each-any)))))
        (syntax-dispatch
          syntmp-tmp-1510
          '(any () any . each-any))))
     syntmp-x-1509)))
(install-global-transformer
  'syntax-rules
  (lambda (syntmp-x-1550)
    ((lambda (syntmp-tmp-1551)
       ((lambda (syntmp-tmp-1552)
          (if syntmp-tmp-1552
            (apply (lambda (syntmp-_-1553
                            syntmp-k-1554
                            syntmp-keyword-1555
                            syntmp-pattern-1556
                            syntmp-template-1557)
                     (list '#(syntax-object
                              lambda
                              ((top)
                               #(ribcage
                                 #(_ k keyword pattern template)
                                 #((top) (top) (top) (top) (top))
                                 #("i" "i" "i" "i" "i"))
                               #(ribcage () () ())
                               #(ribcage #(x) #((top)) #("i")))
                              (ice-9 syncase))
                           '(#(syntax-object
                               x
                               ((top)
                                #(ribcage
                                  #(_ k keyword pattern template)
                                  #((top) (top) (top) (top) (top))
                                  #("i" "i" "i" "i" "i"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i")))
                               (ice-9 syncase)))
                           (cons '#(syntax-object
                                    syntax-case
                                    ((top)
                                     #(ribcage
                                       #(_ k keyword pattern template)
                                       #((top) (top) (top) (top) (top))
                                       #("i" "i" "i" "i" "i"))
                                     #(ribcage () () ())
                                     #(ribcage #(x) #((top)) #("i")))
                                    (ice-9 syncase))
                                 (cons '#(syntax-object
                                          x
                                          ((top)
                                           #(ribcage
                                             #(_ k keyword pattern template)
                                             #((top) (top) (top) (top) (top))
                                             #("i" "i" "i" "i" "i"))
                                           #(ribcage () () ())
                                           #(ribcage #(x) #((top)) #("i")))
                                          (ice-9 syncase))
                                       (cons syntmp-k-1554
                                             (map (lambda (syntmp-tmp-1560
                                                           syntmp-tmp-1559)
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
                                                                   (ice-9 syncase))
                                                                syntmp-tmp-1559)
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
                                                                   (ice-9 syncase))
                                                                syntmp-tmp-1560)))
                                                  syntmp-template-1557
                                                  syntmp-pattern-1556))))))
                   syntmp-tmp-1552)
            (syntax-error syntmp-tmp-1551)))
        (syntax-dispatch
          syntmp-tmp-1551
          '(any each-any . #(each ((any . any) any))))))
     syntmp-x-1550)))
(install-global-transformer
  'let*
  (lambda (syntmp-x-1571)
    ((lambda (syntmp-tmp-1572)
       ((lambda (syntmp-tmp-1573)
          (if (if syntmp-tmp-1573
                (apply (lambda (syntmp-let*-1574
                                syntmp-x-1575
                                syntmp-v-1576
                                syntmp-e1-1577
                                syntmp-e2-1578)
                         (andmap identifier? syntmp-x-1575))
                       syntmp-tmp-1573)
                #f)
            (apply (lambda (syntmp-let*-1580
                            syntmp-x-1581
                            syntmp-v-1582
                            syntmp-e1-1583
                            syntmp-e2-1584)
                     (let syntmp-f-1585 ((syntmp-bindings-1586
                                           (map list
                                                syntmp-x-1581
                                                syntmp-v-1582)))
                       (if (null? syntmp-bindings-1586)
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
                                     #((top) (top) (top) (top) (top))
                                     #("i" "i" "i" "i" "i"))
                                   #(ribcage () () ())
                                   #(ribcage #(x) #((top)) #("i")))
                                  (ice-9 syncase))
                               (cons '()
                                     (cons syntmp-e1-1583 syntmp-e2-1584)))
                         ((lambda (syntmp-tmp-1590)
                            ((lambda (syntmp-tmp-1591)
                               (if syntmp-tmp-1591
                                 (apply (lambda (syntmp-body-1592
                                                 syntmp-binding-1593)
                                          (list '#(syntax-object
                                                   let
                                                   ((top)
                                                    #(ribcage
                                                      #(body binding)
                                                      #((top) (top))
                                                      #("i" "i"))
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
                                                   (ice-9 syncase))
                                                (list syntmp-binding-1593)
                                                syntmp-body-1592))
                                        syntmp-tmp-1591)
                                 (syntax-error syntmp-tmp-1590)))
                             (syntax-dispatch
                               syntmp-tmp-1590
                               '(any any))))
                          (list (syntmp-f-1585 (cdr syntmp-bindings-1586))
                                (car syntmp-bindings-1586))))))
                   syntmp-tmp-1573)
            (syntax-error syntmp-tmp-1572)))
        (syntax-dispatch
          syntmp-tmp-1572
          '(any #(each (any any)) any . each-any))))
     syntmp-x-1571)))
(install-global-transformer
  'do
  (lambda (syntmp-orig-x-1613)
    ((lambda (syntmp-tmp-1614)
       ((lambda (syntmp-tmp-1615)
          (if syntmp-tmp-1615
            (apply (lambda (syntmp-_-1616
                            syntmp-var-1617
                            syntmp-init-1618
                            syntmp-step-1619
                            syntmp-e0-1620
                            syntmp-e1-1621
                            syntmp-c-1622)
                     ((lambda (syntmp-tmp-1623)
                        ((lambda (syntmp-tmp-1624)
                           (if syntmp-tmp-1624
                             (apply (lambda (syntmp-step-1625)
                                      ((lambda (syntmp-tmp-1626)
                                         ((lambda (syntmp-tmp-1627)
                                            (if syntmp-tmp-1627
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
                                                                (ice-9 syncase))
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
                                                                (ice-9 syncase))
                                                             (map list
                                                                  syntmp-var-1617
                                                                  syntmp-init-1618)
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
                                                                      (ice-9 syncase))
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
                                                                            (ice-9 syncase))
                                                                         syntmp-e0-1620)
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
                                                                            (ice-9 syncase))
                                                                         (append
                                                                           syntmp-c-1622
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
                                                                                          (ice-9 syncase))
                                                                                       syntmp-step-1625)))))))
                                                     syntmp-tmp-1627)
                                              ((lambda (syntmp-tmp-1632)
                                                 (if syntmp-tmp-1632
                                                   (apply (lambda (syntmp-e1-1633
                                                                   syntmp-e2-1634)
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
                                                                     (ice-9 syncase))
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
                                                                     (ice-9 syncase))
                                                                  (map list
                                                                       syntmp-var-1617
                                                                       syntmp-init-1618)
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
                                                                           (ice-9 syncase))
                                                                        syntmp-e0-1620
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
                                                                                 (ice-9 syncase))
                                                                              (cons syntmp-e1-1633
                                                                                    syntmp-e2-1634))
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
                                                                                 (ice-9 syncase))
                                                                              (append
                                                                                syntmp-c-1622
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
                                                                                               (ice-9 syncase))
                                                                                            syntmp-step-1625)))))))
                                                          syntmp-tmp-1632)
                                                   (syntax-error
                                                     syntmp-tmp-1626)))
                                               (syntax-dispatch
                                                 syntmp-tmp-1626
                                                 '(any . each-any)))))
                                          (syntax-dispatch
                                            syntmp-tmp-1626
                                            '())))
                                       syntmp-e1-1621))
                                    syntmp-tmp-1624)
                             (syntax-error syntmp-tmp-1623)))
                         (syntax-dispatch
                           syntmp-tmp-1623
                           'each-any)))
                      (map (lambda (syntmp-v-1641 syntmp-s-1642)
                             ((lambda (syntmp-tmp-1643)
                                ((lambda (syntmp-tmp-1644)
                                   (if syntmp-tmp-1644
                                     (apply (lambda () syntmp-v-1641)
                                            syntmp-tmp-1644)
                                     ((lambda (syntmp-tmp-1645)
                                        (if syntmp-tmp-1645
                                          (apply (lambda (syntmp-e-1646)
                                                   syntmp-e-1646)
                                                 syntmp-tmp-1645)
                                          ((lambda (syntmp-_-1647)
                                             (syntax-error syntmp-orig-x-1613))
                                           syntmp-tmp-1643)))
                                      (syntax-dispatch
                                        syntmp-tmp-1643
                                        '(any)))))
                                 (syntax-dispatch syntmp-tmp-1643 (quote ()))))
                              syntmp-s-1642))
                           syntmp-var-1617
                           syntmp-step-1619)))
                   syntmp-tmp-1615)
            (syntax-error syntmp-tmp-1614)))
        (syntax-dispatch
          syntmp-tmp-1614
          '(any #(each (any any . any))
                (any . each-any)
                .
                each-any))))
     syntmp-orig-x-1613)))
(install-global-transformer
  'quasiquote
  (letrec ((syntmp-quasicons-1675
             (lambda (syntmp-x-1679 syntmp-y-1680)
               ((lambda (syntmp-tmp-1681)
                  ((lambda (syntmp-tmp-1682)
                     (if syntmp-tmp-1682
                       (apply (lambda (syntmp-x-1683 syntmp-y-1684)
                                ((lambda (syntmp-tmp-1685)
                                   ((lambda (syntmp-tmp-1686)
                                      (if syntmp-tmp-1686
                                        (apply (lambda (syntmp-dy-1687)
                                                 ((lambda (syntmp-tmp-1688)
                                                    ((lambda (syntmp-tmp-1689)
                                                       (if syntmp-tmp-1689
                                                         (apply (lambda (syntmp-dx-1690)
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
                                                                           (ice-9 syncase))
                                                                        (cons syntmp-dx-1690
                                                                              syntmp-dy-1687)))
                                                                syntmp-tmp-1689)
                                                         ((lambda (syntmp-_-1691)
                                                            (if (null? syntmp-dy-1687)
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
                                                                       (ice-9 syncase))
                                                                    syntmp-x-1683)
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
                                                                       (ice-9 syncase))
                                                                    syntmp-x-1683
                                                                    syntmp-y-1684)))
                                                          syntmp-tmp-1688)))
                                                     (syntax-dispatch
                                                       syntmp-tmp-1688
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
                                                                #((top) (top))
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
                                                             (ice-9 syncase)))
                                                         any))))
                                                  syntmp-x-1683))
                                               syntmp-tmp-1686)
                                        ((lambda (syntmp-tmp-1692)
                                           (if syntmp-tmp-1692
                                             (apply (lambda (syntmp-stuff-1693)
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
                                                               (ice-9 syncase))
                                                            (cons syntmp-x-1683
                                                                  syntmp-stuff-1693)))
                                                    syntmp-tmp-1692)
                                             ((lambda (syntmp-else-1694)
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
                                                         (ice-9 syncase))
                                                      syntmp-x-1683
                                                      syntmp-y-1684))
                                              syntmp-tmp-1685)))
                                         (syntax-dispatch
                                           syntmp-tmp-1685
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
                                                    #((top) (top) (top) (top))
                                                    #("i" "i" "i" "i")))
                                                 (ice-9 syncase)))
                                             .
                                             any)))))
                                    (syntax-dispatch
                                      syntmp-tmp-1685
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
                                            (ice-9 syncase)))
                                        any))))
                                 syntmp-y-1684))
                              syntmp-tmp-1682)
                       (syntax-error syntmp-tmp-1681)))
                   (syntax-dispatch
                     syntmp-tmp-1681
                     '(any any))))
                (list syntmp-x-1679 syntmp-y-1680))))
           (syntmp-quasiappend-1676
             (lambda (syntmp-x-1695 syntmp-y-1696)
               ((lambda (syntmp-tmp-1697)
                  ((lambda (syntmp-tmp-1698)
                     (if syntmp-tmp-1698
                       (apply (lambda (syntmp-x-1699 syntmp-y-1700)
                                ((lambda (syntmp-tmp-1701)
                                   ((lambda (syntmp-tmp-1702)
                                      (if syntmp-tmp-1702
                                        (apply (lambda () syntmp-x-1699)
                                               syntmp-tmp-1702)
                                        ((lambda (syntmp-_-1703)
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
                                                    (ice-9 syncase))
                                                 syntmp-x-1699
                                                 syntmp-y-1700))
                                         syntmp-tmp-1701)))
                                    (syntax-dispatch
                                      syntmp-tmp-1701
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
                                            (ice-9 syncase)))
                                        ()))))
                                 syntmp-y-1700))
                              syntmp-tmp-1698)
                       (syntax-error syntmp-tmp-1697)))
                   (syntax-dispatch
                     syntmp-tmp-1697
                     '(any any))))
                (list syntmp-x-1695 syntmp-y-1696))))
           (syntmp-quasivector-1677
             (lambda (syntmp-x-1704)
               ((lambda (syntmp-tmp-1705)
                  ((lambda (syntmp-x-1706)
                     ((lambda (syntmp-tmp-1707)
                        ((lambda (syntmp-tmp-1708)
                           (if syntmp-tmp-1708
                             (apply (lambda (syntmp-x-1709)
                                      (list '#(syntax-object
                                               quote
                                               ((top)
                                                #(ribcage #(x) #((top)) #("i"))
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
                                               (ice-9 syncase))
                                            (list->vector syntmp-x-1709)))
                                    syntmp-tmp-1708)
                             ((lambda (syntmp-tmp-1711)
                                (if syntmp-tmp-1711
                                  (apply (lambda (syntmp-x-1712)
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
                                                    (ice-9 syncase))
                                                 syntmp-x-1712))
                                         syntmp-tmp-1711)
                                  ((lambda (syntmp-_-1714)
                                     (list '#(syntax-object
                                              list->vector
                                              ((top)
                                               #(ribcage #(_) #((top)) #("i"))
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
                                              (ice-9 syncase))
                                           syntmp-x-1706))
                                   syntmp-tmp-1707)))
                              (syntax-dispatch
                                syntmp-tmp-1707
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
                                      (ice-9 syncase)))
                                  .
                                  each-any)))))
                         (syntax-dispatch
                           syntmp-tmp-1707
                           '(#(free-id
                               #(syntax-object
                                 quote
                                 ((top)
                                  #(ribcage #(x) #((top)) #("i"))
                                  #(ribcage () () ())
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i"))
                                  #(ribcage
                                    #(quasicons quasiappend quasivector quasi)
                                    #((top) (top) (top) (top))
                                    #("i" "i" "i" "i")))
                                 (ice-9 syncase)))
                             each-any))))
                      syntmp-x-1706))
                   syntmp-tmp-1705))
                syntmp-x-1704)))
           (syntmp-quasi-1678
             (lambda (syntmp-p-1715 syntmp-lev-1716)
               ((lambda (syntmp-tmp-1717)
                  ((lambda (syntmp-tmp-1718)
                     (if syntmp-tmp-1718
                       (apply (lambda (syntmp-p-1719)
                                (if (= syntmp-lev-1716 0)
                                  syntmp-p-1719
                                  (syntmp-quasicons-1675
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
                                        (ice-9 syncase))
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
                                        (ice-9 syncase)))
                                    (syntmp-quasi-1678
                                      (list syntmp-p-1719)
                                      (- syntmp-lev-1716 1)))))
                              syntmp-tmp-1718)
                       ((lambda (syntmp-tmp-1720)
                          (if syntmp-tmp-1720
                            (apply (lambda (syntmp-p-1721 syntmp-q-1722)
                                     (if (= syntmp-lev-1716 0)
                                       (syntmp-quasiappend-1676
                                         syntmp-p-1721
                                         (syntmp-quasi-1678
                                           syntmp-q-1722
                                           syntmp-lev-1716))
                                       (syntmp-quasicons-1675
                                         (syntmp-quasicons-1675
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
                                                  #((top) (top) (top) (top))
                                                  #("i" "i" "i" "i")))
                                               (ice-9 syncase))
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
                                                  #((top) (top) (top) (top))
                                                  #("i" "i" "i" "i")))
                                               (ice-9 syncase)))
                                           (syntmp-quasi-1678
                                             (list syntmp-p-1721)
                                             (- syntmp-lev-1716 1)))
                                         (syntmp-quasi-1678
                                           syntmp-q-1722
                                           syntmp-lev-1716))))
                                   syntmp-tmp-1720)
                            ((lambda (syntmp-tmp-1723)
                               (if syntmp-tmp-1723
                                 (apply (lambda (syntmp-p-1724)
                                          (syntmp-quasicons-1675
                                            '(#(syntax-object
                                                quote
                                                ((top)
                                                 #(ribcage
                                                   #(p)
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
                                                (ice-9 syncase))
                                              #(syntax-object
                                                quasiquote
                                                ((top)
                                                 #(ribcage
                                                   #(p)
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
                                                (ice-9 syncase)))
                                            (syntmp-quasi-1678
                                              (list syntmp-p-1724)
                                              (+ syntmp-lev-1716 1))))
                                        syntmp-tmp-1723)
                                 ((lambda (syntmp-tmp-1725)
                                    (if syntmp-tmp-1725
                                      (apply (lambda (syntmp-p-1726
                                                      syntmp-q-1727)
                                               (syntmp-quasicons-1675
                                                 (syntmp-quasi-1678
                                                   syntmp-p-1726
                                                   syntmp-lev-1716)
                                                 (syntmp-quasi-1678
                                                   syntmp-q-1727
                                                   syntmp-lev-1716)))
                                             syntmp-tmp-1725)
                                      ((lambda (syntmp-tmp-1728)
                                         (if syntmp-tmp-1728
                                           (apply (lambda (syntmp-x-1729)
                                                    (syntmp-quasivector-1677
                                                      (syntmp-quasi-1678
                                                        syntmp-x-1729
                                                        syntmp-lev-1716)))
                                                  syntmp-tmp-1728)
                                           ((lambda (syntmp-p-1731)
                                              (list '#(syntax-object
                                                       quote
                                                       ((top)
                                                        #(ribcage
                                                          #(p)
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
                                                          #((top)
                                                            (top)
                                                            (top)
                                                            (top))
                                                          #("i" "i" "i" "i")))
                                                       (ice-9 syncase))
                                                    syntmp-p-1731))
                                            syntmp-tmp-1717)))
                                       (syntax-dispatch
                                         syntmp-tmp-1717
                                         '#(vector each-any)))))
                                  (syntax-dispatch
                                    syntmp-tmp-1717
                                    '(any . any)))))
                             (syntax-dispatch
                               syntmp-tmp-1717
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
                                     (ice-9 syncase)))
                                 any)))))
                        (syntax-dispatch
                          syntmp-tmp-1717
                          '((#(free-id
                               #(syntax-object
                                 unquote-splicing
                                 ((top)
                                  #(ribcage () () ())
                                  #(ribcage #(p lev) #((top) (top)) #("i" "i"))
                                  #(ribcage
                                    #(quasicons quasiappend quasivector quasi)
                                    #((top) (top) (top) (top))
                                    #("i" "i" "i" "i")))
                                 (ice-9 syncase)))
                             any)
                            .
                            any)))))
                   (syntax-dispatch
                     syntmp-tmp-1717
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
                           (ice-9 syncase)))
                       any))))
                syntmp-p-1715))))
    (lambda (syntmp-x-1732)
      ((lambda (syntmp-tmp-1733)
         ((lambda (syntmp-tmp-1734)
            (if syntmp-tmp-1734
              (apply (lambda (syntmp-_-1735 syntmp-e-1736)
                       (syntmp-quasi-1678 syntmp-e-1736 0))
                     syntmp-tmp-1734)
              (syntax-error syntmp-tmp-1733)))
          (syntax-dispatch
            syntmp-tmp-1733
            '(any any))))
       syntmp-x-1732))))
(install-global-transformer
  'include
  (lambda (syntmp-x-1796)
    (letrec ((syntmp-read-file-1797
               (lambda (syntmp-fn-1798 syntmp-k-1799)
                 (let ((syntmp-p-1800 (open-input-file syntmp-fn-1798)))
                   (let syntmp-f-1801 ((syntmp-x-1802 (read syntmp-p-1800)))
                     (if (eof-object? syntmp-x-1802)
                       (begin
                         (close-input-port syntmp-p-1800)
                         '())
                       (cons (datum->syntax-object
                               syntmp-k-1799
                               syntmp-x-1802)
                             (syntmp-f-1801 (read syntmp-p-1800)))))))))
      ((lambda (syntmp-tmp-1803)
         ((lambda (syntmp-tmp-1804)
            (if syntmp-tmp-1804
              (apply (lambda (syntmp-k-1805 syntmp-filename-1806)
                       (let ((syntmp-fn-1807
                               (syntax-object->datum syntmp-filename-1806)))
                         ((lambda (syntmp-tmp-1808)
                            ((lambda (syntmp-tmp-1809)
                               (if syntmp-tmp-1809
                                 (apply (lambda (syntmp-exp-1810)
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
                                                   (ice-9 syncase))
                                                syntmp-exp-1810))
                                        syntmp-tmp-1809)
                                 (syntax-error syntmp-tmp-1808)))
                             (syntax-dispatch
                               syntmp-tmp-1808
                               'each-any)))
                          (syntmp-read-file-1797
                            syntmp-fn-1807
                            syntmp-k-1805))))
                     syntmp-tmp-1804)
              (syntax-error syntmp-tmp-1803)))
          (syntax-dispatch
            syntmp-tmp-1803
            '(any any))))
       syntmp-x-1796))))
(install-global-transformer
  'unquote
  (lambda (syntmp-x-1827)
    ((lambda (syntmp-tmp-1828)
       ((lambda (syntmp-tmp-1829)
          (if syntmp-tmp-1829
            (apply (lambda (syntmp-_-1830 syntmp-e-1831)
                     (error 'unquote
                            "expression ,~s not valid outside of quasiquote"
                            (syntax-object->datum syntmp-e-1831)))
                   syntmp-tmp-1829)
            (syntax-error syntmp-tmp-1828)))
        (syntax-dispatch
          syntmp-tmp-1828
          '(any any))))
     syntmp-x-1827)))
(install-global-transformer
  'unquote-splicing
  (lambda (syntmp-x-1837)
    ((lambda (syntmp-tmp-1838)
       ((lambda (syntmp-tmp-1839)
          (if syntmp-tmp-1839
            (apply (lambda (syntmp-_-1840 syntmp-e-1841)
                     (error 'unquote-splicing
                            "expression ,@~s not valid outside of quasiquote"
                            (syntax-object->datum syntmp-e-1841)))
                   syntmp-tmp-1839)
            (syntax-error syntmp-tmp-1838)))
        (syntax-dispatch
          syntmp-tmp-1838
          '(any any))))
     syntmp-x-1837)))
(install-global-transformer
  'case
  (lambda (syntmp-x-1847)
    ((lambda (syntmp-tmp-1848)
       ((lambda (syntmp-tmp-1849)
          (if syntmp-tmp-1849
            (apply (lambda (syntmp-_-1850
                            syntmp-e-1851
                            syntmp-m1-1852
                            syntmp-m2-1853)
                     ((lambda (syntmp-tmp-1854)
                        ((lambda (syntmp-body-1855)
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
                                    (ice-9 syncase))
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
                                                (ice-9 syncase))
                                             syntmp-e-1851))
                                 syntmp-body-1855))
                         syntmp-tmp-1854))
                      (let syntmp-f-1856 ((syntmp-clause-1857 syntmp-m1-1852)
                                          (syntmp-clauses-1858 syntmp-m2-1853))
                        (if (null? syntmp-clauses-1858)
                          ((lambda (syntmp-tmp-1860)
                             ((lambda (syntmp-tmp-1861)
                                (if syntmp-tmp-1861
                                  (apply (lambda (syntmp-e1-1862
                                                  syntmp-e2-1863)
                                           (cons '#(syntax-object
                                                    begin
                                                    ((top)
                                                     #(ribcage
                                                       #(e1 e2)
                                                       #((top) (top))
                                                       #("i" "i"))
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
                                                    (ice-9 syncase))
                                                 (cons syntmp-e1-1862
                                                       syntmp-e2-1863)))
                                         syntmp-tmp-1861)
                                  ((lambda (syntmp-tmp-1865)
                                     (if syntmp-tmp-1865
                                       (apply (lambda (syntmp-k-1866
                                                       syntmp-e1-1867
                                                       syntmp-e2-1868)
                                                (list '#(syntax-object
                                                         if
                                                         ((top)
                                                          #(ribcage
                                                            #(k e1 e2)
                                                            #((top)
                                                              (top)
                                                              (top))
                                                            #("i" "i" "i"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(f clause clauses)
                                                            #((top)
                                                              (top)
                                                              (top))
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
                                                         (ice-9 syncase))
                                                      (list '#(syntax-object
                                                               memv
                                                               ((top)
                                                                #(ribcage
                                                                  #(k e1 e2)
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
                                                                  #(_ e m1 m2)
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
                                                               (ice-9 syncase))
                                                            '#(syntax-object
                                                               t
                                                               ((top)
                                                                #(ribcage
                                                                  #(k e1 e2)
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
                                                                  #(_ e m1 m2)
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
                                                               (ice-9 syncase))
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
                                                                     (ice-9 syncase))
                                                                  syntmp-k-1866))
                                                      (cons '#(syntax-object
                                                               begin
                                                               ((top)
                                                                #(ribcage
                                                                  #(k e1 e2)
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
                                                                  #(_ e m1 m2)
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
                                                               (ice-9 syncase))
                                                            (cons syntmp-e1-1867
                                                                  syntmp-e2-1868))))
                                              syntmp-tmp-1865)
                                       ((lambda (syntmp-_-1871)
                                          (syntax-error syntmp-x-1847))
                                        syntmp-tmp-1860)))
                                   (syntax-dispatch
                                     syntmp-tmp-1860
                                     '(each-any any . each-any)))))
                              (syntax-dispatch
                                syntmp-tmp-1860
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
                                         #((top) (top) (top) (top))
                                         #("i" "i" "i" "i"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i")))
                                      (ice-9 syncase)))
                                  any
                                  .
                                  each-any))))
                           syntmp-clause-1857)
                          ((lambda (syntmp-tmp-1872)
                             ((lambda (syntmp-rest-1873)
                                ((lambda (syntmp-tmp-1874)
                                   ((lambda (syntmp-tmp-1875)
                                      (if syntmp-tmp-1875
                                        (apply (lambda (syntmp-k-1876
                                                        syntmp-e1-1877
                                                        syntmp-e2-1878)
                                                 (list '#(syntax-object
                                                          if
                                                          ((top)
                                                           #(ribcage
                                                             #(k e1 e2)
                                                             #((top)
                                                               (top)
                                                               (top))
                                                             #("i" "i" "i"))
                                                           #(ribcage
                                                             #(rest)
                                                             #((top))
                                                             #("i"))
                                                           #(ribcage () () ())
                                                           #(ribcage
                                                             #(f
                                                               clause
                                                               clauses)
                                                             #((top)
                                                               (top)
                                                               (top))
                                                             #("i" "i" "i"))
                                                           #(ribcage
                                                             #(_ e m1 m2)
                                                             #((top)
                                                               (top)
                                                               (top)
                                                               (top))
                                                             #("i"
                                                               "i"
                                                               "i"
                                                               "i"))
                                                           #(ribcage () () ())
                                                           #(ribcage
                                                             #(x)
                                                             #((top))
                                                             #("i")))
                                                          (ice-9 syncase))
                                                       (list '#(syntax-object
                                                                memv
                                                                ((top)
                                                                 #(ribcage
                                                                   #(k e1 e2)
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
                                                                   #(_ e m1 m2)
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
                                                                (ice-9 syncase))
                                                             '#(syntax-object
                                                                t
                                                                ((top)
                                                                 #(ribcage
                                                                   #(k e1 e2)
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
                                                                   #(_ e m1 m2)
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
                                                                (ice-9 syncase))
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
                                                                      (ice-9 syncase))
                                                                   syntmp-k-1876))
                                                       (cons '#(syntax-object
                                                                begin
                                                                ((top)
                                                                 #(ribcage
                                                                   #(k e1 e2)
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
                                                                   #(_ e m1 m2)
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
                                                                (ice-9 syncase))
                                                             (cons syntmp-e1-1877
                                                                   syntmp-e2-1878))
                                                       syntmp-rest-1873))
                                               syntmp-tmp-1875)
                                        ((lambda (syntmp-_-1881)
                                           (syntax-error syntmp-x-1847))
                                         syntmp-tmp-1874)))
                                    (syntax-dispatch
                                      syntmp-tmp-1874
                                      '(each-any any . each-any))))
                                 syntmp-clause-1857))
                              syntmp-tmp-1872))
                           (syntmp-f-1856
                             (car syntmp-clauses-1858)
                             (cdr syntmp-clauses-1858)))))))
                   syntmp-tmp-1849)
            (syntax-error syntmp-tmp-1848)))
        (syntax-dispatch
          syntmp-tmp-1848
          '(any any any . each-any))))
     syntmp-x-1847)))
(install-global-transformer
  'identifier-syntax
  (lambda (syntmp-x-1911)
    ((lambda (syntmp-tmp-1912)
       ((lambda (syntmp-tmp-1913)
          (if syntmp-tmp-1913
            (apply (lambda (syntmp-_-1914 syntmp-e-1915)
                     (list '#(syntax-object
                              lambda
                              ((top)
                               #(ribcage #(_ e) #((top) (top)) #("i" "i"))
                               #(ribcage () () ())
                               #(ribcage #(x) #((top)) #("i")))
                              (ice-9 syncase))
                           '(#(syntax-object
                               x
                               ((top)
                                #(ribcage #(_ e) #((top) (top)) #("i" "i"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i")))
                               (ice-9 syncase)))
                           (list '#(syntax-object
                                    syntax-case
                                    ((top)
                                     #(ribcage
                                       #(_ e)
                                       #((top) (top))
                                       #("i" "i"))
                                     #(ribcage () () ())
                                     #(ribcage #(x) #((top)) #("i")))
                                    (ice-9 syncase))
                                 '#(syntax-object
                                    x
                                    ((top)
                                     #(ribcage
                                       #(_ e)
                                       #((top) (top))
                                       #("i" "i"))
                                     #(ribcage () () ())
                                     #(ribcage #(x) #((top)) #("i")))
                                    (ice-9 syncase))
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
                                          (ice-9 syncase))
                                       '(#(syntax-object
                                           identifier?
                                           ((top)
                                            #(ribcage
                                              #(_ e)
                                              #((top) (top))
                                              #("i" "i"))
                                            #(ribcage () () ())
                                            #(ribcage #(x) #((top)) #("i")))
                                           (ice-9 syncase))
                                         (#(syntax-object
                                            syntax
                                            ((top)
                                             #(ribcage
                                               #(_ e)
                                               #((top) (top))
                                               #("i" "i"))
                                             #(ribcage () () ())
                                             #(ribcage #(x) #((top)) #("i")))
                                            (ice-9 syncase))
                                          #(syntax-object
                                            id
                                            ((top)
                                             #(ribcage
                                               #(_ e)
                                               #((top) (top))
                                               #("i" "i"))
                                             #(ribcage () () ())
                                             #(ribcage #(x) #((top)) #("i")))
                                            (ice-9 syncase))))
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
                                                (ice-9 syncase))
                                             syntmp-e-1915))
                                 (list (cons syntmp-_-1914
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
                                                 (ice-9 syncase))
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
                                                 (ice-9 syncase))))
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
                                                (ice-9 syncase))
                                             (cons syntmp-e-1915
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
                                                       (ice-9 syncase))
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
                                                       (ice-9 syncase)))))))))
                   syntmp-tmp-1913)
            (syntax-error syntmp-tmp-1912)))
        (syntax-dispatch
          syntmp-tmp-1912
          '(any any))))
     syntmp-x-1911)))
