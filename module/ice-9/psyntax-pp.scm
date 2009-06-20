(eval-when (compile) (set-current-module (resolve-module (quote (guile)))))
(if #f #f)

(letrec ((and-map*2378
           (lambda (f2418 first2417 . rest2416)
             (let ((t2419 (null? first2417)))
               (if t2419
                 t2419
                 (if (null? rest2416)
                   (letrec ((andmap2420
                              (lambda (first2421)
                                (let ((x2422 (car first2421))
                                      (first2423 (cdr first2421)))
                                  (if (null? first2423)
                                    (f2418 x2422)
                                    (if (f2418 x2422)
                                      (andmap2420 first2423)
                                      #f))))))
                     (andmap2420 first2417))
                   (letrec ((andmap2424
                              (lambda (first2425 rest2426)
                                (let ((x2427 (car first2425))
                                      (xr2428 (map car rest2426))
                                      (first2429 (cdr first2425))
                                      (rest2430 (map cdr rest2426)))
                                  (if (null? first2429)
                                    (apply f2418 (cons x2427 xr2428))
                                    (if (apply f2418 (cons x2427 xr2428))
                                      (andmap2424 first2429 rest2430)
                                      #f))))))
                     (andmap2424 first2417 rest2416))))))))
  (letrec ((lambda-var-list2524
             (lambda (vars2648)
               (letrec ((lvl2649
                          (lambda (vars2650 ls2651 w2652)
                            (if (pair? vars2650)
                              (lvl2649
                                (cdr vars2650)
                                (cons (wrap2504 (car vars2650) w2652 #f)
                                      ls2651)
                                w2652)
                              (if (id?2476 vars2650)
                                (cons (wrap2504 vars2650 w2652 #f) ls2651)
                                (if (null? vars2650)
                                  ls2651
                                  (if (syntax-object?2460 vars2650)
                                    (lvl2649
                                      (syntax-object-expression2461 vars2650)
                                      ls2651
                                      (join-wraps2495
                                        w2652
                                        (syntax-object-wrap2462 vars2650)))
                                    (cons vars2650 ls2651))))))))
                 (lvl2649 vars2648 (quote ()) (quote (()))))))
           (gen-var2523
             (lambda (id2653)
               (let ((id2654
                       (if (syntax-object?2460 id2653)
                         (syntax-object-expression2461 id2653)
                         id2653)))
                 (gensym (symbol->string id2654)))))
           (strip2522
             (lambda (x2655 w2656)
               (if (memq (quote top) (wrap-marks2479 w2656))
                 x2655
                 (letrec ((f2657 (lambda (x2658)
                                   (if (syntax-object?2460 x2658)
                                     (strip2522
                                       (syntax-object-expression2461 x2658)
                                       (syntax-object-wrap2462 x2658))
                                     (if (pair? x2658)
                                       (let ((a2659 (f2657 (car x2658)))
                                             (d2660 (f2657 (cdr x2658))))
                                         (if (if (eq? a2659 (car x2658))
                                               (eq? d2660 (cdr x2658))
                                               #f)
                                           x2658
                                           (cons a2659 d2660)))
                                       (if (vector? x2658)
                                         (let ((old2661 (vector->list x2658)))
                                           (let ((new2662 (map f2657 old2661)))
                                             (if (and-map*2378
                                                   eq?
                                                   old2661
                                                   new2662)
                                               x2658
                                               (list->vector new2662))))
                                         x2658))))))
                   (f2657 x2655)))))
           (ellipsis?2521
             (lambda (x2663)
               (if (nonsymbol-id?2475 x2663)
                 (free-id=?2499
                   x2663
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
           (chi-void2520 (lambda () (build-void2442 #f)))
           (eval-local-transformer2519
             (lambda (expanded2664 mod2665)
               (let ((p2666 (local-eval-hook2438 expanded2664 mod2665)))
                 (if (procedure? p2666)
                   p2666
                   (syntax-violation
                     #f
                     "nonprocedure transformer"
                     p2666)))))
           (chi-local-syntax2518
             (lambda (rec?2667 e2668 r2669 w2670 s2671 mod2672 k2673)
               ((lambda (tmp2674)
                  ((lambda (tmp2675)
                     (if tmp2675
                       (apply (lambda (_2676 id2677 val2678 e12679 e22680)
                                (let ((ids2681 id2677))
                                  (if (not (valid-bound-ids?2501 ids2681))
                                    (syntax-violation
                                      #f
                                      "duplicate bound keyword"
                                      e2668)
                                    (let ((labels2683
                                            (gen-labels2482 ids2681)))
                                      (let ((new-w2684
                                              (make-binding-wrap2493
                                                ids2681
                                                labels2683
                                                w2670)))
                                        (k2673 (cons e12679 e22680)
                                               (extend-env2470
                                                 labels2683
                                                 (let ((w2686 (if rec?2667
                                                                new-w2684
                                                                w2670))
                                                       (trans-r2687
                                                         (macros-only-env2472
                                                           r2669)))
                                                   (map (lambda (x2688)
                                                          (cons 'macro
                                                                (eval-local-transformer2519
                                                                  (chi2512
                                                                    x2688
                                                                    trans-r2687
                                                                    w2686
                                                                    mod2672)
                                                                  mod2672)))
                                                        val2678))
                                                 r2669)
                                               new-w2684
                                               s2671
                                               mod2672))))))
                              tmp2675)
                       ((lambda (_2690)
                          (syntax-violation
                            #f
                            "bad local syntax definition"
                            (source-wrap2505 e2668 w2670 s2671 mod2672)))
                        tmp2674)))
                   ($sc-dispatch
                     tmp2674
                     '(any #(each (any any)) any . each-any))))
                e2668)))
           (chi-lambda-clause2517
             (lambda (e2691
                      docstring2692
                      c2693
                      r2694
                      w2695
                      mod2696
                      k2697)
               ((lambda (tmp2698)
                  ((lambda (tmp2699)
                     (if (if tmp2699
                           (apply (lambda (args2700 doc2701 e12702 e22703)
                                    (if (string? (syntax->datum doc2701))
                                      (not docstring2692)
                                      #f))
                                  tmp2699)
                           #f)
                       (apply (lambda (args2704 doc2705 e12706 e22707)
                                (chi-lambda-clause2517
                                  e2691
                                  doc2705
                                  (cons args2704 (cons e12706 e22707))
                                  r2694
                                  w2695
                                  mod2696
                                  k2697))
                              tmp2699)
                       ((lambda (tmp2709)
                          (if tmp2709
                            (apply (lambda (id2710 e12711 e22712)
                                     (let ((ids2713 id2710))
                                       (if (not (valid-bound-ids?2501 ids2713))
                                         (syntax-violation
                                           'lambda
                                           "invalid parameter list"
                                           e2691)
                                         (let ((labels2715
                                                 (gen-labels2482 ids2713))
                                               (new-vars2716
                                                 (map gen-var2523 ids2713)))
                                           (k2697 (map syntax->datum ids2713)
                                                  new-vars2716
                                                  (if docstring2692
                                                    (syntax->datum
                                                      docstring2692)
                                                    #f)
                                                  (chi-body2516
                                                    (cons e12711 e22712)
                                                    e2691
                                                    (extend-var-env2471
                                                      labels2715
                                                      new-vars2716
                                                      r2694)
                                                    (make-binding-wrap2493
                                                      ids2713
                                                      labels2715
                                                      w2695)
                                                    mod2696))))))
                                   tmp2709)
                            ((lambda (tmp2718)
                               (if tmp2718
                                 (apply (lambda (ids2719 e12720 e22721)
                                          (let ((old-ids2722
                                                  (lambda-var-list2524
                                                    ids2719)))
                                            (if (not (valid-bound-ids?2501
                                                       old-ids2722))
                                              (syntax-violation
                                                'lambda
                                                "invalid parameter list"
                                                e2691)
                                              (let ((labels2723
                                                      (gen-labels2482
                                                        old-ids2722))
                                                    (new-vars2724
                                                      (map gen-var2523
                                                           old-ids2722)))
                                                (k2697 (letrec ((f2725 (lambda (ls12726
                                                                                ls22727)
                                                                         (if (null? ls12726)
                                                                           (syntax->datum
                                                                             ls22727)
                                                                           (f2725 (cdr ls12726)
                                                                                  (cons (syntax->datum
                                                                                          (car ls12726))
                                                                                        ls22727))))))
                                                         (f2725 (cdr old-ids2722)
                                                                (car old-ids2722)))
                                                       (letrec ((f2728 (lambda (ls12729
                                                                                ls22730)
                                                                         (if (null? ls12729)
                                                                           ls22730
                                                                           (f2728 (cdr ls12729)
                                                                                  (cons (car ls12729)
                                                                                        ls22730))))))
                                                         (f2728 (cdr new-vars2724)
                                                                (car new-vars2724)))
                                                       (if docstring2692
                                                         (syntax->datum
                                                           docstring2692)
                                                         #f)
                                                       (chi-body2516
                                                         (cons e12720 e22721)
                                                         e2691
                                                         (extend-var-env2471
                                                           labels2723
                                                           new-vars2724
                                                           r2694)
                                                         (make-binding-wrap2493
                                                           old-ids2722
                                                           labels2723
                                                           w2695)
                                                         mod2696))))))
                                        tmp2718)
                                 ((lambda (_2732)
                                    (syntax-violation
                                      'lambda
                                      "bad lambda"
                                      e2691))
                                  tmp2698)))
                             ($sc-dispatch
                               tmp2698
                               '(any any . each-any)))))
                        ($sc-dispatch
                          tmp2698
                          '(each-any any . each-any)))))
                   ($sc-dispatch
                     tmp2698
                     '(any any any . each-any))))
                c2693)))
           (chi-body2516
             (lambda (body2733 outer-form2734 r2735 w2736 mod2737)
               (let ((r2738 (cons (quote ("placeholder" placeholder)) r2735)))
                 (let ((ribcage2739
                         (make-ribcage2483
                           '()
                           '()
                           '())))
                   (let ((w2740 (make-wrap2478
                                  (wrap-marks2479 w2736)
                                  (cons ribcage2739 (wrap-subst2480 w2736)))))
                     (letrec ((parse2741
                                (lambda (body2742
                                         ids2743
                                         labels2744
                                         var-ids2745
                                         vars2746
                                         vals2747
                                         bindings2748)
                                  (if (null? body2742)
                                    (syntax-violation
                                      #f
                                      "no expressions in body"
                                      outer-form2734)
                                    (let ((e2750 (cdar body2742))
                                          (er2751 (caar body2742)))
                                      (call-with-values
                                        (lambda ()
                                          (syntax-type2510
                                            e2750
                                            er2751
                                            '(())
                                            (source-annotation2467 er2751)
                                            ribcage2739
                                            mod2737
                                            #f))
                                        (lambda (type2752
                                                 value2753
                                                 e2754
                                                 w2755
                                                 s2756
                                                 mod2757)
                                          (if (memv type2752
                                                    '(define-form))
                                            (let ((id2758
                                                    (wrap2504
                                                      value2753
                                                      w2755
                                                      mod2757))
                                                  (label2759 (gen-label2481)))
                                              (let ((var2760
                                                      (gen-var2523 id2758)))
                                                (begin
                                                  (extend-ribcage!2492
                                                    ribcage2739
                                                    id2758
                                                    label2759)
                                                  (parse2741
                                                    (cdr body2742)
                                                    (cons id2758 ids2743)
                                                    (cons label2759 labels2744)
                                                    (cons id2758 var-ids2745)
                                                    (cons var2760 vars2746)
                                                    (cons (cons er2751
                                                                (wrap2504
                                                                  e2754
                                                                  w2755
                                                                  mod2757))
                                                          vals2747)
                                                    (cons (cons 'lexical
                                                                var2760)
                                                          bindings2748)))))
                                            (if (memv type2752
                                                      '(define-syntax-form))
                                              (let ((id2761
                                                      (wrap2504
                                                        value2753
                                                        w2755
                                                        mod2757))
                                                    (label2762
                                                      (gen-label2481)))
                                                (begin
                                                  (extend-ribcage!2492
                                                    ribcage2739
                                                    id2761
                                                    label2762)
                                                  (parse2741
                                                    (cdr body2742)
                                                    (cons id2761 ids2743)
                                                    (cons label2762 labels2744)
                                                    var-ids2745
                                                    vars2746
                                                    vals2747
                                                    (cons (cons 'macro
                                                                (cons er2751
                                                                      (wrap2504
                                                                        e2754
                                                                        w2755
                                                                        mod2757)))
                                                          bindings2748))))
                                              (if (memv type2752
                                                        '(begin-form))
                                                ((lambda (tmp2763)
                                                   ((lambda (tmp2764)
                                                      (if tmp2764
                                                        (apply (lambda (_2765
                                                                        e12766)
                                                                 (parse2741
                                                                   (letrec ((f2767 (lambda (forms2768)
                                                                                     (if (null? forms2768)
                                                                                       (cdr body2742)
                                                                                       (cons (cons er2751
                                                                                                   (wrap2504
                                                                                                     (car forms2768)
                                                                                                     w2755
                                                                                                     mod2757))
                                                                                             (f2767 (cdr forms2768)))))))
                                                                     (f2767 e12766))
                                                                   ids2743
                                                                   labels2744
                                                                   var-ids2745
                                                                   vars2746
                                                                   vals2747
                                                                   bindings2748))
                                                               tmp2764)
                                                        (syntax-violation
                                                          #f
                                                          "source expression failed to match any pattern"
                                                          tmp2763)))
                                                    ($sc-dispatch
                                                      tmp2763
                                                      '(any . each-any))))
                                                 e2754)
                                                (if (memv type2752
                                                          '(local-syntax-form))
                                                  (chi-local-syntax2518
                                                    value2753
                                                    e2754
                                                    er2751
                                                    w2755
                                                    s2756
                                                    mod2757
                                                    (lambda (forms2770
                                                             er2771
                                                             w2772
                                                             s2773
                                                             mod2774)
                                                      (parse2741
                                                        (letrec ((f2775 (lambda (forms2776)
                                                                          (if (null? forms2776)
                                                                            (cdr body2742)
                                                                            (cons (cons er2771
                                                                                        (wrap2504
                                                                                          (car forms2776)
                                                                                          w2772
                                                                                          mod2774))
                                                                                  (f2775 (cdr forms2776)))))))
                                                          (f2775 forms2770))
                                                        ids2743
                                                        labels2744
                                                        var-ids2745
                                                        vars2746
                                                        vals2747
                                                        bindings2748)))
                                                  (if (null? ids2743)
                                                    (build-sequence2455
                                                      #f
                                                      (map (lambda (x2777)
                                                             (chi2512
                                                               (cdr x2777)
                                                               (car x2777)
                                                               '(())
                                                               mod2757))
                                                           (cons (cons er2751
                                                                       (source-wrap2505
                                                                         e2754
                                                                         w2755
                                                                         s2756
                                                                         mod2757))
                                                                 (cdr body2742))))
                                                    (begin
                                                      (if (not (valid-bound-ids?2501
                                                                 ids2743))
                                                        (syntax-violation
                                                          #f
                                                          "invalid or duplicate identifier in definition"
                                                          outer-form2734))
                                                      (letrec ((loop2778
                                                                 (lambda (bs2779
                                                                          er-cache2780
                                                                          r-cache2781)
                                                                   (if (not (null? bs2779))
                                                                     (let ((b2782 (car bs2779)))
                                                                       (if (eq? (car b2782)
                                                                                'macro)
                                                                         (let ((er2783
                                                                                 (cadr b2782)))
                                                                           (let ((r-cache2784
                                                                                   (if (eq? er2783
                                                                                            er-cache2780)
                                                                                     r-cache2781
                                                                                     (macros-only-env2472
                                                                                       er2783))))
                                                                             (begin
                                                                               (set-cdr!
                                                                                 b2782
                                                                                 (eval-local-transformer2519
                                                                                   (chi2512
                                                                                     (cddr b2782)
                                                                                     r-cache2784
                                                                                     '(())
                                                                                     mod2757)
                                                                                   mod2757))
                                                                               (loop2778
                                                                                 (cdr bs2779)
                                                                                 er2783
                                                                                 r-cache2784))))
                                                                         (loop2778
                                                                           (cdr bs2779)
                                                                           er-cache2780
                                                                           r-cache2781)))))))
                                                        (loop2778
                                                          bindings2748
                                                          #f
                                                          #f))
                                                      (set-cdr!
                                                        r2738
                                                        (extend-env2470
                                                          labels2744
                                                          bindings2748
                                                          (cdr r2738)))
                                                      (build-letrec2458
                                                        #f
                                                        (map syntax->datum
                                                             var-ids2745)
                                                        vars2746
                                                        (map (lambda (x2785)
                                                               (chi2512
                                                                 (cdr x2785)
                                                                 (car x2785)
                                                                 '(())
                                                                 mod2757))
                                                             vals2747)
                                                        (build-sequence2455
                                                          #f
                                                          (map (lambda (x2786)
                                                                 (chi2512
                                                                   (cdr x2786)
                                                                   (car x2786)
                                                                   '(())
                                                                   mod2757))
                                                               (cons (cons er2751
                                                                           (source-wrap2505
                                                                             e2754
                                                                             w2755
                                                                             s2756
                                                                             mod2757))
                                                                     (cdr body2742))))))))))))))))))
                       (parse2741
                         (map (lambda (x2749)
                                (cons r2738 (wrap2504 x2749 w2740 mod2737)))
                              body2733)
                         '()
                         '()
                         '()
                         '()
                         '()
                         '())))))))
           (chi-macro2515
             (lambda (p2787 e2788 r2789 w2790 rib2791 mod2792)
               (letrec ((rebuild-macro-output2793
                          (lambda (x2794 m2795)
                            (if (pair? x2794)
                              (cons (rebuild-macro-output2793
                                      (car x2794)
                                      m2795)
                                    (rebuild-macro-output2793
                                      (cdr x2794)
                                      m2795))
                              (if (syntax-object?2460 x2794)
                                (let ((w2796 (syntax-object-wrap2462 x2794)))
                                  (let ((ms2797 (wrap-marks2479 w2796))
                                        (s2798 (wrap-subst2480 w2796)))
                                    (if (if (pair? ms2797)
                                          (eq? (car ms2797) #f)
                                          #f)
                                      (make-syntax-object2459
                                        (syntax-object-expression2461 x2794)
                                        (make-wrap2478
                                          (cdr ms2797)
                                          (if rib2791
                                            (cons rib2791 (cdr s2798))
                                            (cdr s2798)))
                                        (syntax-object-module2463 x2794))
                                      (make-syntax-object2459
                                        (syntax-object-expression2461 x2794)
                                        (make-wrap2478
                                          (cons m2795 ms2797)
                                          (if rib2791
                                            (cons rib2791
                                                  (cons (quote shift) s2798))
                                            (cons (quote shift) s2798)))
                                        (let ((pmod2799
                                                (procedure-module p2787)))
                                          (if pmod2799
                                            (cons 'hygiene
                                                  (module-name pmod2799))
                                            '(hygiene guile)))))))
                                (if (vector? x2794)
                                  (let ((n2800 (vector-length x2794)))
                                    (let ((v2801 (make-vector n2800)))
                                      (letrec ((loop2802
                                                 (lambda (i2803)
                                                   (if (fx=2435 i2803 n2800)
                                                     (begin (if #f #f) v2801)
                                                     (begin
                                                       (vector-set!
                                                         v2801
                                                         i2803
                                                         (rebuild-macro-output2793
                                                           (vector-ref
                                                             x2794
                                                             i2803)
                                                           m2795))
                                                       (loop2802
                                                         (fx+2433
                                                           i2803
                                                           1)))))))
                                        (loop2802 0))))
                                  (if (symbol? x2794)
                                    (syntax-violation
                                      #f
                                      "encountered raw symbol in macro output"
                                      (source-wrap2505 e2788 w2790 s mod2792)
                                      x2794)
                                    x2794)))))))
                 (rebuild-macro-output2793
                   (p2787 (wrap2504 e2788 (anti-mark2491 w2790) mod2792))
                   (string #\m)))))
           (chi-application2514
             (lambda (x2804 e2805 r2806 w2807 s2808 mod2809)
               ((lambda (tmp2810)
                  ((lambda (tmp2811)
                     (if tmp2811
                       (apply (lambda (e02812 e12813)
                                (build-application2443
                                  s2808
                                  x2804
                                  (map (lambda (e2814)
                                         (chi2512 e2814 r2806 w2807 mod2809))
                                       e12813)))
                              tmp2811)
                       (syntax-violation
                         #f
                         "source expression failed to match any pattern"
                         tmp2810)))
                   ($sc-dispatch tmp2810 (quote (any . each-any)))))
                e2805)))
           (chi-expr2513
             (lambda (type2816
                      value2817
                      e2818
                      r2819
                      w2820
                      s2821
                      mod2822)
               (if (memv type2816 (quote (lexical)))
                 (build-lexical-reference2445
                   'value
                   s2821
                   e2818
                   value2817)
                 (if (memv type2816 (quote (core core-form)))
                   (value2817 e2818 r2819 w2820 s2821 mod2822)
                   (if (memv type2816 (quote (module-ref)))
                     (call-with-values
                       (lambda () (value2817 e2818))
                       (lambda (id2823 mod2824)
                         (build-global-reference2448 s2821 id2823 mod2824)))
                     (if (memv type2816 (quote (lexical-call)))
                       (chi-application2514
                         (build-lexical-reference2445
                           'fun
                           (source-annotation2467 (car e2818))
                           (car e2818)
                           value2817)
                         e2818
                         r2819
                         w2820
                         s2821
                         mod2822)
                       (if (memv type2816 (quote (global-call)))
                         (chi-application2514
                           (build-global-reference2448
                             (source-annotation2467 (car e2818))
                             (if (syntax-object?2460 value2817)
                               (syntax-object-expression2461 value2817)
                               value2817)
                             (if (syntax-object?2460 value2817)
                               (syntax-object-module2463 value2817)
                               mod2822))
                           e2818
                           r2819
                           w2820
                           s2821
                           mod2822)
                         (if (memv type2816 (quote (constant)))
                           (build-data2454
                             s2821
                             (strip2522
                               (source-wrap2505 e2818 w2820 s2821 mod2822)
                               '(())))
                           (if (memv type2816 (quote (global)))
                             (build-global-reference2448
                               s2821
                               value2817
                               mod2822)
                             (if (memv type2816 (quote (call)))
                               (chi-application2514
                                 (chi2512 (car e2818) r2819 w2820 mod2822)
                                 e2818
                                 r2819
                                 w2820
                                 s2821
                                 mod2822)
                               (if (memv type2816 (quote (begin-form)))
                                 ((lambda (tmp2825)
                                    ((lambda (tmp2826)
                                       (if tmp2826
                                         (apply (lambda (_2827 e12828 e22829)
                                                  (chi-sequence2506
                                                    (cons e12828 e22829)
                                                    r2819
                                                    w2820
                                                    s2821
                                                    mod2822))
                                                tmp2826)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           tmp2825)))
                                     ($sc-dispatch
                                       tmp2825
                                       '(any any . each-any))))
                                  e2818)
                                 (if (memv type2816
                                           '(local-syntax-form))
                                   (chi-local-syntax2518
                                     value2817
                                     e2818
                                     r2819
                                     w2820
                                     s2821
                                     mod2822
                                     chi-sequence2506)
                                   (if (memv type2816 (quote (eval-when-form)))
                                     ((lambda (tmp2831)
                                        ((lambda (tmp2832)
                                           (if tmp2832
                                             (apply (lambda (_2833
                                                             x2834
                                                             e12835
                                                             e22836)
                                                      (let ((when-list2837
                                                              (chi-when-list2509
                                                                e2818
                                                                x2834
                                                                w2820)))
                                                        (if (memq 'eval
                                                                  when-list2837)
                                                          (chi-sequence2506
                                                            (cons e12835
                                                                  e22836)
                                                            r2819
                                                            w2820
                                                            s2821
                                                            mod2822)
                                                          (chi-void2520))))
                                                    tmp2832)
                                             (syntax-violation
                                               #f
                                               "source expression failed to match any pattern"
                                               tmp2831)))
                                         ($sc-dispatch
                                           tmp2831
                                           '(any each-any any . each-any))))
                                      e2818)
                                     (if (memv type2816
                                               '(define-form
                                                  define-syntax-form))
                                       (syntax-violation
                                         #f
                                         "definition in expression context"
                                         e2818
                                         (wrap2504 value2817 w2820 mod2822))
                                       (if (memv type2816 (quote (syntax)))
                                         (syntax-violation
                                           #f
                                           "reference to pattern variable outside syntax form"
                                           (source-wrap2505
                                             e2818
                                             w2820
                                             s2821
                                             mod2822))
                                         (if (memv type2816
                                                   '(displaced-lexical))
                                           (syntax-violation
                                             #f
                                             "reference to identifier outside its scope"
                                             (source-wrap2505
                                               e2818
                                               w2820
                                               s2821
                                               mod2822))
                                           (syntax-violation
                                             #f
                                             "unexpected syntax"
                                             (source-wrap2505
                                               e2818
                                               w2820
                                               s2821
                                               mod2822))))))))))))))))))
           (chi2512
             (lambda (e2840 r2841 w2842 mod2843)
               (call-with-values
                 (lambda ()
                   (syntax-type2510
                     e2840
                     r2841
                     w2842
                     (source-annotation2467 e2840)
                     #f
                     mod2843
                     #f))
                 (lambda (type2844 value2845 e2846 w2847 s2848 mod2849)
                   (chi-expr2513
                     type2844
                     value2845
                     e2846
                     r2841
                     w2847
                     s2848
                     mod2849)))))
           (chi-top2511
             (lambda (e2850 r2851 w2852 m2853 esew2854 mod2855)
               (call-with-values
                 (lambda ()
                   (syntax-type2510
                     e2850
                     r2851
                     w2852
                     (source-annotation2467 e2850)
                     #f
                     mod2855
                     #f))
                 (lambda (type2863 value2864 e2865 w2866 s2867 mod2868)
                   (if (memv type2863 (quote (begin-form)))
                     ((lambda (tmp2869)
                        ((lambda (tmp2870)
                           (if tmp2870
                             (apply (lambda (_2871) (chi-void2520)) tmp2870)
                             ((lambda (tmp2872)
                                (if tmp2872
                                  (apply (lambda (_2873 e12874 e22875)
                                           (chi-top-sequence2507
                                             (cons e12874 e22875)
                                             r2851
                                             w2866
                                             s2867
                                             m2853
                                             esew2854
                                             mod2868))
                                         tmp2872)
                                  (syntax-violation
                                    #f
                                    "source expression failed to match any pattern"
                                    tmp2869)))
                              ($sc-dispatch
                                tmp2869
                                '(any any . each-any)))))
                         ($sc-dispatch tmp2869 (quote (any)))))
                      e2865)
                     (if (memv type2863 (quote (local-syntax-form)))
                       (chi-local-syntax2518
                         value2864
                         e2865
                         r2851
                         w2866
                         s2867
                         mod2868
                         (lambda (body2877 r2878 w2879 s2880 mod2881)
                           (chi-top-sequence2507
                             body2877
                             r2878
                             w2879
                             s2880
                             m2853
                             esew2854
                             mod2881)))
                       (if (memv type2863 (quote (eval-when-form)))
                         ((lambda (tmp2882)
                            ((lambda (tmp2883)
                               (if tmp2883
                                 (apply (lambda (_2884 x2885 e12886 e22887)
                                          (let ((when-list2888
                                                  (chi-when-list2509
                                                    e2865
                                                    x2885
                                                    w2866))
                                                (body2889
                                                  (cons e12886 e22887)))
                                            (if (eq? m2853 (quote e))
                                              (if (memq 'eval
                                                        when-list2888)
                                                (chi-top-sequence2507
                                                  body2889
                                                  r2851
                                                  w2866
                                                  s2867
                                                  'e
                                                  '(eval)
                                                  mod2868)
                                                (chi-void2520))
                                              (if (memq 'load
                                                        when-list2888)
                                                (if (let ((t2892 (memq 'compile
                                                                       when-list2888)))
                                                      (if t2892
                                                        t2892
                                                        (if (eq? m2853
                                                                 'c&e)
                                                          (memq 'eval
                                                                when-list2888)
                                                          #f)))
                                                  (chi-top-sequence2507
                                                    body2889
                                                    r2851
                                                    w2866
                                                    s2867
                                                    'c&e
                                                    '(compile load)
                                                    mod2868)
                                                  (if (memq m2853
                                                            '(c c&e))
                                                    (chi-top-sequence2507
                                                      body2889
                                                      r2851
                                                      w2866
                                                      s2867
                                                      'c
                                                      '(load)
                                                      mod2868)
                                                    (chi-void2520)))
                                                (if (let ((t2893 (memq 'compile
                                                                       when-list2888)))
                                                      (if t2893
                                                        t2893
                                                        (if (eq? m2853
                                                                 'c&e)
                                                          (memq 'eval
                                                                when-list2888)
                                                          #f)))
                                                  (begin
                                                    (top-level-eval-hook2437
                                                      (chi-top-sequence2507
                                                        body2889
                                                        r2851
                                                        w2866
                                                        s2867
                                                        'e
                                                        '(eval)
                                                        mod2868)
                                                      mod2868)
                                                    (chi-void2520))
                                                  (chi-void2520))))))
                                        tmp2883)
                                 (syntax-violation
                                   #f
                                   "source expression failed to match any pattern"
                                   tmp2882)))
                             ($sc-dispatch
                               tmp2882
                               '(any each-any any . each-any))))
                          e2865)
                         (if (memv type2863 (quote (define-syntax-form)))
                           (let ((n2894 (id-var-name2498 value2864 w2866))
                                 (r2895 (macros-only-env2472 r2851)))
                             (if (memv m2853 (quote (c)))
                               (if (memq (quote compile) esew2854)
                                 (let ((e2896 (chi-install-global2508
                                                n2894
                                                (chi2512
                                                  e2865
                                                  r2895
                                                  w2866
                                                  mod2868))))
                                   (begin
                                     (top-level-eval-hook2437 e2896 mod2868)
                                     (if (memq (quote load) esew2854)
                                       e2896
                                       (chi-void2520))))
                                 (if (memq (quote load) esew2854)
                                   (chi-install-global2508
                                     n2894
                                     (chi2512 e2865 r2895 w2866 mod2868))
                                   (chi-void2520)))
                               (if (memv m2853 (quote (c&e)))
                                 (let ((e2897 (chi-install-global2508
                                                n2894
                                                (chi2512
                                                  e2865
                                                  r2895
                                                  w2866
                                                  mod2868))))
                                   (begin
                                     (top-level-eval-hook2437 e2897 mod2868)
                                     e2897))
                                 (begin
                                   (if (memq (quote eval) esew2854)
                                     (top-level-eval-hook2437
                                       (chi-install-global2508
                                         n2894
                                         (chi2512 e2865 r2895 w2866 mod2868))
                                       mod2868))
                                   (chi-void2520)))))
                           (if (memv type2863 (quote (define-form)))
                             (let ((n2898 (id-var-name2498 value2864 w2866)))
                               (let ((type2899
                                       (binding-type2468
                                         (lookup2473 n2898 r2851 mod2868))))
                                 (if (memv type2899
                                           '(global core macro module-ref))
                                   (begin
                                     (if (if (not (module-local-variable
                                                    (current-module)
                                                    n2898))
                                           (current-module)
                                           #f)
                                       (module-define!
                                         (current-module)
                                         n2898
                                         #f))
                                     (let ((x2900 (build-global-definition2451
                                                    s2867
                                                    n2898
                                                    (chi2512
                                                      e2865
                                                      r2851
                                                      w2866
                                                      mod2868))))
                                       (begin
                                         (if (eq? m2853 (quote c&e))
                                           (top-level-eval-hook2437
                                             x2900
                                             mod2868))
                                         x2900)))
                                   (if (memv type2899
                                             '(displaced-lexical))
                                     (syntax-violation
                                       #f
                                       "identifier out of context"
                                       e2865
                                       (wrap2504 value2864 w2866 mod2868))
                                     (syntax-violation
                                       #f
                                       "cannot define keyword at top level"
                                       e2865
                                       (wrap2504 value2864 w2866 mod2868))))))
                             (let ((x2901 (chi-expr2513
                                            type2863
                                            value2864
                                            e2865
                                            r2851
                                            w2866
                                            s2867
                                            mod2868)))
                               (begin
                                 (if (eq? m2853 (quote c&e))
                                   (top-level-eval-hook2437 x2901 mod2868))
                                 x2901)))))))))))
           (syntax-type2510
             (lambda (e2902
                      r2903
                      w2904
                      s2905
                      rib2906
                      mod2907
                      for-car?2908)
               (if (symbol? e2902)
                 (let ((n2909 (id-var-name2498 e2902 w2904)))
                   (let ((b2910 (lookup2473 n2909 r2903 mod2907)))
                     (let ((type2911 (binding-type2468 b2910)))
                       (if (memv type2911 (quote (lexical)))
                         (values
                           type2911
                           (binding-value2469 b2910)
                           e2902
                           w2904
                           s2905
                           mod2907)
                         (if (memv type2911 (quote (global)))
                           (values type2911 n2909 e2902 w2904 s2905 mod2907)
                           (if (memv type2911 (quote (macro)))
                             (if for-car?2908
                               (values
                                 type2911
                                 (binding-value2469 b2910)
                                 e2902
                                 w2904
                                 s2905
                                 mod2907)
                               (syntax-type2510
                                 (chi-macro2515
                                   (binding-value2469 b2910)
                                   e2902
                                   r2903
                                   w2904
                                   rib2906
                                   mod2907)
                                 r2903
                                 '(())
                                 s2905
                                 rib2906
                                 mod2907
                                 #f))
                             (values
                               type2911
                               (binding-value2469 b2910)
                               e2902
                               w2904
                               s2905
                               mod2907)))))))
                 (if (pair? e2902)
                   (let ((first2912 (car e2902)))
                     (call-with-values
                       (lambda ()
                         (syntax-type2510
                           first2912
                           r2903
                           w2904
                           s2905
                           rib2906
                           mod2907
                           #t))
                       (lambda (ftype2913
                                fval2914
                                fe2915
                                fw2916
                                fs2917
                                fmod2918)
                         (if (memv ftype2913 (quote (lexical)))
                           (values
                             'lexical-call
                             fval2914
                             e2902
                             w2904
                             s2905
                             mod2907)
                           (if (memv ftype2913 (quote (global)))
                             (values
                               'global-call
                               (make-syntax-object2459 fval2914 w2904 fmod2918)
                               e2902
                               w2904
                               s2905
                               mod2907)
                             (if (memv ftype2913 (quote (macro)))
                               (syntax-type2510
                                 (chi-macro2515
                                   fval2914
                                   e2902
                                   r2903
                                   w2904
                                   rib2906
                                   mod2907)
                                 r2903
                                 '(())
                                 s2905
                                 rib2906
                                 mod2907
                                 for-car?2908)
                               (if (memv ftype2913 (quote (module-ref)))
                                 (call-with-values
                                   (lambda () (fval2914 e2902))
                                   (lambda (sym2919 mod2920)
                                     (syntax-type2510
                                       sym2919
                                       r2903
                                       w2904
                                       s2905
                                       rib2906
                                       mod2920
                                       for-car?2908)))
                                 (if (memv ftype2913 (quote (core)))
                                   (values
                                     'core-form
                                     fval2914
                                     e2902
                                     w2904
                                     s2905
                                     mod2907)
                                   (if (memv ftype2913 (quote (local-syntax)))
                                     (values
                                       'local-syntax-form
                                       fval2914
                                       e2902
                                       w2904
                                       s2905
                                       mod2907)
                                     (if (memv ftype2913 (quote (begin)))
                                       (values
                                         'begin-form
                                         #f
                                         e2902
                                         w2904
                                         s2905
                                         mod2907)
                                       (if (memv ftype2913 (quote (eval-when)))
                                         (values
                                           'eval-when-form
                                           #f
                                           e2902
                                           w2904
                                           s2905
                                           mod2907)
                                         (if (memv ftype2913 (quote (define)))
                                           ((lambda (tmp2921)
                                              ((lambda (tmp2922)
                                                 (if (if tmp2922
                                                       (apply (lambda (_2923
                                                                       name2924
                                                                       val2925)
                                                                (id?2476
                                                                  name2924))
                                                              tmp2922)
                                                       #f)
                                                   (apply (lambda (_2926
                                                                   name2927
                                                                   val2928)
                                                            (values
                                                              'define-form
                                                              name2927
                                                              val2928
                                                              w2904
                                                              s2905
                                                              mod2907))
                                                          tmp2922)
                                                   ((lambda (tmp2929)
                                                      (if (if tmp2929
                                                            (apply (lambda (_2930
                                                                            name2931
                                                                            args2932
                                                                            e12933
                                                                            e22934)
                                                                     (if (id?2476
                                                                           name2931)
                                                                       (valid-bound-ids?2501
                                                                         (lambda-var-list2524
                                                                           args2932))
                                                                       #f))
                                                                   tmp2929)
                                                            #f)
                                                        (apply (lambda (_2935
                                                                        name2936
                                                                        args2937
                                                                        e12938
                                                                        e22939)
                                                                 (values
                                                                   'define-form
                                                                   (wrap2504
                                                                     name2936
                                                                     w2904
                                                                     mod2907)
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
                                                                         (wrap2504
                                                                           (cons args2937
                                                                                 (cons e12938
                                                                                       e22939))
                                                                           w2904
                                                                           mod2907))
                                                                   '(())
                                                                   s2905
                                                                   mod2907))
                                                               tmp2929)
                                                        ((lambda (tmp2941)
                                                           (if (if tmp2941
                                                                 (apply (lambda (_2942
                                                                                 name2943)
                                                                          (id?2476
                                                                            name2943))
                                                                        tmp2941)
                                                                 #f)
                                                             (apply (lambda (_2944
                                                                             name2945)
                                                                      (values
                                                                        'define-form
                                                                        (wrap2504
                                                                          name2945
                                                                          w2904
                                                                          mod2907)
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
                                                                        s2905
                                                                        mod2907))
                                                                    tmp2941)
                                                             (syntax-violation
                                                               #f
                                                               "source expression failed to match any pattern"
                                                               tmp2921)))
                                                         ($sc-dispatch
                                                           tmp2921
                                                           '(any any)))))
                                                    ($sc-dispatch
                                                      tmp2921
                                                      '(any (any . any)
                                                            any
                                                            .
                                                            each-any)))))
                                               ($sc-dispatch
                                                 tmp2921
                                                 '(any any any))))
                                            e2902)
                                           (if (memv ftype2913
                                                     '(define-syntax))
                                             ((lambda (tmp2946)
                                                ((lambda (tmp2947)
                                                   (if (if tmp2947
                                                         (apply (lambda (_2948
                                                                         name2949
                                                                         val2950)
                                                                  (id?2476
                                                                    name2949))
                                                                tmp2947)
                                                         #f)
                                                     (apply (lambda (_2951
                                                                     name2952
                                                                     val2953)
                                                              (values
                                                                'define-syntax-form
                                                                name2952
                                                                val2953
                                                                w2904
                                                                s2905
                                                                mod2907))
                                                            tmp2947)
                                                     (syntax-violation
                                                       #f
                                                       "source expression failed to match any pattern"
                                                       tmp2946)))
                                                 ($sc-dispatch
                                                   tmp2946
                                                   '(any any any))))
                                              e2902)
                                             (values
                                               'call
                                               #f
                                               e2902
                                               w2904
                                               s2905
                                               mod2907))))))))))))))
                   (if (syntax-object?2460 e2902)
                     (syntax-type2510
                       (syntax-object-expression2461 e2902)
                       r2903
                       (join-wraps2495
                         w2904
                         (syntax-object-wrap2462 e2902))
                       s2905
                       rib2906
                       (let ((t2954 (syntax-object-module2463 e2902)))
                         (if t2954 t2954 mod2907))
                       for-car?2908)
                     (if (self-evaluating? e2902)
                       (values
                         'constant
                         #f
                         e2902
                         w2904
                         s2905
                         mod2907)
                       (values
                         'other
                         #f
                         e2902
                         w2904
                         s2905
                         mod2907)))))))
           (chi-when-list2509
             (lambda (e2955 when-list2956 w2957)
               (letrec ((f2958 (lambda (when-list2959 situations2960)
                                 (if (null? when-list2959)
                                   situations2960
                                   (f2958 (cdr when-list2959)
                                          (cons (let ((x2961 (car when-list2959)))
                                                  (if (free-id=?2499
                                                        x2961
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
                                                    (if (free-id=?2499
                                                          x2961
                                                          '#(syntax-object
                                                             load
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
                                                      'load
                                                      (if (free-id=?2499
                                                            x2961
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
                                                                  #("i"
                                                                    "i"
                                                                    "i"))
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
                                                                  #("i"
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
                                                                  ((top) (top))
                                                                  ("i" "i")))
                                                               (hygiene
                                                                 guile)))
                                                        'eval
                                                        (syntax-violation
                                                          'eval-when
                                                          "invalid situation"
                                                          e2955
                                                          (wrap2504
                                                            x2961
                                                            w2957
                                                            #f))))))
                                                situations2960))))))
                 (f2958 when-list2956 (quote ())))))
           (chi-install-global2508
             (lambda (name2962 e2963)
               (build-global-definition2451
                 #f
                 name2962
                 (if (let ((v2964 (module-variable (current-module) name2962)))
                       (if v2964
                         (if (variable-bound? v2964)
                           (if (macro? (variable-ref v2964))
                             (not (eq? (macro-type (variable-ref v2964))
                                       'syncase-macro))
                             #f)
                           #f)
                         #f))
                   (build-application2443
                     #f
                     (build-primref2453
                       #f
                       'make-extended-syncase-macro)
                     (list (build-application2443
                             #f
                             (build-primref2453 #f (quote module-ref))
                             (list (build-application2443
                                     #f
                                     (build-primref2453
                                       #f
                                       'current-module)
                                     '())
                                   (build-data2454 #f name2962)))
                           (build-data2454 #f (quote macro))
                           e2963))
                   (build-application2443
                     #f
                     (build-primref2453 #f (quote make-syncase-macro))
                     (list (build-data2454 #f (quote macro)) e2963))))))
           (chi-top-sequence2507
             (lambda (body2965
                      r2966
                      w2967
                      s2968
                      m2969
                      esew2970
                      mod2971)
               (build-sequence2455
                 s2968
                 (letrec ((dobody2972
                            (lambda (body2973
                                     r2974
                                     w2975
                                     m2976
                                     esew2977
                                     mod2978)
                              (if (null? body2973)
                                '()
                                (let ((first2979
                                        (chi-top2511
                                          (car body2973)
                                          r2974
                                          w2975
                                          m2976
                                          esew2977
                                          mod2978)))
                                  (cons first2979
                                        (dobody2972
                                          (cdr body2973)
                                          r2974
                                          w2975
                                          m2976
                                          esew2977
                                          mod2978)))))))
                   (dobody2972
                     body2965
                     r2966
                     w2967
                     m2969
                     esew2970
                     mod2971)))))
           (chi-sequence2506
             (lambda (body2980 r2981 w2982 s2983 mod2984)
               (build-sequence2455
                 s2983
                 (letrec ((dobody2985
                            (lambda (body2986 r2987 w2988 mod2989)
                              (if (null? body2986)
                                '()
                                (let ((first2990
                                        (chi2512
                                          (car body2986)
                                          r2987
                                          w2988
                                          mod2989)))
                                  (cons first2990
                                        (dobody2985
                                          (cdr body2986)
                                          r2987
                                          w2988
                                          mod2989)))))))
                   (dobody2985 body2980 r2981 w2982 mod2984)))))
           (source-wrap2505
             (lambda (x2991 w2992 s2993 defmod2994)
               (begin
                 (if (if s2993 (pair? x2991) #f)
                   (set-source-properties! x2991 s2993))
                 (wrap2504 x2991 w2992 defmod2994))))
           (wrap2504
             (lambda (x2995 w2996 defmod2997)
               (if (if (null? (wrap-marks2479 w2996))
                     (null? (wrap-subst2480 w2996))
                     #f)
                 x2995
                 (if (syntax-object?2460 x2995)
                   (make-syntax-object2459
                     (syntax-object-expression2461 x2995)
                     (join-wraps2495
                       w2996
                       (syntax-object-wrap2462 x2995))
                     (syntax-object-module2463 x2995))
                   (if (null? x2995)
                     x2995
                     (make-syntax-object2459 x2995 w2996 defmod2997))))))
           (bound-id-member?2503
             (lambda (x2998 list2999)
               (if (not (null? list2999))
                 (let ((t3000 (bound-id=?2500 x2998 (car list2999))))
                   (if t3000
                     t3000
                     (bound-id-member?2503 x2998 (cdr list2999))))
                 #f)))
           (distinct-bound-ids?2502
             (lambda (ids3001)
               (letrec ((distinct?3002
                          (lambda (ids3003)
                            (let ((t3004 (null? ids3003)))
                              (if t3004
                                t3004
                                (if (not (bound-id-member?2503
                                           (car ids3003)
                                           (cdr ids3003)))
                                  (distinct?3002 (cdr ids3003))
                                  #f))))))
                 (distinct?3002 ids3001))))
           (valid-bound-ids?2501
             (lambda (ids3005)
               (if (letrec ((all-ids?3006
                              (lambda (ids3007)
                                (let ((t3008 (null? ids3007)))
                                  (if t3008
                                    t3008
                                    (if (id?2476 (car ids3007))
                                      (all-ids?3006 (cdr ids3007))
                                      #f))))))
                     (all-ids?3006 ids3005))
                 (distinct-bound-ids?2502 ids3005)
                 #f)))
           (bound-id=?2500
             (lambda (i3009 j3010)
               (if (if (syntax-object?2460 i3009)
                     (syntax-object?2460 j3010)
                     #f)
                 (if (eq? (syntax-object-expression2461 i3009)
                          (syntax-object-expression2461 j3010))
                   (same-marks?2497
                     (wrap-marks2479 (syntax-object-wrap2462 i3009))
                     (wrap-marks2479 (syntax-object-wrap2462 j3010)))
                   #f)
                 (eq? i3009 j3010))))
           (free-id=?2499
             (lambda (i3011 j3012)
               (if (eq? (let ((x3013 i3011))
                          (if (syntax-object?2460 x3013)
                            (syntax-object-expression2461 x3013)
                            x3013))
                        (let ((x3014 j3012))
                          (if (syntax-object?2460 x3014)
                            (syntax-object-expression2461 x3014)
                            x3014)))
                 (eq? (id-var-name2498 i3011 (quote (())))
                      (id-var-name2498 j3012 (quote (()))))
                 #f)))
           (id-var-name2498
             (lambda (id3015 w3016)
               (letrec ((search-vector-rib3019
                          (lambda (sym3025
                                   subst3026
                                   marks3027
                                   symnames3028
                                   ribcage3029)
                            (let ((n3030 (vector-length symnames3028)))
                              (letrec ((f3031 (lambda (i3032)
                                                (if (fx=2435 i3032 n3030)
                                                  (search3017
                                                    sym3025
                                                    (cdr subst3026)
                                                    marks3027)
                                                  (if (if (eq? (vector-ref
                                                                 symnames3028
                                                                 i3032)
                                                               sym3025)
                                                        (same-marks?2497
                                                          marks3027
                                                          (vector-ref
                                                            (ribcage-marks2486
                                                              ribcage3029)
                                                            i3032))
                                                        #f)
                                                    (values
                                                      (vector-ref
                                                        (ribcage-labels2487
                                                          ribcage3029)
                                                        i3032)
                                                      marks3027)
                                                    (f3031 (fx+2433
                                                             i3032
                                                             1)))))))
                                (f3031 0)))))
                        (search-list-rib3018
                          (lambda (sym3033
                                   subst3034
                                   marks3035
                                   symnames3036
                                   ribcage3037)
                            (letrec ((f3038 (lambda (symnames3039 i3040)
                                              (if (null? symnames3039)
                                                (search3017
                                                  sym3033
                                                  (cdr subst3034)
                                                  marks3035)
                                                (if (if (eq? (car symnames3039)
                                                             sym3033)
                                                      (same-marks?2497
                                                        marks3035
                                                        (list-ref
                                                          (ribcage-marks2486
                                                            ribcage3037)
                                                          i3040))
                                                      #f)
                                                  (values
                                                    (list-ref
                                                      (ribcage-labels2487
                                                        ribcage3037)
                                                      i3040)
                                                    marks3035)
                                                  (f3038 (cdr symnames3039)
                                                         (fx+2433
                                                           i3040
                                                           1)))))))
                              (f3038 symnames3036 0))))
                        (search3017
                          (lambda (sym3041 subst3042 marks3043)
                            (if (null? subst3042)
                              (values #f marks3043)
                              (let ((fst3044 (car subst3042)))
                                (if (eq? fst3044 (quote shift))
                                  (search3017
                                    sym3041
                                    (cdr subst3042)
                                    (cdr marks3043))
                                  (let ((symnames3045
                                          (ribcage-symnames2485 fst3044)))
                                    (if (vector? symnames3045)
                                      (search-vector-rib3019
                                        sym3041
                                        subst3042
                                        marks3043
                                        symnames3045
                                        fst3044)
                                      (search-list-rib3018
                                        sym3041
                                        subst3042
                                        marks3043
                                        symnames3045
                                        fst3044)))))))))
                 (if (symbol? id3015)
                   (let ((t3046 (call-with-values
                                  (lambda ()
                                    (search3017
                                      id3015
                                      (wrap-subst2480 w3016)
                                      (wrap-marks2479 w3016)))
                                  (lambda (x3048 . ignore3047) x3048))))
                     (if t3046 t3046 id3015))
                   (if (syntax-object?2460 id3015)
                     (let ((id3049 (syntax-object-expression2461 id3015))
                           (w13050 (syntax-object-wrap2462 id3015)))
                       (let ((marks3051
                               (join-marks2496
                                 (wrap-marks2479 w3016)
                                 (wrap-marks2479 w13050))))
                         (call-with-values
                           (lambda ()
                             (search3017
                               id3049
                               (wrap-subst2480 w3016)
                               marks3051))
                           (lambda (new-id3052 marks3053)
                             (let ((t3054 new-id3052))
                               (if t3054
                                 t3054
                                 (let ((t3055 (call-with-values
                                                (lambda ()
                                                  (search3017
                                                    id3049
                                                    (wrap-subst2480 w13050)
                                                    marks3053))
                                                (lambda (x3057 . ignore3056)
                                                  x3057))))
                                   (if t3055 t3055 id3049))))))))
                     (syntax-violation
                       'id-var-name
                       "invalid id"
                       id3015))))))
           (same-marks?2497
             (lambda (x3058 y3059)
               (let ((t3060 (eq? x3058 y3059)))
                 (if t3060
                   t3060
                   (if (not (null? x3058))
                     (if (not (null? y3059))
                       (if (eq? (car x3058) (car y3059))
                         (same-marks?2497 (cdr x3058) (cdr y3059))
                         #f)
                       #f)
                     #f)))))
           (join-marks2496
             (lambda (m13061 m23062)
               (smart-append2494 m13061 m23062)))
           (join-wraps2495
             (lambda (w13063 w23064)
               (let ((m13065 (wrap-marks2479 w13063))
                     (s13066 (wrap-subst2480 w13063)))
                 (if (null? m13065)
                   (if (null? s13066)
                     w23064
                     (make-wrap2478
                       (wrap-marks2479 w23064)
                       (smart-append2494 s13066 (wrap-subst2480 w23064))))
                   (make-wrap2478
                     (smart-append2494 m13065 (wrap-marks2479 w23064))
                     (smart-append2494 s13066 (wrap-subst2480 w23064)))))))
           (smart-append2494
             (lambda (m13067 m23068)
               (if (null? m23068) m13067 (append m13067 m23068))))
           (make-binding-wrap2493
             (lambda (ids3069 labels3070 w3071)
               (if (null? ids3069)
                 w3071
                 (make-wrap2478
                   (wrap-marks2479 w3071)
                   (cons (let ((labelvec3072 (list->vector labels3070)))
                           (let ((n3073 (vector-length labelvec3072)))
                             (let ((symnamevec3074 (make-vector n3073))
                                   (marksvec3075 (make-vector n3073)))
                               (begin
                                 (letrec ((f3076 (lambda (ids3077 i3078)
                                                   (if (not (null? ids3077))
                                                     (call-with-values
                                                       (lambda ()
                                                         (id-sym-name&marks2477
                                                           (car ids3077)
                                                           w3071))
                                                       (lambda (symname3079
                                                                marks3080)
                                                         (begin
                                                           (vector-set!
                                                             symnamevec3074
                                                             i3078
                                                             symname3079)
                                                           (vector-set!
                                                             marksvec3075
                                                             i3078
                                                             marks3080)
                                                           (f3076 (cdr ids3077)
                                                                  (fx+2433
                                                                    i3078
                                                                    1)))))))))
                                   (f3076 ids3069 0))
                                 (make-ribcage2483
                                   symnamevec3074
                                   marksvec3075
                                   labelvec3072)))))
                         (wrap-subst2480 w3071))))))
           (extend-ribcage!2492
             (lambda (ribcage3081 id3082 label3083)
               (begin
                 (set-ribcage-symnames!2488
                   ribcage3081
                   (cons (syntax-object-expression2461 id3082)
                         (ribcage-symnames2485 ribcage3081)))
                 (set-ribcage-marks!2489
                   ribcage3081
                   (cons (wrap-marks2479 (syntax-object-wrap2462 id3082))
                         (ribcage-marks2486 ribcage3081)))
                 (set-ribcage-labels!2490
                   ribcage3081
                   (cons label3083 (ribcage-labels2487 ribcage3081))))))
           (anti-mark2491
             (lambda (w3084)
               (make-wrap2478
                 (cons #f (wrap-marks2479 w3084))
                 (cons (quote shift) (wrap-subst2480 w3084)))))
           (set-ribcage-labels!2490
             (lambda (x3085 update3086)
               (vector-set! x3085 3 update3086)))
           (set-ribcage-marks!2489
             (lambda (x3087 update3088)
               (vector-set! x3087 2 update3088)))
           (set-ribcage-symnames!2488
             (lambda (x3089 update3090)
               (vector-set! x3089 1 update3090)))
           (ribcage-labels2487
             (lambda (x3091) (vector-ref x3091 3)))
           (ribcage-marks2486
             (lambda (x3092) (vector-ref x3092 2)))
           (ribcage-symnames2485
             (lambda (x3093) (vector-ref x3093 1)))
           (ribcage?2484
             (lambda (x3094)
               (if (vector? x3094)
                 (if (= (vector-length x3094) 4)
                   (eq? (vector-ref x3094 0) (quote ribcage))
                   #f)
                 #f)))
           (make-ribcage2483
             (lambda (symnames3095 marks3096 labels3097)
               (vector
                 'ribcage
                 symnames3095
                 marks3096
                 labels3097)))
           (gen-labels2482
             (lambda (ls3098)
               (if (null? ls3098)
                 '()
                 (cons (gen-label2481)
                       (gen-labels2482 (cdr ls3098))))))
           (gen-label2481 (lambda () (string #\i)))
           (wrap-subst2480 cdr)
           (wrap-marks2479 car)
           (make-wrap2478 cons)
           (id-sym-name&marks2477
             (lambda (x3099 w3100)
               (if (syntax-object?2460 x3099)
                 (values
                   (syntax-object-expression2461 x3099)
                   (join-marks2496
                     (wrap-marks2479 w3100)
                     (wrap-marks2479 (syntax-object-wrap2462 x3099))))
                 (values x3099 (wrap-marks2479 w3100)))))
           (id?2476
             (lambda (x3101)
               (if (symbol? x3101)
                 #t
                 (if (syntax-object?2460 x3101)
                   (symbol? (syntax-object-expression2461 x3101))
                   #f))))
           (nonsymbol-id?2475
             (lambda (x3102)
               (if (syntax-object?2460 x3102)
                 (symbol? (syntax-object-expression2461 x3102))
                 #f)))
           (global-extend2474
             (lambda (type3103 sym3104 val3105)
               (put-global-definition-hook2439
                 sym3104
                 type3103
                 val3105)))
           (lookup2473
             (lambda (x3106 r3107 mod3108)
               (let ((t3109 (assq x3106 r3107)))
                 (if t3109
                   (cdr t3109)
                   (if (symbol? x3106)
                     (let ((t3110 (get-global-definition-hook2440
                                    x3106
                                    mod3108)))
                       (if t3110 t3110 (quote (global))))
                     '(displaced-lexical))))))
           (macros-only-env2472
             (lambda (r3111)
               (if (null? r3111)
                 '()
                 (let ((a3112 (car r3111)))
                   (if (eq? (cadr a3112) (quote macro))
                     (cons a3112 (macros-only-env2472 (cdr r3111)))
                     (macros-only-env2472 (cdr r3111)))))))
           (extend-var-env2471
             (lambda (labels3113 vars3114 r3115)
               (if (null? labels3113)
                 r3115
                 (extend-var-env2471
                   (cdr labels3113)
                   (cdr vars3114)
                   (cons (cons (car labels3113)
                               (cons (quote lexical) (car vars3114)))
                         r3115)))))
           (extend-env2470
             (lambda (labels3116 bindings3117 r3118)
               (if (null? labels3116)
                 r3118
                 (extend-env2470
                   (cdr labels3116)
                   (cdr bindings3117)
                   (cons (cons (car labels3116) (car bindings3117))
                         r3118)))))
           (binding-value2469 cdr)
           (binding-type2468 car)
           (source-annotation2467
             (lambda (x3119)
               (if (syntax-object?2460 x3119)
                 (source-annotation2467
                   (syntax-object-expression2461 x3119))
                 (if (pair? x3119)
                   (let ((props3120 (source-properties x3119)))
                     (if (pair? props3120) props3120 #f))
                   #f))))
           (set-syntax-object-module!2466
             (lambda (x3121 update3122)
               (vector-set! x3121 3 update3122)))
           (set-syntax-object-wrap!2465
             (lambda (x3123 update3124)
               (vector-set! x3123 2 update3124)))
           (set-syntax-object-expression!2464
             (lambda (x3125 update3126)
               (vector-set! x3125 1 update3126)))
           (syntax-object-module2463
             (lambda (x3127) (vector-ref x3127 3)))
           (syntax-object-wrap2462
             (lambda (x3128) (vector-ref x3128 2)))
           (syntax-object-expression2461
             (lambda (x3129) (vector-ref x3129 1)))
           (syntax-object?2460
             (lambda (x3130)
               (if (vector? x3130)
                 (if (= (vector-length x3130) 4)
                   (eq? (vector-ref x3130 0) (quote syntax-object))
                   #f)
                 #f)))
           (make-syntax-object2459
             (lambda (expression3131 wrap3132 module3133)
               (vector
                 'syntax-object
                 expression3131
                 wrap3132
                 module3133)))
           (build-letrec2458
             (lambda (src3134
                      ids3135
                      vars3136
                      val-exps3137
                      body-exp3138)
               (if (null? vars3136)
                 body-exp3138
                 (let ((atom-key3139 (fluid-ref *mode*2432)))
                   (if (memv atom-key3139 (quote (c)))
                     (begin
                       (for-each
                         maybe-name-value!2450
                         ids3135
                         val-exps3137)
                       ((@ (language tree-il) make-letrec)
                        src3134
                        ids3135
                        vars3136
                        val-exps3137
                        body-exp3138))
                     (decorate-source2441
                       (list 'letrec
                             (map list vars3136 val-exps3137)
                             body-exp3138)
                       src3134))))))
           (build-named-let2457
             (lambda (src3140
                      ids3141
                      vars3142
                      val-exps3143
                      body-exp3144)
               (let ((f3145 (car vars3142))
                     (f-name3146 (car ids3141))
                     (vars3147 (cdr vars3142))
                     (ids3148 (cdr ids3141)))
                 (let ((atom-key3149 (fluid-ref *mode*2432)))
                   (if (memv atom-key3149 (quote (c)))
                     (let ((proc3150
                             (build-lambda2452
                               src3140
                               ids3148
                               vars3147
                               #f
                               body-exp3144)))
                       (begin
                         (maybe-name-value!2450 f-name3146 proc3150)
                         (for-each
                           maybe-name-value!2450
                           ids3148
                           val-exps3143)
                         ((@ (language tree-il) make-letrec)
                          src3140
                          (list f-name3146)
                          (list f3145)
                          (list proc3150)
                          (build-application2443
                            src3140
                            (build-lexical-reference2445
                              'fun
                              src3140
                              f-name3146
                              f3145)
                            val-exps3143))))
                     (decorate-source2441
                       (list 'let
                             f3145
                             (map list vars3147 val-exps3143)
                             body-exp3144)
                       src3140))))))
           (build-let2456
             (lambda (src3151
                      ids3152
                      vars3153
                      val-exps3154
                      body-exp3155)
               (if (null? vars3153)
                 body-exp3155
                 (let ((atom-key3156 (fluid-ref *mode*2432)))
                   (if (memv atom-key3156 (quote (c)))
                     (begin
                       (for-each
                         maybe-name-value!2450
                         ids3152
                         val-exps3154)
                       ((@ (language tree-il) make-let)
                        src3151
                        ids3152
                        vars3153
                        val-exps3154
                        body-exp3155))
                     (decorate-source2441
                       (list 'let
                             (map list vars3153 val-exps3154)
                             body-exp3155)
                       src3151))))))
           (build-sequence2455
             (lambda (src3157 exps3158)
               (if (null? (cdr exps3158))
                 (car exps3158)
                 (let ((atom-key3159 (fluid-ref *mode*2432)))
                   (if (memv atom-key3159 (quote (c)))
                     ((@ (language tree-il) make-sequence)
                      src3157
                      exps3158)
                     (decorate-source2441
                       (cons (quote begin) exps3158)
                       src3157))))))
           (build-data2454
             (lambda (src3160 exp3161)
               (let ((atom-key3162 (fluid-ref *mode*2432)))
                 (if (memv atom-key3162 (quote (c)))
                   ((@ (language tree-il) make-const)
                    src3160
                    exp3161)
                   (decorate-source2441
                     (if (if (self-evaluating? exp3161)
                           (not (vector? exp3161))
                           #f)
                       exp3161
                       (list (quote quote) exp3161))
                     src3160)))))
           (build-primref2453
             (lambda (src3163 name3164)
               (if (equal?
                     (module-name (current-module))
                     '(guile))
                 (let ((atom-key3165 (fluid-ref *mode*2432)))
                   (if (memv atom-key3165 (quote (c)))
                     ((@ (language tree-il) make-toplevel-ref)
                      src3163
                      name3164)
                     (decorate-source2441 name3164 src3163)))
                 (let ((atom-key3166 (fluid-ref *mode*2432)))
                   (if (memv atom-key3166 (quote (c)))
                     ((@ (language tree-il) make-module-ref)
                      src3163
                      '(guile)
                      name3164
                      #f)
                     (decorate-source2441
                       (list (quote @@) (quote (guile)) name3164)
                       src3163))))))
           (build-lambda2452
             (lambda (src3167 ids3168 vars3169 docstring3170 exp3171)
               (let ((atom-key3172 (fluid-ref *mode*2432)))
                 (if (memv atom-key3172 (quote (c)))
                   ((@ (language tree-il) make-lambda)
                    src3167
                    ids3168
                    vars3169
                    (if docstring3170
                      (list (cons (quote documentation) docstring3170))
                      '())
                    exp3171)
                   (decorate-source2441
                     (cons 'lambda
                           (cons vars3169
                                 (append
                                   (if docstring3170
                                     (list docstring3170)
                                     '())
                                   (list exp3171))))
                     src3167)))))
           (build-global-definition2451
             (lambda (source3173 var3174 exp3175)
               (let ((atom-key3176 (fluid-ref *mode*2432)))
                 (if (memv atom-key3176 (quote (c)))
                   (begin
                     (maybe-name-value!2450 var3174 exp3175)
                     ((@ (language tree-il) make-toplevel-define)
                      source3173
                      var3174
                      exp3175))
                   (decorate-source2441
                     (list (quote define) var3174 exp3175)
                     source3173)))))
           (maybe-name-value!2450
             (lambda (name3177 val3178)
               (if ((@ (language tree-il) lambda?) val3178)
                 (let ((meta3179
                         ((@ (language tree-il) lambda-meta) val3178)))
                   (if (not (assq (quote name) meta3179))
                     ((setter (@ (language tree-il) lambda-meta))
                      val3178
                      (acons (quote name) name3177 meta3179)))))))
           (build-global-assignment2449
             (lambda (source3180 var3181 exp3182 mod3183)
               (analyze-variable2447
                 mod3183
                 var3181
                 (lambda (mod3184 var3185 public?3186)
                   (let ((atom-key3187 (fluid-ref *mode*2432)))
                     (if (memv atom-key3187 (quote (c)))
                       ((@ (language tree-il) make-module-set)
                        source3180
                        mod3184
                        var3185
                        public?3186
                        exp3182)
                       (decorate-source2441
                         (list 'set!
                               (list (if public?3186 (quote @) (quote @@))
                                     mod3184
                                     var3185)
                               exp3182)
                         source3180))))
                 (lambda (var3188)
                   (let ((atom-key3189 (fluid-ref *mode*2432)))
                     (if (memv atom-key3189 (quote (c)))
                       ((@ (language tree-il) make-toplevel-set)
                        source3180
                        var3188
                        exp3182)
                       (decorate-source2441
                         (list (quote set!) var3188 exp3182)
                         source3180)))))))
           (build-global-reference2448
             (lambda (source3190 var3191 mod3192)
               (analyze-variable2447
                 mod3192
                 var3191
                 (lambda (mod3193 var3194 public?3195)
                   (let ((atom-key3196 (fluid-ref *mode*2432)))
                     (if (memv atom-key3196 (quote (c)))
                       ((@ (language tree-il) make-module-ref)
                        source3190
                        mod3193
                        var3194
                        public?3195)
                       (decorate-source2441
                         (list (if public?3195 (quote @) (quote @@))
                               mod3193
                               var3194)
                         source3190))))
                 (lambda (var3197)
                   (let ((atom-key3198 (fluid-ref *mode*2432)))
                     (if (memv atom-key3198 (quote (c)))
                       ((@ (language tree-il) make-toplevel-ref)
                        source3190
                        var3197)
                       (decorate-source2441 var3197 source3190)))))))
           (analyze-variable2447
             (lambda (mod3199 var3200 modref-cont3201 bare-cont3202)
               (if (not mod3199)
                 (bare-cont3202 var3200)
                 (let ((kind3203 (car mod3199))
                       (mod3204 (cdr mod3199)))
                   (if (memv kind3203 (quote (public)))
                     (modref-cont3201 mod3204 var3200 #t)
                     (if (memv kind3203 (quote (private)))
                       (if (not (equal?
                                  mod3204
                                  (module-name (current-module))))
                         (modref-cont3201 mod3204 var3200 #f)
                         (bare-cont3202 var3200))
                       (if (memv kind3203 (quote (bare)))
                         (bare-cont3202 var3200)
                         (if (memv kind3203 (quote (hygiene)))
                           (if (if (not (equal?
                                          mod3204
                                          (module-name (current-module))))
                                 (module-variable
                                   (resolve-module mod3204)
                                   var3200)
                                 #f)
                             (modref-cont3201 mod3204 var3200 #f)
                             (bare-cont3202 var3200))
                           (syntax-violation
                             #f
                             "bad module kind"
                             var3200
                             mod3204)))))))))
           (build-lexical-assignment2446
             (lambda (source3205 name3206 var3207 exp3208)
               (let ((atom-key3209 (fluid-ref *mode*2432)))
                 (if (memv atom-key3209 (quote (c)))
                   ((@ (language tree-il) make-lexical-set)
                    source3205
                    name3206
                    var3207
                    exp3208)
                   (decorate-source2441
                     (list (quote set!) var3207 exp3208)
                     source3205)))))
           (build-lexical-reference2445
             (lambda (type3210 source3211 name3212 var3213)
               (let ((atom-key3214 (fluid-ref *mode*2432)))
                 (if (memv atom-key3214 (quote (c)))
                   ((@ (language tree-il) make-lexical-ref)
                    source3211
                    name3212
                    var3213)
                   (decorate-source2441 var3213 source3211)))))
           (build-conditional2444
             (lambda (source3215
                      test-exp3216
                      then-exp3217
                      else-exp3218)
               (let ((atom-key3219 (fluid-ref *mode*2432)))
                 (if (memv atom-key3219 (quote (c)))
                   ((@ (language tree-il) make-conditional)
                    source3215
                    test-exp3216
                    then-exp3217
                    else-exp3218)
                   (decorate-source2441
                     (if (equal? else-exp3218 (quote (if #f #f)))
                       (list (quote if) test-exp3216 then-exp3217)
                       (list 'if
                             test-exp3216
                             then-exp3217
                             else-exp3218))
                     source3215)))))
           (build-application2443
             (lambda (source3220 fun-exp3221 arg-exps3222)
               (let ((atom-key3223 (fluid-ref *mode*2432)))
                 (if (memv atom-key3223 (quote (c)))
                   ((@ (language tree-il) make-application)
                    source3220
                    fun-exp3221
                    arg-exps3222)
                   (decorate-source2441
                     (cons fun-exp3221 arg-exps3222)
                     source3220)))))
           (build-void2442
             (lambda (source3224)
               (let ((atom-key3225 (fluid-ref *mode*2432)))
                 (if (memv atom-key3225 (quote (c)))
                   ((@ (language tree-il) make-void) source3224)
                   (decorate-source2441
                     '(if #f #f)
                     source3224)))))
           (decorate-source2441
             (lambda (e3226 s3227)
               (begin
                 (if (if (pair? e3226) s3227 #f)
                   (set-source-properties! e3226 s3227))
                 e3226)))
           (get-global-definition-hook2440
             (lambda (symbol3228 module3229)
               (begin
                 (if (if (not module3229) (current-module) #f)
                   (warn "module system is booted, we should have a module"
                         symbol3228))
                 (let ((v3230 (module-variable
                                (if module3229
                                  (resolve-module (cdr module3229))
                                  (current-module))
                                symbol3228)))
                   (if v3230
                     (if (variable-bound? v3230)
                       (let ((val3231 (variable-ref v3230)))
                         (if (macro? val3231)
                           (if (syncase-macro-type val3231)
                             (cons (syncase-macro-type val3231)
                                   (syncase-macro-binding val3231))
                             #f)
                           #f))
                       #f)
                     #f)))))
           (put-global-definition-hook2439
             (lambda (symbol3232 type3233 val3234)
               (let ((existing3235
                       (let ((v3236 (module-variable
                                      (current-module)
                                      symbol3232)))
                         (if v3236
                           (if (variable-bound? v3236)
                             (let ((val3237 (variable-ref v3236)))
                               (if (macro? val3237)
                                 (if (not (syncase-macro-type val3237))
                                   val3237
                                   #f)
                                 #f))
                             #f)
                           #f))))
                 (module-define!
                   (current-module)
                   symbol3232
                   (if existing3235
                     (make-extended-syncase-macro
                       existing3235
                       type3233
                       val3234)
                     (make-syncase-macro type3233 val3234))))))
           (local-eval-hook2438
             (lambda (x3238 mod3239)
               (primitive-eval
                 (list noexpand2431
                       (let ((atom-key3240 (fluid-ref *mode*2432)))
                         (if (memv atom-key3240 (quote (c)))
                           ((@ (language tree-il) tree-il->scheme) x3238)
                           x3238))))))
           (top-level-eval-hook2437
             (lambda (x3241 mod3242)
               (primitive-eval
                 (list noexpand2431
                       (let ((atom-key3243 (fluid-ref *mode*2432)))
                         (if (memv atom-key3243 (quote (c)))
                           ((@ (language tree-il) tree-il->scheme) x3241)
                           x3241))))))
           (fx<2436 <)
           (fx=2435 =)
           (fx-2434 -)
           (fx+2433 +)
           (*mode*2432 (make-fluid))
           (noexpand2431 "noexpand"))
    (begin
      (global-extend2474
        'local-syntax
        'letrec-syntax
        #t)
      (global-extend2474
        'local-syntax
        'let-syntax
        #f)
      (global-extend2474
        'core
        'fluid-let-syntax
        (lambda (e3244 r3245 w3246 s3247 mod3248)
          ((lambda (tmp3249)
             ((lambda (tmp3250)
                (if (if tmp3250
                      (apply (lambda (_3251 var3252 val3253 e13254 e23255)
                               (valid-bound-ids?2501 var3252))
                             tmp3250)
                      #f)
                  (apply (lambda (_3257 var3258 val3259 e13260 e23261)
                           (let ((names3262
                                   (map (lambda (x3263)
                                          (id-var-name2498 x3263 w3246))
                                        var3258)))
                             (begin
                               (for-each
                                 (lambda (id3265 n3266)
                                   (let ((atom-key3267
                                           (binding-type2468
                                             (lookup2473
                                               n3266
                                               r3245
                                               mod3248))))
                                     (if (memv atom-key3267
                                               '(displaced-lexical))
                                       (syntax-violation
                                         'fluid-let-syntax
                                         "identifier out of context"
                                         e3244
                                         (source-wrap2505
                                           id3265
                                           w3246
                                           s3247
                                           mod3248)))))
                                 var3258
                                 names3262)
                               (chi-body2516
                                 (cons e13260 e23261)
                                 (source-wrap2505 e3244 w3246 s3247 mod3248)
                                 (extend-env2470
                                   names3262
                                   (let ((trans-r3270
                                           (macros-only-env2472 r3245)))
                                     (map (lambda (x3271)
                                            (cons 'macro
                                                  (eval-local-transformer2519
                                                    (chi2512
                                                      x3271
                                                      trans-r3270
                                                      w3246
                                                      mod3248)
                                                    mod3248)))
                                          val3259))
                                   r3245)
                                 w3246
                                 mod3248))))
                         tmp3250)
                  ((lambda (_3273)
                     (syntax-violation
                       'fluid-let-syntax
                       "bad syntax"
                       (source-wrap2505 e3244 w3246 s3247 mod3248)))
                   tmp3249)))
              ($sc-dispatch
                tmp3249
                '(any #(each (any any)) any . each-any))))
           e3244)))
      (global-extend2474
        'core
        'quote
        (lambda (e3274 r3275 w3276 s3277 mod3278)
          ((lambda (tmp3279)
             ((lambda (tmp3280)
                (if tmp3280
                  (apply (lambda (_3281 e3282)
                           (build-data2454 s3277 (strip2522 e3282 w3276)))
                         tmp3280)
                  ((lambda (_3283)
                     (syntax-violation
                       'quote
                       "bad syntax"
                       (source-wrap2505 e3274 w3276 s3277 mod3278)))
                   tmp3279)))
              ($sc-dispatch tmp3279 (quote (any any)))))
           e3274)))
      (global-extend2474
        'core
        'syntax
        (letrec ((regen3291
                   (lambda (x3292)
                     (let ((atom-key3293 (car x3292)))
                       (if (memv atom-key3293 (quote (ref)))
                         (build-lexical-reference2445
                           'value
                           #f
                           (cadr x3292)
                           (cadr x3292))
                         (if (memv atom-key3293 (quote (primitive)))
                           (build-primref2453 #f (cadr x3292))
                           (if (memv atom-key3293 (quote (quote)))
                             (build-data2454 #f (cadr x3292))
                             (if (memv atom-key3293 (quote (lambda)))
                               (build-lambda2452
                                 #f
                                 (cadr x3292)
                                 (cadr x3292)
                                 #f
                                 (regen3291 (caddr x3292)))
                               (build-application2443
                                 #f
                                 (build-primref2453 #f (car x3292))
                                 (map regen3291 (cdr x3292))))))))))
                 (gen-vector3290
                   (lambda (x3294)
                     (if (eq? (car x3294) (quote list))
                       (cons (quote vector) (cdr x3294))
                       (if (eq? (car x3294) (quote quote))
                         (list (quote quote) (list->vector (cadr x3294)))
                         (list (quote list->vector) x3294)))))
                 (gen-append3289
                   (lambda (x3295 y3296)
                     (if (equal? y3296 (quote (quote ())))
                       x3295
                       (list (quote append) x3295 y3296))))
                 (gen-cons3288
                   (lambda (x3297 y3298)
                     (let ((atom-key3299 (car y3298)))
                       (if (memv atom-key3299 (quote (quote)))
                         (if (eq? (car x3297) (quote quote))
                           (list 'quote
                                 (cons (cadr x3297) (cadr y3298)))
                           (if (eq? (cadr y3298) (quote ()))
                             (list (quote list) x3297)
                             (list (quote cons) x3297 y3298)))
                         (if (memv atom-key3299 (quote (list)))
                           (cons (quote list) (cons x3297 (cdr y3298)))
                           (list (quote cons) x3297 y3298))))))
                 (gen-map3287
                   (lambda (e3300 map-env3301)
                     (let ((formals3302 (map cdr map-env3301))
                           (actuals3303
                             (map (lambda (x3304)
                                    (list (quote ref) (car x3304)))
                                  map-env3301)))
                       (if (eq? (car e3300) (quote ref))
                         (car actuals3303)
                         (if (and-map
                               (lambda (x3305)
                                 (if (eq? (car x3305) (quote ref))
                                   (memq (cadr x3305) formals3302)
                                   #f))
                               (cdr e3300))
                           (cons 'map
                                 (cons (list (quote primitive) (car e3300))
                                       (map (let ((r3306 (map cons
                                                              formals3302
                                                              actuals3303)))
                                              (lambda (x3307)
                                                (cdr (assq (cadr x3307)
                                                           r3306))))
                                            (cdr e3300))))
                           (cons 'map
                                 (cons (list (quote lambda) formals3302 e3300)
                                       actuals3303)))))))
                 (gen-mappend3286
                   (lambda (e3308 map-env3309)
                     (list 'apply
                           '(primitive append)
                           (gen-map3287 e3308 map-env3309))))
                 (gen-ref3285
                   (lambda (src3310 var3311 level3312 maps3313)
                     (if (fx=2435 level3312 0)
                       (values var3311 maps3313)
                       (if (null? maps3313)
                         (syntax-violation
                           'syntax
                           "missing ellipsis"
                           src3310)
                         (call-with-values
                           (lambda ()
                             (gen-ref3285
                               src3310
                               var3311
                               (fx-2434 level3312 1)
                               (cdr maps3313)))
                           (lambda (outer-var3314 outer-maps3315)
                             (let ((b3316 (assq outer-var3314 (car maps3313))))
                               (if b3316
                                 (values (cdr b3316) maps3313)
                                 (let ((inner-var3317
                                         (gen-var2523 (quote tmp))))
                                   (values
                                     inner-var3317
                                     (cons (cons (cons outer-var3314
                                                       inner-var3317)
                                                 (car maps3313))
                                           outer-maps3315)))))))))))
                 (gen-syntax3284
                   (lambda (src3318
                            e3319
                            r3320
                            maps3321
                            ellipsis?3322
                            mod3323)
                     (if (id?2476 e3319)
                       (let ((label3324 (id-var-name2498 e3319 (quote (())))))
                         (let ((b3325 (lookup2473 label3324 r3320 mod3323)))
                           (if (eq? (binding-type2468 b3325) (quote syntax))
                             (call-with-values
                               (lambda ()
                                 (let ((var.lev3326 (binding-value2469 b3325)))
                                   (gen-ref3285
                                     src3318
                                     (car var.lev3326)
                                     (cdr var.lev3326)
                                     maps3321)))
                               (lambda (var3327 maps3328)
                                 (values (list (quote ref) var3327) maps3328)))
                             (if (ellipsis?3322 e3319)
                               (syntax-violation
                                 'syntax
                                 "misplaced ellipsis"
                                 src3318)
                               (values (list (quote quote) e3319) maps3321)))))
                       ((lambda (tmp3329)
                          ((lambda (tmp3330)
                             (if (if tmp3330
                                   (apply (lambda (dots3331 e3332)
                                            (ellipsis?3322 dots3331))
                                          tmp3330)
                                   #f)
                               (apply (lambda (dots3333 e3334)
                                        (gen-syntax3284
                                          src3318
                                          e3334
                                          r3320
                                          maps3321
                                          (lambda (x3335) #f)
                                          mod3323))
                                      tmp3330)
                               ((lambda (tmp3336)
                                  (if (if tmp3336
                                        (apply (lambda (x3337 dots3338 y3339)
                                                 (ellipsis?3322 dots3338))
                                               tmp3336)
                                        #f)
                                    (apply (lambda (x3340 dots3341 y3342)
                                             (letrec ((f3343 (lambda (y3344
                                                                      k3345)
                                                               ((lambda (tmp3349)
                                                                  ((lambda (tmp3350)
                                                                     (if (if tmp3350
                                                                           (apply (lambda (dots3351
                                                                                           y3352)
                                                                                    (ellipsis?3322
                                                                                      dots3351))
                                                                                  tmp3350)
                                                                           #f)
                                                                       (apply (lambda (dots3353
                                                                                       y3354)
                                                                                (f3343 y3354
                                                                                       (lambda (maps3355)
                                                                                         (call-with-values
                                                                                           (lambda ()
                                                                                             (k3345 (cons '()
                                                                                                          maps3355)))
                                                                                           (lambda (x3356
                                                                                                    maps3357)
                                                                                             (if (null? (car maps3357))
                                                                                               (syntax-violation
                                                                                                 'syntax
                                                                                                 "extra ellipsis"
                                                                                                 src3318)
                                                                                               (values
                                                                                                 (gen-mappend3286
                                                                                                   x3356
                                                                                                   (car maps3357))
                                                                                                 (cdr maps3357))))))))
                                                                              tmp3350)
                                                                       ((lambda (_3358)
                                                                          (call-with-values
                                                                            (lambda ()
                                                                              (gen-syntax3284
                                                                                src3318
                                                                                y3344
                                                                                r3320
                                                                                maps3321
                                                                                ellipsis?3322
                                                                                mod3323))
                                                                            (lambda (y3359
                                                                                     maps3360)
                                                                              (call-with-values
                                                                                (lambda ()
                                                                                  (k3345 maps3360))
                                                                                (lambda (x3361
                                                                                         maps3362)
                                                                                  (values
                                                                                    (gen-append3289
                                                                                      x3361
                                                                                      y3359)
                                                                                    maps3362))))))
                                                                        tmp3349)))
                                                                   ($sc-dispatch
                                                                     tmp3349
                                                                     '(any .
                                                                           any))))
                                                                y3344))))
                                               (f3343 y3342
                                                      (lambda (maps3346)
                                                        (call-with-values
                                                          (lambda ()
                                                            (gen-syntax3284
                                                              src3318
                                                              x3340
                                                              r3320
                                                              (cons '()
                                                                    maps3346)
                                                              ellipsis?3322
                                                              mod3323))
                                                          (lambda (x3347
                                                                   maps3348)
                                                            (if (null? (car maps3348))
                                                              (syntax-violation
                                                                'syntax
                                                                "extra ellipsis"
                                                                src3318)
                                                              (values
                                                                (gen-map3287
                                                                  x3347
                                                                  (car maps3348))
                                                                (cdr maps3348)))))))))
                                           tmp3336)
                                    ((lambda (tmp3363)
                                       (if tmp3363
                                         (apply (lambda (x3364 y3365)
                                                  (call-with-values
                                                    (lambda ()
                                                      (gen-syntax3284
                                                        src3318
                                                        x3364
                                                        r3320
                                                        maps3321
                                                        ellipsis?3322
                                                        mod3323))
                                                    (lambda (x3366 maps3367)
                                                      (call-with-values
                                                        (lambda ()
                                                          (gen-syntax3284
                                                            src3318
                                                            y3365
                                                            r3320
                                                            maps3367
                                                            ellipsis?3322
                                                            mod3323))
                                                        (lambda (y3368
                                                                 maps3369)
                                                          (values
                                                            (gen-cons3288
                                                              x3366
                                                              y3368)
                                                            maps3369))))))
                                                tmp3363)
                                         ((lambda (tmp3370)
                                            (if tmp3370
                                              (apply (lambda (e13371 e23372)
                                                       (call-with-values
                                                         (lambda ()
                                                           (gen-syntax3284
                                                             src3318
                                                             (cons e13371
                                                                   e23372)
                                                             r3320
                                                             maps3321
                                                             ellipsis?3322
                                                             mod3323))
                                                         (lambda (e3374
                                                                  maps3375)
                                                           (values
                                                             (gen-vector3290
                                                               e3374)
                                                             maps3375))))
                                                     tmp3370)
                                              ((lambda (_3376)
                                                 (values
                                                   (list (quote quote) e3319)
                                                   maps3321))
                                               tmp3329)))
                                          ($sc-dispatch
                                            tmp3329
                                            '#(vector (any . each-any))))))
                                     ($sc-dispatch
                                       tmp3329
                                       '(any . any)))))
                                ($sc-dispatch
                                  tmp3329
                                  '(any any . any)))))
                           ($sc-dispatch tmp3329 (quote (any any)))))
                        e3319)))))
          (lambda (e3377 r3378 w3379 s3380 mod3381)
            (let ((e3382 (source-wrap2505 e3377 w3379 s3380 mod3381)))
              ((lambda (tmp3383)
                 ((lambda (tmp3384)
                    (if tmp3384
                      (apply (lambda (_3385 x3386)
                               (call-with-values
                                 (lambda ()
                                   (gen-syntax3284
                                     e3382
                                     x3386
                                     r3378
                                     '()
                                     ellipsis?2521
                                     mod3381))
                                 (lambda (e3387 maps3388) (regen3291 e3387))))
                             tmp3384)
                      ((lambda (_3389)
                         (syntax-violation
                           'syntax
                           "bad `syntax' form"
                           e3382))
                       tmp3383)))
                  ($sc-dispatch tmp3383 (quote (any any)))))
               e3382)))))
      (global-extend2474
        'core
        'lambda
        (lambda (e3390 r3391 w3392 s3393 mod3394)
          ((lambda (tmp3395)
             ((lambda (tmp3396)
                (if tmp3396
                  (apply (lambda (_3397 c3398)
                           (chi-lambda-clause2517
                             (source-wrap2505 e3390 w3392 s3393 mod3394)
                             #f
                             c3398
                             r3391
                             w3392
                             mod3394
                             (lambda (names3399
                                      vars3400
                                      docstring3401
                                      body3402)
                               (build-lambda2452
                                 s3393
                                 names3399
                                 vars3400
                                 docstring3401
                                 body3402))))
                         tmp3396)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    tmp3395)))
              ($sc-dispatch tmp3395 (quote (any . any)))))
           e3390)))
      (global-extend2474
        'core
        'let
        (letrec ((chi-let3403
                   (lambda (e3404
                            r3405
                            w3406
                            s3407
                            mod3408
                            constructor3409
                            ids3410
                            vals3411
                            exps3412)
                     (if (not (valid-bound-ids?2501 ids3410))
                       (syntax-violation
                         'let
                         "duplicate bound variable"
                         e3404)
                       (let ((labels3413 (gen-labels2482 ids3410))
                             (new-vars3414 (map gen-var2523 ids3410)))
                         (let ((nw3415
                                 (make-binding-wrap2493
                                   ids3410
                                   labels3413
                                   w3406))
                               (nr3416
                                 (extend-var-env2471
                                   labels3413
                                   new-vars3414
                                   r3405)))
                           (constructor3409
                             s3407
                             (map syntax->datum ids3410)
                             new-vars3414
                             (map (lambda (x3417)
                                    (chi2512 x3417 r3405 w3406 mod3408))
                                  vals3411)
                             (chi-body2516
                               exps3412
                               (source-wrap2505 e3404 nw3415 s3407 mod3408)
                               nr3416
                               nw3415
                               mod3408))))))))
          (lambda (e3418 r3419 w3420 s3421 mod3422)
            ((lambda (tmp3423)
               ((lambda (tmp3424)
                  (if (if tmp3424
                        (apply (lambda (_3425 id3426 val3427 e13428 e23429)
                                 (and-map id?2476 id3426))
                               tmp3424)
                        #f)
                    (apply (lambda (_3431 id3432 val3433 e13434 e23435)
                             (chi-let3403
                               e3418
                               r3419
                               w3420
                               s3421
                               mod3422
                               build-let2456
                               id3432
                               val3433
                               (cons e13434 e23435)))
                           tmp3424)
                    ((lambda (tmp3439)
                       (if (if tmp3439
                             (apply (lambda (_3440
                                             f3441
                                             id3442
                                             val3443
                                             e13444
                                             e23445)
                                      (if (id?2476 f3441)
                                        (and-map id?2476 id3442)
                                        #f))
                                    tmp3439)
                             #f)
                         (apply (lambda (_3447
                                         f3448
                                         id3449
                                         val3450
                                         e13451
                                         e23452)
                                  (chi-let3403
                                    e3418
                                    r3419
                                    w3420
                                    s3421
                                    mod3422
                                    build-named-let2457
                                    (cons f3448 id3449)
                                    val3450
                                    (cons e13451 e23452)))
                                tmp3439)
                         ((lambda (_3456)
                            (syntax-violation
                              'let
                              "bad let"
                              (source-wrap2505 e3418 w3420 s3421 mod3422)))
                          tmp3423)))
                     ($sc-dispatch
                       tmp3423
                       '(any any #(each (any any)) any . each-any)))))
                ($sc-dispatch
                  tmp3423
                  '(any #(each (any any)) any . each-any))))
             e3418))))
      (global-extend2474
        'core
        'letrec
        (lambda (e3457 r3458 w3459 s3460 mod3461)
          ((lambda (tmp3462)
             ((lambda (tmp3463)
                (if (if tmp3463
                      (apply (lambda (_3464 id3465 val3466 e13467 e23468)
                               (and-map id?2476 id3465))
                             tmp3463)
                      #f)
                  (apply (lambda (_3470 id3471 val3472 e13473 e23474)
                           (let ((ids3475 id3471))
                             (if (not (valid-bound-ids?2501 ids3475))
                               (syntax-violation
                                 'letrec
                                 "duplicate bound variable"
                                 e3457)
                               (let ((labels3477 (gen-labels2482 ids3475))
                                     (new-vars3478 (map gen-var2523 ids3475)))
                                 (let ((w3479 (make-binding-wrap2493
                                                ids3475
                                                labels3477
                                                w3459))
                                       (r3480 (extend-var-env2471
                                                labels3477
                                                new-vars3478
                                                r3458)))
                                   (build-letrec2458
                                     s3460
                                     (map syntax->datum ids3475)
                                     new-vars3478
                                     (map (lambda (x3481)
                                            (chi2512
                                              x3481
                                              r3480
                                              w3479
                                              mod3461))
                                          val3472)
                                     (chi-body2516
                                       (cons e13473 e23474)
                                       (source-wrap2505
                                         e3457
                                         w3479
                                         s3460
                                         mod3461)
                                       r3480
                                       w3479
                                       mod3461)))))))
                         tmp3463)
                  ((lambda (_3484)
                     (syntax-violation
                       'letrec
                       "bad letrec"
                       (source-wrap2505 e3457 w3459 s3460 mod3461)))
                   tmp3462)))
              ($sc-dispatch
                tmp3462
                '(any #(each (any any)) any . each-any))))
           e3457)))
      (global-extend2474
        'core
        'set!
        (lambda (e3485 r3486 w3487 s3488 mod3489)
          ((lambda (tmp3490)
             ((lambda (tmp3491)
                (if (if tmp3491
                      (apply (lambda (_3492 id3493 val3494) (id?2476 id3493))
                             tmp3491)
                      #f)
                  (apply (lambda (_3495 id3496 val3497)
                           (let ((val3498
                                   (chi2512 val3497 r3486 w3487 mod3489))
                                 (n3499 (id-var-name2498 id3496 w3487)))
                             (let ((b3500 (lookup2473 n3499 r3486 mod3489)))
                               (let ((atom-key3501 (binding-type2468 b3500)))
                                 (if (memv atom-key3501 (quote (lexical)))
                                   (build-lexical-assignment2446
                                     s3488
                                     (syntax->datum id3496)
                                     (binding-value2469 b3500)
                                     val3498)
                                   (if (memv atom-key3501 (quote (global)))
                                     (build-global-assignment2449
                                       s3488
                                       n3499
                                       val3498
                                       mod3489)
                                     (if (memv atom-key3501
                                               '(displaced-lexical))
                                       (syntax-violation
                                         'set!
                                         "identifier out of context"
                                         (wrap2504 id3496 w3487 mod3489))
                                       (syntax-violation
                                         'set!
                                         "bad set!"
                                         (source-wrap2505
                                           e3485
                                           w3487
                                           s3488
                                           mod3489)))))))))
                         tmp3491)
                  ((lambda (tmp3502)
                     (if tmp3502
                       (apply (lambda (_3503 head3504 tail3505 val3506)
                                (call-with-values
                                  (lambda ()
                                    (syntax-type2510
                                      head3504
                                      r3486
                                      '(())
                                      #f
                                      #f
                                      mod3489
                                      #t))
                                  (lambda (type3507
                                           value3508
                                           ee3509
                                           ww3510
                                           ss3511
                                           modmod3512)
                                    (if (memv type3507 (quote (module-ref)))
                                      (let ((val3513
                                              (chi2512
                                                val3506
                                                r3486
                                                w3487
                                                mod3489)))
                                        (call-with-values
                                          (lambda ()
                                            (value3508
                                              (cons head3504 tail3505)))
                                          (lambda (id3515 mod3516)
                                            (build-global-assignment2449
                                              s3488
                                              id3515
                                              val3513
                                              mod3516))))
                                      (build-application2443
                                        s3488
                                        (chi2512
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
                                                head3504)
                                          r3486
                                          w3487
                                          mod3489)
                                        (map (lambda (e3517)
                                               (chi2512
                                                 e3517
                                                 r3486
                                                 w3487
                                                 mod3489))
                                             (append
                                               tail3505
                                               (list val3506))))))))
                              tmp3502)
                       ((lambda (_3519)
                          (syntax-violation
                            'set!
                            "bad set!"
                            (source-wrap2505 e3485 w3487 s3488 mod3489)))
                        tmp3490)))
                   ($sc-dispatch
                     tmp3490
                     '(any (any . each-any) any)))))
              ($sc-dispatch tmp3490 (quote (any any any)))))
           e3485)))
      (global-extend2474
        'module-ref
        '@
        (lambda (e3520)
          ((lambda (tmp3521)
             ((lambda (tmp3522)
                (if (if tmp3522
                      (apply (lambda (_3523 mod3524 id3525)
                               (if (and-map id?2476 mod3524)
                                 (id?2476 id3525)
                                 #f))
                             tmp3522)
                      #f)
                  (apply (lambda (_3527 mod3528 id3529)
                           (values
                             (syntax->datum id3529)
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
                                     mod3528))))
                         tmp3522)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    tmp3521)))
              ($sc-dispatch tmp3521 (quote (any each-any any)))))
           e3520)))
      (global-extend2474
        'module-ref
        '@@
        (lambda (e3531)
          ((lambda (tmp3532)
             ((lambda (tmp3533)
                (if (if tmp3533
                      (apply (lambda (_3534 mod3535 id3536)
                               (if (and-map id?2476 mod3535)
                                 (id?2476 id3536)
                                 #f))
                             tmp3533)
                      #f)
                  (apply (lambda (_3538 mod3539 id3540)
                           (values
                             (syntax->datum id3540)
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
                                     mod3539))))
                         tmp3533)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    tmp3532)))
              ($sc-dispatch tmp3532 (quote (any each-any any)))))
           e3531)))
      (global-extend2474
        'core
        'if
        (lambda (e3542 r3543 w3544 s3545 mod3546)
          ((lambda (tmp3547)
             ((lambda (tmp3548)
                (if tmp3548
                  (apply (lambda (_3549 test3550 then3551)
                           (build-conditional2444
                             s3545
                             (chi2512 test3550 r3543 w3544 mod3546)
                             (chi2512 then3551 r3543 w3544 mod3546)
                             (build-void2442 #f)))
                         tmp3548)
                  ((lambda (tmp3552)
                     (if tmp3552
                       (apply (lambda (_3553 test3554 then3555 else3556)
                                (build-conditional2444
                                  s3545
                                  (chi2512 test3554 r3543 w3544 mod3546)
                                  (chi2512 then3555 r3543 w3544 mod3546)
                                  (chi2512 else3556 r3543 w3544 mod3546)))
                              tmp3552)
                       (syntax-violation
                         #f
                         "source expression failed to match any pattern"
                         tmp3547)))
                   ($sc-dispatch tmp3547 (quote (any any any any))))))
              ($sc-dispatch tmp3547 (quote (any any any)))))
           e3542)))
      (global-extend2474
        'begin
        'begin
        '())
      (global-extend2474
        'define
        'define
        '())
      (global-extend2474
        'define-syntax
        'define-syntax
        '())
      (global-extend2474
        'eval-when
        'eval-when
        '())
      (global-extend2474
        'core
        'syntax-case
        (letrec ((gen-syntax-case3560
                   (lambda (x3561 keys3562 clauses3563 r3564 mod3565)
                     (if (null? clauses3563)
                       (build-application2443
                         #f
                         (build-primref2453 #f (quote syntax-violation))
                         (list (build-data2454 #f #f)
                               (build-data2454
                                 #f
                                 "source expression failed to match any pattern")
                               x3561))
                       ((lambda (tmp3566)
                          ((lambda (tmp3567)
                             (if tmp3567
                               (apply (lambda (pat3568 exp3569)
                                        (if (if (id?2476 pat3568)
                                              (and-map
                                                (lambda (x3570)
                                                  (not (free-id=?2499
                                                         pat3568
                                                         x3570)))
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
                                                      keys3562))
                                              #f)
                                          (let ((labels3571
                                                  (list (gen-label2481)))
                                                (var3572
                                                  (gen-var2523 pat3568)))
                                            (build-application2443
                                              #f
                                              (build-lambda2452
                                                #f
                                                (list (syntax->datum pat3568))
                                                (list var3572)
                                                #f
                                                (chi2512
                                                  exp3569
                                                  (extend-env2470
                                                    labels3571
                                                    (list (cons 'syntax
                                                                (cons var3572
                                                                      0)))
                                                    r3564)
                                                  (make-binding-wrap2493
                                                    (list pat3568)
                                                    labels3571
                                                    '(()))
                                                  mod3565))
                                              (list x3561)))
                                          (gen-clause3559
                                            x3561
                                            keys3562
                                            (cdr clauses3563)
                                            r3564
                                            pat3568
                                            #t
                                            exp3569
                                            mod3565)))
                                      tmp3567)
                               ((lambda (tmp3573)
                                  (if tmp3573
                                    (apply (lambda (pat3574 fender3575 exp3576)
                                             (gen-clause3559
                                               x3561
                                               keys3562
                                               (cdr clauses3563)
                                               r3564
                                               pat3574
                                               fender3575
                                               exp3576
                                               mod3565))
                                           tmp3573)
                                    ((lambda (_3577)
                                       (syntax-violation
                                         'syntax-case
                                         "invalid clause"
                                         (car clauses3563)))
                                     tmp3566)))
                                ($sc-dispatch tmp3566 (quote (any any any))))))
                           ($sc-dispatch tmp3566 (quote (any any)))))
                        (car clauses3563)))))
                 (gen-clause3559
                   (lambda (x3578
                            keys3579
                            clauses3580
                            r3581
                            pat3582
                            fender3583
                            exp3584
                            mod3585)
                     (call-with-values
                       (lambda ()
                         (convert-pattern3557 pat3582 keys3579))
                       (lambda (p3586 pvars3587)
                         (if (not (distinct-bound-ids?2502
                                    (map car pvars3587)))
                           (syntax-violation
                             'syntax-case
                             "duplicate pattern variable"
                             pat3582)
                           (if (not (and-map
                                      (lambda (x3588)
                                        (not (ellipsis?2521 (car x3588))))
                                      pvars3587))
                             (syntax-violation
                               'syntax-case
                               "misplaced ellipsis"
                               pat3582)
                             (let ((y3589 (gen-var2523 (quote tmp))))
                               (build-application2443
                                 #f
                                 (build-lambda2452
                                   #f
                                   (list (quote tmp))
                                   (list y3589)
                                   #f
                                   (let ((y3590 (build-lexical-reference2445
                                                  'value
                                                  #f
                                                  'tmp
                                                  y3589)))
                                     (build-conditional2444
                                       #f
                                       ((lambda (tmp3591)
                                          ((lambda (tmp3592)
                                             (if tmp3592
                                               (apply (lambda () y3590)
                                                      tmp3592)
                                               ((lambda (_3593)
                                                  (build-conditional2444
                                                    #f
                                                    y3590
                                                    (build-dispatch-call3558
                                                      pvars3587
                                                      fender3583
                                                      y3590
                                                      r3581
                                                      mod3585)
                                                    (build-data2454 #f #f)))
                                                tmp3591)))
                                           ($sc-dispatch
                                             tmp3591
                                             '#(atom #t))))
                                        fender3583)
                                       (build-dispatch-call3558
                                         pvars3587
                                         exp3584
                                         y3590
                                         r3581
                                         mod3585)
                                       (gen-syntax-case3560
                                         x3578
                                         keys3579
                                         clauses3580
                                         r3581
                                         mod3585))))
                                 (list (if (eq? p3586 (quote any))
                                         (build-application2443
                                           #f
                                           (build-primref2453 #f (quote list))
                                           (list x3578))
                                         (build-application2443
                                           #f
                                           (build-primref2453
                                             #f
                                             '$sc-dispatch)
                                           (list x3578
                                                 (build-data2454
                                                   #f
                                                   p3586)))))))))))))
                 (build-dispatch-call3558
                   (lambda (pvars3594 exp3595 y3596 r3597 mod3598)
                     (let ((ids3599 (map car pvars3594))
                           (levels3600 (map cdr pvars3594)))
                       (let ((labels3601 (gen-labels2482 ids3599))
                             (new-vars3602 (map gen-var2523 ids3599)))
                         (build-application2443
                           #f
                           (build-primref2453 #f (quote apply))
                           (list (build-lambda2452
                                   #f
                                   (map syntax->datum ids3599)
                                   new-vars3602
                                   #f
                                   (chi2512
                                     exp3595
                                     (extend-env2470
                                       labels3601
                                       (map (lambda (var3603 level3604)
                                              (cons 'syntax
                                                    (cons var3603 level3604)))
                                            new-vars3602
                                            (map cdr pvars3594))
                                       r3597)
                                     (make-binding-wrap2493
                                       ids3599
                                       labels3601
                                       '(()))
                                     mod3598))
                                 y3596))))))
                 (convert-pattern3557
                   (lambda (pattern3605 keys3606)
                     (letrec ((cvt3607
                                (lambda (p3608 n3609 ids3610)
                                  (if (id?2476 p3608)
                                    (if (bound-id-member?2503 p3608 keys3606)
                                      (values
                                        (vector (quote free-id) p3608)
                                        ids3610)
                                      (values
                                        'any
                                        (cons (cons p3608 n3609) ids3610)))
                                    ((lambda (tmp3611)
                                       ((lambda (tmp3612)
                                          (if (if tmp3612
                                                (apply (lambda (x3613 dots3614)
                                                         (ellipsis?2521
                                                           dots3614))
                                                       tmp3612)
                                                #f)
                                            (apply (lambda (x3615 dots3616)
                                                     (call-with-values
                                                       (lambda ()
                                                         (cvt3607
                                                           x3615
                                                           (fx+2433 n3609 1)
                                                           ids3610))
                                                       (lambda (p3617 ids3618)
                                                         (values
                                                           (if (eq? p3617
                                                                    'any)
                                                             'each-any
                                                             (vector
                                                               'each
                                                               p3617))
                                                           ids3618))))
                                                   tmp3612)
                                            ((lambda (tmp3619)
                                               (if tmp3619
                                                 (apply (lambda (x3620 y3621)
                                                          (call-with-values
                                                            (lambda ()
                                                              (cvt3607
                                                                y3621
                                                                n3609
                                                                ids3610))
                                                            (lambda (y3622
                                                                     ids3623)
                                                              (call-with-values
                                                                (lambda ()
                                                                  (cvt3607
                                                                    x3620
                                                                    n3609
                                                                    ids3623))
                                                                (lambda (x3624
                                                                         ids3625)
                                                                  (values
                                                                    (cons x3624
                                                                          y3622)
                                                                    ids3625))))))
                                                        tmp3619)
                                                 ((lambda (tmp3626)
                                                    (if tmp3626
                                                      (apply (lambda ()
                                                               (values
                                                                 '()
                                                                 ids3610))
                                                             tmp3626)
                                                      ((lambda (tmp3627)
                                                         (if tmp3627
                                                           (apply (lambda (x3628)
                                                                    (call-with-values
                                                                      (lambda ()
                                                                        (cvt3607
                                                                          x3628
                                                                          n3609
                                                                          ids3610))
                                                                      (lambda (p3630
                                                                               ids3631)
                                                                        (values
                                                                          (vector
                                                                            'vector
                                                                            p3630)
                                                                          ids3631))))
                                                                  tmp3627)
                                                           ((lambda (x3632)
                                                              (values
                                                                (vector
                                                                  'atom
                                                                  (strip2522
                                                                    p3608
                                                                    '(())))
                                                                ids3610))
                                                            tmp3611)))
                                                       ($sc-dispatch
                                                         tmp3611
                                                         '#(vector
                                                            each-any)))))
                                                  ($sc-dispatch
                                                    tmp3611
                                                    '()))))
                                             ($sc-dispatch
                                               tmp3611
                                               '(any . any)))))
                                        ($sc-dispatch
                                          tmp3611
                                          '(any any))))
                                     p3608)))))
                       (cvt3607 pattern3605 0 (quote ()))))))
          (lambda (e3633 r3634 w3635 s3636 mod3637)
            (let ((e3638 (source-wrap2505 e3633 w3635 s3636 mod3637)))
              ((lambda (tmp3639)
                 ((lambda (tmp3640)
                    (if tmp3640
                      (apply (lambda (_3641 val3642 key3643 m3644)
                               (if (and-map
                                     (lambda (x3645)
                                       (if (id?2476 x3645)
                                         (not (ellipsis?2521 x3645))
                                         #f))
                                     key3643)
                                 (let ((x3647 (gen-var2523 (quote tmp))))
                                   (build-application2443
                                     s3636
                                     (build-lambda2452
                                       #f
                                       (list (quote tmp))
                                       (list x3647)
                                       #f
                                       (gen-syntax-case3560
                                         (build-lexical-reference2445
                                           'value
                                           #f
                                           'tmp
                                           x3647)
                                         key3643
                                         m3644
                                         r3634
                                         mod3637))
                                     (list (chi2512
                                             val3642
                                             r3634
                                             '(())
                                             mod3637))))
                                 (syntax-violation
                                   'syntax-case
                                   "invalid literals list"
                                   e3638)))
                             tmp3640)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        tmp3639)))
                  ($sc-dispatch
                    tmp3639
                    '(any any each-any . each-any))))
               e3638)))))
      (set! sc-expand
        (lambda (x3651 . rest3650)
          (if (if (pair? x3651)
                (equal? (car x3651) noexpand2431)
                #f)
            (cadr x3651)
            (let ((m3652 (if (null? rest3650) (quote e) (car rest3650)))
                  (esew3653
                    (if (let ((t3654 (null? rest3650)))
                          (if t3654 t3654 (null? (cdr rest3650))))
                      '(eval)
                      (cadr rest3650))))
              (with-fluid*
                *mode*2432
                m3652
                (lambda ()
                  (chi-top2511
                    x3651
                    '()
                    '((top))
                    m3652
                    esew3653
                    (cons 'hygiene
                          (module-name (current-module))))))))))
      (set! identifier?
        (lambda (x3655) (nonsymbol-id?2475 x3655)))
      (set! datum->syntax
        (lambda (id3656 datum3657)
          (make-syntax-object2459
            datum3657
            (syntax-object-wrap2462 id3656)
            #f)))
      (set! syntax->datum
        (lambda (x3658) (strip2522 x3658 (quote (())))))
      (set! generate-temporaries
        (lambda (ls3659)
          (begin
            (let ((x3660 ls3659))
              (if (not (list? x3660))
                (syntax-violation
                  'generate-temporaries
                  "invalid argument"
                  x3660)))
            (map (lambda (x3661)
                   (wrap2504 (gensym) (quote ((top))) #f))
                 ls3659))))
      (set! free-identifier=?
        (lambda (x3662 y3663)
          (begin
            (let ((x3664 x3662))
              (if (not (nonsymbol-id?2475 x3664))
                (syntax-violation
                  'free-identifier=?
                  "invalid argument"
                  x3664)))
            (let ((x3665 y3663))
              (if (not (nonsymbol-id?2475 x3665))
                (syntax-violation
                  'free-identifier=?
                  "invalid argument"
                  x3665)))
            (free-id=?2499 x3662 y3663))))
      (set! bound-identifier=?
        (lambda (x3666 y3667)
          (begin
            (let ((x3668 x3666))
              (if (not (nonsymbol-id?2475 x3668))
                (syntax-violation
                  'bound-identifier=?
                  "invalid argument"
                  x3668)))
            (let ((x3669 y3667))
              (if (not (nonsymbol-id?2475 x3669))
                (syntax-violation
                  'bound-identifier=?
                  "invalid argument"
                  x3669)))
            (bound-id=?2500 x3666 y3667))))
      (set! syntax-violation
        (lambda (who3673 message3672 form3671 . subform3670)
          (begin
            (let ((x3674 who3673))
              (if (not ((lambda (x3675)
                          (let ((t3676 (not x3675)))
                            (if t3676
                              t3676
                              (let ((t3677 (string? x3675)))
                                (if t3677 t3677 (symbol? x3675))))))
                        x3674))
                (syntax-violation
                  'syntax-violation
                  "invalid argument"
                  x3674)))
            (let ((x3678 message3672))
              (if (not (string? x3678))
                (syntax-violation
                  'syntax-violation
                  "invalid argument"
                  x3678)))
            (scm-error
              'syntax-error
              'sc-expand
              (string-append
                (if who3673 "~a: " "")
                "~a "
                (if (null? subform3670)
                  "in ~a"
                  "in subform `~s' of `~s'"))
              (let ((tail3679
                      (cons message3672
                            (map (lambda (x3680)
                                   (strip2522 x3680 (quote (()))))
                                 (append subform3670 (list form3671))))))
                (if who3673 (cons who3673 tail3679) tail3679))
              #f))))
      (letrec ((match3685
                 (lambda (e3686 p3687 w3688 r3689 mod3690)
                   (if (not r3689)
                     #f
                     (if (eq? p3687 (quote any))
                       (cons (wrap2504 e3686 w3688 mod3690) r3689)
                       (if (syntax-object?2460 e3686)
                         (match*3684
                           (syntax-object-expression2461 e3686)
                           p3687
                           (join-wraps2495
                             w3688
                             (syntax-object-wrap2462 e3686))
                           r3689
                           (syntax-object-module2463 e3686))
                         (match*3684 e3686 p3687 w3688 r3689 mod3690))))))
               (match*3684
                 (lambda (e3691 p3692 w3693 r3694 mod3695)
                   (if (null? p3692)
                     (if (null? e3691) r3694 #f)
                     (if (pair? p3692)
                       (if (pair? e3691)
                         (match3685
                           (car e3691)
                           (car p3692)
                           w3693
                           (match3685
                             (cdr e3691)
                             (cdr p3692)
                             w3693
                             r3694
                             mod3695)
                           mod3695)
                         #f)
                       (if (eq? p3692 (quote each-any))
                         (let ((l3696 (match-each-any3682
                                        e3691
                                        w3693
                                        mod3695)))
                           (if l3696 (cons l3696 r3694) #f))
                         (let ((atom-key3697 (vector-ref p3692 0)))
                           (if (memv atom-key3697 (quote (each)))
                             (if (null? e3691)
                               (match-empty3683 (vector-ref p3692 1) r3694)
                               (let ((l3698 (match-each3681
                                              e3691
                                              (vector-ref p3692 1)
                                              w3693
                                              mod3695)))
                                 (if l3698
                                   (letrec ((collect3699
                                              (lambda (l3700)
                                                (if (null? (car l3700))
                                                  r3694
                                                  (cons (map car l3700)
                                                        (collect3699
                                                          (map cdr l3700)))))))
                                     (collect3699 l3698))
                                   #f)))
                             (if (memv atom-key3697 (quote (free-id)))
                               (if (id?2476 e3691)
                                 (if (free-id=?2499
                                       (wrap2504 e3691 w3693 mod3695)
                                       (vector-ref p3692 1))
                                   r3694
                                   #f)
                                 #f)
                               (if (memv atom-key3697 (quote (atom)))
                                 (if (equal?
                                       (vector-ref p3692 1)
                                       (strip2522 e3691 w3693))
                                   r3694
                                   #f)
                                 (if (memv atom-key3697 (quote (vector)))
                                   (if (vector? e3691)
                                     (match3685
                                       (vector->list e3691)
                                       (vector-ref p3692 1)
                                       w3693
                                       r3694
                                       mod3695)
                                     #f)))))))))))
               (match-empty3683
                 (lambda (p3701 r3702)
                   (if (null? p3701)
                     r3702
                     (if (eq? p3701 (quote any))
                       (cons (quote ()) r3702)
                       (if (pair? p3701)
                         (match-empty3683
                           (car p3701)
                           (match-empty3683 (cdr p3701) r3702))
                         (if (eq? p3701 (quote each-any))
                           (cons (quote ()) r3702)
                           (let ((atom-key3703 (vector-ref p3701 0)))
                             (if (memv atom-key3703 (quote (each)))
                               (match-empty3683 (vector-ref p3701 1) r3702)
                               (if (memv atom-key3703 (quote (free-id atom)))
                                 r3702
                                 (if (memv atom-key3703 (quote (vector)))
                                   (match-empty3683
                                     (vector-ref p3701 1)
                                     r3702)))))))))))
               (match-each-any3682
                 (lambda (e3704 w3705 mod3706)
                   (if (pair? e3704)
                     (let ((l3707 (match-each-any3682
                                    (cdr e3704)
                                    w3705
                                    mod3706)))
                       (if l3707
                         (cons (wrap2504 (car e3704) w3705 mod3706) l3707)
                         #f))
                     (if (null? e3704)
                       '()
                       (if (syntax-object?2460 e3704)
                         (match-each-any3682
                           (syntax-object-expression2461 e3704)
                           (join-wraps2495
                             w3705
                             (syntax-object-wrap2462 e3704))
                           mod3706)
                         #f)))))
               (match-each3681
                 (lambda (e3708 p3709 w3710 mod3711)
                   (if (pair? e3708)
                     (let ((first3712
                             (match3685
                               (car e3708)
                               p3709
                               w3710
                               '()
                               mod3711)))
                       (if first3712
                         (let ((rest3713
                                 (match-each3681
                                   (cdr e3708)
                                   p3709
                                   w3710
                                   mod3711)))
                           (if rest3713 (cons first3712 rest3713) #f))
                         #f))
                     (if (null? e3708)
                       '()
                       (if (syntax-object?2460 e3708)
                         (match-each3681
                           (syntax-object-expression2461 e3708)
                           p3709
                           (join-wraps2495
                             w3710
                             (syntax-object-wrap2462 e3708))
                           (syntax-object-module2463 e3708))
                         #f))))))
        (set! $sc-dispatch
          (lambda (e3714 p3715)
            (if (eq? p3715 (quote any))
              (list e3714)
              (if (syntax-object?2460 e3714)
                (match*3684
                  (syntax-object-expression2461 e3714)
                  p3715
                  (syntax-object-wrap2462 e3714)
                  '()
                  (syntax-object-module2463 e3714))
                (match*3684
                  e3714
                  p3715
                  '(())
                  '()
                  #f)))))))))

(define with-syntax
  (make-syncase-macro
    'macro
    (lambda (x3716)
      ((lambda (tmp3717)
         ((lambda (tmp3718)
            (if tmp3718
              (apply (lambda (_3719 e13720 e23721)
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
                             (cons e13720 e23721)))
                     tmp3718)
              ((lambda (tmp3723)
                 (if tmp3723
                   (apply (lambda (_3724 out3725 in3726 e13727 e23728)
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
                                  in3726
                                  '()
                                  (list out3725
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
                                              (cons e13727 e23728)))))
                          tmp3723)
                   ((lambda (tmp3730)
                      (if tmp3730
                        (apply (lambda (_3731 out3732 in3733 e13734 e23735)
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
                                             in3733)
                                       '()
                                       (list out3732
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
                                                   (cons e13734 e23735)))))
                               tmp3730)
                        (syntax-violation
                          #f
                          "source expression failed to match any pattern"
                          tmp3717)))
                    ($sc-dispatch
                      tmp3717
                      '(any #(each (any any)) any . each-any)))))
               ($sc-dispatch
                 tmp3717
                 '(any ((any any)) any . each-any)))))
          ($sc-dispatch
            tmp3717
            '(any () any . each-any))))
       x3716))))

(define syntax-rules
  (make-syncase-macro
    'macro
    (lambda (x3739)
      ((lambda (tmp3740)
         ((lambda (tmp3741)
            (if tmp3741
              (apply (lambda (_3742
                              k3743
                              keyword3744
                              pattern3745
                              template3746)
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
                                         (cons k3743
                                               (map (lambda (tmp3749 tmp3748)
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
                                                                  tmp3748)
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
                                                                  tmp3749)))
                                                    template3746
                                                    pattern3745))))))
                     tmp3741)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                tmp3740)))
          ($sc-dispatch
            tmp3740
            '(any each-any . #(each ((any . any) any))))))
       x3739))))

(define let*
  (make-extended-syncase-macro
    (module-ref (current-module) (quote let*))
    'macro
    (lambda (x3750)
      ((lambda (tmp3751)
         ((lambda (tmp3752)
            (if (if tmp3752
                  (apply (lambda (let*3753 x3754 v3755 e13756 e23757)
                           (and-map identifier? x3754))
                         tmp3752)
                  #f)
              (apply (lambda (let*3759 x3760 v3761 e13762 e23763)
                       (letrec ((f3764 (lambda (bindings3765)
                                         (if (null? bindings3765)
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
                                                       (cons e13762 e23763)))
                                           ((lambda (tmp3769)
                                              ((lambda (tmp3770)
                                                 (if tmp3770
                                                   (apply (lambda (body3771
                                                                   binding3772)
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
                                                                  (list binding3772)
                                                                  body3771))
                                                          tmp3770)
                                                   (syntax-violation
                                                     #f
                                                     "source expression failed to match any pattern"
                                                     tmp3769)))
                                               ($sc-dispatch
                                                 tmp3769
                                                 '(any any))))
                                            (list (f3764 (cdr bindings3765))
                                                  (car bindings3765)))))))
                         (f3764 (map list x3760 v3761))))
                     tmp3752)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                tmp3751)))
          ($sc-dispatch
            tmp3751
            '(any #(each (any any)) any . each-any))))
       x3750))))

(define do
  (make-extended-syncase-macro
    (module-ref (current-module) (quote do))
    'macro
    (lambda (orig-x3773)
      ((lambda (tmp3774)
         ((lambda (tmp3775)
            (if tmp3775
              (apply (lambda (_3776
                              var3777
                              init3778
                              step3779
                              e03780
                              e13781
                              c3782)
                       ((lambda (tmp3783)
                          ((lambda (tmp3784)
                             (if tmp3784
                               (apply (lambda (step3785)
                                        ((lambda (tmp3786)
                                           ((lambda (tmp3787)
                                              (if tmp3787
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
                                                                    var3777
                                                                    init3778)
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
                                                                           e03780)
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
                                                                             c3782
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
                                                                                         step3785)))))))
                                                       tmp3787)
                                                ((lambda (tmp3792)
                                                   (if tmp3792
                                                     (apply (lambda (e13793
                                                                     e23794)
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
                                                                         var3777
                                                                         init3778)
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
                                                                          e03780
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
                                                                                (cons e13793
                                                                                      e23794))
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
                                                                                  c3782
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
                                                                                              step3785)))))))
                                                            tmp3792)
                                                     (syntax-violation
                                                       #f
                                                       "source expression failed to match any pattern"
                                                       tmp3786)))
                                                 ($sc-dispatch
                                                   tmp3786
                                                   '(any . each-any)))))
                                            ($sc-dispatch tmp3786 (quote ()))))
                                         e13781))
                                      tmp3784)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 tmp3783)))
                           ($sc-dispatch tmp3783 (quote each-any))))
                        (map (lambda (v3801 s3802)
                               ((lambda (tmp3803)
                                  ((lambda (tmp3804)
                                     (if tmp3804
                                       (apply (lambda () v3801) tmp3804)
                                       ((lambda (tmp3805)
                                          (if tmp3805
                                            (apply (lambda (e3806) e3806)
                                                   tmp3805)
                                            ((lambda (_3807)
                                               (syntax-violation
                                                 'do
                                                 "bad step expression"
                                                 orig-x3773
                                                 s3802))
                                             tmp3803)))
                                        ($sc-dispatch tmp3803 (quote (any))))))
                                   ($sc-dispatch tmp3803 (quote ()))))
                                s3802))
                             var3777
                             step3779)))
                     tmp3775)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                tmp3774)))
          ($sc-dispatch
            tmp3774
            '(any #(each (any any . any))
                  (any . each-any)
                  .
                  each-any))))
       orig-x3773))))

(define quasiquote
  (make-extended-syncase-macro
    (module-ref (current-module) (quote quasiquote))
    'macro
    (letrec ((quasicons3810
               (lambda (x3814 y3815)
                 ((lambda (tmp3816)
                    ((lambda (tmp3817)
                       (if tmp3817
                         (apply (lambda (x3818 y3819)
                                  ((lambda (tmp3820)
                                     ((lambda (tmp3821)
                                        (if tmp3821
                                          (apply (lambda (dy3822)
                                                   ((lambda (tmp3823)
                                                      ((lambda (tmp3824)
                                                         (if tmp3824
                                                           (apply (lambda (dx3825)
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
                                                                          (cons dx3825
                                                                                dy3822)))
                                                                  tmp3824)
                                                           ((lambda (_3826)
                                                              (if (null? dy3822)
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
                                                                      x3818)
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
                                                                      x3818
                                                                      y3819)))
                                                            tmp3823)))
                                                       ($sc-dispatch
                                                         tmp3823
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
                                                    x3818))
                                                 tmp3821)
                                          ((lambda (tmp3827)
                                             (if tmp3827
                                               (apply (lambda (stuff3828)
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
                                                              (cons x3818
                                                                    stuff3828)))
                                                      tmp3827)
                                               ((lambda (else3829)
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
                                                        x3818
                                                        y3819))
                                                tmp3820)))
                                           ($sc-dispatch
                                             tmp3820
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
                                        tmp3820
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
                                   y3819))
                                tmp3817)
                         (syntax-violation
                           #f
                           "source expression failed to match any pattern"
                           tmp3816)))
                     ($sc-dispatch tmp3816 (quote (any any)))))
                  (list x3814 y3815))))
             (quasiappend3811
               (lambda (x3830 y3831)
                 ((lambda (tmp3832)
                    ((lambda (tmp3833)
                       (if tmp3833
                         (apply (lambda (x3834 y3835)
                                  ((lambda (tmp3836)
                                     ((lambda (tmp3837)
                                        (if tmp3837
                                          (apply (lambda () x3834) tmp3837)
                                          ((lambda (_3838)
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
                                                   x3834
                                                   y3835))
                                           tmp3836)))
                                      ($sc-dispatch
                                        tmp3836
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
                                   y3835))
                                tmp3833)
                         (syntax-violation
                           #f
                           "source expression failed to match any pattern"
                           tmp3832)))
                     ($sc-dispatch tmp3832 (quote (any any)))))
                  (list x3830 y3831))))
             (quasivector3812
               (lambda (x3839)
                 ((lambda (tmp3840)
                    ((lambda (x3841)
                       ((lambda (tmp3842)
                          ((lambda (tmp3843)
                             (if tmp3843
                               (apply (lambda (x3844)
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
                                              (list->vector x3844)))
                                      tmp3843)
                               ((lambda (tmp3846)
                                  (if tmp3846
                                    (apply (lambda (x3847)
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
                                                   x3847))
                                           tmp3846)
                                    ((lambda (_3849)
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
                                             x3841))
                                     tmp3842)))
                                ($sc-dispatch
                                  tmp3842
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
                             tmp3842
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
                        x3841))
                     tmp3840))
                  x3839)))
             (quasi3813
               (lambda (p3850 lev3851)
                 ((lambda (tmp3852)
                    ((lambda (tmp3853)
                       (if tmp3853
                         (apply (lambda (p3854)
                                  (if (= lev3851 0)
                                    p3854
                                    (quasicons3810
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
                                      (quasi3813 (list p3854) (- lev3851 1)))))
                                tmp3853)
                         ((lambda (tmp3855)
                            (if (if tmp3855
                                  (apply (lambda (args3856) (= lev3851 0))
                                         tmp3855)
                                  #f)
                              (apply (lambda (args3857)
                                       (syntax-violation
                                         'unquote
                                         "unquote takes exactly one argument"
                                         p3850
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
                                               args3857)))
                                     tmp3855)
                              ((lambda (tmp3858)
                                 (if tmp3858
                                   (apply (lambda (p3859 q3860)
                                            (if (= lev3851 0)
                                              (quasiappend3811
                                                p3859
                                                (quasi3813 q3860 lev3851))
                                              (quasicons3810
                                                (quasicons3810
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
                                                  (quasi3813
                                                    (list p3859)
                                                    (- lev3851 1)))
                                                (quasi3813 q3860 lev3851))))
                                          tmp3858)
                                   ((lambda (tmp3861)
                                      (if (if tmp3861
                                            (apply (lambda (args3862 q3863)
                                                     (= lev3851 0))
                                                   tmp3861)
                                            #f)
                                        (apply (lambda (args3864 q3865)
                                                 (syntax-violation
                                                   'unquote-splicing
                                                   "unquote-splicing takes exactly one argument"
                                                   p3850
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
                                                         args3864)))
                                               tmp3861)
                                        ((lambda (tmp3866)
                                           (if tmp3866
                                             (apply (lambda (p3867)
                                                      (quasicons3810
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
                                                        (quasi3813
                                                          (list p3867)
                                                          (+ lev3851 1))))
                                                    tmp3866)
                                             ((lambda (tmp3868)
                                                (if tmp3868
                                                  (apply (lambda (p3869 q3870)
                                                           (quasicons3810
                                                             (quasi3813
                                                               p3869
                                                               lev3851)
                                                             (quasi3813
                                                               q3870
                                                               lev3851)))
                                                         tmp3868)
                                                  ((lambda (tmp3871)
                                                     (if tmp3871
                                                       (apply (lambda (x3872)
                                                                (quasivector3812
                                                                  (quasi3813
                                                                    x3872
                                                                    lev3851)))
                                                              tmp3871)
                                                       ((lambda (p3874)
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
                                                                p3874))
                                                        tmp3852)))
                                                   ($sc-dispatch
                                                     tmp3852
                                                     '#(vector each-any)))))
                                              ($sc-dispatch
                                                tmp3852
                                                '(any . any)))))
                                         ($sc-dispatch
                                           tmp3852
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
                                      tmp3852
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
                                 tmp3852
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
                            tmp3852
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
                       tmp3852
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
                  p3850))))
      (lambda (x3875)
        ((lambda (tmp3876)
           ((lambda (tmp3877)
              (if tmp3877
                (apply (lambda (_3878 e3879) (quasi3813 e3879 0))
                       tmp3877)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  tmp3876)))
            ($sc-dispatch tmp3876 (quote (any any)))))
         x3875)))))

(define include
  (make-syncase-macro
    'macro
    (lambda (x3880)
      (letrec ((read-file3881
                 (lambda (fn3882 k3883)
                   (let ((p3884 (open-input-file fn3882)))
                     (letrec ((f3885 (lambda (x3886)
                                       (if (eof-object? x3886)
                                         (begin
                                           (close-input-port p3884)
                                           '())
                                         (cons (datum->syntax k3883 x3886)
                                               (f3885 (read p3884)))))))
                       (f3885 (read p3884)))))))
        ((lambda (tmp3887)
           ((lambda (tmp3888)
              (if tmp3888
                (apply (lambda (k3889 filename3890)
                         (let ((fn3891 (syntax->datum filename3890)))
                           ((lambda (tmp3892)
                              ((lambda (tmp3893)
                                 (if tmp3893
                                   (apply (lambda (exp3894)
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
                                                  exp3894))
                                          tmp3893)
                                   (syntax-violation
                                     #f
                                     "source expression failed to match any pattern"
                                     tmp3892)))
                               ($sc-dispatch tmp3892 (quote each-any))))
                            (read-file3881 fn3891 k3889))))
                       tmp3888)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  tmp3887)))
            ($sc-dispatch tmp3887 (quote (any any)))))
         x3880)))))

(define unquote
  (make-syncase-macro
    'macro
    (lambda (x3896)
      ((lambda (tmp3897)
         ((lambda (tmp3898)
            (if tmp3898
              (apply (lambda (_3899 e3900)
                       (syntax-violation
                         'unquote
                         "expression not valid outside of quasiquote"
                         x3896))
                     tmp3898)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                tmp3897)))
          ($sc-dispatch tmp3897 (quote (any any)))))
       x3896))))

(define unquote-splicing
  (make-syncase-macro
    'macro
    (lambda (x3901)
      ((lambda (tmp3902)
         ((lambda (tmp3903)
            (if tmp3903
              (apply (lambda (_3904 e3905)
                       (syntax-violation
                         'unquote-splicing
                         "expression not valid outside of quasiquote"
                         x3901))
                     tmp3903)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                tmp3902)))
          ($sc-dispatch tmp3902 (quote (any any)))))
       x3901))))

(define case
  (make-extended-syncase-macro
    (module-ref (current-module) (quote case))
    'macro
    (lambda (x3906)
      ((lambda (tmp3907)
         ((lambda (tmp3908)
            (if tmp3908
              (apply (lambda (_3909 e3910 m13911 m23912)
                       ((lambda (tmp3913)
                          ((lambda (body3914)
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
                                               e3910))
                                   body3914))
                           tmp3913))
                        (letrec ((f3915 (lambda (clause3916 clauses3917)
                                          (if (null? clauses3917)
                                            ((lambda (tmp3919)
                                               ((lambda (tmp3920)
                                                  (if tmp3920
                                                    (apply (lambda (e13921
                                                                    e23922)
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
                                                                   (cons e13921
                                                                         e23922)))
                                                           tmp3920)
                                                    ((lambda (tmp3924)
                                                       (if tmp3924
                                                         (apply (lambda (k3925
                                                                         e13926
                                                                         e23927)
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
                                                                                    k3925))
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
                                                                              (cons e13926
                                                                                    e23927))))
                                                                tmp3924)
                                                         ((lambda (_3930)
                                                            (syntax-violation
                                                              'case
                                                              "bad clause"
                                                              x3906
                                                              clause3916))
                                                          tmp3919)))
                                                     ($sc-dispatch
                                                       tmp3919
                                                       '(each-any
                                                          any
                                                          .
                                                          each-any)))))
                                                ($sc-dispatch
                                                  tmp3919
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
                                             clause3916)
                                            ((lambda (tmp3931)
                                               ((lambda (rest3932)
                                                  ((lambda (tmp3933)
                                                     ((lambda (tmp3934)
                                                        (if tmp3934
                                                          (apply (lambda (k3935
                                                                          e13936
                                                                          e23937)
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
                                                                                     k3935))
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
                                                                               (cons e13936
                                                                                     e23937))
                                                                         rest3932))
                                                                 tmp3934)
                                                          ((lambda (_3940)
                                                             (syntax-violation
                                                               'case
                                                               "bad clause"
                                                               x3906
                                                               clause3916))
                                                           tmp3933)))
                                                      ($sc-dispatch
                                                        tmp3933
                                                        '(each-any
                                                           any
                                                           .
                                                           each-any))))
                                                   clause3916))
                                                tmp3931))
                                             (f3915 (car clauses3917)
                                                    (cdr clauses3917)))))))
                          (f3915 m13911 m23912))))
                     tmp3908)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                tmp3907)))
          ($sc-dispatch
            tmp3907
            '(any any any . each-any))))
       x3906))))

(define identifier-syntax
  (make-syncase-macro
    'macro
    (lambda (x3941)
      ((lambda (tmp3942)
         ((lambda (tmp3943)
            (if tmp3943
              (apply (lambda (_3944 e3945)
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
                                               e3945))
                                   (list (cons _3944
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
                                               (cons e3945
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
                     tmp3943)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                tmp3942)))
          ($sc-dispatch tmp3942 (quote (any any)))))
       x3941))))

