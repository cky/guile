(eval-when (compile) (set-current-module (resolve-module (quote (guile)))))
(if #f #f)

(letrec ((#{and-map*\ 2581}#
           (lambda (#{f\ 2619}# #{first\ 2620}# . #{rest\ 2621}#)
             (let ((#{t\ 2622}# (null? #{first\ 2620}#)))
               (if #{t\ 2622}#
                 #{t\ 2622}#
                 (if (null? #{rest\ 2621}#)
                   (letrec ((#{andmap\ 2623}#
                              (lambda (#{first\ 2624}#)
                                (let ((#{x\ 2625}# (car #{first\ 2624}#))
                                      (#{first\ 2626}# (cdr #{first\ 2624}#)))
                                  (if (null? #{first\ 2626}#)
                                    (#{f\ 2619}# #{x\ 2625}#)
                                    (if (#{f\ 2619}# #{x\ 2625}#)
                                      (#{andmap\ 2623}# #{first\ 2626}#)
                                      #f))))))
                     (#{andmap\ 2623}# #{first\ 2620}#))
                   (letrec ((#{andmap\ 2627}#
                              (lambda (#{first\ 2628}# #{rest\ 2629}#)
                                (let ((#{x\ 2630}# (car #{first\ 2628}#))
                                      (#{xr\ 2631}# (map car #{rest\ 2629}#))
                                      (#{first\ 2632}# (cdr #{first\ 2628}#))
                                      (#{rest\ 2633}#
                                        (map cdr #{rest\ 2629}#)))
                                  (if (null? #{first\ 2632}#)
                                    (apply #{f\ 2619}#
                                           (cons #{x\ 2630}# #{xr\ 2631}#))
                                    (if (apply #{f\ 2619}#
                                               (cons #{x\ 2630}# #{xr\ 2631}#))
                                      (#{andmap\ 2627}#
                                        #{first\ 2632}#
                                        #{rest\ 2633}#)
                                      #f))))))
                     (#{andmap\ 2627}# #{first\ 2620}# #{rest\ 2621}#))))))))
  (letrec ((#{lambda-var-list\ 2732}#
             (lambda (#{vars\ 2856}#)
               (letrec ((#{lvl\ 2857}#
                          (lambda (#{vars\ 2858}# #{ls\ 2859}# #{w\ 2860}#)
                            (if (pair? #{vars\ 2858}#)
                              (#{lvl\ 2857}#
                                (cdr #{vars\ 2858}#)
                                (cons (#{wrap\ 2709}#
                                        (car #{vars\ 2858}#)
                                        #{w\ 2860}#
                                        #f)
                                      #{ls\ 2859}#)
                                #{w\ 2860}#)
                              (if (#{id?\ 2681}# #{vars\ 2858}#)
                                (cons (#{wrap\ 2709}#
                                        #{vars\ 2858}#
                                        #{w\ 2860}#
                                        #f)
                                      #{ls\ 2859}#)
                                (if (null? #{vars\ 2858}#)
                                  #{ls\ 2859}#
                                  (if (#{syntax-object?\ 2665}# #{vars\ 2858}#)
                                    (#{lvl\ 2857}#
                                      (#{syntax-object-expression\ 2666}#
                                        #{vars\ 2858}#)
                                      #{ls\ 2859}#
                                      (#{join-wraps\ 2700}#
                                        #{w\ 2860}#
                                        (#{syntax-object-wrap\ 2667}#
                                          #{vars\ 2858}#)))
                                    (cons #{vars\ 2858}# #{ls\ 2859}#))))))))
                 (#{lvl\ 2857}#
                   #{vars\ 2856}#
                   '()
                   '(())))))
           (#{gen-var\ 2731}#
             (lambda (#{id\ 2861}#)
               (let ((#{id\ 2862}#
                       (if (#{syntax-object?\ 2665}# #{id\ 2861}#)
                         (#{syntax-object-expression\ 2666}# #{id\ 2861}#)
                         #{id\ 2861}#)))
                 (gensym
                   (string-append (symbol->string #{id\ 2862}#) " ")))))
           (#{strip\ 2730}#
             (lambda (#{x\ 2863}# #{w\ 2864}#)
               (if (memq 'top
                         (#{wrap-marks\ 2684}# #{w\ 2864}#))
                 #{x\ 2863}#
                 (letrec ((#{f\ 2865}#
                            (lambda (#{x\ 2866}#)
                              (if (#{syntax-object?\ 2665}# #{x\ 2866}#)
                                (#{strip\ 2730}#
                                  (#{syntax-object-expression\ 2666}#
                                    #{x\ 2866}#)
                                  (#{syntax-object-wrap\ 2667}# #{x\ 2866}#))
                                (if (pair? #{x\ 2866}#)
                                  (let ((#{a\ 2867}#
                                          (#{f\ 2865}# (car #{x\ 2866}#)))
                                        (#{d\ 2868}#
                                          (#{f\ 2865}# (cdr #{x\ 2866}#))))
                                    (if (if (eq? #{a\ 2867}# (car #{x\ 2866}#))
                                          (eq? #{d\ 2868}# (cdr #{x\ 2866}#))
                                          #f)
                                      #{x\ 2866}#
                                      (cons #{a\ 2867}# #{d\ 2868}#)))
                                  (if (vector? #{x\ 2866}#)
                                    (let ((#{old\ 2869}#
                                            (vector->list #{x\ 2866}#)))
                                      (let ((#{new\ 2870}#
                                              (map #{f\ 2865}# #{old\ 2869}#)))
                                        (if (#{and-map*\ 2581}#
                                              eq?
                                              #{old\ 2869}#
                                              #{new\ 2870}#)
                                          #{x\ 2866}#
                                          (list->vector #{new\ 2870}#))))
                                    #{x\ 2866}#))))))
                   (#{f\ 2865}# #{x\ 2863}#)))))
           (#{chi-lambda-case\ 2729}#
             (lambda (#{e\ 2871}#
                      #{r\ 2872}#
                      #{w\ 2873}#
                      #{s\ 2874}#
                      #{mod\ 2875}#
                      #{get-formals\ 2876}#
                      #{clauses\ 2877}#)
               (letrec ((#{expand-body\ 2881}#
                          (lambda (#{req\ 2882}#
                                   #{opt\ 2883}#
                                   #{rest\ 2884}#
                                   #{kw\ 2885}#
                                   #{body\ 2886}#
                                   #{vars\ 2887}#
                                   #{r*\ 2888}#
                                   #{w*\ 2889}#
                                   #{inits\ 2890}#)
                            ((lambda (#{tmp\ 2891}#)
                               ((lambda (#{tmp\ 2892}#)
                                  (if (if #{tmp\ 2892}#
                                        (apply (lambda (#{docstring\ 2893}#
                                                        #{e1\ 2894}#
                                                        #{e2\ 2895}#)
                                                 (string?
                                                   (syntax->datum
                                                     #{docstring\ 2893}#)))
                                               #{tmp\ 2892}#)
                                        #f)
                                    (apply (lambda (#{docstring\ 2896}#
                                                    #{e1\ 2897}#
                                                    #{e2\ 2898}#)
                                             (values
                                               (syntax->datum
                                                 #{docstring\ 2896}#)
                                               #{req\ 2882}#
                                               #{opt\ 2883}#
                                               #{rest\ 2884}#
                                               #{kw\ 2885}#
                                               #{inits\ 2890}#
                                               #{vars\ 2887}#
                                               (#{chi-body\ 2721}#
                                                 (cons #{e1\ 2897}#
                                                       #{e2\ 2898}#)
                                                 (#{source-wrap\ 2710}#
                                                   #{e\ 2871}#
                                                   #{w\ 2873}#
                                                   #{s\ 2874}#
                                                   #{mod\ 2875}#)
                                                 #{r*\ 2888}#
                                                 #{w*\ 2889}#
                                                 #{mod\ 2875}#)))
                                           #{tmp\ 2892}#)
                                    ((lambda (#{tmp\ 2900}#)
                                       (if #{tmp\ 2900}#
                                         (apply (lambda (#{e1\ 2901}#
                                                         #{e2\ 2902}#)
                                                  (values
                                                    #f
                                                    #{req\ 2882}#
                                                    #{opt\ 2883}#
                                                    #{rest\ 2884}#
                                                    #{kw\ 2885}#
                                                    #{inits\ 2890}#
                                                    #{vars\ 2887}#
                                                    (#{chi-body\ 2721}#
                                                      (cons #{e1\ 2901}#
                                                            #{e2\ 2902}#)
                                                      (#{source-wrap\ 2710}#
                                                        #{e\ 2871}#
                                                        #{w\ 2873}#
                                                        #{s\ 2874}#
                                                        #{mod\ 2875}#)
                                                      #{r*\ 2888}#
                                                      #{w*\ 2889}#
                                                      #{mod\ 2875}#)))
                                                #{tmp\ 2900}#)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           #{tmp\ 2891}#)))
                                     ($sc-dispatch
                                       #{tmp\ 2891}#
                                       '(any . each-any)))))
                                ($sc-dispatch
                                  #{tmp\ 2891}#
                                  '(any any . each-any))))
                             #{body\ 2886}#)))
                        (#{expand-kw\ 2880}#
                          (lambda (#{req\ 2904}#
                                   #{opt\ 2905}#
                                   #{rest\ 2906}#
                                   #{kw\ 2907}#
                                   #{body\ 2908}#
                                   #{vars\ 2909}#
                                   #{r*\ 2910}#
                                   #{w*\ 2911}#
                                   #{aok\ 2912}#
                                   #{out\ 2913}#
                                   #{inits\ 2914}#)
                            (if (pair? #{kw\ 2907}#)
                              ((lambda (#{tmp\ 2915}#)
                                 ((lambda (#{tmp\ 2916}#)
                                    (if #{tmp\ 2916}#
                                      (apply (lambda (#{k\ 2917}#
                                                      #{id\ 2918}#
                                                      #{i\ 2919}#)
                                               (let ((#{v\ 2920}#
                                                       (#{gen-var\ 2731}#
                                                         #{id\ 2918}#)))
                                                 (let ((#{l\ 2921}#
                                                         (#{gen-labels\ 2687}#
                                                           (list #{v\ 2920}#))))
                                                   (let ((#{r**\ 2922}#
                                                           (#{extend-var-env\ 2676}#
                                                             #{l\ 2921}#
                                                             (list #{v\ 2920}#)
                                                             #{r*\ 2910}#)))
                                                     (let ((#{w**\ 2923}#
                                                             (#{make-binding-wrap\ 2698}#
                                                               (list #{id\ 2918}#)
                                                               #{l\ 2921}#
                                                               #{w*\ 2911}#)))
                                                       (#{expand-kw\ 2880}#
                                                         #{req\ 2904}#
                                                         #{opt\ 2905}#
                                                         #{rest\ 2906}#
                                                         (cdr #{kw\ 2907}#)
                                                         #{body\ 2908}#
                                                         (cons #{v\ 2920}#
                                                               #{vars\ 2909}#)
                                                         #{r**\ 2922}#
                                                         #{w**\ 2923}#
                                                         #{aok\ 2912}#
                                                         (cons (list (syntax->datum
                                                                       #{k\ 2917}#)
                                                                     (syntax->datum
                                                                       #{id\ 2918}#)
                                                                     #{v\ 2920}#)
                                                               #{out\ 2913}#)
                                                         (cons (#{chi\ 2717}#
                                                                 #{i\ 2919}#
                                                                 #{r*\ 2910}#
                                                                 #{w*\ 2911}#
                                                                 #{mod\ 2875}#)
                                                               #{inits\ 2914}#)))))))
                                             #{tmp\ 2916}#)
                                      (syntax-violation
                                        #f
                                        "source expression failed to match any pattern"
                                        #{tmp\ 2915}#)))
                                  ($sc-dispatch
                                    #{tmp\ 2915}#
                                    '(any any any))))
                               (car #{kw\ 2907}#))
                              (#{expand-body\ 2881}#
                                #{req\ 2904}#
                                #{opt\ 2905}#
                                #{rest\ 2906}#
                                (if (let ((#{t\ 2924}# #{aok\ 2912}#))
                                      (if #{t\ 2924}#
                                        #{t\ 2924}#
                                        (pair? #{out\ 2913}#)))
                                  (cons #{aok\ 2912}# (reverse #{out\ 2913}#))
                                  #f)
                                #{body\ 2908}#
                                (reverse #{vars\ 2909}#)
                                #{r*\ 2910}#
                                #{w*\ 2911}#
                                (reverse #{inits\ 2914}#)))))
                        (#{expand-opt\ 2879}#
                          (lambda (#{req\ 2925}#
                                   #{opt\ 2926}#
                                   #{rest\ 2927}#
                                   #{kw\ 2928}#
                                   #{body\ 2929}#
                                   #{vars\ 2930}#
                                   #{r*\ 2931}#
                                   #{w*\ 2932}#
                                   #{out\ 2933}#
                                   #{inits\ 2934}#)
                            (if (pair? #{opt\ 2926}#)
                              ((lambda (#{tmp\ 2935}#)
                                 ((lambda (#{tmp\ 2936}#)
                                    (if #{tmp\ 2936}#
                                      (apply (lambda (#{id\ 2937}# #{i\ 2938}#)
                                               (let ((#{v\ 2939}#
                                                       (#{gen-var\ 2731}#
                                                         #{id\ 2937}#)))
                                                 (let ((#{l\ 2940}#
                                                         (#{gen-labels\ 2687}#
                                                           (list #{v\ 2939}#))))
                                                   (let ((#{r**\ 2941}#
                                                           (#{extend-var-env\ 2676}#
                                                             #{l\ 2940}#
                                                             (list #{v\ 2939}#)
                                                             #{r*\ 2931}#)))
                                                     (let ((#{w**\ 2942}#
                                                             (#{make-binding-wrap\ 2698}#
                                                               (list #{id\ 2937}#)
                                                               #{l\ 2940}#
                                                               #{w*\ 2932}#)))
                                                       (#{expand-opt\ 2879}#
                                                         #{req\ 2925}#
                                                         (cdr #{opt\ 2926}#)
                                                         #{rest\ 2927}#
                                                         #{kw\ 2928}#
                                                         #{body\ 2929}#
                                                         (cons #{v\ 2939}#
                                                               #{vars\ 2930}#)
                                                         #{r**\ 2941}#
                                                         #{w**\ 2942}#
                                                         (cons (syntax->datum
                                                                 #{id\ 2937}#)
                                                               #{out\ 2933}#)
                                                         (cons (#{chi\ 2717}#
                                                                 #{i\ 2938}#
                                                                 #{r*\ 2931}#
                                                                 #{w*\ 2932}#
                                                                 #{mod\ 2875}#)
                                                               #{inits\ 2934}#)))))))
                                             #{tmp\ 2936}#)
                                      (syntax-violation
                                        #f
                                        "source expression failed to match any pattern"
                                        #{tmp\ 2935}#)))
                                  ($sc-dispatch
                                    #{tmp\ 2935}#
                                    '(any any))))
                               (car #{opt\ 2926}#))
                              (if #{rest\ 2927}#
                                (let ((#{v\ 2943}#
                                        (#{gen-var\ 2731}# #{rest\ 2927}#)))
                                  (let ((#{l\ 2944}#
                                          (#{gen-labels\ 2687}#
                                            (list #{v\ 2943}#))))
                                    (let ((#{r*\ 2945}#
                                            (#{extend-var-env\ 2676}#
                                              #{l\ 2944}#
                                              (list #{v\ 2943}#)
                                              #{r*\ 2931}#)))
                                      (let ((#{w*\ 2946}#
                                              (#{make-binding-wrap\ 2698}#
                                                (list #{rest\ 2927}#)
                                                #{l\ 2944}#
                                                #{w*\ 2932}#)))
                                        (#{expand-kw\ 2880}#
                                          #{req\ 2925}#
                                          (if (pair? #{out\ 2933}#)
                                            (reverse #{out\ 2933}#)
                                            #f)
                                          (syntax->datum #{rest\ 2927}#)
                                          (if (pair? #{kw\ 2928}#)
                                            (cdr #{kw\ 2928}#)
                                            #{kw\ 2928}#)
                                          #{body\ 2929}#
                                          (cons #{v\ 2943}# #{vars\ 2930}#)
                                          #{r*\ 2945}#
                                          #{w*\ 2946}#
                                          (if (pair? #{kw\ 2928}#)
                                            (car #{kw\ 2928}#)
                                            #f)
                                          '()
                                          #{inits\ 2934}#)))))
                                (#{expand-kw\ 2880}#
                                  #{req\ 2925}#
                                  (if (pair? #{out\ 2933}#)
                                    (reverse #{out\ 2933}#)
                                    #f)
                                  #f
                                  (if (pair? #{kw\ 2928}#)
                                    (cdr #{kw\ 2928}#)
                                    #{kw\ 2928}#)
                                  #{body\ 2929}#
                                  #{vars\ 2930}#
                                  #{r*\ 2931}#
                                  #{w*\ 2932}#
                                  (if (pair? #{kw\ 2928}#)
                                    (car #{kw\ 2928}#)
                                    #f)
                                  '()
                                  #{inits\ 2934}#)))))
                        (#{expand-req\ 2878}#
                          (lambda (#{req\ 2947}#
                                   #{opt\ 2948}#
                                   #{rest\ 2949}#
                                   #{kw\ 2950}#
                                   #{body\ 2951}#)
                            (let ((#{vars\ 2952}#
                                    (map #{gen-var\ 2731}# #{req\ 2947}#))
                                  (#{labels\ 2953}#
                                    (#{gen-labels\ 2687}# #{req\ 2947}#)))
                              (let ((#{r*\ 2954}#
                                      (#{extend-var-env\ 2676}#
                                        #{labels\ 2953}#
                                        #{vars\ 2952}#
                                        #{r\ 2872}#))
                                    (#{w*\ 2955}#
                                      (#{make-binding-wrap\ 2698}#
                                        #{req\ 2947}#
                                        #{labels\ 2953}#
                                        #{w\ 2873}#)))
                                (#{expand-opt\ 2879}#
                                  (map syntax->datum #{req\ 2947}#)
                                  #{opt\ 2948}#
                                  #{rest\ 2949}#
                                  #{kw\ 2950}#
                                  #{body\ 2951}#
                                  (reverse #{vars\ 2952}#)
                                  #{r*\ 2954}#
                                  #{w*\ 2955}#
                                  '()
                                  '()))))))
                 ((lambda (#{tmp\ 2956}#)
                    ((lambda (#{tmp\ 2957}#)
                       (if #{tmp\ 2957}#
                         (apply (lambda () (values #f #f)) #{tmp\ 2957}#)
                         ((lambda (#{tmp\ 2958}#)
                            (if #{tmp\ 2958}#
                              (apply (lambda (#{args\ 2959}#
                                              #{e1\ 2960}#
                                              #{e2\ 2961}#
                                              #{args*\ 2962}#
                                              #{e1*\ 2963}#
                                              #{e2*\ 2964}#)
                                       (call-with-values
                                         (lambda ()
                                           (#{get-formals\ 2876}#
                                             #{args\ 2959}#))
                                         (lambda (#{req\ 2965}#
                                                  #{opt\ 2966}#
                                                  #{rest\ 2967}#
                                                  #{kw\ 2968}#)
                                           (call-with-values
                                             (lambda ()
                                               (#{expand-req\ 2878}#
                                                 #{req\ 2965}#
                                                 #{opt\ 2966}#
                                                 #{rest\ 2967}#
                                                 #{kw\ 2968}#
                                                 (cons #{e1\ 2960}#
                                                       #{e2\ 2961}#)))
                                             (lambda (#{docstring\ 2970}#
                                                      #{req\ 2971}#
                                                      #{opt\ 2972}#
                                                      #{rest\ 2973}#
                                                      #{kw\ 2974}#
                                                      #{inits\ 2975}#
                                                      #{vars\ 2976}#
                                                      #{body\ 2977}#)
                                               (call-with-values
                                                 (lambda ()
                                                   (#{chi-lambda-case\ 2729}#
                                                     #{e\ 2871}#
                                                     #{r\ 2872}#
                                                     #{w\ 2873}#
                                                     #{s\ 2874}#
                                                     #{mod\ 2875}#
                                                     #{get-formals\ 2876}#
                                                     (map (lambda (#{tmp\ 2980}#
                                                                   #{tmp\ 2979}#
                                                                   #{tmp\ 2978}#)
                                                            (cons #{tmp\ 2978}#
                                                                  (cons #{tmp\ 2979}#
                                                                        #{tmp\ 2980}#)))
                                                          #{e2*\ 2964}#
                                                          #{e1*\ 2963}#
                                                          #{args*\ 2962}#)))
                                                 (lambda (#{docstring*\ 2982}#
                                                          #{else*\ 2983}#)
                                                   (values
                                                     (let ((#{t\ 2984}#
                                                             #{docstring\ 2970}#))
                                                       (if #{t\ 2984}#
                                                         #{t\ 2984}#
                                                         #{docstring*\ 2982}#))
                                                     (#{build-lambda-case\ 2657}#
                                                       #{s\ 2874}#
                                                       #{req\ 2971}#
                                                       #{opt\ 2972}#
                                                       #{rest\ 2973}#
                                                       #{kw\ 2974}#
                                                       #{inits\ 2975}#
                                                       #{vars\ 2976}#
                                                       #{body\ 2977}#
                                                       #{else*\ 2983}#)))))))))
                                     #{tmp\ 2958}#)
                              (syntax-violation
                                #f
                                "source expression failed to match any pattern"
                                #{tmp\ 2956}#)))
                          ($sc-dispatch
                            #{tmp\ 2956}#
                            '((any any . each-any)
                              .
                              #(each (any any . each-any)))))))
                     ($sc-dispatch #{tmp\ 2956}# (quote ()))))
                  #{clauses\ 2877}#))))
           (#{lambda*-formals\ 2728}#
             (lambda (#{orig-args\ 2985}#)
               (letrec ((#{check\ 2990}#
                          (lambda (#{req\ 2991}#
                                   #{opt\ 2992}#
                                   #{rest\ 2993}#
                                   #{kw\ 2994}#)
                            (if (#{distinct-bound-ids?\ 2707}#
                                  (append
                                    #{req\ 2991}#
                                    (map car #{opt\ 2992}#)
                                    (if #{rest\ 2993}#
                                      (list #{rest\ 2993}#)
                                      '())
                                    (if (pair? #{kw\ 2994}#)
                                      (map cadr (cdr #{kw\ 2994}#))
                                      '())))
                              (values
                                #{req\ 2991}#
                                #{opt\ 2992}#
                                #{rest\ 2993}#
                                #{kw\ 2994}#)
                              (syntax-violation
                                'lambda*
                                "duplicate identifier in argument list"
                                #{orig-args\ 2985}#))))
                        (#{rest\ 2989}#
                          (lambda (#{args\ 2995}#
                                   #{req\ 2996}#
                                   #{opt\ 2997}#
                                   #{kw\ 2998}#)
                            ((lambda (#{tmp\ 2999}#)
                               ((lambda (#{tmp\ 3000}#)
                                  (if (if #{tmp\ 3000}#
                                        (apply (lambda (#{r\ 3001}#)
                                                 (#{id?\ 2681}# #{r\ 3001}#))
                                               #{tmp\ 3000}#)
                                        #f)
                                    (apply (lambda (#{r\ 3002}#)
                                             (#{check\ 2990}#
                                               #{req\ 2996}#
                                               #{opt\ 2997}#
                                               #{r\ 3002}#
                                               #{kw\ 2998}#))
                                           #{tmp\ 3000}#)
                                    ((lambda (#{else\ 3003}#)
                                       (syntax-violation
                                         'lambda*
                                         "invalid rest argument"
                                         #{orig-args\ 2985}#
                                         #{args\ 2995}#))
                                     #{tmp\ 2999}#)))
                                (list #{tmp\ 2999}#)))
                             #{args\ 2995}#)))
                        (#{key\ 2988}#
                          (lambda (#{args\ 3004}#
                                   #{req\ 3005}#
                                   #{opt\ 3006}#
                                   #{rkey\ 3007}#)
                            ((lambda (#{tmp\ 3008}#)
                               ((lambda (#{tmp\ 3009}#)
                                  (if #{tmp\ 3009}#
                                    (apply (lambda ()
                                             (#{check\ 2990}#
                                               #{req\ 3005}#
                                               #{opt\ 3006}#
                                               #f
                                               (cons #f
                                                     (reverse
                                                       #{rkey\ 3007}#))))
                                           #{tmp\ 3009}#)
                                    ((lambda (#{tmp\ 3010}#)
                                       (if (if #{tmp\ 3010}#
                                             (apply (lambda (#{a\ 3011}#
                                                             #{b\ 3012}#)
                                                      (#{id?\ 2681}#
                                                        #{a\ 3011}#))
                                                    #{tmp\ 3010}#)
                                             #f)
                                         (apply (lambda (#{a\ 3013}#
                                                         #{b\ 3014}#)
                                                  ((lambda (#{tmp\ 3015}#)
                                                     ((lambda (#{k\ 3016}#)
                                                        (#{key\ 2988}#
                                                          #{b\ 3014}#
                                                          #{req\ 3005}#
                                                          #{opt\ 3006}#
                                                          (cons (cons #{k\ 3016}#
                                                                      (cons #{a\ 3013}#
                                                                            '(#(syntax-object
                                                                                #f
                                                                                ((top)
                                                                                 #(ribcage
                                                                                   #(k)
                                                                                   #((top))
                                                                                   #("i"))
                                                                                 #(ribcage
                                                                                   #(a
                                                                                     b)
                                                                                   #((top)
                                                                                     (top))
                                                                                   #("i"
                                                                                     "i"))
                                                                                 #(ribcage
                                                                                   ()
                                                                                   ()
                                                                                   ())
                                                                                 #(ribcage
                                                                                   #(args
                                                                                     req
                                                                                     opt
                                                                                     rkey)
                                                                                   #((top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top))
                                                                                   #("i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"))
                                                                                 #(ribcage
                                                                                   (check rest
                                                                                          key
                                                                                          opt
                                                                                          req)
                                                                                   ((top)
                                                                                    (top)
                                                                                    (top)
                                                                                    (top)
                                                                                    (top))
                                                                                   ("i"
                                                                                    "i"
                                                                                    "i"
                                                                                    "i"
                                                                                    "i"))
                                                                                 #(ribcage
                                                                                   #(orig-args)
                                                                                   #((top))
                                                                                   #("i"))
                                                                                 #(ribcage
                                                                                   (lambda-var-list
                                                                                     gen-var
                                                                                     strip
                                                                                     chi-lambda-case
                                                                                     lambda*-formals
                                                                                     chi-simple-lambda
                                                                                     lambda-formals
                                                                                     ellipsis?
                                                                                     chi-void
                                                                                     eval-local-transformer
                                                                                     chi-local-syntax
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
                                                                                     build-lambda-case
                                                                                     build-case-lambda
                                                                                     build-simple-lambda
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
                                                                                  guile)))))
                                                                #{rkey\ 3007}#)))
                                                      #{tmp\ 3015}#))
                                                   (symbol->keyword
                                                     (syntax->datum
                                                       #{a\ 3013}#))))
                                                #{tmp\ 3010}#)
                                         ((lambda (#{tmp\ 3017}#)
                                            (if (if #{tmp\ 3017}#
                                                  (apply (lambda (#{a\ 3018}#
                                                                  #{init\ 3019}#
                                                                  #{b\ 3020}#)
                                                           (#{id?\ 2681}#
                                                             #{a\ 3018}#))
                                                         #{tmp\ 3017}#)
                                                  #f)
                                              (apply (lambda (#{a\ 3021}#
                                                              #{init\ 3022}#
                                                              #{b\ 3023}#)
                                                       ((lambda (#{tmp\ 3024}#)
                                                          ((lambda (#{k\ 3025}#)
                                                             (#{key\ 2988}#
                                                               #{b\ 3023}#
                                                               #{req\ 3005}#
                                                               #{opt\ 3006}#
                                                               (cons (list #{k\ 3025}#
                                                                           #{a\ 3021}#
                                                                           #{init\ 3022}#)
                                                                     #{rkey\ 3007}#)))
                                                           #{tmp\ 3024}#))
                                                        (symbol->keyword
                                                          (syntax->datum
                                                            #{a\ 3021}#))))
                                                     #{tmp\ 3017}#)
                                              ((lambda (#{tmp\ 3026}#)
                                                 (if (if #{tmp\ 3026}#
                                                       (apply (lambda (#{a\ 3027}#
                                                                       #{init\ 3028}#
                                                                       #{k\ 3029}#
                                                                       #{b\ 3030}#)
                                                                (if (#{id?\ 2681}#
                                                                      #{a\ 3027}#)
                                                                  (keyword?
                                                                    (syntax->datum
                                                                      #{k\ 3029}#))
                                                                  #f))
                                                              #{tmp\ 3026}#)
                                                       #f)
                                                   (apply (lambda (#{a\ 3031}#
                                                                   #{init\ 3032}#
                                                                   #{k\ 3033}#
                                                                   #{b\ 3034}#)
                                                            (#{key\ 2988}#
                                                              #{b\ 3034}#
                                                              #{req\ 3005}#
                                                              #{opt\ 3006}#
                                                              (cons (list #{k\ 3033}#
                                                                          #{a\ 3031}#
                                                                          #{init\ 3032}#)
                                                                    #{rkey\ 3007}#)))
                                                          #{tmp\ 3026}#)
                                                   ((lambda (#{tmp\ 3035}#)
                                                      (if (if #{tmp\ 3035}#
                                                            (apply (lambda (#{aok\ 3036}#)
                                                                     (eq? (syntax->datum
                                                                            #{aok\ 3036}#)
                                                                          #:allow-other-keys))
                                                                   #{tmp\ 3035}#)
                                                            #f)
                                                        (apply (lambda (#{aok\ 3037}#)
                                                                 (#{check\ 2990}#
                                                                   #{req\ 3005}#
                                                                   #{opt\ 3006}#
                                                                   #f
                                                                   (cons #t
                                                                         (reverse
                                                                           #{rkey\ 3007}#))))
                                                               #{tmp\ 3035}#)
                                                        ((lambda (#{tmp\ 3038}#)
                                                           (if (if #{tmp\ 3038}#
                                                                 (apply (lambda (#{aok\ 3039}#
                                                                                 #{a\ 3040}#
                                                                                 #{b\ 3041}#)
                                                                          (if (eq? (syntax->datum
                                                                                     #{aok\ 3039}#)
                                                                                   #:allow-other-keys)
                                                                            (eq? (syntax->datum
                                                                                   #{a\ 3040}#)
                                                                                 #:rest)
                                                                            #f))
                                                                        #{tmp\ 3038}#)
                                                                 #f)
                                                             (apply (lambda (#{aok\ 3042}#
                                                                             #{a\ 3043}#
                                                                             #{b\ 3044}#)
                                                                      (#{rest\ 2989}#
                                                                        #{b\ 3044}#
                                                                        #{req\ 3005}#
                                                                        #{opt\ 3006}#
                                                                        (cons #t
                                                                              (reverse
                                                                                #{rkey\ 3007}#))))
                                                                    #{tmp\ 3038}#)
                                                             ((lambda (#{tmp\ 3045}#)
                                                                (if (if #{tmp\ 3045}#
                                                                      (apply (lambda (#{aok\ 3046}#
                                                                                      #{r\ 3047}#)
                                                                               (if (eq? (syntax->datum
                                                                                          #{aok\ 3046}#)
                                                                                        #:allow-other-keys)
                                                                                 (#{id?\ 2681}#
                                                                                   #{r\ 3047}#)
                                                                                 #f))
                                                                             #{tmp\ 3045}#)
                                                                      #f)
                                                                  (apply (lambda (#{aok\ 3048}#
                                                                                  #{r\ 3049}#)
                                                                           (#{rest\ 2989}#
                                                                             #{r\ 3049}#
                                                                             #{req\ 3005}#
                                                                             #{opt\ 3006}#
                                                                             (cons #t
                                                                                   (reverse
                                                                                     #{rkey\ 3007}#))))
                                                                         #{tmp\ 3045}#)
                                                                  ((lambda (#{tmp\ 3050}#)
                                                                     (if (if #{tmp\ 3050}#
                                                                           (apply (lambda (#{a\ 3051}#
                                                                                           #{b\ 3052}#)
                                                                                    (eq? (syntax->datum
                                                                                           #{a\ 3051}#)
                                                                                         #:rest))
                                                                                  #{tmp\ 3050}#)
                                                                           #f)
                                                                       (apply (lambda (#{a\ 3053}#
                                                                                       #{b\ 3054}#)
                                                                                (#{rest\ 2989}#
                                                                                  #{b\ 3054}#
                                                                                  #{req\ 3005}#
                                                                                  #{opt\ 3006}#
                                                                                  (cons #f
                                                                                        (reverse
                                                                                          #{rkey\ 3007}#))))
                                                                              #{tmp\ 3050}#)
                                                                       ((lambda (#{tmp\ 3055}#)
                                                                          (if (if #{tmp\ 3055}#
                                                                                (apply (lambda (#{r\ 3056}#)
                                                                                         (#{id?\ 2681}#
                                                                                           #{r\ 3056}#))
                                                                                       #{tmp\ 3055}#)
                                                                                #f)
                                                                            (apply (lambda (#{r\ 3057}#)
                                                                                     (#{rest\ 2989}#
                                                                                       #{r\ 3057}#
                                                                                       #{req\ 3005}#
                                                                                       #{opt\ 3006}#
                                                                                       (cons #f
                                                                                             (reverse
                                                                                               #{rkey\ 3007}#))))
                                                                                   #{tmp\ 3055}#)
                                                                            ((lambda (#{else\ 3058}#)
                                                                               (syntax-violation
                                                                                 'lambda*
                                                                                 "invalid keyword argument list"
                                                                                 #{orig-args\ 2985}#
                                                                                 #{args\ 3004}#))
                                                                             #{tmp\ 3008}#)))
                                                                        (list #{tmp\ 3008}#))))
                                                                   ($sc-dispatch
                                                                     #{tmp\ 3008}#
                                                                     '(any any)))))
                                                              ($sc-dispatch
                                                                #{tmp\ 3008}#
                                                                '(any .
                                                                      any)))))
                                                         ($sc-dispatch
                                                           #{tmp\ 3008}#
                                                           '(any any any)))))
                                                    ($sc-dispatch
                                                      #{tmp\ 3008}#
                                                      '(any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 3008}#
                                                 '((any any any) . any)))))
                                          ($sc-dispatch
                                            #{tmp\ 3008}#
                                            '((any any) . any)))))
                                     ($sc-dispatch
                                       #{tmp\ 3008}#
                                       '(any . any)))))
                                ($sc-dispatch #{tmp\ 3008}# (quote ()))))
                             #{args\ 3004}#)))
                        (#{opt\ 2987}#
                          (lambda (#{args\ 3059}# #{req\ 3060}# #{ropt\ 3061}#)
                            ((lambda (#{tmp\ 3062}#)
                               ((lambda (#{tmp\ 3063}#)
                                  (if #{tmp\ 3063}#
                                    (apply (lambda ()
                                             (#{check\ 2990}#
                                               #{req\ 3060}#
                                               (reverse #{ropt\ 3061}#)
                                               #f
                                               '()))
                                           #{tmp\ 3063}#)
                                    ((lambda (#{tmp\ 3064}#)
                                       (if (if #{tmp\ 3064}#
                                             (apply (lambda (#{a\ 3065}#
                                                             #{b\ 3066}#)
                                                      (#{id?\ 2681}#
                                                        #{a\ 3065}#))
                                                    #{tmp\ 3064}#)
                                             #f)
                                         (apply (lambda (#{a\ 3067}#
                                                         #{b\ 3068}#)
                                                  (#{opt\ 2987}#
                                                    #{b\ 3068}#
                                                    #{req\ 3060}#
                                                    (cons (cons #{a\ 3067}#
                                                                '(#(syntax-object
                                                                    #f
                                                                    ((top)
                                                                     #(ribcage
                                                                       #(a b)
                                                                       #((top)
                                                                         (top))
                                                                       #("i"
                                                                         "i"))
                                                                     #(ribcage
                                                                       ()
                                                                       ()
                                                                       ())
                                                                     #(ribcage
                                                                       #(args
                                                                         req
                                                                         ropt)
                                                                       #((top)
                                                                         (top)
                                                                         (top))
                                                                       #("i"
                                                                         "i"
                                                                         "i"))
                                                                     #(ribcage
                                                                       (check rest
                                                                              key
                                                                              opt
                                                                              req)
                                                                       ((top)
                                                                        (top)
                                                                        (top)
                                                                        (top)
                                                                        (top))
                                                                       ("i"
                                                                        "i"
                                                                        "i"
                                                                        "i"
                                                                        "i"))
                                                                     #(ribcage
                                                                       #(orig-args)
                                                                       #((top))
                                                                       #("i"))
                                                                     #(ribcage
                                                                       (lambda-var-list
                                                                         gen-var
                                                                         strip
                                                                         chi-lambda-case
                                                                         lambda*-formals
                                                                         chi-simple-lambda
                                                                         lambda-formals
                                                                         ellipsis?
                                                                         chi-void
                                                                         eval-local-transformer
                                                                         chi-local-syntax
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
                                                                         build-lambda-case
                                                                         build-case-lambda
                                                                         build-simple-lambda
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
                                                                      guile))))
                                                          #{ropt\ 3061}#)))
                                                #{tmp\ 3064}#)
                                         ((lambda (#{tmp\ 3069}#)
                                            (if (if #{tmp\ 3069}#
                                                  (apply (lambda (#{a\ 3070}#
                                                                  #{init\ 3071}#
                                                                  #{b\ 3072}#)
                                                           (#{id?\ 2681}#
                                                             #{a\ 3070}#))
                                                         #{tmp\ 3069}#)
                                                  #f)
                                              (apply (lambda (#{a\ 3073}#
                                                              #{init\ 3074}#
                                                              #{b\ 3075}#)
                                                       (#{opt\ 2987}#
                                                         #{b\ 3075}#
                                                         #{req\ 3060}#
                                                         (cons (list #{a\ 3073}#
                                                                     #{init\ 3074}#)
                                                               #{ropt\ 3061}#)))
                                                     #{tmp\ 3069}#)
                                              ((lambda (#{tmp\ 3076}#)
                                                 (if (if #{tmp\ 3076}#
                                                       (apply (lambda (#{a\ 3077}#
                                                                       #{b\ 3078}#)
                                                                (eq? (syntax->datum
                                                                       #{a\ 3077}#)
                                                                     #:key))
                                                              #{tmp\ 3076}#)
                                                       #f)
                                                   (apply (lambda (#{a\ 3079}#
                                                                   #{b\ 3080}#)
                                                            (#{key\ 2988}#
                                                              #{b\ 3080}#
                                                              #{req\ 3060}#
                                                              (reverse
                                                                #{ropt\ 3061}#)
                                                              '()))
                                                          #{tmp\ 3076}#)
                                                   ((lambda (#{tmp\ 3081}#)
                                                      (if (if #{tmp\ 3081}#
                                                            (apply (lambda (#{a\ 3082}#
                                                                            #{b\ 3083}#)
                                                                     (eq? (syntax->datum
                                                                            #{a\ 3082}#)
                                                                          #:rest))
                                                                   #{tmp\ 3081}#)
                                                            #f)
                                                        (apply (lambda (#{a\ 3084}#
                                                                        #{b\ 3085}#)
                                                                 (#{rest\ 2989}#
                                                                   #{b\ 3085}#
                                                                   #{req\ 3060}#
                                                                   (reverse
                                                                     #{ropt\ 3061}#)
                                                                   '()))
                                                               #{tmp\ 3081}#)
                                                        ((lambda (#{tmp\ 3086}#)
                                                           (if (if #{tmp\ 3086}#
                                                                 (apply (lambda (#{r\ 3087}#)
                                                                          (#{id?\ 2681}#
                                                                            #{r\ 3087}#))
                                                                        #{tmp\ 3086}#)
                                                                 #f)
                                                             (apply (lambda (#{r\ 3088}#)
                                                                      (#{rest\ 2989}#
                                                                        #{r\ 3088}#
                                                                        #{req\ 3060}#
                                                                        (reverse
                                                                          #{ropt\ 3061}#)
                                                                        '()))
                                                                    #{tmp\ 3086}#)
                                                             ((lambda (#{else\ 3089}#)
                                                                (syntax-violation
                                                                  'lambda*
                                                                  "invalid optional argument list"
                                                                  #{orig-args\ 2985}#
                                                                  #{args\ 3059}#))
                                                              #{tmp\ 3062}#)))
                                                         (list #{tmp\ 3062}#))))
                                                    ($sc-dispatch
                                                      #{tmp\ 3062}#
                                                      '(any any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 3062}#
                                                 '(any . any)))))
                                          ($sc-dispatch
                                            #{tmp\ 3062}#
                                            '((any any) . any)))))
                                     ($sc-dispatch
                                       #{tmp\ 3062}#
                                       '(any . any)))))
                                ($sc-dispatch #{tmp\ 3062}# (quote ()))))
                             #{args\ 3059}#)))
                        (#{req\ 2986}#
                          (lambda (#{args\ 3090}# #{rreq\ 3091}#)
                            ((lambda (#{tmp\ 3092}#)
                               ((lambda (#{tmp\ 3093}#)
                                  (if #{tmp\ 3093}#
                                    (apply (lambda ()
                                             (#{check\ 2990}#
                                               (reverse #{rreq\ 3091}#)
                                               '()
                                               #f
                                               '()))
                                           #{tmp\ 3093}#)
                                    ((lambda (#{tmp\ 3094}#)
                                       (if (if #{tmp\ 3094}#
                                             (apply (lambda (#{a\ 3095}#
                                                             #{b\ 3096}#)
                                                      (#{id?\ 2681}#
                                                        #{a\ 3095}#))
                                                    #{tmp\ 3094}#)
                                             #f)
                                         (apply (lambda (#{a\ 3097}#
                                                         #{b\ 3098}#)
                                                  (#{req\ 2986}#
                                                    #{b\ 3098}#
                                                    (cons #{a\ 3097}#
                                                          #{rreq\ 3091}#)))
                                                #{tmp\ 3094}#)
                                         ((lambda (#{tmp\ 3099}#)
                                            (if (if #{tmp\ 3099}#
                                                  (apply (lambda (#{a\ 3100}#
                                                                  #{b\ 3101}#)
                                                           (eq? (syntax->datum
                                                                  #{a\ 3100}#)
                                                                #:optional))
                                                         #{tmp\ 3099}#)
                                                  #f)
                                              (apply (lambda (#{a\ 3102}#
                                                              #{b\ 3103}#)
                                                       (#{opt\ 2987}#
                                                         #{b\ 3103}#
                                                         (reverse
                                                           #{rreq\ 3091}#)
                                                         '()))
                                                     #{tmp\ 3099}#)
                                              ((lambda (#{tmp\ 3104}#)
                                                 (if (if #{tmp\ 3104}#
                                                       (apply (lambda (#{a\ 3105}#
                                                                       #{b\ 3106}#)
                                                                (eq? (syntax->datum
                                                                       #{a\ 3105}#)
                                                                     #:key))
                                                              #{tmp\ 3104}#)
                                                       #f)
                                                   (apply (lambda (#{a\ 3107}#
                                                                   #{b\ 3108}#)
                                                            (#{key\ 2988}#
                                                              #{b\ 3108}#
                                                              (reverse
                                                                #{rreq\ 3091}#)
                                                              '()
                                                              '()))
                                                          #{tmp\ 3104}#)
                                                   ((lambda (#{tmp\ 3109}#)
                                                      (if (if #{tmp\ 3109}#
                                                            (apply (lambda (#{a\ 3110}#
                                                                            #{b\ 3111}#)
                                                                     (eq? (syntax->datum
                                                                            #{a\ 3110}#)
                                                                          #:rest))
                                                                   #{tmp\ 3109}#)
                                                            #f)
                                                        (apply (lambda (#{a\ 3112}#
                                                                        #{b\ 3113}#)
                                                                 (#{rest\ 2989}#
                                                                   #{b\ 3113}#
                                                                   (reverse
                                                                     #{rreq\ 3091}#)
                                                                   '()
                                                                   '()))
                                                               #{tmp\ 3109}#)
                                                        ((lambda (#{tmp\ 3114}#)
                                                           (if (if #{tmp\ 3114}#
                                                                 (apply (lambda (#{r\ 3115}#)
                                                                          (#{id?\ 2681}#
                                                                            #{r\ 3115}#))
                                                                        #{tmp\ 3114}#)
                                                                 #f)
                                                             (apply (lambda (#{r\ 3116}#)
                                                                      (#{rest\ 2989}#
                                                                        #{r\ 3116}#
                                                                        (reverse
                                                                          #{rreq\ 3091}#)
                                                                        '()
                                                                        '()))
                                                                    #{tmp\ 3114}#)
                                                             ((lambda (#{else\ 3117}#)
                                                                (syntax-violation
                                                                  'lambda*
                                                                  "invalid argument list"
                                                                  #{orig-args\ 2985}#
                                                                  #{args\ 3090}#))
                                                              #{tmp\ 3092}#)))
                                                         (list #{tmp\ 3092}#))))
                                                    ($sc-dispatch
                                                      #{tmp\ 3092}#
                                                      '(any any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 3092}#
                                                 '(any . any)))))
                                          ($sc-dispatch
                                            #{tmp\ 3092}#
                                            '(any . any)))))
                                     ($sc-dispatch
                                       #{tmp\ 3092}#
                                       '(any . any)))))
                                ($sc-dispatch #{tmp\ 3092}# (quote ()))))
                             #{args\ 3090}#))))
                 (#{req\ 2986}# #{orig-args\ 2985}# (quote ())))))
           (#{chi-simple-lambda\ 2727}#
             (lambda (#{e\ 3118}#
                      #{r\ 3119}#
                      #{w\ 3120}#
                      #{s\ 3121}#
                      #{mod\ 3122}#
                      #{req\ 3123}#
                      #{rest\ 3124}#
                      #{docstring\ 3125}#
                      #{body\ 3126}#)
               (let ((#{ids\ 3127}#
                       (if #{rest\ 3124}#
                         (append #{req\ 3123}# (list #{rest\ 3124}#))
                         #{req\ 3123}#)))
                 (let ((#{vars\ 3128}#
                         (map #{gen-var\ 2731}# #{ids\ 3127}#)))
                   (let ((#{labels\ 3129}#
                           (#{gen-labels\ 2687}# #{ids\ 3127}#)))
                     (#{build-simple-lambda\ 2655}#
                       #{s\ 3121}#
                       (map syntax->datum #{req\ 3123}#)
                       (if #{rest\ 3124}#
                         (syntax->datum #{rest\ 3124}#)
                         #f)
                       #{vars\ 3128}#
                       #{docstring\ 3125}#
                       (#{chi-body\ 2721}#
                         #{body\ 3126}#
                         (#{source-wrap\ 2710}#
                           #{e\ 3118}#
                           #{w\ 3120}#
                           #{s\ 3121}#
                           #{mod\ 3122}#)
                         (#{extend-var-env\ 2676}#
                           #{labels\ 3129}#
                           #{vars\ 3128}#
                           #{r\ 3119}#)
                         (#{make-binding-wrap\ 2698}#
                           #{ids\ 3127}#
                           #{labels\ 3129}#
                           #{w\ 3120}#)
                         #{mod\ 3122}#)))))))
           (#{lambda-formals\ 2726}#
             (lambda (#{orig-args\ 3130}#)
               (letrec ((#{check\ 3132}#
                          (lambda (#{req\ 3133}# #{rest\ 3134}#)
                            (if (#{distinct-bound-ids?\ 2707}#
                                  (if #{rest\ 3134}#
                                    (cons #{rest\ 3134}# #{req\ 3133}#)
                                    #{req\ 3133}#))
                              (values #{req\ 3133}# #f #{rest\ 3134}# #f)
                              (syntax-violation
                                'lambda
                                "duplicate identifier in argument list"
                                #{orig-args\ 3130}#))))
                        (#{req\ 3131}#
                          (lambda (#{args\ 3135}# #{rreq\ 3136}#)
                            ((lambda (#{tmp\ 3137}#)
                               ((lambda (#{tmp\ 3138}#)
                                  (if #{tmp\ 3138}#
                                    (apply (lambda ()
                                             (#{check\ 3132}#
                                               (reverse #{rreq\ 3136}#)
                                               #f))
                                           #{tmp\ 3138}#)
                                    ((lambda (#{tmp\ 3139}#)
                                       (if (if #{tmp\ 3139}#
                                             (apply (lambda (#{a\ 3140}#
                                                             #{b\ 3141}#)
                                                      (#{id?\ 2681}#
                                                        #{a\ 3140}#))
                                                    #{tmp\ 3139}#)
                                             #f)
                                         (apply (lambda (#{a\ 3142}#
                                                         #{b\ 3143}#)
                                                  (#{req\ 3131}#
                                                    #{b\ 3143}#
                                                    (cons #{a\ 3142}#
                                                          #{rreq\ 3136}#)))
                                                #{tmp\ 3139}#)
                                         ((lambda (#{tmp\ 3144}#)
                                            (if (if #{tmp\ 3144}#
                                                  (apply (lambda (#{r\ 3145}#)
                                                           (#{id?\ 2681}#
                                                             #{r\ 3145}#))
                                                         #{tmp\ 3144}#)
                                                  #f)
                                              (apply (lambda (#{r\ 3146}#)
                                                       (#{check\ 3132}#
                                                         (reverse
                                                           #{rreq\ 3136}#)
                                                         #{r\ 3146}#))
                                                     #{tmp\ 3144}#)
                                              ((lambda (#{else\ 3147}#)
                                                 (syntax-violation
                                                   'lambda
                                                   "invalid argument list"
                                                   #{orig-args\ 3130}#
                                                   #{args\ 3135}#))
                                               #{tmp\ 3137}#)))
                                          (list #{tmp\ 3137}#))))
                                     ($sc-dispatch
                                       #{tmp\ 3137}#
                                       '(any . any)))))
                                ($sc-dispatch #{tmp\ 3137}# (quote ()))))
                             #{args\ 3135}#))))
                 (#{req\ 3131}# #{orig-args\ 3130}# (quote ())))))
           (#{ellipsis?\ 2725}#
             (lambda (#{x\ 3148}#)
               (if (#{nonsymbol-id?\ 2680}# #{x\ 3148}#)
                 (#{free-id=?\ 2704}#
                   #{x\ 3148}#
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
                           chi-lambda-case
                           lambda*-formals
                           chi-simple-lambda
                           lambda-formals
                           ellipsis?
                           chi-void
                           eval-local-transformer
                           chi-local-syntax
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
                           build-lambda-case
                           build-case-lambda
                           build-simple-lambda
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
           (#{chi-void\ 2724}#
             (lambda () (#{build-void\ 2645}# #f)))
           (#{eval-local-transformer\ 2723}#
             (lambda (#{expanded\ 3149}# #{mod\ 3150}#)
               (let ((#{p\ 3151}#
                       (#{local-eval-hook\ 2641}#
                         #{expanded\ 3149}#
                         #{mod\ 3150}#)))
                 (if (procedure? #{p\ 3151}#)
                   #{p\ 3151}#
                   (syntax-violation
                     #f
                     "nonprocedure transformer"
                     #{p\ 3151}#)))))
           (#{chi-local-syntax\ 2722}#
             (lambda (#{rec?\ 3152}#
                      #{e\ 3153}#
                      #{r\ 3154}#
                      #{w\ 3155}#
                      #{s\ 3156}#
                      #{mod\ 3157}#
                      #{k\ 3158}#)
               ((lambda (#{tmp\ 3159}#)
                  ((lambda (#{tmp\ 3160}#)
                     (if #{tmp\ 3160}#
                       (apply (lambda (#{_\ 3161}#
                                       #{id\ 3162}#
                                       #{val\ 3163}#
                                       #{e1\ 3164}#
                                       #{e2\ 3165}#)
                                (let ((#{ids\ 3166}# #{id\ 3162}#))
                                  (if (not (#{valid-bound-ids?\ 2706}#
                                             #{ids\ 3166}#))
                                    (syntax-violation
                                      #f
                                      "duplicate bound keyword"
                                      #{e\ 3153}#)
                                    (let ((#{labels\ 3168}#
                                            (#{gen-labels\ 2687}#
                                              #{ids\ 3166}#)))
                                      (let ((#{new-w\ 3169}#
                                              (#{make-binding-wrap\ 2698}#
                                                #{ids\ 3166}#
                                                #{labels\ 3168}#
                                                #{w\ 3155}#)))
                                        (#{k\ 3158}#
                                          (cons #{e1\ 3164}# #{e2\ 3165}#)
                                          (#{extend-env\ 2675}#
                                            #{labels\ 3168}#
                                            (let ((#{w\ 3171}#
                                                    (if #{rec?\ 3152}#
                                                      #{new-w\ 3169}#
                                                      #{w\ 3155}#))
                                                  (#{trans-r\ 3172}#
                                                    (#{macros-only-env\ 2677}#
                                                      #{r\ 3154}#)))
                                              (map (lambda (#{x\ 3173}#)
                                                     (cons 'macro
                                                           (#{eval-local-transformer\ 2723}#
                                                             (#{chi\ 2717}#
                                                               #{x\ 3173}#
                                                               #{trans-r\ 3172}#
                                                               #{w\ 3171}#
                                                               #{mod\ 3157}#)
                                                             #{mod\ 3157}#)))
                                                   #{val\ 3163}#))
                                            #{r\ 3154}#)
                                          #{new-w\ 3169}#
                                          #{s\ 3156}#
                                          #{mod\ 3157}#))))))
                              #{tmp\ 3160}#)
                       ((lambda (#{_\ 3175}#)
                          (syntax-violation
                            #f
                            "bad local syntax definition"
                            (#{source-wrap\ 2710}#
                              #{e\ 3153}#
                              #{w\ 3155}#
                              #{s\ 3156}#
                              #{mod\ 3157}#)))
                        #{tmp\ 3159}#)))
                   ($sc-dispatch
                     #{tmp\ 3159}#
                     '(any #(each (any any)) any . each-any))))
                #{e\ 3153}#)))
           (#{chi-body\ 2721}#
             (lambda (#{body\ 3176}#
                      #{outer-form\ 3177}#
                      #{r\ 3178}#
                      #{w\ 3179}#
                      #{mod\ 3180}#)
               (let ((#{r\ 3181}#
                       (cons '("placeholder" placeholder)
                             #{r\ 3178}#)))
                 (let ((#{ribcage\ 3182}#
                         (#{make-ribcage\ 2688}#
                           '()
                           '()
                           '())))
                   (let ((#{w\ 3183}#
                           (#{make-wrap\ 2683}#
                             (#{wrap-marks\ 2684}# #{w\ 3179}#)
                             (cons #{ribcage\ 3182}#
                                   (#{wrap-subst\ 2685}# #{w\ 3179}#)))))
                     (letrec ((#{parse\ 3184}#
                                (lambda (#{body\ 3185}#
                                         #{ids\ 3186}#
                                         #{labels\ 3187}#
                                         #{var-ids\ 3188}#
                                         #{vars\ 3189}#
                                         #{vals\ 3190}#
                                         #{bindings\ 3191}#)
                                  (if (null? #{body\ 3185}#)
                                    (syntax-violation
                                      #f
                                      "no expressions in body"
                                      #{outer-form\ 3177}#)
                                    (let ((#{e\ 3193}# (cdar #{body\ 3185}#))
                                          (#{er\ 3194}# (caar #{body\ 3185}#)))
                                      (call-with-values
                                        (lambda ()
                                          (#{syntax-type\ 2715}#
                                            #{e\ 3193}#
                                            #{er\ 3194}#
                                            '(())
                                            (#{source-annotation\ 2672}#
                                              #{er\ 3194}#)
                                            #{ribcage\ 3182}#
                                            #{mod\ 3180}#
                                            #f))
                                        (lambda (#{type\ 3195}#
                                                 #{value\ 3196}#
                                                 #{e\ 3197}#
                                                 #{w\ 3198}#
                                                 #{s\ 3199}#
                                                 #{mod\ 3200}#)
                                          (if (memv #{type\ 3195}#
                                                    '(define-form))
                                            (let ((#{id\ 3201}#
                                                    (#{wrap\ 2709}#
                                                      #{value\ 3196}#
                                                      #{w\ 3198}#
                                                      #{mod\ 3200}#))
                                                  (#{label\ 3202}#
                                                    (#{gen-label\ 2686}#)))
                                              (let ((#{var\ 3203}#
                                                      (#{gen-var\ 2731}#
                                                        #{id\ 3201}#)))
                                                (begin
                                                  (#{extend-ribcage!\ 2697}#
                                                    #{ribcage\ 3182}#
                                                    #{id\ 3201}#
                                                    #{label\ 3202}#)
                                                  (#{parse\ 3184}#
                                                    (cdr #{body\ 3185}#)
                                                    (cons #{id\ 3201}#
                                                          #{ids\ 3186}#)
                                                    (cons #{label\ 3202}#
                                                          #{labels\ 3187}#)
                                                    (cons #{id\ 3201}#
                                                          #{var-ids\ 3188}#)
                                                    (cons #{var\ 3203}#
                                                          #{vars\ 3189}#)
                                                    (cons (cons #{er\ 3194}#
                                                                (#{wrap\ 2709}#
                                                                  #{e\ 3197}#
                                                                  #{w\ 3198}#
                                                                  #{mod\ 3200}#))
                                                          #{vals\ 3190}#)
                                                    (cons (cons 'lexical
                                                                #{var\ 3203}#)
                                                          #{bindings\ 3191}#)))))
                                            (if (memv #{type\ 3195}#
                                                      '(define-syntax-form))
                                              (let ((#{id\ 3204}#
                                                      (#{wrap\ 2709}#
                                                        #{value\ 3196}#
                                                        #{w\ 3198}#
                                                        #{mod\ 3200}#))
                                                    (#{label\ 3205}#
                                                      (#{gen-label\ 2686}#)))
                                                (begin
                                                  (#{extend-ribcage!\ 2697}#
                                                    #{ribcage\ 3182}#
                                                    #{id\ 3204}#
                                                    #{label\ 3205}#)
                                                  (#{parse\ 3184}#
                                                    (cdr #{body\ 3185}#)
                                                    (cons #{id\ 3204}#
                                                          #{ids\ 3186}#)
                                                    (cons #{label\ 3205}#
                                                          #{labels\ 3187}#)
                                                    #{var-ids\ 3188}#
                                                    #{vars\ 3189}#
                                                    #{vals\ 3190}#
                                                    (cons (cons 'macro
                                                                (cons #{er\ 3194}#
                                                                      (#{wrap\ 2709}#
                                                                        #{e\ 3197}#
                                                                        #{w\ 3198}#
                                                                        #{mod\ 3200}#)))
                                                          #{bindings\ 3191}#))))
                                              (if (memv #{type\ 3195}#
                                                        '(begin-form))
                                                ((lambda (#{tmp\ 3206}#)
                                                   ((lambda (#{tmp\ 3207}#)
                                                      (if #{tmp\ 3207}#
                                                        (apply (lambda (#{_\ 3208}#
                                                                        #{e1\ 3209}#)
                                                                 (#{parse\ 3184}#
                                                                   (letrec ((#{f\ 3210}#
                                                                              (lambda (#{forms\ 3211}#)
                                                                                (if (null? #{forms\ 3211}#)
                                                                                  (cdr #{body\ 3185}#)
                                                                                  (cons (cons #{er\ 3194}#
                                                                                              (#{wrap\ 2709}#
                                                                                                (car #{forms\ 3211}#)
                                                                                                #{w\ 3198}#
                                                                                                #{mod\ 3200}#))
                                                                                        (#{f\ 3210}#
                                                                                          (cdr #{forms\ 3211}#)))))))
                                                                     (#{f\ 3210}#
                                                                       #{e1\ 3209}#))
                                                                   #{ids\ 3186}#
                                                                   #{labels\ 3187}#
                                                                   #{var-ids\ 3188}#
                                                                   #{vars\ 3189}#
                                                                   #{vals\ 3190}#
                                                                   #{bindings\ 3191}#))
                                                               #{tmp\ 3207}#)
                                                        (syntax-violation
                                                          #f
                                                          "source expression failed to match any pattern"
                                                          #{tmp\ 3206}#)))
                                                    ($sc-dispatch
                                                      #{tmp\ 3206}#
                                                      '(any . each-any))))
                                                 #{e\ 3197}#)
                                                (if (memv #{type\ 3195}#
                                                          '(local-syntax-form))
                                                  (#{chi-local-syntax\ 2722}#
                                                    #{value\ 3196}#
                                                    #{e\ 3197}#
                                                    #{er\ 3194}#
                                                    #{w\ 3198}#
                                                    #{s\ 3199}#
                                                    #{mod\ 3200}#
                                                    (lambda (#{forms\ 3213}#
                                                             #{er\ 3214}#
                                                             #{w\ 3215}#
                                                             #{s\ 3216}#
                                                             #{mod\ 3217}#)
                                                      (#{parse\ 3184}#
                                                        (letrec ((#{f\ 3218}#
                                                                   (lambda (#{forms\ 3219}#)
                                                                     (if (null? #{forms\ 3219}#)
                                                                       (cdr #{body\ 3185}#)
                                                                       (cons (cons #{er\ 3214}#
                                                                                   (#{wrap\ 2709}#
                                                                                     (car #{forms\ 3219}#)
                                                                                     #{w\ 3215}#
                                                                                     #{mod\ 3217}#))
                                                                             (#{f\ 3218}#
                                                                               (cdr #{forms\ 3219}#)))))))
                                                          (#{f\ 3218}#
                                                            #{forms\ 3213}#))
                                                        #{ids\ 3186}#
                                                        #{labels\ 3187}#
                                                        #{var-ids\ 3188}#
                                                        #{vars\ 3189}#
                                                        #{vals\ 3190}#
                                                        #{bindings\ 3191}#)))
                                                  (if (null? #{ids\ 3186}#)
                                                    (#{build-sequence\ 2660}#
                                                      #f
                                                      (map (lambda (#{x\ 3220}#)
                                                             (#{chi\ 2717}#
                                                               (cdr #{x\ 3220}#)
                                                               (car #{x\ 3220}#)
                                                               '(())
                                                               #{mod\ 3200}#))
                                                           (cons (cons #{er\ 3194}#
                                                                       (#{source-wrap\ 2710}#
                                                                         #{e\ 3197}#
                                                                         #{w\ 3198}#
                                                                         #{s\ 3199}#
                                                                         #{mod\ 3200}#))
                                                                 (cdr #{body\ 3185}#))))
                                                    (begin
                                                      (if (not (#{valid-bound-ids?\ 2706}#
                                                                 #{ids\ 3186}#))
                                                        (syntax-violation
                                                          #f
                                                          "invalid or duplicate identifier in definition"
                                                          #{outer-form\ 3177}#))
                                                      (letrec ((#{loop\ 3221}#
                                                                 (lambda (#{bs\ 3222}#
                                                                          #{er-cache\ 3223}#
                                                                          #{r-cache\ 3224}#)
                                                                   (if (not (null? #{bs\ 3222}#))
                                                                     (let ((#{b\ 3225}#
                                                                             (car #{bs\ 3222}#)))
                                                                       (if (eq? (car #{b\ 3225}#)
                                                                                'macro)
                                                                         (let ((#{er\ 3226}#
                                                                                 (cadr #{b\ 3225}#)))
                                                                           (let ((#{r-cache\ 3227}#
                                                                                   (if (eq? #{er\ 3226}#
                                                                                            #{er-cache\ 3223}#)
                                                                                     #{r-cache\ 3224}#
                                                                                     (#{macros-only-env\ 2677}#
                                                                                       #{er\ 3226}#))))
                                                                             (begin
                                                                               (set-cdr!
                                                                                 #{b\ 3225}#
                                                                                 (#{eval-local-transformer\ 2723}#
                                                                                   (#{chi\ 2717}#
                                                                                     (cddr #{b\ 3225}#)
                                                                                     #{r-cache\ 3227}#
                                                                                     '(())
                                                                                     #{mod\ 3200}#)
                                                                                   #{mod\ 3200}#))
                                                                               (#{loop\ 3221}#
                                                                                 (cdr #{bs\ 3222}#)
                                                                                 #{er\ 3226}#
                                                                                 #{r-cache\ 3227}#))))
                                                                         (#{loop\ 3221}#
                                                                           (cdr #{bs\ 3222}#)
                                                                           #{er-cache\ 3223}#
                                                                           #{r-cache\ 3224}#)))))))
                                                        (#{loop\ 3221}#
                                                          #{bindings\ 3191}#
                                                          #f
                                                          #f))
                                                      (set-cdr!
                                                        #{r\ 3181}#
                                                        (#{extend-env\ 2675}#
                                                          #{labels\ 3187}#
                                                          #{bindings\ 3191}#
                                                          (cdr #{r\ 3181}#)))
                                                      (#{build-letrec\ 2663}#
                                                        #f
                                                        (map syntax->datum
                                                             #{var-ids\ 3188}#)
                                                        #{vars\ 3189}#
                                                        (map (lambda (#{x\ 3228}#)
                                                               (#{chi\ 2717}#
                                                                 (cdr #{x\ 3228}#)
                                                                 (car #{x\ 3228}#)
                                                                 '(())
                                                                 #{mod\ 3200}#))
                                                             #{vals\ 3190}#)
                                                        (#{build-sequence\ 2660}#
                                                          #f
                                                          (map (lambda (#{x\ 3229}#)
                                                                 (#{chi\ 2717}#
                                                                   (cdr #{x\ 3229}#)
                                                                   (car #{x\ 3229}#)
                                                                   '(())
                                                                   #{mod\ 3200}#))
                                                               (cons (cons #{er\ 3194}#
                                                                           (#{source-wrap\ 2710}#
                                                                             #{e\ 3197}#
                                                                             #{w\ 3198}#
                                                                             #{s\ 3199}#
                                                                             #{mod\ 3200}#))
                                                                     (cdr #{body\ 3185}#))))))))))))))))))
                       (#{parse\ 3184}#
                         (map (lambda (#{x\ 3192}#)
                                (cons #{r\ 3181}#
                                      (#{wrap\ 2709}#
                                        #{x\ 3192}#
                                        #{w\ 3183}#
                                        #{mod\ 3180}#)))
                              #{body\ 3176}#)
                         '()
                         '()
                         '()
                         '()
                         '()
                         '())))))))
           (#{chi-macro\ 2720}#
             (lambda (#{p\ 3230}#
                      #{e\ 3231}#
                      #{r\ 3232}#
                      #{w\ 3233}#
                      #{rib\ 3234}#
                      #{mod\ 3235}#)
               (letrec ((#{rebuild-macro-output\ 3236}#
                          (lambda (#{x\ 3237}# #{m\ 3238}#)
                            (if (pair? #{x\ 3237}#)
                              (cons (#{rebuild-macro-output\ 3236}#
                                      (car #{x\ 3237}#)
                                      #{m\ 3238}#)
                                    (#{rebuild-macro-output\ 3236}#
                                      (cdr #{x\ 3237}#)
                                      #{m\ 3238}#))
                              (if (#{syntax-object?\ 2665}# #{x\ 3237}#)
                                (let ((#{w\ 3239}#
                                        (#{syntax-object-wrap\ 2667}#
                                          #{x\ 3237}#)))
                                  (let ((#{ms\ 3240}#
                                          (#{wrap-marks\ 2684}# #{w\ 3239}#))
                                        (#{s\ 3241}#
                                          (#{wrap-subst\ 2685}# #{w\ 3239}#)))
                                    (if (if (pair? #{ms\ 3240}#)
                                          (eq? (car #{ms\ 3240}#) #f)
                                          #f)
                                      (#{make-syntax-object\ 2664}#
                                        (#{syntax-object-expression\ 2666}#
                                          #{x\ 3237}#)
                                        (#{make-wrap\ 2683}#
                                          (cdr #{ms\ 3240}#)
                                          (if #{rib\ 3234}#
                                            (cons #{rib\ 3234}#
                                                  (cdr #{s\ 3241}#))
                                            (cdr #{s\ 3241}#)))
                                        (#{syntax-object-module\ 2668}#
                                          #{x\ 3237}#))
                                      (#{make-syntax-object\ 2664}#
                                        (#{syntax-object-expression\ 2666}#
                                          #{x\ 3237}#)
                                        (#{make-wrap\ 2683}#
                                          (cons #{m\ 3238}# #{ms\ 3240}#)
                                          (if #{rib\ 3234}#
                                            (cons #{rib\ 3234}#
                                                  (cons 'shift
                                                        #{s\ 3241}#))
                                            (cons (quote shift) #{s\ 3241}#)))
                                        (let ((#{pmod\ 3242}#
                                                (procedure-module
                                                  #{p\ 3230}#)))
                                          (if #{pmod\ 3242}#
                                            (cons 'hygiene
                                                  (module-name #{pmod\ 3242}#))
                                            '(hygiene guile)))))))
                                (if (vector? #{x\ 3237}#)
                                  (let ((#{n\ 3243}#
                                          (vector-length #{x\ 3237}#)))
                                    (let ((#{v\ 3244}#
                                            (make-vector #{n\ 3243}#)))
                                      (letrec ((#{loop\ 3245}#
                                                 (lambda (#{i\ 3246}#)
                                                   (if (#{fx=\ 2638}#
                                                         #{i\ 3246}#
                                                         #{n\ 3243}#)
                                                     (begin
                                                       (if #f #f)
                                                       #{v\ 3244}#)
                                                     (begin
                                                       (vector-set!
                                                         #{v\ 3244}#
                                                         #{i\ 3246}#
                                                         (#{rebuild-macro-output\ 3236}#
                                                           (vector-ref
                                                             #{x\ 3237}#
                                                             #{i\ 3246}#)
                                                           #{m\ 3238}#))
                                                       (#{loop\ 3245}#
                                                         (#{fx+\ 2636}#
                                                           #{i\ 3246}#
                                                           1)))))))
                                        (#{loop\ 3245}# 0))))
                                  (if (symbol? #{x\ 3237}#)
                                    (syntax-violation
                                      #f
                                      "encountered raw symbol in macro output"
                                      (#{source-wrap\ 2710}#
                                        #{e\ 3231}#
                                        #{w\ 3233}#
                                        (#{wrap-subst\ 2685}# #{w\ 3233}#)
                                        #{mod\ 3235}#)
                                      #{x\ 3237}#)
                                    #{x\ 3237}#)))))))
                 (#{rebuild-macro-output\ 3236}#
                   (#{p\ 3230}#
                     (#{wrap\ 2709}#
                       #{e\ 3231}#
                       (#{anti-mark\ 2696}# #{w\ 3233}#)
                       #{mod\ 3235}#))
                   (string #\m)))))
           (#{chi-application\ 2719}#
             (lambda (#{x\ 3247}#
                      #{e\ 3248}#
                      #{r\ 3249}#
                      #{w\ 3250}#
                      #{s\ 3251}#
                      #{mod\ 3252}#)
               ((lambda (#{tmp\ 3253}#)
                  ((lambda (#{tmp\ 3254}#)
                     (if #{tmp\ 3254}#
                       (apply (lambda (#{e0\ 3255}# #{e1\ 3256}#)
                                (#{build-application\ 2646}#
                                  #{s\ 3251}#
                                  #{x\ 3247}#
                                  (map (lambda (#{e\ 3257}#)
                                         (#{chi\ 2717}#
                                           #{e\ 3257}#
                                           #{r\ 3249}#
                                           #{w\ 3250}#
                                           #{mod\ 3252}#))
                                       #{e1\ 3256}#)))
                              #{tmp\ 3254}#)
                       (syntax-violation
                         #f
                         "source expression failed to match any pattern"
                         #{tmp\ 3253}#)))
                   ($sc-dispatch
                     #{tmp\ 3253}#
                     '(any . each-any))))
                #{e\ 3248}#)))
           (#{chi-expr\ 2718}#
             (lambda (#{type\ 3259}#
                      #{value\ 3260}#
                      #{e\ 3261}#
                      #{r\ 3262}#
                      #{w\ 3263}#
                      #{s\ 3264}#
                      #{mod\ 3265}#)
               (if (memv #{type\ 3259}# (quote (lexical)))
                 (#{build-lexical-reference\ 2648}#
                   'value
                   #{s\ 3264}#
                   #{e\ 3261}#
                   #{value\ 3260}#)
                 (if (memv #{type\ 3259}# (quote (core core-form)))
                   (#{value\ 3260}#
                     #{e\ 3261}#
                     #{r\ 3262}#
                     #{w\ 3263}#
                     #{s\ 3264}#
                     #{mod\ 3265}#)
                   (if (memv #{type\ 3259}# (quote (module-ref)))
                     (call-with-values
                       (lambda () (#{value\ 3260}# #{e\ 3261}#))
                       (lambda (#{id\ 3266}# #{mod\ 3267}#)
                         (#{build-global-reference\ 2651}#
                           #{s\ 3264}#
                           #{id\ 3266}#
                           #{mod\ 3267}#)))
                     (if (memv #{type\ 3259}# (quote (lexical-call)))
                       (#{chi-application\ 2719}#
                         (#{build-lexical-reference\ 2648}#
                           'fun
                           (#{source-annotation\ 2672}# (car #{e\ 3261}#))
                           (car #{e\ 3261}#)
                           #{value\ 3260}#)
                         #{e\ 3261}#
                         #{r\ 3262}#
                         #{w\ 3263}#
                         #{s\ 3264}#
                         #{mod\ 3265}#)
                       (if (memv #{type\ 3259}# (quote (global-call)))
                         (#{chi-application\ 2719}#
                           (#{build-global-reference\ 2651}#
                             (#{source-annotation\ 2672}# (car #{e\ 3261}#))
                             (if (#{syntax-object?\ 2665}# #{value\ 3260}#)
                               (#{syntax-object-expression\ 2666}#
                                 #{value\ 3260}#)
                               #{value\ 3260}#)
                             (if (#{syntax-object?\ 2665}# #{value\ 3260}#)
                               (#{syntax-object-module\ 2668}# #{value\ 3260}#)
                               #{mod\ 3265}#))
                           #{e\ 3261}#
                           #{r\ 3262}#
                           #{w\ 3263}#
                           #{s\ 3264}#
                           #{mod\ 3265}#)
                         (if (memv #{type\ 3259}# (quote (constant)))
                           (#{build-data\ 2659}#
                             #{s\ 3264}#
                             (#{strip\ 2730}#
                               (#{source-wrap\ 2710}#
                                 #{e\ 3261}#
                                 #{w\ 3263}#
                                 #{s\ 3264}#
                                 #{mod\ 3265}#)
                               '(())))
                           (if (memv #{type\ 3259}# (quote (global)))
                             (#{build-global-reference\ 2651}#
                               #{s\ 3264}#
                               #{value\ 3260}#
                               #{mod\ 3265}#)
                             (if (memv #{type\ 3259}# (quote (call)))
                               (#{chi-application\ 2719}#
                                 (#{chi\ 2717}#
                                   (car #{e\ 3261}#)
                                   #{r\ 3262}#
                                   #{w\ 3263}#
                                   #{mod\ 3265}#)
                                 #{e\ 3261}#
                                 #{r\ 3262}#
                                 #{w\ 3263}#
                                 #{s\ 3264}#
                                 #{mod\ 3265}#)
                               (if (memv #{type\ 3259}# (quote (begin-form)))
                                 ((lambda (#{tmp\ 3268}#)
                                    ((lambda (#{tmp\ 3269}#)
                                       (if #{tmp\ 3269}#
                                         (apply (lambda (#{_\ 3270}#
                                                         #{e1\ 3271}#
                                                         #{e2\ 3272}#)
                                                  (#{chi-sequence\ 2711}#
                                                    (cons #{e1\ 3271}#
                                                          #{e2\ 3272}#)
                                                    #{r\ 3262}#
                                                    #{w\ 3263}#
                                                    #{s\ 3264}#
                                                    #{mod\ 3265}#))
                                                #{tmp\ 3269}#)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           #{tmp\ 3268}#)))
                                     ($sc-dispatch
                                       #{tmp\ 3268}#
                                       '(any any . each-any))))
                                  #{e\ 3261}#)
                                 (if (memv #{type\ 3259}#
                                           '(local-syntax-form))
                                   (#{chi-local-syntax\ 2722}#
                                     #{value\ 3260}#
                                     #{e\ 3261}#
                                     #{r\ 3262}#
                                     #{w\ 3263}#
                                     #{s\ 3264}#
                                     #{mod\ 3265}#
                                     #{chi-sequence\ 2711}#)
                                   (if (memv #{type\ 3259}#
                                             '(eval-when-form))
                                     ((lambda (#{tmp\ 3274}#)
                                        ((lambda (#{tmp\ 3275}#)
                                           (if #{tmp\ 3275}#
                                             (apply (lambda (#{_\ 3276}#
                                                             #{x\ 3277}#
                                                             #{e1\ 3278}#
                                                             #{e2\ 3279}#)
                                                      (let ((#{when-list\ 3280}#
                                                              (#{chi-when-list\ 2714}#
                                                                #{e\ 3261}#
                                                                #{x\ 3277}#
                                                                #{w\ 3263}#)))
                                                        (if (memq 'eval
                                                                  #{when-list\ 3280}#)
                                                          (#{chi-sequence\ 2711}#
                                                            (cons #{e1\ 3278}#
                                                                  #{e2\ 3279}#)
                                                            #{r\ 3262}#
                                                            #{w\ 3263}#
                                                            #{s\ 3264}#
                                                            #{mod\ 3265}#)
                                                          (#{chi-void\ 2724}#))))
                                                    #{tmp\ 3275}#)
                                             (syntax-violation
                                               #f
                                               "source expression failed to match any pattern"
                                               #{tmp\ 3274}#)))
                                         ($sc-dispatch
                                           #{tmp\ 3274}#
                                           '(any each-any any . each-any))))
                                      #{e\ 3261}#)
                                     (if (memv #{type\ 3259}#
                                               '(define-form
                                                  define-syntax-form))
                                       (syntax-violation
                                         #f
                                         "definition in expression context"
                                         #{e\ 3261}#
                                         (#{wrap\ 2709}#
                                           #{value\ 3260}#
                                           #{w\ 3263}#
                                           #{mod\ 3265}#))
                                       (if (memv #{type\ 3259}#
                                                 '(syntax))
                                         (syntax-violation
                                           #f
                                           "reference to pattern variable outside syntax form"
                                           (#{source-wrap\ 2710}#
                                             #{e\ 3261}#
                                             #{w\ 3263}#
                                             #{s\ 3264}#
                                             #{mod\ 3265}#))
                                         (if (memv #{type\ 3259}#
                                                   '(displaced-lexical))
                                           (syntax-violation
                                             #f
                                             "reference to identifier outside its scope"
                                             (#{source-wrap\ 2710}#
                                               #{e\ 3261}#
                                               #{w\ 3263}#
                                               #{s\ 3264}#
                                               #{mod\ 3265}#))
                                           (syntax-violation
                                             #f
                                             "unexpected syntax"
                                             (#{source-wrap\ 2710}#
                                               #{e\ 3261}#
                                               #{w\ 3263}#
                                               #{s\ 3264}#
                                               #{mod\ 3265}#))))))))))))))))))
           (#{chi\ 2717}#
             (lambda (#{e\ 3283}#
                      #{r\ 3284}#
                      #{w\ 3285}#
                      #{mod\ 3286}#)
               (call-with-values
                 (lambda ()
                   (#{syntax-type\ 2715}#
                     #{e\ 3283}#
                     #{r\ 3284}#
                     #{w\ 3285}#
                     (#{source-annotation\ 2672}# #{e\ 3283}#)
                     #f
                     #{mod\ 3286}#
                     #f))
                 (lambda (#{type\ 3287}#
                          #{value\ 3288}#
                          #{e\ 3289}#
                          #{w\ 3290}#
                          #{s\ 3291}#
                          #{mod\ 3292}#)
                   (#{chi-expr\ 2718}#
                     #{type\ 3287}#
                     #{value\ 3288}#
                     #{e\ 3289}#
                     #{r\ 3284}#
                     #{w\ 3290}#
                     #{s\ 3291}#
                     #{mod\ 3292}#)))))
           (#{chi-top\ 2716}#
             (lambda (#{e\ 3293}#
                      #{r\ 3294}#
                      #{w\ 3295}#
                      #{m\ 3296}#
                      #{esew\ 3297}#
                      #{mod\ 3298}#)
               (call-with-values
                 (lambda ()
                   (#{syntax-type\ 2715}#
                     #{e\ 3293}#
                     #{r\ 3294}#
                     #{w\ 3295}#
                     (#{source-annotation\ 2672}# #{e\ 3293}#)
                     #f
                     #{mod\ 3298}#
                     #f))
                 (lambda (#{type\ 3306}#
                          #{value\ 3307}#
                          #{e\ 3308}#
                          #{w\ 3309}#
                          #{s\ 3310}#
                          #{mod\ 3311}#)
                   (if (memv #{type\ 3306}# (quote (begin-form)))
                     ((lambda (#{tmp\ 3312}#)
                        ((lambda (#{tmp\ 3313}#)
                           (if #{tmp\ 3313}#
                             (apply (lambda (#{_\ 3314}#) (#{chi-void\ 2724}#))
                                    #{tmp\ 3313}#)
                             ((lambda (#{tmp\ 3315}#)
                                (if #{tmp\ 3315}#
                                  (apply (lambda (#{_\ 3316}#
                                                  #{e1\ 3317}#
                                                  #{e2\ 3318}#)
                                           (#{chi-top-sequence\ 2712}#
                                             (cons #{e1\ 3317}# #{e2\ 3318}#)
                                             #{r\ 3294}#
                                             #{w\ 3309}#
                                             #{s\ 3310}#
                                             #{m\ 3296}#
                                             #{esew\ 3297}#
                                             #{mod\ 3311}#))
                                         #{tmp\ 3315}#)
                                  (syntax-violation
                                    #f
                                    "source expression failed to match any pattern"
                                    #{tmp\ 3312}#)))
                              ($sc-dispatch
                                #{tmp\ 3312}#
                                '(any any . each-any)))))
                         ($sc-dispatch #{tmp\ 3312}# (quote (any)))))
                      #{e\ 3308}#)
                     (if (memv #{type\ 3306}# (quote (local-syntax-form)))
                       (#{chi-local-syntax\ 2722}#
                         #{value\ 3307}#
                         #{e\ 3308}#
                         #{r\ 3294}#
                         #{w\ 3309}#
                         #{s\ 3310}#
                         #{mod\ 3311}#
                         (lambda (#{body\ 3320}#
                                  #{r\ 3321}#
                                  #{w\ 3322}#
                                  #{s\ 3323}#
                                  #{mod\ 3324}#)
                           (#{chi-top-sequence\ 2712}#
                             #{body\ 3320}#
                             #{r\ 3321}#
                             #{w\ 3322}#
                             #{s\ 3323}#
                             #{m\ 3296}#
                             #{esew\ 3297}#
                             #{mod\ 3324}#)))
                       (if (memv #{type\ 3306}# (quote (eval-when-form)))
                         ((lambda (#{tmp\ 3325}#)
                            ((lambda (#{tmp\ 3326}#)
                               (if #{tmp\ 3326}#
                                 (apply (lambda (#{_\ 3327}#
                                                 #{x\ 3328}#
                                                 #{e1\ 3329}#
                                                 #{e2\ 3330}#)
                                          (let ((#{when-list\ 3331}#
                                                  (#{chi-when-list\ 2714}#
                                                    #{e\ 3308}#
                                                    #{x\ 3328}#
                                                    #{w\ 3309}#))
                                                (#{body\ 3332}#
                                                  (cons #{e1\ 3329}#
                                                        #{e2\ 3330}#)))
                                            (if (eq? #{m\ 3296}# (quote e))
                                              (if (memq 'eval
                                                        #{when-list\ 3331}#)
                                                (#{chi-top-sequence\ 2712}#
                                                  #{body\ 3332}#
                                                  #{r\ 3294}#
                                                  #{w\ 3309}#
                                                  #{s\ 3310}#
                                                  'e
                                                  '(eval)
                                                  #{mod\ 3311}#)
                                                (#{chi-void\ 2724}#))
                                              (if (memq 'load
                                                        #{when-list\ 3331}#)
                                                (if (let ((#{t\ 3335}#
                                                            (memq 'compile
                                                                  #{when-list\ 3331}#)))
                                                      (if #{t\ 3335}#
                                                        #{t\ 3335}#
                                                        (if (eq? #{m\ 3296}#
                                                                 'c&e)
                                                          (memq 'eval
                                                                #{when-list\ 3331}#)
                                                          #f)))
                                                  (#{chi-top-sequence\ 2712}#
                                                    #{body\ 3332}#
                                                    #{r\ 3294}#
                                                    #{w\ 3309}#
                                                    #{s\ 3310}#
                                                    'c&e
                                                    '(compile load)
                                                    #{mod\ 3311}#)
                                                  (if (memq #{m\ 3296}#
                                                            '(c c&e))
                                                    (#{chi-top-sequence\ 2712}#
                                                      #{body\ 3332}#
                                                      #{r\ 3294}#
                                                      #{w\ 3309}#
                                                      #{s\ 3310}#
                                                      'c
                                                      '(load)
                                                      #{mod\ 3311}#)
                                                    (#{chi-void\ 2724}#)))
                                                (if (let ((#{t\ 3336}#
                                                            (memq 'compile
                                                                  #{when-list\ 3331}#)))
                                                      (if #{t\ 3336}#
                                                        #{t\ 3336}#
                                                        (if (eq? #{m\ 3296}#
                                                                 'c&e)
                                                          (memq 'eval
                                                                #{when-list\ 3331}#)
                                                          #f)))
                                                  (begin
                                                    (#{top-level-eval-hook\ 2640}#
                                                      (#{chi-top-sequence\ 2712}#
                                                        #{body\ 3332}#
                                                        #{r\ 3294}#
                                                        #{w\ 3309}#
                                                        #{s\ 3310}#
                                                        'e
                                                        '(eval)
                                                        #{mod\ 3311}#)
                                                      #{mod\ 3311}#)
                                                    (#{chi-void\ 2724}#))
                                                  (#{chi-void\ 2724}#))))))
                                        #{tmp\ 3326}#)
                                 (syntax-violation
                                   #f
                                   "source expression failed to match any pattern"
                                   #{tmp\ 3325}#)))
                             ($sc-dispatch
                               #{tmp\ 3325}#
                               '(any each-any any . each-any))))
                          #{e\ 3308}#)
                         (if (memv #{type\ 3306}#
                                   '(define-syntax-form))
                           (let ((#{n\ 3337}#
                                   (#{id-var-name\ 2703}#
                                     #{value\ 3307}#
                                     #{w\ 3309}#))
                                 (#{r\ 3338}#
                                   (#{macros-only-env\ 2677}# #{r\ 3294}#)))
                             (if (memv #{m\ 3296}# (quote (c)))
                               (if (memq (quote compile) #{esew\ 3297}#)
                                 (let ((#{e\ 3339}#
                                         (#{chi-install-global\ 2713}#
                                           #{n\ 3337}#
                                           (#{chi\ 2717}#
                                             #{e\ 3308}#
                                             #{r\ 3338}#
                                             #{w\ 3309}#
                                             #{mod\ 3311}#))))
                                   (begin
                                     (#{top-level-eval-hook\ 2640}#
                                       #{e\ 3339}#
                                       #{mod\ 3311}#)
                                     (if (memq (quote load) #{esew\ 3297}#)
                                       #{e\ 3339}#
                                       (#{chi-void\ 2724}#))))
                                 (if (memq (quote load) #{esew\ 3297}#)
                                   (#{chi-install-global\ 2713}#
                                     #{n\ 3337}#
                                     (#{chi\ 2717}#
                                       #{e\ 3308}#
                                       #{r\ 3338}#
                                       #{w\ 3309}#
                                       #{mod\ 3311}#))
                                   (#{chi-void\ 2724}#)))
                               (if (memv #{m\ 3296}# (quote (c&e)))
                                 (let ((#{e\ 3340}#
                                         (#{chi-install-global\ 2713}#
                                           #{n\ 3337}#
                                           (#{chi\ 2717}#
                                             #{e\ 3308}#
                                             #{r\ 3338}#
                                             #{w\ 3309}#
                                             #{mod\ 3311}#))))
                                   (begin
                                     (#{top-level-eval-hook\ 2640}#
                                       #{e\ 3340}#
                                       #{mod\ 3311}#)
                                     #{e\ 3340}#))
                                 (begin
                                   (if (memq (quote eval) #{esew\ 3297}#)
                                     (#{top-level-eval-hook\ 2640}#
                                       (#{chi-install-global\ 2713}#
                                         #{n\ 3337}#
                                         (#{chi\ 2717}#
                                           #{e\ 3308}#
                                           #{r\ 3338}#
                                           #{w\ 3309}#
                                           #{mod\ 3311}#))
                                       #{mod\ 3311}#))
                                   (#{chi-void\ 2724}#)))))
                           (if (memv #{type\ 3306}# (quote (define-form)))
                             (let ((#{n\ 3341}#
                                     (#{id-var-name\ 2703}#
                                       #{value\ 3307}#
                                       #{w\ 3309}#)))
                               (let ((#{type\ 3342}#
                                       (#{binding-type\ 2673}#
                                         (#{lookup\ 2678}#
                                           #{n\ 3341}#
                                           #{r\ 3294}#
                                           #{mod\ 3311}#))))
                                 (if (memv #{type\ 3342}#
                                           '(global core macro module-ref))
                                   (begin
                                     (if (if (not (module-local-variable
                                                    (current-module)
                                                    #{n\ 3341}#))
                                           (current-module)
                                           #f)
                                       (let ((#{old\ 3343}#
                                               (module-variable
                                                 (current-module)
                                                 #{n\ 3341}#)))
                                         (module-define!
                                           (current-module)
                                           #{n\ 3341}#
                                           (if (variable? #{old\ 3343}#)
                                             (variable-ref #{old\ 3343}#)
                                             #f))))
                                     (let ((#{x\ 3344}#
                                             (#{build-global-definition\ 2654}#
                                               #{s\ 3310}#
                                               #{n\ 3341}#
                                               (#{chi\ 2717}#
                                                 #{e\ 3308}#
                                                 #{r\ 3294}#
                                                 #{w\ 3309}#
                                                 #{mod\ 3311}#))))
                                       (begin
                                         (if (eq? #{m\ 3296}# (quote c&e))
                                           (#{top-level-eval-hook\ 2640}#
                                             #{x\ 3344}#
                                             #{mod\ 3311}#))
                                         #{x\ 3344}#)))
                                   (if (memv #{type\ 3342}#
                                             '(displaced-lexical))
                                     (syntax-violation
                                       #f
                                       "identifier out of context"
                                       #{e\ 3308}#
                                       (#{wrap\ 2709}#
                                         #{value\ 3307}#
                                         #{w\ 3309}#
                                         #{mod\ 3311}#))
                                     (syntax-violation
                                       #f
                                       "cannot define keyword at top level"
                                       #{e\ 3308}#
                                       (#{wrap\ 2709}#
                                         #{value\ 3307}#
                                         #{w\ 3309}#
                                         #{mod\ 3311}#))))))
                             (let ((#{x\ 3345}#
                                     (#{chi-expr\ 2718}#
                                       #{type\ 3306}#
                                       #{value\ 3307}#
                                       #{e\ 3308}#
                                       #{r\ 3294}#
                                       #{w\ 3309}#
                                       #{s\ 3310}#
                                       #{mod\ 3311}#)))
                               (begin
                                 (if (eq? #{m\ 3296}# (quote c&e))
                                   (#{top-level-eval-hook\ 2640}#
                                     #{x\ 3345}#
                                     #{mod\ 3311}#))
                                 #{x\ 3345}#)))))))))))
           (#{syntax-type\ 2715}#
             (lambda (#{e\ 3346}#
                      #{r\ 3347}#
                      #{w\ 3348}#
                      #{s\ 3349}#
                      #{rib\ 3350}#
                      #{mod\ 3351}#
                      #{for-car?\ 3352}#)
               (if (symbol? #{e\ 3346}#)
                 (let ((#{n\ 3353}#
                         (#{id-var-name\ 2703}# #{e\ 3346}# #{w\ 3348}#)))
                   (let ((#{b\ 3354}#
                           (#{lookup\ 2678}#
                             #{n\ 3353}#
                             #{r\ 3347}#
                             #{mod\ 3351}#)))
                     (let ((#{type\ 3355}#
                             (#{binding-type\ 2673}# #{b\ 3354}#)))
                       (if (memv #{type\ 3355}# (quote (lexical)))
                         (values
                           #{type\ 3355}#
                           (#{binding-value\ 2674}# #{b\ 3354}#)
                           #{e\ 3346}#
                           #{w\ 3348}#
                           #{s\ 3349}#
                           #{mod\ 3351}#)
                         (if (memv #{type\ 3355}# (quote (global)))
                           (values
                             #{type\ 3355}#
                             #{n\ 3353}#
                             #{e\ 3346}#
                             #{w\ 3348}#
                             #{s\ 3349}#
                             #{mod\ 3351}#)
                           (if (memv #{type\ 3355}# (quote (macro)))
                             (if #{for-car?\ 3352}#
                               (values
                                 #{type\ 3355}#
                                 (#{binding-value\ 2674}# #{b\ 3354}#)
                                 #{e\ 3346}#
                                 #{w\ 3348}#
                                 #{s\ 3349}#
                                 #{mod\ 3351}#)
                               (#{syntax-type\ 2715}#
                                 (#{chi-macro\ 2720}#
                                   (#{binding-value\ 2674}# #{b\ 3354}#)
                                   #{e\ 3346}#
                                   #{r\ 3347}#
                                   #{w\ 3348}#
                                   #{rib\ 3350}#
                                   #{mod\ 3351}#)
                                 #{r\ 3347}#
                                 '(())
                                 #{s\ 3349}#
                                 #{rib\ 3350}#
                                 #{mod\ 3351}#
                                 #f))
                             (values
                               #{type\ 3355}#
                               (#{binding-value\ 2674}# #{b\ 3354}#)
                               #{e\ 3346}#
                               #{w\ 3348}#
                               #{s\ 3349}#
                               #{mod\ 3351}#)))))))
                 (if (pair? #{e\ 3346}#)
                   (let ((#{first\ 3356}# (car #{e\ 3346}#)))
                     (call-with-values
                       (lambda ()
                         (#{syntax-type\ 2715}#
                           #{first\ 3356}#
                           #{r\ 3347}#
                           #{w\ 3348}#
                           #{s\ 3349}#
                           #{rib\ 3350}#
                           #{mod\ 3351}#
                           #t))
                       (lambda (#{ftype\ 3357}#
                                #{fval\ 3358}#
                                #{fe\ 3359}#
                                #{fw\ 3360}#
                                #{fs\ 3361}#
                                #{fmod\ 3362}#)
                         (if (memv #{ftype\ 3357}# (quote (lexical)))
                           (values
                             'lexical-call
                             #{fval\ 3358}#
                             #{e\ 3346}#
                             #{w\ 3348}#
                             #{s\ 3349}#
                             #{mod\ 3351}#)
                           (if (memv #{ftype\ 3357}# (quote (global)))
                             (values
                               'global-call
                               (#{make-syntax-object\ 2664}#
                                 #{fval\ 3358}#
                                 #{w\ 3348}#
                                 #{fmod\ 3362}#)
                               #{e\ 3346}#
                               #{w\ 3348}#
                               #{s\ 3349}#
                               #{mod\ 3351}#)
                             (if (memv #{ftype\ 3357}# (quote (macro)))
                               (#{syntax-type\ 2715}#
                                 (#{chi-macro\ 2720}#
                                   #{fval\ 3358}#
                                   #{e\ 3346}#
                                   #{r\ 3347}#
                                   #{w\ 3348}#
                                   #{rib\ 3350}#
                                   #{mod\ 3351}#)
                                 #{r\ 3347}#
                                 '(())
                                 #{s\ 3349}#
                                 #{rib\ 3350}#
                                 #{mod\ 3351}#
                                 #{for-car?\ 3352}#)
                               (if (memv #{ftype\ 3357}# (quote (module-ref)))
                                 (call-with-values
                                   (lambda () (#{fval\ 3358}# #{e\ 3346}#))
                                   (lambda (#{sym\ 3363}# #{mod\ 3364}#)
                                     (#{syntax-type\ 2715}#
                                       #{sym\ 3363}#
                                       #{r\ 3347}#
                                       #{w\ 3348}#
                                       #{s\ 3349}#
                                       #{rib\ 3350}#
                                       #{mod\ 3364}#
                                       #{for-car?\ 3352}#)))
                                 (if (memv #{ftype\ 3357}# (quote (core)))
                                   (values
                                     'core-form
                                     #{fval\ 3358}#
                                     #{e\ 3346}#
                                     #{w\ 3348}#
                                     #{s\ 3349}#
                                     #{mod\ 3351}#)
                                   (if (memv #{ftype\ 3357}#
                                             '(local-syntax))
                                     (values
                                       'local-syntax-form
                                       #{fval\ 3358}#
                                       #{e\ 3346}#
                                       #{w\ 3348}#
                                       #{s\ 3349}#
                                       #{mod\ 3351}#)
                                     (if (memv #{ftype\ 3357}# (quote (begin)))
                                       (values
                                         'begin-form
                                         #f
                                         #{e\ 3346}#
                                         #{w\ 3348}#
                                         #{s\ 3349}#
                                         #{mod\ 3351}#)
                                       (if (memv #{ftype\ 3357}#
                                                 '(eval-when))
                                         (values
                                           'eval-when-form
                                           #f
                                           #{e\ 3346}#
                                           #{w\ 3348}#
                                           #{s\ 3349}#
                                           #{mod\ 3351}#)
                                         (if (memv #{ftype\ 3357}#
                                                   '(define))
                                           ((lambda (#{tmp\ 3365}#)
                                              ((lambda (#{tmp\ 3366}#)
                                                 (if (if #{tmp\ 3366}#
                                                       (apply (lambda (#{_\ 3367}#
                                                                       #{name\ 3368}#
                                                                       #{val\ 3369}#)
                                                                (#{id?\ 2681}#
                                                                  #{name\ 3368}#))
                                                              #{tmp\ 3366}#)
                                                       #f)
                                                   (apply (lambda (#{_\ 3370}#
                                                                   #{name\ 3371}#
                                                                   #{val\ 3372}#)
                                                            (values
                                                              'define-form
                                                              #{name\ 3371}#
                                                              #{val\ 3372}#
                                                              #{w\ 3348}#
                                                              #{s\ 3349}#
                                                              #{mod\ 3351}#))
                                                          #{tmp\ 3366}#)
                                                   ((lambda (#{tmp\ 3373}#)
                                                      (if (if #{tmp\ 3373}#
                                                            (apply (lambda (#{_\ 3374}#
                                                                            #{name\ 3375}#
                                                                            #{args\ 3376}#
                                                                            #{e1\ 3377}#
                                                                            #{e2\ 3378}#)
                                                                     (if (#{id?\ 2681}#
                                                                           #{name\ 3375}#)
                                                                       (#{valid-bound-ids?\ 2706}#
                                                                         (#{lambda-var-list\ 2732}#
                                                                           #{args\ 3376}#))
                                                                       #f))
                                                                   #{tmp\ 3373}#)
                                                            #f)
                                                        (apply (lambda (#{_\ 3379}#
                                                                        #{name\ 3380}#
                                                                        #{args\ 3381}#
                                                                        #{e1\ 3382}#
                                                                        #{e2\ 3383}#)
                                                                 (values
                                                                   'define-form
                                                                   (#{wrap\ 2709}#
                                                                     #{name\ 3380}#
                                                                     #{w\ 3348}#
                                                                     #{mod\ 3351}#)
                                                                   (#{decorate-source\ 2644}#
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
                                                                                   chi-lambda-case
                                                                                   lambda*-formals
                                                                                   chi-simple-lambda
                                                                                   lambda-formals
                                                                                   ellipsis?
                                                                                   chi-void
                                                                                   eval-local-transformer
                                                                                   chi-local-syntax
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
                                                                                   build-lambda-case
                                                                                   build-case-lambda
                                                                                   build-simple-lambda
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
                                                                           (#{wrap\ 2709}#
                                                                             (cons #{args\ 3381}#
                                                                                   (cons #{e1\ 3382}#
                                                                                         #{e2\ 3383}#))
                                                                             #{w\ 3348}#
                                                                             #{mod\ 3351}#))
                                                                     #{s\ 3349}#)
                                                                   '(())
                                                                   #{s\ 3349}#
                                                                   #{mod\ 3351}#))
                                                               #{tmp\ 3373}#)
                                                        ((lambda (#{tmp\ 3385}#)
                                                           (if (if #{tmp\ 3385}#
                                                                 (apply (lambda (#{_\ 3386}#
                                                                                 #{name\ 3387}#)
                                                                          (#{id?\ 2681}#
                                                                            #{name\ 3387}#))
                                                                        #{tmp\ 3385}#)
                                                                 #f)
                                                             (apply (lambda (#{_\ 3388}#
                                                                             #{name\ 3389}#)
                                                                      (values
                                                                        'define-form
                                                                        (#{wrap\ 2709}#
                                                                          #{name\ 3389}#
                                                                          #{w\ 3348}#
                                                                          #{mod\ 3351}#)
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
                                                                                 chi-lambda-case
                                                                                 lambda*-formals
                                                                                 chi-simple-lambda
                                                                                 lambda-formals
                                                                                 ellipsis?
                                                                                 chi-void
                                                                                 eval-local-transformer
                                                                                 chi-local-syntax
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
                                                                                 build-lambda-case
                                                                                 build-case-lambda
                                                                                 build-simple-lambda
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
                                                                                 chi-lambda-case
                                                                                 lambda*-formals
                                                                                 chi-simple-lambda
                                                                                 lambda-formals
                                                                                 ellipsis?
                                                                                 chi-void
                                                                                 eval-local-transformer
                                                                                 chi-local-syntax
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
                                                                                 build-lambda-case
                                                                                 build-case-lambda
                                                                                 build-simple-lambda
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
                                                                                 chi-lambda-case
                                                                                 lambda*-formals
                                                                                 chi-simple-lambda
                                                                                 lambda-formals
                                                                                 ellipsis?
                                                                                 chi-void
                                                                                 eval-local-transformer
                                                                                 chi-local-syntax
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
                                                                                 build-lambda-case
                                                                                 build-case-lambda
                                                                                 build-simple-lambda
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
                                                                        #{s\ 3349}#
                                                                        #{mod\ 3351}#))
                                                                    #{tmp\ 3385}#)
                                                             (syntax-violation
                                                               #f
                                                               "source expression failed to match any pattern"
                                                               #{tmp\ 3365}#)))
                                                         ($sc-dispatch
                                                           #{tmp\ 3365}#
                                                           '(any any)))))
                                                    ($sc-dispatch
                                                      #{tmp\ 3365}#
                                                      '(any (any . any)
                                                            any
                                                            .
                                                            each-any)))))
                                               ($sc-dispatch
                                                 #{tmp\ 3365}#
                                                 '(any any any))))
                                            #{e\ 3346}#)
                                           (if (memv #{ftype\ 3357}#
                                                     '(define-syntax))
                                             ((lambda (#{tmp\ 3390}#)
                                                ((lambda (#{tmp\ 3391}#)
                                                   (if (if #{tmp\ 3391}#
                                                         (apply (lambda (#{_\ 3392}#
                                                                         #{name\ 3393}#
                                                                         #{val\ 3394}#)
                                                                  (#{id?\ 2681}#
                                                                    #{name\ 3393}#))
                                                                #{tmp\ 3391}#)
                                                         #f)
                                                     (apply (lambda (#{_\ 3395}#
                                                                     #{name\ 3396}#
                                                                     #{val\ 3397}#)
                                                              (values
                                                                'define-syntax-form
                                                                #{name\ 3396}#
                                                                #{val\ 3397}#
                                                                #{w\ 3348}#
                                                                #{s\ 3349}#
                                                                #{mod\ 3351}#))
                                                            #{tmp\ 3391}#)
                                                     (syntax-violation
                                                       #f
                                                       "source expression failed to match any pattern"
                                                       #{tmp\ 3390}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 3390}#
                                                   '(any any any))))
                                              #{e\ 3346}#)
                                             (values
                                               'call
                                               #f
                                               #{e\ 3346}#
                                               #{w\ 3348}#
                                               #{s\ 3349}#
                                               #{mod\ 3351}#))))))))))))))
                   (if (#{syntax-object?\ 2665}# #{e\ 3346}#)
                     (#{syntax-type\ 2715}#
                       (#{syntax-object-expression\ 2666}# #{e\ 3346}#)
                       #{r\ 3347}#
                       (#{join-wraps\ 2700}#
                         #{w\ 3348}#
                         (#{syntax-object-wrap\ 2667}# #{e\ 3346}#))
                       #{s\ 3349}#
                       #{rib\ 3350}#
                       (let ((#{t\ 3398}#
                               (#{syntax-object-module\ 2668}# #{e\ 3346}#)))
                         (if #{t\ 3398}# #{t\ 3398}# #{mod\ 3351}#))
                       #{for-car?\ 3352}#)
                     (if (self-evaluating? #{e\ 3346}#)
                       (values
                         'constant
                         #f
                         #{e\ 3346}#
                         #{w\ 3348}#
                         #{s\ 3349}#
                         #{mod\ 3351}#)
                       (values
                         'other
                         #f
                         #{e\ 3346}#
                         #{w\ 3348}#
                         #{s\ 3349}#
                         #{mod\ 3351}#)))))))
           (#{chi-when-list\ 2714}#
             (lambda (#{e\ 3399}# #{when-list\ 3400}# #{w\ 3401}#)
               (letrec ((#{f\ 3402}#
                          (lambda (#{when-list\ 3403}# #{situations\ 3404}#)
                            (if (null? #{when-list\ 3403}#)
                              #{situations\ 3404}#
                              (#{f\ 3402}#
                                (cdr #{when-list\ 3403}#)
                                (cons (let ((#{x\ 3405}#
                                              (car #{when-list\ 3403}#)))
                                        (if (#{free-id=?\ 2704}#
                                              #{x\ 3405}#
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
                                                      chi-lambda-case
                                                      lambda*-formals
                                                      chi-simple-lambda
                                                      lambda-formals
                                                      ellipsis?
                                                      chi-void
                                                      eval-local-transformer
                                                      chi-local-syntax
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
                                                      build-lambda-case
                                                      build-case-lambda
                                                      build-simple-lambda
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
                                          'compile
                                          (if (#{free-id=?\ 2704}#
                                                #{x\ 3405}#
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
                                                        chi-lambda-case
                                                        lambda*-formals
                                                        chi-simple-lambda
                                                        lambda-formals
                                                        ellipsis?
                                                        chi-void
                                                        eval-local-transformer
                                                        chi-local-syntax
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
                                                        build-lambda-case
                                                        build-case-lambda
                                                        build-simple-lambda
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
                                            (if (#{free-id=?\ 2704}#
                                                  #{x\ 3405}#
                                                  '#(syntax-object
                                                     eval
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
                                                          chi-lambda-case
                                                          lambda*-formals
                                                          chi-simple-lambda
                                                          lambda-formals
                                                          ellipsis?
                                                          chi-void
                                                          eval-local-transformer
                                                          chi-local-syntax
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
                                                          build-lambda-case
                                                          build-case-lambda
                                                          build-simple-lambda
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
                                                #{e\ 3399}#
                                                (#{wrap\ 2709}#
                                                  #{x\ 3405}#
                                                  #{w\ 3401}#
                                                  #f))))))
                                      #{situations\ 3404}#))))))
                 (#{f\ 3402}# #{when-list\ 3400}# (quote ())))))
           (#{chi-install-global\ 2713}#
             (lambda (#{name\ 3406}# #{e\ 3407}#)
               (#{build-global-definition\ 2654}#
                 #f
                 #{name\ 3406}#
                 (if (let ((#{v\ 3408}#
                             (module-variable
                               (current-module)
                               #{name\ 3406}#)))
                       (if #{v\ 3408}#
                         (if (variable-bound? #{v\ 3408}#)
                           (if (macro? (variable-ref #{v\ 3408}#))
                             (not (eq? (macro-type (variable-ref #{v\ 3408}#))
                                       'syncase-macro))
                             #f)
                           #f)
                         #f))
                   (#{build-application\ 2646}#
                     #f
                     (#{build-primref\ 2658}#
                       #f
                       'make-extended-syncase-macro)
                     (list (#{build-application\ 2646}#
                             #f
                             (#{build-primref\ 2658}# #f (quote module-ref))
                             (list (#{build-application\ 2646}#
                                     #f
                                     (#{build-primref\ 2658}#
                                       #f
                                       'current-module)
                                     '())
                                   (#{build-data\ 2659}# #f #{name\ 3406}#)))
                           (#{build-data\ 2659}# #f (quote macro))
                           #{e\ 3407}#))
                   (#{build-application\ 2646}#
                     #f
                     (#{build-primref\ 2658}#
                       #f
                       'make-syncase-macro)
                     (list (#{build-data\ 2659}# #f (quote macro))
                           #{e\ 3407}#))))))
           (#{chi-top-sequence\ 2712}#
             (lambda (#{body\ 3409}#
                      #{r\ 3410}#
                      #{w\ 3411}#
                      #{s\ 3412}#
                      #{m\ 3413}#
                      #{esew\ 3414}#
                      #{mod\ 3415}#)
               (#{build-sequence\ 2660}#
                 #{s\ 3412}#
                 (letrec ((#{dobody\ 3416}#
                            (lambda (#{body\ 3417}#
                                     #{r\ 3418}#
                                     #{w\ 3419}#
                                     #{m\ 3420}#
                                     #{esew\ 3421}#
                                     #{mod\ 3422}#)
                              (if (null? #{body\ 3417}#)
                                '()
                                (let ((#{first\ 3423}#
                                        (#{chi-top\ 2716}#
                                          (car #{body\ 3417}#)
                                          #{r\ 3418}#
                                          #{w\ 3419}#
                                          #{m\ 3420}#
                                          #{esew\ 3421}#
                                          #{mod\ 3422}#)))
                                  (cons #{first\ 3423}#
                                        (#{dobody\ 3416}#
                                          (cdr #{body\ 3417}#)
                                          #{r\ 3418}#
                                          #{w\ 3419}#
                                          #{m\ 3420}#
                                          #{esew\ 3421}#
                                          #{mod\ 3422}#)))))))
                   (#{dobody\ 3416}#
                     #{body\ 3409}#
                     #{r\ 3410}#
                     #{w\ 3411}#
                     #{m\ 3413}#
                     #{esew\ 3414}#
                     #{mod\ 3415}#)))))
           (#{chi-sequence\ 2711}#
             (lambda (#{body\ 3424}#
                      #{r\ 3425}#
                      #{w\ 3426}#
                      #{s\ 3427}#
                      #{mod\ 3428}#)
               (#{build-sequence\ 2660}#
                 #{s\ 3427}#
                 (letrec ((#{dobody\ 3429}#
                            (lambda (#{body\ 3430}#
                                     #{r\ 3431}#
                                     #{w\ 3432}#
                                     #{mod\ 3433}#)
                              (if (null? #{body\ 3430}#)
                                '()
                                (let ((#{first\ 3434}#
                                        (#{chi\ 2717}#
                                          (car #{body\ 3430}#)
                                          #{r\ 3431}#
                                          #{w\ 3432}#
                                          #{mod\ 3433}#)))
                                  (cons #{first\ 3434}#
                                        (#{dobody\ 3429}#
                                          (cdr #{body\ 3430}#)
                                          #{r\ 3431}#
                                          #{w\ 3432}#
                                          #{mod\ 3433}#)))))))
                   (#{dobody\ 3429}#
                     #{body\ 3424}#
                     #{r\ 3425}#
                     #{w\ 3426}#
                     #{mod\ 3428}#)))))
           (#{source-wrap\ 2710}#
             (lambda (#{x\ 3435}#
                      #{w\ 3436}#
                      #{s\ 3437}#
                      #{defmod\ 3438}#)
               (#{wrap\ 2709}#
                 (#{decorate-source\ 2644}#
                   #{x\ 3435}#
                   #{s\ 3437}#)
                 #{w\ 3436}#
                 #{defmod\ 3438}#)))
           (#{wrap\ 2709}#
             (lambda (#{x\ 3439}# #{w\ 3440}# #{defmod\ 3441}#)
               (if (if (null? (#{wrap-marks\ 2684}# #{w\ 3440}#))
                     (null? (#{wrap-subst\ 2685}# #{w\ 3440}#))
                     #f)
                 #{x\ 3439}#
                 (if (#{syntax-object?\ 2665}# #{x\ 3439}#)
                   (#{make-syntax-object\ 2664}#
                     (#{syntax-object-expression\ 2666}# #{x\ 3439}#)
                     (#{join-wraps\ 2700}#
                       #{w\ 3440}#
                       (#{syntax-object-wrap\ 2667}# #{x\ 3439}#))
                     (#{syntax-object-module\ 2668}# #{x\ 3439}#))
                   (if (null? #{x\ 3439}#)
                     #{x\ 3439}#
                     (#{make-syntax-object\ 2664}#
                       #{x\ 3439}#
                       #{w\ 3440}#
                       #{defmod\ 3441}#))))))
           (#{bound-id-member?\ 2708}#
             (lambda (#{x\ 3442}# #{list\ 3443}#)
               (if (not (null? #{list\ 3443}#))
                 (let ((#{t\ 3444}#
                         (#{bound-id=?\ 2705}#
                           #{x\ 3442}#
                           (car #{list\ 3443}#))))
                   (if #{t\ 3444}#
                     #{t\ 3444}#
                     (#{bound-id-member?\ 2708}#
                       #{x\ 3442}#
                       (cdr #{list\ 3443}#))))
                 #f)))
           (#{distinct-bound-ids?\ 2707}#
             (lambda (#{ids\ 3445}#)
               (letrec ((#{distinct?\ 3446}#
                          (lambda (#{ids\ 3447}#)
                            (let ((#{t\ 3448}# (null? #{ids\ 3447}#)))
                              (if #{t\ 3448}#
                                #{t\ 3448}#
                                (if (not (#{bound-id-member?\ 2708}#
                                           (car #{ids\ 3447}#)
                                           (cdr #{ids\ 3447}#)))
                                  (#{distinct?\ 3446}# (cdr #{ids\ 3447}#))
                                  #f))))))
                 (#{distinct?\ 3446}# #{ids\ 3445}#))))
           (#{valid-bound-ids?\ 2706}#
             (lambda (#{ids\ 3449}#)
               (if (letrec ((#{all-ids?\ 3450}#
                              (lambda (#{ids\ 3451}#)
                                (let ((#{t\ 3452}# (null? #{ids\ 3451}#)))
                                  (if #{t\ 3452}#
                                    #{t\ 3452}#
                                    (if (#{id?\ 2681}# (car #{ids\ 3451}#))
                                      (#{all-ids?\ 3450}# (cdr #{ids\ 3451}#))
                                      #f))))))
                     (#{all-ids?\ 3450}# #{ids\ 3449}#))
                 (#{distinct-bound-ids?\ 2707}# #{ids\ 3449}#)
                 #f)))
           (#{bound-id=?\ 2705}#
             (lambda (#{i\ 3453}# #{j\ 3454}#)
               (if (if (#{syntax-object?\ 2665}# #{i\ 3453}#)
                     (#{syntax-object?\ 2665}# #{j\ 3454}#)
                     #f)
                 (if (eq? (#{syntax-object-expression\ 2666}# #{i\ 3453}#)
                          (#{syntax-object-expression\ 2666}# #{j\ 3454}#))
                   (#{same-marks?\ 2702}#
                     (#{wrap-marks\ 2684}#
                       (#{syntax-object-wrap\ 2667}# #{i\ 3453}#))
                     (#{wrap-marks\ 2684}#
                       (#{syntax-object-wrap\ 2667}# #{j\ 3454}#)))
                   #f)
                 (eq? #{i\ 3453}# #{j\ 3454}#))))
           (#{free-id=?\ 2704}#
             (lambda (#{i\ 3455}# #{j\ 3456}#)
               (if (eq? (let ((#{x\ 3457}# #{i\ 3455}#))
                          (if (#{syntax-object?\ 2665}# #{x\ 3457}#)
                            (#{syntax-object-expression\ 2666}# #{x\ 3457}#)
                            #{x\ 3457}#))
                        (let ((#{x\ 3458}# #{j\ 3456}#))
                          (if (#{syntax-object?\ 2665}# #{x\ 3458}#)
                            (#{syntax-object-expression\ 2666}# #{x\ 3458}#)
                            #{x\ 3458}#)))
                 (eq? (#{id-var-name\ 2703}# #{i\ 3455}# (quote (())))
                      (#{id-var-name\ 2703}# #{j\ 3456}# (quote (()))))
                 #f)))
           (#{id-var-name\ 2703}#
             (lambda (#{id\ 3459}# #{w\ 3460}#)
               (letrec ((#{search-vector-rib\ 3463}#
                          (lambda (#{sym\ 3469}#
                                   #{subst\ 3470}#
                                   #{marks\ 3471}#
                                   #{symnames\ 3472}#
                                   #{ribcage\ 3473}#)
                            (let ((#{n\ 3474}#
                                    (vector-length #{symnames\ 3472}#)))
                              (letrec ((#{f\ 3475}#
                                         (lambda (#{i\ 3476}#)
                                           (if (#{fx=\ 2638}#
                                                 #{i\ 3476}#
                                                 #{n\ 3474}#)
                                             (#{search\ 3461}#
                                               #{sym\ 3469}#
                                               (cdr #{subst\ 3470}#)
                                               #{marks\ 3471}#)
                                             (if (if (eq? (vector-ref
                                                            #{symnames\ 3472}#
                                                            #{i\ 3476}#)
                                                          #{sym\ 3469}#)
                                                   (#{same-marks?\ 2702}#
                                                     #{marks\ 3471}#
                                                     (vector-ref
                                                       (#{ribcage-marks\ 2691}#
                                                         #{ribcage\ 3473}#)
                                                       #{i\ 3476}#))
                                                   #f)
                                               (values
                                                 (vector-ref
                                                   (#{ribcage-labels\ 2692}#
                                                     #{ribcage\ 3473}#)
                                                   #{i\ 3476}#)
                                                 #{marks\ 3471}#)
                                               (#{f\ 3475}#
                                                 (#{fx+\ 2636}#
                                                   #{i\ 3476}#
                                                   1)))))))
                                (#{f\ 3475}# 0)))))
                        (#{search-list-rib\ 3462}#
                          (lambda (#{sym\ 3477}#
                                   #{subst\ 3478}#
                                   #{marks\ 3479}#
                                   #{symnames\ 3480}#
                                   #{ribcage\ 3481}#)
                            (letrec ((#{f\ 3482}#
                                       (lambda (#{symnames\ 3483}# #{i\ 3484}#)
                                         (if (null? #{symnames\ 3483}#)
                                           (#{search\ 3461}#
                                             #{sym\ 3477}#
                                             (cdr #{subst\ 3478}#)
                                             #{marks\ 3479}#)
                                           (if (if (eq? (car #{symnames\ 3483}#)
                                                        #{sym\ 3477}#)
                                                 (#{same-marks?\ 2702}#
                                                   #{marks\ 3479}#
                                                   (list-ref
                                                     (#{ribcage-marks\ 2691}#
                                                       #{ribcage\ 3481}#)
                                                     #{i\ 3484}#))
                                                 #f)
                                             (values
                                               (list-ref
                                                 (#{ribcage-labels\ 2692}#
                                                   #{ribcage\ 3481}#)
                                                 #{i\ 3484}#)
                                               #{marks\ 3479}#)
                                             (#{f\ 3482}#
                                               (cdr #{symnames\ 3483}#)
                                               (#{fx+\ 2636}#
                                                 #{i\ 3484}#
                                                 1)))))))
                              (#{f\ 3482}# #{symnames\ 3480}# 0))))
                        (#{search\ 3461}#
                          (lambda (#{sym\ 3485}#
                                   #{subst\ 3486}#
                                   #{marks\ 3487}#)
                            (if (null? #{subst\ 3486}#)
                              (values #f #{marks\ 3487}#)
                              (let ((#{fst\ 3488}# (car #{subst\ 3486}#)))
                                (if (eq? #{fst\ 3488}# (quote shift))
                                  (#{search\ 3461}#
                                    #{sym\ 3485}#
                                    (cdr #{subst\ 3486}#)
                                    (cdr #{marks\ 3487}#))
                                  (let ((#{symnames\ 3489}#
                                          (#{ribcage-symnames\ 2690}#
                                            #{fst\ 3488}#)))
                                    (if (vector? #{symnames\ 3489}#)
                                      (#{search-vector-rib\ 3463}#
                                        #{sym\ 3485}#
                                        #{subst\ 3486}#
                                        #{marks\ 3487}#
                                        #{symnames\ 3489}#
                                        #{fst\ 3488}#)
                                      (#{search-list-rib\ 3462}#
                                        #{sym\ 3485}#
                                        #{subst\ 3486}#
                                        #{marks\ 3487}#
                                        #{symnames\ 3489}#
                                        #{fst\ 3488}#)))))))))
                 (if (symbol? #{id\ 3459}#)
                   (let ((#{t\ 3490}#
                           (call-with-values
                             (lambda ()
                               (#{search\ 3461}#
                                 #{id\ 3459}#
                                 (#{wrap-subst\ 2685}# #{w\ 3460}#)
                                 (#{wrap-marks\ 2684}# #{w\ 3460}#)))
                             (lambda (#{x\ 3491}# . #{ignore\ 3492}#)
                               #{x\ 3491}#))))
                     (if #{t\ 3490}# #{t\ 3490}# #{id\ 3459}#))
                   (if (#{syntax-object?\ 2665}# #{id\ 3459}#)
                     (let ((#{id\ 3493}#
                             (#{syntax-object-expression\ 2666}# #{id\ 3459}#))
                           (#{w1\ 3494}#
                             (#{syntax-object-wrap\ 2667}# #{id\ 3459}#)))
                       (let ((#{marks\ 3495}#
                               (#{join-marks\ 2701}#
                                 (#{wrap-marks\ 2684}# #{w\ 3460}#)
                                 (#{wrap-marks\ 2684}# #{w1\ 3494}#))))
                         (call-with-values
                           (lambda ()
                             (#{search\ 3461}#
                               #{id\ 3493}#
                               (#{wrap-subst\ 2685}# #{w\ 3460}#)
                               #{marks\ 3495}#))
                           (lambda (#{new-id\ 3496}# #{marks\ 3497}#)
                             (let ((#{t\ 3498}# #{new-id\ 3496}#))
                               (if #{t\ 3498}#
                                 #{t\ 3498}#
                                 (let ((#{t\ 3499}#
                                         (call-with-values
                                           (lambda ()
                                             (#{search\ 3461}#
                                               #{id\ 3493}#
                                               (#{wrap-subst\ 2685}#
                                                 #{w1\ 3494}#)
                                               #{marks\ 3497}#))
                                           (lambda (#{x\ 3500}#
                                                    .
                                                    #{ignore\ 3501}#)
                                             #{x\ 3500}#))))
                                   (if #{t\ 3499}#
                                     #{t\ 3499}#
                                     #{id\ 3493}#))))))))
                     (syntax-violation
                       'id-var-name
                       "invalid id"
                       #{id\ 3459}#))))))
           (#{same-marks?\ 2702}#
             (lambda (#{x\ 3502}# #{y\ 3503}#)
               (let ((#{t\ 3504}# (eq? #{x\ 3502}# #{y\ 3503}#)))
                 (if #{t\ 3504}#
                   #{t\ 3504}#
                   (if (not (null? #{x\ 3502}#))
                     (if (not (null? #{y\ 3503}#))
                       (if (eq? (car #{x\ 3502}#) (car #{y\ 3503}#))
                         (#{same-marks?\ 2702}#
                           (cdr #{x\ 3502}#)
                           (cdr #{y\ 3503}#))
                         #f)
                       #f)
                     #f)))))
           (#{join-marks\ 2701}#
             (lambda (#{m1\ 3505}# #{m2\ 3506}#)
               (#{smart-append\ 2699}#
                 #{m1\ 3505}#
                 #{m2\ 3506}#)))
           (#{join-wraps\ 2700}#
             (lambda (#{w1\ 3507}# #{w2\ 3508}#)
               (let ((#{m1\ 3509}#
                       (#{wrap-marks\ 2684}# #{w1\ 3507}#))
                     (#{s1\ 3510}#
                       (#{wrap-subst\ 2685}# #{w1\ 3507}#)))
                 (if (null? #{m1\ 3509}#)
                   (if (null? #{s1\ 3510}#)
                     #{w2\ 3508}#
                     (#{make-wrap\ 2683}#
                       (#{wrap-marks\ 2684}# #{w2\ 3508}#)
                       (#{smart-append\ 2699}#
                         #{s1\ 3510}#
                         (#{wrap-subst\ 2685}# #{w2\ 3508}#))))
                   (#{make-wrap\ 2683}#
                     (#{smart-append\ 2699}#
                       #{m1\ 3509}#
                       (#{wrap-marks\ 2684}# #{w2\ 3508}#))
                     (#{smart-append\ 2699}#
                       #{s1\ 3510}#
                       (#{wrap-subst\ 2685}# #{w2\ 3508}#)))))))
           (#{smart-append\ 2699}#
             (lambda (#{m1\ 3511}# #{m2\ 3512}#)
               (if (null? #{m2\ 3512}#)
                 #{m1\ 3511}#
                 (append #{m1\ 3511}# #{m2\ 3512}#))))
           (#{make-binding-wrap\ 2698}#
             (lambda (#{ids\ 3513}# #{labels\ 3514}# #{w\ 3515}#)
               (if (null? #{ids\ 3513}#)
                 #{w\ 3515}#
                 (#{make-wrap\ 2683}#
                   (#{wrap-marks\ 2684}# #{w\ 3515}#)
                   (cons (let ((#{labelvec\ 3516}#
                                 (list->vector #{labels\ 3514}#)))
                           (let ((#{n\ 3517}#
                                   (vector-length #{labelvec\ 3516}#)))
                             (let ((#{symnamevec\ 3518}#
                                     (make-vector #{n\ 3517}#))
                                   (#{marksvec\ 3519}#
                                     (make-vector #{n\ 3517}#)))
                               (begin
                                 (letrec ((#{f\ 3520}#
                                            (lambda (#{ids\ 3521}# #{i\ 3522}#)
                                              (if (not (null? #{ids\ 3521}#))
                                                (call-with-values
                                                  (lambda ()
                                                    (#{id-sym-name&marks\ 2682}#
                                                      (car #{ids\ 3521}#)
                                                      #{w\ 3515}#))
                                                  (lambda (#{symname\ 3523}#
                                                           #{marks\ 3524}#)
                                                    (begin
                                                      (vector-set!
                                                        #{symnamevec\ 3518}#
                                                        #{i\ 3522}#
                                                        #{symname\ 3523}#)
                                                      (vector-set!
                                                        #{marksvec\ 3519}#
                                                        #{i\ 3522}#
                                                        #{marks\ 3524}#)
                                                      (#{f\ 3520}#
                                                        (cdr #{ids\ 3521}#)
                                                        (#{fx+\ 2636}#
                                                          #{i\ 3522}#
                                                          1)))))))))
                                   (#{f\ 3520}# #{ids\ 3513}# 0))
                                 (#{make-ribcage\ 2688}#
                                   #{symnamevec\ 3518}#
                                   #{marksvec\ 3519}#
                                   #{labelvec\ 3516}#)))))
                         (#{wrap-subst\ 2685}# #{w\ 3515}#))))))
           (#{extend-ribcage!\ 2697}#
             (lambda (#{ribcage\ 3525}# #{id\ 3526}# #{label\ 3527}#)
               (begin
                 (#{set-ribcage-symnames!\ 2693}#
                   #{ribcage\ 3525}#
                   (cons (#{syntax-object-expression\ 2666}# #{id\ 3526}#)
                         (#{ribcage-symnames\ 2690}# #{ribcage\ 3525}#)))
                 (#{set-ribcage-marks!\ 2694}#
                   #{ribcage\ 3525}#
                   (cons (#{wrap-marks\ 2684}#
                           (#{syntax-object-wrap\ 2667}# #{id\ 3526}#))
                         (#{ribcage-marks\ 2691}# #{ribcage\ 3525}#)))
                 (#{set-ribcage-labels!\ 2695}#
                   #{ribcage\ 3525}#
                   (cons #{label\ 3527}#
                         (#{ribcage-labels\ 2692}# #{ribcage\ 3525}#))))))
           (#{anti-mark\ 2696}#
             (lambda (#{w\ 3528}#)
               (#{make-wrap\ 2683}#
                 (cons #f (#{wrap-marks\ 2684}# #{w\ 3528}#))
                 (cons 'shift
                       (#{wrap-subst\ 2685}# #{w\ 3528}#)))))
           (#{set-ribcage-labels!\ 2695}#
             (lambda (#{x\ 3529}# #{update\ 3530}#)
               (vector-set! #{x\ 3529}# 3 #{update\ 3530}#)))
           (#{set-ribcage-marks!\ 2694}#
             (lambda (#{x\ 3531}# #{update\ 3532}#)
               (vector-set! #{x\ 3531}# 2 #{update\ 3532}#)))
           (#{set-ribcage-symnames!\ 2693}#
             (lambda (#{x\ 3533}# #{update\ 3534}#)
               (vector-set! #{x\ 3533}# 1 #{update\ 3534}#)))
           (#{ribcage-labels\ 2692}#
             (lambda (#{x\ 3535}#) (vector-ref #{x\ 3535}# 3)))
           (#{ribcage-marks\ 2691}#
             (lambda (#{x\ 3536}#) (vector-ref #{x\ 3536}# 2)))
           (#{ribcage-symnames\ 2690}#
             (lambda (#{x\ 3537}#) (vector-ref #{x\ 3537}# 1)))
           (#{ribcage?\ 2689}#
             (lambda (#{x\ 3538}#)
               (if (vector? #{x\ 3538}#)
                 (if (= (vector-length #{x\ 3538}#) 4)
                   (eq? (vector-ref #{x\ 3538}# 0) (quote ribcage))
                   #f)
                 #f)))
           (#{make-ribcage\ 2688}#
             (lambda (#{symnames\ 3539}#
                      #{marks\ 3540}#
                      #{labels\ 3541}#)
               (vector
                 'ribcage
                 #{symnames\ 3539}#
                 #{marks\ 3540}#
                 #{labels\ 3541}#)))
           (#{gen-labels\ 2687}#
             (lambda (#{ls\ 3542}#)
               (if (null? #{ls\ 3542}#)
                 '()
                 (cons (#{gen-label\ 2686}#)
                       (#{gen-labels\ 2687}# (cdr #{ls\ 3542}#))))))
           (#{gen-label\ 2686}# (lambda () (string #\i)))
           (#{wrap-subst\ 2685}# cdr)
           (#{wrap-marks\ 2684}# car)
           (#{make-wrap\ 2683}# cons)
           (#{id-sym-name&marks\ 2682}#
             (lambda (#{x\ 3543}# #{w\ 3544}#)
               (if (#{syntax-object?\ 2665}# #{x\ 3543}#)
                 (values
                   (#{syntax-object-expression\ 2666}# #{x\ 3543}#)
                   (#{join-marks\ 2701}#
                     (#{wrap-marks\ 2684}# #{w\ 3544}#)
                     (#{wrap-marks\ 2684}#
                       (#{syntax-object-wrap\ 2667}# #{x\ 3543}#))))
                 (values
                   #{x\ 3543}#
                   (#{wrap-marks\ 2684}# #{w\ 3544}#)))))
           (#{id?\ 2681}#
             (lambda (#{x\ 3545}#)
               (if (symbol? #{x\ 3545}#)
                 #t
                 (if (#{syntax-object?\ 2665}# #{x\ 3545}#)
                   (symbol?
                     (#{syntax-object-expression\ 2666}# #{x\ 3545}#))
                   #f))))
           (#{nonsymbol-id?\ 2680}#
             (lambda (#{x\ 3546}#)
               (if (#{syntax-object?\ 2665}# #{x\ 3546}#)
                 (symbol?
                   (#{syntax-object-expression\ 2666}# #{x\ 3546}#))
                 #f)))
           (#{global-extend\ 2679}#
             (lambda (#{type\ 3547}# #{sym\ 3548}# #{val\ 3549}#)
               (#{put-global-definition-hook\ 2642}#
                 #{sym\ 3548}#
                 #{type\ 3547}#
                 #{val\ 3549}#)))
           (#{lookup\ 2678}#
             (lambda (#{x\ 3550}# #{r\ 3551}# #{mod\ 3552}#)
               (let ((#{t\ 3553}# (assq #{x\ 3550}# #{r\ 3551}#)))
                 (if #{t\ 3553}#
                   (cdr #{t\ 3553}#)
                   (if (symbol? #{x\ 3550}#)
                     (let ((#{t\ 3554}#
                             (#{get-global-definition-hook\ 2643}#
                               #{x\ 3550}#
                               #{mod\ 3552}#)))
                       (if #{t\ 3554}# #{t\ 3554}# (quote (global))))
                     '(displaced-lexical))))))
           (#{macros-only-env\ 2677}#
             (lambda (#{r\ 3555}#)
               (if (null? #{r\ 3555}#)
                 '()
                 (let ((#{a\ 3556}# (car #{r\ 3555}#)))
                   (if (eq? (cadr #{a\ 3556}#) (quote macro))
                     (cons #{a\ 3556}#
                           (#{macros-only-env\ 2677}# (cdr #{r\ 3555}#)))
                     (#{macros-only-env\ 2677}# (cdr #{r\ 3555}#)))))))
           (#{extend-var-env\ 2676}#
             (lambda (#{labels\ 3557}# #{vars\ 3558}# #{r\ 3559}#)
               (if (null? #{labels\ 3557}#)
                 #{r\ 3559}#
                 (#{extend-var-env\ 2676}#
                   (cdr #{labels\ 3557}#)
                   (cdr #{vars\ 3558}#)
                   (cons (cons (car #{labels\ 3557}#)
                               (cons (quote lexical) (car #{vars\ 3558}#)))
                         #{r\ 3559}#)))))
           (#{extend-env\ 2675}#
             (lambda (#{labels\ 3560}# #{bindings\ 3561}# #{r\ 3562}#)
               (if (null? #{labels\ 3560}#)
                 #{r\ 3562}#
                 (#{extend-env\ 2675}#
                   (cdr #{labels\ 3560}#)
                   (cdr #{bindings\ 3561}#)
                   (cons (cons (car #{labels\ 3560}#)
                               (car #{bindings\ 3561}#))
                         #{r\ 3562}#)))))
           (#{binding-value\ 2674}# cdr)
           (#{binding-type\ 2673}# car)
           (#{source-annotation\ 2672}#
             (lambda (#{x\ 3563}#)
               (if (#{syntax-object?\ 2665}# #{x\ 3563}#)
                 (#{source-annotation\ 2672}#
                   (#{syntax-object-expression\ 2666}# #{x\ 3563}#))
                 (if (pair? #{x\ 3563}#)
                   (let ((#{props\ 3564}# (source-properties #{x\ 3563}#)))
                     (if (pair? #{props\ 3564}#) #{props\ 3564}# #f))
                   #f))))
           (#{set-syntax-object-module!\ 2671}#
             (lambda (#{x\ 3565}# #{update\ 3566}#)
               (vector-set! #{x\ 3565}# 3 #{update\ 3566}#)))
           (#{set-syntax-object-wrap!\ 2670}#
             (lambda (#{x\ 3567}# #{update\ 3568}#)
               (vector-set! #{x\ 3567}# 2 #{update\ 3568}#)))
           (#{set-syntax-object-expression!\ 2669}#
             (lambda (#{x\ 3569}# #{update\ 3570}#)
               (vector-set! #{x\ 3569}# 1 #{update\ 3570}#)))
           (#{syntax-object-module\ 2668}#
             (lambda (#{x\ 3571}#) (vector-ref #{x\ 3571}# 3)))
           (#{syntax-object-wrap\ 2667}#
             (lambda (#{x\ 3572}#) (vector-ref #{x\ 3572}# 2)))
           (#{syntax-object-expression\ 2666}#
             (lambda (#{x\ 3573}#) (vector-ref #{x\ 3573}# 1)))
           (#{syntax-object?\ 2665}#
             (lambda (#{x\ 3574}#)
               (if (vector? #{x\ 3574}#)
                 (if (= (vector-length #{x\ 3574}#) 4)
                   (eq? (vector-ref #{x\ 3574}# 0)
                        'syntax-object)
                   #f)
                 #f)))
           (#{make-syntax-object\ 2664}#
             (lambda (#{expression\ 3575}#
                      #{wrap\ 3576}#
                      #{module\ 3577}#)
               (vector
                 'syntax-object
                 #{expression\ 3575}#
                 #{wrap\ 3576}#
                 #{module\ 3577}#)))
           (#{build-letrec\ 2663}#
             (lambda (#{src\ 3578}#
                      #{ids\ 3579}#
                      #{vars\ 3580}#
                      #{val-exps\ 3581}#
                      #{body-exp\ 3582}#)
               (if (null? #{vars\ 3580}#)
                 #{body-exp\ 3582}#
                 (let ((#{atom-key\ 3583}# (fluid-ref #{*mode*\ 2635}#)))
                   (if (memv #{atom-key\ 3583}# (quote (c)))
                     (begin
                       (for-each
                         #{maybe-name-value!\ 2653}#
                         #{ids\ 3579}#
                         #{val-exps\ 3581}#)
                       ((@ (language tree-il) make-letrec)
                        #{src\ 3578}#
                        #{ids\ 3579}#
                        #{vars\ 3580}#
                        #{val-exps\ 3581}#
                        #{body-exp\ 3582}#))
                     (#{decorate-source\ 2644}#
                       (list 'letrec
                             (map list #{vars\ 3580}# #{val-exps\ 3581}#)
                             #{body-exp\ 3582}#)
                       #{src\ 3578}#))))))
           (#{build-named-let\ 2662}#
             (lambda (#{src\ 3584}#
                      #{ids\ 3585}#
                      #{vars\ 3586}#
                      #{val-exps\ 3587}#
                      #{body-exp\ 3588}#)
               (let ((#{f\ 3589}# (car #{vars\ 3586}#))
                     (#{f-name\ 3590}# (car #{ids\ 3585}#))
                     (#{vars\ 3591}# (cdr #{vars\ 3586}#))
                     (#{ids\ 3592}# (cdr #{ids\ 3585}#)))
                 (let ((#{atom-key\ 3593}# (fluid-ref #{*mode*\ 2635}#)))
                   (if (memv #{atom-key\ 3593}# (quote (c)))
                     (let ((#{proc\ 3594}#
                             (#{build-simple-lambda\ 2655}#
                               #{src\ 3584}#
                               #{ids\ 3592}#
                               #f
                               #{vars\ 3591}#
                               #f
                               #{body-exp\ 3588}#)))
                       (begin
                         (#{maybe-name-value!\ 2653}#
                           #{f-name\ 3590}#
                           #{proc\ 3594}#)
                         (for-each
                           #{maybe-name-value!\ 2653}#
                           #{ids\ 3592}#
                           #{val-exps\ 3587}#)
                         ((@ (language tree-il) make-letrec)
                          #{src\ 3584}#
                          (list #{f-name\ 3590}#)
                          (list #{f\ 3589}#)
                          (list #{proc\ 3594}#)
                          (#{build-application\ 2646}#
                            #{src\ 3584}#
                            (#{build-lexical-reference\ 2648}#
                              'fun
                              #{src\ 3584}#
                              #{f-name\ 3590}#
                              #{f\ 3589}#)
                            #{val-exps\ 3587}#))))
                     (#{decorate-source\ 2644}#
                       (list 'letrec
                             (list (list #{f\ 3589}#
                                         (list 'lambda
                                               #{vars\ 3591}#
                                               #{body-exp\ 3588}#)))
                             (cons #{f\ 3589}# #{val-exps\ 3587}#))
                       #{src\ 3584}#))))))
           (#{build-let\ 2661}#
             (lambda (#{src\ 3595}#
                      #{ids\ 3596}#
                      #{vars\ 3597}#
                      #{val-exps\ 3598}#
                      #{body-exp\ 3599}#)
               (if (null? #{vars\ 3597}#)
                 #{body-exp\ 3599}#
                 (let ((#{atom-key\ 3600}# (fluid-ref #{*mode*\ 2635}#)))
                   (if (memv #{atom-key\ 3600}# (quote (c)))
                     (begin
                       (for-each
                         #{maybe-name-value!\ 2653}#
                         #{ids\ 3596}#
                         #{val-exps\ 3598}#)
                       ((@ (language tree-il) make-let)
                        #{src\ 3595}#
                        #{ids\ 3596}#
                        #{vars\ 3597}#
                        #{val-exps\ 3598}#
                        #{body-exp\ 3599}#))
                     (#{decorate-source\ 2644}#
                       (list 'let
                             (map list #{vars\ 3597}# #{val-exps\ 3598}#)
                             #{body-exp\ 3599}#)
                       #{src\ 3595}#))))))
           (#{build-sequence\ 2660}#
             (lambda (#{src\ 3601}# #{exps\ 3602}#)
               (if (null? (cdr #{exps\ 3602}#))
                 (car #{exps\ 3602}#)
                 (let ((#{atom-key\ 3603}# (fluid-ref #{*mode*\ 2635}#)))
                   (if (memv #{atom-key\ 3603}# (quote (c)))
                     ((@ (language tree-il) make-sequence)
                      #{src\ 3601}#
                      #{exps\ 3602}#)
                     (#{decorate-source\ 2644}#
                       (cons (quote begin) #{exps\ 3602}#)
                       #{src\ 3601}#))))))
           (#{build-data\ 2659}#
             (lambda (#{src\ 3604}# #{exp\ 3605}#)
               (let ((#{atom-key\ 3606}# (fluid-ref #{*mode*\ 2635}#)))
                 (if (memv #{atom-key\ 3606}# (quote (c)))
                   ((@ (language tree-il) make-const)
                    #{src\ 3604}#
                    #{exp\ 3605}#)
                   (#{decorate-source\ 2644}#
                     (if (if (self-evaluating? #{exp\ 3605}#)
                           (not (vector? #{exp\ 3605}#))
                           #f)
                       #{exp\ 3605}#
                       (list (quote quote) #{exp\ 3605}#))
                     #{src\ 3604}#)))))
           (#{build-primref\ 2658}#
             (lambda (#{src\ 3607}# #{name\ 3608}#)
               (if (equal?
                     (module-name (current-module))
                     '(guile))
                 (let ((#{atom-key\ 3609}# (fluid-ref #{*mode*\ 2635}#)))
                   (if (memv #{atom-key\ 3609}# (quote (c)))
                     ((@ (language tree-il) make-toplevel-ref)
                      #{src\ 3607}#
                      #{name\ 3608}#)
                     (#{decorate-source\ 2644}#
                       #{name\ 3608}#
                       #{src\ 3607}#)))
                 (let ((#{atom-key\ 3610}# (fluid-ref #{*mode*\ 2635}#)))
                   (if (memv #{atom-key\ 3610}# (quote (c)))
                     ((@ (language tree-il) make-module-ref)
                      #{src\ 3607}#
                      '(guile)
                      #{name\ 3608}#
                      #f)
                     (#{decorate-source\ 2644}#
                       (list (quote @@) (quote (guile)) #{name\ 3608}#)
                       #{src\ 3607}#))))))
           (#{build-lambda-case\ 2657}#
             (lambda (#{src\ 3611}#
                      #{req\ 3612}#
                      #{opt\ 3613}#
                      #{rest\ 3614}#
                      #{kw\ 3615}#
                      #{inits\ 3616}#
                      #{vars\ 3617}#
                      #{body\ 3618}#
                      #{else-case\ 3619}#)
               (let ((#{atom-key\ 3620}# (fluid-ref #{*mode*\ 2635}#)))
                 (if (memv #{atom-key\ 3620}# (quote (c)))
                   ((@ (language tree-il) make-lambda-case)
                    #{src\ 3611}#
                    #{req\ 3612}#
                    #{opt\ 3613}#
                    #{rest\ 3614}#
                    #{kw\ 3615}#
                    #{inits\ 3616}#
                    #{vars\ 3617}#
                    #{body\ 3618}#
                    #{else-case\ 3619}#)
                   (let ((#{nreq\ 3621}# (length #{req\ 3612}#)))
                     (let ((#{nopt\ 3622}#
                             (if #{opt\ 3613}# (length #{opt\ 3613}#) 0)))
                       (let ((#{rest-idx\ 3623}#
                               (if #{rest\ 3614}#
                                 (+ #{nreq\ 3621}# #{nopt\ 3622}#)
                                 #f)))
                         (let ((#{allow-other-keys?\ 3624}#
                                 (if #{kw\ 3615}# (car #{kw\ 3615}#) #f)))
                           (let ((#{kw-indices\ 3625}#
                                   (map (lambda (#{x\ 3626}#)
                                          (cons (car #{x\ 3626}#)
                                                (list-index
                                                  #{vars\ 3617}#
                                                  (caddr #{x\ 3626}#))))
                                        (if #{kw\ 3615}#
                                          (cdr #{kw\ 3615}#)
                                          '()))))
                             (let ((#{nargs\ 3627}#
                                     (apply max
                                            (+ #{nreq\ 3621}#
                                               #{nopt\ 3622}#
                                               (if #{rest\ 3614}# 1 0))
                                            (map 1+
                                                 (map cdr
                                                      #{kw-indices\ 3625}#)))))
                               (begin
                                 (let ((#{t\ 3628}#
                                         (= #{nargs\ 3627}#
                                            (length #{vars\ 3617}#)
                                            (+ #{nreq\ 3621}#
                                               (length #{inits\ 3616}#)
                                               (if #{rest\ 3614}# 1 0)))))
                                   (if #{t\ 3628}#
                                     #{t\ 3628}#
                                     (error "something went wrong"
                                            #{req\ 3612}#
                                            #{opt\ 3613}#
                                            #{rest\ 3614}#
                                            #{kw\ 3615}#
                                            #{inits\ 3616}#
                                            #{vars\ 3617}#
                                            #{nreq\ 3621}#
                                            #{nopt\ 3622}#
                                            #{kw-indices\ 3625}#
                                            #{nargs\ 3627}#)))
                                 (#{decorate-source\ 2644}#
                                   (cons (list (cons '(@@ (ice-9 optargs)
                                                          parse-lambda-case)
                                                     (cons (list 'quote
                                                                 (list #{nreq\ 3621}#
                                                                       #{nopt\ 3622}#
                                                                       #{rest-idx\ 3623}#
                                                                       #{nargs\ 3627}#
                                                                       #{allow-other-keys?\ 3624}#
                                                                       #{kw-indices\ 3625}#))
                                                           (cons (cons 'list
                                                                       (map (lambda (#{i\ 3629}#)
                                                                              (list 'lambda
                                                                                    #{vars\ 3617}#
                                                                                    #{i\ 3629}#))
                                                                            #{inits\ 3616}#))
                                                                 '(%%args))))
                                               '=>
                                               (list 'lambda
                                                     '(%%%args . _)
                                                     (cons 'apply
                                                           (cons (list 'lambda
                                                                       #{vars\ 3617}#
                                                                       #{body\ 3618}#)
                                                                 '(%%%args)))))
                                         (let ((#{t\ 3630}#
                                                 #{else-case\ 3619}#))
                                           (if #{t\ 3630}#
                                             #{t\ 3630}#
                                             '((%%args
                                                 (error "wrong number of arguments"
                                                        %%args))))))
                                   #{src\ 3611}#))))))))))))
           (#{build-case-lambda\ 2656}#
             (lambda (#{src\ 3631}#
                      #{docstring\ 3632}#
                      #{body\ 3633}#)
               (let ((#{atom-key\ 3634}# (fluid-ref #{*mode*\ 2635}#)))
                 (if (memv #{atom-key\ 3634}# (quote (c)))
                   ((@ (language tree-il) make-lambda)
                    #{src\ 3631}#
                    (if #{docstring\ 3632}#
                      (list (cons (quote documentation) #{docstring\ 3632}#))
                      '())
                    #{body\ 3633}#)
                   (#{decorate-source\ 2644}#
                     (cons 'lambda
                           (cons '%%args
                                 (append
                                   (if #{docstring\ 3632}#
                                     (list #{docstring\ 3632}#)
                                     '())
                                   (list (cons (quote cond) #{body\ 3633}#)))))
                     #{src\ 3631}#)))))
           (#{build-simple-lambda\ 2655}#
             (lambda (#{src\ 3635}#
                      #{req\ 3636}#
                      #{rest\ 3637}#
                      #{vars\ 3638}#
                      #{docstring\ 3639}#
                      #{exp\ 3640}#)
               (let ((#{atom-key\ 3641}# (fluid-ref #{*mode*\ 2635}#)))
                 (if (memv #{atom-key\ 3641}# (quote (c)))
                   ((@ (language tree-il) make-lambda)
                    #{src\ 3635}#
                    (if #{docstring\ 3639}#
                      (list (cons (quote documentation) #{docstring\ 3639}#))
                      '())
                    ((@ (language tree-il) make-lambda-case)
                     #{src\ 3635}#
                     #{req\ 3636}#
                     #f
                     #{rest\ 3637}#
                     #f
                     '()
                     #{vars\ 3638}#
                     #{exp\ 3640}#
                     #f))
                   (#{decorate-source\ 2644}#
                     (cons 'lambda
                           (cons (if #{rest\ 3637}#
                                   (apply cons* #{vars\ 3638}#)
                                   #{vars\ 3638}#)
                                 (append
                                   (if #{docstring\ 3639}#
                                     (list #{docstring\ 3639}#)
                                     '())
                                   (list #{exp\ 3640}#))))
                     #{src\ 3635}#)))))
           (#{build-global-definition\ 2654}#
             (lambda (#{source\ 3642}# #{var\ 3643}# #{exp\ 3644}#)
               (let ((#{atom-key\ 3645}# (fluid-ref #{*mode*\ 2635}#)))
                 (if (memv #{atom-key\ 3645}# (quote (c)))
                   (begin
                     (#{maybe-name-value!\ 2653}#
                       #{var\ 3643}#
                       #{exp\ 3644}#)
                     ((@ (language tree-il) make-toplevel-define)
                      #{source\ 3642}#
                      #{var\ 3643}#
                      #{exp\ 3644}#))
                   (#{decorate-source\ 2644}#
                     (list (quote define) #{var\ 3643}# #{exp\ 3644}#)
                     #{source\ 3642}#)))))
           (#{maybe-name-value!\ 2653}#
             (lambda (#{name\ 3646}# #{val\ 3647}#)
               (if ((@ (language tree-il) lambda?) #{val\ 3647}#)
                 (let ((#{meta\ 3648}#
                         ((@ (language tree-il) lambda-meta)
                          #{val\ 3647}#)))
                   (if (not (assq (quote name) #{meta\ 3648}#))
                     ((setter (@ (language tree-il) lambda-meta))
                      #{val\ 3647}#
                      (acons 'name
                             #{name\ 3646}#
                             #{meta\ 3648}#)))))))
           (#{build-global-assignment\ 2652}#
             (lambda (#{source\ 3649}#
                      #{var\ 3650}#
                      #{exp\ 3651}#
                      #{mod\ 3652}#)
               (#{analyze-variable\ 2650}#
                 #{mod\ 3652}#
                 #{var\ 3650}#
                 (lambda (#{mod\ 3653}# #{var\ 3654}# #{public?\ 3655}#)
                   (let ((#{atom-key\ 3656}# (fluid-ref #{*mode*\ 2635}#)))
                     (if (memv #{atom-key\ 3656}# (quote (c)))
                       ((@ (language tree-il) make-module-set)
                        #{source\ 3649}#
                        #{mod\ 3653}#
                        #{var\ 3654}#
                        #{public?\ 3655}#
                        #{exp\ 3651}#)
                       (#{decorate-source\ 2644}#
                         (list 'set!
                               (list (if #{public?\ 3655}#
                                       '@
                                       '@@)
                                     #{mod\ 3653}#
                                     #{var\ 3654}#)
                               #{exp\ 3651}#)
                         #{source\ 3649}#))))
                 (lambda (#{var\ 3657}#)
                   (let ((#{atom-key\ 3658}# (fluid-ref #{*mode*\ 2635}#)))
                     (if (memv #{atom-key\ 3658}# (quote (c)))
                       ((@ (language tree-il) make-toplevel-set)
                        #{source\ 3649}#
                        #{var\ 3657}#
                        #{exp\ 3651}#)
                       (#{decorate-source\ 2644}#
                         (list (quote set!) #{var\ 3657}# #{exp\ 3651}#)
                         #{source\ 3649}#)))))))
           (#{build-global-reference\ 2651}#
             (lambda (#{source\ 3659}# #{var\ 3660}# #{mod\ 3661}#)
               (#{analyze-variable\ 2650}#
                 #{mod\ 3661}#
                 #{var\ 3660}#
                 (lambda (#{mod\ 3662}# #{var\ 3663}# #{public?\ 3664}#)
                   (let ((#{atom-key\ 3665}# (fluid-ref #{*mode*\ 2635}#)))
                     (if (memv #{atom-key\ 3665}# (quote (c)))
                       ((@ (language tree-il) make-module-ref)
                        #{source\ 3659}#
                        #{mod\ 3662}#
                        #{var\ 3663}#
                        #{public?\ 3664}#)
                       (#{decorate-source\ 2644}#
                         (list (if #{public?\ 3664}# (quote @) (quote @@))
                               #{mod\ 3662}#
                               #{var\ 3663}#)
                         #{source\ 3659}#))))
                 (lambda (#{var\ 3666}#)
                   (let ((#{atom-key\ 3667}# (fluid-ref #{*mode*\ 2635}#)))
                     (if (memv #{atom-key\ 3667}# (quote (c)))
                       ((@ (language tree-il) make-toplevel-ref)
                        #{source\ 3659}#
                        #{var\ 3666}#)
                       (#{decorate-source\ 2644}#
                         #{var\ 3666}#
                         #{source\ 3659}#)))))))
           (#{analyze-variable\ 2650}#
             (lambda (#{mod\ 3668}#
                      #{var\ 3669}#
                      #{modref-cont\ 3670}#
                      #{bare-cont\ 3671}#)
               (if (not #{mod\ 3668}#)
                 (#{bare-cont\ 3671}# #{var\ 3669}#)
                 (let ((#{kind\ 3672}# (car #{mod\ 3668}#))
                       (#{mod\ 3673}# (cdr #{mod\ 3668}#)))
                   (if (memv #{kind\ 3672}# (quote (public)))
                     (#{modref-cont\ 3670}#
                       #{mod\ 3673}#
                       #{var\ 3669}#
                       #t)
                     (if (memv #{kind\ 3672}# (quote (private)))
                       (if (not (equal?
                                  #{mod\ 3673}#
                                  (module-name (current-module))))
                         (#{modref-cont\ 3670}#
                           #{mod\ 3673}#
                           #{var\ 3669}#
                           #f)
                         (#{bare-cont\ 3671}# #{var\ 3669}#))
                       (if (memv #{kind\ 3672}# (quote (bare)))
                         (#{bare-cont\ 3671}# #{var\ 3669}#)
                         (if (memv #{kind\ 3672}# (quote (hygiene)))
                           (if (if (not (equal?
                                          #{mod\ 3673}#
                                          (module-name (current-module))))
                                 (module-variable
                                   (resolve-module #{mod\ 3673}#)
                                   #{var\ 3669}#)
                                 #f)
                             (#{modref-cont\ 3670}#
                               #{mod\ 3673}#
                               #{var\ 3669}#
                               #f)
                             (#{bare-cont\ 3671}# #{var\ 3669}#))
                           (syntax-violation
                             #f
                             "bad module kind"
                             #{var\ 3669}#
                             #{mod\ 3673}#)))))))))
           (#{build-lexical-assignment\ 2649}#
             (lambda (#{source\ 3674}#
                      #{name\ 3675}#
                      #{var\ 3676}#
                      #{exp\ 3677}#)
               (let ((#{atom-key\ 3678}# (fluid-ref #{*mode*\ 2635}#)))
                 (if (memv #{atom-key\ 3678}# (quote (c)))
                   ((@ (language tree-il) make-lexical-set)
                    #{source\ 3674}#
                    #{name\ 3675}#
                    #{var\ 3676}#
                    #{exp\ 3677}#)
                   (#{decorate-source\ 2644}#
                     (list (quote set!) #{var\ 3676}# #{exp\ 3677}#)
                     #{source\ 3674}#)))))
           (#{build-lexical-reference\ 2648}#
             (lambda (#{type\ 3679}#
                      #{source\ 3680}#
                      #{name\ 3681}#
                      #{var\ 3682}#)
               (let ((#{atom-key\ 3683}# (fluid-ref #{*mode*\ 2635}#)))
                 (if (memv #{atom-key\ 3683}# (quote (c)))
                   ((@ (language tree-il) make-lexical-ref)
                    #{source\ 3680}#
                    #{name\ 3681}#
                    #{var\ 3682}#)
                   (#{decorate-source\ 2644}#
                     #{var\ 3682}#
                     #{source\ 3680}#)))))
           (#{build-conditional\ 2647}#
             (lambda (#{source\ 3684}#
                      #{test-exp\ 3685}#
                      #{then-exp\ 3686}#
                      #{else-exp\ 3687}#)
               (let ((#{atom-key\ 3688}# (fluid-ref #{*mode*\ 2635}#)))
                 (if (memv #{atom-key\ 3688}# (quote (c)))
                   ((@ (language tree-il) make-conditional)
                    #{source\ 3684}#
                    #{test-exp\ 3685}#
                    #{then-exp\ 3686}#
                    #{else-exp\ 3687}#)
                   (#{decorate-source\ 2644}#
                     (if (equal? #{else-exp\ 3687}# (quote (if #f #f)))
                       (list 'if
                             #{test-exp\ 3685}#
                             #{then-exp\ 3686}#)
                       (list 'if
                             #{test-exp\ 3685}#
                             #{then-exp\ 3686}#
                             #{else-exp\ 3687}#))
                     #{source\ 3684}#)))))
           (#{build-application\ 2646}#
             (lambda (#{source\ 3689}#
                      #{fun-exp\ 3690}#
                      #{arg-exps\ 3691}#)
               (let ((#{atom-key\ 3692}# (fluid-ref #{*mode*\ 2635}#)))
                 (if (memv #{atom-key\ 3692}# (quote (c)))
                   ((@ (language tree-il) make-application)
                    #{source\ 3689}#
                    #{fun-exp\ 3690}#
                    #{arg-exps\ 3691}#)
                   (#{decorate-source\ 2644}#
                     (cons #{fun-exp\ 3690}# #{arg-exps\ 3691}#)
                     #{source\ 3689}#)))))
           (#{build-void\ 2645}#
             (lambda (#{source\ 3693}#)
               (let ((#{atom-key\ 3694}# (fluid-ref #{*mode*\ 2635}#)))
                 (if (memv #{atom-key\ 3694}# (quote (c)))
                   ((@ (language tree-il) make-void)
                    #{source\ 3693}#)
                   (#{decorate-source\ 2644}#
                     '(if #f #f)
                     #{source\ 3693}#)))))
           (#{decorate-source\ 2644}#
             (lambda (#{e\ 3695}# #{s\ 3696}#)
               (begin
                 (if (if (pair? #{e\ 3695}#) #{s\ 3696}# #f)
                   (set-source-properties! #{e\ 3695}# #{s\ 3696}#))
                 #{e\ 3695}#)))
           (#{get-global-definition-hook\ 2643}#
             (lambda (#{symbol\ 3697}# #{module\ 3698}#)
               (begin
                 (if (if (not #{module\ 3698}#) (current-module) #f)
                   (warn "module system is booted, we should have a module"
                         #{symbol\ 3697}#))
                 (let ((#{v\ 3699}#
                         (module-variable
                           (if #{module\ 3698}#
                             (resolve-module (cdr #{module\ 3698}#))
                             (current-module))
                           #{symbol\ 3697}#)))
                   (if #{v\ 3699}#
                     (if (variable-bound? #{v\ 3699}#)
                       (let ((#{val\ 3700}# (variable-ref #{v\ 3699}#)))
                         (if (macro? #{val\ 3700}#)
                           (if (syncase-macro-type #{val\ 3700}#)
                             (cons (syncase-macro-type #{val\ 3700}#)
                                   (syncase-macro-binding #{val\ 3700}#))
                             #f)
                           #f))
                       #f)
                     #f)))))
           (#{put-global-definition-hook\ 2642}#
             (lambda (#{symbol\ 3701}# #{type\ 3702}# #{val\ 3703}#)
               (let ((#{existing\ 3704}#
                       (let ((#{v\ 3705}#
                               (module-variable
                                 (current-module)
                                 #{symbol\ 3701}#)))
                         (if #{v\ 3705}#
                           (if (variable-bound? #{v\ 3705}#)
                             (let ((#{val\ 3706}# (variable-ref #{v\ 3705}#)))
                               (if (macro? #{val\ 3706}#)
                                 (if (not (syncase-macro-type #{val\ 3706}#))
                                   #{val\ 3706}#
                                   #f)
                                 #f))
                             #f)
                           #f))))
                 (module-define!
                   (current-module)
                   #{symbol\ 3701}#
                   (if #{existing\ 3704}#
                     (make-extended-syncase-macro
                       #{existing\ 3704}#
                       #{type\ 3702}#
                       #{val\ 3703}#)
                     (make-syncase-macro #{type\ 3702}# #{val\ 3703}#))))))
           (#{local-eval-hook\ 2641}#
             (lambda (#{x\ 3707}# #{mod\ 3708}#)
               (primitive-eval
                 (list #{noexpand\ 2634}#
                       (let ((#{atom-key\ 3709}# (fluid-ref #{*mode*\ 2635}#)))
                         (if (memv #{atom-key\ 3709}# (quote (c)))
                           ((@ (language tree-il) tree-il->scheme)
                            #{x\ 3707}#)
                           #{x\ 3707}#))))))
           (#{top-level-eval-hook\ 2640}#
             (lambda (#{x\ 3710}# #{mod\ 3711}#)
               (primitive-eval
                 (list #{noexpand\ 2634}#
                       (let ((#{atom-key\ 3712}# (fluid-ref #{*mode*\ 2635}#)))
                         (if (memv #{atom-key\ 3712}# (quote (c)))
                           ((@ (language tree-il) tree-il->scheme)
                            #{x\ 3710}#)
                           #{x\ 3710}#))))))
           (#{fx<\ 2639}# <)
           (#{fx=\ 2638}# =)
           (#{fx-\ 2637}# -)
           (#{fx+\ 2636}# +)
           (#{*mode*\ 2635}# (make-fluid))
           (#{noexpand\ 2634}# "noexpand"))
    (begin
      (#{global-extend\ 2679}#
        'local-syntax
        'letrec-syntax
        #t)
      (#{global-extend\ 2679}#
        'local-syntax
        'let-syntax
        #f)
      (#{global-extend\ 2679}#
        'core
        'fluid-let-syntax
        (lambda (#{e\ 3713}#
                 #{r\ 3714}#
                 #{w\ 3715}#
                 #{s\ 3716}#
                 #{mod\ 3717}#)
          ((lambda (#{tmp\ 3718}#)
             ((lambda (#{tmp\ 3719}#)
                (if (if #{tmp\ 3719}#
                      (apply (lambda (#{_\ 3720}#
                                      #{var\ 3721}#
                                      #{val\ 3722}#
                                      #{e1\ 3723}#
                                      #{e2\ 3724}#)
                               (#{valid-bound-ids?\ 2706}# #{var\ 3721}#))
                             #{tmp\ 3719}#)
                      #f)
                  (apply (lambda (#{_\ 3726}#
                                  #{var\ 3727}#
                                  #{val\ 3728}#
                                  #{e1\ 3729}#
                                  #{e2\ 3730}#)
                           (let ((#{names\ 3731}#
                                   (map (lambda (#{x\ 3732}#)
                                          (#{id-var-name\ 2703}#
                                            #{x\ 3732}#
                                            #{w\ 3715}#))
                                        #{var\ 3727}#)))
                             (begin
                               (for-each
                                 (lambda (#{id\ 3734}# #{n\ 3735}#)
                                   (let ((#{atom-key\ 3736}#
                                           (#{binding-type\ 2673}#
                                             (#{lookup\ 2678}#
                                               #{n\ 3735}#
                                               #{r\ 3714}#
                                               #{mod\ 3717}#))))
                                     (if (memv #{atom-key\ 3736}#
                                               '(displaced-lexical))
                                       (syntax-violation
                                         'fluid-let-syntax
                                         "identifier out of context"
                                         #{e\ 3713}#
                                         (#{source-wrap\ 2710}#
                                           #{id\ 3734}#
                                           #{w\ 3715}#
                                           #{s\ 3716}#
                                           #{mod\ 3717}#)))))
                                 #{var\ 3727}#
                                 #{names\ 3731}#)
                               (#{chi-body\ 2721}#
                                 (cons #{e1\ 3729}# #{e2\ 3730}#)
                                 (#{source-wrap\ 2710}#
                                   #{e\ 3713}#
                                   #{w\ 3715}#
                                   #{s\ 3716}#
                                   #{mod\ 3717}#)
                                 (#{extend-env\ 2675}#
                                   #{names\ 3731}#
                                   (let ((#{trans-r\ 3739}#
                                           (#{macros-only-env\ 2677}#
                                             #{r\ 3714}#)))
                                     (map (lambda (#{x\ 3740}#)
                                            (cons 'macro
                                                  (#{eval-local-transformer\ 2723}#
                                                    (#{chi\ 2717}#
                                                      #{x\ 3740}#
                                                      #{trans-r\ 3739}#
                                                      #{w\ 3715}#
                                                      #{mod\ 3717}#)
                                                    #{mod\ 3717}#)))
                                          #{val\ 3728}#))
                                   #{r\ 3714}#)
                                 #{w\ 3715}#
                                 #{mod\ 3717}#))))
                         #{tmp\ 3719}#)
                  ((lambda (#{_\ 3742}#)
                     (syntax-violation
                       'fluid-let-syntax
                       "bad syntax"
                       (#{source-wrap\ 2710}#
                         #{e\ 3713}#
                         #{w\ 3715}#
                         #{s\ 3716}#
                         #{mod\ 3717}#)))
                   #{tmp\ 3718}#)))
              ($sc-dispatch
                #{tmp\ 3718}#
                '(any #(each (any any)) any . each-any))))
           #{e\ 3713}#)))
      (#{global-extend\ 2679}#
        'core
        'quote
        (lambda (#{e\ 3743}#
                 #{r\ 3744}#
                 #{w\ 3745}#
                 #{s\ 3746}#
                 #{mod\ 3747}#)
          ((lambda (#{tmp\ 3748}#)
             ((lambda (#{tmp\ 3749}#)
                (if #{tmp\ 3749}#
                  (apply (lambda (#{_\ 3750}# #{e\ 3751}#)
                           (#{build-data\ 2659}#
                             #{s\ 3746}#
                             (#{strip\ 2730}# #{e\ 3751}# #{w\ 3745}#)))
                         #{tmp\ 3749}#)
                  ((lambda (#{_\ 3752}#)
                     (syntax-violation
                       'quote
                       "bad syntax"
                       (#{source-wrap\ 2710}#
                         #{e\ 3743}#
                         #{w\ 3745}#
                         #{s\ 3746}#
                         #{mod\ 3747}#)))
                   #{tmp\ 3748}#)))
              ($sc-dispatch #{tmp\ 3748}# (quote (any any)))))
           #{e\ 3743}#)))
      (#{global-extend\ 2679}#
        'core
        'syntax
        (letrec ((#{regen\ 3760}#
                   (lambda (#{x\ 3761}#)
                     (let ((#{atom-key\ 3762}# (car #{x\ 3761}#)))
                       (if (memv #{atom-key\ 3762}# (quote (ref)))
                         (#{build-lexical-reference\ 2648}#
                           'value
                           #f
                           (cadr #{x\ 3761}#)
                           (cadr #{x\ 3761}#))
                         (if (memv #{atom-key\ 3762}# (quote (primitive)))
                           (#{build-primref\ 2658}# #f (cadr #{x\ 3761}#))
                           (if (memv #{atom-key\ 3762}# (quote (quote)))
                             (#{build-data\ 2659}# #f (cadr #{x\ 3761}#))
                             (if (memv #{atom-key\ 3762}# (quote (lambda)))
                               (if (list? (cadr #{x\ 3761}#))
                                 (#{build-simple-lambda\ 2655}#
                                   #f
                                   (cadr #{x\ 3761}#)
                                   #f
                                   (cadr #{x\ 3761}#)
                                   #f
                                   (#{regen\ 3760}# (caddr #{x\ 3761}#)))
                                 (error "how did we get here" #{x\ 3761}#))
                               (#{build-application\ 2646}#
                                 #f
                                 (#{build-primref\ 2658}# #f (car #{x\ 3761}#))
                                 (map #{regen\ 3760}#
                                      (cdr #{x\ 3761}#))))))))))
                 (#{gen-vector\ 3759}#
                   (lambda (#{x\ 3763}#)
                     (if (eq? (car #{x\ 3763}#) (quote list))
                       (cons (quote vector) (cdr #{x\ 3763}#))
                       (if (eq? (car #{x\ 3763}#) (quote quote))
                         (list 'quote
                               (list->vector (cadr #{x\ 3763}#)))
                         (list (quote list->vector) #{x\ 3763}#)))))
                 (#{gen-append\ 3758}#
                   (lambda (#{x\ 3764}# #{y\ 3765}#)
                     (if (equal? #{y\ 3765}# (quote (quote ())))
                       #{x\ 3764}#
                       (list (quote append) #{x\ 3764}# #{y\ 3765}#))))
                 (#{gen-cons\ 3757}#
                   (lambda (#{x\ 3766}# #{y\ 3767}#)
                     (let ((#{atom-key\ 3768}# (car #{y\ 3767}#)))
                       (if (memv #{atom-key\ 3768}# (quote (quote)))
                         (if (eq? (car #{x\ 3766}#) (quote quote))
                           (list 'quote
                                 (cons (cadr #{x\ 3766}#) (cadr #{y\ 3767}#)))
                           (if (eq? (cadr #{y\ 3767}#) (quote ()))
                             (list (quote list) #{x\ 3766}#)
                             (list (quote cons) #{x\ 3766}# #{y\ 3767}#)))
                         (if (memv #{atom-key\ 3768}# (quote (list)))
                           (cons 'list
                                 (cons #{x\ 3766}# (cdr #{y\ 3767}#)))
                           (list (quote cons) #{x\ 3766}# #{y\ 3767}#))))))
                 (#{gen-map\ 3756}#
                   (lambda (#{e\ 3769}# #{map-env\ 3770}#)
                     (let ((#{formals\ 3771}# (map cdr #{map-env\ 3770}#))
                           (#{actuals\ 3772}#
                             (map (lambda (#{x\ 3773}#)
                                    (list (quote ref) (car #{x\ 3773}#)))
                                  #{map-env\ 3770}#)))
                       (if (eq? (car #{e\ 3769}#) (quote ref))
                         (car #{actuals\ 3772}#)
                         (if (and-map
                               (lambda (#{x\ 3774}#)
                                 (if (eq? (car #{x\ 3774}#) (quote ref))
                                   (memq (cadr #{x\ 3774}#) #{formals\ 3771}#)
                                   #f))
                               (cdr #{e\ 3769}#))
                           (cons 'map
                                 (cons (list 'primitive
                                             (car #{e\ 3769}#))
                                       (map (let ((#{r\ 3775}#
                                                    (map cons
                                                         #{formals\ 3771}#
                                                         #{actuals\ 3772}#)))
                                              (lambda (#{x\ 3776}#)
                                                (cdr (assq (cadr #{x\ 3776}#)
                                                           #{r\ 3775}#))))
                                            (cdr #{e\ 3769}#))))
                           (cons 'map
                                 (cons (list 'lambda
                                             #{formals\ 3771}#
                                             #{e\ 3769}#)
                                       #{actuals\ 3772}#)))))))
                 (#{gen-mappend\ 3755}#
                   (lambda (#{e\ 3777}# #{map-env\ 3778}#)
                     (list 'apply
                           '(primitive append)
                           (#{gen-map\ 3756}# #{e\ 3777}# #{map-env\ 3778}#))))
                 (#{gen-ref\ 3754}#
                   (lambda (#{src\ 3779}#
                            #{var\ 3780}#
                            #{level\ 3781}#
                            #{maps\ 3782}#)
                     (if (#{fx=\ 2638}# #{level\ 3781}# 0)
                       (values #{var\ 3780}# #{maps\ 3782}#)
                       (if (null? #{maps\ 3782}#)
                         (syntax-violation
                           'syntax
                           "missing ellipsis"
                           #{src\ 3779}#)
                         (call-with-values
                           (lambda ()
                             (#{gen-ref\ 3754}#
                               #{src\ 3779}#
                               #{var\ 3780}#
                               (#{fx-\ 2637}# #{level\ 3781}# 1)
                               (cdr #{maps\ 3782}#)))
                           (lambda (#{outer-var\ 3783}# #{outer-maps\ 3784}#)
                             (let ((#{b\ 3785}#
                                     (assq #{outer-var\ 3783}#
                                           (car #{maps\ 3782}#))))
                               (if #{b\ 3785}#
                                 (values (cdr #{b\ 3785}#) #{maps\ 3782}#)
                                 (let ((#{inner-var\ 3786}#
                                         (#{gen-var\ 2731}# (quote tmp))))
                                   (values
                                     #{inner-var\ 3786}#
                                     (cons (cons (cons #{outer-var\ 3783}#
                                                       #{inner-var\ 3786}#)
                                                 (car #{maps\ 3782}#))
                                           #{outer-maps\ 3784}#)))))))))))
                 (#{gen-syntax\ 3753}#
                   (lambda (#{src\ 3787}#
                            #{e\ 3788}#
                            #{r\ 3789}#
                            #{maps\ 3790}#
                            #{ellipsis?\ 3791}#
                            #{mod\ 3792}#)
                     (if (#{id?\ 2681}# #{e\ 3788}#)
                       (let ((#{label\ 3793}#
                               (#{id-var-name\ 2703}#
                                 #{e\ 3788}#
                                 '(()))))
                         (let ((#{b\ 3794}#
                                 (#{lookup\ 2678}#
                                   #{label\ 3793}#
                                   #{r\ 3789}#
                                   #{mod\ 3792}#)))
                           (if (eq? (#{binding-type\ 2673}# #{b\ 3794}#)
                                    'syntax)
                             (call-with-values
                               (lambda ()
                                 (let ((#{var.lev\ 3795}#
                                         (#{binding-value\ 2674}#
                                           #{b\ 3794}#)))
                                   (#{gen-ref\ 3754}#
                                     #{src\ 3787}#
                                     (car #{var.lev\ 3795}#)
                                     (cdr #{var.lev\ 3795}#)
                                     #{maps\ 3790}#)))
                               (lambda (#{var\ 3796}# #{maps\ 3797}#)
                                 (values
                                   (list (quote ref) #{var\ 3796}#)
                                   #{maps\ 3797}#)))
                             (if (#{ellipsis?\ 3791}# #{e\ 3788}#)
                               (syntax-violation
                                 'syntax
                                 "misplaced ellipsis"
                                 #{src\ 3787}#)
                               (values
                                 (list (quote quote) #{e\ 3788}#)
                                 #{maps\ 3790}#)))))
                       ((lambda (#{tmp\ 3798}#)
                          ((lambda (#{tmp\ 3799}#)
                             (if (if #{tmp\ 3799}#
                                   (apply (lambda (#{dots\ 3800}# #{e\ 3801}#)
                                            (#{ellipsis?\ 3791}#
                                              #{dots\ 3800}#))
                                          #{tmp\ 3799}#)
                                   #f)
                               (apply (lambda (#{dots\ 3802}# #{e\ 3803}#)
                                        (#{gen-syntax\ 3753}#
                                          #{src\ 3787}#
                                          #{e\ 3803}#
                                          #{r\ 3789}#
                                          #{maps\ 3790}#
                                          (lambda (#{x\ 3804}#) #f)
                                          #{mod\ 3792}#))
                                      #{tmp\ 3799}#)
                               ((lambda (#{tmp\ 3805}#)
                                  (if (if #{tmp\ 3805}#
                                        (apply (lambda (#{x\ 3806}#
                                                        #{dots\ 3807}#
                                                        #{y\ 3808}#)
                                                 (#{ellipsis?\ 3791}#
                                                   #{dots\ 3807}#))
                                               #{tmp\ 3805}#)
                                        #f)
                                    (apply (lambda (#{x\ 3809}#
                                                    #{dots\ 3810}#
                                                    #{y\ 3811}#)
                                             (letrec ((#{f\ 3812}#
                                                        (lambda (#{y\ 3813}#
                                                                 #{k\ 3814}#)
                                                          ((lambda (#{tmp\ 3818}#)
                                                             ((lambda (#{tmp\ 3819}#)
                                                                (if (if #{tmp\ 3819}#
                                                                      (apply (lambda (#{dots\ 3820}#
                                                                                      #{y\ 3821}#)
                                                                               (#{ellipsis?\ 3791}#
                                                                                 #{dots\ 3820}#))
                                                                             #{tmp\ 3819}#)
                                                                      #f)
                                                                  (apply (lambda (#{dots\ 3822}#
                                                                                  #{y\ 3823}#)
                                                                           (#{f\ 3812}#
                                                                             #{y\ 3823}#
                                                                             (lambda (#{maps\ 3824}#)
                                                                               (call-with-values
                                                                                 (lambda ()
                                                                                   (#{k\ 3814}#
                                                                                     (cons '()
                                                                                           #{maps\ 3824}#)))
                                                                                 (lambda (#{x\ 3825}#
                                                                                          #{maps\ 3826}#)
                                                                                   (if (null? (car #{maps\ 3826}#))
                                                                                     (syntax-violation
                                                                                       'syntax
                                                                                       "extra ellipsis"
                                                                                       #{src\ 3787}#)
                                                                                     (values
                                                                                       (#{gen-mappend\ 3755}#
                                                                                         #{x\ 3825}#
                                                                                         (car #{maps\ 3826}#))
                                                                                       (cdr #{maps\ 3826}#))))))))
                                                                         #{tmp\ 3819}#)
                                                                  ((lambda (#{_\ 3827}#)
                                                                     (call-with-values
                                                                       (lambda ()
                                                                         (#{gen-syntax\ 3753}#
                                                                           #{src\ 3787}#
                                                                           #{y\ 3813}#
                                                                           #{r\ 3789}#
                                                                           #{maps\ 3790}#
                                                                           #{ellipsis?\ 3791}#
                                                                           #{mod\ 3792}#))
                                                                       (lambda (#{y\ 3828}#
                                                                                #{maps\ 3829}#)
                                                                         (call-with-values
                                                                           (lambda ()
                                                                             (#{k\ 3814}#
                                                                               #{maps\ 3829}#))
                                                                           (lambda (#{x\ 3830}#
                                                                                    #{maps\ 3831}#)
                                                                             (values
                                                                               (#{gen-append\ 3758}#
                                                                                 #{x\ 3830}#
                                                                                 #{y\ 3828}#)
                                                                               #{maps\ 3831}#))))))
                                                                   #{tmp\ 3818}#)))
                                                              ($sc-dispatch
                                                                #{tmp\ 3818}#
                                                                '(any . any))))
                                                           #{y\ 3813}#))))
                                               (#{f\ 3812}#
                                                 #{y\ 3811}#
                                                 (lambda (#{maps\ 3815}#)
                                                   (call-with-values
                                                     (lambda ()
                                                       (#{gen-syntax\ 3753}#
                                                         #{src\ 3787}#
                                                         #{x\ 3809}#
                                                         #{r\ 3789}#
                                                         (cons '()
                                                               #{maps\ 3815}#)
                                                         #{ellipsis?\ 3791}#
                                                         #{mod\ 3792}#))
                                                     (lambda (#{x\ 3816}#
                                                              #{maps\ 3817}#)
                                                       (if (null? (car #{maps\ 3817}#))
                                                         (syntax-violation
                                                           'syntax
                                                           "extra ellipsis"
                                                           #{src\ 3787}#)
                                                         (values
                                                           (#{gen-map\ 3756}#
                                                             #{x\ 3816}#
                                                             (car #{maps\ 3817}#))
                                                           (cdr #{maps\ 3817}#)))))))))
                                           #{tmp\ 3805}#)
                                    ((lambda (#{tmp\ 3832}#)
                                       (if #{tmp\ 3832}#
                                         (apply (lambda (#{x\ 3833}#
                                                         #{y\ 3834}#)
                                                  (call-with-values
                                                    (lambda ()
                                                      (#{gen-syntax\ 3753}#
                                                        #{src\ 3787}#
                                                        #{x\ 3833}#
                                                        #{r\ 3789}#
                                                        #{maps\ 3790}#
                                                        #{ellipsis?\ 3791}#
                                                        #{mod\ 3792}#))
                                                    (lambda (#{x\ 3835}#
                                                             #{maps\ 3836}#)
                                                      (call-with-values
                                                        (lambda ()
                                                          (#{gen-syntax\ 3753}#
                                                            #{src\ 3787}#
                                                            #{y\ 3834}#
                                                            #{r\ 3789}#
                                                            #{maps\ 3836}#
                                                            #{ellipsis?\ 3791}#
                                                            #{mod\ 3792}#))
                                                        (lambda (#{y\ 3837}#
                                                                 #{maps\ 3838}#)
                                                          (values
                                                            (#{gen-cons\ 3757}#
                                                              #{x\ 3835}#
                                                              #{y\ 3837}#)
                                                            #{maps\ 3838}#))))))
                                                #{tmp\ 3832}#)
                                         ((lambda (#{tmp\ 3839}#)
                                            (if #{tmp\ 3839}#
                                              (apply (lambda (#{e1\ 3840}#
                                                              #{e2\ 3841}#)
                                                       (call-with-values
                                                         (lambda ()
                                                           (#{gen-syntax\ 3753}#
                                                             #{src\ 3787}#
                                                             (cons #{e1\ 3840}#
                                                                   #{e2\ 3841}#)
                                                             #{r\ 3789}#
                                                             #{maps\ 3790}#
                                                             #{ellipsis?\ 3791}#
                                                             #{mod\ 3792}#))
                                                         (lambda (#{e\ 3843}#
                                                                  #{maps\ 3844}#)
                                                           (values
                                                             (#{gen-vector\ 3759}#
                                                               #{e\ 3843}#)
                                                             #{maps\ 3844}#))))
                                                     #{tmp\ 3839}#)
                                              ((lambda (#{_\ 3845}#)
                                                 (values
                                                   (list 'quote
                                                         #{e\ 3788}#)
                                                   #{maps\ 3790}#))
                                               #{tmp\ 3798}#)))
                                          ($sc-dispatch
                                            #{tmp\ 3798}#
                                            '#(vector (any . each-any))))))
                                     ($sc-dispatch
                                       #{tmp\ 3798}#
                                       '(any . any)))))
                                ($sc-dispatch
                                  #{tmp\ 3798}#
                                  '(any any . any)))))
                           ($sc-dispatch #{tmp\ 3798}# (quote (any any)))))
                        #{e\ 3788}#)))))
          (lambda (#{e\ 3846}#
                   #{r\ 3847}#
                   #{w\ 3848}#
                   #{s\ 3849}#
                   #{mod\ 3850}#)
            (let ((#{e\ 3851}#
                    (#{source-wrap\ 2710}#
                      #{e\ 3846}#
                      #{w\ 3848}#
                      #{s\ 3849}#
                      #{mod\ 3850}#)))
              ((lambda (#{tmp\ 3852}#)
                 ((lambda (#{tmp\ 3853}#)
                    (if #{tmp\ 3853}#
                      (apply (lambda (#{_\ 3854}# #{x\ 3855}#)
                               (call-with-values
                                 (lambda ()
                                   (#{gen-syntax\ 3753}#
                                     #{e\ 3851}#
                                     #{x\ 3855}#
                                     #{r\ 3847}#
                                     '()
                                     #{ellipsis?\ 2725}#
                                     #{mod\ 3850}#))
                                 (lambda (#{e\ 3856}# #{maps\ 3857}#)
                                   (#{regen\ 3760}# #{e\ 3856}#))))
                             #{tmp\ 3853}#)
                      ((lambda (#{_\ 3858}#)
                         (syntax-violation
                           'syntax
                           "bad `syntax' form"
                           #{e\ 3851}#))
                       #{tmp\ 3852}#)))
                  ($sc-dispatch #{tmp\ 3852}# (quote (any any)))))
               #{e\ 3851}#)))))
      (#{global-extend\ 2679}#
        'core
        'lambda
        (lambda (#{e\ 3859}#
                 #{r\ 3860}#
                 #{w\ 3861}#
                 #{s\ 3862}#
                 #{mod\ 3863}#)
          ((lambda (#{tmp\ 3864}#)
             ((lambda (#{tmp\ 3865}#)
                (if (if #{tmp\ 3865}#
                      (apply (lambda (#{_\ 3866}#
                                      #{args\ 3867}#
                                      #{docstring\ 3868}#
                                      #{e1\ 3869}#
                                      #{e2\ 3870}#)
                               (string? (syntax->datum #{docstring\ 3868}#)))
                             #{tmp\ 3865}#)
                      #f)
                  (apply (lambda (#{_\ 3871}#
                                  #{args\ 3872}#
                                  #{docstring\ 3873}#
                                  #{e1\ 3874}#
                                  #{e2\ 3875}#)
                           (call-with-values
                             (lambda ()
                               (#{lambda-formals\ 2726}# #{args\ 3872}#))
                             (lambda (#{req\ 3876}#
                                      #{opt\ 3877}#
                                      #{rest\ 3878}#
                                      #{kw\ 3879}#)
                               (#{chi-simple-lambda\ 2727}#
                                 #{e\ 3859}#
                                 #{r\ 3860}#
                                 #{w\ 3861}#
                                 #{s\ 3862}#
                                 #{mod\ 3863}#
                                 #{req\ 3876}#
                                 #{rest\ 3878}#
                                 (syntax->datum #{docstring\ 3873}#)
                                 (cons #{e1\ 3874}# #{e2\ 3875}#)))))
                         #{tmp\ 3865}#)
                  ((lambda (#{tmp\ 3881}#)
                     (if #{tmp\ 3881}#
                       (apply (lambda (#{_\ 3882}#
                                       #{args\ 3883}#
                                       #{e1\ 3884}#
                                       #{e2\ 3885}#)
                                (call-with-values
                                  (lambda ()
                                    (#{lambda-formals\ 2726}# #{args\ 3883}#))
                                  (lambda (#{req\ 3886}#
                                           #{opt\ 3887}#
                                           #{rest\ 3888}#
                                           #{kw\ 3889}#)
                                    (#{chi-simple-lambda\ 2727}#
                                      #{e\ 3859}#
                                      #{r\ 3860}#
                                      #{w\ 3861}#
                                      #{s\ 3862}#
                                      #{mod\ 3863}#
                                      #{req\ 3886}#
                                      #{rest\ 3888}#
                                      #f
                                      (cons #{e1\ 3884}# #{e2\ 3885}#)))))
                              #{tmp\ 3881}#)
                       ((lambda (#{_\ 3891}#)
                          (syntax-violation
                            'lambda
                            "bad lambda"
                            #{e\ 3859}#))
                        #{tmp\ 3864}#)))
                   ($sc-dispatch
                     #{tmp\ 3864}#
                     '(any any any . each-any)))))
              ($sc-dispatch
                #{tmp\ 3864}#
                '(any any any any . each-any))))
           #{e\ 3859}#)))
      (#{global-extend\ 2679}#
        'core
        'lambda*
        (lambda (#{e\ 3892}#
                 #{r\ 3893}#
                 #{w\ 3894}#
                 #{s\ 3895}#
                 #{mod\ 3896}#)
          ((lambda (#{tmp\ 3897}#)
             ((lambda (#{tmp\ 3898}#)
                (if #{tmp\ 3898}#
                  (apply (lambda (#{_\ 3899}#
                                  #{args\ 3900}#
                                  #{e1\ 3901}#
                                  #{e2\ 3902}#)
                           (call-with-values
                             (lambda ()
                               (#{chi-lambda-case\ 2729}#
                                 #{e\ 3892}#
                                 #{r\ 3893}#
                                 #{w\ 3894}#
                                 #{s\ 3895}#
                                 #{mod\ 3896}#
                                 #{lambda*-formals\ 2728}#
                                 (list (cons #{args\ 3900}#
                                             (cons #{e1\ 3901}#
                                                   #{e2\ 3902}#)))))
                             (lambda (#{docstring\ 3904}# #{lcase\ 3905}#)
                               (#{build-case-lambda\ 2656}#
                                 #{s\ 3895}#
                                 #{docstring\ 3904}#
                                 #{lcase\ 3905}#))))
                         #{tmp\ 3898}#)
                  ((lambda (#{_\ 3906}#)
                     (syntax-violation
                       'lambda
                       "bad lambda*"
                       #{e\ 3892}#))
                   #{tmp\ 3897}#)))
              ($sc-dispatch
                #{tmp\ 3897}#
                '(any any any . each-any))))
           #{e\ 3892}#)))
      (#{global-extend\ 2679}#
        'core
        'case-lambda
        (lambda (#{e\ 3907}#
                 #{r\ 3908}#
                 #{w\ 3909}#
                 #{s\ 3910}#
                 #{mod\ 3911}#)
          ((lambda (#{tmp\ 3912}#)
             ((lambda (#{tmp\ 3913}#)
                (if #{tmp\ 3913}#
                  (apply (lambda (#{_\ 3914}#
                                  #{args\ 3915}#
                                  #{e1\ 3916}#
                                  #{e2\ 3917}#
                                  #{args*\ 3918}#
                                  #{e1*\ 3919}#
                                  #{e2*\ 3920}#)
                           (call-with-values
                             (lambda ()
                               (#{chi-lambda-case\ 2729}#
                                 #{e\ 3907}#
                                 #{r\ 3908}#
                                 #{w\ 3909}#
                                 #{s\ 3910}#
                                 #{mod\ 3911}#
                                 #{lambda-formals\ 2726}#
                                 (cons (cons #{args\ 3915}#
                                             (cons #{e1\ 3916}# #{e2\ 3917}#))
                                       (map (lambda (#{tmp\ 3924}#
                                                     #{tmp\ 3923}#
                                                     #{tmp\ 3922}#)
                                              (cons #{tmp\ 3922}#
                                                    (cons #{tmp\ 3923}#
                                                          #{tmp\ 3924}#)))
                                            #{e2*\ 3920}#
                                            #{e1*\ 3919}#
                                            #{args*\ 3918}#))))
                             (lambda (#{docstring\ 3926}# #{lcase\ 3927}#)
                               (#{build-case-lambda\ 2656}#
                                 #{s\ 3910}#
                                 #{docstring\ 3926}#
                                 #{lcase\ 3927}#))))
                         #{tmp\ 3913}#)
                  ((lambda (#{_\ 3928}#)
                     (syntax-violation
                       'case-lambda
                       "bad case-lambda"
                       #{e\ 3907}#))
                   #{tmp\ 3912}#)))
              ($sc-dispatch
                #{tmp\ 3912}#
                '(any (any any . each-any)
                      .
                      #(each (any any . each-any))))))
           #{e\ 3907}#)))
      (#{global-extend\ 2679}#
        'core
        'case-lambda*
        (lambda (#{e\ 3929}#
                 #{r\ 3930}#
                 #{w\ 3931}#
                 #{s\ 3932}#
                 #{mod\ 3933}#)
          ((lambda (#{tmp\ 3934}#)
             ((lambda (#{tmp\ 3935}#)
                (if #{tmp\ 3935}#
                  (apply (lambda (#{_\ 3936}#
                                  #{args\ 3937}#
                                  #{e1\ 3938}#
                                  #{e2\ 3939}#
                                  #{args*\ 3940}#
                                  #{e1*\ 3941}#
                                  #{e2*\ 3942}#)
                           (call-with-values
                             (lambda ()
                               (#{chi-lambda-case\ 2729}#
                                 #{e\ 3929}#
                                 #{r\ 3930}#
                                 #{w\ 3931}#
                                 #{s\ 3932}#
                                 #{mod\ 3933}#
                                 #{lambda*-formals\ 2728}#
                                 (cons (cons #{args\ 3937}#
                                             (cons #{e1\ 3938}# #{e2\ 3939}#))
                                       (map (lambda (#{tmp\ 3946}#
                                                     #{tmp\ 3945}#
                                                     #{tmp\ 3944}#)
                                              (cons #{tmp\ 3944}#
                                                    (cons #{tmp\ 3945}#
                                                          #{tmp\ 3946}#)))
                                            #{e2*\ 3942}#
                                            #{e1*\ 3941}#
                                            #{args*\ 3940}#))))
                             (lambda (#{docstring\ 3948}# #{lcase\ 3949}#)
                               (#{build-case-lambda\ 2656}#
                                 #{s\ 3932}#
                                 #{docstring\ 3948}#
                                 #{lcase\ 3949}#))))
                         #{tmp\ 3935}#)
                  ((lambda (#{_\ 3950}#)
                     (syntax-violation
                       'case-lambda
                       "bad case-lambda*"
                       #{e\ 3929}#))
                   #{tmp\ 3934}#)))
              ($sc-dispatch
                #{tmp\ 3934}#
                '(any (any any . each-any)
                      .
                      #(each (any any . each-any))))))
           #{e\ 3929}#)))
      (#{global-extend\ 2679}#
        'core
        'let
        (letrec ((#{chi-let\ 3951}#
                   (lambda (#{e\ 3952}#
                            #{r\ 3953}#
                            #{w\ 3954}#
                            #{s\ 3955}#
                            #{mod\ 3956}#
                            #{constructor\ 3957}#
                            #{ids\ 3958}#
                            #{vals\ 3959}#
                            #{exps\ 3960}#)
                     (if (not (#{valid-bound-ids?\ 2706}# #{ids\ 3958}#))
                       (syntax-violation
                         'let
                         "duplicate bound variable"
                         #{e\ 3952}#)
                       (let ((#{labels\ 3961}#
                               (#{gen-labels\ 2687}# #{ids\ 3958}#))
                             (#{new-vars\ 3962}#
                               (map #{gen-var\ 2731}# #{ids\ 3958}#)))
                         (let ((#{nw\ 3963}#
                                 (#{make-binding-wrap\ 2698}#
                                   #{ids\ 3958}#
                                   #{labels\ 3961}#
                                   #{w\ 3954}#))
                               (#{nr\ 3964}#
                                 (#{extend-var-env\ 2676}#
                                   #{labels\ 3961}#
                                   #{new-vars\ 3962}#
                                   #{r\ 3953}#)))
                           (#{constructor\ 3957}#
                             #{s\ 3955}#
                             (map syntax->datum #{ids\ 3958}#)
                             #{new-vars\ 3962}#
                             (map (lambda (#{x\ 3965}#)
                                    (#{chi\ 2717}#
                                      #{x\ 3965}#
                                      #{r\ 3953}#
                                      #{w\ 3954}#
                                      #{mod\ 3956}#))
                                  #{vals\ 3959}#)
                             (#{chi-body\ 2721}#
                               #{exps\ 3960}#
                               (#{source-wrap\ 2710}#
                                 #{e\ 3952}#
                                 #{nw\ 3963}#
                                 #{s\ 3955}#
                                 #{mod\ 3956}#)
                               #{nr\ 3964}#
                               #{nw\ 3963}#
                               #{mod\ 3956}#))))))))
          (lambda (#{e\ 3966}#
                   #{r\ 3967}#
                   #{w\ 3968}#
                   #{s\ 3969}#
                   #{mod\ 3970}#)
            ((lambda (#{tmp\ 3971}#)
               ((lambda (#{tmp\ 3972}#)
                  (if (if #{tmp\ 3972}#
                        (apply (lambda (#{_\ 3973}#
                                        #{id\ 3974}#
                                        #{val\ 3975}#
                                        #{e1\ 3976}#
                                        #{e2\ 3977}#)
                                 (and-map #{id?\ 2681}# #{id\ 3974}#))
                               #{tmp\ 3972}#)
                        #f)
                    (apply (lambda (#{_\ 3979}#
                                    #{id\ 3980}#
                                    #{val\ 3981}#
                                    #{e1\ 3982}#
                                    #{e2\ 3983}#)
                             (#{chi-let\ 3951}#
                               #{e\ 3966}#
                               #{r\ 3967}#
                               #{w\ 3968}#
                               #{s\ 3969}#
                               #{mod\ 3970}#
                               #{build-let\ 2661}#
                               #{id\ 3980}#
                               #{val\ 3981}#
                               (cons #{e1\ 3982}# #{e2\ 3983}#)))
                           #{tmp\ 3972}#)
                    ((lambda (#{tmp\ 3987}#)
                       (if (if #{tmp\ 3987}#
                             (apply (lambda (#{_\ 3988}#
                                             #{f\ 3989}#
                                             #{id\ 3990}#
                                             #{val\ 3991}#
                                             #{e1\ 3992}#
                                             #{e2\ 3993}#)
                                      (if (#{id?\ 2681}# #{f\ 3989}#)
                                        (and-map #{id?\ 2681}# #{id\ 3990}#)
                                        #f))
                                    #{tmp\ 3987}#)
                             #f)
                         (apply (lambda (#{_\ 3995}#
                                         #{f\ 3996}#
                                         #{id\ 3997}#
                                         #{val\ 3998}#
                                         #{e1\ 3999}#
                                         #{e2\ 4000}#)
                                  (#{chi-let\ 3951}#
                                    #{e\ 3966}#
                                    #{r\ 3967}#
                                    #{w\ 3968}#
                                    #{s\ 3969}#
                                    #{mod\ 3970}#
                                    #{build-named-let\ 2662}#
                                    (cons #{f\ 3996}# #{id\ 3997}#)
                                    #{val\ 3998}#
                                    (cons #{e1\ 3999}# #{e2\ 4000}#)))
                                #{tmp\ 3987}#)
                         ((lambda (#{_\ 4004}#)
                            (syntax-violation
                              'let
                              "bad let"
                              (#{source-wrap\ 2710}#
                                #{e\ 3966}#
                                #{w\ 3968}#
                                #{s\ 3969}#
                                #{mod\ 3970}#)))
                          #{tmp\ 3971}#)))
                     ($sc-dispatch
                       #{tmp\ 3971}#
                       '(any any #(each (any any)) any . each-any)))))
                ($sc-dispatch
                  #{tmp\ 3971}#
                  '(any #(each (any any)) any . each-any))))
             #{e\ 3966}#))))
      (#{global-extend\ 2679}#
        'core
        'letrec
        (lambda (#{e\ 4005}#
                 #{r\ 4006}#
                 #{w\ 4007}#
                 #{s\ 4008}#
                 #{mod\ 4009}#)
          ((lambda (#{tmp\ 4010}#)
             ((lambda (#{tmp\ 4011}#)
                (if (if #{tmp\ 4011}#
                      (apply (lambda (#{_\ 4012}#
                                      #{id\ 4013}#
                                      #{val\ 4014}#
                                      #{e1\ 4015}#
                                      #{e2\ 4016}#)
                               (and-map #{id?\ 2681}# #{id\ 4013}#))
                             #{tmp\ 4011}#)
                      #f)
                  (apply (lambda (#{_\ 4018}#
                                  #{id\ 4019}#
                                  #{val\ 4020}#
                                  #{e1\ 4021}#
                                  #{e2\ 4022}#)
                           (let ((#{ids\ 4023}# #{id\ 4019}#))
                             (if (not (#{valid-bound-ids?\ 2706}#
                                        #{ids\ 4023}#))
                               (syntax-violation
                                 'letrec
                                 "duplicate bound variable"
                                 #{e\ 4005}#)
                               (let ((#{labels\ 4025}#
                                       (#{gen-labels\ 2687}# #{ids\ 4023}#))
                                     (#{new-vars\ 4026}#
                                       (map #{gen-var\ 2731}# #{ids\ 4023}#)))
                                 (let ((#{w\ 4027}#
                                         (#{make-binding-wrap\ 2698}#
                                           #{ids\ 4023}#
                                           #{labels\ 4025}#
                                           #{w\ 4007}#))
                                       (#{r\ 4028}#
                                         (#{extend-var-env\ 2676}#
                                           #{labels\ 4025}#
                                           #{new-vars\ 4026}#
                                           #{r\ 4006}#)))
                                   (#{build-letrec\ 2663}#
                                     #{s\ 4008}#
                                     (map syntax->datum #{ids\ 4023}#)
                                     #{new-vars\ 4026}#
                                     (map (lambda (#{x\ 4029}#)
                                            (#{chi\ 2717}#
                                              #{x\ 4029}#
                                              #{r\ 4028}#
                                              #{w\ 4027}#
                                              #{mod\ 4009}#))
                                          #{val\ 4020}#)
                                     (#{chi-body\ 2721}#
                                       (cons #{e1\ 4021}# #{e2\ 4022}#)
                                       (#{source-wrap\ 2710}#
                                         #{e\ 4005}#
                                         #{w\ 4027}#
                                         #{s\ 4008}#
                                         #{mod\ 4009}#)
                                       #{r\ 4028}#
                                       #{w\ 4027}#
                                       #{mod\ 4009}#)))))))
                         #{tmp\ 4011}#)
                  ((lambda (#{_\ 4032}#)
                     (syntax-violation
                       'letrec
                       "bad letrec"
                       (#{source-wrap\ 2710}#
                         #{e\ 4005}#
                         #{w\ 4007}#
                         #{s\ 4008}#
                         #{mod\ 4009}#)))
                   #{tmp\ 4010}#)))
              ($sc-dispatch
                #{tmp\ 4010}#
                '(any #(each (any any)) any . each-any))))
           #{e\ 4005}#)))
      (#{global-extend\ 2679}#
        'core
        'set!
        (lambda (#{e\ 4033}#
                 #{r\ 4034}#
                 #{w\ 4035}#
                 #{s\ 4036}#
                 #{mod\ 4037}#)
          ((lambda (#{tmp\ 4038}#)
             ((lambda (#{tmp\ 4039}#)
                (if (if #{tmp\ 4039}#
                      (apply (lambda (#{_\ 4040}# #{id\ 4041}# #{val\ 4042}#)
                               (#{id?\ 2681}# #{id\ 4041}#))
                             #{tmp\ 4039}#)
                      #f)
                  (apply (lambda (#{_\ 4043}# #{id\ 4044}# #{val\ 4045}#)
                           (let ((#{val\ 4046}#
                                   (#{chi\ 2717}#
                                     #{val\ 4045}#
                                     #{r\ 4034}#
                                     #{w\ 4035}#
                                     #{mod\ 4037}#))
                                 (#{n\ 4047}#
                                   (#{id-var-name\ 2703}#
                                     #{id\ 4044}#
                                     #{w\ 4035}#)))
                             (let ((#{b\ 4048}#
                                     (#{lookup\ 2678}#
                                       #{n\ 4047}#
                                       #{r\ 4034}#
                                       #{mod\ 4037}#)))
                               (let ((#{atom-key\ 4049}#
                                       (#{binding-type\ 2673}# #{b\ 4048}#)))
                                 (if (memv #{atom-key\ 4049}#
                                           '(lexical))
                                   (#{build-lexical-assignment\ 2649}#
                                     #{s\ 4036}#
                                     (syntax->datum #{id\ 4044}#)
                                     (#{binding-value\ 2674}# #{b\ 4048}#)
                                     #{val\ 4046}#)
                                   (if (memv #{atom-key\ 4049}#
                                             '(global))
                                     (#{build-global-assignment\ 2652}#
                                       #{s\ 4036}#
                                       #{n\ 4047}#
                                       #{val\ 4046}#
                                       #{mod\ 4037}#)
                                     (if (memv #{atom-key\ 4049}#
                                               '(displaced-lexical))
                                       (syntax-violation
                                         'set!
                                         "identifier out of context"
                                         (#{wrap\ 2709}#
                                           #{id\ 4044}#
                                           #{w\ 4035}#
                                           #{mod\ 4037}#))
                                       (syntax-violation
                                         'set!
                                         "bad set!"
                                         (#{source-wrap\ 2710}#
                                           #{e\ 4033}#
                                           #{w\ 4035}#
                                           #{s\ 4036}#
                                           #{mod\ 4037}#)))))))))
                         #{tmp\ 4039}#)
                  ((lambda (#{tmp\ 4050}#)
                     (if #{tmp\ 4050}#
                       (apply (lambda (#{_\ 4051}#
                                       #{head\ 4052}#
                                       #{tail\ 4053}#
                                       #{val\ 4054}#)
                                (call-with-values
                                  (lambda ()
                                    (#{syntax-type\ 2715}#
                                      #{head\ 4052}#
                                      #{r\ 4034}#
                                      '(())
                                      #f
                                      #f
                                      #{mod\ 4037}#
                                      #t))
                                  (lambda (#{type\ 4055}#
                                           #{value\ 4056}#
                                           #{ee\ 4057}#
                                           #{ww\ 4058}#
                                           #{ss\ 4059}#
                                           #{modmod\ 4060}#)
                                    (if (memv #{type\ 4055}#
                                              '(module-ref))
                                      (let ((#{val\ 4061}#
                                              (#{chi\ 2717}#
                                                #{val\ 4054}#
                                                #{r\ 4034}#
                                                #{w\ 4035}#
                                                #{mod\ 4037}#)))
                                        (call-with-values
                                          (lambda ()
                                            (#{value\ 4056}#
                                              (cons #{head\ 4052}#
                                                    #{tail\ 4053}#)))
                                          (lambda (#{id\ 4063}# #{mod\ 4064}#)
                                            (#{build-global-assignment\ 2652}#
                                              #{s\ 4036}#
                                              #{id\ 4063}#
                                              #{val\ 4061}#
                                              #{mod\ 4064}#))))
                                      (#{build-application\ 2646}#
                                        #{s\ 4036}#
                                        (#{chi\ 2717}#
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
                                                        chi-lambda-case
                                                        lambda*-formals
                                                        chi-simple-lambda
                                                        lambda-formals
                                                        ellipsis?
                                                        chi-void
                                                        eval-local-transformer
                                                        chi-local-syntax
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
                                                        build-lambda-case
                                                        build-case-lambda
                                                        build-simple-lambda
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
                                                #{head\ 4052}#)
                                          #{r\ 4034}#
                                          #{w\ 4035}#
                                          #{mod\ 4037}#)
                                        (map (lambda (#{e\ 4065}#)
                                               (#{chi\ 2717}#
                                                 #{e\ 4065}#
                                                 #{r\ 4034}#
                                                 #{w\ 4035}#
                                                 #{mod\ 4037}#))
                                             (append
                                               #{tail\ 4053}#
                                               (list #{val\ 4054}#))))))))
                              #{tmp\ 4050}#)
                       ((lambda (#{_\ 4067}#)
                          (syntax-violation
                            'set!
                            "bad set!"
                            (#{source-wrap\ 2710}#
                              #{e\ 4033}#
                              #{w\ 4035}#
                              #{s\ 4036}#
                              #{mod\ 4037}#)))
                        #{tmp\ 4038}#)))
                   ($sc-dispatch
                     #{tmp\ 4038}#
                     '(any (any . each-any) any)))))
              ($sc-dispatch
                #{tmp\ 4038}#
                '(any any any))))
           #{e\ 4033}#)))
      (#{global-extend\ 2679}#
        'module-ref
        '@
        (lambda (#{e\ 4068}#)
          ((lambda (#{tmp\ 4069}#)
             ((lambda (#{tmp\ 4070}#)
                (if (if #{tmp\ 4070}#
                      (apply (lambda (#{_\ 4071}# #{mod\ 4072}# #{id\ 4073}#)
                               (if (and-map #{id?\ 2681}# #{mod\ 4072}#)
                                 (#{id?\ 2681}# #{id\ 4073}#)
                                 #f))
                             #{tmp\ 4070}#)
                      #f)
                  (apply (lambda (#{_\ 4075}# #{mod\ 4076}# #{id\ 4077}#)
                           (values
                             (syntax->datum #{id\ 4077}#)
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
                                             chi-lambda-case
                                             lambda*-formals
                                             chi-simple-lambda
                                             lambda-formals
                                             ellipsis?
                                             chi-void
                                             eval-local-transformer
                                             chi-local-syntax
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
                                             build-lambda-case
                                             build-case-lambda
                                             build-simple-lambda
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
                                     #{mod\ 4076}#))))
                         #{tmp\ 4070}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp\ 4069}#)))
              ($sc-dispatch
                #{tmp\ 4069}#
                '(any each-any any))))
           #{e\ 4068}#)))
      (#{global-extend\ 2679}#
        'module-ref
        '@@
        (lambda (#{e\ 4079}#)
          ((lambda (#{tmp\ 4080}#)
             ((lambda (#{tmp\ 4081}#)
                (if (if #{tmp\ 4081}#
                      (apply (lambda (#{_\ 4082}# #{mod\ 4083}# #{id\ 4084}#)
                               (if (and-map #{id?\ 2681}# #{mod\ 4083}#)
                                 (#{id?\ 2681}# #{id\ 4084}#)
                                 #f))
                             #{tmp\ 4081}#)
                      #f)
                  (apply (lambda (#{_\ 4086}# #{mod\ 4087}# #{id\ 4088}#)
                           (values
                             (syntax->datum #{id\ 4088}#)
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
                                             chi-lambda-case
                                             lambda*-formals
                                             chi-simple-lambda
                                             lambda-formals
                                             ellipsis?
                                             chi-void
                                             eval-local-transformer
                                             chi-local-syntax
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
                                             build-lambda-case
                                             build-case-lambda
                                             build-simple-lambda
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
                                     #{mod\ 4087}#))))
                         #{tmp\ 4081}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp\ 4080}#)))
              ($sc-dispatch
                #{tmp\ 4080}#
                '(any each-any any))))
           #{e\ 4079}#)))
      (#{global-extend\ 2679}#
        'core
        'if
        (lambda (#{e\ 4090}#
                 #{r\ 4091}#
                 #{w\ 4092}#
                 #{s\ 4093}#
                 #{mod\ 4094}#)
          ((lambda (#{tmp\ 4095}#)
             ((lambda (#{tmp\ 4096}#)
                (if #{tmp\ 4096}#
                  (apply (lambda (#{_\ 4097}# #{test\ 4098}# #{then\ 4099}#)
                           (#{build-conditional\ 2647}#
                             #{s\ 4093}#
                             (#{chi\ 2717}#
                               #{test\ 4098}#
                               #{r\ 4091}#
                               #{w\ 4092}#
                               #{mod\ 4094}#)
                             (#{chi\ 2717}#
                               #{then\ 4099}#
                               #{r\ 4091}#
                               #{w\ 4092}#
                               #{mod\ 4094}#)
                             (#{build-void\ 2645}# #f)))
                         #{tmp\ 4096}#)
                  ((lambda (#{tmp\ 4100}#)
                     (if #{tmp\ 4100}#
                       (apply (lambda (#{_\ 4101}#
                                       #{test\ 4102}#
                                       #{then\ 4103}#
                                       #{else\ 4104}#)
                                (#{build-conditional\ 2647}#
                                  #{s\ 4093}#
                                  (#{chi\ 2717}#
                                    #{test\ 4102}#
                                    #{r\ 4091}#
                                    #{w\ 4092}#
                                    #{mod\ 4094}#)
                                  (#{chi\ 2717}#
                                    #{then\ 4103}#
                                    #{r\ 4091}#
                                    #{w\ 4092}#
                                    #{mod\ 4094}#)
                                  (#{chi\ 2717}#
                                    #{else\ 4104}#
                                    #{r\ 4091}#
                                    #{w\ 4092}#
                                    #{mod\ 4094}#)))
                              #{tmp\ 4100}#)
                       (syntax-violation
                         #f
                         "source expression failed to match any pattern"
                         #{tmp\ 4095}#)))
                   ($sc-dispatch
                     #{tmp\ 4095}#
                     '(any any any any)))))
              ($sc-dispatch
                #{tmp\ 4095}#
                '(any any any))))
           #{e\ 4090}#)))
      (#{global-extend\ 2679}#
        'begin
        'begin
        '())
      (#{global-extend\ 2679}#
        'define
        'define
        '())
      (#{global-extend\ 2679}#
        'define-syntax
        'define-syntax
        '())
      (#{global-extend\ 2679}#
        'eval-when
        'eval-when
        '())
      (#{global-extend\ 2679}#
        'core
        'syntax-case
        (letrec ((#{gen-syntax-case\ 4108}#
                   (lambda (#{x\ 4109}#
                            #{keys\ 4110}#
                            #{clauses\ 4111}#
                            #{r\ 4112}#
                            #{mod\ 4113}#)
                     (if (null? #{clauses\ 4111}#)
                       (#{build-application\ 2646}#
                         #f
                         (#{build-primref\ 2658}#
                           #f
                           'syntax-violation)
                         (list (#{build-data\ 2659}# #f #f)
                               (#{build-data\ 2659}#
                                 #f
                                 "source expression failed to match any pattern")
                               #{x\ 4109}#))
                       ((lambda (#{tmp\ 4114}#)
                          ((lambda (#{tmp\ 4115}#)
                             (if #{tmp\ 4115}#
                               (apply (lambda (#{pat\ 4116}# #{exp\ 4117}#)
                                        (if (if (#{id?\ 2681}# #{pat\ 4116}#)
                                              (and-map
                                                (lambda (#{x\ 4118}#)
                                                  (not (#{free-id=?\ 2704}#
                                                         #{pat\ 4116}#
                                                         #{x\ 4118}#)))
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
                                                              chi-lambda-case
                                                              lambda*-formals
                                                              chi-simple-lambda
                                                              lambda-formals
                                                              ellipsis?
                                                              chi-void
                                                              eval-local-transformer
                                                              chi-local-syntax
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
                                                              build-lambda-case
                                                              build-case-lambda
                                                              build-simple-lambda
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
                                                      #{keys\ 4110}#))
                                              #f)
                                          (let ((#{labels\ 4119}#
                                                  (list (#{gen-label\ 2686}#)))
                                                (#{var\ 4120}#
                                                  (#{gen-var\ 2731}#
                                                    #{pat\ 4116}#)))
                                            (#{build-application\ 2646}#
                                              #f
                                              (#{build-simple-lambda\ 2655}#
                                                #f
                                                (list (syntax->datum
                                                        #{pat\ 4116}#))
                                                #f
                                                (list #{var\ 4120}#)
                                                #f
                                                (#{chi\ 2717}#
                                                  #{exp\ 4117}#
                                                  (#{extend-env\ 2675}#
                                                    #{labels\ 4119}#
                                                    (list (cons 'syntax
                                                                (cons #{var\ 4120}#
                                                                      0)))
                                                    #{r\ 4112}#)
                                                  (#{make-binding-wrap\ 2698}#
                                                    (list #{pat\ 4116}#)
                                                    #{labels\ 4119}#
                                                    '(()))
                                                  #{mod\ 4113}#))
                                              (list #{x\ 4109}#)))
                                          (#{gen-clause\ 4107}#
                                            #{x\ 4109}#
                                            #{keys\ 4110}#
                                            (cdr #{clauses\ 4111}#)
                                            #{r\ 4112}#
                                            #{pat\ 4116}#
                                            #t
                                            #{exp\ 4117}#
                                            #{mod\ 4113}#)))
                                      #{tmp\ 4115}#)
                               ((lambda (#{tmp\ 4121}#)
                                  (if #{tmp\ 4121}#
                                    (apply (lambda (#{pat\ 4122}#
                                                    #{fender\ 4123}#
                                                    #{exp\ 4124}#)
                                             (#{gen-clause\ 4107}#
                                               #{x\ 4109}#
                                               #{keys\ 4110}#
                                               (cdr #{clauses\ 4111}#)
                                               #{r\ 4112}#
                                               #{pat\ 4122}#
                                               #{fender\ 4123}#
                                               #{exp\ 4124}#
                                               #{mod\ 4113}#))
                                           #{tmp\ 4121}#)
                                    ((lambda (#{_\ 4125}#)
                                       (syntax-violation
                                         'syntax-case
                                         "invalid clause"
                                         (car #{clauses\ 4111}#)))
                                     #{tmp\ 4114}#)))
                                ($sc-dispatch
                                  #{tmp\ 4114}#
                                  '(any any any)))))
                           ($sc-dispatch #{tmp\ 4114}# (quote (any any)))))
                        (car #{clauses\ 4111}#)))))
                 (#{gen-clause\ 4107}#
                   (lambda (#{x\ 4126}#
                            #{keys\ 4127}#
                            #{clauses\ 4128}#
                            #{r\ 4129}#
                            #{pat\ 4130}#
                            #{fender\ 4131}#
                            #{exp\ 4132}#
                            #{mod\ 4133}#)
                     (call-with-values
                       (lambda ()
                         (#{convert-pattern\ 4105}#
                           #{pat\ 4130}#
                           #{keys\ 4127}#))
                       (lambda (#{p\ 4134}# #{pvars\ 4135}#)
                         (if (not (#{distinct-bound-ids?\ 2707}#
                                    (map car #{pvars\ 4135}#)))
                           (syntax-violation
                             'syntax-case
                             "duplicate pattern variable"
                             #{pat\ 4130}#)
                           (if (not (and-map
                                      (lambda (#{x\ 4136}#)
                                        (not (#{ellipsis?\ 2725}#
                                               (car #{x\ 4136}#))))
                                      #{pvars\ 4135}#))
                             (syntax-violation
                               'syntax-case
                               "misplaced ellipsis"
                               #{pat\ 4130}#)
                             (let ((#{y\ 4137}#
                                     (#{gen-var\ 2731}# (quote tmp))))
                               (#{build-application\ 2646}#
                                 #f
                                 (#{build-simple-lambda\ 2655}#
                                   #f
                                   (list (quote tmp))
                                   #f
                                   (list #{y\ 4137}#)
                                   #f
                                   (let ((#{y\ 4138}#
                                           (#{build-lexical-reference\ 2648}#
                                             'value
                                             #f
                                             'tmp
                                             #{y\ 4137}#)))
                                     (#{build-conditional\ 2647}#
                                       #f
                                       ((lambda (#{tmp\ 4139}#)
                                          ((lambda (#{tmp\ 4140}#)
                                             (if #{tmp\ 4140}#
                                               (apply (lambda () #{y\ 4138}#)
                                                      #{tmp\ 4140}#)
                                               ((lambda (#{_\ 4141}#)
                                                  (#{build-conditional\ 2647}#
                                                    #f
                                                    #{y\ 4138}#
                                                    (#{build-dispatch-call\ 4106}#
                                                      #{pvars\ 4135}#
                                                      #{fender\ 4131}#
                                                      #{y\ 4138}#
                                                      #{r\ 4129}#
                                                      #{mod\ 4133}#)
                                                    (#{build-data\ 2659}#
                                                      #f
                                                      #f)))
                                                #{tmp\ 4139}#)))
                                           ($sc-dispatch
                                             #{tmp\ 4139}#
                                             '#(atom #t))))
                                        #{fender\ 4131}#)
                                       (#{build-dispatch-call\ 4106}#
                                         #{pvars\ 4135}#
                                         #{exp\ 4132}#
                                         #{y\ 4138}#
                                         #{r\ 4129}#
                                         #{mod\ 4133}#)
                                       (#{gen-syntax-case\ 4108}#
                                         #{x\ 4126}#
                                         #{keys\ 4127}#
                                         #{clauses\ 4128}#
                                         #{r\ 4129}#
                                         #{mod\ 4133}#))))
                                 (list (if (eq? #{p\ 4134}# (quote any))
                                         (#{build-application\ 2646}#
                                           #f
                                           (#{build-primref\ 2658}#
                                             #f
                                             'list)
                                           (list #{x\ 4126}#))
                                         (#{build-application\ 2646}#
                                           #f
                                           (#{build-primref\ 2658}#
                                             #f
                                             '$sc-dispatch)
                                           (list #{x\ 4126}#
                                                 (#{build-data\ 2659}#
                                                   #f
                                                   #{p\ 4134}#)))))))))))))
                 (#{build-dispatch-call\ 4106}#
                   (lambda (#{pvars\ 4142}#
                            #{exp\ 4143}#
                            #{y\ 4144}#
                            #{r\ 4145}#
                            #{mod\ 4146}#)
                     (let ((#{ids\ 4147}# (map car #{pvars\ 4142}#))
                           (#{levels\ 4148}# (map cdr #{pvars\ 4142}#)))
                       (let ((#{labels\ 4149}#
                               (#{gen-labels\ 2687}# #{ids\ 4147}#))
                             (#{new-vars\ 4150}#
                               (map #{gen-var\ 2731}# #{ids\ 4147}#)))
                         (#{build-application\ 2646}#
                           #f
                           (#{build-primref\ 2658}# #f (quote apply))
                           (list (#{build-simple-lambda\ 2655}#
                                   #f
                                   (map syntax->datum #{ids\ 4147}#)
                                   #f
                                   #{new-vars\ 4150}#
                                   #f
                                   (#{chi\ 2717}#
                                     #{exp\ 4143}#
                                     (#{extend-env\ 2675}#
                                       #{labels\ 4149}#
                                       (map (lambda (#{var\ 4151}#
                                                     #{level\ 4152}#)
                                              (cons 'syntax
                                                    (cons #{var\ 4151}#
                                                          #{level\ 4152}#)))
                                            #{new-vars\ 4150}#
                                            (map cdr #{pvars\ 4142}#))
                                       #{r\ 4145}#)
                                     (#{make-binding-wrap\ 2698}#
                                       #{ids\ 4147}#
                                       #{labels\ 4149}#
                                       '(()))
                                     #{mod\ 4146}#))
                                 #{y\ 4144}#))))))
                 (#{convert-pattern\ 4105}#
                   (lambda (#{pattern\ 4153}# #{keys\ 4154}#)
                     (letrec ((#{cvt\ 4155}#
                                (lambda (#{p\ 4156}# #{n\ 4157}# #{ids\ 4158}#)
                                  (if (#{id?\ 2681}# #{p\ 4156}#)
                                    (if (#{bound-id-member?\ 2708}#
                                          #{p\ 4156}#
                                          #{keys\ 4154}#)
                                      (values
                                        (vector (quote free-id) #{p\ 4156}#)
                                        #{ids\ 4158}#)
                                      (values
                                        'any
                                        (cons (cons #{p\ 4156}# #{n\ 4157}#)
                                              #{ids\ 4158}#)))
                                    ((lambda (#{tmp\ 4159}#)
                                       ((lambda (#{tmp\ 4160}#)
                                          (if (if #{tmp\ 4160}#
                                                (apply (lambda (#{x\ 4161}#
                                                                #{dots\ 4162}#)
                                                         (#{ellipsis?\ 2725}#
                                                           #{dots\ 4162}#))
                                                       #{tmp\ 4160}#)
                                                #f)
                                            (apply (lambda (#{x\ 4163}#
                                                            #{dots\ 4164}#)
                                                     (call-with-values
                                                       (lambda ()
                                                         (#{cvt\ 4155}#
                                                           #{x\ 4163}#
                                                           (#{fx+\ 2636}#
                                                             #{n\ 4157}#
                                                             1)
                                                           #{ids\ 4158}#))
                                                       (lambda (#{p\ 4165}#
                                                                #{ids\ 4166}#)
                                                         (values
                                                           (if (eq? #{p\ 4165}#
                                                                    'any)
                                                             'each-any
                                                             (vector
                                                               'each
                                                               #{p\ 4165}#))
                                                           #{ids\ 4166}#))))
                                                   #{tmp\ 4160}#)
                                            ((lambda (#{tmp\ 4167}#)
                                               (if #{tmp\ 4167}#
                                                 (apply (lambda (#{x\ 4168}#
                                                                 #{y\ 4169}#)
                                                          (call-with-values
                                                            (lambda ()
                                                              (#{cvt\ 4155}#
                                                                #{y\ 4169}#
                                                                #{n\ 4157}#
                                                                #{ids\ 4158}#))
                                                            (lambda (#{y\ 4170}#
                                                                     #{ids\ 4171}#)
                                                              (call-with-values
                                                                (lambda ()
                                                                  (#{cvt\ 4155}#
                                                                    #{x\ 4168}#
                                                                    #{n\ 4157}#
                                                                    #{ids\ 4171}#))
                                                                (lambda (#{x\ 4172}#
                                                                         #{ids\ 4173}#)
                                                                  (values
                                                                    (cons #{x\ 4172}#
                                                                          #{y\ 4170}#)
                                                                    #{ids\ 4173}#))))))
                                                        #{tmp\ 4167}#)
                                                 ((lambda (#{tmp\ 4174}#)
                                                    (if #{tmp\ 4174}#
                                                      (apply (lambda ()
                                                               (values
                                                                 '()
                                                                 #{ids\ 4158}#))
                                                             #{tmp\ 4174}#)
                                                      ((lambda (#{tmp\ 4175}#)
                                                         (if #{tmp\ 4175}#
                                                           (apply (lambda (#{x\ 4176}#)
                                                                    (call-with-values
                                                                      (lambda ()
                                                                        (#{cvt\ 4155}#
                                                                          #{x\ 4176}#
                                                                          #{n\ 4157}#
                                                                          #{ids\ 4158}#))
                                                                      (lambda (#{p\ 4178}#
                                                                               #{ids\ 4179}#)
                                                                        (values
                                                                          (vector
                                                                            'vector
                                                                            #{p\ 4178}#)
                                                                          #{ids\ 4179}#))))
                                                                  #{tmp\ 4175}#)
                                                           ((lambda (#{x\ 4180}#)
                                                              (values
                                                                (vector
                                                                  'atom
                                                                  (#{strip\ 2730}#
                                                                    #{p\ 4156}#
                                                                    '(())))
                                                                #{ids\ 4158}#))
                                                            #{tmp\ 4159}#)))
                                                       ($sc-dispatch
                                                         #{tmp\ 4159}#
                                                         '#(vector
                                                            each-any)))))
                                                  ($sc-dispatch
                                                    #{tmp\ 4159}#
                                                    '()))))
                                             ($sc-dispatch
                                               #{tmp\ 4159}#
                                               '(any . any)))))
                                        ($sc-dispatch
                                          #{tmp\ 4159}#
                                          '(any any))))
                                     #{p\ 4156}#)))))
                       (#{cvt\ 4155}# #{pattern\ 4153}# 0 (quote ()))))))
          (lambda (#{e\ 4181}#
                   #{r\ 4182}#
                   #{w\ 4183}#
                   #{s\ 4184}#
                   #{mod\ 4185}#)
            (let ((#{e\ 4186}#
                    (#{source-wrap\ 2710}#
                      #{e\ 4181}#
                      #{w\ 4183}#
                      #{s\ 4184}#
                      #{mod\ 4185}#)))
              ((lambda (#{tmp\ 4187}#)
                 ((lambda (#{tmp\ 4188}#)
                    (if #{tmp\ 4188}#
                      (apply (lambda (#{_\ 4189}#
                                      #{val\ 4190}#
                                      #{key\ 4191}#
                                      #{m\ 4192}#)
                               (if (and-map
                                     (lambda (#{x\ 4193}#)
                                       (if (#{id?\ 2681}# #{x\ 4193}#)
                                         (not (#{ellipsis?\ 2725}#
                                                #{x\ 4193}#))
                                         #f))
                                     #{key\ 4191}#)
                                 (let ((#{x\ 4195}#
                                         (#{gen-var\ 2731}# (quote tmp))))
                                   (#{build-application\ 2646}#
                                     #{s\ 4184}#
                                     (#{build-simple-lambda\ 2655}#
                                       #f
                                       (list (quote tmp))
                                       #f
                                       (list #{x\ 4195}#)
                                       #f
                                       (#{gen-syntax-case\ 4108}#
                                         (#{build-lexical-reference\ 2648}#
                                           'value
                                           #f
                                           'tmp
                                           #{x\ 4195}#)
                                         #{key\ 4191}#
                                         #{m\ 4192}#
                                         #{r\ 4182}#
                                         #{mod\ 4185}#))
                                     (list (#{chi\ 2717}#
                                             #{val\ 4190}#
                                             #{r\ 4182}#
                                             '(())
                                             #{mod\ 4185}#))))
                                 (syntax-violation
                                   'syntax-case
                                   "invalid literals list"
                                   #{e\ 4186}#)))
                             #{tmp\ 4188}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp\ 4187}#)))
                  ($sc-dispatch
                    #{tmp\ 4187}#
                    '(any any each-any . each-any))))
               #{e\ 4186}#)))))
      (set! sc-expand
        (lambda (#{x\ 4198}# . #{rest\ 4199}#)
          (if (if (pair? #{x\ 4198}#)
                (equal? (car #{x\ 4198}#) #{noexpand\ 2634}#)
                #f)
            (cadr #{x\ 4198}#)
            (let ((#{m\ 4200}#
                    (if (null? #{rest\ 4199}#)
                      'e
                      (car #{rest\ 4199}#)))
                  (#{esew\ 4201}#
                    (if (let ((#{t\ 4202}# (null? #{rest\ 4199}#)))
                          (if #{t\ 4202}#
                            #{t\ 4202}#
                            (null? (cdr #{rest\ 4199}#))))
                      '(eval)
                      (cadr #{rest\ 4199}#))))
              (with-fluid*
                #{*mode*\ 2635}#
                #{m\ 4200}#
                (lambda ()
                  (#{chi-top\ 2716}#
                    #{x\ 4198}#
                    '()
                    '((top))
                    #{m\ 4200}#
                    #{esew\ 4201}#
                    (cons 'hygiene
                          (module-name (current-module))))))))))
      (set! identifier?
        (lambda (#{x\ 4203}#)
          (#{nonsymbol-id?\ 2680}# #{x\ 4203}#)))
      (set! datum->syntax
        (lambda (#{id\ 4204}# #{datum\ 4205}#)
          (#{make-syntax-object\ 2664}#
            #{datum\ 4205}#
            (#{syntax-object-wrap\ 2667}# #{id\ 4204}#)
            #f)))
      (set! syntax->datum
        (lambda (#{x\ 4206}#)
          (#{strip\ 2730}# #{x\ 4206}# (quote (())))))
      (set! generate-temporaries
        (lambda (#{ls\ 4207}#)
          (begin
            (let ((#{x\ 4208}# #{ls\ 4207}#))
              (if (not (list? #{x\ 4208}#))
                (syntax-violation
                  'generate-temporaries
                  "invalid argument"
                  #{x\ 4208}#)))
            (map (lambda (#{x\ 4209}#)
                   (#{wrap\ 2709}# (gensym) (quote ((top))) #f))
                 #{ls\ 4207}#))))
      (set! free-identifier=?
        (lambda (#{x\ 4210}# #{y\ 4211}#)
          (begin
            (let ((#{x\ 4212}# #{x\ 4210}#))
              (if (not (#{nonsymbol-id?\ 2680}# #{x\ 4212}#))
                (syntax-violation
                  'free-identifier=?
                  "invalid argument"
                  #{x\ 4212}#)))
            (let ((#{x\ 4213}# #{y\ 4211}#))
              (if (not (#{nonsymbol-id?\ 2680}# #{x\ 4213}#))
                (syntax-violation
                  'free-identifier=?
                  "invalid argument"
                  #{x\ 4213}#)))
            (#{free-id=?\ 2704}# #{x\ 4210}# #{y\ 4211}#))))
      (set! bound-identifier=?
        (lambda (#{x\ 4214}# #{y\ 4215}#)
          (begin
            (let ((#{x\ 4216}# #{x\ 4214}#))
              (if (not (#{nonsymbol-id?\ 2680}# #{x\ 4216}#))
                (syntax-violation
                  'bound-identifier=?
                  "invalid argument"
                  #{x\ 4216}#)))
            (let ((#{x\ 4217}# #{y\ 4215}#))
              (if (not (#{nonsymbol-id?\ 2680}# #{x\ 4217}#))
                (syntax-violation
                  'bound-identifier=?
                  "invalid argument"
                  #{x\ 4217}#)))
            (#{bound-id=?\ 2705}# #{x\ 4214}# #{y\ 4215}#))))
      (set! syntax-violation
        (lambda (#{who\ 4218}#
                 #{message\ 4219}#
                 #{form\ 4220}#
                 .
                 #{subform\ 4221}#)
          (begin
            (let ((#{x\ 4222}# #{who\ 4218}#))
              (if (not ((lambda (#{x\ 4223}#)
                          (let ((#{t\ 4224}# (not #{x\ 4223}#)))
                            (if #{t\ 4224}#
                              #{t\ 4224}#
                              (let ((#{t\ 4225}# (string? #{x\ 4223}#)))
                                (if #{t\ 4225}#
                                  #{t\ 4225}#
                                  (symbol? #{x\ 4223}#))))))
                        #{x\ 4222}#))
                (syntax-violation
                  'syntax-violation
                  "invalid argument"
                  #{x\ 4222}#)))
            (let ((#{x\ 4226}# #{message\ 4219}#))
              (if (not (string? #{x\ 4226}#))
                (syntax-violation
                  'syntax-violation
                  "invalid argument"
                  #{x\ 4226}#)))
            (scm-error
              'syntax-error
              'sc-expand
              (string-append
                (if #{who\ 4218}# "~a: " "")
                "~a "
                (if (null? #{subform\ 4221}#)
                  "in ~a"
                  "in subform `~s' of `~s'"))
              (let ((#{tail\ 4227}#
                      (cons #{message\ 4219}#
                            (map (lambda (#{x\ 4228}#)
                                   (#{strip\ 2730}# #{x\ 4228}# (quote (()))))
                                 (append
                                   #{subform\ 4221}#
                                   (list #{form\ 4220}#))))))
                (if #{who\ 4218}#
                  (cons #{who\ 4218}# #{tail\ 4227}#)
                  #{tail\ 4227}#))
              #f))))
      (letrec ((#{match\ 4233}#
                 (lambda (#{e\ 4234}#
                          #{p\ 4235}#
                          #{w\ 4236}#
                          #{r\ 4237}#
                          #{mod\ 4238}#)
                   (if (not #{r\ 4237}#)
                     #f
                     (if (eq? #{p\ 4235}# (quote any))
                       (cons (#{wrap\ 2709}#
                               #{e\ 4234}#
                               #{w\ 4236}#
                               #{mod\ 4238}#)
                             #{r\ 4237}#)
                       (if (#{syntax-object?\ 2665}# #{e\ 4234}#)
                         (#{match*\ 4232}#
                           (#{syntax-object-expression\ 2666}# #{e\ 4234}#)
                           #{p\ 4235}#
                           (#{join-wraps\ 2700}#
                             #{w\ 4236}#
                             (#{syntax-object-wrap\ 2667}# #{e\ 4234}#))
                           #{r\ 4237}#
                           (#{syntax-object-module\ 2668}# #{e\ 4234}#))
                         (#{match*\ 4232}#
                           #{e\ 4234}#
                           #{p\ 4235}#
                           #{w\ 4236}#
                           #{r\ 4237}#
                           #{mod\ 4238}#))))))
               (#{match*\ 4232}#
                 (lambda (#{e\ 4239}#
                          #{p\ 4240}#
                          #{w\ 4241}#
                          #{r\ 4242}#
                          #{mod\ 4243}#)
                   (if (null? #{p\ 4240}#)
                     (if (null? #{e\ 4239}#) #{r\ 4242}# #f)
                     (if (pair? #{p\ 4240}#)
                       (if (pair? #{e\ 4239}#)
                         (#{match\ 4233}#
                           (car #{e\ 4239}#)
                           (car #{p\ 4240}#)
                           #{w\ 4241}#
                           (#{match\ 4233}#
                             (cdr #{e\ 4239}#)
                             (cdr #{p\ 4240}#)
                             #{w\ 4241}#
                             #{r\ 4242}#
                             #{mod\ 4243}#)
                           #{mod\ 4243}#)
                         #f)
                       (if (eq? #{p\ 4240}# (quote each-any))
                         (let ((#{l\ 4244}#
                                 (#{match-each-any\ 4230}#
                                   #{e\ 4239}#
                                   #{w\ 4241}#
                                   #{mod\ 4243}#)))
                           (if #{l\ 4244}#
                             (cons #{l\ 4244}# #{r\ 4242}#)
                             #f))
                         (let ((#{atom-key\ 4245}# (vector-ref #{p\ 4240}# 0)))
                           (if (memv #{atom-key\ 4245}# (quote (each)))
                             (if (null? #{e\ 4239}#)
                               (#{match-empty\ 4231}#
                                 (vector-ref #{p\ 4240}# 1)
                                 #{r\ 4242}#)
                               (let ((#{l\ 4246}#
                                       (#{match-each\ 4229}#
                                         #{e\ 4239}#
                                         (vector-ref #{p\ 4240}# 1)
                                         #{w\ 4241}#
                                         #{mod\ 4243}#)))
                                 (if #{l\ 4246}#
                                   (letrec ((#{collect\ 4247}#
                                              (lambda (#{l\ 4248}#)
                                                (if (null? (car #{l\ 4248}#))
                                                  #{r\ 4242}#
                                                  (cons (map car #{l\ 4248}#)
                                                        (#{collect\ 4247}#
                                                          (map cdr
                                                               #{l\ 4248}#)))))))
                                     (#{collect\ 4247}# #{l\ 4246}#))
                                   #f)))
                             (if (memv #{atom-key\ 4245}# (quote (free-id)))
                               (if (#{id?\ 2681}# #{e\ 4239}#)
                                 (if (#{free-id=?\ 2704}#
                                       (#{wrap\ 2709}#
                                         #{e\ 4239}#
                                         #{w\ 4241}#
                                         #{mod\ 4243}#)
                                       (vector-ref #{p\ 4240}# 1))
                                   #{r\ 4242}#
                                   #f)
                                 #f)
                               (if (memv #{atom-key\ 4245}# (quote (atom)))
                                 (if (equal?
                                       (vector-ref #{p\ 4240}# 1)
                                       (#{strip\ 2730}#
                                         #{e\ 4239}#
                                         #{w\ 4241}#))
                                   #{r\ 4242}#
                                   #f)
                                 (if (memv #{atom-key\ 4245}# (quote (vector)))
                                   (if (vector? #{e\ 4239}#)
                                     (#{match\ 4233}#
                                       (vector->list #{e\ 4239}#)
                                       (vector-ref #{p\ 4240}# 1)
                                       #{w\ 4241}#
                                       #{r\ 4242}#
                                       #{mod\ 4243}#)
                                     #f)))))))))))
               (#{match-empty\ 4231}#
                 (lambda (#{p\ 4249}# #{r\ 4250}#)
                   (if (null? #{p\ 4249}#)
                     #{r\ 4250}#
                     (if (eq? #{p\ 4249}# (quote any))
                       (cons (quote ()) #{r\ 4250}#)
                       (if (pair? #{p\ 4249}#)
                         (#{match-empty\ 4231}#
                           (car #{p\ 4249}#)
                           (#{match-empty\ 4231}#
                             (cdr #{p\ 4249}#)
                             #{r\ 4250}#))
                         (if (eq? #{p\ 4249}# (quote each-any))
                           (cons (quote ()) #{r\ 4250}#)
                           (let ((#{atom-key\ 4251}#
                                   (vector-ref #{p\ 4249}# 0)))
                             (if (memv #{atom-key\ 4251}# (quote (each)))
                               (#{match-empty\ 4231}#
                                 (vector-ref #{p\ 4249}# 1)
                                 #{r\ 4250}#)
                               (if (memv #{atom-key\ 4251}#
                                         '(free-id atom))
                                 #{r\ 4250}#
                                 (if (memv #{atom-key\ 4251}# (quote (vector)))
                                   (#{match-empty\ 4231}#
                                     (vector-ref #{p\ 4249}# 1)
                                     #{r\ 4250}#)))))))))))
               (#{match-each-any\ 4230}#
                 (lambda (#{e\ 4252}# #{w\ 4253}# #{mod\ 4254}#)
                   (if (pair? #{e\ 4252}#)
                     (let ((#{l\ 4255}#
                             (#{match-each-any\ 4230}#
                               (cdr #{e\ 4252}#)
                               #{w\ 4253}#
                               #{mod\ 4254}#)))
                       (if #{l\ 4255}#
                         (cons (#{wrap\ 2709}#
                                 (car #{e\ 4252}#)
                                 #{w\ 4253}#
                                 #{mod\ 4254}#)
                               #{l\ 4255}#)
                         #f))
                     (if (null? #{e\ 4252}#)
                       '()
                       (if (#{syntax-object?\ 2665}# #{e\ 4252}#)
                         (#{match-each-any\ 4230}#
                           (#{syntax-object-expression\ 2666}# #{e\ 4252}#)
                           (#{join-wraps\ 2700}#
                             #{w\ 4253}#
                             (#{syntax-object-wrap\ 2667}# #{e\ 4252}#))
                           #{mod\ 4254}#)
                         #f)))))
               (#{match-each\ 4229}#
                 (lambda (#{e\ 4256}#
                          #{p\ 4257}#
                          #{w\ 4258}#
                          #{mod\ 4259}#)
                   (if (pair? #{e\ 4256}#)
                     (let ((#{first\ 4260}#
                             (#{match\ 4233}#
                               (car #{e\ 4256}#)
                               #{p\ 4257}#
                               #{w\ 4258}#
                               '()
                               #{mod\ 4259}#)))
                       (if #{first\ 4260}#
                         (let ((#{rest\ 4261}#
                                 (#{match-each\ 4229}#
                                   (cdr #{e\ 4256}#)
                                   #{p\ 4257}#
                                   #{w\ 4258}#
                                   #{mod\ 4259}#)))
                           (if #{rest\ 4261}#
                             (cons #{first\ 4260}# #{rest\ 4261}#)
                             #f))
                         #f))
                     (if (null? #{e\ 4256}#)
                       '()
                       (if (#{syntax-object?\ 2665}# #{e\ 4256}#)
                         (#{match-each\ 4229}#
                           (#{syntax-object-expression\ 2666}# #{e\ 4256}#)
                           #{p\ 4257}#
                           (#{join-wraps\ 2700}#
                             #{w\ 4258}#
                             (#{syntax-object-wrap\ 2667}# #{e\ 4256}#))
                           (#{syntax-object-module\ 2668}# #{e\ 4256}#))
                         #f))))))
        (set! $sc-dispatch
          (lambda (#{e\ 4262}# #{p\ 4263}#)
            (if (eq? #{p\ 4263}# (quote any))
              (list #{e\ 4262}#)
              (if (#{syntax-object?\ 2665}# #{e\ 4262}#)
                (#{match*\ 4232}#
                  (#{syntax-object-expression\ 2666}# #{e\ 4262}#)
                  #{p\ 4263}#
                  (#{syntax-object-wrap\ 2667}# #{e\ 4262}#)
                  '()
                  (#{syntax-object-module\ 2668}# #{e\ 4262}#))
                (#{match*\ 4232}#
                  #{e\ 4262}#
                  #{p\ 4263}#
                  '(())
                  '()
                  #f)))))))))

(define with-syntax
  (make-syncase-macro
    'macro
    (lambda (#{x\ 4264}#)
      ((lambda (#{tmp\ 4265}#)
         ((lambda (#{tmp\ 4266}#)
            (if #{tmp\ 4266}#
              (apply (lambda (#{_\ 4267}# #{e1\ 4268}# #{e2\ 4269}#)
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
                             (cons #{e1\ 4268}# #{e2\ 4269}#)))
                     #{tmp\ 4266}#)
              ((lambda (#{tmp\ 4271}#)
                 (if #{tmp\ 4271}#
                   (apply (lambda (#{_\ 4272}#
                                   #{out\ 4273}#
                                   #{in\ 4274}#
                                   #{e1\ 4275}#
                                   #{e2\ 4276}#)
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
                                  #{in\ 4274}#
                                  '()
                                  (list #{out\ 4273}#
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
                                              (cons #{e1\ 4275}#
                                                    #{e2\ 4276}#)))))
                          #{tmp\ 4271}#)
                   ((lambda (#{tmp\ 4278}#)
                      (if #{tmp\ 4278}#
                        (apply (lambda (#{_\ 4279}#
                                        #{out\ 4280}#
                                        #{in\ 4281}#
                                        #{e1\ 4282}#
                                        #{e2\ 4283}#)
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
                                             #{in\ 4281}#)
                                       '()
                                       (list #{out\ 4280}#
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
                                                   (cons #{e1\ 4282}#
                                                         #{e2\ 4283}#)))))
                               #{tmp\ 4278}#)
                        (syntax-violation
                          #f
                          "source expression failed to match any pattern"
                          #{tmp\ 4265}#)))
                    ($sc-dispatch
                      #{tmp\ 4265}#
                      '(any #(each (any any)) any . each-any)))))
               ($sc-dispatch
                 #{tmp\ 4265}#
                 '(any ((any any)) any . each-any)))))
          ($sc-dispatch
            #{tmp\ 4265}#
            '(any () any . each-any))))
       #{x\ 4264}#))))

(define syntax-rules
  (make-syncase-macro
    'macro
    (lambda (#{x\ 4287}#)
      ((lambda (#{tmp\ 4288}#)
         ((lambda (#{tmp\ 4289}#)
            (if #{tmp\ 4289}#
              (apply (lambda (#{_\ 4290}#
                              #{k\ 4291}#
                              #{keyword\ 4292}#
                              #{pattern\ 4293}#
                              #{template\ 4294}#)
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
                                         (cons #{k\ 4291}#
                                               (map (lambda (#{tmp\ 4297}#
                                                             #{tmp\ 4296}#)
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
                                                                  #{tmp\ 4296}#)
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
                                                                  #{tmp\ 4297}#)))
                                                    #{template\ 4294}#
                                                    #{pattern\ 4293}#))))))
                     #{tmp\ 4289}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 4288}#)))
          ($sc-dispatch
            #{tmp\ 4288}#
            '(any each-any . #(each ((any . any) any))))))
       #{x\ 4287}#))))

(define let*
  (make-extended-syncase-macro
    (module-ref (current-module) (quote let*))
    'macro
    (lambda (#{x\ 4298}#)
      ((lambda (#{tmp\ 4299}#)
         ((lambda (#{tmp\ 4300}#)
            (if (if #{tmp\ 4300}#
                  (apply (lambda (#{let*\ 4301}#
                                  #{x\ 4302}#
                                  #{v\ 4303}#
                                  #{e1\ 4304}#
                                  #{e2\ 4305}#)
                           (and-map identifier? #{x\ 4302}#))
                         #{tmp\ 4300}#)
                  #f)
              (apply (lambda (#{let*\ 4307}#
                              #{x\ 4308}#
                              #{v\ 4309}#
                              #{e1\ 4310}#
                              #{e2\ 4311}#)
                       (letrec ((#{f\ 4312}#
                                  (lambda (#{bindings\ 4313}#)
                                    (if (null? #{bindings\ 4313}#)
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
                                                  (cons #{e1\ 4310}#
                                                        #{e2\ 4311}#)))
                                      ((lambda (#{tmp\ 4317}#)
                                         ((lambda (#{tmp\ 4318}#)
                                            (if #{tmp\ 4318}#
                                              (apply (lambda (#{body\ 4319}#
                                                              #{binding\ 4320}#)
                                                       (list '#(syntax-object
                                                                let
                                                                ((top)
                                                                 #(ribcage
                                                                   #(body
                                                                     binding)
                                                                   #((top)
                                                                     (top))
                                                                   #("i" "i"))
                                                                 #(ribcage
                                                                   ()
                                                                   ()
                                                                   ())
                                                                 #(ribcage
                                                                   #(f
                                                                     bindings)
                                                                   #((top)
                                                                     (top))
                                                                   #("i" "i"))
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
                                                             (list #{binding\ 4320}#)
                                                             #{body\ 4319}#))
                                                     #{tmp\ 4318}#)
                                              (syntax-violation
                                                #f
                                                "source expression failed to match any pattern"
                                                #{tmp\ 4317}#)))
                                          ($sc-dispatch
                                            #{tmp\ 4317}#
                                            '(any any))))
                                       (list (#{f\ 4312}#
                                               (cdr #{bindings\ 4313}#))
                                             (car #{bindings\ 4313}#)))))))
                         (#{f\ 4312}# (map list #{x\ 4308}# #{v\ 4309}#))))
                     #{tmp\ 4300}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 4299}#)))
          ($sc-dispatch
            #{tmp\ 4299}#
            '(any #(each (any any)) any . each-any))))
       #{x\ 4298}#))))

(define do
  (make-extended-syncase-macro
    (module-ref (current-module) (quote do))
    'macro
    (lambda (#{orig-x\ 4321}#)
      ((lambda (#{tmp\ 4322}#)
         ((lambda (#{tmp\ 4323}#)
            (if #{tmp\ 4323}#
              (apply (lambda (#{_\ 4324}#
                              #{var\ 4325}#
                              #{init\ 4326}#
                              #{step\ 4327}#
                              #{e0\ 4328}#
                              #{e1\ 4329}#
                              #{c\ 4330}#)
                       ((lambda (#{tmp\ 4331}#)
                          ((lambda (#{tmp\ 4332}#)
                             (if #{tmp\ 4332}#
                               (apply (lambda (#{step\ 4333}#)
                                        ((lambda (#{tmp\ 4334}#)
                                           ((lambda (#{tmp\ 4335}#)
                                              (if #{tmp\ 4335}#
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
                                                                    #{var\ 4325}#
                                                                    #{init\ 4326}#)
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
                                                                           #{e0\ 4328}#)
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
                                                                             #{c\ 4330}#
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
                                                                                         #{step\ 4333}#)))))))
                                                       #{tmp\ 4335}#)
                                                ((lambda (#{tmp\ 4340}#)
                                                   (if #{tmp\ 4340}#
                                                     (apply (lambda (#{e1\ 4341}#
                                                                     #{e2\ 4342}#)
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
                                                                         #{var\ 4325}#
                                                                         #{init\ 4326}#)
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
                                                                          #{e0\ 4328}#
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
                                                                                (cons #{e1\ 4341}#
                                                                                      #{e2\ 4342}#))
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
                                                                                  #{c\ 4330}#
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
                                                                                              #{step\ 4333}#)))))))
                                                            #{tmp\ 4340}#)
                                                     (syntax-violation
                                                       #f
                                                       "source expression failed to match any pattern"
                                                       #{tmp\ 4334}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 4334}#
                                                   '(any . each-any)))))
                                            ($sc-dispatch
                                              #{tmp\ 4334}#
                                              '())))
                                         #{e1\ 4329}#))
                                      #{tmp\ 4332}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp\ 4331}#)))
                           ($sc-dispatch #{tmp\ 4331}# (quote each-any))))
                        (map (lambda (#{v\ 4349}# #{s\ 4350}#)
                               ((lambda (#{tmp\ 4351}#)
                                  ((lambda (#{tmp\ 4352}#)
                                     (if #{tmp\ 4352}#
                                       (apply (lambda () #{v\ 4349}#)
                                              #{tmp\ 4352}#)
                                       ((lambda (#{tmp\ 4353}#)
                                          (if #{tmp\ 4353}#
                                            (apply (lambda (#{e\ 4354}#)
                                                     #{e\ 4354}#)
                                                   #{tmp\ 4353}#)
                                            ((lambda (#{_\ 4355}#)
                                               (syntax-violation
                                                 'do
                                                 "bad step expression"
                                                 #{orig-x\ 4321}#
                                                 #{s\ 4350}#))
                                             #{tmp\ 4351}#)))
                                        ($sc-dispatch
                                          #{tmp\ 4351}#
                                          '(any)))))
                                   ($sc-dispatch #{tmp\ 4351}# (quote ()))))
                                #{s\ 4350}#))
                             #{var\ 4325}#
                             #{step\ 4327}#)))
                     #{tmp\ 4323}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 4322}#)))
          ($sc-dispatch
            #{tmp\ 4322}#
            '(any #(each (any any . any))
                  (any . each-any)
                  .
                  each-any))))
       #{orig-x\ 4321}#))))

(define quasiquote
  (make-extended-syncase-macro
    (module-ref (current-module) (quote quasiquote))
    'macro
    (letrec ((#{quasicons\ 4358}#
               (lambda (#{x\ 4362}# #{y\ 4363}#)
                 ((lambda (#{tmp\ 4364}#)
                    ((lambda (#{tmp\ 4365}#)
                       (if #{tmp\ 4365}#
                         (apply (lambda (#{x\ 4366}# #{y\ 4367}#)
                                  ((lambda (#{tmp\ 4368}#)
                                     ((lambda (#{tmp\ 4369}#)
                                        (if #{tmp\ 4369}#
                                          (apply (lambda (#{dy\ 4370}#)
                                                   ((lambda (#{tmp\ 4371}#)
                                                      ((lambda (#{tmp\ 4372}#)
                                                         (if #{tmp\ 4372}#
                                                           (apply (lambda (#{dx\ 4373}#)
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
                                                                          (cons #{dx\ 4373}#
                                                                                #{dy\ 4370}#)))
                                                                  #{tmp\ 4372}#)
                                                           ((lambda (#{_\ 4374}#)
                                                              (if (null? #{dy\ 4370}#)
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
                                                                      #{x\ 4366}#)
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
                                                                      #{x\ 4366}#
                                                                      #{y\ 4367}#)))
                                                            #{tmp\ 4371}#)))
                                                       ($sc-dispatch
                                                         #{tmp\ 4371}#
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
                                                    #{x\ 4366}#))
                                                 #{tmp\ 4369}#)
                                          ((lambda (#{tmp\ 4375}#)
                                             (if #{tmp\ 4375}#
                                               (apply (lambda (#{stuff\ 4376}#)
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
                                                              (cons #{x\ 4366}#
                                                                    #{stuff\ 4376}#)))
                                                      #{tmp\ 4375}#)
                                               ((lambda (#{else\ 4377}#)
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
                                                        #{x\ 4366}#
                                                        #{y\ 4367}#))
                                                #{tmp\ 4368}#)))
                                           ($sc-dispatch
                                             #{tmp\ 4368}#
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
                                        #{tmp\ 4368}#
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
                                   #{y\ 4367}#))
                                #{tmp\ 4365}#)
                         (syntax-violation
                           #f
                           "source expression failed to match any pattern"
                           #{tmp\ 4364}#)))
                     ($sc-dispatch #{tmp\ 4364}# (quote (any any)))))
                  (list #{x\ 4362}# #{y\ 4363}#))))
             (#{quasiappend\ 4359}#
               (lambda (#{x\ 4378}# #{y\ 4379}#)
                 ((lambda (#{tmp\ 4380}#)
                    ((lambda (#{tmp\ 4381}#)
                       (if #{tmp\ 4381}#
                         (apply (lambda (#{x\ 4382}# #{y\ 4383}#)
                                  ((lambda (#{tmp\ 4384}#)
                                     ((lambda (#{tmp\ 4385}#)
                                        (if #{tmp\ 4385}#
                                          (apply (lambda () #{x\ 4382}#)
                                                 #{tmp\ 4385}#)
                                          ((lambda (#{_\ 4386}#)
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
                                                   #{x\ 4382}#
                                                   #{y\ 4383}#))
                                           #{tmp\ 4384}#)))
                                      ($sc-dispatch
                                        #{tmp\ 4384}#
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
                                   #{y\ 4383}#))
                                #{tmp\ 4381}#)
                         (syntax-violation
                           #f
                           "source expression failed to match any pattern"
                           #{tmp\ 4380}#)))
                     ($sc-dispatch #{tmp\ 4380}# (quote (any any)))))
                  (list #{x\ 4378}# #{y\ 4379}#))))
             (#{quasivector\ 4360}#
               (lambda (#{x\ 4387}#)
                 ((lambda (#{tmp\ 4388}#)
                    ((lambda (#{x\ 4389}#)
                       ((lambda (#{tmp\ 4390}#)
                          ((lambda (#{tmp\ 4391}#)
                             (if #{tmp\ 4391}#
                               (apply (lambda (#{x\ 4392}#)
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
                                              (list->vector #{x\ 4392}#)))
                                      #{tmp\ 4391}#)
                               ((lambda (#{tmp\ 4394}#)
                                  (if #{tmp\ 4394}#
                                    (apply (lambda (#{x\ 4395}#)
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
                                                   #{x\ 4395}#))
                                           #{tmp\ 4394}#)
                                    ((lambda (#{_\ 4397}#)
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
                                             #{x\ 4389}#))
                                     #{tmp\ 4390}#)))
                                ($sc-dispatch
                                  #{tmp\ 4390}#
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
                             #{tmp\ 4390}#
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
                        #{x\ 4389}#))
                     #{tmp\ 4388}#))
                  #{x\ 4387}#)))
             (#{quasi\ 4361}#
               (lambda (#{p\ 4398}# #{lev\ 4399}#)
                 ((lambda (#{tmp\ 4400}#)
                    ((lambda (#{tmp\ 4401}#)
                       (if #{tmp\ 4401}#
                         (apply (lambda (#{p\ 4402}#)
                                  (if (= #{lev\ 4399}# 0)
                                    #{p\ 4402}#
                                    (#{quasicons\ 4358}#
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
                                      (#{quasi\ 4361}#
                                        (list #{p\ 4402}#)
                                        (- #{lev\ 4399}# 1)))))
                                #{tmp\ 4401}#)
                         ((lambda (#{tmp\ 4403}#)
                            (if (if #{tmp\ 4403}#
                                  (apply (lambda (#{args\ 4404}#)
                                           (= #{lev\ 4399}# 0))
                                         #{tmp\ 4403}#)
                                  #f)
                              (apply (lambda (#{args\ 4405}#)
                                       (syntax-violation
                                         'unquote
                                         "unquote takes exactly one argument"
                                         #{p\ 4398}#
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
                                               #{args\ 4405}#)))
                                     #{tmp\ 4403}#)
                              ((lambda (#{tmp\ 4406}#)
                                 (if #{tmp\ 4406}#
                                   (apply (lambda (#{p\ 4407}# #{q\ 4408}#)
                                            (if (= #{lev\ 4399}# 0)
                                              (#{quasiappend\ 4359}#
                                                #{p\ 4407}#
                                                (#{quasi\ 4361}#
                                                  #{q\ 4408}#
                                                  #{lev\ 4399}#))
                                              (#{quasicons\ 4358}#
                                                (#{quasicons\ 4358}#
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
                                                  (#{quasi\ 4361}#
                                                    (list #{p\ 4407}#)
                                                    (- #{lev\ 4399}# 1)))
                                                (#{quasi\ 4361}#
                                                  #{q\ 4408}#
                                                  #{lev\ 4399}#))))
                                          #{tmp\ 4406}#)
                                   ((lambda (#{tmp\ 4409}#)
                                      (if (if #{tmp\ 4409}#
                                            (apply (lambda (#{args\ 4410}#
                                                            #{q\ 4411}#)
                                                     (= #{lev\ 4399}# 0))
                                                   #{tmp\ 4409}#)
                                            #f)
                                        (apply (lambda (#{args\ 4412}#
                                                        #{q\ 4413}#)
                                                 (syntax-violation
                                                   'unquote-splicing
                                                   "unquote-splicing takes exactly one argument"
                                                   #{p\ 4398}#
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
                                                         #{args\ 4412}#)))
                                               #{tmp\ 4409}#)
                                        ((lambda (#{tmp\ 4414}#)
                                           (if #{tmp\ 4414}#
                                             (apply (lambda (#{p\ 4415}#)
                                                      (#{quasicons\ 4358}#
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
                                                        (#{quasi\ 4361}#
                                                          (list #{p\ 4415}#)
                                                          (+ #{lev\ 4399}#
                                                             1))))
                                                    #{tmp\ 4414}#)
                                             ((lambda (#{tmp\ 4416}#)
                                                (if #{tmp\ 4416}#
                                                  (apply (lambda (#{p\ 4417}#
                                                                  #{q\ 4418}#)
                                                           (#{quasicons\ 4358}#
                                                             (#{quasi\ 4361}#
                                                               #{p\ 4417}#
                                                               #{lev\ 4399}#)
                                                             (#{quasi\ 4361}#
                                                               #{q\ 4418}#
                                                               #{lev\ 4399}#)))
                                                         #{tmp\ 4416}#)
                                                  ((lambda (#{tmp\ 4419}#)
                                                     (if #{tmp\ 4419}#
                                                       (apply (lambda (#{x\ 4420}#)
                                                                (#{quasivector\ 4360}#
                                                                  (#{quasi\ 4361}#
                                                                    #{x\ 4420}#
                                                                    #{lev\ 4399}#)))
                                                              #{tmp\ 4419}#)
                                                       ((lambda (#{p\ 4422}#)
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
                                                                #{p\ 4422}#))
                                                        #{tmp\ 4400}#)))
                                                   ($sc-dispatch
                                                     #{tmp\ 4400}#
                                                     '#(vector each-any)))))
                                              ($sc-dispatch
                                                #{tmp\ 4400}#
                                                '(any . any)))))
                                         ($sc-dispatch
                                           #{tmp\ 4400}#
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
                                      #{tmp\ 4400}#
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
                                 #{tmp\ 4400}#
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
                            #{tmp\ 4400}#
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
                       #{tmp\ 4400}#
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
                  #{p\ 4398}#))))
      (lambda (#{x\ 4423}#)
        ((lambda (#{tmp\ 4424}#)
           ((lambda (#{tmp\ 4425}#)
              (if #{tmp\ 4425}#
                (apply (lambda (#{_\ 4426}# #{e\ 4427}#)
                         (#{quasi\ 4361}# #{e\ 4427}# 0))
                       #{tmp\ 4425}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 4424}#)))
            ($sc-dispatch #{tmp\ 4424}# (quote (any any)))))
         #{x\ 4423}#)))))

(define include
  (make-syncase-macro
    'macro
    (lambda (#{x\ 4428}#)
      (letrec ((#{read-file\ 4429}#
                 (lambda (#{fn\ 4430}# #{k\ 4431}#)
                   (let ((#{p\ 4432}# (open-input-file #{fn\ 4430}#)))
                     (letrec ((#{f\ 4433}#
                                (lambda (#{x\ 4434}#)
                                  (if (eof-object? #{x\ 4434}#)
                                    (begin
                                      (close-input-port #{p\ 4432}#)
                                      '())
                                    (cons (datum->syntax
                                            #{k\ 4431}#
                                            #{x\ 4434}#)
                                          (#{f\ 4433}# (read #{p\ 4432}#)))))))
                       (#{f\ 4433}# (read #{p\ 4432}#)))))))
        ((lambda (#{tmp\ 4435}#)
           ((lambda (#{tmp\ 4436}#)
              (if #{tmp\ 4436}#
                (apply (lambda (#{k\ 4437}# #{filename\ 4438}#)
                         (let ((#{fn\ 4439}#
                                 (syntax->datum #{filename\ 4438}#)))
                           ((lambda (#{tmp\ 4440}#)
                              ((lambda (#{tmp\ 4441}#)
                                 (if #{tmp\ 4441}#
                                   (apply (lambda (#{exp\ 4442}#)
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
                                                  #{exp\ 4442}#))
                                          #{tmp\ 4441}#)
                                   (syntax-violation
                                     #f
                                     "source expression failed to match any pattern"
                                     #{tmp\ 4440}#)))
                               ($sc-dispatch #{tmp\ 4440}# (quote each-any))))
                            (#{read-file\ 4429}# #{fn\ 4439}# #{k\ 4437}#))))
                       #{tmp\ 4436}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 4435}#)))
            ($sc-dispatch #{tmp\ 4435}# (quote (any any)))))
         #{x\ 4428}#)))))

(define include-from-path
  (make-syncase-macro
    'macro
    (lambda (#{x\ 4444}#)
      ((lambda (#{tmp\ 4445}#)
         ((lambda (#{tmp\ 4446}#)
            (if #{tmp\ 4446}#
              (apply (lambda (#{k\ 4447}# #{filename\ 4448}#)
                       (let ((#{fn\ 4449}# (syntax->datum #{filename\ 4448}#)))
                         ((lambda (#{tmp\ 4450}#)
                            ((lambda (#{fn\ 4451}#)
                               (list '#(syntax-object
                                        include
                                        ((top)
                                         #(ribcage #(fn) #((top)) #("i"))
                                         #(ribcage () () ())
                                         #(ribcage () () ())
                                         #(ribcage #(fn) #((top)) #("i"))
                                         #(ribcage
                                           #(k filename)
                                           #((top) (top))
                                           #("i" "i"))
                                         #(ribcage () () ())
                                         #(ribcage #(x) #((top)) #("i")))
                                        (hygiene guile))
                                     #{fn\ 4451}#))
                             #{tmp\ 4450}#))
                          (let ((#{t\ 4452}# (%search-load-path #{fn\ 4449}#)))
                            (if #{t\ 4452}#
                              #{t\ 4452}#
                              (syntax-violation
                                'include-from-path
                                "file not found in path"
                                #{x\ 4444}#
                                #{filename\ 4448}#))))))
                     #{tmp\ 4446}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 4445}#)))
          ($sc-dispatch #{tmp\ 4445}# (quote (any any)))))
       #{x\ 4444}#))))

(define unquote
  (make-syncase-macro
    'macro
    (lambda (#{x\ 4453}#)
      ((lambda (#{tmp\ 4454}#)
         ((lambda (#{tmp\ 4455}#)
            (if #{tmp\ 4455}#
              (apply (lambda (#{_\ 4456}# #{e\ 4457}#)
                       (syntax-violation
                         'unquote
                         "expression not valid outside of quasiquote"
                         #{x\ 4453}#))
                     #{tmp\ 4455}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 4454}#)))
          ($sc-dispatch #{tmp\ 4454}# (quote (any any)))))
       #{x\ 4453}#))))

(define unquote-splicing
  (make-syncase-macro
    'macro
    (lambda (#{x\ 4458}#)
      ((lambda (#{tmp\ 4459}#)
         ((lambda (#{tmp\ 4460}#)
            (if #{tmp\ 4460}#
              (apply (lambda (#{_\ 4461}# #{e\ 4462}#)
                       (syntax-violation
                         'unquote-splicing
                         "expression not valid outside of quasiquote"
                         #{x\ 4458}#))
                     #{tmp\ 4460}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 4459}#)))
          ($sc-dispatch #{tmp\ 4459}# (quote (any any)))))
       #{x\ 4458}#))))

(define case
  (make-extended-syncase-macro
    (module-ref (current-module) (quote case))
    'macro
    (lambda (#{x\ 4463}#)
      ((lambda (#{tmp\ 4464}#)
         ((lambda (#{tmp\ 4465}#)
            (if #{tmp\ 4465}#
              (apply (lambda (#{_\ 4466}#
                              #{e\ 4467}#
                              #{m1\ 4468}#
                              #{m2\ 4469}#)
                       ((lambda (#{tmp\ 4470}#)
                          ((lambda (#{body\ 4471}#)
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
                                               #{e\ 4467}#))
                                   #{body\ 4471}#))
                           #{tmp\ 4470}#))
                        (letrec ((#{f\ 4472}#
                                   (lambda (#{clause\ 4473}# #{clauses\ 4474}#)
                                     (if (null? #{clauses\ 4474}#)
                                       ((lambda (#{tmp\ 4476}#)
                                          ((lambda (#{tmp\ 4477}#)
                                             (if #{tmp\ 4477}#
                                               (apply (lambda (#{e1\ 4478}#
                                                               #{e2\ 4479}#)
                                                        (cons '#(syntax-object
                                                                 begin
                                                                 ((top)
                                                                  #(ribcage
                                                                    #(e1 e2)
                                                                    #((top)
                                                                      (top))
                                                                    #("i" "i"))
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
                                                              (cons #{e1\ 4478}#
                                                                    #{e2\ 4479}#)))
                                                      #{tmp\ 4477}#)
                                               ((lambda (#{tmp\ 4481}#)
                                                  (if #{tmp\ 4481}#
                                                    (apply (lambda (#{k\ 4482}#
                                                                    #{e1\ 4483}#
                                                                    #{e2\ 4484}#)
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
                                                                               #{k\ 4482}#))
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
                                                                         (cons #{e1\ 4483}#
                                                                               #{e2\ 4484}#))))
                                                           #{tmp\ 4481}#)
                                                    ((lambda (#{_\ 4487}#)
                                                       (syntax-violation
                                                         'case
                                                         "bad clause"
                                                         #{x\ 4463}#
                                                         #{clause\ 4473}#))
                                                     #{tmp\ 4476}#)))
                                                ($sc-dispatch
                                                  #{tmp\ 4476}#
                                                  '(each-any
                                                     any
                                                     .
                                                     each-any)))))
                                           ($sc-dispatch
                                             #{tmp\ 4476}#
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
                                        #{clause\ 4473}#)
                                       ((lambda (#{tmp\ 4488}#)
                                          ((lambda (#{rest\ 4489}#)
                                             ((lambda (#{tmp\ 4490}#)
                                                ((lambda (#{tmp\ 4491}#)
                                                   (if #{tmp\ 4491}#
                                                     (apply (lambda (#{k\ 4492}#
                                                                     #{e1\ 4493}#
                                                                     #{e2\ 4494}#)
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
                                                                                #{k\ 4492}#))
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
                                                                          (cons #{e1\ 4493}#
                                                                                #{e2\ 4494}#))
                                                                    #{rest\ 4489}#))
                                                            #{tmp\ 4491}#)
                                                     ((lambda (#{_\ 4497}#)
                                                        (syntax-violation
                                                          'case
                                                          "bad clause"
                                                          #{x\ 4463}#
                                                          #{clause\ 4473}#))
                                                      #{tmp\ 4490}#)))
                                                 ($sc-dispatch
                                                   #{tmp\ 4490}#
                                                   '(each-any
                                                      any
                                                      .
                                                      each-any))))
                                              #{clause\ 4473}#))
                                           #{tmp\ 4488}#))
                                        (#{f\ 4472}#
                                          (car #{clauses\ 4474}#)
                                          (cdr #{clauses\ 4474}#)))))))
                          (#{f\ 4472}# #{m1\ 4468}# #{m2\ 4469}#))))
                     #{tmp\ 4465}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 4464}#)))
          ($sc-dispatch
            #{tmp\ 4464}#
            '(any any any . each-any))))
       #{x\ 4463}#))))

(define identifier-syntax
  (make-syncase-macro
    'macro
    (lambda (#{x\ 4498}#)
      ((lambda (#{tmp\ 4499}#)
         ((lambda (#{tmp\ 4500}#)
            (if #{tmp\ 4500}#
              (apply (lambda (#{_\ 4501}# #{e\ 4502}#)
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
                                               #{e\ 4502}#))
                                   (list (cons #{_\ 4501}#
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
                                               (cons #{e\ 4502}#
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
                     #{tmp\ 4500}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 4499}#)))
          ($sc-dispatch #{tmp\ 4499}# (quote (any any)))))
       #{x\ 4498}#))))

(define define*
  (make-syncase-macro
    'macro
    (lambda (#{x\ 4503}#)
      ((lambda (#{tmp\ 4504}#)
         ((lambda (#{tmp\ 4505}#)
            (if #{tmp\ 4505}#
              (apply (lambda (#{dummy\ 4506}#
                              #{id\ 4507}#
                              #{args\ 4508}#
                              #{b0\ 4509}#
                              #{b1\ 4510}#)
                       (list '#(syntax-object
                                define
                                ((top)
                                 #(ribcage
                                   #(dummy id args b0 b1)
                                   #(("m" top) (top) (top) (top) (top))
                                   #("i" "i" "i" "i" "i"))
                                 #(ribcage () () ())
                                 #(ribcage #(x) #(("m" top)) #("i")))
                                (hygiene guile))
                             #{id\ 4507}#
                             (cons '#(syntax-object
                                      lambda*
                                      ((top)
                                       #(ribcage
                                         #(dummy id args b0 b1)
                                         #(("m" top) (top) (top) (top) (top))
                                         #("i" "i" "i" "i" "i"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #(("m" top)) #("i")))
                                      (hygiene guile))
                                   (cons #{args\ 4508}#
                                         (cons #{b0\ 4509}# #{b1\ 4510}#)))))
                     #{tmp\ 4505}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp\ 4504}#)))
          ($sc-dispatch
            #{tmp\ 4504}#
            '(any (any . any) any . each-any))))
       #{x\ 4503}#))))

