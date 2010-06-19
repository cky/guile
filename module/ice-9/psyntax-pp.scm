(eval-when (compile) (set-current-module (resolve-module (quote (guile)))))
(if #f #f)

(letrec*
  ((#{and-map*\ 2144}#
     (lambda (#{f\ 2316}# #{first\ 2317}# . #{rest\ 2318}#)
       (begin
         (let ((#{t\ 2324}# (null? #{first\ 2317}#)))
           (if #{t\ 2324}#
             #{t\ 2324}#
             (if (null? #{rest\ 2318}#)
               (letrec*
                 ((#{andmap\ 2328}#
                    (lambda (#{first\ 2329}#)
                      (begin
                        (let ((#{x\ 2332}# (car #{first\ 2329}#))
                              (#{first\ 2333}# (cdr #{first\ 2329}#)))
                          (if (null? #{first\ 2333}#)
                            (#{f\ 2316}# #{x\ 2332}#)
                            (if (#{f\ 2316}# #{x\ 2332}#)
                              (#{andmap\ 2328}# #{first\ 2333}#)
                              #f)))))))
                 (begin (#{andmap\ 2328}# #{first\ 2317}#)))
               (letrec*
                 ((#{andmap\ 2339}#
                    (lambda (#{first\ 2340}# #{rest\ 2341}#)
                      (begin
                        (let ((#{x\ 2346}# (car #{first\ 2340}#))
                              (#{xr\ 2347}# (map car #{rest\ 2341}#))
                              (#{first\ 2348}# (cdr #{first\ 2340}#))
                              (#{rest\ 2349}# (map cdr #{rest\ 2341}#)))
                          (if (null? #{first\ 2348}#)
                            (@apply
                              #{f\ 2316}#
                              (cons #{x\ 2346}# #{xr\ 2347}#))
                            (if (@apply
                                  #{f\ 2316}#
                                  (cons #{x\ 2346}# #{xr\ 2347}#))
                              (#{andmap\ 2339}# #{first\ 2348}# #{rest\ 2349}#)
                              #f)))))))
                 (begin
                   (#{andmap\ 2339}# #{first\ 2317}# #{rest\ 2318}#))))))))))
  (begin
    (let ((#{make-primitive-ref\ 2358}# (if #f #f))
          (#{fx+\ 2397}# (if #f #f))
          (#{fx-\ 2399}# (if #f #f))
          (#{fx=\ 2401}# (if #f #f))
          (#{fx<\ 2403}# (if #f #f))
          (#{set-syntax-object-expression!\ 2468}#
            (if #f #f))
          (#{set-syntax-object-wrap!\ 2470}# (if #f #f))
          (#{set-syntax-object-module!\ 2472}# (if #f #f))
          (#{binding-type\ 2479}# (if #f #f))
          (#{binding-value\ 2481}# (if #f #f))
          (#{make-wrap\ 2501}# (if #f #f))
          (#{wrap-marks\ 2503}# (if #f #f))
          (#{wrap-subst\ 2505}# (if #f #f))
          (#{ribcage?\ 2519}# (if #f #f)))
      (letrec*
        ((#{make-void\ 2354}#
           (lambda (#{src\ 2826}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 0)
               #{src\ 2826}#)))
         (#{make-const\ 2356}#
           (lambda (#{src\ 2828}# #{exp\ 2829}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 1)
               #{src\ 2828}#
               #{exp\ 2829}#)))
         (#{make-lexical-ref\ 2360}#
           (lambda (#{src\ 2836}# #{name\ 2837}# #{gensym\ 2838}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 3)
               #{src\ 2836}#
               #{name\ 2837}#
               #{gensym\ 2838}#)))
         (#{make-lexical-set\ 2362}#
           (lambda (#{src\ 2842}#
                    #{name\ 2843}#
                    #{gensym\ 2844}#
                    #{exp\ 2845}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 4)
               #{src\ 2842}#
               #{name\ 2843}#
               #{gensym\ 2844}#
               #{exp\ 2845}#)))
         (#{make-module-ref\ 2364}#
           (lambda (#{src\ 2850}#
                    #{mod\ 2851}#
                    #{name\ 2852}#
                    #{public?\ 2853}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 5)
               #{src\ 2850}#
               #{mod\ 2851}#
               #{name\ 2852}#
               #{public?\ 2853}#)))
         (#{make-module-set\ 2366}#
           (lambda (#{src\ 2858}#
                    #{mod\ 2859}#
                    #{name\ 2860}#
                    #{public?\ 2861}#
                    #{exp\ 2862}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 6)
               #{src\ 2858}#
               #{mod\ 2859}#
               #{name\ 2860}#
               #{public?\ 2861}#
               #{exp\ 2862}#)))
         (#{make-toplevel-ref\ 2368}#
           (lambda (#{src\ 2868}# #{name\ 2869}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 7)
               #{src\ 2868}#
               #{name\ 2869}#)))
         (#{make-toplevel-set\ 2370}#
           (lambda (#{src\ 2872}# #{name\ 2873}# #{exp\ 2874}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 8)
               #{src\ 2872}#
               #{name\ 2873}#
               #{exp\ 2874}#)))
         (#{make-toplevel-define\ 2372}#
           (lambda (#{src\ 2878}# #{name\ 2879}# #{exp\ 2880}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 9)
               #{src\ 2878}#
               #{name\ 2879}#
               #{exp\ 2880}#)))
         (#{make-conditional\ 2374}#
           (lambda (#{src\ 2884}#
                    #{test\ 2885}#
                    #{consequent\ 2886}#
                    #{alternate\ 2887}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 10)
               #{src\ 2884}#
               #{test\ 2885}#
               #{consequent\ 2886}#
               #{alternate\ 2887}#)))
         (#{make-application\ 2376}#
           (lambda (#{src\ 2892}# #{proc\ 2893}# #{args\ 2894}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 11)
               #{src\ 2892}#
               #{proc\ 2893}#
               #{args\ 2894}#)))
         (#{make-sequence\ 2378}#
           (lambda (#{src\ 2898}# #{exps\ 2899}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 12)
               #{src\ 2898}#
               #{exps\ 2899}#)))
         (#{make-lambda\ 2380}#
           (lambda (#{src\ 2902}# #{meta\ 2903}# #{body\ 2904}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 13)
               #{src\ 2902}#
               #{meta\ 2903}#
               #{body\ 2904}#)))
         (#{make-lambda-case\ 2382}#
           (lambda (#{src\ 2908}#
                    #{req\ 2909}#
                    #{opt\ 2910}#
                    #{rest\ 2911}#
                    #{kw\ 2912}#
                    #{inits\ 2913}#
                    #{gensyms\ 2914}#
                    #{body\ 2915}#
                    #{alternate\ 2916}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 14)
               #{src\ 2908}#
               #{req\ 2909}#
               #{opt\ 2910}#
               #{rest\ 2911}#
               #{kw\ 2912}#
               #{inits\ 2913}#
               #{gensyms\ 2914}#
               #{body\ 2915}#
               #{alternate\ 2916}#)))
         (#{make-let\ 2384}#
           (lambda (#{src\ 2926}#
                    #{names\ 2927}#
                    #{gensyms\ 2928}#
                    #{vals\ 2929}#
                    #{body\ 2930}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 15)
               #{src\ 2926}#
               #{names\ 2927}#
               #{gensyms\ 2928}#
               #{vals\ 2929}#
               #{body\ 2930}#)))
         (#{make-letrec\ 2386}#
           (lambda (#{src\ 2936}#
                    #{in-order?\ 2937}#
                    #{names\ 2938}#
                    #{gensyms\ 2939}#
                    #{vals\ 2940}#
                    #{body\ 2941}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 16)
               #{src\ 2936}#
               #{in-order?\ 2937}#
               #{names\ 2938}#
               #{gensyms\ 2939}#
               #{vals\ 2940}#
               #{body\ 2941}#)))
         (#{make-dynlet\ 2388}#
           (lambda (#{src\ 2948}#
                    #{fluids\ 2949}#
                    #{vals\ 2950}#
                    #{body\ 2951}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 17)
               #{src\ 2948}#
               #{fluids\ 2949}#
               #{vals\ 2950}#
               #{body\ 2951}#)))
         (#{lambda?\ 2391}#
           (lambda (#{x\ 2956}#)
             (if (struct? #{x\ 2956}#)
               (eq? (struct-vtable #{x\ 2956}#)
                    (vector-ref %expanded-vtables 13))
               #f)))
         (#{lambda-meta\ 2393}#
           (lambda (#{x\ 2960}#) (struct-ref #{x\ 2960}# 1)))
         (#{set-lambda-meta!\ 2395}#
           (lambda (#{x\ 2962}# #{v\ 2963}#)
             (struct-set! #{x\ 2962}# 1 #{v\ 2963}#)))
         (#{top-level-eval-hook\ 2405}#
           (lambda (#{x\ 2966}# #{mod\ 2967}#)
             (primitive-eval #{x\ 2966}#)))
         (#{local-eval-hook\ 2407}#
           (lambda (#{x\ 2970}# #{mod\ 2971}#)
             (primitive-eval #{x\ 2970}#)))
         (#{put-global-definition-hook\ 2410}#
           (lambda (#{symbol\ 2974}# #{type\ 2975}# #{val\ 2976}#)
             (module-define!
               (current-module)
               #{symbol\ 2974}#
               (make-syntax-transformer
                 #{symbol\ 2974}#
                 #{type\ 2975}#
                 #{val\ 2976}#))))
         (#{get-global-definition-hook\ 2412}#
           (lambda (#{symbol\ 2980}# #{module\ 2981}#)
             (begin
               (if (if (not #{module\ 2981}#) (current-module) #f)
                 (warn "module system is booted, we should have a module"
                       #{symbol\ 2980}#))
               (begin
                 (let ((#{v\ 2987}#
                         (module-variable
                           (if #{module\ 2981}#
                             (resolve-module (cdr #{module\ 2981}#))
                             (current-module))
                           #{symbol\ 2980}#)))
                   (if #{v\ 2987}#
                     (if (variable-bound? #{v\ 2987}#)
                       (begin
                         (let ((#{val\ 2992}# (variable-ref #{v\ 2987}#)))
                           (if (macro? #{val\ 2992}#)
                             (if (macro-type #{val\ 2992}#)
                               (cons (macro-type #{val\ 2992}#)
                                     (macro-binding #{val\ 2992}#))
                               #f)
                             #f)))
                       #f)
                     #f))))))
         (#{decorate-source\ 2414}#
           (lambda (#{e\ 2996}# #{s\ 2997}#)
             (begin
               (if (if (pair? #{e\ 2996}#) #{s\ 2997}# #f)
                 (set-source-properties! #{e\ 2996}# #{s\ 2997}#))
               #{e\ 2996}#)))
         (#{maybe-name-value!\ 2416}#
           (lambda (#{name\ 3002}# #{val\ 3003}#)
             (if (#{lambda?\ 2391}# #{val\ 3003}#)
               (begin
                 (let ((#{meta\ 3007}#
                         (#{lambda-meta\ 2393}# #{val\ 3003}#)))
                   (if (not (assq (quote name) #{meta\ 3007}#))
                     (#{set-lambda-meta!\ 2395}#
                       #{val\ 3003}#
                       (cons (cons (quote name) #{name\ 3002}#)
                             #{meta\ 3007}#))))))))
         (#{build-void\ 2418}#
           (lambda (#{source\ 3008}#)
             (#{make-void\ 2354}# #{source\ 3008}#)))
         (#{build-application\ 2420}#
           (lambda (#{source\ 3010}#
                    #{fun-exp\ 3011}#
                    #{arg-exps\ 3012}#)
             (#{make-application\ 2376}#
               #{source\ 3010}#
               #{fun-exp\ 3011}#
               #{arg-exps\ 3012}#)))
         (#{build-conditional\ 2422}#
           (lambda (#{source\ 3016}#
                    #{test-exp\ 3017}#
                    #{then-exp\ 3018}#
                    #{else-exp\ 3019}#)
             (#{make-conditional\ 2374}#
               #{source\ 3016}#
               #{test-exp\ 3017}#
               #{then-exp\ 3018}#
               #{else-exp\ 3019}#)))
         (#{build-dynlet\ 2424}#
           (lambda (#{source\ 3024}#
                    #{fluids\ 3025}#
                    #{vals\ 3026}#
                    #{body\ 3027}#)
             (#{make-dynlet\ 2388}#
               #{source\ 3024}#
               #{fluids\ 3025}#
               #{vals\ 3026}#
               #{body\ 3027}#)))
         (#{build-lexical-reference\ 2426}#
           (lambda (#{type\ 3032}#
                    #{source\ 3033}#
                    #{name\ 3034}#
                    #{var\ 3035}#)
             (#{make-lexical-ref\ 2360}#
               #{source\ 3033}#
               #{name\ 3034}#
               #{var\ 3035}#)))
         (#{build-lexical-assignment\ 2428}#
           (lambda (#{source\ 3040}#
                    #{name\ 3041}#
                    #{var\ 3042}#
                    #{exp\ 3043}#)
             (begin
               (#{maybe-name-value!\ 2416}#
                 #{name\ 3041}#
                 #{exp\ 3043}#)
               (#{make-lexical-set\ 2362}#
                 #{source\ 3040}#
                 #{name\ 3041}#
                 #{var\ 3042}#
                 #{exp\ 3043}#))))
         (#{analyze-variable\ 2430}#
           (lambda (#{mod\ 3048}#
                    #{var\ 3049}#
                    #{modref-cont\ 3050}#
                    #{bare-cont\ 3051}#)
             (if (not #{mod\ 3048}#)
               (#{bare-cont\ 3051}# #{var\ 3049}#)
               (begin
                 (let ((#{kind\ 3058}# (car #{mod\ 3048}#))
                       (#{mod\ 3059}# (cdr #{mod\ 3048}#)))
                   (if (eqv? #{kind\ 3058}# (quote public))
                     (#{modref-cont\ 3050}#
                       #{mod\ 3059}#
                       #{var\ 3049}#
                       #t)
                     (if (eqv? #{kind\ 3058}# (quote private))
                       (if (not (equal?
                                  #{mod\ 3059}#
                                  (module-name (current-module))))
                         (#{modref-cont\ 3050}#
                           #{mod\ 3059}#
                           #{var\ 3049}#
                           #f)
                         (#{bare-cont\ 3051}# #{var\ 3049}#))
                       (if (eqv? #{kind\ 3058}# (quote bare))
                         (#{bare-cont\ 3051}# #{var\ 3049}#)
                         (if (eqv? #{kind\ 3058}# (quote hygiene))
                           (if (if (not (equal?
                                          #{mod\ 3059}#
                                          (module-name (current-module))))
                                 (module-variable
                                   (resolve-module #{mod\ 3059}#)
                                   #{var\ 3049}#)
                                 #f)
                             (#{modref-cont\ 3050}#
                               #{mod\ 3059}#
                               #{var\ 3049}#
                               #f)
                             (#{bare-cont\ 3051}# #{var\ 3049}#))
                           (syntax-violation
                             #f
                             "bad module kind"
                             #{var\ 3049}#
                             #{mod\ 3059}#))))))))))
         (#{build-global-reference\ 2432}#
           (lambda (#{source\ 3067}# #{var\ 3068}# #{mod\ 3069}#)
             (#{analyze-variable\ 2430}#
               #{mod\ 3069}#
               #{var\ 3068}#
               (lambda (#{mod\ 3073}# #{var\ 3074}# #{public?\ 3075}#)
                 (#{make-module-ref\ 2364}#
                   #{source\ 3067}#
                   #{mod\ 3073}#
                   #{var\ 3074}#
                   #{public?\ 3075}#))
               (lambda (#{var\ 3079}#)
                 (#{make-toplevel-ref\ 2368}#
                   #{source\ 3067}#
                   #{var\ 3079}#)))))
         (#{build-global-assignment\ 2434}#
           (lambda (#{source\ 3081}#
                    #{var\ 3082}#
                    #{exp\ 3083}#
                    #{mod\ 3084}#)
             (begin
               (#{maybe-name-value!\ 2416}#
                 #{var\ 3082}#
                 #{exp\ 3083}#)
               (#{analyze-variable\ 2430}#
                 #{mod\ 3084}#
                 #{var\ 3082}#
                 (lambda (#{mod\ 3089}# #{var\ 3090}# #{public?\ 3091}#)
                   (#{make-module-set\ 2366}#
                     #{source\ 3081}#
                     #{mod\ 3089}#
                     #{var\ 3090}#
                     #{public?\ 3091}#
                     #{exp\ 3083}#))
                 (lambda (#{var\ 3095}#)
                   (#{make-toplevel-set\ 2370}#
                     #{source\ 3081}#
                     #{var\ 3095}#
                     #{exp\ 3083}#))))))
         (#{build-global-definition\ 2436}#
           (lambda (#{source\ 3097}# #{var\ 3098}# #{exp\ 3099}#)
             (begin
               (#{maybe-name-value!\ 2416}#
                 #{var\ 3098}#
                 #{exp\ 3099}#)
               (#{make-toplevel-define\ 2372}#
                 #{source\ 3097}#
                 #{var\ 3098}#
                 #{exp\ 3099}#))))
         (#{build-simple-lambda\ 2438}#
           (lambda (#{src\ 3103}#
                    #{req\ 3104}#
                    #{rest\ 3105}#
                    #{vars\ 3106}#
                    #{meta\ 3107}#
                    #{exp\ 3108}#)
             (#{make-lambda\ 2380}#
               #{src\ 3103}#
               #{meta\ 3107}#
               (#{make-lambda-case\ 2382}#
                 #{src\ 3103}#
                 #{req\ 3104}#
                 #f
                 #{rest\ 3105}#
                 #f
                 '()
                 #{vars\ 3106}#
                 #{exp\ 3108}#
                 #f))))
         (#{build-case-lambda\ 2440}#
           (lambda (#{src\ 3115}# #{meta\ 3116}# #{body\ 3117}#)
             (#{make-lambda\ 2380}#
               #{src\ 3115}#
               #{meta\ 3116}#
               #{body\ 3117}#)))
         (#{build-lambda-case\ 2442}#
           (lambda (#{src\ 3121}#
                    #{req\ 3122}#
                    #{opt\ 3123}#
                    #{rest\ 3124}#
                    #{kw\ 3125}#
                    #{inits\ 3126}#
                    #{vars\ 3127}#
                    #{body\ 3128}#
                    #{else-case\ 3129}#)
             (#{make-lambda-case\ 2382}#
               #{src\ 3121}#
               #{req\ 3122}#
               #{opt\ 3123}#
               #{rest\ 3124}#
               #{kw\ 3125}#
               #{inits\ 3126}#
               #{vars\ 3127}#
               #{body\ 3128}#
               #{else-case\ 3129}#)))
         (#{build-primref\ 2444}#
           (lambda (#{src\ 3139}# #{name\ 3140}#)
             (if (equal?
                   (module-name (current-module))
                   '(guile))
               (#{make-toplevel-ref\ 2368}#
                 #{src\ 3139}#
                 #{name\ 3140}#)
               (#{make-module-ref\ 2364}#
                 #{src\ 3139}#
                 '(guile)
                 #{name\ 3140}#
                 #f))))
         (#{build-data\ 2446}#
           (lambda (#{src\ 3143}# #{exp\ 3144}#)
             (#{make-const\ 2356}#
               #{src\ 3143}#
               #{exp\ 3144}#)))
         (#{build-sequence\ 2448}#
           (lambda (#{src\ 3147}# #{exps\ 3148}#)
             (if (null? (cdr #{exps\ 3148}#))
               (car #{exps\ 3148}#)
               (#{make-sequence\ 2378}#
                 #{src\ 3147}#
                 #{exps\ 3148}#))))
         (#{build-let\ 2450}#
           (lambda (#{src\ 3151}#
                    #{ids\ 3152}#
                    #{vars\ 3153}#
                    #{val-exps\ 3154}#
                    #{body-exp\ 3155}#)
             (begin
               (for-each
                 #{maybe-name-value!\ 2416}#
                 #{ids\ 3152}#
                 #{val-exps\ 3154}#)
               (if (null? #{vars\ 3153}#)
                 #{body-exp\ 3155}#
                 (#{make-let\ 2384}#
                   #{src\ 3151}#
                   #{ids\ 3152}#
                   #{vars\ 3153}#
                   #{val-exps\ 3154}#
                   #{body-exp\ 3155}#)))))
         (#{build-named-let\ 2452}#
           (lambda (#{src\ 3161}#
                    #{ids\ 3162}#
                    #{vars\ 3163}#
                    #{val-exps\ 3164}#
                    #{body-exp\ 3165}#)
             (begin
               (let ((#{f\ 3175}# (car #{vars\ 3163}#))
                     (#{f-name\ 3176}# (car #{ids\ 3162}#))
                     (#{vars\ 3177}# (cdr #{vars\ 3163}#))
                     (#{ids\ 3178}# (cdr #{ids\ 3162}#)))
                 (begin
                   (let ((#{proc\ 3180}#
                           (#{build-simple-lambda\ 2438}#
                             #{src\ 3161}#
                             #{ids\ 3178}#
                             #f
                             #{vars\ 3177}#
                             '()
                             #{body-exp\ 3165}#)))
                     (begin
                       (#{maybe-name-value!\ 2416}#
                         #{f-name\ 3176}#
                         #{proc\ 3180}#)
                       (for-each
                         #{maybe-name-value!\ 2416}#
                         #{ids\ 3178}#
                         #{val-exps\ 3164}#)
                       (#{make-letrec\ 2386}#
                         #{src\ 3161}#
                         #f
                         (list #{f-name\ 3176}#)
                         (list #{f\ 3175}#)
                         (list #{proc\ 3180}#)
                         (#{build-application\ 2420}#
                           #{src\ 3161}#
                           (#{build-lexical-reference\ 2426}#
                             'fun
                             #{src\ 3161}#
                             #{f-name\ 3176}#
                             #{f\ 3175}#)
                           #{val-exps\ 3164}#)))))))))
         (#{build-letrec\ 2454}#
           (lambda (#{src\ 3181}#
                    #{in-order?\ 3182}#
                    #{ids\ 3183}#
                    #{vars\ 3184}#
                    #{val-exps\ 3185}#
                    #{body-exp\ 3186}#)
             (if (null? #{vars\ 3184}#)
               #{body-exp\ 3186}#
               (begin
                 (for-each
                   #{maybe-name-value!\ 2416}#
                   #{ids\ 3183}#
                   #{val-exps\ 3185}#)
                 (#{make-letrec\ 2386}#
                   #{src\ 3181}#
                   #{in-order?\ 3182}#
                   #{ids\ 3183}#
                   #{vars\ 3184}#
                   #{val-exps\ 3185}#
                   #{body-exp\ 3186}#)))))
         (#{make-syntax-object\ 2458}#
           (lambda (#{expression\ 3193}#
                    #{wrap\ 3194}#
                    #{module\ 3195}#)
             (vector
               'syntax-object
               #{expression\ 3193}#
               #{wrap\ 3194}#
               #{module\ 3195}#)))
         (#{syntax-object?\ 2460}#
           (lambda (#{x\ 3199}#)
             (if (vector? #{x\ 3199}#)
               (if (= (vector-length #{x\ 3199}#) 4)
                 (eq? (vector-ref #{x\ 3199}# 0)
                      'syntax-object)
                 #f)
               #f)))
         (#{syntax-object-expression\ 2462}#
           (lambda (#{x\ 3204}#) (vector-ref #{x\ 3204}# 1)))
         (#{syntax-object-wrap\ 2464}#
           (lambda (#{x\ 3206}#) (vector-ref #{x\ 3206}# 2)))
         (#{syntax-object-module\ 2466}#
           (lambda (#{x\ 3208}#) (vector-ref #{x\ 3208}# 3)))
         (#{source-annotation\ 2475}#
           (lambda (#{x\ 3222}#)
             (if (#{syntax-object?\ 2460}# #{x\ 3222}#)
               (#{source-annotation\ 2475}#
                 (#{syntax-object-expression\ 2462}# #{x\ 3222}#))
               (if (pair? #{x\ 3222}#)
                 (begin
                   (let ((#{props\ 3229}# (source-properties #{x\ 3222}#)))
                     (if (pair? #{props\ 3229}#) #{props\ 3229}# #f)))
                 #f))))
         (#{extend-env\ 2484}#
           (lambda (#{labels\ 3231}# #{bindings\ 3232}# #{r\ 3233}#)
             (if (null? #{labels\ 3231}#)
               #{r\ 3233}#
               (#{extend-env\ 2484}#
                 (cdr #{labels\ 3231}#)
                 (cdr #{bindings\ 3232}#)
                 (cons (cons (car #{labels\ 3231}#)
                             (car #{bindings\ 3232}#))
                       #{r\ 3233}#)))))
         (#{extend-var-env\ 2486}#
           (lambda (#{labels\ 3237}# #{vars\ 3238}# #{r\ 3239}#)
             (if (null? #{labels\ 3237}#)
               #{r\ 3239}#
               (#{extend-var-env\ 2486}#
                 (cdr #{labels\ 3237}#)
                 (cdr #{vars\ 3238}#)
                 (cons (cons (car #{labels\ 3237}#)
                             (cons (quote lexical) (car #{vars\ 3238}#)))
                       #{r\ 3239}#)))))
         (#{macros-only-env\ 2488}#
           (lambda (#{r\ 3244}#)
             (if (null? #{r\ 3244}#)
               '()
               (begin
                 (let ((#{a\ 3247}# (car #{r\ 3244}#)))
                   (if (eq? (car (cdr #{a\ 3247}#)) (quote macro))
                     (cons #{a\ 3247}#
                           (#{macros-only-env\ 2488}# (cdr #{r\ 3244}#)))
                     (#{macros-only-env\ 2488}# (cdr #{r\ 3244}#))))))))
         (#{lookup\ 2490}#
           (lambda (#{x\ 3248}# #{r\ 3249}# #{mod\ 3250}#)
             (begin
               (let ((#{t\ 3256}# (assq #{x\ 3248}# #{r\ 3249}#)))
                 (if #{t\ 3256}#
                   (cdr #{t\ 3256}#)
                   (if (symbol? #{x\ 3248}#)
                     (begin
                       (let ((#{t\ 3262}#
                               (#{get-global-definition-hook\ 2412}#
                                 #{x\ 3248}#
                                 #{mod\ 3250}#)))
                         (if #{t\ 3262}# #{t\ 3262}# (quote (global)))))
                     '(displaced-lexical)))))))
         (#{global-extend\ 2492}#
           (lambda (#{type\ 3267}# #{sym\ 3268}# #{val\ 3269}#)
             (#{put-global-definition-hook\ 2410}#
               #{sym\ 3268}#
               #{type\ 3267}#
               #{val\ 3269}#)))
         (#{nonsymbol-id?\ 2494}#
           (lambda (#{x\ 3273}#)
             (if (#{syntax-object?\ 2460}# #{x\ 3273}#)
               (symbol?
                 (#{syntax-object-expression\ 2462}# #{x\ 3273}#))
               #f)))
         (#{id?\ 2496}#
           (lambda (#{x\ 3277}#)
             (if (symbol? #{x\ 3277}#)
               #t
               (if (#{syntax-object?\ 2460}# #{x\ 3277}#)
                 (symbol?
                   (#{syntax-object-expression\ 2462}# #{x\ 3277}#))
                 #f))))
         (#{id-sym-name&marks\ 2499}#
           (lambda (#{x\ 3284}# #{w\ 3285}#)
             (if (#{syntax-object?\ 2460}# #{x\ 3284}#)
               (values
                 (#{syntax-object-expression\ 2462}# #{x\ 3284}#)
                 (#{join-marks\ 2549}#
                   (#{wrap-marks\ 2503}# #{w\ 3285}#)
                   (#{wrap-marks\ 2503}#
                     (#{syntax-object-wrap\ 2464}# #{x\ 3284}#))))
               (values
                 #{x\ 3284}#
                 (#{wrap-marks\ 2503}# #{w\ 3285}#)))))
         (#{gen-label\ 2512}#
           (lambda () (symbol->string (gensym "i"))))
         (#{gen-labels\ 2514}#
           (lambda (#{ls\ 3288}#)
             (if (null? #{ls\ 3288}#)
               '()
               (cons (#{gen-label\ 2512}#)
                     (#{gen-labels\ 2514}# (cdr #{ls\ 3288}#))))))
         (#{make-ribcage\ 2517}#
           (lambda (#{symnames\ 3290}#
                    #{marks\ 3291}#
                    #{labels\ 3292}#)
             (vector
               'ribcage
               #{symnames\ 3290}#
               #{marks\ 3291}#
               #{labels\ 3292}#)))
         (#{ribcage-symnames\ 2521}#
           (lambda (#{x\ 3301}#) (vector-ref #{x\ 3301}# 1)))
         (#{ribcage-marks\ 2523}#
           (lambda (#{x\ 3303}#) (vector-ref #{x\ 3303}# 2)))
         (#{ribcage-labels\ 2525}#
           (lambda (#{x\ 3305}#) (vector-ref #{x\ 3305}# 3)))
         (#{set-ribcage-symnames!\ 2527}#
           (lambda (#{x\ 3307}# #{update\ 3308}#)
             (vector-set! #{x\ 3307}# 1 #{update\ 3308}#)))
         (#{set-ribcage-marks!\ 2529}#
           (lambda (#{x\ 3311}# #{update\ 3312}#)
             (vector-set! #{x\ 3311}# 2 #{update\ 3312}#)))
         (#{set-ribcage-labels!\ 2531}#
           (lambda (#{x\ 3315}# #{update\ 3316}#)
             (vector-set! #{x\ 3315}# 3 #{update\ 3316}#)))
         (#{anti-mark\ 2537}#
           (lambda (#{w\ 3319}#)
             (#{make-wrap\ 2501}#
               (cons #f (#{wrap-marks\ 2503}# #{w\ 3319}#))
               (cons 'shift
                     (#{wrap-subst\ 2505}# #{w\ 3319}#)))))
         (#{extend-ribcage!\ 2541}#
           (lambda (#{ribcage\ 3322}# #{id\ 3323}# #{label\ 3324}#)
             (begin
               (#{set-ribcage-symnames!\ 2527}#
                 #{ribcage\ 3322}#
                 (cons (#{syntax-object-expression\ 2462}# #{id\ 3323}#)
                       (#{ribcage-symnames\ 2521}# #{ribcage\ 3322}#)))
               (#{set-ribcage-marks!\ 2529}#
                 #{ribcage\ 3322}#
                 (cons (#{wrap-marks\ 2503}#
                         (#{syntax-object-wrap\ 2464}# #{id\ 3323}#))
                       (#{ribcage-marks\ 2523}# #{ribcage\ 3322}#)))
               (#{set-ribcage-labels!\ 2531}#
                 #{ribcage\ 3322}#
                 (cons #{label\ 3324}#
                       (#{ribcage-labels\ 2525}# #{ribcage\ 3322}#))))))
         (#{make-binding-wrap\ 2543}#
           (lambda (#{ids\ 3328}# #{labels\ 3329}# #{w\ 3330}#)
             (if (null? #{ids\ 3328}#)
               #{w\ 3330}#
               (#{make-wrap\ 2501}#
                 (#{wrap-marks\ 2503}# #{w\ 3330}#)
                 (cons (begin
                         (let ((#{labelvec\ 3335}#
                                 (list->vector #{labels\ 3329}#)))
                           (begin
                             (let ((#{n\ 3337}#
                                     (vector-length #{labelvec\ 3335}#)))
                               (begin
                                 (let ((#{symnamevec\ 3340}#
                                         (make-vector #{n\ 3337}#))
                                       (#{marksvec\ 3341}#
                                         (make-vector #{n\ 3337}#)))
                                   (begin
                                     (letrec*
                                       ((#{f\ 3345}#
                                          (lambda (#{ids\ 3346}# #{i\ 3347}#)
                                            (if (not (null? #{ids\ 3346}#))
                                              (call-with-values
                                                (lambda ()
                                                  (#{id-sym-name&marks\ 2499}#
                                                    (car #{ids\ 3346}#)
                                                    #{w\ 3330}#))
                                                (lambda (#{symname\ 3348}#
                                                         #{marks\ 3349}#)
                                                  (begin
                                                    (vector-set!
                                                      #{symnamevec\ 3340}#
                                                      #{i\ 3347}#
                                                      #{symname\ 3348}#)
                                                    (vector-set!
                                                      #{marksvec\ 3341}#
                                                      #{i\ 3347}#
                                                      #{marks\ 3349}#)
                                                    (#{f\ 3345}#
                                                      (cdr #{ids\ 3346}#)
                                                      (#{fx+\ 2397}#
                                                        #{i\ 3347}#
                                                        1)))))))))
                                       (begin (#{f\ 3345}# #{ids\ 3328}# 0)))
                                     (#{make-ribcage\ 2517}#
                                       #{symnamevec\ 3340}#
                                       #{marksvec\ 3341}#
                                       #{labelvec\ 3335}#))))))))
                       (#{wrap-subst\ 2505}# #{w\ 3330}#))))))
         (#{smart-append\ 2545}#
           (lambda (#{m1\ 3352}# #{m2\ 3353}#)
             (if (null? #{m2\ 3353}#)
               #{m1\ 3352}#
               (append #{m1\ 3352}# #{m2\ 3353}#))))
         (#{join-wraps\ 2547}#
           (lambda (#{w1\ 3356}# #{w2\ 3357}#)
             (begin
               (let ((#{m1\ 3362}#
                       (#{wrap-marks\ 2503}# #{w1\ 3356}#))
                     (#{s1\ 3363}#
                       (#{wrap-subst\ 2505}# #{w1\ 3356}#)))
                 (if (null? #{m1\ 3362}#)
                   (if (null? #{s1\ 3363}#)
                     #{w2\ 3357}#
                     (#{make-wrap\ 2501}#
                       (#{wrap-marks\ 2503}# #{w2\ 3357}#)
                       (#{smart-append\ 2545}#
                         #{s1\ 3363}#
                         (#{wrap-subst\ 2505}# #{w2\ 3357}#))))
                   (#{make-wrap\ 2501}#
                     (#{smart-append\ 2545}#
                       #{m1\ 3362}#
                       (#{wrap-marks\ 2503}# #{w2\ 3357}#))
                     (#{smart-append\ 2545}#
                       #{s1\ 3363}#
                       (#{wrap-subst\ 2505}# #{w2\ 3357}#))))))))
         (#{join-marks\ 2549}#
           (lambda (#{m1\ 3364}# #{m2\ 3365}#)
             (#{smart-append\ 2545}#
               #{m1\ 3364}#
               #{m2\ 3365}#)))
         (#{same-marks?\ 2551}#
           (lambda (#{x\ 3368}# #{y\ 3369}#)
             (begin
               (let ((#{t\ 3374}# (eq? #{x\ 3368}# #{y\ 3369}#)))
                 (if #{t\ 3374}#
                   #{t\ 3374}#
                   (if (not (null? #{x\ 3368}#))
                     (if (not (null? #{y\ 3369}#))
                       (if (eq? (car #{x\ 3368}#) (car #{y\ 3369}#))
                         (#{same-marks?\ 2551}#
                           (cdr #{x\ 3368}#)
                           (cdr #{y\ 3369}#))
                         #f)
                       #f)
                     #f))))))
         (#{id-var-name\ 2553}#
           (lambda (#{id\ 3380}# #{w\ 3381}#)
             (letrec*
               ((#{search\ 3386}#
                  (lambda (#{sym\ 3402}# #{subst\ 3403}# #{marks\ 3404}#)
                    (if (null? #{subst\ 3403}#)
                      (values #f #{marks\ 3404}#)
                      (begin
                        (let ((#{fst\ 3409}# (car #{subst\ 3403}#)))
                          (if (eq? #{fst\ 3409}# (quote shift))
                            (#{search\ 3386}#
                              #{sym\ 3402}#
                              (cdr #{subst\ 3403}#)
                              (cdr #{marks\ 3404}#))
                            (begin
                              (let ((#{symnames\ 3411}#
                                      (#{ribcage-symnames\ 2521}#
                                        #{fst\ 3409}#)))
                                (if (vector? #{symnames\ 3411}#)
                                  (#{search-vector-rib\ 3390}#
                                    #{sym\ 3402}#
                                    #{subst\ 3403}#
                                    #{marks\ 3404}#
                                    #{symnames\ 3411}#
                                    #{fst\ 3409}#)
                                  (#{search-list-rib\ 3388}#
                                    #{sym\ 3402}#
                                    #{subst\ 3403}#
                                    #{marks\ 3404}#
                                    #{symnames\ 3411}#
                                    #{fst\ 3409}#))))))))))
                (#{search-list-rib\ 3388}#
                  (lambda (#{sym\ 3412}#
                           #{subst\ 3413}#
                           #{marks\ 3414}#
                           #{symnames\ 3415}#
                           #{ribcage\ 3416}#)
                    (letrec*
                      ((#{f\ 3425}#
                         (lambda (#{symnames\ 3426}# #{i\ 3427}#)
                           (if (null? #{symnames\ 3426}#)
                             (#{search\ 3386}#
                               #{sym\ 3412}#
                               (cdr #{subst\ 3413}#)
                               #{marks\ 3414}#)
                             (if (if (eq? (car #{symnames\ 3426}#)
                                          #{sym\ 3412}#)
                                   (#{same-marks?\ 2551}#
                                     #{marks\ 3414}#
                                     (list-ref
                                       (#{ribcage-marks\ 2523}#
                                         #{ribcage\ 3416}#)
                                       #{i\ 3427}#))
                                   #f)
                               (values
                                 (list-ref
                                   (#{ribcage-labels\ 2525}# #{ribcage\ 3416}#)
                                   #{i\ 3427}#)
                                 #{marks\ 3414}#)
                               (#{f\ 3425}#
                                 (cdr #{symnames\ 3426}#)
                                 (#{fx+\ 2397}# #{i\ 3427}# 1)))))))
                      (begin (#{f\ 3425}# #{symnames\ 3415}# 0)))))
                (#{search-vector-rib\ 3390}#
                  (lambda (#{sym\ 3435}#
                           #{subst\ 3436}#
                           #{marks\ 3437}#
                           #{symnames\ 3438}#
                           #{ribcage\ 3439}#)
                    (begin
                      (let ((#{n\ 3446}# (vector-length #{symnames\ 3438}#)))
                        (letrec*
                          ((#{f\ 3449}#
                             (lambda (#{i\ 3450}#)
                               (if (#{fx=\ 2401}# #{i\ 3450}# #{n\ 3446}#)
                                 (#{search\ 3386}#
                                   #{sym\ 3435}#
                                   (cdr #{subst\ 3436}#)
                                   #{marks\ 3437}#)
                                 (if (if (eq? (vector-ref
                                                #{symnames\ 3438}#
                                                #{i\ 3450}#)
                                              #{sym\ 3435}#)
                                       (#{same-marks?\ 2551}#
                                         #{marks\ 3437}#
                                         (vector-ref
                                           (#{ribcage-marks\ 2523}#
                                             #{ribcage\ 3439}#)
                                           #{i\ 3450}#))
                                       #f)
                                   (values
                                     (vector-ref
                                       (#{ribcage-labels\ 2525}#
                                         #{ribcage\ 3439}#)
                                       #{i\ 3450}#)
                                     #{marks\ 3437}#)
                                   (#{f\ 3449}#
                                     (#{fx+\ 2397}# #{i\ 3450}# 1)))))))
                          (begin (#{f\ 3449}# 0))))))))
               (begin
                 (if (symbol? #{id\ 3380}#)
                   (begin
                     (let ((#{t\ 3460}#
                             (call-with-values
                               (lambda ()
                                 (#{search\ 3386}#
                                   #{id\ 3380}#
                                   (#{wrap-subst\ 2505}# #{w\ 3381}#)
                                   (#{wrap-marks\ 2503}# #{w\ 3381}#)))
                               (lambda (#{x\ 3462}# . #{ignore\ 3463}#)
                                 #{x\ 3462}#))))
                       (if #{t\ 3460}# #{t\ 3460}# #{id\ 3380}#)))
                   (if (#{syntax-object?\ 2460}# #{id\ 3380}#)
                     (begin
                       (let ((#{id\ 3471}#
                               (#{syntax-object-expression\ 2462}#
                                 #{id\ 3380}#))
                             (#{w1\ 3472}#
                               (#{syntax-object-wrap\ 2464}# #{id\ 3380}#)))
                         (begin
                           (let ((#{marks\ 3474}#
                                   (#{join-marks\ 2549}#
                                     (#{wrap-marks\ 2503}# #{w\ 3381}#)
                                     (#{wrap-marks\ 2503}# #{w1\ 3472}#))))
                             (call-with-values
                               (lambda ()
                                 (#{search\ 3386}#
                                   #{id\ 3471}#
                                   (#{wrap-subst\ 2505}# #{w\ 3381}#)
                                   #{marks\ 3474}#))
                               (lambda (#{new-id\ 3475}# #{marks\ 3476}#)
                                 (begin
                                   (let ((#{t\ 3481}# #{new-id\ 3475}#))
                                     (if #{t\ 3481}#
                                       #{t\ 3481}#
                                       (begin
                                         (let ((#{t\ 3484}#
                                                 (call-with-values
                                                   (lambda ()
                                                     (#{search\ 3386}#
                                                       #{id\ 3471}#
                                                       (#{wrap-subst\ 2505}#
                                                         #{w1\ 3472}#)
                                                       #{marks\ 3476}#))
                                                   (lambda (#{x\ 3486}#
                                                            .
                                                            #{ignore\ 3487}#)
                                                     #{x\ 3486}#))))
                                           (if #{t\ 3484}#
                                             #{t\ 3484}#
                                             #{id\ 3471}#))))))))))))
                     (syntax-violation
                       'id-var-name
                       "invalid id"
                       #{id\ 3380}#)))))))
         (#{free-id=?\ 2555}#
           (lambda (#{i\ 3492}# #{j\ 3493}#)
             (if (eq? (begin
                        (let ((#{x\ 3499}# #{i\ 3492}#))
                          (if (#{syntax-object?\ 2460}# #{x\ 3499}#)
                            (#{syntax-object-expression\ 2462}# #{x\ 3499}#)
                            #{x\ 3499}#)))
                      (begin
                        (let ((#{x\ 3502}# #{j\ 3493}#))
                          (if (#{syntax-object?\ 2460}# #{x\ 3502}#)
                            (#{syntax-object-expression\ 2462}# #{x\ 3502}#)
                            #{x\ 3502}#))))
               (eq? (#{id-var-name\ 2553}# #{i\ 3492}# (quote (())))
                    (#{id-var-name\ 2553}# #{j\ 3493}# (quote (()))))
               #f)))
         (#{bound-id=?\ 2557}#
           (lambda (#{i\ 3506}# #{j\ 3507}#)
             (if (if (#{syntax-object?\ 2460}# #{i\ 3506}#)
                   (#{syntax-object?\ 2460}# #{j\ 3507}#)
                   #f)
               (if (eq? (#{syntax-object-expression\ 2462}# #{i\ 3506}#)
                        (#{syntax-object-expression\ 2462}# #{j\ 3507}#))
                 (#{same-marks?\ 2551}#
                   (#{wrap-marks\ 2503}#
                     (#{syntax-object-wrap\ 2464}# #{i\ 3506}#))
                   (#{wrap-marks\ 2503}#
                     (#{syntax-object-wrap\ 2464}# #{j\ 3507}#)))
                 #f)
               (eq? #{i\ 3506}# #{j\ 3507}#))))
         (#{valid-bound-ids?\ 2559}#
           (lambda (#{ids\ 3514}#)
             (if (letrec*
                   ((#{all-ids?\ 3519}#
                      (lambda (#{ids\ 3520}#)
                        (begin
                          (let ((#{t\ 3523}# (null? #{ids\ 3520}#)))
                            (if #{t\ 3523}#
                              #{t\ 3523}#
                              (if (#{id?\ 2496}# (car #{ids\ 3520}#))
                                (#{all-ids?\ 3519}# (cdr #{ids\ 3520}#))
                                #f)))))))
                   (begin (#{all-ids?\ 3519}# #{ids\ 3514}#)))
               (#{distinct-bound-ids?\ 2561}# #{ids\ 3514}#)
               #f)))
         (#{distinct-bound-ids?\ 2561}#
           (lambda (#{ids\ 3528}#)
             (letrec*
               ((#{distinct?\ 3532}#
                  (lambda (#{ids\ 3533}#)
                    (begin
                      (let ((#{t\ 3536}# (null? #{ids\ 3533}#)))
                        (if #{t\ 3536}#
                          #{t\ 3536}#
                          (if (not (#{bound-id-member?\ 2563}#
                                     (car #{ids\ 3533}#)
                                     (cdr #{ids\ 3533}#)))
                            (#{distinct?\ 3532}# (cdr #{ids\ 3533}#))
                            #f)))))))
               (begin (#{distinct?\ 3532}# #{ids\ 3528}#)))))
         (#{bound-id-member?\ 2563}#
           (lambda (#{x\ 3540}# #{list\ 3541}#)
             (if (not (null? #{list\ 3541}#))
               (begin
                 (let ((#{t\ 3548}#
                         (#{bound-id=?\ 2557}#
                           #{x\ 3540}#
                           (car #{list\ 3541}#))))
                   (if #{t\ 3548}#
                     #{t\ 3548}#
                     (#{bound-id-member?\ 2563}#
                       #{x\ 3540}#
                       (cdr #{list\ 3541}#)))))
               #f)))
         (#{wrap\ 2565}#
           (lambda (#{x\ 3550}# #{w\ 3551}# #{defmod\ 3552}#)
             (if (if (null? (#{wrap-marks\ 2503}# #{w\ 3551}#))
                   (null? (#{wrap-subst\ 2505}# #{w\ 3551}#))
                   #f)
               #{x\ 3550}#
               (if (#{syntax-object?\ 2460}# #{x\ 3550}#)
                 (#{make-syntax-object\ 2458}#
                   (#{syntax-object-expression\ 2462}# #{x\ 3550}#)
                   (#{join-wraps\ 2547}#
                     #{w\ 3551}#
                     (#{syntax-object-wrap\ 2464}# #{x\ 3550}#))
                   (#{syntax-object-module\ 2466}# #{x\ 3550}#))
                 (if (null? #{x\ 3550}#)
                   #{x\ 3550}#
                   (#{make-syntax-object\ 2458}#
                     #{x\ 3550}#
                     #{w\ 3551}#
                     #{defmod\ 3552}#))))))
         (#{source-wrap\ 2567}#
           (lambda (#{x\ 3565}#
                    #{w\ 3566}#
                    #{s\ 3567}#
                    #{defmod\ 3568}#)
             (#{wrap\ 2565}#
               (#{decorate-source\ 2414}#
                 #{x\ 3565}#
                 #{s\ 3567}#)
               #{w\ 3566}#
               #{defmod\ 3568}#)))
         (#{chi-sequence\ 2569}#
           (lambda (#{body\ 3573}#
                    #{r\ 3574}#
                    #{w\ 3575}#
                    #{s\ 3576}#
                    #{mod\ 3577}#)
             (#{build-sequence\ 2448}#
               #{s\ 3576}#
               (letrec*
                 ((#{dobody\ 3588}#
                    (lambda (#{body\ 3589}#
                             #{r\ 3590}#
                             #{w\ 3591}#
                             #{mod\ 3592}#)
                      (if (null? #{body\ 3589}#)
                        '()
                        (begin
                          (let ((#{first\ 3594}#
                                  (#{chi\ 2581}#
                                    (car #{body\ 3589}#)
                                    #{r\ 3590}#
                                    #{w\ 3591}#
                                    #{mod\ 3592}#)))
                            (cons #{first\ 3594}#
                                  (#{dobody\ 3588}#
                                    (cdr #{body\ 3589}#)
                                    #{r\ 3590}#
                                    #{w\ 3591}#
                                    #{mod\ 3592}#))))))))
                 (begin
                   (#{dobody\ 3588}#
                     #{body\ 3573}#
                     #{r\ 3574}#
                     #{w\ 3575}#
                     #{mod\ 3577}#))))))
         (#{chi-top-sequence\ 2571}#
           (lambda (#{body\ 3595}#
                    #{r\ 3596}#
                    #{w\ 3597}#
                    #{s\ 3598}#
                    #{m\ 3599}#
                    #{esew\ 3600}#
                    #{mod\ 3601}#)
             (#{build-sequence\ 2448}#
               #{s\ 3598}#
               (letrec*
                 ((#{dobody\ 3617}#
                    (lambda (#{body\ 3618}#
                             #{r\ 3619}#
                             #{w\ 3620}#
                             #{m\ 3621}#
                             #{esew\ 3622}#
                             #{mod\ 3623}#
                             #{out\ 3624}#)
                      (if (null? #{body\ 3618}#)
                        (reverse #{out\ 3624}#)
                        (#{dobody\ 3617}#
                          (cdr #{body\ 3618}#)
                          #{r\ 3619}#
                          #{w\ 3620}#
                          #{m\ 3621}#
                          #{esew\ 3622}#
                          #{mod\ 3623}#
                          (cons (#{chi-top\ 2579}#
                                  (car #{body\ 3618}#)
                                  #{r\ 3619}#
                                  #{w\ 3620}#
                                  #{m\ 3621}#
                                  #{esew\ 3622}#
                                  #{mod\ 3623}#)
                                #{out\ 3624}#))))))
                 (begin
                   (#{dobody\ 3617}#
                     #{body\ 3595}#
                     #{r\ 3596}#
                     #{w\ 3597}#
                     #{m\ 3599}#
                     #{esew\ 3600}#
                     #{mod\ 3601}#
                     '()))))))
         (#{chi-install-global\ 2573}#
           (lambda (#{name\ 3625}# #{e\ 3626}#)
             (#{build-global-definition\ 2436}#
               #f
               #{name\ 3625}#
               (#{build-application\ 2420}#
                 #f
                 (#{build-primref\ 2444}#
                   #f
                   'make-syntax-transformer)
                 (list (#{build-data\ 2446}# #f #{name\ 3625}#)
                       (#{build-data\ 2446}# #f (quote macro))
                       #{e\ 3626}#)))))
         (#{chi-when-list\ 2575}#
           (lambda (#{e\ 3634}# #{when-list\ 3635}# #{w\ 3636}#)
             (letrec*
               ((#{f\ 3643}#
                  (lambda (#{when-list\ 3644}# #{situations\ 3645}#)
                    (if (null? #{when-list\ 3644}#)
                      #{situations\ 3645}#
                      (#{f\ 3643}#
                        (cdr #{when-list\ 3644}#)
                        (cons (begin
                                (let ((#{x\ 3647}# (car #{when-list\ 3644}#)))
                                  (if (#{free-id=?\ 2555}#
                                        #{x\ 3647}#
                                        '#(syntax-object
                                           compile
                                           ((top)
                                            #(ribcage () () ())
                                            #(ribcage () () ())
                                            #(ribcage () () ())
                                            #(ribcage #(x) #((top)) #("i3646"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(f when-list situations)
                                              #((top) (top) (top))
                                              #("i3640" "i3641" "i3642"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(e when-list w)
                                              #((top) (top) (top))
                                              #("i3637" "i3638" "i3639"))
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
                                                gensym-hook
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
                                               (top))
                                              ("i2610"
                                               "i2608"
                                               "i2606"
                                               "i2604"
                                               "i2602"
                                               "i2600"
                                               "i2598"
                                               "i2596"
                                               "i2594"
                                               "i2592"
                                               "i2590"
                                               "i2588"
                                               "i2586"
                                               "i2584"
                                               "i2582"
                                               "i2580"
                                               "i2578"
                                               "i2576"
                                               "i2574"
                                               "i2572"
                                               "i2570"
                                               "i2568"
                                               "i2566"
                                               "i2564"
                                               "i2562"
                                               "i2560"
                                               "i2558"
                                               "i2556"
                                               "i2554"
                                               "i2552"
                                               "i2550"
                                               "i2548"
                                               "i2546"
                                               "i2544"
                                               "i2542"
                                               "i2540"
                                               "i2539"
                                               "i2538"
                                               "i2536"
                                               "i2535"
                                               "i2534"
                                               "i2533"
                                               "i2532"
                                               "i2530"
                                               "i2528"
                                               "i2526"
                                               "i2524"
                                               "i2522"
                                               "i2520"
                                               "i2518"
                                               "i2516"
                                               "i2513"
                                               "i2511"
                                               "i2510"
                                               "i2509"
                                               "i2508"
                                               "i2507"
                                               "i2506"
                                               "i2504"
                                               "i2502"
                                               "i2500"
                                               "i2498"
                                               "i2497"
                                               "i2495"
                                               "i2493"
                                               "i2491"
                                               "i2489"
                                               "i2487"
                                               "i2485"
                                               "i2483"
                                               "i2482"
                                               "i2480"
                                               "i2478"
                                               "i2477"
                                               "i2476"
                                               "i2474"
                                               "i2473"
                                               "i2471"
                                               "i2469"
                                               "i2467"
                                               "i2465"
                                               "i2463"
                                               "i2461"
                                               "i2459"
                                               "i2457"
                                               "i2455"
                                               "i2453"
                                               "i2451"
                                               "i2449"
                                               "i2447"
                                               "i2445"
                                               "i2443"
                                               "i2441"
                                               "i2439"
                                               "i2437"
                                               "i2435"
                                               "i2433"
                                               "i2431"
                                               "i2429"
                                               "i2427"
                                               "i2425"
                                               "i2423"
                                               "i2421"
                                               "i2419"
                                               "i2417"
                                               "i2415"
                                               "i2413"
                                               "i2411"
                                               "i2409"
                                               "i2408"
                                               "i2406"
                                               "i2404"
                                               "i2402"
                                               "i2400"
                                               "i2398"
                                               "i2396"
                                               "i2394"
                                               "i2392"
                                               "i2390"
                                               "i2387"
                                               "i2385"
                                               "i2383"
                                               "i2381"
                                               "i2379"
                                               "i2377"
                                               "i2375"
                                               "i2373"
                                               "i2371"
                                               "i2369"
                                               "i2367"
                                               "i2365"
                                               "i2363"
                                               "i2361"
                                               "i2359"
                                               "i2357"
                                               "i2355"
                                               "i2353"))
                                            #(ribcage
                                              (define-structure
                                                define-expansion-accessors
                                                define-expansion-constructors
                                                and-map*)
                                              ((top) (top) (top) (top))
                                              ("i2147"
                                               "i2146"
                                               "i2145"
                                               "i2143")))
                                           (hygiene guile)))
                                    'compile
                                    (if (#{free-id=?\ 2555}#
                                          #{x\ 3647}#
                                          '#(syntax-object
                                             load
                                             ((top)
                                              #(ribcage () () ())
                                              #(ribcage () () ())
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(x)
                                                #((top))
                                                #("i3646"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(f when-list situations)
                                                #((top) (top) (top))
                                                #("i3640" "i3641" "i3642"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(e when-list w)
                                                #((top) (top) (top))
                                                #("i3637" "i3638" "i3639"))
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
                                                  gensym-hook
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
                                                 (top))
                                                ("i2610"
                                                 "i2608"
                                                 "i2606"
                                                 "i2604"
                                                 "i2602"
                                                 "i2600"
                                                 "i2598"
                                                 "i2596"
                                                 "i2594"
                                                 "i2592"
                                                 "i2590"
                                                 "i2588"
                                                 "i2586"
                                                 "i2584"
                                                 "i2582"
                                                 "i2580"
                                                 "i2578"
                                                 "i2576"
                                                 "i2574"
                                                 "i2572"
                                                 "i2570"
                                                 "i2568"
                                                 "i2566"
                                                 "i2564"
                                                 "i2562"
                                                 "i2560"
                                                 "i2558"
                                                 "i2556"
                                                 "i2554"
                                                 "i2552"
                                                 "i2550"
                                                 "i2548"
                                                 "i2546"
                                                 "i2544"
                                                 "i2542"
                                                 "i2540"
                                                 "i2539"
                                                 "i2538"
                                                 "i2536"
                                                 "i2535"
                                                 "i2534"
                                                 "i2533"
                                                 "i2532"
                                                 "i2530"
                                                 "i2528"
                                                 "i2526"
                                                 "i2524"
                                                 "i2522"
                                                 "i2520"
                                                 "i2518"
                                                 "i2516"
                                                 "i2513"
                                                 "i2511"
                                                 "i2510"
                                                 "i2509"
                                                 "i2508"
                                                 "i2507"
                                                 "i2506"
                                                 "i2504"
                                                 "i2502"
                                                 "i2500"
                                                 "i2498"
                                                 "i2497"
                                                 "i2495"
                                                 "i2493"
                                                 "i2491"
                                                 "i2489"
                                                 "i2487"
                                                 "i2485"
                                                 "i2483"
                                                 "i2482"
                                                 "i2480"
                                                 "i2478"
                                                 "i2477"
                                                 "i2476"
                                                 "i2474"
                                                 "i2473"
                                                 "i2471"
                                                 "i2469"
                                                 "i2467"
                                                 "i2465"
                                                 "i2463"
                                                 "i2461"
                                                 "i2459"
                                                 "i2457"
                                                 "i2455"
                                                 "i2453"
                                                 "i2451"
                                                 "i2449"
                                                 "i2447"
                                                 "i2445"
                                                 "i2443"
                                                 "i2441"
                                                 "i2439"
                                                 "i2437"
                                                 "i2435"
                                                 "i2433"
                                                 "i2431"
                                                 "i2429"
                                                 "i2427"
                                                 "i2425"
                                                 "i2423"
                                                 "i2421"
                                                 "i2419"
                                                 "i2417"
                                                 "i2415"
                                                 "i2413"
                                                 "i2411"
                                                 "i2409"
                                                 "i2408"
                                                 "i2406"
                                                 "i2404"
                                                 "i2402"
                                                 "i2400"
                                                 "i2398"
                                                 "i2396"
                                                 "i2394"
                                                 "i2392"
                                                 "i2390"
                                                 "i2387"
                                                 "i2385"
                                                 "i2383"
                                                 "i2381"
                                                 "i2379"
                                                 "i2377"
                                                 "i2375"
                                                 "i2373"
                                                 "i2371"
                                                 "i2369"
                                                 "i2367"
                                                 "i2365"
                                                 "i2363"
                                                 "i2361"
                                                 "i2359"
                                                 "i2357"
                                                 "i2355"
                                                 "i2353"))
                                              #(ribcage
                                                (define-structure
                                                  define-expansion-accessors
                                                  define-expansion-constructors
                                                  and-map*)
                                                ((top) (top) (top) (top))
                                                ("i2147"
                                                 "i2146"
                                                 "i2145"
                                                 "i2143")))
                                             (hygiene guile)))
                                      'load
                                      (if (#{free-id=?\ 2555}#
                                            #{x\ 3647}#
                                            '#(syntax-object
                                               eval
                                               ((top)
                                                #(ribcage () () ())
                                                #(ribcage () () ())
                                                #(ribcage () () ())
                                                #(ribcage
                                                  #(x)
                                                  #((top))
                                                  #("i3646"))
                                                #(ribcage () () ())
                                                #(ribcage
                                                  #(f when-list situations)
                                                  #((top) (top) (top))
                                                  #("i3640" "i3641" "i3642"))
                                                #(ribcage () () ())
                                                #(ribcage
                                                  #(e when-list w)
                                                  #((top) (top) (top))
                                                  #("i3637" "i3638" "i3639"))
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
                                                    gensym-hook
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
                                                   (top))
                                                  ("i2610"
                                                   "i2608"
                                                   "i2606"
                                                   "i2604"
                                                   "i2602"
                                                   "i2600"
                                                   "i2598"
                                                   "i2596"
                                                   "i2594"
                                                   "i2592"
                                                   "i2590"
                                                   "i2588"
                                                   "i2586"
                                                   "i2584"
                                                   "i2582"
                                                   "i2580"
                                                   "i2578"
                                                   "i2576"
                                                   "i2574"
                                                   "i2572"
                                                   "i2570"
                                                   "i2568"
                                                   "i2566"
                                                   "i2564"
                                                   "i2562"
                                                   "i2560"
                                                   "i2558"
                                                   "i2556"
                                                   "i2554"
                                                   "i2552"
                                                   "i2550"
                                                   "i2548"
                                                   "i2546"
                                                   "i2544"
                                                   "i2542"
                                                   "i2540"
                                                   "i2539"
                                                   "i2538"
                                                   "i2536"
                                                   "i2535"
                                                   "i2534"
                                                   "i2533"
                                                   "i2532"
                                                   "i2530"
                                                   "i2528"
                                                   "i2526"
                                                   "i2524"
                                                   "i2522"
                                                   "i2520"
                                                   "i2518"
                                                   "i2516"
                                                   "i2513"
                                                   "i2511"
                                                   "i2510"
                                                   "i2509"
                                                   "i2508"
                                                   "i2507"
                                                   "i2506"
                                                   "i2504"
                                                   "i2502"
                                                   "i2500"
                                                   "i2498"
                                                   "i2497"
                                                   "i2495"
                                                   "i2493"
                                                   "i2491"
                                                   "i2489"
                                                   "i2487"
                                                   "i2485"
                                                   "i2483"
                                                   "i2482"
                                                   "i2480"
                                                   "i2478"
                                                   "i2477"
                                                   "i2476"
                                                   "i2474"
                                                   "i2473"
                                                   "i2471"
                                                   "i2469"
                                                   "i2467"
                                                   "i2465"
                                                   "i2463"
                                                   "i2461"
                                                   "i2459"
                                                   "i2457"
                                                   "i2455"
                                                   "i2453"
                                                   "i2451"
                                                   "i2449"
                                                   "i2447"
                                                   "i2445"
                                                   "i2443"
                                                   "i2441"
                                                   "i2439"
                                                   "i2437"
                                                   "i2435"
                                                   "i2433"
                                                   "i2431"
                                                   "i2429"
                                                   "i2427"
                                                   "i2425"
                                                   "i2423"
                                                   "i2421"
                                                   "i2419"
                                                   "i2417"
                                                   "i2415"
                                                   "i2413"
                                                   "i2411"
                                                   "i2409"
                                                   "i2408"
                                                   "i2406"
                                                   "i2404"
                                                   "i2402"
                                                   "i2400"
                                                   "i2398"
                                                   "i2396"
                                                   "i2394"
                                                   "i2392"
                                                   "i2390"
                                                   "i2387"
                                                   "i2385"
                                                   "i2383"
                                                   "i2381"
                                                   "i2379"
                                                   "i2377"
                                                   "i2375"
                                                   "i2373"
                                                   "i2371"
                                                   "i2369"
                                                   "i2367"
                                                   "i2365"
                                                   "i2363"
                                                   "i2361"
                                                   "i2359"
                                                   "i2357"
                                                   "i2355"
                                                   "i2353"))
                                                #(ribcage
                                                  (define-structure
                                                    define-expansion-accessors
                                                    define-expansion-constructors
                                                    and-map*)
                                                  ((top) (top) (top) (top))
                                                  ("i2147"
                                                   "i2146"
                                                   "i2145"
                                                   "i2143")))
                                               (hygiene guile)))
                                        'eval
                                        (if (#{free-id=?\ 2555}#
                                              #{x\ 3647}#
                                              '#(syntax-object
                                                 expand
                                                 ((top)
                                                  #(ribcage () () ())
                                                  #(ribcage () () ())
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(x)
                                                    #((top))
                                                    #("i3646"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(f when-list situations)
                                                    #((top) (top) (top))
                                                    #("i3640" "i3641" "i3642"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(e when-list w)
                                                    #((top) (top) (top))
                                                    #("i3637" "i3638" "i3639"))
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
                                                      gensym-hook
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
                                                     (top))
                                                    ("i2610"
                                                     "i2608"
                                                     "i2606"
                                                     "i2604"
                                                     "i2602"
                                                     "i2600"
                                                     "i2598"
                                                     "i2596"
                                                     "i2594"
                                                     "i2592"
                                                     "i2590"
                                                     "i2588"
                                                     "i2586"
                                                     "i2584"
                                                     "i2582"
                                                     "i2580"
                                                     "i2578"
                                                     "i2576"
                                                     "i2574"
                                                     "i2572"
                                                     "i2570"
                                                     "i2568"
                                                     "i2566"
                                                     "i2564"
                                                     "i2562"
                                                     "i2560"
                                                     "i2558"
                                                     "i2556"
                                                     "i2554"
                                                     "i2552"
                                                     "i2550"
                                                     "i2548"
                                                     "i2546"
                                                     "i2544"
                                                     "i2542"
                                                     "i2540"
                                                     "i2539"
                                                     "i2538"
                                                     "i2536"
                                                     "i2535"
                                                     "i2534"
                                                     "i2533"
                                                     "i2532"
                                                     "i2530"
                                                     "i2528"
                                                     "i2526"
                                                     "i2524"
                                                     "i2522"
                                                     "i2520"
                                                     "i2518"
                                                     "i2516"
                                                     "i2513"
                                                     "i2511"
                                                     "i2510"
                                                     "i2509"
                                                     "i2508"
                                                     "i2507"
                                                     "i2506"
                                                     "i2504"
                                                     "i2502"
                                                     "i2500"
                                                     "i2498"
                                                     "i2497"
                                                     "i2495"
                                                     "i2493"
                                                     "i2491"
                                                     "i2489"
                                                     "i2487"
                                                     "i2485"
                                                     "i2483"
                                                     "i2482"
                                                     "i2480"
                                                     "i2478"
                                                     "i2477"
                                                     "i2476"
                                                     "i2474"
                                                     "i2473"
                                                     "i2471"
                                                     "i2469"
                                                     "i2467"
                                                     "i2465"
                                                     "i2463"
                                                     "i2461"
                                                     "i2459"
                                                     "i2457"
                                                     "i2455"
                                                     "i2453"
                                                     "i2451"
                                                     "i2449"
                                                     "i2447"
                                                     "i2445"
                                                     "i2443"
                                                     "i2441"
                                                     "i2439"
                                                     "i2437"
                                                     "i2435"
                                                     "i2433"
                                                     "i2431"
                                                     "i2429"
                                                     "i2427"
                                                     "i2425"
                                                     "i2423"
                                                     "i2421"
                                                     "i2419"
                                                     "i2417"
                                                     "i2415"
                                                     "i2413"
                                                     "i2411"
                                                     "i2409"
                                                     "i2408"
                                                     "i2406"
                                                     "i2404"
                                                     "i2402"
                                                     "i2400"
                                                     "i2398"
                                                     "i2396"
                                                     "i2394"
                                                     "i2392"
                                                     "i2390"
                                                     "i2387"
                                                     "i2385"
                                                     "i2383"
                                                     "i2381"
                                                     "i2379"
                                                     "i2377"
                                                     "i2375"
                                                     "i2373"
                                                     "i2371"
                                                     "i2369"
                                                     "i2367"
                                                     "i2365"
                                                     "i2363"
                                                     "i2361"
                                                     "i2359"
                                                     "i2357"
                                                     "i2355"
                                                     "i2353"))
                                                  #(ribcage
                                                    (define-structure
                                                      define-expansion-accessors
                                                      define-expansion-constructors
                                                      and-map*)
                                                    ((top) (top) (top) (top))
                                                    ("i2147"
                                                     "i2146"
                                                     "i2145"
                                                     "i2143")))
                                                 (hygiene guile)))
                                          'expand
                                          (syntax-violation
                                            'eval-when
                                            "invalid situation"
                                            #{e\ 3634}#
                                            (#{wrap\ 2565}#
                                              #{x\ 3647}#
                                              #{w\ 3636}#
                                              #f))))))))
                              #{situations\ 3645}#))))))
               (begin
                 (#{f\ 3643}# #{when-list\ 3635}# (quote ()))))))
         (#{syntax-type\ 2577}#
           (lambda (#{e\ 3657}#
                    #{r\ 3658}#
                    #{w\ 3659}#
                    #{s\ 3660}#
                    #{rib\ 3661}#
                    #{mod\ 3662}#
                    #{for-car?\ 3663}#)
             (if (symbol? #{e\ 3657}#)
               (begin
                 (let ((#{n\ 3675}#
                         (#{id-var-name\ 2553}# #{e\ 3657}# #{w\ 3659}#)))
                   (begin
                     (let ((#{b\ 3677}#
                             (#{lookup\ 2490}#
                               #{n\ 3675}#
                               #{r\ 3658}#
                               #{mod\ 3662}#)))
                       (begin
                         (let ((#{type\ 3679}#
                                 (#{binding-type\ 2479}# #{b\ 3677}#)))
                           (if (eqv? #{type\ 3679}# (quote lexical))
                             (values
                               #{type\ 3679}#
                               (#{binding-value\ 2481}# #{b\ 3677}#)
                               #{e\ 3657}#
                               #{w\ 3659}#
                               #{s\ 3660}#
                               #{mod\ 3662}#)
                             (if (eqv? #{type\ 3679}# (quote global))
                               (values
                                 #{type\ 3679}#
                                 #{n\ 3675}#
                                 #{e\ 3657}#
                                 #{w\ 3659}#
                                 #{s\ 3660}#
                                 #{mod\ 3662}#)
                               (if (eqv? #{type\ 3679}# (quote macro))
                                 (if #{for-car?\ 3663}#
                                   (values
                                     #{type\ 3679}#
                                     (#{binding-value\ 2481}# #{b\ 3677}#)
                                     #{e\ 3657}#
                                     #{w\ 3659}#
                                     #{s\ 3660}#
                                     #{mod\ 3662}#)
                                   (#{syntax-type\ 2577}#
                                     (#{chi-macro\ 2587}#
                                       (#{binding-value\ 2481}# #{b\ 3677}#)
                                       #{e\ 3657}#
                                       #{r\ 3658}#
                                       #{w\ 3659}#
                                       #{s\ 3660}#
                                       #{rib\ 3661}#
                                       #{mod\ 3662}#)
                                     #{r\ 3658}#
                                     '(())
                                     #{s\ 3660}#
                                     #{rib\ 3661}#
                                     #{mod\ 3662}#
                                     #f))
                                 (values
                                   #{type\ 3679}#
                                   (#{binding-value\ 2481}# #{b\ 3677}#)
                                   #{e\ 3657}#
                                   #{w\ 3659}#
                                   #{s\ 3660}#
                                   #{mod\ 3662}#))))))))))
               (if (pair? #{e\ 3657}#)
                 (begin
                   (let ((#{first\ 3688}# (car #{e\ 3657}#)))
                     (call-with-values
                       (lambda ()
                         (#{syntax-type\ 2577}#
                           #{first\ 3688}#
                           #{r\ 3658}#
                           #{w\ 3659}#
                           #{s\ 3660}#
                           #{rib\ 3661}#
                           #{mod\ 3662}#
                           #t))
                       (lambda (#{ftype\ 3689}#
                                #{fval\ 3690}#
                                #{fe\ 3691}#
                                #{fw\ 3692}#
                                #{fs\ 3693}#
                                #{fmod\ 3694}#)
                         (if (eqv? #{ftype\ 3689}# (quote lexical))
                           (values
                             'lexical-call
                             #{fval\ 3690}#
                             #{e\ 3657}#
                             #{w\ 3659}#
                             #{s\ 3660}#
                             #{mod\ 3662}#)
                           (if (eqv? #{ftype\ 3689}# (quote global))
                             (values
                               'global-call
                               (#{make-syntax-object\ 2458}#
                                 #{fval\ 3690}#
                                 #{w\ 3659}#
                                 #{fmod\ 3694}#)
                               #{e\ 3657}#
                               #{w\ 3659}#
                               #{s\ 3660}#
                               #{mod\ 3662}#)
                             (if (eqv? #{ftype\ 3689}# (quote macro))
                               (#{syntax-type\ 2577}#
                                 (#{chi-macro\ 2587}#
                                   #{fval\ 3690}#
                                   #{e\ 3657}#
                                   #{r\ 3658}#
                                   #{w\ 3659}#
                                   #{s\ 3660}#
                                   #{rib\ 3661}#
                                   #{mod\ 3662}#)
                                 #{r\ 3658}#
                                 '(())
                                 #{s\ 3660}#
                                 #{rib\ 3661}#
                                 #{mod\ 3662}#
                                 #{for-car?\ 3663}#)
                               (if (eqv? #{ftype\ 3689}# (quote module-ref))
                                 (call-with-values
                                   (lambda ()
                                     (#{fval\ 3690}#
                                       #{e\ 3657}#
                                       #{r\ 3658}#
                                       #{w\ 3659}#))
                                   (lambda (#{e\ 3706}#
                                            #{r\ 3707}#
                                            #{w\ 3708}#
                                            #{s\ 3709}#
                                            #{mod\ 3710}#)
                                     (#{syntax-type\ 2577}#
                                       #{e\ 3706}#
                                       #{r\ 3707}#
                                       #{w\ 3708}#
                                       #{s\ 3709}#
                                       #{rib\ 3661}#
                                       #{mod\ 3710}#
                                       #{for-car?\ 3663}#)))
                                 (if (eqv? #{ftype\ 3689}# (quote core))
                                   (values
                                     'core-form
                                     #{fval\ 3690}#
                                     #{e\ 3657}#
                                     #{w\ 3659}#
                                     #{s\ 3660}#
                                     #{mod\ 3662}#)
                                   (if (eqv? #{ftype\ 3689}#
                                             'local-syntax)
                                     (values
                                       'local-syntax-form
                                       #{fval\ 3690}#
                                       #{e\ 3657}#
                                       #{w\ 3659}#
                                       #{s\ 3660}#
                                       #{mod\ 3662}#)
                                     (if (eqv? #{ftype\ 3689}# (quote begin))
                                       (values
                                         'begin-form
                                         #f
                                         #{e\ 3657}#
                                         #{w\ 3659}#
                                         #{s\ 3660}#
                                         #{mod\ 3662}#)
                                       (if (eqv? #{ftype\ 3689}#
                                                 'eval-when)
                                         (values
                                           'eval-when-form
                                           #f
                                           #{e\ 3657}#
                                           #{w\ 3659}#
                                           #{s\ 3660}#
                                           #{mod\ 3662}#)
                                         (if (eqv? #{ftype\ 3689}#
                                                   'define)
                                           (let ((#{tmp\ 3721}# #{e\ 3657}#))
                                             (let ((#{tmp\ 3722}#
                                                     ($sc-dispatch
                                                       #{tmp\ 3721}#
                                                       '(any any any))))
                                               (if (if #{tmp\ 3722}#
                                                     (@apply
                                                       (lambda (#{_\ 3726}#
                                                                #{name\ 3727}#
                                                                #{val\ 3728}#)
                                                         (#{id?\ 2496}#
                                                           #{name\ 3727}#))
                                                       #{tmp\ 3722}#)
                                                     #f)
                                                 (@apply
                                                   (lambda (#{_\ 3732}#
                                                            #{name\ 3733}#
                                                            #{val\ 3734}#)
                                                     (values
                                                       'define-form
                                                       #{name\ 3733}#
                                                       #{val\ 3734}#
                                                       #{w\ 3659}#
                                                       #{s\ 3660}#
                                                       #{mod\ 3662}#))
                                                   #{tmp\ 3722}#)
                                                 (let ((#{tmp\ 3735}#
                                                         ($sc-dispatch
                                                           #{tmp\ 3721}#
                                                           '(any (any . any)
                                                                 any
                                                                 .
                                                                 each-any))))
                                                   (if (if #{tmp\ 3735}#
                                                         (@apply
                                                           (lambda (#{_\ 3741}#
                                                                    #{name\ 3742}#
                                                                    #{args\ 3743}#
                                                                    #{e1\ 3744}#
                                                                    #{e2\ 3745}#)
                                                             (if (#{id?\ 2496}#
                                                                   #{name\ 3742}#)
                                                               (#{valid-bound-ids?\ 2559}#
                                                                 (#{lambda-var-list\ 2611}#
                                                                   #{args\ 3743}#))
                                                               #f))
                                                           #{tmp\ 3735}#)
                                                         #f)
                                                     (@apply
                                                       (lambda (#{_\ 3753}#
                                                                #{name\ 3754}#
                                                                #{args\ 3755}#
                                                                #{e1\ 3756}#
                                                                #{e2\ 3757}#)
                                                         (values
                                                           'define-form
                                                           (#{wrap\ 2565}#
                                                             #{name\ 3754}#
                                                             #{w\ 3659}#
                                                             #{mod\ 3662}#)
                                                           (#{decorate-source\ 2414}#
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
                                                                         #("i3748"
                                                                           "i3749"
                                                                           "i3750"
                                                                           "i3751"
                                                                           "i3752"))
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
                                                                         #("i3695"
                                                                           "i3696"
                                                                           "i3697"
                                                                           "i3698"
                                                                           "i3699"
                                                                           "i3700"))
                                                                       #(ribcage
                                                                         ()
                                                                         ()
                                                                         ())
                                                                       #(ribcage
                                                                         #(first)
                                                                         #((top))
                                                                         #("i3687"))
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
                                                                         #("i3664"
                                                                           "i3665"
                                                                           "i3666"
                                                                           "i3667"
                                                                           "i3668"
                                                                           "i3669"
                                                                           "i3670"))
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
                                                                           gensym-hook
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
                                                                          (top))
                                                                         ("i2610"
                                                                          "i2608"
                                                                          "i2606"
                                                                          "i2604"
                                                                          "i2602"
                                                                          "i2600"
                                                                          "i2598"
                                                                          "i2596"
                                                                          "i2594"
                                                                          "i2592"
                                                                          "i2590"
                                                                          "i2588"
                                                                          "i2586"
                                                                          "i2584"
                                                                          "i2582"
                                                                          "i2580"
                                                                          "i2578"
                                                                          "i2576"
                                                                          "i2574"
                                                                          "i2572"
                                                                          "i2570"
                                                                          "i2568"
                                                                          "i2566"
                                                                          "i2564"
                                                                          "i2562"
                                                                          "i2560"
                                                                          "i2558"
                                                                          "i2556"
                                                                          "i2554"
                                                                          "i2552"
                                                                          "i2550"
                                                                          "i2548"
                                                                          "i2546"
                                                                          "i2544"
                                                                          "i2542"
                                                                          "i2540"
                                                                          "i2539"
                                                                          "i2538"
                                                                          "i2536"
                                                                          "i2535"
                                                                          "i2534"
                                                                          "i2533"
                                                                          "i2532"
                                                                          "i2530"
                                                                          "i2528"
                                                                          "i2526"
                                                                          "i2524"
                                                                          "i2522"
                                                                          "i2520"
                                                                          "i2518"
                                                                          "i2516"
                                                                          "i2513"
                                                                          "i2511"
                                                                          "i2510"
                                                                          "i2509"
                                                                          "i2508"
                                                                          "i2507"
                                                                          "i2506"
                                                                          "i2504"
                                                                          "i2502"
                                                                          "i2500"
                                                                          "i2498"
                                                                          "i2497"
                                                                          "i2495"
                                                                          "i2493"
                                                                          "i2491"
                                                                          "i2489"
                                                                          "i2487"
                                                                          "i2485"
                                                                          "i2483"
                                                                          "i2482"
                                                                          "i2480"
                                                                          "i2478"
                                                                          "i2477"
                                                                          "i2476"
                                                                          "i2474"
                                                                          "i2473"
                                                                          "i2471"
                                                                          "i2469"
                                                                          "i2467"
                                                                          "i2465"
                                                                          "i2463"
                                                                          "i2461"
                                                                          "i2459"
                                                                          "i2457"
                                                                          "i2455"
                                                                          "i2453"
                                                                          "i2451"
                                                                          "i2449"
                                                                          "i2447"
                                                                          "i2445"
                                                                          "i2443"
                                                                          "i2441"
                                                                          "i2439"
                                                                          "i2437"
                                                                          "i2435"
                                                                          "i2433"
                                                                          "i2431"
                                                                          "i2429"
                                                                          "i2427"
                                                                          "i2425"
                                                                          "i2423"
                                                                          "i2421"
                                                                          "i2419"
                                                                          "i2417"
                                                                          "i2415"
                                                                          "i2413"
                                                                          "i2411"
                                                                          "i2409"
                                                                          "i2408"
                                                                          "i2406"
                                                                          "i2404"
                                                                          "i2402"
                                                                          "i2400"
                                                                          "i2398"
                                                                          "i2396"
                                                                          "i2394"
                                                                          "i2392"
                                                                          "i2390"
                                                                          "i2387"
                                                                          "i2385"
                                                                          "i2383"
                                                                          "i2381"
                                                                          "i2379"
                                                                          "i2377"
                                                                          "i2375"
                                                                          "i2373"
                                                                          "i2371"
                                                                          "i2369"
                                                                          "i2367"
                                                                          "i2365"
                                                                          "i2363"
                                                                          "i2361"
                                                                          "i2359"
                                                                          "i2357"
                                                                          "i2355"
                                                                          "i2353"))
                                                                       #(ribcage
                                                                         (define-structure
                                                                           define-expansion-accessors
                                                                           define-expansion-constructors
                                                                           and-map*)
                                                                         ((top)
                                                                          (top)
                                                                          (top)
                                                                          (top))
                                                                         ("i2147"
                                                                          "i2146"
                                                                          "i2145"
                                                                          "i2143")))
                                                                      (hygiene
                                                                        guile))
                                                                   (#{wrap\ 2565}#
                                                                     (cons #{args\ 3755}#
                                                                           (cons #{e1\ 3756}#
                                                                                 #{e2\ 3757}#))
                                                                     #{w\ 3659}#
                                                                     #{mod\ 3662}#))
                                                             #{s\ 3660}#)
                                                           '(())
                                                           #{s\ 3660}#
                                                           #{mod\ 3662}#))
                                                       #{tmp\ 3735}#)
                                                     (let ((#{tmp\ 3760}#
                                                             ($sc-dispatch
                                                               #{tmp\ 3721}#
                                                               '(any any))))
                                                       (if (if #{tmp\ 3760}#
                                                             (@apply
                                                               (lambda (#{_\ 3763}#
                                                                        #{name\ 3764}#)
                                                                 (#{id?\ 2496}#
                                                                   #{name\ 3764}#))
                                                               #{tmp\ 3760}#)
                                                             #f)
                                                         (@apply
                                                           (lambda (#{_\ 3767}#
                                                                    #{name\ 3768}#)
                                                             (values
                                                               'define-form
                                                               (#{wrap\ 2565}#
                                                                 #{name\ 3768}#
                                                                 #{w\ 3659}#
                                                                 #{mod\ 3662}#)
                                                               '(#(syntax-object
                                                                   if
                                                                   ((top)
                                                                    #(ribcage
                                                                      #(_ name)
                                                                      #((top)
                                                                        (top))
                                                                      #("i3765"
                                                                        "i3766"))
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
                                                                      #("i3695"
                                                                        "i3696"
                                                                        "i3697"
                                                                        "i3698"
                                                                        "i3699"
                                                                        "i3700"))
                                                                    #(ribcage
                                                                      ()
                                                                      ()
                                                                      ())
                                                                    #(ribcage
                                                                      #(first)
                                                                      #((top))
                                                                      #("i3687"))
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
                                                                      #("i3664"
                                                                        "i3665"
                                                                        "i3666"
                                                                        "i3667"
                                                                        "i3668"
                                                                        "i3669"
                                                                        "i3670"))
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
                                                                        gensym-hook
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
                                                                       (top))
                                                                      ("i2610"
                                                                       "i2608"
                                                                       "i2606"
                                                                       "i2604"
                                                                       "i2602"
                                                                       "i2600"
                                                                       "i2598"
                                                                       "i2596"
                                                                       "i2594"
                                                                       "i2592"
                                                                       "i2590"
                                                                       "i2588"
                                                                       "i2586"
                                                                       "i2584"
                                                                       "i2582"
                                                                       "i2580"
                                                                       "i2578"
                                                                       "i2576"
                                                                       "i2574"
                                                                       "i2572"
                                                                       "i2570"
                                                                       "i2568"
                                                                       "i2566"
                                                                       "i2564"
                                                                       "i2562"
                                                                       "i2560"
                                                                       "i2558"
                                                                       "i2556"
                                                                       "i2554"
                                                                       "i2552"
                                                                       "i2550"
                                                                       "i2548"
                                                                       "i2546"
                                                                       "i2544"
                                                                       "i2542"
                                                                       "i2540"
                                                                       "i2539"
                                                                       "i2538"
                                                                       "i2536"
                                                                       "i2535"
                                                                       "i2534"
                                                                       "i2533"
                                                                       "i2532"
                                                                       "i2530"
                                                                       "i2528"
                                                                       "i2526"
                                                                       "i2524"
                                                                       "i2522"
                                                                       "i2520"
                                                                       "i2518"
                                                                       "i2516"
                                                                       "i2513"
                                                                       "i2511"
                                                                       "i2510"
                                                                       "i2509"
                                                                       "i2508"
                                                                       "i2507"
                                                                       "i2506"
                                                                       "i2504"
                                                                       "i2502"
                                                                       "i2500"
                                                                       "i2498"
                                                                       "i2497"
                                                                       "i2495"
                                                                       "i2493"
                                                                       "i2491"
                                                                       "i2489"
                                                                       "i2487"
                                                                       "i2485"
                                                                       "i2483"
                                                                       "i2482"
                                                                       "i2480"
                                                                       "i2478"
                                                                       "i2477"
                                                                       "i2476"
                                                                       "i2474"
                                                                       "i2473"
                                                                       "i2471"
                                                                       "i2469"
                                                                       "i2467"
                                                                       "i2465"
                                                                       "i2463"
                                                                       "i2461"
                                                                       "i2459"
                                                                       "i2457"
                                                                       "i2455"
                                                                       "i2453"
                                                                       "i2451"
                                                                       "i2449"
                                                                       "i2447"
                                                                       "i2445"
                                                                       "i2443"
                                                                       "i2441"
                                                                       "i2439"
                                                                       "i2437"
                                                                       "i2435"
                                                                       "i2433"
                                                                       "i2431"
                                                                       "i2429"
                                                                       "i2427"
                                                                       "i2425"
                                                                       "i2423"
                                                                       "i2421"
                                                                       "i2419"
                                                                       "i2417"
                                                                       "i2415"
                                                                       "i2413"
                                                                       "i2411"
                                                                       "i2409"
                                                                       "i2408"
                                                                       "i2406"
                                                                       "i2404"
                                                                       "i2402"
                                                                       "i2400"
                                                                       "i2398"
                                                                       "i2396"
                                                                       "i2394"
                                                                       "i2392"
                                                                       "i2390"
                                                                       "i2387"
                                                                       "i2385"
                                                                       "i2383"
                                                                       "i2381"
                                                                       "i2379"
                                                                       "i2377"
                                                                       "i2375"
                                                                       "i2373"
                                                                       "i2371"
                                                                       "i2369"
                                                                       "i2367"
                                                                       "i2365"
                                                                       "i2363"
                                                                       "i2361"
                                                                       "i2359"
                                                                       "i2357"
                                                                       "i2355"
                                                                       "i2353"))
                                                                    #(ribcage
                                                                      (define-structure
                                                                        define-expansion-accessors
                                                                        define-expansion-constructors
                                                                        and-map*)
                                                                      ((top)
                                                                       (top)
                                                                       (top)
                                                                       (top))
                                                                      ("i2147"
                                                                       "i2146"
                                                                       "i2145"
                                                                       "i2143")))
                                                                   (hygiene
                                                                     guile))
                                                                 #(syntax-object
                                                                   #f
                                                                   ((top)
                                                                    #(ribcage
                                                                      #(_ name)
                                                                      #((top)
                                                                        (top))
                                                                      #("i3765"
                                                                        "i3766"))
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
                                                                      #("i3695"
                                                                        "i3696"
                                                                        "i3697"
                                                                        "i3698"
                                                                        "i3699"
                                                                        "i3700"))
                                                                    #(ribcage
                                                                      ()
                                                                      ()
                                                                      ())
                                                                    #(ribcage
                                                                      #(first)
                                                                      #((top))
                                                                      #("i3687"))
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
                                                                      #("i3664"
                                                                        "i3665"
                                                                        "i3666"
                                                                        "i3667"
                                                                        "i3668"
                                                                        "i3669"
                                                                        "i3670"))
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
                                                                        gensym-hook
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
                                                                       (top))
                                                                      ("i2610"
                                                                       "i2608"
                                                                       "i2606"
                                                                       "i2604"
                                                                       "i2602"
                                                                       "i2600"
                                                                       "i2598"
                                                                       "i2596"
                                                                       "i2594"
                                                                       "i2592"
                                                                       "i2590"
                                                                       "i2588"
                                                                       "i2586"
                                                                       "i2584"
                                                                       "i2582"
                                                                       "i2580"
                                                                       "i2578"
                                                                       "i2576"
                                                                       "i2574"
                                                                       "i2572"
                                                                       "i2570"
                                                                       "i2568"
                                                                       "i2566"
                                                                       "i2564"
                                                                       "i2562"
                                                                       "i2560"
                                                                       "i2558"
                                                                       "i2556"
                                                                       "i2554"
                                                                       "i2552"
                                                                       "i2550"
                                                                       "i2548"
                                                                       "i2546"
                                                                       "i2544"
                                                                       "i2542"
                                                                       "i2540"
                                                                       "i2539"
                                                                       "i2538"
                                                                       "i2536"
                                                                       "i2535"
                                                                       "i2534"
                                                                       "i2533"
                                                                       "i2532"
                                                                       "i2530"
                                                                       "i2528"
                                                                       "i2526"
                                                                       "i2524"
                                                                       "i2522"
                                                                       "i2520"
                                                                       "i2518"
                                                                       "i2516"
                                                                       "i2513"
                                                                       "i2511"
                                                                       "i2510"
                                                                       "i2509"
                                                                       "i2508"
                                                                       "i2507"
                                                                       "i2506"
                                                                       "i2504"
                                                                       "i2502"
                                                                       "i2500"
                                                                       "i2498"
                                                                       "i2497"
                                                                       "i2495"
                                                                       "i2493"
                                                                       "i2491"
                                                                       "i2489"
                                                                       "i2487"
                                                                       "i2485"
                                                                       "i2483"
                                                                       "i2482"
                                                                       "i2480"
                                                                       "i2478"
                                                                       "i2477"
                                                                       "i2476"
                                                                       "i2474"
                                                                       "i2473"
                                                                       "i2471"
                                                                       "i2469"
                                                                       "i2467"
                                                                       "i2465"
                                                                       "i2463"
                                                                       "i2461"
                                                                       "i2459"
                                                                       "i2457"
                                                                       "i2455"
                                                                       "i2453"
                                                                       "i2451"
                                                                       "i2449"
                                                                       "i2447"
                                                                       "i2445"
                                                                       "i2443"
                                                                       "i2441"
                                                                       "i2439"
                                                                       "i2437"
                                                                       "i2435"
                                                                       "i2433"
                                                                       "i2431"
                                                                       "i2429"
                                                                       "i2427"
                                                                       "i2425"
                                                                       "i2423"
                                                                       "i2421"
                                                                       "i2419"
                                                                       "i2417"
                                                                       "i2415"
                                                                       "i2413"
                                                                       "i2411"
                                                                       "i2409"
                                                                       "i2408"
                                                                       "i2406"
                                                                       "i2404"
                                                                       "i2402"
                                                                       "i2400"
                                                                       "i2398"
                                                                       "i2396"
                                                                       "i2394"
                                                                       "i2392"
                                                                       "i2390"
                                                                       "i2387"
                                                                       "i2385"
                                                                       "i2383"
                                                                       "i2381"
                                                                       "i2379"
                                                                       "i2377"
                                                                       "i2375"
                                                                       "i2373"
                                                                       "i2371"
                                                                       "i2369"
                                                                       "i2367"
                                                                       "i2365"
                                                                       "i2363"
                                                                       "i2361"
                                                                       "i2359"
                                                                       "i2357"
                                                                       "i2355"
                                                                       "i2353"))
                                                                    #(ribcage
                                                                      (define-structure
                                                                        define-expansion-accessors
                                                                        define-expansion-constructors
                                                                        and-map*)
                                                                      ((top)
                                                                       (top)
                                                                       (top)
                                                                       (top))
                                                                      ("i2147"
                                                                       "i2146"
                                                                       "i2145"
                                                                       "i2143")))
                                                                   (hygiene
                                                                     guile))
                                                                 #(syntax-object
                                                                   #f
                                                                   ((top)
                                                                    #(ribcage
                                                                      #(_ name)
                                                                      #((top)
                                                                        (top))
                                                                      #("i3765"
                                                                        "i3766"))
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
                                                                      #("i3695"
                                                                        "i3696"
                                                                        "i3697"
                                                                        "i3698"
                                                                        "i3699"
                                                                        "i3700"))
                                                                    #(ribcage
                                                                      ()
                                                                      ()
                                                                      ())
                                                                    #(ribcage
                                                                      #(first)
                                                                      #((top))
                                                                      #("i3687"))
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
                                                                      #("i3664"
                                                                        "i3665"
                                                                        "i3666"
                                                                        "i3667"
                                                                        "i3668"
                                                                        "i3669"
                                                                        "i3670"))
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
                                                                        gensym-hook
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
                                                                       (top))
                                                                      ("i2610"
                                                                       "i2608"
                                                                       "i2606"
                                                                       "i2604"
                                                                       "i2602"
                                                                       "i2600"
                                                                       "i2598"
                                                                       "i2596"
                                                                       "i2594"
                                                                       "i2592"
                                                                       "i2590"
                                                                       "i2588"
                                                                       "i2586"
                                                                       "i2584"
                                                                       "i2582"
                                                                       "i2580"
                                                                       "i2578"
                                                                       "i2576"
                                                                       "i2574"
                                                                       "i2572"
                                                                       "i2570"
                                                                       "i2568"
                                                                       "i2566"
                                                                       "i2564"
                                                                       "i2562"
                                                                       "i2560"
                                                                       "i2558"
                                                                       "i2556"
                                                                       "i2554"
                                                                       "i2552"
                                                                       "i2550"
                                                                       "i2548"
                                                                       "i2546"
                                                                       "i2544"
                                                                       "i2542"
                                                                       "i2540"
                                                                       "i2539"
                                                                       "i2538"
                                                                       "i2536"
                                                                       "i2535"
                                                                       "i2534"
                                                                       "i2533"
                                                                       "i2532"
                                                                       "i2530"
                                                                       "i2528"
                                                                       "i2526"
                                                                       "i2524"
                                                                       "i2522"
                                                                       "i2520"
                                                                       "i2518"
                                                                       "i2516"
                                                                       "i2513"
                                                                       "i2511"
                                                                       "i2510"
                                                                       "i2509"
                                                                       "i2508"
                                                                       "i2507"
                                                                       "i2506"
                                                                       "i2504"
                                                                       "i2502"
                                                                       "i2500"
                                                                       "i2498"
                                                                       "i2497"
                                                                       "i2495"
                                                                       "i2493"
                                                                       "i2491"
                                                                       "i2489"
                                                                       "i2487"
                                                                       "i2485"
                                                                       "i2483"
                                                                       "i2482"
                                                                       "i2480"
                                                                       "i2478"
                                                                       "i2477"
                                                                       "i2476"
                                                                       "i2474"
                                                                       "i2473"
                                                                       "i2471"
                                                                       "i2469"
                                                                       "i2467"
                                                                       "i2465"
                                                                       "i2463"
                                                                       "i2461"
                                                                       "i2459"
                                                                       "i2457"
                                                                       "i2455"
                                                                       "i2453"
                                                                       "i2451"
                                                                       "i2449"
                                                                       "i2447"
                                                                       "i2445"
                                                                       "i2443"
                                                                       "i2441"
                                                                       "i2439"
                                                                       "i2437"
                                                                       "i2435"
                                                                       "i2433"
                                                                       "i2431"
                                                                       "i2429"
                                                                       "i2427"
                                                                       "i2425"
                                                                       "i2423"
                                                                       "i2421"
                                                                       "i2419"
                                                                       "i2417"
                                                                       "i2415"
                                                                       "i2413"
                                                                       "i2411"
                                                                       "i2409"
                                                                       "i2408"
                                                                       "i2406"
                                                                       "i2404"
                                                                       "i2402"
                                                                       "i2400"
                                                                       "i2398"
                                                                       "i2396"
                                                                       "i2394"
                                                                       "i2392"
                                                                       "i2390"
                                                                       "i2387"
                                                                       "i2385"
                                                                       "i2383"
                                                                       "i2381"
                                                                       "i2379"
                                                                       "i2377"
                                                                       "i2375"
                                                                       "i2373"
                                                                       "i2371"
                                                                       "i2369"
                                                                       "i2367"
                                                                       "i2365"
                                                                       "i2363"
                                                                       "i2361"
                                                                       "i2359"
                                                                       "i2357"
                                                                       "i2355"
                                                                       "i2353"))
                                                                    #(ribcage
                                                                      (define-structure
                                                                        define-expansion-accessors
                                                                        define-expansion-constructors
                                                                        and-map*)
                                                                      ((top)
                                                                       (top)
                                                                       (top)
                                                                       (top))
                                                                      ("i2147"
                                                                       "i2146"
                                                                       "i2145"
                                                                       "i2143")))
                                                                   (hygiene
                                                                     guile)))
                                                               '(())
                                                               #{s\ 3660}#
                                                               #{mod\ 3662}#))
                                                           #{tmp\ 3760}#)
                                                         (syntax-violation
                                                           #f
                                                           "source expression failed to match any pattern"
                                                           #{tmp\ 3721}#))))))))
                                           (if (eqv? #{ftype\ 3689}#
                                                     'define-syntax)
                                             (let ((#{tmp\ 3771}# #{e\ 3657}#))
                                               (let ((#{tmp\ 3772}#
                                                       ($sc-dispatch
                                                         #{tmp\ 3771}#
                                                         '(any any any))))
                                                 (if (if #{tmp\ 3772}#
                                                       (@apply
                                                         (lambda (#{_\ 3776}#
                                                                  #{name\ 3777}#
                                                                  #{val\ 3778}#)
                                                           (#{id?\ 2496}#
                                                             #{name\ 3777}#))
                                                         #{tmp\ 3772}#)
                                                       #f)
                                                   (@apply
                                                     (lambda (#{_\ 3782}#
                                                              #{name\ 3783}#
                                                              #{val\ 3784}#)
                                                       (values
                                                         'define-syntax-form
                                                         #{name\ 3783}#
                                                         #{val\ 3784}#
                                                         #{w\ 3659}#
                                                         #{s\ 3660}#
                                                         #{mod\ 3662}#))
                                                     #{tmp\ 3772}#)
                                                   (syntax-violation
                                                     #f
                                                     "source expression failed to match any pattern"
                                                     #{tmp\ 3771}#))))
                                             (values
                                               'call
                                               #f
                                               #{e\ 3657}#
                                               #{w\ 3659}#
                                               #{s\ 3660}#
                                               #{mod\ 3662}#)))))))))))))))
                 (if (#{syntax-object?\ 2460}# #{e\ 3657}#)
                   (#{syntax-type\ 2577}#
                     (#{syntax-object-expression\ 2462}# #{e\ 3657}#)
                     #{r\ 3658}#
                     (#{join-wraps\ 2547}#
                       #{w\ 3659}#
                       (#{syntax-object-wrap\ 2464}# #{e\ 3657}#))
                     (begin
                       (let ((#{t\ 3790}#
                               (#{source-annotation\ 2475}# #{e\ 3657}#)))
                         (if #{t\ 3790}# #{t\ 3790}# #{s\ 3660}#)))
                     #{rib\ 3661}#
                     (begin
                       (let ((#{t\ 3794}#
                               (#{syntax-object-module\ 2466}# #{e\ 3657}#)))
                         (if #{t\ 3794}# #{t\ 3794}# #{mod\ 3662}#)))
                     #{for-car?\ 3663}#)
                   (if (self-evaluating? #{e\ 3657}#)
                     (values
                       'constant
                       #f
                       #{e\ 3657}#
                       #{w\ 3659}#
                       #{s\ 3660}#
                       #{mod\ 3662}#)
                     (values
                       'other
                       #f
                       #{e\ 3657}#
                       #{w\ 3659}#
                       #{s\ 3660}#
                       #{mod\ 3662}#)))))))
         (#{chi-top\ 2579}#
           (lambda (#{e\ 3799}#
                    #{r\ 3800}#
                    #{w\ 3801}#
                    #{m\ 3802}#
                    #{esew\ 3803}#
                    #{mod\ 3804}#)
             (call-with-values
               (lambda ()
                 (#{syntax-type\ 2577}#
                   #{e\ 3799}#
                   #{r\ 3800}#
                   #{w\ 3801}#
                   (#{source-annotation\ 2475}# #{e\ 3799}#)
                   #f
                   #{mod\ 3804}#
                   #f))
               (lambda (#{type\ 3825}#
                        #{value\ 3826}#
                        #{e\ 3827}#
                        #{w\ 3828}#
                        #{s\ 3829}#
                        #{mod\ 3830}#)
                 (if (eqv? #{type\ 3825}# (quote begin-form))
                   (let ((#{tmp\ 3838}# #{e\ 3827}#))
                     (let ((#{tmp\ 3839}#
                             ($sc-dispatch #{tmp\ 3838}# (quote (any)))))
                       (if #{tmp\ 3839}#
                         (@apply
                           (lambda (#{_\ 3841}#) (#{chi-void\ 2595}#))
                           #{tmp\ 3839}#)
                         (let ((#{tmp\ 3842}#
                                 ($sc-dispatch
                                   #{tmp\ 3838}#
                                   '(any any . each-any))))
                           (if #{tmp\ 3842}#
                             (@apply
                               (lambda (#{_\ 3846}# #{e1\ 3847}# #{e2\ 3848}#)
                                 (#{chi-top-sequence\ 2571}#
                                   (cons #{e1\ 3847}# #{e2\ 3848}#)
                                   #{r\ 3800}#
                                   #{w\ 3828}#
                                   #{s\ 3829}#
                                   #{m\ 3802}#
                                   #{esew\ 3803}#
                                   #{mod\ 3830}#))
                               #{tmp\ 3842}#)
                             (syntax-violation
                               #f
                               "source expression failed to match any pattern"
                               #{tmp\ 3838}#))))))
                   (if (eqv? #{type\ 3825}# (quote local-syntax-form))
                     (#{chi-local-syntax\ 2591}#
                       #{value\ 3826}#
                       #{e\ 3827}#
                       #{r\ 3800}#
                       #{w\ 3828}#
                       #{s\ 3829}#
                       #{mod\ 3830}#
                       (lambda (#{body\ 3851}#
                                #{r\ 3852}#
                                #{w\ 3853}#
                                #{s\ 3854}#
                                #{mod\ 3855}#)
                         (#{chi-top-sequence\ 2571}#
                           #{body\ 3851}#
                           #{r\ 3852}#
                           #{w\ 3853}#
                           #{s\ 3854}#
                           #{m\ 3802}#
                           #{esew\ 3803}#
                           #{mod\ 3855}#)))
                     (if (eqv? #{type\ 3825}# (quote eval-when-form))
                       (let ((#{tmp\ 3862}# #{e\ 3827}#))
                         (let ((#{tmp\ 3863}#
                                 ($sc-dispatch
                                   #{tmp\ 3862}#
                                   '(any each-any any . each-any))))
                           (if #{tmp\ 3863}#
                             (@apply
                               (lambda (#{_\ 3868}#
                                        #{x\ 3869}#
                                        #{e1\ 3870}#
                                        #{e2\ 3871}#)
                                 (begin
                                   (let ((#{when-list\ 3874}#
                                           (#{chi-when-list\ 2575}#
                                             #{e\ 3827}#
                                             #{x\ 3869}#
                                             #{w\ 3828}#))
                                         (#{body\ 3875}#
                                           (cons #{e1\ 3870}# #{e2\ 3871}#)))
                                     (if (eq? #{m\ 3802}# (quote e))
                                       (if (memq 'eval
                                                 #{when-list\ 3874}#)
                                         (#{chi-top-sequence\ 2571}#
                                           #{body\ 3875}#
                                           #{r\ 3800}#
                                           #{w\ 3828}#
                                           #{s\ 3829}#
                                           (if (memq 'expand
                                                     #{when-list\ 3874}#)
                                             'c&e
                                             'e)
                                           '(eval)
                                           #{mod\ 3830}#)
                                         (begin
                                           (if (memq 'expand
                                                     #{when-list\ 3874}#)
                                             (#{top-level-eval-hook\ 2405}#
                                               (#{chi-top-sequence\ 2571}#
                                                 #{body\ 3875}#
                                                 #{r\ 3800}#
                                                 #{w\ 3828}#
                                                 #{s\ 3829}#
                                                 'e
                                                 '(eval)
                                                 #{mod\ 3830}#)
                                               #{mod\ 3830}#))
                                           (#{chi-void\ 2595}#)))
                                       (if (memq 'load
                                                 #{when-list\ 3874}#)
                                         (if (begin
                                               (let ((#{t\ 3884}#
                                                       (memq 'compile
                                                             #{when-list\ 3874}#)))
                                                 (if #{t\ 3884}#
                                                   #{t\ 3884}#
                                                   (begin
                                                     (let ((#{t\ 3887}#
                                                             (memq 'expand
                                                                   #{when-list\ 3874}#)))
                                                       (if #{t\ 3887}#
                                                         #{t\ 3887}#
                                                         (if (eq? #{m\ 3802}#
                                                                  'c&e)
                                                           (memq 'eval
                                                                 #{when-list\ 3874}#)
                                                           #f)))))))
                                           (#{chi-top-sequence\ 2571}#
                                             #{body\ 3875}#
                                             #{r\ 3800}#
                                             #{w\ 3828}#
                                             #{s\ 3829}#
                                             'c&e
                                             '(compile load)
                                             #{mod\ 3830}#)
                                           (if (if (eq? #{m\ 3802}# (quote c))
                                                 #t
                                                 (eq? #{m\ 3802}# (quote c&e)))
                                             (#{chi-top-sequence\ 2571}#
                                               #{body\ 3875}#
                                               #{r\ 3800}#
                                               #{w\ 3828}#
                                               #{s\ 3829}#
                                               'c
                                               '(load)
                                               #{mod\ 3830}#)
                                             (#{chi-void\ 2595}#)))
                                         (if (begin
                                               (let ((#{t\ 3895}#
                                                       (memq 'compile
                                                             #{when-list\ 3874}#)))
                                                 (if #{t\ 3895}#
                                                   #{t\ 3895}#
                                                   (begin
                                                     (let ((#{t\ 3898}#
                                                             (memq 'expand
                                                                   #{when-list\ 3874}#)))
                                                       (if #{t\ 3898}#
                                                         #{t\ 3898}#
                                                         (if (eq? #{m\ 3802}#
                                                                  'c&e)
                                                           (memq 'eval
                                                                 #{when-list\ 3874}#)
                                                           #f)))))))
                                           (begin
                                             (#{top-level-eval-hook\ 2405}#
                                               (#{chi-top-sequence\ 2571}#
                                                 #{body\ 3875}#
                                                 #{r\ 3800}#
                                                 #{w\ 3828}#
                                                 #{s\ 3829}#
                                                 'e
                                                 '(eval)
                                                 #{mod\ 3830}#)
                                               #{mod\ 3830}#)
                                             (#{chi-void\ 2595}#))
                                           (#{chi-void\ 2595}#)))))))
                               #{tmp\ 3863}#)
                             (syntax-violation
                               #f
                               "source expression failed to match any pattern"
                               #{tmp\ 3862}#))))
                       (if (eqv? #{type\ 3825}# (quote define-syntax-form))
                         (begin
                           (let ((#{n\ 3906}#
                                   (#{id-var-name\ 2553}#
                                     #{value\ 3826}#
                                     #{w\ 3828}#))
                                 (#{r\ 3907}#
                                   (#{macros-only-env\ 2488}# #{r\ 3800}#)))
                             (if (eqv? #{m\ 3802}# (quote c))
                               (if (memq (quote compile) #{esew\ 3803}#)
                                 (begin
                                   (let ((#{e\ 3910}#
                                           (#{chi-install-global\ 2573}#
                                             #{n\ 3906}#
                                             (#{chi\ 2581}#
                                               #{e\ 3827}#
                                               #{r\ 3907}#
                                               #{w\ 3828}#
                                               #{mod\ 3830}#))))
                                     (begin
                                       (#{top-level-eval-hook\ 2405}#
                                         #{e\ 3910}#
                                         #{mod\ 3830}#)
                                       (if (memq (quote load) #{esew\ 3803}#)
                                         #{e\ 3910}#
                                         (#{chi-void\ 2595}#)))))
                                 (if (memq (quote load) #{esew\ 3803}#)
                                   (#{chi-install-global\ 2573}#
                                     #{n\ 3906}#
                                     (#{chi\ 2581}#
                                       #{e\ 3827}#
                                       #{r\ 3907}#
                                       #{w\ 3828}#
                                       #{mod\ 3830}#))
                                   (#{chi-void\ 2595}#)))
                               (if (eqv? #{m\ 3802}# (quote c&e))
                                 (begin
                                   (let ((#{e\ 3913}#
                                           (#{chi-install-global\ 2573}#
                                             #{n\ 3906}#
                                             (#{chi\ 2581}#
                                               #{e\ 3827}#
                                               #{r\ 3907}#
                                               #{w\ 3828}#
                                               #{mod\ 3830}#))))
                                     (begin
                                       (#{top-level-eval-hook\ 2405}#
                                         #{e\ 3913}#
                                         #{mod\ 3830}#)
                                       #{e\ 3913}#)))
                                 (begin
                                   (if (memq (quote eval) #{esew\ 3803}#)
                                     (#{top-level-eval-hook\ 2405}#
                                       (#{chi-install-global\ 2573}#
                                         #{n\ 3906}#
                                         (#{chi\ 2581}#
                                           #{e\ 3827}#
                                           #{r\ 3907}#
                                           #{w\ 3828}#
                                           #{mod\ 3830}#))
                                       #{mod\ 3830}#))
                                   (#{chi-void\ 2595}#))))))
                         (if (eqv? #{type\ 3825}# (quote define-form))
                           (begin
                             (let ((#{n\ 3918}#
                                     (#{id-var-name\ 2553}#
                                       #{value\ 3826}#
                                       #{w\ 3828}#)))
                               (begin
                                 (let ((#{type\ 3920}#
                                         (#{binding-type\ 2479}#
                                           (#{lookup\ 2490}#
                                             #{n\ 3918}#
                                             #{r\ 3800}#
                                             #{mod\ 3830}#))))
                                   (if (if (eqv? #{type\ 3920}# (quote global))
                                         #t
                                         (if (eqv? #{type\ 3920}# (quote core))
                                           #t
                                           (if (eqv? #{type\ 3920}#
                                                     'macro)
                                             #t
                                             (eqv? #{type\ 3920}#
                                                   'module-ref))))
                                     (begin
                                       (if (if (if (eq? #{m\ 3802}# (quote c))
                                                 #t
                                                 (eq? #{m\ 3802}# (quote c&e)))
                                             (if (not (module-local-variable
                                                        (current-module)
                                                        #{n\ 3918}#))
                                               (current-module)
                                               #f)
                                             #f)
                                         (begin
                                           (let ((#{old\ 3926}#
                                                   (module-variable
                                                     (current-module)
                                                     #{n\ 3918}#)))
                                             (module-define!
                                               (current-module)
                                               #{n\ 3918}#
                                               (if (variable? #{old\ 3926}#)
                                                 (variable-ref #{old\ 3926}#)
                                                 #f)))))
                                       (begin
                                         (let ((#{x\ 3929}#
                                                 (#{build-global-definition\ 2436}#
                                                   #{s\ 3829}#
                                                   #{n\ 3918}#
                                                   (#{chi\ 2581}#
                                                     #{e\ 3827}#
                                                     #{r\ 3800}#
                                                     #{w\ 3828}#
                                                     #{mod\ 3830}#))))
                                           (begin
                                             (if (eq? #{m\ 3802}# (quote c&e))
                                               (#{top-level-eval-hook\ 2405}#
                                                 #{x\ 3929}#
                                                 #{mod\ 3830}#))
                                             #{x\ 3929}#))))
                                     (if (eqv? #{type\ 3920}#
                                               'displaced-lexical)
                                       (syntax-violation
                                         #f
                                         "identifier out of context"
                                         #{e\ 3827}#
                                         (#{wrap\ 2565}#
                                           #{value\ 3826}#
                                           #{w\ 3828}#
                                           #{mod\ 3830}#))
                                       (syntax-violation
                                         #f
                                         "cannot define keyword at top level"
                                         #{e\ 3827}#
                                         (#{wrap\ 2565}#
                                           #{value\ 3826}#
                                           #{w\ 3828}#
                                           #{mod\ 3830}#))))))))
                           (begin
                             (let ((#{x\ 3935}#
                                     (#{chi-expr\ 2583}#
                                       #{type\ 3825}#
                                       #{value\ 3826}#
                                       #{e\ 3827}#
                                       #{r\ 3800}#
                                       #{w\ 3828}#
                                       #{s\ 3829}#
                                       #{mod\ 3830}#)))
                               (begin
                                 (if (eq? #{m\ 3802}# (quote c&e))
                                   (#{top-level-eval-hook\ 2405}#
                                     #{x\ 3935}#
                                     #{mod\ 3830}#))
                                 #{x\ 3935}#))))))))))))
         (#{chi\ 2581}#
           (lambda (#{e\ 3936}#
                    #{r\ 3937}#
                    #{w\ 3938}#
                    #{mod\ 3939}#)
             (call-with-values
               (lambda ()
                 (#{syntax-type\ 2577}#
                   #{e\ 3936}#
                   #{r\ 3937}#
                   #{w\ 3938}#
                   (#{source-annotation\ 2475}# #{e\ 3936}#)
                   #f
                   #{mod\ 3939}#
                   #f))
               (lambda (#{type\ 3944}#
                        #{value\ 3945}#
                        #{e\ 3946}#
                        #{w\ 3947}#
                        #{s\ 3948}#
                        #{mod\ 3949}#)
                 (#{chi-expr\ 2583}#
                   #{type\ 3944}#
                   #{value\ 3945}#
                   #{e\ 3946}#
                   #{r\ 3937}#
                   #{w\ 3947}#
                   #{s\ 3948}#
                   #{mod\ 3949}#)))))
         (#{chi-expr\ 2583}#
           (lambda (#{type\ 3956}#
                    #{value\ 3957}#
                    #{e\ 3958}#
                    #{r\ 3959}#
                    #{w\ 3960}#
                    #{s\ 3961}#
                    #{mod\ 3962}#)
             (if (eqv? #{type\ 3956}# (quote lexical))
               (#{build-lexical-reference\ 2426}#
                 'value
                 #{s\ 3961}#
                 #{e\ 3958}#
                 #{value\ 3957}#)
               (if (if (eqv? #{type\ 3956}# (quote core))
                     #t
                     (eqv? #{type\ 3956}# (quote core-form)))
                 (#{value\ 3957}#
                   #{e\ 3958}#
                   #{r\ 3959}#
                   #{w\ 3960}#
                   #{s\ 3961}#
                   #{mod\ 3962}#)
                 (if (eqv? #{type\ 3956}# (quote module-ref))
                   (call-with-values
                     (lambda ()
                       (#{value\ 3957}#
                         #{e\ 3958}#
                         #{r\ 3959}#
                         #{w\ 3960}#))
                     (lambda (#{e\ 3973}#
                              #{r\ 3974}#
                              #{w\ 3975}#
                              #{s\ 3976}#
                              #{mod\ 3977}#)
                       (#{chi\ 2581}#
                         #{e\ 3973}#
                         #{r\ 3974}#
                         #{w\ 3975}#
                         #{mod\ 3977}#)))
                   (if (eqv? #{type\ 3956}# (quote lexical-call))
                     (#{chi-application\ 2585}#
                       (begin
                         (let ((#{id\ 3985}# (car #{e\ 3958}#)))
                           (#{build-lexical-reference\ 2426}#
                             'fun
                             (#{source-annotation\ 2475}# #{id\ 3985}#)
                             (if (#{syntax-object?\ 2460}# #{id\ 3985}#)
                               (syntax->datum #{id\ 3985}#)
                               #{id\ 3985}#)
                             #{value\ 3957}#)))
                       #{e\ 3958}#
                       #{r\ 3959}#
                       #{w\ 3960}#
                       #{s\ 3961}#
                       #{mod\ 3962}#)
                     (if (eqv? #{type\ 3956}# (quote global-call))
                       (#{chi-application\ 2585}#
                         (#{build-global-reference\ 2432}#
                           (#{source-annotation\ 2475}# (car #{e\ 3958}#))
                           (if (#{syntax-object?\ 2460}# #{value\ 3957}#)
                             (#{syntax-object-expression\ 2462}#
                               #{value\ 3957}#)
                             #{value\ 3957}#)
                           (if (#{syntax-object?\ 2460}# #{value\ 3957}#)
                             (#{syntax-object-module\ 2466}# #{value\ 3957}#)
                             #{mod\ 3962}#))
                         #{e\ 3958}#
                         #{r\ 3959}#
                         #{w\ 3960}#
                         #{s\ 3961}#
                         #{mod\ 3962}#)
                       (if (eqv? #{type\ 3956}# (quote constant))
                         (#{build-data\ 2446}#
                           #{s\ 3961}#
                           (#{strip\ 2607}#
                             (#{source-wrap\ 2567}#
                               #{e\ 3958}#
                               #{w\ 3960}#
                               #{s\ 3961}#
                               #{mod\ 3962}#)
                             '(())))
                         (if (eqv? #{type\ 3956}# (quote global))
                           (#{build-global-reference\ 2432}#
                             #{s\ 3961}#
                             #{value\ 3957}#
                             #{mod\ 3962}#)
                           (if (eqv? #{type\ 3956}# (quote call))
                             (#{chi-application\ 2585}#
                               (#{chi\ 2581}#
                                 (car #{e\ 3958}#)
                                 #{r\ 3959}#
                                 #{w\ 3960}#
                                 #{mod\ 3962}#)
                               #{e\ 3958}#
                               #{r\ 3959}#
                               #{w\ 3960}#
                               #{s\ 3961}#
                               #{mod\ 3962}#)
                             (if (eqv? #{type\ 3956}# (quote begin-form))
                               (let ((#{tmp\ 3992}# #{e\ 3958}#))
                                 (let ((#{tmp\ 3993}#
                                         ($sc-dispatch
                                           #{tmp\ 3992}#
                                           '(any any . each-any))))
                                   (if #{tmp\ 3993}#
                                     (@apply
                                       (lambda (#{_\ 3997}#
                                                #{e1\ 3998}#
                                                #{e2\ 3999}#)
                                         (#{chi-sequence\ 2569}#
                                           (cons #{e1\ 3998}# #{e2\ 3999}#)
                                           #{r\ 3959}#
                                           #{w\ 3960}#
                                           #{s\ 3961}#
                                           #{mod\ 3962}#))
                                       #{tmp\ 3993}#)
                                     (syntax-violation
                                       #f
                                       "source expression failed to match any pattern"
                                       #{tmp\ 3992}#))))
                               (if (eqv? #{type\ 3956}#
                                         'local-syntax-form)
                                 (#{chi-local-syntax\ 2591}#
                                   #{value\ 3957}#
                                   #{e\ 3958}#
                                   #{r\ 3959}#
                                   #{w\ 3960}#
                                   #{s\ 3961}#
                                   #{mod\ 3962}#
                                   #{chi-sequence\ 2569}#)
                                 (if (eqv? #{type\ 3956}#
                                           'eval-when-form)
                                   (let ((#{tmp\ 4003}# #{e\ 3958}#))
                                     (let ((#{tmp\ 4004}#
                                             ($sc-dispatch
                                               #{tmp\ 4003}#
                                               '(any each-any
                                                     any
                                                     .
                                                     each-any))))
                                       (if #{tmp\ 4004}#
                                         (@apply
                                           (lambda (#{_\ 4009}#
                                                    #{x\ 4010}#
                                                    #{e1\ 4011}#
                                                    #{e2\ 4012}#)
                                             (begin
                                               (let ((#{when-list\ 4014}#
                                                       (#{chi-when-list\ 2575}#
                                                         #{e\ 3958}#
                                                         #{x\ 4010}#
                                                         #{w\ 3960}#)))
                                                 (if (memq 'eval
                                                           #{when-list\ 4014}#)
                                                   (#{chi-sequence\ 2569}#
                                                     (cons #{e1\ 4011}#
                                                           #{e2\ 4012}#)
                                                     #{r\ 3959}#
                                                     #{w\ 3960}#
                                                     #{s\ 3961}#
                                                     #{mod\ 3962}#)
                                                   (#{chi-void\ 2595}#)))))
                                           #{tmp\ 4004}#)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           #{tmp\ 4003}#))))
                                   (if (if (eqv? #{type\ 3956}#
                                                 'define-form)
                                         #t
                                         (eqv? #{type\ 3956}#
                                               'define-syntax-form))
                                     (syntax-violation
                                       #f
                                       "definition in expression context"
                                       #{e\ 3958}#
                                       (#{wrap\ 2565}#
                                         #{value\ 3957}#
                                         #{w\ 3960}#
                                         #{mod\ 3962}#))
                                     (if (eqv? #{type\ 3956}# (quote syntax))
                                       (syntax-violation
                                         #f
                                         "reference to pattern variable outside syntax form"
                                         (#{source-wrap\ 2567}#
                                           #{e\ 3958}#
                                           #{w\ 3960}#
                                           #{s\ 3961}#
                                           #{mod\ 3962}#))
                                       (if (eqv? #{type\ 3956}#
                                                 'displaced-lexical)
                                         (syntax-violation
                                           #f
                                           "reference to identifier outside its scope"
                                           (#{source-wrap\ 2567}#
                                             #{e\ 3958}#
                                             #{w\ 3960}#
                                             #{s\ 3961}#
                                             #{mod\ 3962}#))
                                         (syntax-violation
                                           #f
                                           "unexpected syntax"
                                           (#{source-wrap\ 2567}#
                                             #{e\ 3958}#
                                             #{w\ 3960}#
                                             #{s\ 3961}#
                                             #{mod\ 3962}#))))))))))))))))))
         (#{chi-application\ 2585}#
           (lambda (#{x\ 4021}#
                    #{e\ 4022}#
                    #{r\ 4023}#
                    #{w\ 4024}#
                    #{s\ 4025}#
                    #{mod\ 4026}#)
             (let ((#{tmp\ 4033}# #{e\ 4022}#))
               (let ((#{tmp\ 4034}#
                       ($sc-dispatch
                         #{tmp\ 4033}#
                         '(any . each-any))))
                 (if #{tmp\ 4034}#
                   (@apply
                     (lambda (#{e0\ 4037}# #{e1\ 4038}#)
                       (#{build-application\ 2420}#
                         #{s\ 4025}#
                         #{x\ 4021}#
                         (map (lambda (#{e\ 4039}#)
                                (#{chi\ 2581}#
                                  #{e\ 4039}#
                                  #{r\ 4023}#
                                  #{w\ 4024}#
                                  #{mod\ 4026}#))
                              #{e1\ 4038}#)))
                     #{tmp\ 4034}#)
                   (syntax-violation
                     #f
                     "source expression failed to match any pattern"
                     #{tmp\ 4033}#))))))
         (#{chi-macro\ 2587}#
           (lambda (#{p\ 4042}#
                    #{e\ 4043}#
                    #{r\ 4044}#
                    #{w\ 4045}#
                    #{s\ 4046}#
                    #{rib\ 4047}#
                    #{mod\ 4048}#)
             (letrec*
               ((#{rebuild-macro-output\ 4057}#
                  (lambda (#{x\ 4058}# #{m\ 4059}#)
                    (if (pair? #{x\ 4058}#)
                      (#{decorate-source\ 2414}#
                        (cons (#{rebuild-macro-output\ 4057}#
                                (car #{x\ 4058}#)
                                #{m\ 4059}#)
                              (#{rebuild-macro-output\ 4057}#
                                (cdr #{x\ 4058}#)
                                #{m\ 4059}#))
                        #{s\ 4046}#)
                      (if (#{syntax-object?\ 2460}# #{x\ 4058}#)
                        (begin
                          (let ((#{w\ 4067}#
                                  (#{syntax-object-wrap\ 2464}# #{x\ 4058}#)))
                            (begin
                              (let ((#{ms\ 4070}#
                                      (#{wrap-marks\ 2503}# #{w\ 4067}#))
                                    (#{s\ 4071}#
                                      (#{wrap-subst\ 2505}# #{w\ 4067}#)))
                                (if (if (pair? #{ms\ 4070}#)
                                      (eq? (car #{ms\ 4070}#) #f)
                                      #f)
                                  (#{make-syntax-object\ 2458}#
                                    (#{syntax-object-expression\ 2462}#
                                      #{x\ 4058}#)
                                    (#{make-wrap\ 2501}#
                                      (cdr #{ms\ 4070}#)
                                      (if #{rib\ 4047}#
                                        (cons #{rib\ 4047}# (cdr #{s\ 4071}#))
                                        (cdr #{s\ 4071}#)))
                                    (#{syntax-object-module\ 2466}#
                                      #{x\ 4058}#))
                                  (#{make-syntax-object\ 2458}#
                                    (#{decorate-source\ 2414}#
                                      (#{syntax-object-expression\ 2462}#
                                        #{x\ 4058}#)
                                      #{s\ 4071}#)
                                    (#{make-wrap\ 2501}#
                                      (cons #{m\ 4059}# #{ms\ 4070}#)
                                      (if #{rib\ 4047}#
                                        (cons #{rib\ 4047}#
                                              (cons (quote shift) #{s\ 4071}#))
                                        (cons (quote shift) #{s\ 4071}#)))
                                    (#{syntax-object-module\ 2466}#
                                      #{x\ 4058}#)))))))
                        (if (vector? #{x\ 4058}#)
                          (begin
                            (let ((#{n\ 4079}# (vector-length #{x\ 4058}#)))
                              (begin
                                (let ((#{v\ 4081}#
                                        (#{decorate-source\ 2414}#
                                          (make-vector #{n\ 4079}#)
                                          #{x\ 4058}#)))
                                  (letrec*
                                    ((#{loop\ 4084}#
                                       (lambda (#{i\ 4085}#)
                                         (if (#{fx=\ 2401}#
                                               #{i\ 4085}#
                                               #{n\ 4079}#)
                                           (begin (if #f #f) #{v\ 4081}#)
                                           (begin
                                             (vector-set!
                                               #{v\ 4081}#
                                               #{i\ 4085}#
                                               (#{rebuild-macro-output\ 4057}#
                                                 (vector-ref
                                                   #{x\ 4058}#
                                                   #{i\ 4085}#)
                                                 #{m\ 4059}#))
                                             (#{loop\ 4084}#
                                               (#{fx+\ 2397}#
                                                 #{i\ 4085}#
                                                 1)))))))
                                    (begin (#{loop\ 4084}# 0)))))))
                          (if (symbol? #{x\ 4058}#)
                            (syntax-violation
                              #f
                              "encountered raw symbol in macro output"
                              (#{source-wrap\ 2567}#
                                #{e\ 4043}#
                                #{w\ 4045}#
                                (#{wrap-subst\ 2505}# #{w\ 4045}#)
                                #{mod\ 4048}#)
                              #{x\ 4058}#)
                            (#{decorate-source\ 2414}#
                              #{x\ 4058}#
                              #{s\ 4046}#))))))))
               (begin
                 (#{rebuild-macro-output\ 4057}#
                   (#{p\ 4042}#
                     (#{source-wrap\ 2567}#
                       #{e\ 4043}#
                       (#{anti-mark\ 2537}# #{w\ 4045}#)
                       #{s\ 4046}#
                       #{mod\ 4048}#))
                   (gensym "m"))))))
         (#{chi-body\ 2589}#
           (lambda (#{body\ 4092}#
                    #{outer-form\ 4093}#
                    #{r\ 4094}#
                    #{w\ 4095}#
                    #{mod\ 4096}#)
             (begin
               (let ((#{r\ 4104}#
                       (cons '("placeholder" placeholder)
                             #{r\ 4094}#)))
                 (begin
                   (let ((#{ribcage\ 4106}#
                           (#{make-ribcage\ 2517}#
                             '()
                             '()
                             '())))
                     (begin
                       (let ((#{w\ 4109}#
                               (#{make-wrap\ 2501}#
                                 (#{wrap-marks\ 2503}# #{w\ 4095}#)
                                 (cons #{ribcage\ 4106}#
                                       (#{wrap-subst\ 2505}# #{w\ 4095}#)))))
                         (letrec*
                           ((#{parse\ 4118}#
                              (lambda (#{body\ 4119}#
                                       #{ids\ 4120}#
                                       #{labels\ 4121}#
                                       #{var-ids\ 4122}#
                                       #{vars\ 4123}#
                                       #{vals\ 4124}#
                                       #{bindings\ 4125}#)
                                (if (null? #{body\ 4119}#)
                                  (syntax-violation
                                    #f
                                    "no expressions in body"
                                    #{outer-form\ 4093}#)
                                  (begin
                                    (let ((#{e\ 4130}#
                                            (cdr (car #{body\ 4119}#)))
                                          (#{er\ 4131}#
                                            (car (car #{body\ 4119}#))))
                                      (call-with-values
                                        (lambda ()
                                          (#{syntax-type\ 2577}#
                                            #{e\ 4130}#
                                            #{er\ 4131}#
                                            '(())
                                            (#{source-annotation\ 2475}#
                                              #{er\ 4131}#)
                                            #{ribcage\ 4106}#
                                            #{mod\ 4096}#
                                            #f))
                                        (lambda (#{type\ 4133}#
                                                 #{value\ 4134}#
                                                 #{e\ 4135}#
                                                 #{w\ 4136}#
                                                 #{s\ 4137}#
                                                 #{mod\ 4138}#)
                                          (if (eqv? #{type\ 4133}#
                                                    'define-form)
                                            (begin
                                              (let ((#{id\ 4148}#
                                                      (#{wrap\ 2565}#
                                                        #{value\ 4134}#
                                                        #{w\ 4136}#
                                                        #{mod\ 4138}#))
                                                    (#{label\ 4149}#
                                                      (#{gen-label\ 2512}#)))
                                                (begin
                                                  (let ((#{var\ 4151}#
                                                          (#{gen-var\ 2609}#
                                                            #{id\ 4148}#)))
                                                    (begin
                                                      (#{extend-ribcage!\ 2541}#
                                                        #{ribcage\ 4106}#
                                                        #{id\ 4148}#
                                                        #{label\ 4149}#)
                                                      (#{parse\ 4118}#
                                                        (cdr #{body\ 4119}#)
                                                        (cons #{id\ 4148}#
                                                              #{ids\ 4120}#)
                                                        (cons #{label\ 4149}#
                                                              #{labels\ 4121}#)
                                                        (cons #{id\ 4148}#
                                                              #{var-ids\ 4122}#)
                                                        (cons #{var\ 4151}#
                                                              #{vars\ 4123}#)
                                                        (cons (cons #{er\ 4131}#
                                                                    (#{wrap\ 2565}#
                                                                      #{e\ 4135}#
                                                                      #{w\ 4136}#
                                                                      #{mod\ 4138}#))
                                                              #{vals\ 4124}#)
                                                        (cons (cons 'lexical
                                                                    #{var\ 4151}#)
                                                              #{bindings\ 4125}#)))))))
                                            (if (eqv? #{type\ 4133}#
                                                      'define-syntax-form)
                                              (begin
                                                (let ((#{id\ 4156}#
                                                        (#{wrap\ 2565}#
                                                          #{value\ 4134}#
                                                          #{w\ 4136}#
                                                          #{mod\ 4138}#))
                                                      (#{label\ 4157}#
                                                        (#{gen-label\ 2512}#)))
                                                  (begin
                                                    (#{extend-ribcage!\ 2541}#
                                                      #{ribcage\ 4106}#
                                                      #{id\ 4156}#
                                                      #{label\ 4157}#)
                                                    (#{parse\ 4118}#
                                                      (cdr #{body\ 4119}#)
                                                      (cons #{id\ 4156}#
                                                            #{ids\ 4120}#)
                                                      (cons #{label\ 4157}#
                                                            #{labels\ 4121}#)
                                                      #{var-ids\ 4122}#
                                                      #{vars\ 4123}#
                                                      #{vals\ 4124}#
                                                      (cons (cons 'macro
                                                                  (cons #{er\ 4131}#
                                                                        (#{wrap\ 2565}#
                                                                          #{e\ 4135}#
                                                                          #{w\ 4136}#
                                                                          #{mod\ 4138}#)))
                                                            #{bindings\ 4125}#)))))
                                              (if (eqv? #{type\ 4133}#
                                                        'begin-form)
                                                (let ((#{tmp\ 4160}#
                                                        #{e\ 4135}#))
                                                  (let ((#{tmp\ 4161}#
                                                          ($sc-dispatch
                                                            #{tmp\ 4160}#
                                                            '(any .
                                                                  each-any))))
                                                    (if #{tmp\ 4161}#
                                                      (@apply
                                                        (lambda (#{_\ 4164}#
                                                                 #{e1\ 4165}#)
                                                          (#{parse\ 4118}#
                                                            (letrec*
                                                              ((#{f\ 4168}#
                                                                 (lambda (#{forms\ 4169}#)
                                                                   (if (null? #{forms\ 4169}#)
                                                                     (cdr #{body\ 4119}#)
                                                                     (cons (cons #{er\ 4131}#
                                                                                 (#{wrap\ 2565}#
                                                                                   (car #{forms\ 4169}#)
                                                                                   #{w\ 4136}#
                                                                                   #{mod\ 4138}#))
                                                                           (#{f\ 4168}#
                                                                             (cdr #{forms\ 4169}#)))))))
                                                              (begin
                                                                (#{f\ 4168}#
                                                                  #{e1\ 4165}#)))
                                                            #{ids\ 4120}#
                                                            #{labels\ 4121}#
                                                            #{var-ids\ 4122}#
                                                            #{vars\ 4123}#
                                                            #{vals\ 4124}#
                                                            #{bindings\ 4125}#))
                                                        #{tmp\ 4161}#)
                                                      (syntax-violation
                                                        #f
                                                        "source expression failed to match any pattern"
                                                        #{tmp\ 4160}#))))
                                                (if (eqv? #{type\ 4133}#
                                                          'local-syntax-form)
                                                  (#{chi-local-syntax\ 2591}#
                                                    #{value\ 4134}#
                                                    #{e\ 4135}#
                                                    #{er\ 4131}#
                                                    #{w\ 4136}#
                                                    #{s\ 4137}#
                                                    #{mod\ 4138}#
                                                    (lambda (#{forms\ 4172}#
                                                             #{er\ 4173}#
                                                             #{w\ 4174}#
                                                             #{s\ 4175}#
                                                             #{mod\ 4176}#)
                                                      (#{parse\ 4118}#
                                                        (letrec*
                                                          ((#{f\ 4184}#
                                                             (lambda (#{forms\ 4185}#)
                                                               (if (null? #{forms\ 4185}#)
                                                                 (cdr #{body\ 4119}#)
                                                                 (cons (cons #{er\ 4173}#
                                                                             (#{wrap\ 2565}#
                                                                               (car #{forms\ 4185}#)
                                                                               #{w\ 4174}#
                                                                               #{mod\ 4176}#))
                                                                       (#{f\ 4184}#
                                                                         (cdr #{forms\ 4185}#)))))))
                                                          (begin
                                                            (#{f\ 4184}#
                                                              #{forms\ 4172}#)))
                                                        #{ids\ 4120}#
                                                        #{labels\ 4121}#
                                                        #{var-ids\ 4122}#
                                                        #{vars\ 4123}#
                                                        #{vals\ 4124}#
                                                        #{bindings\ 4125}#)))
                                                  (if (null? #{ids\ 4120}#)
                                                    (#{build-sequence\ 2448}#
                                                      #f
                                                      (map (lambda (#{x\ 4188}#)
                                                             (#{chi\ 2581}#
                                                               (cdr #{x\ 4188}#)
                                                               (car #{x\ 4188}#)
                                                               '(())
                                                               #{mod\ 4138}#))
                                                           (cons (cons #{er\ 4131}#
                                                                       (#{source-wrap\ 2567}#
                                                                         #{e\ 4135}#
                                                                         #{w\ 4136}#
                                                                         #{s\ 4137}#
                                                                         #{mod\ 4138}#))
                                                                 (cdr #{body\ 4119}#))))
                                                    (begin
                                                      (if (not (#{valid-bound-ids?\ 2559}#
                                                                 #{ids\ 4120}#))
                                                        (syntax-violation
                                                          #f
                                                          "invalid or duplicate identifier in definition"
                                                          #{outer-form\ 4093}#))
                                                      (letrec*
                                                        ((#{loop\ 4195}#
                                                           (lambda (#{bs\ 4196}#
                                                                    #{er-cache\ 4197}#
                                                                    #{r-cache\ 4198}#)
                                                             (if (not (null? #{bs\ 4196}#))
                                                               (begin
                                                                 (let ((#{b\ 4201}#
                                                                         (car #{bs\ 4196}#)))
                                                                   (if (eq? (car #{b\ 4201}#)
                                                                            'macro)
                                                                     (begin
                                                                       (let ((#{er\ 4204}#
                                                                               (car (cdr #{b\ 4201}#))))
                                                                         (begin
                                                                           (let ((#{r-cache\ 4206}#
                                                                                   (if (eq? #{er\ 4204}#
                                                                                            #{er-cache\ 4197}#)
                                                                                     #{r-cache\ 4198}#
                                                                                     (#{macros-only-env\ 2488}#
                                                                                       #{er\ 4204}#))))
                                                                             (begin
                                                                               (set-cdr!
                                                                                 #{b\ 4201}#
                                                                                 (#{eval-local-transformer\ 2593}#
                                                                                   (#{chi\ 2581}#
                                                                                     (cdr (cdr #{b\ 4201}#))
                                                                                     #{r-cache\ 4206}#
                                                                                     '(())
                                                                                     #{mod\ 4138}#)
                                                                                   #{mod\ 4138}#))
                                                                               (#{loop\ 4195}#
                                                                                 (cdr #{bs\ 4196}#)
                                                                                 #{er\ 4204}#
                                                                                 #{r-cache\ 4206}#))))))
                                                                     (#{loop\ 4195}#
                                                                       (cdr #{bs\ 4196}#)
                                                                       #{er-cache\ 4197}#
                                                                       #{r-cache\ 4198}#))))))))
                                                        (begin
                                                          (#{loop\ 4195}#
                                                            #{bindings\ 4125}#
                                                            #f
                                                            #f)))
                                                      (set-cdr!
                                                        #{r\ 4104}#
                                                        (#{extend-env\ 2484}#
                                                          #{labels\ 4121}#
                                                          #{bindings\ 4125}#
                                                          (cdr #{r\ 4104}#)))
                                                      (#{build-letrec\ 2454}#
                                                        #f
                                                        #t
                                                        (reverse
                                                          (map syntax->datum
                                                               #{var-ids\ 4122}#))
                                                        (reverse
                                                          #{vars\ 4123}#)
                                                        (map (lambda (#{x\ 4209}#)
                                                               (#{chi\ 2581}#
                                                                 (cdr #{x\ 4209}#)
                                                                 (car #{x\ 4209}#)
                                                                 '(())
                                                                 #{mod\ 4138}#))
                                                             (reverse
                                                               #{vals\ 4124}#))
                                                        (#{build-sequence\ 2448}#
                                                          #f
                                                          (map (lambda (#{x\ 4213}#)
                                                                 (#{chi\ 2581}#
                                                                   (cdr #{x\ 4213}#)
                                                                   (car #{x\ 4213}#)
                                                                   '(())
                                                                   #{mod\ 4138}#))
                                                               (cons (cons #{er\ 4131}#
                                                                           (#{source-wrap\ 2567}#
                                                                             #{e\ 4135}#
                                                                             #{w\ 4136}#
                                                                             #{s\ 4137}#
                                                                             #{mod\ 4138}#))
                                                                     (cdr #{body\ 4119}#)))))))))))))))))))
                           (begin
                             (#{parse\ 4118}#
                               (map (lambda (#{x\ 4126}#)
                                      (cons #{r\ 4104}#
                                            (#{wrap\ 2565}#
                                              #{x\ 4126}#
                                              #{w\ 4109}#
                                              #{mod\ 4096}#)))
                                    #{body\ 4092}#)
                               '()
                               '()
                               '()
                               '()
                               '()
                               '())))))))))))
         (#{chi-local-syntax\ 2591}#
           (lambda (#{rec?\ 4216}#
                    #{e\ 4217}#
                    #{r\ 4218}#
                    #{w\ 4219}#
                    #{s\ 4220}#
                    #{mod\ 4221}#
                    #{k\ 4222}#)
             (let ((#{tmp\ 4230}# #{e\ 4217}#))
               (let ((#{tmp\ 4231}#
                       ($sc-dispatch
                         #{tmp\ 4230}#
                         '(any #(each (any any)) any . each-any))))
                 (if #{tmp\ 4231}#
                   (@apply
                     (lambda (#{_\ 4237}#
                              #{id\ 4238}#
                              #{val\ 4239}#
                              #{e1\ 4240}#
                              #{e2\ 4241}#)
                       (begin
                         (let ((#{ids\ 4243}# #{id\ 4238}#))
                           (if (not (#{valid-bound-ids?\ 2559}# #{ids\ 4243}#))
                             (syntax-violation
                               #f
                               "duplicate bound keyword"
                               #{e\ 4217}#)
                             (begin
                               (let ((#{labels\ 4246}#
                                       (#{gen-labels\ 2514}# #{ids\ 4243}#)))
                                 (begin
                                   (let ((#{new-w\ 4248}#
                                           (#{make-binding-wrap\ 2543}#
                                             #{ids\ 4243}#
                                             #{labels\ 4246}#
                                             #{w\ 4219}#)))
                                     (#{k\ 4222}#
                                       (cons #{e1\ 4240}# #{e2\ 4241}#)
                                       (#{extend-env\ 2484}#
                                         #{labels\ 4246}#
                                         (begin
                                           (let ((#{w\ 4252}#
                                                   (if #{rec?\ 4216}#
                                                     #{new-w\ 4248}#
                                                     #{w\ 4219}#))
                                                 (#{trans-r\ 4253}#
                                                   (#{macros-only-env\ 2488}#
                                                     #{r\ 4218}#)))
                                             (map (lambda (#{x\ 4254}#)
                                                    (cons 'macro
                                                          (#{eval-local-transformer\ 2593}#
                                                            (#{chi\ 2581}#
                                                              #{x\ 4254}#
                                                              #{trans-r\ 4253}#
                                                              #{w\ 4252}#
                                                              #{mod\ 4221}#)
                                                            #{mod\ 4221}#)))
                                                  #{val\ 4239}#)))
                                         #{r\ 4218}#)
                                       #{new-w\ 4248}#
                                       #{s\ 4220}#
                                       #{mod\ 4221}#)))))))))
                     #{tmp\ 4231}#)
                   (let ((#{_\ 4259}# #{tmp\ 4230}#))
                     (syntax-violation
                       #f
                       "bad local syntax definition"
                       (#{source-wrap\ 2567}#
                         #{e\ 4217}#
                         #{w\ 4219}#
                         #{s\ 4220}#
                         #{mod\ 4221}#))))))))
         (#{eval-local-transformer\ 2593}#
           (lambda (#{expanded\ 4260}# #{mod\ 4261}#)
             (begin
               (let ((#{p\ 4265}#
                       (#{local-eval-hook\ 2407}#
                         #{expanded\ 4260}#
                         #{mod\ 4261}#)))
                 (if (procedure? #{p\ 4265}#)
                   #{p\ 4265}#
                   (syntax-violation
                     #f
                     "nonprocedure transformer"
                     #{p\ 4265}#))))))
         (#{chi-void\ 2595}#
           (lambda () (#{build-void\ 2418}# #f)))
         (#{ellipsis?\ 2597}#
           (lambda (#{x\ 4267}#)
             (if (#{nonsymbol-id?\ 2494}# #{x\ 4267}#)
               (#{free-id=?\ 2555}#
                 #{x\ 4267}#
                 '#(syntax-object
                    ...
                    ((top)
                     #(ribcage () () ())
                     #(ribcage () () ())
                     #(ribcage #(x) #((top)) #("i4268"))
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
                         gensym-hook
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
                        (top))
                       ("i2610"
                        "i2608"
                        "i2606"
                        "i2604"
                        "i2602"
                        "i2600"
                        "i2598"
                        "i2596"
                        "i2594"
                        "i2592"
                        "i2590"
                        "i2588"
                        "i2586"
                        "i2584"
                        "i2582"
                        "i2580"
                        "i2578"
                        "i2576"
                        "i2574"
                        "i2572"
                        "i2570"
                        "i2568"
                        "i2566"
                        "i2564"
                        "i2562"
                        "i2560"
                        "i2558"
                        "i2556"
                        "i2554"
                        "i2552"
                        "i2550"
                        "i2548"
                        "i2546"
                        "i2544"
                        "i2542"
                        "i2540"
                        "i2539"
                        "i2538"
                        "i2536"
                        "i2535"
                        "i2534"
                        "i2533"
                        "i2532"
                        "i2530"
                        "i2528"
                        "i2526"
                        "i2524"
                        "i2522"
                        "i2520"
                        "i2518"
                        "i2516"
                        "i2513"
                        "i2511"
                        "i2510"
                        "i2509"
                        "i2508"
                        "i2507"
                        "i2506"
                        "i2504"
                        "i2502"
                        "i2500"
                        "i2498"
                        "i2497"
                        "i2495"
                        "i2493"
                        "i2491"
                        "i2489"
                        "i2487"
                        "i2485"
                        "i2483"
                        "i2482"
                        "i2480"
                        "i2478"
                        "i2477"
                        "i2476"
                        "i2474"
                        "i2473"
                        "i2471"
                        "i2469"
                        "i2467"
                        "i2465"
                        "i2463"
                        "i2461"
                        "i2459"
                        "i2457"
                        "i2455"
                        "i2453"
                        "i2451"
                        "i2449"
                        "i2447"
                        "i2445"
                        "i2443"
                        "i2441"
                        "i2439"
                        "i2437"
                        "i2435"
                        "i2433"
                        "i2431"
                        "i2429"
                        "i2427"
                        "i2425"
                        "i2423"
                        "i2421"
                        "i2419"
                        "i2417"
                        "i2415"
                        "i2413"
                        "i2411"
                        "i2409"
                        "i2408"
                        "i2406"
                        "i2404"
                        "i2402"
                        "i2400"
                        "i2398"
                        "i2396"
                        "i2394"
                        "i2392"
                        "i2390"
                        "i2387"
                        "i2385"
                        "i2383"
                        "i2381"
                        "i2379"
                        "i2377"
                        "i2375"
                        "i2373"
                        "i2371"
                        "i2369"
                        "i2367"
                        "i2365"
                        "i2363"
                        "i2361"
                        "i2359"
                        "i2357"
                        "i2355"
                        "i2353"))
                     #(ribcage
                       (define-structure
                         define-expansion-accessors
                         define-expansion-constructors
                         and-map*)
                       ((top) (top) (top) (top))
                       ("i2147" "i2146" "i2145" "i2143")))
                    (hygiene guile)))
               #f)))
         (#{lambda-formals\ 2599}#
           (lambda (#{orig-args\ 4271}#)
             (letrec*
               ((#{req\ 4274}#
                  (lambda (#{args\ 4277}# #{rreq\ 4278}#)
                    (let ((#{tmp\ 4281}# #{args\ 4277}#))
                      (let ((#{tmp\ 4282}#
                              ($sc-dispatch #{tmp\ 4281}# (quote ()))))
                        (if #{tmp\ 4282}#
                          (@apply
                            (lambda ()
                              (#{check\ 4276}# (reverse #{rreq\ 4278}#) #f))
                            #{tmp\ 4282}#)
                          (let ((#{tmp\ 4283}#
                                  ($sc-dispatch
                                    #{tmp\ 4281}#
                                    '(any . any))))
                            (if (if #{tmp\ 4283}#
                                  (@apply
                                    (lambda (#{a\ 4286}# #{b\ 4287}#)
                                      (#{id?\ 2496}# #{a\ 4286}#))
                                    #{tmp\ 4283}#)
                                  #f)
                              (@apply
                                (lambda (#{a\ 4290}# #{b\ 4291}#)
                                  (#{req\ 4274}#
                                    #{b\ 4291}#
                                    (cons #{a\ 4290}# #{rreq\ 4278}#)))
                                #{tmp\ 4283}#)
                              (let ((#{tmp\ 4292}# (list #{tmp\ 4281}#)))
                                (if (if #{tmp\ 4292}#
                                      (@apply
                                        (lambda (#{r\ 4294}#)
                                          (#{id?\ 2496}# #{r\ 4294}#))
                                        #{tmp\ 4292}#)
                                      #f)
                                  (@apply
                                    (lambda (#{r\ 4296}#)
                                      (#{check\ 4276}#
                                        (reverse #{rreq\ 4278}#)
                                        #{r\ 4296}#))
                                    #{tmp\ 4292}#)
                                  (let ((#{else\ 4298}# #{tmp\ 4281}#))
                                    (syntax-violation
                                      'lambda
                                      "invalid argument list"
                                      #{orig-args\ 4271}#
                                      #{args\ 4277}#)))))))))))
                (#{check\ 4276}#
                  (lambda (#{req\ 4299}# #{rest\ 4300}#)
                    (if (#{distinct-bound-ids?\ 2561}#
                          (if #{rest\ 4300}#
                            (cons #{rest\ 4300}# #{req\ 4299}#)
                            #{req\ 4299}#))
                      (values #{req\ 4299}# #f #{rest\ 4300}# #f)
                      (syntax-violation
                        'lambda
                        "duplicate identifier in argument list"
                        #{orig-args\ 4271}#)))))
               (begin
                 (#{req\ 4274}# #{orig-args\ 4271}# (quote ()))))))
         (#{chi-simple-lambda\ 2601}#
           (lambda (#{e\ 4306}#
                    #{r\ 4307}#
                    #{w\ 4308}#
                    #{s\ 4309}#
                    #{mod\ 4310}#
                    #{req\ 4311}#
                    #{rest\ 4312}#
                    #{meta\ 4313}#
                    #{body\ 4314}#)
             (begin
               (let ((#{ids\ 4326}#
                       (if #{rest\ 4312}#
                         (append #{req\ 4311}# (list #{rest\ 4312}#))
                         #{req\ 4311}#)))
                 (begin
                   (let ((#{vars\ 4328}#
                           (map #{gen-var\ 2609}# #{ids\ 4326}#)))
                     (begin
                       (let ((#{labels\ 4330}#
                               (#{gen-labels\ 2514}# #{ids\ 4326}#)))
                         (#{build-simple-lambda\ 2438}#
                           #{s\ 4309}#
                           (map syntax->datum #{req\ 4311}#)
                           (if #{rest\ 4312}#
                             (syntax->datum #{rest\ 4312}#)
                             #f)
                           #{vars\ 4328}#
                           #{meta\ 4313}#
                           (#{chi-body\ 2589}#
                             #{body\ 4314}#
                             (#{source-wrap\ 2567}#
                               #{e\ 4306}#
                               #{w\ 4308}#
                               #{s\ 4309}#
                               #{mod\ 4310}#)
                             (#{extend-var-env\ 2486}#
                               #{labels\ 4330}#
                               #{vars\ 4328}#
                               #{r\ 4307}#)
                             (#{make-binding-wrap\ 2543}#
                               #{ids\ 4326}#
                               #{labels\ 4330}#
                               #{w\ 4308}#)
                             #{mod\ 4310}#))))))))))
         (#{lambda*-formals\ 2603}#
           (lambda (#{orig-args\ 4333}#)
             (letrec*
               ((#{req\ 4336}#
                  (lambda (#{args\ 4345}# #{rreq\ 4346}#)
                    (let ((#{tmp\ 4349}# #{args\ 4345}#))
                      (let ((#{tmp\ 4350}#
                              ($sc-dispatch #{tmp\ 4349}# (quote ()))))
                        (if #{tmp\ 4350}#
                          (@apply
                            (lambda ()
                              (#{check\ 4344}#
                                (reverse #{rreq\ 4346}#)
                                '()
                                #f
                                '()))
                            #{tmp\ 4350}#)
                          (let ((#{tmp\ 4351}#
                                  ($sc-dispatch
                                    #{tmp\ 4349}#
                                    '(any . any))))
                            (if (if #{tmp\ 4351}#
                                  (@apply
                                    (lambda (#{a\ 4354}# #{b\ 4355}#)
                                      (#{id?\ 2496}# #{a\ 4354}#))
                                    #{tmp\ 4351}#)
                                  #f)
                              (@apply
                                (lambda (#{a\ 4358}# #{b\ 4359}#)
                                  (#{req\ 4336}#
                                    #{b\ 4359}#
                                    (cons #{a\ 4358}# #{rreq\ 4346}#)))
                                #{tmp\ 4351}#)
                              (let ((#{tmp\ 4360}#
                                      ($sc-dispatch
                                        #{tmp\ 4349}#
                                        '(any . any))))
                                (if (if #{tmp\ 4360}#
                                      (@apply
                                        (lambda (#{a\ 4363}# #{b\ 4364}#)
                                          (eq? (syntax->datum #{a\ 4363}#)
                                               #:optional))
                                        #{tmp\ 4360}#)
                                      #f)
                                  (@apply
                                    (lambda (#{a\ 4367}# #{b\ 4368}#)
                                      (#{opt\ 4338}#
                                        #{b\ 4368}#
                                        (reverse #{rreq\ 4346}#)
                                        '()))
                                    #{tmp\ 4360}#)
                                  (let ((#{tmp\ 4369}#
                                          ($sc-dispatch
                                            #{tmp\ 4349}#
                                            '(any . any))))
                                    (if (if #{tmp\ 4369}#
                                          (@apply
                                            (lambda (#{a\ 4372}# #{b\ 4373}#)
                                              (eq? (syntax->datum #{a\ 4372}#)
                                                   #:key))
                                            #{tmp\ 4369}#)
                                          #f)
                                      (@apply
                                        (lambda (#{a\ 4376}# #{b\ 4377}#)
                                          (#{key\ 4340}#
                                            #{b\ 4377}#
                                            (reverse #{rreq\ 4346}#)
                                            '()
                                            '()))
                                        #{tmp\ 4369}#)
                                      (let ((#{tmp\ 4378}#
                                              ($sc-dispatch
                                                #{tmp\ 4349}#
                                                '(any any))))
                                        (if (if #{tmp\ 4378}#
                                              (@apply
                                                (lambda (#{a\ 4381}#
                                                         #{b\ 4382}#)
                                                  (eq? (syntax->datum
                                                         #{a\ 4381}#)
                                                       #:rest))
                                                #{tmp\ 4378}#)
                                              #f)
                                          (@apply
                                            (lambda (#{a\ 4385}# #{b\ 4386}#)
                                              (#{rest\ 4342}#
                                                #{b\ 4386}#
                                                (reverse #{rreq\ 4346}#)
                                                '()
                                                '()))
                                            #{tmp\ 4378}#)
                                          (let ((#{tmp\ 4387}#
                                                  (list #{tmp\ 4349}#)))
                                            (if (if #{tmp\ 4387}#
                                                  (@apply
                                                    (lambda (#{r\ 4389}#)
                                                      (#{id?\ 2496}#
                                                        #{r\ 4389}#))
                                                    #{tmp\ 4387}#)
                                                  #f)
                                              (@apply
                                                (lambda (#{r\ 4391}#)
                                                  (#{rest\ 4342}#
                                                    #{r\ 4391}#
                                                    (reverse #{rreq\ 4346}#)
                                                    '()
                                                    '()))
                                                #{tmp\ 4387}#)
                                              (let ((#{else\ 4393}#
                                                      #{tmp\ 4349}#))
                                                (syntax-violation
                                                  'lambda*
                                                  "invalid argument list"
                                                  #{orig-args\ 4333}#
                                                  #{args\ 4345}#)))))))))))))))))
                (#{opt\ 4338}#
                  (lambda (#{args\ 4394}# #{req\ 4395}# #{ropt\ 4396}#)
                    (let ((#{tmp\ 4400}# #{args\ 4394}#))
                      (let ((#{tmp\ 4401}#
                              ($sc-dispatch #{tmp\ 4400}# (quote ()))))
                        (if #{tmp\ 4401}#
                          (@apply
                            (lambda ()
                              (#{check\ 4344}#
                                #{req\ 4395}#
                                (reverse #{ropt\ 4396}#)
                                #f
                                '()))
                            #{tmp\ 4401}#)
                          (let ((#{tmp\ 4402}#
                                  ($sc-dispatch
                                    #{tmp\ 4400}#
                                    '(any . any))))
                            (if (if #{tmp\ 4402}#
                                  (@apply
                                    (lambda (#{a\ 4405}# #{b\ 4406}#)
                                      (#{id?\ 2496}# #{a\ 4405}#))
                                    #{tmp\ 4402}#)
                                  #f)
                              (@apply
                                (lambda (#{a\ 4409}# #{b\ 4410}#)
                                  (#{opt\ 4338}#
                                    #{b\ 4410}#
                                    #{req\ 4395}#
                                    (cons (cons #{a\ 4409}#
                                                '(#(syntax-object
                                                    #f
                                                    ((top)
                                                     #(ribcage
                                                       #(a b)
                                                       #((top) (top))
                                                       #("i4407" "i4408"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(args req ropt)
                                                       #((top) (top) (top))
                                                       #("i4397"
                                                         "i4398"
                                                         "i4399"))
                                                     #(ribcage
                                                       (check rest key opt req)
                                                       ((top)
                                                        (top)
                                                        (top)
                                                        (top)
                                                        (top))
                                                       ("i4343"
                                                        "i4341"
                                                        "i4339"
                                                        "i4337"
                                                        "i4335"))
                                                     #(ribcage
                                                       #(orig-args)
                                                       #((top))
                                                       #("i4334"))
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
                                                         gensym-hook
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
                                                        (top))
                                                       ("i2610"
                                                        "i2608"
                                                        "i2606"
                                                        "i2604"
                                                        "i2602"
                                                        "i2600"
                                                        "i2598"
                                                        "i2596"
                                                        "i2594"
                                                        "i2592"
                                                        "i2590"
                                                        "i2588"
                                                        "i2586"
                                                        "i2584"
                                                        "i2582"
                                                        "i2580"
                                                        "i2578"
                                                        "i2576"
                                                        "i2574"
                                                        "i2572"
                                                        "i2570"
                                                        "i2568"
                                                        "i2566"
                                                        "i2564"
                                                        "i2562"
                                                        "i2560"
                                                        "i2558"
                                                        "i2556"
                                                        "i2554"
                                                        "i2552"
                                                        "i2550"
                                                        "i2548"
                                                        "i2546"
                                                        "i2544"
                                                        "i2542"
                                                        "i2540"
                                                        "i2539"
                                                        "i2538"
                                                        "i2536"
                                                        "i2535"
                                                        "i2534"
                                                        "i2533"
                                                        "i2532"
                                                        "i2530"
                                                        "i2528"
                                                        "i2526"
                                                        "i2524"
                                                        "i2522"
                                                        "i2520"
                                                        "i2518"
                                                        "i2516"
                                                        "i2513"
                                                        "i2511"
                                                        "i2510"
                                                        "i2509"
                                                        "i2508"
                                                        "i2507"
                                                        "i2506"
                                                        "i2504"
                                                        "i2502"
                                                        "i2500"
                                                        "i2498"
                                                        "i2497"
                                                        "i2495"
                                                        "i2493"
                                                        "i2491"
                                                        "i2489"
                                                        "i2487"
                                                        "i2485"
                                                        "i2483"
                                                        "i2482"
                                                        "i2480"
                                                        "i2478"
                                                        "i2477"
                                                        "i2476"
                                                        "i2474"
                                                        "i2473"
                                                        "i2471"
                                                        "i2469"
                                                        "i2467"
                                                        "i2465"
                                                        "i2463"
                                                        "i2461"
                                                        "i2459"
                                                        "i2457"
                                                        "i2455"
                                                        "i2453"
                                                        "i2451"
                                                        "i2449"
                                                        "i2447"
                                                        "i2445"
                                                        "i2443"
                                                        "i2441"
                                                        "i2439"
                                                        "i2437"
                                                        "i2435"
                                                        "i2433"
                                                        "i2431"
                                                        "i2429"
                                                        "i2427"
                                                        "i2425"
                                                        "i2423"
                                                        "i2421"
                                                        "i2419"
                                                        "i2417"
                                                        "i2415"
                                                        "i2413"
                                                        "i2411"
                                                        "i2409"
                                                        "i2408"
                                                        "i2406"
                                                        "i2404"
                                                        "i2402"
                                                        "i2400"
                                                        "i2398"
                                                        "i2396"
                                                        "i2394"
                                                        "i2392"
                                                        "i2390"
                                                        "i2387"
                                                        "i2385"
                                                        "i2383"
                                                        "i2381"
                                                        "i2379"
                                                        "i2377"
                                                        "i2375"
                                                        "i2373"
                                                        "i2371"
                                                        "i2369"
                                                        "i2367"
                                                        "i2365"
                                                        "i2363"
                                                        "i2361"
                                                        "i2359"
                                                        "i2357"
                                                        "i2355"
                                                        "i2353"))
                                                     #(ribcage
                                                       (define-structure
                                                         define-expansion-accessors
                                                         define-expansion-constructors
                                                         and-map*)
                                                       ((top)
                                                        (top)
                                                        (top)
                                                        (top))
                                                       ("i2147"
                                                        "i2146"
                                                        "i2145"
                                                        "i2143")))
                                                    (hygiene guile))))
                                          #{ropt\ 4396}#)))
                                #{tmp\ 4402}#)
                              (let ((#{tmp\ 4411}#
                                      ($sc-dispatch
                                        #{tmp\ 4400}#
                                        '((any any) . any))))
                                (if (if #{tmp\ 4411}#
                                      (@apply
                                        (lambda (#{a\ 4415}#
                                                 #{init\ 4416}#
                                                 #{b\ 4417}#)
                                          (#{id?\ 2496}# #{a\ 4415}#))
                                        #{tmp\ 4411}#)
                                      #f)
                                  (@apply
                                    (lambda (#{a\ 4421}#
                                             #{init\ 4422}#
                                             #{b\ 4423}#)
                                      (#{opt\ 4338}#
                                        #{b\ 4423}#
                                        #{req\ 4395}#
                                        (cons (list #{a\ 4421}# #{init\ 4422}#)
                                              #{ropt\ 4396}#)))
                                    #{tmp\ 4411}#)
                                  (let ((#{tmp\ 4424}#
                                          ($sc-dispatch
                                            #{tmp\ 4400}#
                                            '(any . any))))
                                    (if (if #{tmp\ 4424}#
                                          (@apply
                                            (lambda (#{a\ 4427}# #{b\ 4428}#)
                                              (eq? (syntax->datum #{a\ 4427}#)
                                                   #:key))
                                            #{tmp\ 4424}#)
                                          #f)
                                      (@apply
                                        (lambda (#{a\ 4431}# #{b\ 4432}#)
                                          (#{key\ 4340}#
                                            #{b\ 4432}#
                                            #{req\ 4395}#
                                            (reverse #{ropt\ 4396}#)
                                            '()))
                                        #{tmp\ 4424}#)
                                      (let ((#{tmp\ 4433}#
                                              ($sc-dispatch
                                                #{tmp\ 4400}#
                                                '(any any))))
                                        (if (if #{tmp\ 4433}#
                                              (@apply
                                                (lambda (#{a\ 4436}#
                                                         #{b\ 4437}#)
                                                  (eq? (syntax->datum
                                                         #{a\ 4436}#)
                                                       #:rest))
                                                #{tmp\ 4433}#)
                                              #f)
                                          (@apply
                                            (lambda (#{a\ 4440}# #{b\ 4441}#)
                                              (#{rest\ 4342}#
                                                #{b\ 4441}#
                                                #{req\ 4395}#
                                                (reverse #{ropt\ 4396}#)
                                                '()))
                                            #{tmp\ 4433}#)
                                          (let ((#{tmp\ 4442}#
                                                  (list #{tmp\ 4400}#)))
                                            (if (if #{tmp\ 4442}#
                                                  (@apply
                                                    (lambda (#{r\ 4444}#)
                                                      (#{id?\ 2496}#
                                                        #{r\ 4444}#))
                                                    #{tmp\ 4442}#)
                                                  #f)
                                              (@apply
                                                (lambda (#{r\ 4446}#)
                                                  (#{rest\ 4342}#
                                                    #{r\ 4446}#
                                                    #{req\ 4395}#
                                                    (reverse #{ropt\ 4396}#)
                                                    '()))
                                                #{tmp\ 4442}#)
                                              (let ((#{else\ 4448}#
                                                      #{tmp\ 4400}#))
                                                (syntax-violation
                                                  'lambda*
                                                  "invalid optional argument list"
                                                  #{orig-args\ 4333}#
                                                  #{args\ 4394}#)))))))))))))))))
                (#{key\ 4340}#
                  (lambda (#{args\ 4449}#
                           #{req\ 4450}#
                           #{opt\ 4451}#
                           #{rkey\ 4452}#)
                    (let ((#{tmp\ 4457}# #{args\ 4449}#))
                      (let ((#{tmp\ 4458}#
                              ($sc-dispatch #{tmp\ 4457}# (quote ()))))
                        (if #{tmp\ 4458}#
                          (@apply
                            (lambda ()
                              (#{check\ 4344}#
                                #{req\ 4450}#
                                #{opt\ 4451}#
                                #f
                                (cons #f (reverse #{rkey\ 4452}#))))
                            #{tmp\ 4458}#)
                          (let ((#{tmp\ 4459}#
                                  ($sc-dispatch
                                    #{tmp\ 4457}#
                                    '(any . any))))
                            (if (if #{tmp\ 4459}#
                                  (@apply
                                    (lambda (#{a\ 4462}# #{b\ 4463}#)
                                      (#{id?\ 2496}# #{a\ 4462}#))
                                    #{tmp\ 4459}#)
                                  #f)
                              (@apply
                                (lambda (#{a\ 4466}# #{b\ 4467}#)
                                  (let ((#{tmp\ 4469}#
                                          (symbol->keyword
                                            (syntax->datum #{a\ 4466}#))))
                                    (let ((#{k\ 4471}# #{tmp\ 4469}#))
                                      (#{key\ 4340}#
                                        #{b\ 4467}#
                                        #{req\ 4450}#
                                        #{opt\ 4451}#
                                        (cons (cons #{k\ 4471}#
                                                    (cons #{a\ 4466}#
                                                          '(#(syntax-object
                                                              #f
                                                              ((top)
                                                               #(ribcage
                                                                 #(k)
                                                                 #((top))
                                                                 #("i4470"))
                                                               #(ribcage
                                                                 #(a b)
                                                                 #((top) (top))
                                                                 #("i4464"
                                                                   "i4465"))
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
                                                                 #("i4453"
                                                                   "i4454"
                                                                   "i4455"
                                                                   "i4456"))
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
                                                                 ("i4343"
                                                                  "i4341"
                                                                  "i4339"
                                                                  "i4337"
                                                                  "i4335"))
                                                               #(ribcage
                                                                 #(orig-args)
                                                                 #((top))
                                                                 #("i4334"))
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
                                                                   gensym-hook
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
                                                                  (top))
                                                                 ("i2610"
                                                                  "i2608"
                                                                  "i2606"
                                                                  "i2604"
                                                                  "i2602"
                                                                  "i2600"
                                                                  "i2598"
                                                                  "i2596"
                                                                  "i2594"
                                                                  "i2592"
                                                                  "i2590"
                                                                  "i2588"
                                                                  "i2586"
                                                                  "i2584"
                                                                  "i2582"
                                                                  "i2580"
                                                                  "i2578"
                                                                  "i2576"
                                                                  "i2574"
                                                                  "i2572"
                                                                  "i2570"
                                                                  "i2568"
                                                                  "i2566"
                                                                  "i2564"
                                                                  "i2562"
                                                                  "i2560"
                                                                  "i2558"
                                                                  "i2556"
                                                                  "i2554"
                                                                  "i2552"
                                                                  "i2550"
                                                                  "i2548"
                                                                  "i2546"
                                                                  "i2544"
                                                                  "i2542"
                                                                  "i2540"
                                                                  "i2539"
                                                                  "i2538"
                                                                  "i2536"
                                                                  "i2535"
                                                                  "i2534"
                                                                  "i2533"
                                                                  "i2532"
                                                                  "i2530"
                                                                  "i2528"
                                                                  "i2526"
                                                                  "i2524"
                                                                  "i2522"
                                                                  "i2520"
                                                                  "i2518"
                                                                  "i2516"
                                                                  "i2513"
                                                                  "i2511"
                                                                  "i2510"
                                                                  "i2509"
                                                                  "i2508"
                                                                  "i2507"
                                                                  "i2506"
                                                                  "i2504"
                                                                  "i2502"
                                                                  "i2500"
                                                                  "i2498"
                                                                  "i2497"
                                                                  "i2495"
                                                                  "i2493"
                                                                  "i2491"
                                                                  "i2489"
                                                                  "i2487"
                                                                  "i2485"
                                                                  "i2483"
                                                                  "i2482"
                                                                  "i2480"
                                                                  "i2478"
                                                                  "i2477"
                                                                  "i2476"
                                                                  "i2474"
                                                                  "i2473"
                                                                  "i2471"
                                                                  "i2469"
                                                                  "i2467"
                                                                  "i2465"
                                                                  "i2463"
                                                                  "i2461"
                                                                  "i2459"
                                                                  "i2457"
                                                                  "i2455"
                                                                  "i2453"
                                                                  "i2451"
                                                                  "i2449"
                                                                  "i2447"
                                                                  "i2445"
                                                                  "i2443"
                                                                  "i2441"
                                                                  "i2439"
                                                                  "i2437"
                                                                  "i2435"
                                                                  "i2433"
                                                                  "i2431"
                                                                  "i2429"
                                                                  "i2427"
                                                                  "i2425"
                                                                  "i2423"
                                                                  "i2421"
                                                                  "i2419"
                                                                  "i2417"
                                                                  "i2415"
                                                                  "i2413"
                                                                  "i2411"
                                                                  "i2409"
                                                                  "i2408"
                                                                  "i2406"
                                                                  "i2404"
                                                                  "i2402"
                                                                  "i2400"
                                                                  "i2398"
                                                                  "i2396"
                                                                  "i2394"
                                                                  "i2392"
                                                                  "i2390"
                                                                  "i2387"
                                                                  "i2385"
                                                                  "i2383"
                                                                  "i2381"
                                                                  "i2379"
                                                                  "i2377"
                                                                  "i2375"
                                                                  "i2373"
                                                                  "i2371"
                                                                  "i2369"
                                                                  "i2367"
                                                                  "i2365"
                                                                  "i2363"
                                                                  "i2361"
                                                                  "i2359"
                                                                  "i2357"
                                                                  "i2355"
                                                                  "i2353"))
                                                               #(ribcage
                                                                 (define-structure
                                                                   define-expansion-accessors
                                                                   define-expansion-constructors
                                                                   and-map*)
                                                                 ((top)
                                                                  (top)
                                                                  (top)
                                                                  (top))
                                                                 ("i2147"
                                                                  "i2146"
                                                                  "i2145"
                                                                  "i2143")))
                                                              (hygiene
                                                                guile)))))
                                              #{rkey\ 4452}#)))))
                                #{tmp\ 4459}#)
                              (let ((#{tmp\ 4472}#
                                      ($sc-dispatch
                                        #{tmp\ 4457}#
                                        '((any any) . any))))
                                (if (if #{tmp\ 4472}#
                                      (@apply
                                        (lambda (#{a\ 4476}#
                                                 #{init\ 4477}#
                                                 #{b\ 4478}#)
                                          (#{id?\ 2496}# #{a\ 4476}#))
                                        #{tmp\ 4472}#)
                                      #f)
                                  (@apply
                                    (lambda (#{a\ 4482}#
                                             #{init\ 4483}#
                                             #{b\ 4484}#)
                                      (let ((#{tmp\ 4486}#
                                              (symbol->keyword
                                                (syntax->datum #{a\ 4482}#))))
                                        (let ((#{k\ 4488}# #{tmp\ 4486}#))
                                          (#{key\ 4340}#
                                            #{b\ 4484}#
                                            #{req\ 4450}#
                                            #{opt\ 4451}#
                                            (cons (list #{k\ 4488}#
                                                        #{a\ 4482}#
                                                        #{init\ 4483}#)
                                                  #{rkey\ 4452}#)))))
                                    #{tmp\ 4472}#)
                                  (let ((#{tmp\ 4489}#
                                          ($sc-dispatch
                                            #{tmp\ 4457}#
                                            '((any any any) . any))))
                                    (if (if #{tmp\ 4489}#
                                          (@apply
                                            (lambda (#{a\ 4494}#
                                                     #{init\ 4495}#
                                                     #{k\ 4496}#
                                                     #{b\ 4497}#)
                                              (if (#{id?\ 2496}# #{a\ 4494}#)
                                                (keyword?
                                                  (syntax->datum #{k\ 4496}#))
                                                #f))
                                            #{tmp\ 4489}#)
                                          #f)
                                      (@apply
                                        (lambda (#{a\ 4504}#
                                                 #{init\ 4505}#
                                                 #{k\ 4506}#
                                                 #{b\ 4507}#)
                                          (#{key\ 4340}#
                                            #{b\ 4507}#
                                            #{req\ 4450}#
                                            #{opt\ 4451}#
                                            (cons (list #{k\ 4506}#
                                                        #{a\ 4504}#
                                                        #{init\ 4505}#)
                                                  #{rkey\ 4452}#)))
                                        #{tmp\ 4489}#)
                                      (let ((#{tmp\ 4508}#
                                              ($sc-dispatch
                                                #{tmp\ 4457}#
                                                '(any))))
                                        (if (if #{tmp\ 4508}#
                                              (@apply
                                                (lambda (#{aok\ 4510}#)
                                                  (eq? (syntax->datum
                                                         #{aok\ 4510}#)
                                                       #:allow-other-keys))
                                                #{tmp\ 4508}#)
                                              #f)
                                          (@apply
                                            (lambda (#{aok\ 4512}#)
                                              (#{check\ 4344}#
                                                #{req\ 4450}#
                                                #{opt\ 4451}#
                                                #f
                                                (cons #t
                                                      (reverse
                                                        #{rkey\ 4452}#))))
                                            #{tmp\ 4508}#)
                                          (let ((#{tmp\ 4513}#
                                                  ($sc-dispatch
                                                    #{tmp\ 4457}#
                                                    '(any any any))))
                                            (if (if #{tmp\ 4513}#
                                                  (@apply
                                                    (lambda (#{aok\ 4517}#
                                                             #{a\ 4518}#
                                                             #{b\ 4519}#)
                                                      (if (eq? (syntax->datum
                                                                 #{aok\ 4517}#)
                                                               #:allow-other-keys)
                                                        (eq? (syntax->datum
                                                               #{a\ 4518}#)
                                                             #:rest)
                                                        #f))
                                                    #{tmp\ 4513}#)
                                                  #f)
                                              (@apply
                                                (lambda (#{aok\ 4525}#
                                                         #{a\ 4526}#
                                                         #{b\ 4527}#)
                                                  (#{rest\ 4342}#
                                                    #{b\ 4527}#
                                                    #{req\ 4450}#
                                                    #{opt\ 4451}#
                                                    (cons #t
                                                          (reverse
                                                            #{rkey\ 4452}#))))
                                                #{tmp\ 4513}#)
                                              (let ((#{tmp\ 4528}#
                                                      ($sc-dispatch
                                                        #{tmp\ 4457}#
                                                        '(any . any))))
                                                (if (if #{tmp\ 4528}#
                                                      (@apply
                                                        (lambda (#{aok\ 4531}#
                                                                 #{r\ 4532}#)
                                                          (if (eq? (syntax->datum
                                                                     #{aok\ 4531}#)
                                                                   #:allow-other-keys)
                                                            (#{id?\ 2496}#
                                                              #{r\ 4532}#)
                                                            #f))
                                                        #{tmp\ 4528}#)
                                                      #f)
                                                  (@apply
                                                    (lambda (#{aok\ 4537}#
                                                             #{r\ 4538}#)
                                                      (#{rest\ 4342}#
                                                        #{r\ 4538}#
                                                        #{req\ 4450}#
                                                        #{opt\ 4451}#
                                                        (cons #t
                                                              (reverse
                                                                #{rkey\ 4452}#))))
                                                    #{tmp\ 4528}#)
                                                  (let ((#{tmp\ 4539}#
                                                          ($sc-dispatch
                                                            #{tmp\ 4457}#
                                                            '(any any))))
                                                    (if (if #{tmp\ 4539}#
                                                          (@apply
                                                            (lambda (#{a\ 4542}#
                                                                     #{b\ 4543}#)
                                                              (eq? (syntax->datum
                                                                     #{a\ 4542}#)
                                                                   #:rest))
                                                            #{tmp\ 4539}#)
                                                          #f)
                                                      (@apply
                                                        (lambda (#{a\ 4546}#
                                                                 #{b\ 4547}#)
                                                          (#{rest\ 4342}#
                                                            #{b\ 4547}#
                                                            #{req\ 4450}#
                                                            #{opt\ 4451}#
                                                            (cons #f
                                                                  (reverse
                                                                    #{rkey\ 4452}#))))
                                                        #{tmp\ 4539}#)
                                                      (let ((#{tmp\ 4548}#
                                                              (list #{tmp\ 4457}#)))
                                                        (if (if #{tmp\ 4548}#
                                                              (@apply
                                                                (lambda (#{r\ 4550}#)
                                                                  (#{id?\ 2496}#
                                                                    #{r\ 4550}#))
                                                                #{tmp\ 4548}#)
                                                              #f)
                                                          (@apply
                                                            (lambda (#{r\ 4552}#)
                                                              (#{rest\ 4342}#
                                                                #{r\ 4552}#
                                                                #{req\ 4450}#
                                                                #{opt\ 4451}#
                                                                (cons #f
                                                                      (reverse
                                                                        #{rkey\ 4452}#))))
                                                            #{tmp\ 4548}#)
                                                          (let ((#{else\ 4554}#
                                                                  #{tmp\ 4457}#))
                                                            (syntax-violation
                                                              'lambda*
                                                              "invalid keyword argument list"
                                                              #{orig-args\ 4333}#
                                                              #{args\ 4449}#)))))))))))))))))))))))
                (#{rest\ 4342}#
                  (lambda (#{args\ 4555}#
                           #{req\ 4556}#
                           #{opt\ 4557}#
                           #{kw\ 4558}#)
                    (let ((#{tmp\ 4563}# #{args\ 4555}#))
                      (let ((#{tmp\ 4564}# (list #{tmp\ 4563}#)))
                        (if (if #{tmp\ 4564}#
                              (@apply
                                (lambda (#{r\ 4566}#)
                                  (#{id?\ 2496}# #{r\ 4566}#))
                                #{tmp\ 4564}#)
                              #f)
                          (@apply
                            (lambda (#{r\ 4568}#)
                              (#{check\ 4344}#
                                #{req\ 4556}#
                                #{opt\ 4557}#
                                #{r\ 4568}#
                                #{kw\ 4558}#))
                            #{tmp\ 4564}#)
                          (let ((#{else\ 4570}# #{tmp\ 4563}#))
                            (syntax-violation
                              'lambda*
                              "invalid rest argument"
                              #{orig-args\ 4333}#
                              #{args\ 4555}#)))))))
                (#{check\ 4344}#
                  (lambda (#{req\ 4571}#
                           #{opt\ 4572}#
                           #{rest\ 4573}#
                           #{kw\ 4574}#)
                    (if (#{distinct-bound-ids?\ 2561}#
                          (append
                            #{req\ 4571}#
                            (map car #{opt\ 4572}#)
                            (if #{rest\ 4573}#
                              (list #{rest\ 4573}#)
                              '())
                            (if (pair? #{kw\ 4574}#)
                              (map cadr (cdr #{kw\ 4574}#))
                              '())))
                      (values
                        #{req\ 4571}#
                        #{opt\ 4572}#
                        #{rest\ 4573}#
                        #{kw\ 4574}#)
                      (syntax-violation
                        'lambda*
                        "duplicate identifier in argument list"
                        #{orig-args\ 4333}#)))))
               (begin
                 (#{req\ 4336}# #{orig-args\ 4333}# (quote ()))))))
         (#{chi-lambda-case\ 2605}#
           (lambda (#{e\ 4582}#
                    #{r\ 4583}#
                    #{w\ 4584}#
                    #{s\ 4585}#
                    #{mod\ 4586}#
                    #{get-formals\ 4587}#
                    #{clauses\ 4588}#)
             (letrec*
               ((#{expand-req\ 4597}#
                  (lambda (#{req\ 4604}#
                           #{opt\ 4605}#
                           #{rest\ 4606}#
                           #{kw\ 4607}#
                           #{body\ 4608}#)
                    (begin
                      (let ((#{vars\ 4616}#
                              (map #{gen-var\ 2609}# #{req\ 4604}#))
                            (#{labels\ 4617}#
                              (#{gen-labels\ 2514}# #{req\ 4604}#)))
                        (begin
                          (let ((#{r*\ 4620}#
                                  (#{extend-var-env\ 2486}#
                                    #{labels\ 4617}#
                                    #{vars\ 4616}#
                                    #{r\ 4583}#))
                                (#{w*\ 4621}#
                                  (#{make-binding-wrap\ 2543}#
                                    #{req\ 4604}#
                                    #{labels\ 4617}#
                                    #{w\ 4584}#)))
                            (#{expand-opt\ 4599}#
                              (map syntax->datum #{req\ 4604}#)
                              #{opt\ 4605}#
                              #{rest\ 4606}#
                              #{kw\ 4607}#
                              #{body\ 4608}#
                              (reverse #{vars\ 4616}#)
                              #{r*\ 4620}#
                              #{w*\ 4621}#
                              '()
                              '())))))))
                (#{expand-opt\ 4599}#
                  (lambda (#{req\ 4622}#
                           #{opt\ 4623}#
                           #{rest\ 4624}#
                           #{kw\ 4625}#
                           #{body\ 4626}#
                           #{vars\ 4627}#
                           #{r*\ 4628}#
                           #{w*\ 4629}#
                           #{out\ 4630}#
                           #{inits\ 4631}#)
                    (if (pair? #{opt\ 4623}#)
                      (let ((#{tmp\ 4644}# (car #{opt\ 4623}#)))
                        (let ((#{tmp\ 4645}#
                                ($sc-dispatch
                                  #{tmp\ 4644}#
                                  '(any any))))
                          (if #{tmp\ 4645}#
                            (@apply
                              (lambda (#{id\ 4648}# #{i\ 4649}#)
                                (begin
                                  (let ((#{v\ 4652}#
                                          (#{gen-var\ 2609}# #{id\ 4648}#)))
                                    (begin
                                      (let ((#{l\ 4654}#
                                              (#{gen-labels\ 2514}#
                                                (list #{v\ 4652}#))))
                                        (begin
                                          (let ((#{r**\ 4656}#
                                                  (#{extend-var-env\ 2486}#
                                                    #{l\ 4654}#
                                                    (list #{v\ 4652}#)
                                                    #{r*\ 4628}#)))
                                            (begin
                                              (let ((#{w**\ 4658}#
                                                      (#{make-binding-wrap\ 2543}#
                                                        (list #{id\ 4648}#)
                                                        #{l\ 4654}#
                                                        #{w*\ 4629}#)))
                                                (#{expand-opt\ 4599}#
                                                  #{req\ 4622}#
                                                  (cdr #{opt\ 4623}#)
                                                  #{rest\ 4624}#
                                                  #{kw\ 4625}#
                                                  #{body\ 4626}#
                                                  (cons #{v\ 4652}#
                                                        #{vars\ 4627}#)
                                                  #{r**\ 4656}#
                                                  #{w**\ 4658}#
                                                  (cons (syntax->datum
                                                          #{id\ 4648}#)
                                                        #{out\ 4630}#)
                                                  (cons (#{chi\ 2581}#
                                                          #{i\ 4649}#
                                                          #{r*\ 4628}#
                                                          #{w*\ 4629}#
                                                          #{mod\ 4586}#)
                                                        #{inits\ 4631}#)))))))))))
                              #{tmp\ 4645}#)
                            (syntax-violation
                              #f
                              "source expression failed to match any pattern"
                              #{tmp\ 4644}#))))
                      (if #{rest\ 4624}#
                        (begin
                          (let ((#{v\ 4663}#
                                  (#{gen-var\ 2609}# #{rest\ 4624}#)))
                            (begin
                              (let ((#{l\ 4665}#
                                      (#{gen-labels\ 2514}#
                                        (list #{v\ 4663}#))))
                                (begin
                                  (let ((#{r*\ 4667}#
                                          (#{extend-var-env\ 2486}#
                                            #{l\ 4665}#
                                            (list #{v\ 4663}#)
                                            #{r*\ 4628}#)))
                                    (begin
                                      (let ((#{w*\ 4669}#
                                              (#{make-binding-wrap\ 2543}#
                                                (list #{rest\ 4624}#)
                                                #{l\ 4665}#
                                                #{w*\ 4629}#)))
                                        (#{expand-kw\ 4601}#
                                          #{req\ 4622}#
                                          (if (pair? #{out\ 4630}#)
                                            (reverse #{out\ 4630}#)
                                            #f)
                                          (syntax->datum #{rest\ 4624}#)
                                          (if (pair? #{kw\ 4625}#)
                                            (cdr #{kw\ 4625}#)
                                            #{kw\ 4625}#)
                                          #{body\ 4626}#
                                          (cons #{v\ 4663}# #{vars\ 4627}#)
                                          #{r*\ 4667}#
                                          #{w*\ 4669}#
                                          (if (pair? #{kw\ 4625}#)
                                            (car #{kw\ 4625}#)
                                            #f)
                                          '()
                                          #{inits\ 4631}#)))))))))
                        (#{expand-kw\ 4601}#
                          #{req\ 4622}#
                          (if (pair? #{out\ 4630}#)
                            (reverse #{out\ 4630}#)
                            #f)
                          #f
                          (if (pair? #{kw\ 4625}#)
                            (cdr #{kw\ 4625}#)
                            #{kw\ 4625}#)
                          #{body\ 4626}#
                          #{vars\ 4627}#
                          #{r*\ 4628}#
                          #{w*\ 4629}#
                          (if (pair? #{kw\ 4625}#) (car #{kw\ 4625}#) #f)
                          '()
                          #{inits\ 4631}#)))))
                (#{expand-kw\ 4601}#
                  (lambda (#{req\ 4671}#
                           #{opt\ 4672}#
                           #{rest\ 4673}#
                           #{kw\ 4674}#
                           #{body\ 4675}#
                           #{vars\ 4676}#
                           #{r*\ 4677}#
                           #{w*\ 4678}#
                           #{aok\ 4679}#
                           #{out\ 4680}#
                           #{inits\ 4681}#)
                    (if (pair? #{kw\ 4674}#)
                      (let ((#{tmp\ 4695}# (car #{kw\ 4674}#)))
                        (let ((#{tmp\ 4696}#
                                ($sc-dispatch
                                  #{tmp\ 4695}#
                                  '(any any any))))
                          (if #{tmp\ 4696}#
                            (@apply
                              (lambda (#{k\ 4700}# #{id\ 4701}# #{i\ 4702}#)
                                (begin
                                  (let ((#{v\ 4705}#
                                          (#{gen-var\ 2609}# #{id\ 4701}#)))
                                    (begin
                                      (let ((#{l\ 4707}#
                                              (#{gen-labels\ 2514}#
                                                (list #{v\ 4705}#))))
                                        (begin
                                          (let ((#{r**\ 4709}#
                                                  (#{extend-var-env\ 2486}#
                                                    #{l\ 4707}#
                                                    (list #{v\ 4705}#)
                                                    #{r*\ 4677}#)))
                                            (begin
                                              (let ((#{w**\ 4711}#
                                                      (#{make-binding-wrap\ 2543}#
                                                        (list #{id\ 4701}#)
                                                        #{l\ 4707}#
                                                        #{w*\ 4678}#)))
                                                (#{expand-kw\ 4601}#
                                                  #{req\ 4671}#
                                                  #{opt\ 4672}#
                                                  #{rest\ 4673}#
                                                  (cdr #{kw\ 4674}#)
                                                  #{body\ 4675}#
                                                  (cons #{v\ 4705}#
                                                        #{vars\ 4676}#)
                                                  #{r**\ 4709}#
                                                  #{w**\ 4711}#
                                                  #{aok\ 4679}#
                                                  (cons (list (syntax->datum
                                                                #{k\ 4700}#)
                                                              (syntax->datum
                                                                #{id\ 4701}#)
                                                              #{v\ 4705}#)
                                                        #{out\ 4680}#)
                                                  (cons (#{chi\ 2581}#
                                                          #{i\ 4702}#
                                                          #{r*\ 4677}#
                                                          #{w*\ 4678}#
                                                          #{mod\ 4586}#)
                                                        #{inits\ 4681}#)))))))))))
                              #{tmp\ 4696}#)
                            (syntax-violation
                              #f
                              "source expression failed to match any pattern"
                              #{tmp\ 4695}#))))
                      (#{expand-body\ 4603}#
                        #{req\ 4671}#
                        #{opt\ 4672}#
                        #{rest\ 4673}#
                        (if (begin
                              (let ((#{t\ 4715}# #{aok\ 4679}#))
                                (if #{t\ 4715}#
                                  #{t\ 4715}#
                                  (pair? #{out\ 4680}#))))
                          (cons #{aok\ 4679}# (reverse #{out\ 4680}#))
                          #f)
                        #{body\ 4675}#
                        (reverse #{vars\ 4676}#)
                        #{r*\ 4677}#
                        #{w*\ 4678}#
                        (reverse #{inits\ 4681}#)
                        '()))))
                (#{expand-body\ 4603}#
                  (lambda (#{req\ 4717}#
                           #{opt\ 4718}#
                           #{rest\ 4719}#
                           #{kw\ 4720}#
                           #{body\ 4721}#
                           #{vars\ 4722}#
                           #{r*\ 4723}#
                           #{w*\ 4724}#
                           #{inits\ 4725}#
                           #{meta\ 4726}#)
                    (let ((#{tmp\ 4737}# #{body\ 4721}#))
                      (let ((#{tmp\ 4738}#
                              ($sc-dispatch
                                #{tmp\ 4737}#
                                '(any any . each-any))))
                        (if (if #{tmp\ 4738}#
                              (@apply
                                (lambda (#{docstring\ 4742}#
                                         #{e1\ 4743}#
                                         #{e2\ 4744}#)
                                  (string?
                                    (syntax->datum #{docstring\ 4742}#)))
                                #{tmp\ 4738}#)
                              #f)
                          (@apply
                            (lambda (#{docstring\ 4748}#
                                     #{e1\ 4749}#
                                     #{e2\ 4750}#)
                              (#{expand-body\ 4603}#
                                #{req\ 4717}#
                                #{opt\ 4718}#
                                #{rest\ 4719}#
                                #{kw\ 4720}#
                                (cons #{e1\ 4749}# #{e2\ 4750}#)
                                #{vars\ 4722}#
                                #{r*\ 4723}#
                                #{w*\ 4724}#
                                #{inits\ 4725}#
                                (append
                                  #{meta\ 4726}#
                                  (list (cons 'documentation
                                              (syntax->datum
                                                #{docstring\ 4748}#))))))
                            #{tmp\ 4738}#)
                          (let ((#{tmp\ 4753}#
                                  ($sc-dispatch
                                    #{tmp\ 4737}#
                                    '(#(vector #(each (any . any)))
                                      any
                                      .
                                      each-any))))
                            (if #{tmp\ 4753}#
                              (@apply
                                (lambda (#{k\ 4758}#
                                         #{v\ 4759}#
                                         #{e1\ 4760}#
                                         #{e2\ 4761}#)
                                  (#{expand-body\ 4603}#
                                    #{req\ 4717}#
                                    #{opt\ 4718}#
                                    #{rest\ 4719}#
                                    #{kw\ 4720}#
                                    (cons #{e1\ 4760}# #{e2\ 4761}#)
                                    #{vars\ 4722}#
                                    #{r*\ 4723}#
                                    #{w*\ 4724}#
                                    #{inits\ 4725}#
                                    (append
                                      #{meta\ 4726}#
                                      (syntax->datum
                                        (map cons #{k\ 4758}# #{v\ 4759}#)))))
                                #{tmp\ 4753}#)
                              (let ((#{tmp\ 4765}#
                                      ($sc-dispatch
                                        #{tmp\ 4737}#
                                        '(any . each-any))))
                                (if #{tmp\ 4765}#
                                  (@apply
                                    (lambda (#{e1\ 4768}# #{e2\ 4769}#)
                                      (values
                                        #{meta\ 4726}#
                                        #{req\ 4717}#
                                        #{opt\ 4718}#
                                        #{rest\ 4719}#
                                        #{kw\ 4720}#
                                        #{inits\ 4725}#
                                        #{vars\ 4722}#
                                        (#{chi-body\ 2589}#
                                          (cons #{e1\ 4768}# #{e2\ 4769}#)
                                          (#{source-wrap\ 2567}#
                                            #{e\ 4582}#
                                            #{w\ 4584}#
                                            #{s\ 4585}#
                                            #{mod\ 4586}#)
                                          #{r*\ 4723}#
                                          #{w*\ 4724}#
                                          #{mod\ 4586}#)))
                                    #{tmp\ 4765}#)
                                  (syntax-violation
                                    #f
                                    "source expression failed to match any pattern"
                                    #{tmp\ 4737}#)))))))))))
               (begin
                 (let ((#{tmp\ 4771}# #{clauses\ 4588}#))
                   (let ((#{tmp\ 4772}#
                           ($sc-dispatch #{tmp\ 4771}# (quote ()))))
                     (if #{tmp\ 4772}#
                       (@apply
                         (lambda () (values (quote ()) #f))
                         #{tmp\ 4772}#)
                       (let ((#{tmp\ 4773}#
                               ($sc-dispatch
                                 #{tmp\ 4771}#
                                 '((any any . each-any)
                                   .
                                   #(each (any any . each-any))))))
                         (if #{tmp\ 4773}#
                           (@apply
                             (lambda (#{args\ 4780}#
                                      #{e1\ 4781}#
                                      #{e2\ 4782}#
                                      #{args*\ 4783}#
                                      #{e1*\ 4784}#
                                      #{e2*\ 4785}#)
                               (call-with-values
                                 (lambda ()
                                   (#{get-formals\ 4587}# #{args\ 4780}#))
                                 (lambda (#{req\ 4786}#
                                          #{opt\ 4787}#
                                          #{rest\ 4788}#
                                          #{kw\ 4789}#)
                                   (call-with-values
                                     (lambda ()
                                       (#{expand-req\ 4597}#
                                         #{req\ 4786}#
                                         #{opt\ 4787}#
                                         #{rest\ 4788}#
                                         #{kw\ 4789}#
                                         (cons #{e1\ 4781}# #{e2\ 4782}#)))
                                     (lambda (#{meta\ 4795}#
                                              #{req\ 4796}#
                                              #{opt\ 4797}#
                                              #{rest\ 4798}#
                                              #{kw\ 4799}#
                                              #{inits\ 4800}#
                                              #{vars\ 4801}#
                                              #{body\ 4802}#)
                                       (call-with-values
                                         (lambda ()
                                           (#{chi-lambda-case\ 2605}#
                                             #{e\ 4582}#
                                             #{r\ 4583}#
                                             #{w\ 4584}#
                                             #{s\ 4585}#
                                             #{mod\ 4586}#
                                             #{get-formals\ 4587}#
                                             (map (lambda (#{tmp\ 4813}#
                                                           #{tmp\ 4812}#
                                                           #{tmp\ 4811}#)
                                                    (cons #{tmp\ 4811}#
                                                          (cons #{tmp\ 4812}#
                                                                #{tmp\ 4813}#)))
                                                  #{e2*\ 4785}#
                                                  #{e1*\ 4784}#
                                                  #{args*\ 4783}#)))
                                         (lambda (#{meta*\ 4815}#
                                                  #{else*\ 4816}#)
                                           (values
                                             (append
                                               #{meta\ 4795}#
                                               #{meta*\ 4815}#)
                                             (#{build-lambda-case\ 2442}#
                                               #{s\ 4585}#
                                               #{req\ 4796}#
                                               #{opt\ 4797}#
                                               #{rest\ 4798}#
                                               #{kw\ 4799}#
                                               #{inits\ 4800}#
                                               #{vars\ 4801}#
                                               #{body\ 4802}#
                                               #{else*\ 4816}#)))))))))
                             #{tmp\ 4773}#)
                           (syntax-violation
                             #f
                             "source expression failed to match any pattern"
                             #{tmp\ 4771}#))))))))))
         (#{strip\ 2607}#
           (lambda (#{x\ 4819}# #{w\ 4820}#)
             (if (memq 'top
                       (#{wrap-marks\ 2503}# #{w\ 4820}#))
               #{x\ 4819}#
               (letrec*
                 ((#{f\ 4826}#
                    (lambda (#{x\ 4827}#)
                      (if (#{syntax-object?\ 2460}# #{x\ 4827}#)
                        (#{strip\ 2607}#
                          (#{syntax-object-expression\ 2462}# #{x\ 4827}#)
                          (#{syntax-object-wrap\ 2464}# #{x\ 4827}#))
                        (if (pair? #{x\ 4827}#)
                          (begin
                            (let ((#{a\ 4834}# (#{f\ 4826}# (car #{x\ 4827}#)))
                                  (#{d\ 4835}#
                                    (#{f\ 4826}# (cdr #{x\ 4827}#))))
                              (if (if (eq? #{a\ 4834}# (car #{x\ 4827}#))
                                    (eq? #{d\ 4835}# (cdr #{x\ 4827}#))
                                    #f)
                                #{x\ 4827}#
                                (cons #{a\ 4834}# #{d\ 4835}#))))
                          (if (vector? #{x\ 4827}#)
                            (begin
                              (let ((#{old\ 4841}# (vector->list #{x\ 4827}#)))
                                (begin
                                  (let ((#{new\ 4843}#
                                          (map #{f\ 4826}# #{old\ 4841}#)))
                                    (if (#{and-map*\ 2144}#
                                          eq?
                                          #{old\ 4841}#
                                          #{new\ 4843}#)
                                      #{x\ 4827}#
                                      (list->vector #{new\ 4843}#))))))
                            #{x\ 4827}#))))))
                 (begin (#{f\ 4826}# #{x\ 4819}#))))))
         (#{gen-var\ 2609}#
           (lambda (#{id\ 4845}#)
             (begin
               (let ((#{id\ 4848}#
                       (if (#{syntax-object?\ 2460}# #{id\ 4845}#)
                         (#{syntax-object-expression\ 2462}# #{id\ 4845}#)
                         #{id\ 4845}#)))
                 (gensym
                   (string-append (symbol->string #{id\ 4848}#) " "))))))
         (#{lambda-var-list\ 2611}#
           (lambda (#{vars\ 4850}#)
             (letrec*
               ((#{lvl\ 4856}#
                  (lambda (#{vars\ 4857}# #{ls\ 4858}# #{w\ 4859}#)
                    (if (pair? #{vars\ 4857}#)
                      (#{lvl\ 4856}#
                        (cdr #{vars\ 4857}#)
                        (cons (#{wrap\ 2565}#
                                (car #{vars\ 4857}#)
                                #{w\ 4859}#
                                #f)
                              #{ls\ 4858}#)
                        #{w\ 4859}#)
                      (if (#{id?\ 2496}# #{vars\ 4857}#)
                        (cons (#{wrap\ 2565}# #{vars\ 4857}# #{w\ 4859}# #f)
                              #{ls\ 4858}#)
                        (if (null? #{vars\ 4857}#)
                          #{ls\ 4858}#
                          (if (#{syntax-object?\ 2460}# #{vars\ 4857}#)
                            (#{lvl\ 4856}#
                              (#{syntax-object-expression\ 2462}#
                                #{vars\ 4857}#)
                              #{ls\ 4858}#
                              (#{join-wraps\ 2547}#
                                #{w\ 4859}#
                                (#{syntax-object-wrap\ 2464}# #{vars\ 4857}#)))
                            (cons #{vars\ 4857}# #{ls\ 4858}#))))))))
               (begin
                 (#{lvl\ 4856}#
                   #{vars\ 4850}#
                   '()
                   '(())))))))
        (begin
          (set! #{make-primitive-ref\ 2358}#
            (lambda (#{src\ 2832}# #{name\ 2833}#)
              (make-struct/no-tail
                (vector-ref %expanded-vtables 2)
                #{src\ 2832}#
                #{name\ 2833}#)))
          (set! #{fx+\ 2397}# +)
          (set! #{fx-\ 2399}# -)
          (set! #{fx=\ 2401}# =)
          (set! #{fx<\ 2403}# <)
          (set! #{set-syntax-object-expression!\ 2468}#
            (lambda (#{x\ 3210}# #{update\ 3211}#)
              (vector-set! #{x\ 3210}# 1 #{update\ 3211}#)))
          (set! #{set-syntax-object-wrap!\ 2470}#
            (lambda (#{x\ 3214}# #{update\ 3215}#)
              (vector-set! #{x\ 3214}# 2 #{update\ 3215}#)))
          (set! #{set-syntax-object-module!\ 2472}#
            (lambda (#{x\ 3218}# #{update\ 3219}#)
              (vector-set! #{x\ 3218}# 3 #{update\ 3219}#)))
          (set! #{binding-type\ 2479}# car)
          (set! #{binding-value\ 2481}# cdr)
          (set! #{make-wrap\ 2501}# cons)
          (set! #{wrap-marks\ 2503}# car)
          (set! #{wrap-subst\ 2505}# cdr)
          (set! #{ribcage?\ 2519}#
            (lambda (#{x\ 3296}#)
              (if (vector? #{x\ 3296}#)
                (if (= (vector-length #{x\ 3296}#) 4)
                  (eq? (vector-ref #{x\ 3296}# 0) (quote ribcage))
                  #f)
                #f)))
          (begin
            (#{global-extend\ 2492}#
              'local-syntax
              'letrec-syntax
              #t)
            (#{global-extend\ 2492}#
              'local-syntax
              'let-syntax
              #f)
            (#{global-extend\ 2492}#
              'core
              'fluid-let-syntax
              (lambda (#{e\ 4870}#
                       #{r\ 4871}#
                       #{w\ 4872}#
                       #{s\ 4873}#
                       #{mod\ 4874}#)
                (let ((#{tmp\ 4880}# #{e\ 4870}#))
                  (let ((#{tmp\ 4881}#
                          ($sc-dispatch
                            #{tmp\ 4880}#
                            '(any #(each (any any)) any . each-any))))
                    (if (if #{tmp\ 4881}#
                          (@apply
                            (lambda (#{_\ 4887}#
                                     #{var\ 4888}#
                                     #{val\ 4889}#
                                     #{e1\ 4890}#
                                     #{e2\ 4891}#)
                              (#{valid-bound-ids?\ 2559}# #{var\ 4888}#))
                            #{tmp\ 4881}#)
                          #f)
                      (@apply
                        (lambda (#{_\ 4898}#
                                 #{var\ 4899}#
                                 #{val\ 4900}#
                                 #{e1\ 4901}#
                                 #{e2\ 4902}#)
                          (begin
                            (let ((#{names\ 4904}#
                                    (map (lambda (#{x\ 4905}#)
                                           (#{id-var-name\ 2553}#
                                             #{x\ 4905}#
                                             #{w\ 4872}#))
                                         #{var\ 4899}#)))
                              (begin
                                (for-each
                                  (lambda (#{id\ 4908}# #{n\ 4909}#)
                                    (begin
                                      (let ((#{atom-key\ 4914}#
                                              (#{binding-type\ 2479}#
                                                (#{lookup\ 2490}#
                                                  #{n\ 4909}#
                                                  #{r\ 4871}#
                                                  #{mod\ 4874}#))))
                                        (if (eqv? #{atom-key\ 4914}#
                                                  'displaced-lexical)
                                          (syntax-violation
                                            'fluid-let-syntax
                                            "identifier out of context"
                                            #{e\ 4870}#
                                            (#{source-wrap\ 2567}#
                                              #{id\ 4908}#
                                              #{w\ 4872}#
                                              #{s\ 4873}#
                                              #{mod\ 4874}#))))))
                                  #{var\ 4899}#
                                  #{names\ 4904}#)
                                (#{chi-body\ 2589}#
                                  (cons #{e1\ 4901}# #{e2\ 4902}#)
                                  (#{source-wrap\ 2567}#
                                    #{e\ 4870}#
                                    #{w\ 4872}#
                                    #{s\ 4873}#
                                    #{mod\ 4874}#)
                                  (#{extend-env\ 2484}#
                                    #{names\ 4904}#
                                    (begin
                                      (let ((#{trans-r\ 4919}#
                                              (#{macros-only-env\ 2488}#
                                                #{r\ 4871}#)))
                                        (map (lambda (#{x\ 4920}#)
                                               (cons 'macro
                                                     (#{eval-local-transformer\ 2593}#
                                                       (#{chi\ 2581}#
                                                         #{x\ 4920}#
                                                         #{trans-r\ 4919}#
                                                         #{w\ 4872}#
                                                         #{mod\ 4874}#)
                                                       #{mod\ 4874}#)))
                                             #{val\ 4900}#)))
                                    #{r\ 4871}#)
                                  #{w\ 4872}#
                                  #{mod\ 4874}#)))))
                        #{tmp\ 4881}#)
                      (let ((#{_\ 4925}# #{tmp\ 4880}#))
                        (syntax-violation
                          'fluid-let-syntax
                          "bad syntax"
                          (#{source-wrap\ 2567}#
                            #{e\ 4870}#
                            #{w\ 4872}#
                            #{s\ 4873}#
                            #{mod\ 4874}#))))))))
            (#{global-extend\ 2492}#
              'core
              'quote
              (lambda (#{e\ 4926}#
                       #{r\ 4927}#
                       #{w\ 4928}#
                       #{s\ 4929}#
                       #{mod\ 4930}#)
                (let ((#{tmp\ 4936}# #{e\ 4926}#))
                  (let ((#{tmp\ 4937}#
                          ($sc-dispatch #{tmp\ 4936}# (quote (any any)))))
                    (if #{tmp\ 4937}#
                      (@apply
                        (lambda (#{_\ 4940}# #{e\ 4941}#)
                          (#{build-data\ 2446}#
                            #{s\ 4929}#
                            (#{strip\ 2607}# #{e\ 4941}# #{w\ 4928}#)))
                        #{tmp\ 4937}#)
                      (let ((#{_\ 4943}# #{tmp\ 4936}#))
                        (syntax-violation
                          'quote
                          "bad syntax"
                          (#{source-wrap\ 2567}#
                            #{e\ 4926}#
                            #{w\ 4928}#
                            #{s\ 4929}#
                            #{mod\ 4930}#))))))))
            (#{global-extend\ 2492}#
              'core
              'syntax
              (letrec*
                ((#{gen-syntax\ 4945}#
                   (lambda (#{src\ 4960}#
                            #{e\ 4961}#
                            #{r\ 4962}#
                            #{maps\ 4963}#
                            #{ellipsis?\ 4964}#
                            #{mod\ 4965}#)
                     (if (#{id?\ 2496}# #{e\ 4961}#)
                       (begin
                         (let ((#{label\ 4973}#
                                 (#{id-var-name\ 2553}#
                                   #{e\ 4961}#
                                   '(()))))
                           (begin
                             (let ((#{b\ 4976}#
                                     (#{lookup\ 2490}#
                                       #{label\ 4973}#
                                       #{r\ 4962}#
                                       #{mod\ 4965}#)))
                               (if (eq? (#{binding-type\ 2479}# #{b\ 4976}#)
                                        'syntax)
                                 (call-with-values
                                   (lambda ()
                                     (begin
                                       (let ((#{var.lev\ 4978}#
                                               (#{binding-value\ 2481}#
                                                 #{b\ 4976}#)))
                                         (#{gen-ref\ 4947}#
                                           #{src\ 4960}#
                                           (car #{var.lev\ 4978}#)
                                           (cdr #{var.lev\ 4978}#)
                                           #{maps\ 4963}#))))
                                   (lambda (#{var\ 4979}# #{maps\ 4980}#)
                                     (values
                                       (list (quote ref) #{var\ 4979}#)
                                       #{maps\ 4980}#)))
                                 (if (#{ellipsis?\ 4964}# #{e\ 4961}#)
                                   (syntax-violation
                                     'syntax
                                     "misplaced ellipsis"
                                     #{src\ 4960}#)
                                   (values
                                     (list (quote quote) #{e\ 4961}#)
                                     #{maps\ 4963}#)))))))
                       (let ((#{tmp\ 4985}# #{e\ 4961}#))
                         (let ((#{tmp\ 4986}#
                                 ($sc-dispatch
                                   #{tmp\ 4985}#
                                   '(any any))))
                           (if (if #{tmp\ 4986}#
                                 (@apply
                                   (lambda (#{dots\ 4989}# #{e\ 4990}#)
                                     (#{ellipsis?\ 4964}# #{dots\ 4989}#))
                                   #{tmp\ 4986}#)
                                 #f)
                             (@apply
                               (lambda (#{dots\ 4993}# #{e\ 4994}#)
                                 (#{gen-syntax\ 4945}#
                                   #{src\ 4960}#
                                   #{e\ 4994}#
                                   #{r\ 4962}#
                                   #{maps\ 4963}#
                                   (lambda (#{x\ 4995}#) #f)
                                   #{mod\ 4965}#))
                               #{tmp\ 4986}#)
                             (let ((#{tmp\ 4997}#
                                     ($sc-dispatch
                                       #{tmp\ 4985}#
                                       '(any any . any))))
                               (if (if #{tmp\ 4997}#
                                     (@apply
                                       (lambda (#{x\ 5001}#
                                                #{dots\ 5002}#
                                                #{y\ 5003}#)
                                         (#{ellipsis?\ 4964}# #{dots\ 5002}#))
                                       #{tmp\ 4997}#)
                                     #f)
                                 (@apply
                                   (lambda (#{x\ 5007}#
                                            #{dots\ 5008}#
                                            #{y\ 5009}#)
                                     (letrec*
                                       ((#{f\ 5013}#
                                          (lambda (#{y\ 5014}# #{k\ 5015}#)
                                            (let ((#{tmp\ 5022}# #{y\ 5014}#))
                                              (let ((#{tmp\ 5023}#
                                                      ($sc-dispatch
                                                        #{tmp\ 5022}#
                                                        '(any . any))))
                                                (if (if #{tmp\ 5023}#
                                                      (@apply
                                                        (lambda (#{dots\ 5026}#
                                                                 #{y\ 5027}#)
                                                          (#{ellipsis?\ 4964}#
                                                            #{dots\ 5026}#))
                                                        #{tmp\ 5023}#)
                                                      #f)
                                                  (@apply
                                                    (lambda (#{dots\ 5030}#
                                                             #{y\ 5031}#)
                                                      (#{f\ 5013}#
                                                        #{y\ 5031}#
                                                        (lambda (#{maps\ 5032}#)
                                                          (call-with-values
                                                            (lambda ()
                                                              (#{k\ 5015}#
                                                                (cons '()
                                                                      #{maps\ 5032}#)))
                                                            (lambda (#{x\ 5034}#
                                                                     #{maps\ 5035}#)
                                                              (if (null? (car #{maps\ 5035}#))
                                                                (syntax-violation
                                                                  'syntax
                                                                  "extra ellipsis"
                                                                  #{src\ 4960}#)
                                                                (values
                                                                  (#{gen-mappend\ 4949}#
                                                                    #{x\ 5034}#
                                                                    (car #{maps\ 5035}#))
                                                                  (cdr #{maps\ 5035}#))))))))
                                                    #{tmp\ 5023}#)
                                                  (let ((#{_\ 5039}#
                                                          #{tmp\ 5022}#))
                                                    (call-with-values
                                                      (lambda ()
                                                        (#{gen-syntax\ 4945}#
                                                          #{src\ 4960}#
                                                          #{y\ 5014}#
                                                          #{r\ 4962}#
                                                          #{maps\ 4963}#
                                                          #{ellipsis?\ 4964}#
                                                          #{mod\ 4965}#))
                                                      (lambda (#{y\ 5040}#
                                                               #{maps\ 5041}#)
                                                        (call-with-values
                                                          (lambda ()
                                                            (#{k\ 5015}#
                                                              #{maps\ 5041}#))
                                                          (lambda (#{x\ 5044}#
                                                                   #{maps\ 5045}#)
                                                            (values
                                                              (#{gen-append\ 4955}#
                                                                #{x\ 5044}#
                                                                #{y\ 5040}#)
                                                              #{maps\ 5045}#))))))))))))
                                       (begin
                                         (#{f\ 5013}#
                                           #{y\ 5009}#
                                           (lambda (#{maps\ 5016}#)
                                             (call-with-values
                                               (lambda ()
                                                 (#{gen-syntax\ 4945}#
                                                   #{src\ 4960}#
                                                   #{x\ 5007}#
                                                   #{r\ 4962}#
                                                   (cons '()
                                                         #{maps\ 5016}#)
                                                   #{ellipsis?\ 4964}#
                                                   #{mod\ 4965}#))
                                               (lambda (#{x\ 5018}#
                                                        #{maps\ 5019}#)
                                                 (if (null? (car #{maps\ 5019}#))
                                                   (syntax-violation
                                                     'syntax
                                                     "extra ellipsis"
                                                     #{src\ 4960}#)
                                                   (values
                                                     (#{gen-map\ 4951}#
                                                       #{x\ 5018}#
                                                       (car #{maps\ 5019}#))
                                                     (cdr #{maps\ 5019}#))))))))))
                                   #{tmp\ 4997}#)
                                 (let ((#{tmp\ 5048}#
                                         ($sc-dispatch
                                           #{tmp\ 4985}#
                                           '(any . any))))
                                   (if #{tmp\ 5048}#
                                     (@apply
                                       (lambda (#{x\ 5051}# #{y\ 5052}#)
                                         (call-with-values
                                           (lambda ()
                                             (#{gen-syntax\ 4945}#
                                               #{src\ 4960}#
                                               #{x\ 5051}#
                                               #{r\ 4962}#
                                               #{maps\ 4963}#
                                               #{ellipsis?\ 4964}#
                                               #{mod\ 4965}#))
                                           (lambda (#{x\ 5053}# #{maps\ 5054}#)
                                             (call-with-values
                                               (lambda ()
                                                 (#{gen-syntax\ 4945}#
                                                   #{src\ 4960}#
                                                   #{y\ 5052}#
                                                   #{r\ 4962}#
                                                   #{maps\ 5054}#
                                                   #{ellipsis?\ 4964}#
                                                   #{mod\ 4965}#))
                                               (lambda (#{y\ 5057}#
                                                        #{maps\ 5058}#)
                                                 (values
                                                   (#{gen-cons\ 4953}#
                                                     #{x\ 5053}#
                                                     #{y\ 5057}#)
                                                   #{maps\ 5058}#))))))
                                       #{tmp\ 5048}#)
                                     (let ((#{tmp\ 5061}#
                                             ($sc-dispatch
                                               #{tmp\ 4985}#
                                               '#(vector (any . each-any)))))
                                       (if #{tmp\ 5061}#
                                         (@apply
                                           (lambda (#{e1\ 5064}# #{e2\ 5065}#)
                                             (call-with-values
                                               (lambda ()
                                                 (#{gen-syntax\ 4945}#
                                                   #{src\ 4960}#
                                                   (cons #{e1\ 5064}#
                                                         #{e2\ 5065}#)
                                                   #{r\ 4962}#
                                                   #{maps\ 4963}#
                                                   #{ellipsis?\ 4964}#
                                                   #{mod\ 4965}#))
                                               (lambda (#{e\ 5067}#
                                                        #{maps\ 5068}#)
                                                 (values
                                                   (#{gen-vector\ 4957}#
                                                     #{e\ 5067}#)
                                                   #{maps\ 5068}#))))
                                           #{tmp\ 5061}#)
                                         (let ((#{_\ 5072}# #{tmp\ 4985}#))
                                           (values
                                             (list (quote quote) #{e\ 4961}#)
                                             #{maps\ 4963}#))))))))))))))
                 (#{gen-ref\ 4947}#
                   (lambda (#{src\ 5074}#
                            #{var\ 5075}#
                            #{level\ 5076}#
                            #{maps\ 5077}#)
                     (if (#{fx=\ 2401}# #{level\ 5076}# 0)
                       (values #{var\ 5075}# #{maps\ 5077}#)
                       (if (null? #{maps\ 5077}#)
                         (syntax-violation
                           'syntax
                           "missing ellipsis"
                           #{src\ 5074}#)
                         (call-with-values
                           (lambda ()
                             (#{gen-ref\ 4947}#
                               #{src\ 5074}#
                               #{var\ 5075}#
                               (#{fx-\ 2399}# #{level\ 5076}# 1)
                               (cdr #{maps\ 5077}#)))
                           (lambda (#{outer-var\ 5082}# #{outer-maps\ 5083}#)
                             (begin
                               (let ((#{b\ 5087}#
                                       (assq #{outer-var\ 5082}#
                                             (car #{maps\ 5077}#))))
                                 (if #{b\ 5087}#
                                   (values (cdr #{b\ 5087}#) #{maps\ 5077}#)
                                   (begin
                                     (let ((#{inner-var\ 5089}#
                                             (#{gen-var\ 2609}# (quote tmp))))
                                       (values
                                         #{inner-var\ 5089}#
                                         (cons (cons (cons #{outer-var\ 5082}#
                                                           #{inner-var\ 5089}#)
                                                     (car #{maps\ 5077}#))
                                               #{outer-maps\ 5083}#)))))))))))))
                 (#{gen-mappend\ 4949}#
                   (lambda (#{e\ 5090}# #{map-env\ 5091}#)
                     (list 'apply
                           '(primitive append)
                           (#{gen-map\ 4951}# #{e\ 5090}# #{map-env\ 5091}#))))
                 (#{gen-map\ 4951}#
                   (lambda (#{e\ 5095}# #{map-env\ 5096}#)
                     (begin
                       (let ((#{formals\ 5101}# (map cdr #{map-env\ 5096}#))
                             (#{actuals\ 5102}#
                               (map (lambda (#{x\ 5103}#)
                                      (list (quote ref) (car #{x\ 5103}#)))
                                    #{map-env\ 5096}#)))
                         (if (eq? (car #{e\ 5095}#) (quote ref))
                           (car #{actuals\ 5102}#)
                           (if (and-map
                                 (lambda (#{x\ 5110}#)
                                   (if (eq? (car #{x\ 5110}#) (quote ref))
                                     (memq (car (cdr #{x\ 5110}#))
                                           #{formals\ 5101}#)
                                     #f))
                                 (cdr #{e\ 5095}#))
                             (cons 'map
                                   (cons (list 'primitive
                                               (car #{e\ 5095}#))
                                         (map (begin
                                                (let ((#{r\ 5116}#
                                                        (map cons
                                                             #{formals\ 5101}#
                                                             #{actuals\ 5102}#)))
                                                  (lambda (#{x\ 5117}#)
                                                    (cdr (assq (car (cdr #{x\ 5117}#))
                                                               #{r\ 5116}#)))))
                                              (cdr #{e\ 5095}#))))
                             (cons 'map
                                   (cons (list 'lambda
                                               #{formals\ 5101}#
                                               #{e\ 5095}#)
                                         #{actuals\ 5102}#))))))))
                 (#{gen-cons\ 4953}#
                   (lambda (#{x\ 5121}# #{y\ 5122}#)
                     (begin
                       (let ((#{atom-key\ 5127}# (car #{y\ 5122}#)))
                         (if (eqv? #{atom-key\ 5127}# (quote quote))
                           (if (eq? (car #{x\ 5121}#) (quote quote))
                             (list 'quote
                                   (cons (car (cdr #{x\ 5121}#))
                                         (car (cdr #{y\ 5122}#))))
                             (if (eq? (car (cdr #{y\ 5122}#)) (quote ()))
                               (list (quote list) #{x\ 5121}#)
                               (list (quote cons) #{x\ 5121}# #{y\ 5122}#)))
                           (if (eqv? #{atom-key\ 5127}# (quote list))
                             (cons 'list
                                   (cons #{x\ 5121}# (cdr #{y\ 5122}#)))
                             (list (quote cons) #{x\ 5121}# #{y\ 5122}#)))))))
                 (#{gen-append\ 4955}#
                   (lambda (#{x\ 5136}# #{y\ 5137}#)
                     (if (equal? #{y\ 5137}# (quote (quote ())))
                       #{x\ 5136}#
                       (list (quote append) #{x\ 5136}# #{y\ 5137}#))))
                 (#{gen-vector\ 4957}#
                   (lambda (#{x\ 5141}#)
                     (if (eq? (car #{x\ 5141}#) (quote list))
                       (cons (quote vector) (cdr #{x\ 5141}#))
                       (if (eq? (car #{x\ 5141}#) (quote quote))
                         (list 'quote
                               (list->vector (car (cdr #{x\ 5141}#))))
                         (list (quote list->vector) #{x\ 5141}#)))))
                 (#{regen\ 4959}#
                   (lambda (#{x\ 5151}#)
                     (begin
                       (let ((#{atom-key\ 5155}# (car #{x\ 5151}#)))
                         (if (eqv? #{atom-key\ 5155}# (quote ref))
                           (#{build-lexical-reference\ 2426}#
                             'value
                             #f
                             (car (cdr #{x\ 5151}#))
                             (car (cdr #{x\ 5151}#)))
                           (if (eqv? #{atom-key\ 5155}# (quote primitive))
                             (#{build-primref\ 2444}#
                               #f
                               (car (cdr #{x\ 5151}#)))
                             (if (eqv? #{atom-key\ 5155}# (quote quote))
                               (#{build-data\ 2446}#
                                 #f
                                 (car (cdr #{x\ 5151}#)))
                               (if (eqv? #{atom-key\ 5155}# (quote lambda))
                                 (if (list? (car (cdr #{x\ 5151}#)))
                                   (#{build-simple-lambda\ 2438}#
                                     #f
                                     (car (cdr #{x\ 5151}#))
                                     #f
                                     (car (cdr #{x\ 5151}#))
                                     '()
                                     (#{regen\ 4959}#
                                       (car (cdr (cdr #{x\ 5151}#)))))
                                   (error "how did we get here" #{x\ 5151}#))
                                 (#{build-application\ 2420}#
                                   #f
                                   (#{build-primref\ 2444}#
                                     #f
                                     (car #{x\ 5151}#))
                                   (map #{regen\ 4959}#
                                        (cdr #{x\ 5151}#))))))))))))
                (begin
                  (lambda (#{e\ 5167}#
                           #{r\ 5168}#
                           #{w\ 5169}#
                           #{s\ 5170}#
                           #{mod\ 5171}#)
                    (begin
                      (let ((#{e\ 5178}#
                              (#{source-wrap\ 2567}#
                                #{e\ 5167}#
                                #{w\ 5169}#
                                #{s\ 5170}#
                                #{mod\ 5171}#)))
                        (let ((#{tmp\ 5179}# #{e\ 5178}#))
                          (let ((#{tmp\ 5180}#
                                  ($sc-dispatch
                                    #{tmp\ 5179}#
                                    '(any any))))
                            (if #{tmp\ 5180}#
                              (@apply
                                (lambda (#{_\ 5183}# #{x\ 5184}#)
                                  (call-with-values
                                    (lambda ()
                                      (#{gen-syntax\ 4945}#
                                        #{e\ 5178}#
                                        #{x\ 5184}#
                                        #{r\ 5168}#
                                        '()
                                        #{ellipsis?\ 2597}#
                                        #{mod\ 5171}#))
                                    (lambda (#{e\ 5185}# #{maps\ 5186}#)
                                      (#{regen\ 4959}# #{e\ 5185}#))))
                                #{tmp\ 5180}#)
                              (let ((#{_\ 5190}# #{tmp\ 5179}#))
                                (syntax-violation
                                  'syntax
                                  "bad `syntax' form"
                                  #{e\ 5178}#)))))))))))
            (#{global-extend\ 2492}#
              'core
              'lambda
              (lambda (#{e\ 5191}#
                       #{r\ 5192}#
                       #{w\ 5193}#
                       #{s\ 5194}#
                       #{mod\ 5195}#)
                (let ((#{tmp\ 5201}# #{e\ 5191}#))
                  (let ((#{tmp\ 5202}#
                          ($sc-dispatch
                            #{tmp\ 5201}#
                            '(any any any . each-any))))
                    (if #{tmp\ 5202}#
                      (@apply
                        (lambda (#{_\ 5207}#
                                 #{args\ 5208}#
                                 #{e1\ 5209}#
                                 #{e2\ 5210}#)
                          (call-with-values
                            (lambda ()
                              (#{lambda-formals\ 2599}# #{args\ 5208}#))
                            (lambda (#{req\ 5211}#
                                     #{opt\ 5212}#
                                     #{rest\ 5213}#
                                     #{kw\ 5214}#)
                              (letrec*
                                ((#{lp\ 5222}#
                                   (lambda (#{body\ 5223}# #{meta\ 5224}#)
                                     (let ((#{tmp\ 5226}# #{body\ 5223}#))
                                       (let ((#{tmp\ 5227}#
                                               ($sc-dispatch
                                                 #{tmp\ 5226}#
                                                 '(any any . each-any))))
                                         (if (if #{tmp\ 5227}#
                                               (@apply
                                                 (lambda (#{docstring\ 5231}#
                                                          #{e1\ 5232}#
                                                          #{e2\ 5233}#)
                                                   (string?
                                                     (syntax->datum
                                                       #{docstring\ 5231}#)))
                                                 #{tmp\ 5227}#)
                                               #f)
                                           (@apply
                                             (lambda (#{docstring\ 5237}#
                                                      #{e1\ 5238}#
                                                      #{e2\ 5239}#)
                                               (#{lp\ 5222}#
                                                 (cons #{e1\ 5238}#
                                                       #{e2\ 5239}#)
                                                 (append
                                                   #{meta\ 5224}#
                                                   (list (cons 'documentation
                                                               (syntax->datum
                                                                 #{docstring\ 5237}#))))))
                                             #{tmp\ 5227}#)
                                           (let ((#{tmp\ 5242}#
                                                   ($sc-dispatch
                                                     #{tmp\ 5226}#
                                                     '(#(vector
                                                         #(each (any . any)))
                                                       any
                                                       .
                                                       each-any))))
                                             (if #{tmp\ 5242}#
                                               (@apply
                                                 (lambda (#{k\ 5247}#
                                                          #{v\ 5248}#
                                                          #{e1\ 5249}#
                                                          #{e2\ 5250}#)
                                                   (#{lp\ 5222}#
                                                     (cons #{e1\ 5249}#
                                                           #{e2\ 5250}#)
                                                     (append
                                                       #{meta\ 5224}#
                                                       (syntax->datum
                                                         (map cons
                                                              #{k\ 5247}#
                                                              #{v\ 5248}#)))))
                                                 #{tmp\ 5242}#)
                                               (let ((#{_\ 5255}#
                                                       #{tmp\ 5226}#))
                                                 (#{chi-simple-lambda\ 2601}#
                                                   #{e\ 5191}#
                                                   #{r\ 5192}#
                                                   #{w\ 5193}#
                                                   #{s\ 5194}#
                                                   #{mod\ 5195}#
                                                   #{req\ 5211}#
                                                   #{rest\ 5213}#
                                                   #{meta\ 5224}#
                                                   #{body\ 5223}#))))))))))
                                (begin
                                  (#{lp\ 5222}#
                                    (cons #{e1\ 5209}# #{e2\ 5210}#)
                                    '()))))))
                        #{tmp\ 5202}#)
                      (let ((#{_\ 5257}# #{tmp\ 5201}#))
                        (syntax-violation
                          'lambda
                          "bad lambda"
                          #{e\ 5191}#)))))))
            (#{global-extend\ 2492}#
              'core
              'lambda*
              (lambda (#{e\ 5258}#
                       #{r\ 5259}#
                       #{w\ 5260}#
                       #{s\ 5261}#
                       #{mod\ 5262}#)
                (let ((#{tmp\ 5268}# #{e\ 5258}#))
                  (let ((#{tmp\ 5269}#
                          ($sc-dispatch
                            #{tmp\ 5268}#
                            '(any any any . each-any))))
                    (if #{tmp\ 5269}#
                      (@apply
                        (lambda (#{_\ 5274}#
                                 #{args\ 5275}#
                                 #{e1\ 5276}#
                                 #{e2\ 5277}#)
                          (call-with-values
                            (lambda ()
                              (#{chi-lambda-case\ 2605}#
                                #{e\ 5258}#
                                #{r\ 5259}#
                                #{w\ 5260}#
                                #{s\ 5261}#
                                #{mod\ 5262}#
                                #{lambda*-formals\ 2603}#
                                (list (cons #{args\ 5275}#
                                            (cons #{e1\ 5276}#
                                                  #{e2\ 5277}#)))))
                            (lambda (#{meta\ 5279}# #{lcase\ 5280}#)
                              (#{build-case-lambda\ 2440}#
                                #{s\ 5261}#
                                #{meta\ 5279}#
                                #{lcase\ 5280}#))))
                        #{tmp\ 5269}#)
                      (let ((#{_\ 5284}# #{tmp\ 5268}#))
                        (syntax-violation
                          'lambda
                          "bad lambda*"
                          #{e\ 5258}#)))))))
            (#{global-extend\ 2492}#
              'core
              'case-lambda
              (lambda (#{e\ 5285}#
                       #{r\ 5286}#
                       #{w\ 5287}#
                       #{s\ 5288}#
                       #{mod\ 5289}#)
                (let ((#{tmp\ 5295}# #{e\ 5285}#))
                  (let ((#{tmp\ 5296}#
                          ($sc-dispatch
                            #{tmp\ 5295}#
                            '(any (any any . each-any)
                                  .
                                  #(each (any any . each-any))))))
                    (if #{tmp\ 5296}#
                      (@apply
                        (lambda (#{_\ 5304}#
                                 #{args\ 5305}#
                                 #{e1\ 5306}#
                                 #{e2\ 5307}#
                                 #{args*\ 5308}#
                                 #{e1*\ 5309}#
                                 #{e2*\ 5310}#)
                          (call-with-values
                            (lambda ()
                              (#{chi-lambda-case\ 2605}#
                                #{e\ 5285}#
                                #{r\ 5286}#
                                #{w\ 5287}#
                                #{s\ 5288}#
                                #{mod\ 5289}#
                                #{lambda-formals\ 2599}#
                                (cons (cons #{args\ 5305}#
                                            (cons #{e1\ 5306}# #{e2\ 5307}#))
                                      (map (lambda (#{tmp\ 5314}#
                                                    #{tmp\ 5313}#
                                                    #{tmp\ 5312}#)
                                             (cons #{tmp\ 5312}#
                                                   (cons #{tmp\ 5313}#
                                                         #{tmp\ 5314}#)))
                                           #{e2*\ 5310}#
                                           #{e1*\ 5309}#
                                           #{args*\ 5308}#))))
                            (lambda (#{meta\ 5316}# #{lcase\ 5317}#)
                              (#{build-case-lambda\ 2440}#
                                #{s\ 5288}#
                                #{meta\ 5316}#
                                #{lcase\ 5317}#))))
                        #{tmp\ 5296}#)
                      (let ((#{_\ 5321}# #{tmp\ 5295}#))
                        (syntax-violation
                          'case-lambda
                          "bad case-lambda"
                          #{e\ 5285}#)))))))
            (#{global-extend\ 2492}#
              'core
              'case-lambda*
              (lambda (#{e\ 5322}#
                       #{r\ 5323}#
                       #{w\ 5324}#
                       #{s\ 5325}#
                       #{mod\ 5326}#)
                (let ((#{tmp\ 5332}# #{e\ 5322}#))
                  (let ((#{tmp\ 5333}#
                          ($sc-dispatch
                            #{tmp\ 5332}#
                            '(any (any any . each-any)
                                  .
                                  #(each (any any . each-any))))))
                    (if #{tmp\ 5333}#
                      (@apply
                        (lambda (#{_\ 5341}#
                                 #{args\ 5342}#
                                 #{e1\ 5343}#
                                 #{e2\ 5344}#
                                 #{args*\ 5345}#
                                 #{e1*\ 5346}#
                                 #{e2*\ 5347}#)
                          (call-with-values
                            (lambda ()
                              (#{chi-lambda-case\ 2605}#
                                #{e\ 5322}#
                                #{r\ 5323}#
                                #{w\ 5324}#
                                #{s\ 5325}#
                                #{mod\ 5326}#
                                #{lambda*-formals\ 2603}#
                                (cons (cons #{args\ 5342}#
                                            (cons #{e1\ 5343}# #{e2\ 5344}#))
                                      (map (lambda (#{tmp\ 5351}#
                                                    #{tmp\ 5350}#
                                                    #{tmp\ 5349}#)
                                             (cons #{tmp\ 5349}#
                                                   (cons #{tmp\ 5350}#
                                                         #{tmp\ 5351}#)))
                                           #{e2*\ 5347}#
                                           #{e1*\ 5346}#
                                           #{args*\ 5345}#))))
                            (lambda (#{meta\ 5353}# #{lcase\ 5354}#)
                              (#{build-case-lambda\ 2440}#
                                #{s\ 5325}#
                                #{meta\ 5353}#
                                #{lcase\ 5354}#))))
                        #{tmp\ 5333}#)
                      (let ((#{_\ 5358}# #{tmp\ 5332}#))
                        (syntax-violation
                          'case-lambda
                          "bad case-lambda*"
                          #{e\ 5322}#)))))))
            (#{global-extend\ 2492}#
              'core
              'let
              (letrec*
                ((#{chi-let\ 5360}#
                   (lambda (#{e\ 5361}#
                            #{r\ 5362}#
                            #{w\ 5363}#
                            #{s\ 5364}#
                            #{mod\ 5365}#
                            #{constructor\ 5366}#
                            #{ids\ 5367}#
                            #{vals\ 5368}#
                            #{exps\ 5369}#)
                     (if (not (#{valid-bound-ids?\ 2559}# #{ids\ 5367}#))
                       (syntax-violation
                         'let
                         "duplicate bound variable"
                         #{e\ 5361}#)
                       (begin
                         (let ((#{labels\ 5381}#
                                 (#{gen-labels\ 2514}# #{ids\ 5367}#))
                               (#{new-vars\ 5382}#
                                 (map #{gen-var\ 2609}# #{ids\ 5367}#)))
                           (begin
                             (let ((#{nw\ 5385}#
                                     (#{make-binding-wrap\ 2543}#
                                       #{ids\ 5367}#
                                       #{labels\ 5381}#
                                       #{w\ 5363}#))
                                   (#{nr\ 5386}#
                                     (#{extend-var-env\ 2486}#
                                       #{labels\ 5381}#
                                       #{new-vars\ 5382}#
                                       #{r\ 5362}#)))
                               (#{constructor\ 5366}#
                                 #{s\ 5364}#
                                 (map syntax->datum #{ids\ 5367}#)
                                 #{new-vars\ 5382}#
                                 (map (lambda (#{x\ 5387}#)
                                        (#{chi\ 2581}#
                                          #{x\ 5387}#
                                          #{r\ 5362}#
                                          #{w\ 5363}#
                                          #{mod\ 5365}#))
                                      #{vals\ 5368}#)
                                 (#{chi-body\ 2589}#
                                   #{exps\ 5369}#
                                   (#{source-wrap\ 2567}#
                                     #{e\ 5361}#
                                     #{nw\ 5385}#
                                     #{s\ 5364}#
                                     #{mod\ 5365}#)
                                   #{nr\ 5386}#
                                   #{nw\ 5385}#
                                   #{mod\ 5365}#))))))))))
                (begin
                  (lambda (#{e\ 5389}#
                           #{r\ 5390}#
                           #{w\ 5391}#
                           #{s\ 5392}#
                           #{mod\ 5393}#)
                    (let ((#{tmp\ 5399}# #{e\ 5389}#))
                      (let ((#{tmp\ 5400}#
                              ($sc-dispatch
                                #{tmp\ 5399}#
                                '(any #(each (any any)) any . each-any))))
                        (if (if #{tmp\ 5400}#
                              (@apply
                                (lambda (#{_\ 5406}#
                                         #{id\ 5407}#
                                         #{val\ 5408}#
                                         #{e1\ 5409}#
                                         #{e2\ 5410}#)
                                  (and-map #{id?\ 2496}# #{id\ 5407}#))
                                #{tmp\ 5400}#)
                              #f)
                          (@apply
                            (lambda (#{_\ 5417}#
                                     #{id\ 5418}#
                                     #{val\ 5419}#
                                     #{e1\ 5420}#
                                     #{e2\ 5421}#)
                              (#{chi-let\ 5360}#
                                #{e\ 5389}#
                                #{r\ 5390}#
                                #{w\ 5391}#
                                #{s\ 5392}#
                                #{mod\ 5393}#
                                #{build-let\ 2450}#
                                #{id\ 5418}#
                                #{val\ 5419}#
                                (cons #{e1\ 5420}# #{e2\ 5421}#)))
                            #{tmp\ 5400}#)
                          (let ((#{tmp\ 5425}#
                                  ($sc-dispatch
                                    #{tmp\ 5399}#
                                    '(any any
                                          #(each (any any))
                                          any
                                          .
                                          each-any))))
                            (if (if #{tmp\ 5425}#
                                  (@apply
                                    (lambda (#{_\ 5432}#
                                             #{f\ 5433}#
                                             #{id\ 5434}#
                                             #{val\ 5435}#
                                             #{e1\ 5436}#
                                             #{e2\ 5437}#)
                                      (if (#{id?\ 2496}# #{f\ 5433}#)
                                        (and-map #{id?\ 2496}# #{id\ 5434}#)
                                        #f))
                                    #{tmp\ 5425}#)
                                  #f)
                              (@apply
                                (lambda (#{_\ 5447}#
                                         #{f\ 5448}#
                                         #{id\ 5449}#
                                         #{val\ 5450}#
                                         #{e1\ 5451}#
                                         #{e2\ 5452}#)
                                  (#{chi-let\ 5360}#
                                    #{e\ 5389}#
                                    #{r\ 5390}#
                                    #{w\ 5391}#
                                    #{s\ 5392}#
                                    #{mod\ 5393}#
                                    #{build-named-let\ 2452}#
                                    (cons #{f\ 5448}# #{id\ 5449}#)
                                    #{val\ 5450}#
                                    (cons #{e1\ 5451}# #{e2\ 5452}#)))
                                #{tmp\ 5425}#)
                              (let ((#{_\ 5457}# #{tmp\ 5399}#))
                                (syntax-violation
                                  'let
                                  "bad let"
                                  (#{source-wrap\ 2567}#
                                    #{e\ 5389}#
                                    #{w\ 5391}#
                                    #{s\ 5392}#
                                    #{mod\ 5393}#))))))))))))
            (#{global-extend\ 2492}#
              'core
              'letrec
              (lambda (#{e\ 5458}#
                       #{r\ 5459}#
                       #{w\ 5460}#
                       #{s\ 5461}#
                       #{mod\ 5462}#)
                (let ((#{tmp\ 5468}# #{e\ 5458}#))
                  (let ((#{tmp\ 5469}#
                          ($sc-dispatch
                            #{tmp\ 5468}#
                            '(any #(each (any any)) any . each-any))))
                    (if (if #{tmp\ 5469}#
                          (@apply
                            (lambda (#{_\ 5475}#
                                     #{id\ 5476}#
                                     #{val\ 5477}#
                                     #{e1\ 5478}#
                                     #{e2\ 5479}#)
                              (and-map #{id?\ 2496}# #{id\ 5476}#))
                            #{tmp\ 5469}#)
                          #f)
                      (@apply
                        (lambda (#{_\ 5486}#
                                 #{id\ 5487}#
                                 #{val\ 5488}#
                                 #{e1\ 5489}#
                                 #{e2\ 5490}#)
                          (begin
                            (let ((#{ids\ 5492}# #{id\ 5487}#))
                              (if (not (#{valid-bound-ids?\ 2559}#
                                         #{ids\ 5492}#))
                                (syntax-violation
                                  'letrec
                                  "duplicate bound variable"
                                  #{e\ 5458}#)
                                (begin
                                  (let ((#{labels\ 5496}#
                                          (#{gen-labels\ 2514}# #{ids\ 5492}#))
                                        (#{new-vars\ 5497}#
                                          (map #{gen-var\ 2609}#
                                               #{ids\ 5492}#)))
                                    (begin
                                      (let ((#{w\ 5500}#
                                              (#{make-binding-wrap\ 2543}#
                                                #{ids\ 5492}#
                                                #{labels\ 5496}#
                                                #{w\ 5460}#))
                                            (#{r\ 5501}#
                                              (#{extend-var-env\ 2486}#
                                                #{labels\ 5496}#
                                                #{new-vars\ 5497}#
                                                #{r\ 5459}#)))
                                        (#{build-letrec\ 2454}#
                                          #{s\ 5461}#
                                          #f
                                          (map syntax->datum #{ids\ 5492}#)
                                          #{new-vars\ 5497}#
                                          (map (lambda (#{x\ 5502}#)
                                                 (#{chi\ 2581}#
                                                   #{x\ 5502}#
                                                   #{r\ 5501}#
                                                   #{w\ 5500}#
                                                   #{mod\ 5462}#))
                                               #{val\ 5488}#)
                                          (#{chi-body\ 2589}#
                                            (cons #{e1\ 5489}# #{e2\ 5490}#)
                                            (#{source-wrap\ 2567}#
                                              #{e\ 5458}#
                                              #{w\ 5500}#
                                              #{s\ 5461}#
                                              #{mod\ 5462}#)
                                            #{r\ 5501}#
                                            #{w\ 5500}#
                                            #{mod\ 5462}#))))))))))
                        #{tmp\ 5469}#)
                      (let ((#{_\ 5507}# #{tmp\ 5468}#))
                        (syntax-violation
                          'letrec
                          "bad letrec"
                          (#{source-wrap\ 2567}#
                            #{e\ 5458}#
                            #{w\ 5460}#
                            #{s\ 5461}#
                            #{mod\ 5462}#))))))))
            (#{global-extend\ 2492}#
              'core
              'letrec*
              (lambda (#{e\ 5508}#
                       #{r\ 5509}#
                       #{w\ 5510}#
                       #{s\ 5511}#
                       #{mod\ 5512}#)
                (let ((#{tmp\ 5518}# #{e\ 5508}#))
                  (let ((#{tmp\ 5519}#
                          ($sc-dispatch
                            #{tmp\ 5518}#
                            '(any #(each (any any)) any . each-any))))
                    (if (if #{tmp\ 5519}#
                          (@apply
                            (lambda (#{_\ 5525}#
                                     #{id\ 5526}#
                                     #{val\ 5527}#
                                     #{e1\ 5528}#
                                     #{e2\ 5529}#)
                              (and-map #{id?\ 2496}# #{id\ 5526}#))
                            #{tmp\ 5519}#)
                          #f)
                      (@apply
                        (lambda (#{_\ 5536}#
                                 #{id\ 5537}#
                                 #{val\ 5538}#
                                 #{e1\ 5539}#
                                 #{e2\ 5540}#)
                          (begin
                            (let ((#{ids\ 5542}# #{id\ 5537}#))
                              (if (not (#{valid-bound-ids?\ 2559}#
                                         #{ids\ 5542}#))
                                (syntax-violation
                                  'letrec*
                                  "duplicate bound variable"
                                  #{e\ 5508}#)
                                (begin
                                  (let ((#{labels\ 5546}#
                                          (#{gen-labels\ 2514}# #{ids\ 5542}#))
                                        (#{new-vars\ 5547}#
                                          (map #{gen-var\ 2609}#
                                               #{ids\ 5542}#)))
                                    (begin
                                      (let ((#{w\ 5550}#
                                              (#{make-binding-wrap\ 2543}#
                                                #{ids\ 5542}#
                                                #{labels\ 5546}#
                                                #{w\ 5510}#))
                                            (#{r\ 5551}#
                                              (#{extend-var-env\ 2486}#
                                                #{labels\ 5546}#
                                                #{new-vars\ 5547}#
                                                #{r\ 5509}#)))
                                        (#{build-letrec\ 2454}#
                                          #{s\ 5511}#
                                          #t
                                          (map syntax->datum #{ids\ 5542}#)
                                          #{new-vars\ 5547}#
                                          (map (lambda (#{x\ 5552}#)
                                                 (#{chi\ 2581}#
                                                   #{x\ 5552}#
                                                   #{r\ 5551}#
                                                   #{w\ 5550}#
                                                   #{mod\ 5512}#))
                                               #{val\ 5538}#)
                                          (#{chi-body\ 2589}#
                                            (cons #{e1\ 5539}# #{e2\ 5540}#)
                                            (#{source-wrap\ 2567}#
                                              #{e\ 5508}#
                                              #{w\ 5550}#
                                              #{s\ 5511}#
                                              #{mod\ 5512}#)
                                            #{r\ 5551}#
                                            #{w\ 5550}#
                                            #{mod\ 5512}#))))))))))
                        #{tmp\ 5519}#)
                      (let ((#{_\ 5557}# #{tmp\ 5518}#))
                        (syntax-violation
                          'letrec*
                          "bad letrec*"
                          (#{source-wrap\ 2567}#
                            #{e\ 5508}#
                            #{w\ 5510}#
                            #{s\ 5511}#
                            #{mod\ 5512}#))))))))
            (#{global-extend\ 2492}#
              'core
              'set!
              (lambda (#{e\ 5558}#
                       #{r\ 5559}#
                       #{w\ 5560}#
                       #{s\ 5561}#
                       #{mod\ 5562}#)
                (let ((#{tmp\ 5568}# #{e\ 5558}#))
                  (let ((#{tmp\ 5569}#
                          ($sc-dispatch
                            #{tmp\ 5568}#
                            '(any any any))))
                    (if (if #{tmp\ 5569}#
                          (@apply
                            (lambda (#{_\ 5573}# #{id\ 5574}# #{val\ 5575}#)
                              (#{id?\ 2496}# #{id\ 5574}#))
                            #{tmp\ 5569}#)
                          #f)
                      (@apply
                        (lambda (#{_\ 5579}# #{id\ 5580}# #{val\ 5581}#)
                          (begin
                            (let ((#{n\ 5583}#
                                    (#{id-var-name\ 2553}#
                                      #{id\ 5580}#
                                      #{w\ 5560}#)))
                              (begin
                                (let ((#{b\ 5585}#
                                        (#{lookup\ 2490}#
                                          #{n\ 5583}#
                                          #{r\ 5559}#
                                          #{mod\ 5562}#)))
                                  (begin
                                    (let ((#{atom-key\ 5588}#
                                            (#{binding-type\ 2479}#
                                              #{b\ 5585}#)))
                                      (if (eqv? #{atom-key\ 5588}#
                                                'lexical)
                                        (#{build-lexical-assignment\ 2428}#
                                          #{s\ 5561}#
                                          (syntax->datum #{id\ 5580}#)
                                          (#{binding-value\ 2481}# #{b\ 5585}#)
                                          (#{chi\ 2581}#
                                            #{val\ 5581}#
                                            #{r\ 5559}#
                                            #{w\ 5560}#
                                            #{mod\ 5562}#))
                                        (if (eqv? #{atom-key\ 5588}#
                                                  'global)
                                          (#{build-global-assignment\ 2434}#
                                            #{s\ 5561}#
                                            #{n\ 5583}#
                                            (#{chi\ 2581}#
                                              #{val\ 5581}#
                                              #{r\ 5559}#
                                              #{w\ 5560}#
                                              #{mod\ 5562}#)
                                            #{mod\ 5562}#)
                                          (if (eqv? #{atom-key\ 5588}#
                                                    'macro)
                                            (begin
                                              (let ((#{p\ 5593}#
                                                      (#{binding-value\ 2481}#
                                                        #{b\ 5585}#)))
                                                (if (procedure-property
                                                      #{p\ 5593}#
                                                      'variable-transformer)
                                                  (#{chi\ 2581}#
                                                    (#{chi-macro\ 2587}#
                                                      #{p\ 5593}#
                                                      #{e\ 5558}#
                                                      #{r\ 5559}#
                                                      #{w\ 5560}#
                                                      #{s\ 5561}#
                                                      #f
                                                      #{mod\ 5562}#)
                                                    #{r\ 5559}#
                                                    #{w\ 5560}#
                                                    #{mod\ 5562}#)
                                                  (syntax-violation
                                                    'set!
                                                    "not a variable transformer"
                                                    (#{wrap\ 2565}#
                                                      #{e\ 5558}#
                                                      #{w\ 5560}#
                                                      #{mod\ 5562}#)
                                                    (#{wrap\ 2565}#
                                                      #{id\ 5580}#
                                                      #{w\ 5560}#
                                                      #{mod\ 5562}#)))))
                                            (if (eqv? #{atom-key\ 5588}#
                                                      'displaced-lexical)
                                              (syntax-violation
                                                'set!
                                                "identifier out of context"
                                                (#{wrap\ 2565}#
                                                  #{id\ 5580}#
                                                  #{w\ 5560}#
                                                  #{mod\ 5562}#))
                                              (syntax-violation
                                                'set!
                                                "bad set!"
                                                (#{source-wrap\ 2567}#
                                                  #{e\ 5558}#
                                                  #{w\ 5560}#
                                                  #{s\ 5561}#
                                                  #{mod\ 5562}#)))))))))))))
                        #{tmp\ 5569}#)
                      (let ((#{tmp\ 5596}#
                              ($sc-dispatch
                                #{tmp\ 5568}#
                                '(any (any . each-any) any))))
                        (if #{tmp\ 5596}#
                          (@apply
                            (lambda (#{_\ 5601}#
                                     #{head\ 5602}#
                                     #{tail\ 5603}#
                                     #{val\ 5604}#)
                              (call-with-values
                                (lambda ()
                                  (#{syntax-type\ 2577}#
                                    #{head\ 5602}#
                                    #{r\ 5559}#
                                    '(())
                                    #f
                                    #f
                                    #{mod\ 5562}#
                                    #t))
                                (lambda (#{type\ 5607}#
                                         #{value\ 5608}#
                                         #{ee\ 5609}#
                                         #{ww\ 5610}#
                                         #{ss\ 5611}#
                                         #{modmod\ 5612}#)
                                  (if (eqv? #{type\ 5607}# (quote module-ref))
                                    (begin
                                      (let ((#{val\ 5621}#
                                              (#{chi\ 2581}#
                                                #{val\ 5604}#
                                                #{r\ 5559}#
                                                #{w\ 5560}#
                                                #{mod\ 5562}#)))
                                        (call-with-values
                                          (lambda ()
                                            (#{value\ 5608}#
                                              (cons #{head\ 5602}#
                                                    #{tail\ 5603}#)
                                              #{r\ 5559}#
                                              #{w\ 5560}#))
                                          (lambda (#{e\ 5623}#
                                                   #{r\ 5624}#
                                                   #{w\ 5625}#
                                                   #{s*\ 5626}#
                                                   #{mod\ 5627}#)
                                            (let ((#{tmp\ 5633}# #{e\ 5623}#))
                                              (let ((#{tmp\ 5634}#
                                                      (list #{tmp\ 5633}#)))
                                                (if (if #{tmp\ 5634}#
                                                      (@apply
                                                        (lambda (#{e\ 5636}#)
                                                          (#{id?\ 2496}#
                                                            #{e\ 5636}#))
                                                        #{tmp\ 5634}#)
                                                      #f)
                                                  (@apply
                                                    (lambda (#{e\ 5638}#)
                                                      (#{build-global-assignment\ 2434}#
                                                        #{s\ 5561}#
                                                        (syntax->datum
                                                          #{e\ 5638}#)
                                                        #{val\ 5621}#
                                                        #{mod\ 5627}#))
                                                    #{tmp\ 5634}#)
                                                  (syntax-violation
                                                    #f
                                                    "source expression failed to match any pattern"
                                                    #{tmp\ 5633}#))))))))
                                    (#{build-application\ 2420}#
                                      #{s\ 5561}#
                                      (#{chi\ 2581}#
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
                                                    #("i5613"
                                                      "i5614"
                                                      "i5615"
                                                      "i5616"
                                                      "i5617"
                                                      "i5618"))
                                                  #(ribcage
                                                    #(_ head tail val)
                                                    #((top) (top) (top) (top))
                                                    #("i5597"
                                                      "i5598"
                                                      "i5599"
                                                      "i5600"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(e r w s mod)
                                                    #((top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top))
                                                    #("i5563"
                                                      "i5564"
                                                      "i5565"
                                                      "i5566"
                                                      "i5567"))
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
                                                      gensym-hook
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
                                                     (top))
                                                    ("i2610"
                                                     "i2608"
                                                     "i2606"
                                                     "i2604"
                                                     "i2602"
                                                     "i2600"
                                                     "i2598"
                                                     "i2596"
                                                     "i2594"
                                                     "i2592"
                                                     "i2590"
                                                     "i2588"
                                                     "i2586"
                                                     "i2584"
                                                     "i2582"
                                                     "i2580"
                                                     "i2578"
                                                     "i2576"
                                                     "i2574"
                                                     "i2572"
                                                     "i2570"
                                                     "i2568"
                                                     "i2566"
                                                     "i2564"
                                                     "i2562"
                                                     "i2560"
                                                     "i2558"
                                                     "i2556"
                                                     "i2554"
                                                     "i2552"
                                                     "i2550"
                                                     "i2548"
                                                     "i2546"
                                                     "i2544"
                                                     "i2542"
                                                     "i2540"
                                                     "i2539"
                                                     "i2538"
                                                     "i2536"
                                                     "i2535"
                                                     "i2534"
                                                     "i2533"
                                                     "i2532"
                                                     "i2530"
                                                     "i2528"
                                                     "i2526"
                                                     "i2524"
                                                     "i2522"
                                                     "i2520"
                                                     "i2518"
                                                     "i2516"
                                                     "i2513"
                                                     "i2511"
                                                     "i2510"
                                                     "i2509"
                                                     "i2508"
                                                     "i2507"
                                                     "i2506"
                                                     "i2504"
                                                     "i2502"
                                                     "i2500"
                                                     "i2498"
                                                     "i2497"
                                                     "i2495"
                                                     "i2493"
                                                     "i2491"
                                                     "i2489"
                                                     "i2487"
                                                     "i2485"
                                                     "i2483"
                                                     "i2482"
                                                     "i2480"
                                                     "i2478"
                                                     "i2477"
                                                     "i2476"
                                                     "i2474"
                                                     "i2473"
                                                     "i2471"
                                                     "i2469"
                                                     "i2467"
                                                     "i2465"
                                                     "i2463"
                                                     "i2461"
                                                     "i2459"
                                                     "i2457"
                                                     "i2455"
                                                     "i2453"
                                                     "i2451"
                                                     "i2449"
                                                     "i2447"
                                                     "i2445"
                                                     "i2443"
                                                     "i2441"
                                                     "i2439"
                                                     "i2437"
                                                     "i2435"
                                                     "i2433"
                                                     "i2431"
                                                     "i2429"
                                                     "i2427"
                                                     "i2425"
                                                     "i2423"
                                                     "i2421"
                                                     "i2419"
                                                     "i2417"
                                                     "i2415"
                                                     "i2413"
                                                     "i2411"
                                                     "i2409"
                                                     "i2408"
                                                     "i2406"
                                                     "i2404"
                                                     "i2402"
                                                     "i2400"
                                                     "i2398"
                                                     "i2396"
                                                     "i2394"
                                                     "i2392"
                                                     "i2390"
                                                     "i2387"
                                                     "i2385"
                                                     "i2383"
                                                     "i2381"
                                                     "i2379"
                                                     "i2377"
                                                     "i2375"
                                                     "i2373"
                                                     "i2371"
                                                     "i2369"
                                                     "i2367"
                                                     "i2365"
                                                     "i2363"
                                                     "i2361"
                                                     "i2359"
                                                     "i2357"
                                                     "i2355"
                                                     "i2353"))
                                                  #(ribcage
                                                    (define-structure
                                                      define-expansion-accessors
                                                      define-expansion-constructors
                                                      and-map*)
                                                    ((top) (top) (top) (top))
                                                    ("i2147"
                                                     "i2146"
                                                     "i2145"
                                                     "i2143")))
                                                 (hygiene guile))
                                              #{head\ 5602}#)
                                        #{r\ 5559}#
                                        #{w\ 5560}#
                                        #{mod\ 5562}#)
                                      (map (lambda (#{e\ 5640}#)
                                             (#{chi\ 2581}#
                                               #{e\ 5640}#
                                               #{r\ 5559}#
                                               #{w\ 5560}#
                                               #{mod\ 5562}#))
                                           (append
                                             #{tail\ 5603}#
                                             (list #{val\ 5604}#))))))))
                            #{tmp\ 5596}#)
                          (let ((#{_\ 5644}# #{tmp\ 5568}#))
                            (syntax-violation
                              'set!
                              "bad set!"
                              (#{source-wrap\ 2567}#
                                #{e\ 5558}#
                                #{w\ 5560}#
                                #{s\ 5561}#
                                #{mod\ 5562}#))))))))))
            (#{global-extend\ 2492}#
              'module-ref
              '@
              (lambda (#{e\ 5645}# #{r\ 5646}# #{w\ 5647}#)
                (let ((#{tmp\ 5651}# #{e\ 5645}#))
                  (let ((#{tmp\ 5652}#
                          ($sc-dispatch
                            #{tmp\ 5651}#
                            '(any each-any any))))
                    (if (if #{tmp\ 5652}#
                          (@apply
                            (lambda (#{_\ 5656}# #{mod\ 5657}# #{id\ 5658}#)
                              (if (and-map #{id?\ 2496}# #{mod\ 5657}#)
                                (#{id?\ 2496}# #{id\ 5658}#)
                                #f))
                            #{tmp\ 5652}#)
                          #f)
                      (@apply
                        (lambda (#{_\ 5665}# #{mod\ 5666}# #{id\ 5667}#)
                          (values
                            (syntax->datum #{id\ 5667}#)
                            #{r\ 5646}#
                            #{w\ 5647}#
                            #f
                            (syntax->datum
                              (cons '#(syntax-object
                                       public
                                       ((top)
                                        #(ribcage
                                          #(_ mod id)
                                          #((top) (top) (top))
                                          #("i5662" "i5663" "i5664"))
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(e r w)
                                          #((top) (top) (top))
                                          #("i5648" "i5649" "i5650"))
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
                                            gensym-hook
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
                                           (top))
                                          ("i2610"
                                           "i2608"
                                           "i2606"
                                           "i2604"
                                           "i2602"
                                           "i2600"
                                           "i2598"
                                           "i2596"
                                           "i2594"
                                           "i2592"
                                           "i2590"
                                           "i2588"
                                           "i2586"
                                           "i2584"
                                           "i2582"
                                           "i2580"
                                           "i2578"
                                           "i2576"
                                           "i2574"
                                           "i2572"
                                           "i2570"
                                           "i2568"
                                           "i2566"
                                           "i2564"
                                           "i2562"
                                           "i2560"
                                           "i2558"
                                           "i2556"
                                           "i2554"
                                           "i2552"
                                           "i2550"
                                           "i2548"
                                           "i2546"
                                           "i2544"
                                           "i2542"
                                           "i2540"
                                           "i2539"
                                           "i2538"
                                           "i2536"
                                           "i2535"
                                           "i2534"
                                           "i2533"
                                           "i2532"
                                           "i2530"
                                           "i2528"
                                           "i2526"
                                           "i2524"
                                           "i2522"
                                           "i2520"
                                           "i2518"
                                           "i2516"
                                           "i2513"
                                           "i2511"
                                           "i2510"
                                           "i2509"
                                           "i2508"
                                           "i2507"
                                           "i2506"
                                           "i2504"
                                           "i2502"
                                           "i2500"
                                           "i2498"
                                           "i2497"
                                           "i2495"
                                           "i2493"
                                           "i2491"
                                           "i2489"
                                           "i2487"
                                           "i2485"
                                           "i2483"
                                           "i2482"
                                           "i2480"
                                           "i2478"
                                           "i2477"
                                           "i2476"
                                           "i2474"
                                           "i2473"
                                           "i2471"
                                           "i2469"
                                           "i2467"
                                           "i2465"
                                           "i2463"
                                           "i2461"
                                           "i2459"
                                           "i2457"
                                           "i2455"
                                           "i2453"
                                           "i2451"
                                           "i2449"
                                           "i2447"
                                           "i2445"
                                           "i2443"
                                           "i2441"
                                           "i2439"
                                           "i2437"
                                           "i2435"
                                           "i2433"
                                           "i2431"
                                           "i2429"
                                           "i2427"
                                           "i2425"
                                           "i2423"
                                           "i2421"
                                           "i2419"
                                           "i2417"
                                           "i2415"
                                           "i2413"
                                           "i2411"
                                           "i2409"
                                           "i2408"
                                           "i2406"
                                           "i2404"
                                           "i2402"
                                           "i2400"
                                           "i2398"
                                           "i2396"
                                           "i2394"
                                           "i2392"
                                           "i2390"
                                           "i2387"
                                           "i2385"
                                           "i2383"
                                           "i2381"
                                           "i2379"
                                           "i2377"
                                           "i2375"
                                           "i2373"
                                           "i2371"
                                           "i2369"
                                           "i2367"
                                           "i2365"
                                           "i2363"
                                           "i2361"
                                           "i2359"
                                           "i2357"
                                           "i2355"
                                           "i2353"))
                                        #(ribcage
                                          (define-structure
                                            define-expansion-accessors
                                            define-expansion-constructors
                                            and-map*)
                                          ((top) (top) (top) (top))
                                          ("i2147" "i2146" "i2145" "i2143")))
                                       (hygiene guile))
                                    #{mod\ 5666}#))))
                        #{tmp\ 5652}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp\ 5651}#))))))
            (#{global-extend\ 2492}#
              'module-ref
              '@@
              (lambda (#{e\ 5669}# #{r\ 5670}# #{w\ 5671}#)
                (letrec*
                  ((#{remodulate\ 5676}#
                     (lambda (#{x\ 5677}# #{mod\ 5678}#)
                       (if (pair? #{x\ 5677}#)
                         (cons (#{remodulate\ 5676}#
                                 (car #{x\ 5677}#)
                                 #{mod\ 5678}#)
                               (#{remodulate\ 5676}#
                                 (cdr #{x\ 5677}#)
                                 #{mod\ 5678}#))
                         (if (#{syntax-object?\ 2460}# #{x\ 5677}#)
                           (#{make-syntax-object\ 2458}#
                             (#{remodulate\ 5676}#
                               (#{syntax-object-expression\ 2462}# #{x\ 5677}#)
                               #{mod\ 5678}#)
                             (#{syntax-object-wrap\ 2464}# #{x\ 5677}#)
                             #{mod\ 5678}#)
                           (if (vector? #{x\ 5677}#)
                             (begin
                               (let ((#{n\ 5689}# (vector-length #{x\ 5677}#)))
                                 (begin
                                   (let ((#{v\ 5691}#
                                           (make-vector #{n\ 5689}#)))
                                     (letrec*
                                       ((#{loop\ 5694}#
                                          (lambda (#{i\ 5695}#)
                                            (if (#{fx=\ 2401}#
                                                  #{i\ 5695}#
                                                  #{n\ 5689}#)
                                              (begin (if #f #f) #{v\ 5691}#)
                                              (begin
                                                (vector-set!
                                                  #{v\ 5691}#
                                                  #{i\ 5695}#
                                                  (#{remodulate\ 5676}#
                                                    (vector-ref
                                                      #{x\ 5677}#
                                                      #{i\ 5695}#)
                                                    #{mod\ 5678}#))
                                                (#{loop\ 5694}#
                                                  (#{fx+\ 2397}#
                                                    #{i\ 5695}#
                                                    1)))))))
                                       (begin (#{loop\ 5694}# 0)))))))
                             #{x\ 5677}#))))))
                  (begin
                    (let ((#{tmp\ 5699}# #{e\ 5669}#))
                      (let ((#{tmp\ 5700}#
                              ($sc-dispatch
                                #{tmp\ 5699}#
                                '(any each-any any))))
                        (if (if #{tmp\ 5700}#
                              (@apply
                                (lambda (#{_\ 5704}#
                                         #{mod\ 5705}#
                                         #{exp\ 5706}#)
                                  (and-map #{id?\ 2496}# #{mod\ 5705}#))
                                #{tmp\ 5700}#)
                              #f)
                          (@apply
                            (lambda (#{_\ 5711}# #{mod\ 5712}# #{exp\ 5713}#)
                              (begin
                                (let ((#{mod\ 5715}#
                                        (syntax->datum
                                          (cons '#(syntax-object
                                                   private
                                                   ((top)
                                                    #(ribcage
                                                      #(_ mod exp)
                                                      #((top) (top) (top))
                                                      #("i5708"
                                                        "i5709"
                                                        "i5710"))
                                                    #(ribcage
                                                      (remodulate)
                                                      ((top))
                                                      ("i5675"))
                                                    #(ribcage
                                                      #(e r w)
                                                      #((top) (top) (top))
                                                      #("i5672"
                                                        "i5673"
                                                        "i5674"))
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
                                                        gensym-hook
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
                                                       (top))
                                                      ("i2610"
                                                       "i2608"
                                                       "i2606"
                                                       "i2604"
                                                       "i2602"
                                                       "i2600"
                                                       "i2598"
                                                       "i2596"
                                                       "i2594"
                                                       "i2592"
                                                       "i2590"
                                                       "i2588"
                                                       "i2586"
                                                       "i2584"
                                                       "i2582"
                                                       "i2580"
                                                       "i2578"
                                                       "i2576"
                                                       "i2574"
                                                       "i2572"
                                                       "i2570"
                                                       "i2568"
                                                       "i2566"
                                                       "i2564"
                                                       "i2562"
                                                       "i2560"
                                                       "i2558"
                                                       "i2556"
                                                       "i2554"
                                                       "i2552"
                                                       "i2550"
                                                       "i2548"
                                                       "i2546"
                                                       "i2544"
                                                       "i2542"
                                                       "i2540"
                                                       "i2539"
                                                       "i2538"
                                                       "i2536"
                                                       "i2535"
                                                       "i2534"
                                                       "i2533"
                                                       "i2532"
                                                       "i2530"
                                                       "i2528"
                                                       "i2526"
                                                       "i2524"
                                                       "i2522"
                                                       "i2520"
                                                       "i2518"
                                                       "i2516"
                                                       "i2513"
                                                       "i2511"
                                                       "i2510"
                                                       "i2509"
                                                       "i2508"
                                                       "i2507"
                                                       "i2506"
                                                       "i2504"
                                                       "i2502"
                                                       "i2500"
                                                       "i2498"
                                                       "i2497"
                                                       "i2495"
                                                       "i2493"
                                                       "i2491"
                                                       "i2489"
                                                       "i2487"
                                                       "i2485"
                                                       "i2483"
                                                       "i2482"
                                                       "i2480"
                                                       "i2478"
                                                       "i2477"
                                                       "i2476"
                                                       "i2474"
                                                       "i2473"
                                                       "i2471"
                                                       "i2469"
                                                       "i2467"
                                                       "i2465"
                                                       "i2463"
                                                       "i2461"
                                                       "i2459"
                                                       "i2457"
                                                       "i2455"
                                                       "i2453"
                                                       "i2451"
                                                       "i2449"
                                                       "i2447"
                                                       "i2445"
                                                       "i2443"
                                                       "i2441"
                                                       "i2439"
                                                       "i2437"
                                                       "i2435"
                                                       "i2433"
                                                       "i2431"
                                                       "i2429"
                                                       "i2427"
                                                       "i2425"
                                                       "i2423"
                                                       "i2421"
                                                       "i2419"
                                                       "i2417"
                                                       "i2415"
                                                       "i2413"
                                                       "i2411"
                                                       "i2409"
                                                       "i2408"
                                                       "i2406"
                                                       "i2404"
                                                       "i2402"
                                                       "i2400"
                                                       "i2398"
                                                       "i2396"
                                                       "i2394"
                                                       "i2392"
                                                       "i2390"
                                                       "i2387"
                                                       "i2385"
                                                       "i2383"
                                                       "i2381"
                                                       "i2379"
                                                       "i2377"
                                                       "i2375"
                                                       "i2373"
                                                       "i2371"
                                                       "i2369"
                                                       "i2367"
                                                       "i2365"
                                                       "i2363"
                                                       "i2361"
                                                       "i2359"
                                                       "i2357"
                                                       "i2355"
                                                       "i2353"))
                                                    #(ribcage
                                                      (define-structure
                                                        define-expansion-accessors
                                                        define-expansion-constructors
                                                        and-map*)
                                                      ((top) (top) (top) (top))
                                                      ("i2147"
                                                       "i2146"
                                                       "i2145"
                                                       "i2143")))
                                                   (hygiene guile))
                                                #{mod\ 5712}#))))
                                  (values
                                    (#{remodulate\ 5676}#
                                      #{exp\ 5713}#
                                      #{mod\ 5715}#)
                                    #{r\ 5670}#
                                    #{w\ 5671}#
                                    (#{source-annotation\ 2475}# #{exp\ 5713}#)
                                    #{mod\ 5715}#))))
                            #{tmp\ 5700}#)
                          (syntax-violation
                            #f
                            "source expression failed to match any pattern"
                            #{tmp\ 5699}#))))))))
            (#{global-extend\ 2492}#
              'core
              'if
              (lambda (#{e\ 5717}#
                       #{r\ 5718}#
                       #{w\ 5719}#
                       #{s\ 5720}#
                       #{mod\ 5721}#)
                (let ((#{tmp\ 5727}# #{e\ 5717}#))
                  (let ((#{tmp\ 5728}#
                          ($sc-dispatch
                            #{tmp\ 5727}#
                            '(any any any))))
                    (if #{tmp\ 5728}#
                      (@apply
                        (lambda (#{_\ 5732}# #{test\ 5733}# #{then\ 5734}#)
                          (#{build-conditional\ 2422}#
                            #{s\ 5720}#
                            (#{chi\ 2581}#
                              #{test\ 5733}#
                              #{r\ 5718}#
                              #{w\ 5719}#
                              #{mod\ 5721}#)
                            (#{chi\ 2581}#
                              #{then\ 5734}#
                              #{r\ 5718}#
                              #{w\ 5719}#
                              #{mod\ 5721}#)
                            (#{build-void\ 2418}# #f)))
                        #{tmp\ 5728}#)
                      (let ((#{tmp\ 5736}#
                              ($sc-dispatch
                                #{tmp\ 5727}#
                                '(any any any any))))
                        (if #{tmp\ 5736}#
                          (@apply
                            (lambda (#{_\ 5741}#
                                     #{test\ 5742}#
                                     #{then\ 5743}#
                                     #{else\ 5744}#)
                              (#{build-conditional\ 2422}#
                                #{s\ 5720}#
                                (#{chi\ 2581}#
                                  #{test\ 5742}#
                                  #{r\ 5718}#
                                  #{w\ 5719}#
                                  #{mod\ 5721}#)
                                (#{chi\ 2581}#
                                  #{then\ 5743}#
                                  #{r\ 5718}#
                                  #{w\ 5719}#
                                  #{mod\ 5721}#)
                                (#{chi\ 2581}#
                                  #{else\ 5744}#
                                  #{r\ 5718}#
                                  #{w\ 5719}#
                                  #{mod\ 5721}#)))
                            #{tmp\ 5736}#)
                          (syntax-violation
                            #f
                            "source expression failed to match any pattern"
                            #{tmp\ 5727}#))))))))
            (#{global-extend\ 2492}#
              'core
              'with-fluids
              (lambda (#{e\ 5745}#
                       #{r\ 5746}#
                       #{w\ 5747}#
                       #{s\ 5748}#
                       #{mod\ 5749}#)
                (let ((#{tmp\ 5755}# #{e\ 5745}#))
                  (let ((#{tmp\ 5756}#
                          ($sc-dispatch
                            #{tmp\ 5755}#
                            '(any #(each (any any)) any . each-any))))
                    (if #{tmp\ 5756}#
                      (@apply
                        (lambda (#{_\ 5762}#
                                 #{fluid\ 5763}#
                                 #{val\ 5764}#
                                 #{b\ 5765}#
                                 #{b*\ 5766}#)
                          (#{build-dynlet\ 2424}#
                            #{s\ 5748}#
                            (map (lambda (#{x\ 5767}#)
                                   (#{chi\ 2581}#
                                     #{x\ 5767}#
                                     #{r\ 5746}#
                                     #{w\ 5747}#
                                     #{mod\ 5749}#))
                                 #{fluid\ 5763}#)
                            (map (lambda (#{x\ 5770}#)
                                   (#{chi\ 2581}#
                                     #{x\ 5770}#
                                     #{r\ 5746}#
                                     #{w\ 5747}#
                                     #{mod\ 5749}#))
                                 #{val\ 5764}#)
                            (#{chi-body\ 2589}#
                              (cons #{b\ 5765}# #{b*\ 5766}#)
                              (#{source-wrap\ 2567}#
                                #{e\ 5745}#
                                #{w\ 5747}#
                                #{s\ 5748}#
                                #{mod\ 5749}#)
                              #{r\ 5746}#
                              #{w\ 5747}#
                              #{mod\ 5749}#)))
                        #{tmp\ 5756}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp\ 5755}#))))))
            (#{global-extend\ 2492}#
              'begin
              'begin
              '())
            (#{global-extend\ 2492}#
              'define
              'define
              '())
            (#{global-extend\ 2492}#
              'define-syntax
              'define-syntax
              '())
            (#{global-extend\ 2492}#
              'eval-when
              'eval-when
              '())
            (#{global-extend\ 2492}#
              'core
              'syntax-case
              (letrec*
                ((#{convert-pattern\ 5775}#
                   (lambda (#{pattern\ 5782}# #{keys\ 5783}#)
                     (letrec*
                       ((#{cvt*\ 5787}#
                          (lambda (#{p*\ 5790}# #{n\ 5791}# #{ids\ 5792}#)
                            (if (null? #{p*\ 5790}#)
                              (values (quote ()) #{ids\ 5792}#)
                              (call-with-values
                                (lambda ()
                                  (#{cvt*\ 5787}#
                                    (cdr #{p*\ 5790}#)
                                    #{n\ 5791}#
                                    #{ids\ 5792}#))
                                (lambda (#{y\ 5796}# #{ids\ 5797}#)
                                  (call-with-values
                                    (lambda ()
                                      (#{cvt\ 5789}#
                                        (car #{p*\ 5790}#)
                                        #{n\ 5791}#
                                        #{ids\ 5797}#))
                                    (lambda (#{x\ 5800}# #{ids\ 5801}#)
                                      (values
                                        (cons #{x\ 5800}# #{y\ 5796}#)
                                        #{ids\ 5801}#))))))))
                        (#{cvt\ 5789}#
                          (lambda (#{p\ 5804}# #{n\ 5805}# #{ids\ 5806}#)
                            (if (#{id?\ 2496}# #{p\ 5804}#)
                              (if (#{bound-id-member?\ 2563}#
                                    #{p\ 5804}#
                                    #{keys\ 5783}#)
                                (values
                                  (vector (quote free-id) #{p\ 5804}#)
                                  #{ids\ 5806}#)
                                (values
                                  'any
                                  (cons (cons #{p\ 5804}# #{n\ 5805}#)
                                        #{ids\ 5806}#)))
                              (let ((#{tmp\ 5810}# #{p\ 5804}#))
                                (let ((#{tmp\ 5811}#
                                        ($sc-dispatch
                                          #{tmp\ 5810}#
                                          '(any any))))
                                  (if (if #{tmp\ 5811}#
                                        (@apply
                                          (lambda (#{x\ 5814}# #{dots\ 5815}#)
                                            (#{ellipsis?\ 2597}#
                                              #{dots\ 5815}#))
                                          #{tmp\ 5811}#)
                                        #f)
                                    (@apply
                                      (lambda (#{x\ 5818}# #{dots\ 5819}#)
                                        (call-with-values
                                          (lambda ()
                                            (#{cvt\ 5789}#
                                              #{x\ 5818}#
                                              (#{fx+\ 2397}# #{n\ 5805}# 1)
                                              #{ids\ 5806}#))
                                          (lambda (#{p\ 5820}# #{ids\ 5821}#)
                                            (values
                                              (if (eq? #{p\ 5820}# (quote any))
                                                'each-any
                                                (vector
                                                  'each
                                                  #{p\ 5820}#))
                                              #{ids\ 5821}#))))
                                      #{tmp\ 5811}#)
                                    (let ((#{tmp\ 5824}#
                                            ($sc-dispatch
                                              #{tmp\ 5810}#
                                              '(any any . each-any))))
                                      (if (if #{tmp\ 5824}#
                                            (@apply
                                              (lambda (#{x\ 5828}#
                                                       #{dots\ 5829}#
                                                       #{ys\ 5830}#)
                                                (#{ellipsis?\ 2597}#
                                                  #{dots\ 5829}#))
                                              #{tmp\ 5824}#)
                                            #f)
                                        (@apply
                                          (lambda (#{x\ 5834}#
                                                   #{dots\ 5835}#
                                                   #{ys\ 5836}#)
                                            (call-with-values
                                              (lambda ()
                                                (#{cvt*\ 5787}#
                                                  #{ys\ 5836}#
                                                  #{n\ 5805}#
                                                  #{ids\ 5806}#))
                                              (lambda (#{ys\ 5838}#
                                                       #{ids\ 5839}#)
                                                (call-with-values
                                                  (lambda ()
                                                    (#{cvt\ 5789}#
                                                      #{x\ 5834}#
                                                      (1+ #{n\ 5805}#)
                                                      #{ids\ 5839}#))
                                                  (lambda (#{x\ 5842}#
                                                           #{ids\ 5843}#)
                                                    (values
                                                      (list->vector
                                                        (cons 'each+
                                                              (cons #{x\ 5842}#
                                                                    (cons (reverse
                                                                            #{ys\ 5838}#)
                                                                          '(())))))
                                                      #{ids\ 5843}#))))))
                                          #{tmp\ 5824}#)
                                        (let ((#{tmp\ 5847}#
                                                ($sc-dispatch
                                                  #{tmp\ 5810}#
                                                  '(any . any))))
                                          (if #{tmp\ 5847}#
                                            (@apply
                                              (lambda (#{x\ 5850}# #{y\ 5851}#)
                                                (call-with-values
                                                  (lambda ()
                                                    (#{cvt\ 5789}#
                                                      #{y\ 5851}#
                                                      #{n\ 5805}#
                                                      #{ids\ 5806}#))
                                                  (lambda (#{y\ 5852}#
                                                           #{ids\ 5853}#)
                                                    (call-with-values
                                                      (lambda ()
                                                        (#{cvt\ 5789}#
                                                          #{x\ 5850}#
                                                          #{n\ 5805}#
                                                          #{ids\ 5853}#))
                                                      (lambda (#{x\ 5856}#
                                                               #{ids\ 5857}#)
                                                        (values
                                                          (cons #{x\ 5856}#
                                                                #{y\ 5852}#)
                                                          #{ids\ 5857}#))))))
                                              #{tmp\ 5847}#)
                                            (let ((#{tmp\ 5860}#
                                                    ($sc-dispatch
                                                      #{tmp\ 5810}#
                                                      '())))
                                              (if #{tmp\ 5860}#
                                                (@apply
                                                  (lambda ()
                                                    (values
                                                      '()
                                                      #{ids\ 5806}#))
                                                  #{tmp\ 5860}#)
                                                (let ((#{tmp\ 5861}#
                                                        ($sc-dispatch
                                                          #{tmp\ 5810}#
                                                          '#(vector
                                                             each-any))))
                                                  (if #{tmp\ 5861}#
                                                    (@apply
                                                      (lambda (#{x\ 5863}#)
                                                        (call-with-values
                                                          (lambda ()
                                                            (#{cvt\ 5789}#
                                                              #{x\ 5863}#
                                                              #{n\ 5805}#
                                                              #{ids\ 5806}#))
                                                          (lambda (#{p\ 5865}#
                                                                   #{ids\ 5866}#)
                                                            (values
                                                              (vector
                                                                'vector
                                                                #{p\ 5865}#)
                                                              #{ids\ 5866}#))))
                                                      #{tmp\ 5861}#)
                                                    (let ((#{x\ 5870}#
                                                            #{tmp\ 5810}#))
                                                      (values
                                                        (vector
                                                          'atom
                                                          (#{strip\ 2607}#
                                                            #{p\ 5804}#
                                                            '(())))
                                                        #{ids\ 5806}#)))))))))))))))))
                       (begin
                         (#{cvt\ 5789}# #{pattern\ 5782}# 0 (quote ()))))))
                 (#{build-dispatch-call\ 5777}#
                   (lambda (#{pvars\ 5872}#
                            #{exp\ 5873}#
                            #{y\ 5874}#
                            #{r\ 5875}#
                            #{mod\ 5876}#)
                     (begin
                       (map cdr #{pvars\ 5872}#)
                       (let ((#{ids\ 5884}# (map car #{pvars\ 5872}#)))
                         (begin
                           (let ((#{labels\ 5888}#
                                   (#{gen-labels\ 2514}# #{ids\ 5884}#))
                                 (#{new-vars\ 5889}#
                                   (map #{gen-var\ 2609}# #{ids\ 5884}#)))
                             (#{build-application\ 2420}#
                               #f
                               (#{build-primref\ 2444}# #f (quote apply))
                               (list (#{build-simple-lambda\ 2438}#
                                       #f
                                       (map syntax->datum #{ids\ 5884}#)
                                       #f
                                       #{new-vars\ 5889}#
                                       '()
                                       (#{chi\ 2581}#
                                         #{exp\ 5873}#
                                         (#{extend-env\ 2484}#
                                           #{labels\ 5888}#
                                           (map (lambda (#{var\ 5893}#
                                                         #{level\ 5894}#)
                                                  (cons 'syntax
                                                        (cons #{var\ 5893}#
                                                              #{level\ 5894}#)))
                                                #{new-vars\ 5889}#
                                                (map cdr #{pvars\ 5872}#))
                                           #{r\ 5875}#)
                                         (#{make-binding-wrap\ 2543}#
                                           #{ids\ 5884}#
                                           #{labels\ 5888}#
                                           '(()))
                                         #{mod\ 5876}#))
                                     #{y\ 5874}#))))))))
                 (#{gen-clause\ 5779}#
                   (lambda (#{x\ 5900}#
                            #{keys\ 5901}#
                            #{clauses\ 5902}#
                            #{r\ 5903}#
                            #{pat\ 5904}#
                            #{fender\ 5905}#
                            #{exp\ 5906}#
                            #{mod\ 5907}#)
                     (call-with-values
                       (lambda ()
                         (#{convert-pattern\ 5775}#
                           #{pat\ 5904}#
                           #{keys\ 5901}#))
                       (lambda (#{p\ 5916}# #{pvars\ 5917}#)
                         (if (not (#{distinct-bound-ids?\ 2561}#
                                    (map car #{pvars\ 5917}#)))
                           (syntax-violation
                             'syntax-case
                             "duplicate pattern variable"
                             #{pat\ 5904}#)
                           (if (not (and-map
                                      (lambda (#{x\ 5924}#)
                                        (not (#{ellipsis?\ 2597}#
                                               (car #{x\ 5924}#))))
                                      #{pvars\ 5917}#))
                             (syntax-violation
                               'syntax-case
                               "misplaced ellipsis"
                               #{pat\ 5904}#)
                             (begin
                               (let ((#{y\ 5928}#
                                       (#{gen-var\ 2609}# (quote tmp))))
                                 (#{build-application\ 2420}#
                                   #f
                                   (#{build-simple-lambda\ 2438}#
                                     #f
                                     (list (quote tmp))
                                     #f
                                     (list #{y\ 5928}#)
                                     '()
                                     (begin
                                       (let ((#{y\ 5932}#
                                               (#{build-lexical-reference\ 2426}#
                                                 'value
                                                 #f
                                                 'tmp
                                                 #{y\ 5928}#)))
                                         (#{build-conditional\ 2422}#
                                           #f
                                           (let ((#{tmp\ 5935}#
                                                   #{fender\ 5905}#))
                                             (let ((#{tmp\ 5936}#
                                                     ($sc-dispatch
                                                       #{tmp\ 5935}#
                                                       '#(atom #t))))
                                               (if #{tmp\ 5936}#
                                                 (@apply
                                                   (lambda () #{y\ 5932}#)
                                                   #{tmp\ 5936}#)
                                                 (let ((#{_\ 5938}#
                                                         #{tmp\ 5935}#))
                                                   (#{build-conditional\ 2422}#
                                                     #f
                                                     #{y\ 5932}#
                                                     (#{build-dispatch-call\ 5777}#
                                                       #{pvars\ 5917}#
                                                       #{fender\ 5905}#
                                                       #{y\ 5932}#
                                                       #{r\ 5903}#
                                                       #{mod\ 5907}#)
                                                     (#{build-data\ 2446}#
                                                       #f
                                                       #f))))))
                                           (#{build-dispatch-call\ 5777}#
                                             #{pvars\ 5917}#
                                             #{exp\ 5906}#
                                             #{y\ 5932}#
                                             #{r\ 5903}#
                                             #{mod\ 5907}#)
                                           (#{gen-syntax-case\ 5781}#
                                             #{x\ 5900}#
                                             #{keys\ 5901}#
                                             #{clauses\ 5902}#
                                             #{r\ 5903}#
                                             #{mod\ 5907}#)))))
                                   (list (if (eq? #{p\ 5916}# (quote any))
                                           (#{build-application\ 2420}#
                                             #f
                                             (#{build-primref\ 2444}#
                                               #f
                                               'list)
                                             (list #{x\ 5900}#))
                                           (#{build-application\ 2420}#
                                             #f
                                             (#{build-primref\ 2444}#
                                               #f
                                               '$sc-dispatch)
                                             (list #{x\ 5900}#
                                                   (#{build-data\ 2446}#
                                                     #f
                                                     #{p\ 5916}#))))))))))))))
                 (#{gen-syntax-case\ 5781}#
                   (lambda (#{x\ 5946}#
                            #{keys\ 5947}#
                            #{clauses\ 5948}#
                            #{r\ 5949}#
                            #{mod\ 5950}#)
                     (if (null? #{clauses\ 5948}#)
                       (#{build-application\ 2420}#
                         #f
                         (#{build-primref\ 2444}#
                           #f
                           'syntax-violation)
                         (list (#{build-data\ 2446}# #f #f)
                               (#{build-data\ 2446}#
                                 #f
                                 "source expression failed to match any pattern")
                               #{x\ 5946}#))
                       (let ((#{tmp\ 5960}# (car #{clauses\ 5948}#)))
                         (let ((#{tmp\ 5961}#
                                 ($sc-dispatch
                                   #{tmp\ 5960}#
                                   '(any any))))
                           (if #{tmp\ 5961}#
                             (@apply
                               (lambda (#{pat\ 5964}# #{exp\ 5965}#)
                                 (if (if (#{id?\ 2496}# #{pat\ 5964}#)
                                       (and-map
                                         (lambda (#{x\ 5968}#)
                                           (not (#{free-id=?\ 2555}#
                                                  #{pat\ 5964}#
                                                  #{x\ 5968}#)))
                                         (cons '#(syntax-object
                                                  ...
                                                  ((top)
                                                   #(ribcage
                                                     #(pat exp)
                                                     #((top) (top))
                                                     #("i5962" "i5963"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x keys clauses r mod)
                                                     #((top)
                                                       (top)
                                                       (top)
                                                       (top)
                                                       (top))
                                                     #("i5951"
                                                       "i5952"
                                                       "i5953"
                                                       "i5954"
                                                       "i5955"))
                                                   #(ribcage
                                                     (gen-syntax-case
                                                       gen-clause
                                                       build-dispatch-call
                                                       convert-pattern)
                                                     ((top) (top) (top) (top))
                                                     ("i5780"
                                                      "i5778"
                                                      "i5776"
                                                      "i5774"))
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
                                                       gensym-hook
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
                                                      (top))
                                                     ("i2610"
                                                      "i2608"
                                                      "i2606"
                                                      "i2604"
                                                      "i2602"
                                                      "i2600"
                                                      "i2598"
                                                      "i2596"
                                                      "i2594"
                                                      "i2592"
                                                      "i2590"
                                                      "i2588"
                                                      "i2586"
                                                      "i2584"
                                                      "i2582"
                                                      "i2580"
                                                      "i2578"
                                                      "i2576"
                                                      "i2574"
                                                      "i2572"
                                                      "i2570"
                                                      "i2568"
                                                      "i2566"
                                                      "i2564"
                                                      "i2562"
                                                      "i2560"
                                                      "i2558"
                                                      "i2556"
                                                      "i2554"
                                                      "i2552"
                                                      "i2550"
                                                      "i2548"
                                                      "i2546"
                                                      "i2544"
                                                      "i2542"
                                                      "i2540"
                                                      "i2539"
                                                      "i2538"
                                                      "i2536"
                                                      "i2535"
                                                      "i2534"
                                                      "i2533"
                                                      "i2532"
                                                      "i2530"
                                                      "i2528"
                                                      "i2526"
                                                      "i2524"
                                                      "i2522"
                                                      "i2520"
                                                      "i2518"
                                                      "i2516"
                                                      "i2513"
                                                      "i2511"
                                                      "i2510"
                                                      "i2509"
                                                      "i2508"
                                                      "i2507"
                                                      "i2506"
                                                      "i2504"
                                                      "i2502"
                                                      "i2500"
                                                      "i2498"
                                                      "i2497"
                                                      "i2495"
                                                      "i2493"
                                                      "i2491"
                                                      "i2489"
                                                      "i2487"
                                                      "i2485"
                                                      "i2483"
                                                      "i2482"
                                                      "i2480"
                                                      "i2478"
                                                      "i2477"
                                                      "i2476"
                                                      "i2474"
                                                      "i2473"
                                                      "i2471"
                                                      "i2469"
                                                      "i2467"
                                                      "i2465"
                                                      "i2463"
                                                      "i2461"
                                                      "i2459"
                                                      "i2457"
                                                      "i2455"
                                                      "i2453"
                                                      "i2451"
                                                      "i2449"
                                                      "i2447"
                                                      "i2445"
                                                      "i2443"
                                                      "i2441"
                                                      "i2439"
                                                      "i2437"
                                                      "i2435"
                                                      "i2433"
                                                      "i2431"
                                                      "i2429"
                                                      "i2427"
                                                      "i2425"
                                                      "i2423"
                                                      "i2421"
                                                      "i2419"
                                                      "i2417"
                                                      "i2415"
                                                      "i2413"
                                                      "i2411"
                                                      "i2409"
                                                      "i2408"
                                                      "i2406"
                                                      "i2404"
                                                      "i2402"
                                                      "i2400"
                                                      "i2398"
                                                      "i2396"
                                                      "i2394"
                                                      "i2392"
                                                      "i2390"
                                                      "i2387"
                                                      "i2385"
                                                      "i2383"
                                                      "i2381"
                                                      "i2379"
                                                      "i2377"
                                                      "i2375"
                                                      "i2373"
                                                      "i2371"
                                                      "i2369"
                                                      "i2367"
                                                      "i2365"
                                                      "i2363"
                                                      "i2361"
                                                      "i2359"
                                                      "i2357"
                                                      "i2355"
                                                      "i2353"))
                                                   #(ribcage
                                                     (define-structure
                                                       define-expansion-accessors
                                                       define-expansion-constructors
                                                       and-map*)
                                                     ((top) (top) (top) (top))
                                                     ("i2147"
                                                      "i2146"
                                                      "i2145"
                                                      "i2143")))
                                                  (hygiene guile))
                                               #{keys\ 5947}#))
                                       #f)
                                   (begin
                                     (let ((#{labels\ 5972}#
                                             (list (#{gen-label\ 2512}#)))
                                           (#{var\ 5973}#
                                             (#{gen-var\ 2609}#
                                               #{pat\ 5964}#)))
                                       (#{build-application\ 2420}#
                                         #f
                                         (#{build-simple-lambda\ 2438}#
                                           #f
                                           (list (syntax->datum #{pat\ 5964}#))
                                           #f
                                           (list #{var\ 5973}#)
                                           '()
                                           (#{chi\ 2581}#
                                             #{exp\ 5965}#
                                             (#{extend-env\ 2484}#
                                               #{labels\ 5972}#
                                               (list (cons 'syntax
                                                           (cons #{var\ 5973}#
                                                                 0)))
                                               #{r\ 5949}#)
                                             (#{make-binding-wrap\ 2543}#
                                               (list #{pat\ 5964}#)
                                               #{labels\ 5972}#
                                               '(()))
                                             #{mod\ 5950}#))
                                         (list #{x\ 5946}#))))
                                   (#{gen-clause\ 5779}#
                                     #{x\ 5946}#
                                     #{keys\ 5947}#
                                     (cdr #{clauses\ 5948}#)
                                     #{r\ 5949}#
                                     #{pat\ 5964}#
                                     #t
                                     #{exp\ 5965}#
                                     #{mod\ 5950}#)))
                               #{tmp\ 5961}#)
                             (let ((#{tmp\ 5979}#
                                     ($sc-dispatch
                                       #{tmp\ 5960}#
                                       '(any any any))))
                               (if #{tmp\ 5979}#
                                 (@apply
                                   (lambda (#{pat\ 5983}#
                                            #{fender\ 5984}#
                                            #{exp\ 5985}#)
                                     (#{gen-clause\ 5779}#
                                       #{x\ 5946}#
                                       #{keys\ 5947}#
                                       (cdr #{clauses\ 5948}#)
                                       #{r\ 5949}#
                                       #{pat\ 5983}#
                                       #{fender\ 5984}#
                                       #{exp\ 5985}#
                                       #{mod\ 5950}#))
                                   #{tmp\ 5979}#)
                                 (let ((#{_\ 5987}# #{tmp\ 5960}#))
                                   (syntax-violation
                                     'syntax-case
                                     "invalid clause"
                                     (car #{clauses\ 5948}#))))))))))))
                (begin
                  (lambda (#{e\ 5988}#
                           #{r\ 5989}#
                           #{w\ 5990}#
                           #{s\ 5991}#
                           #{mod\ 5992}#)
                    (begin
                      (let ((#{e\ 5999}#
                              (#{source-wrap\ 2567}#
                                #{e\ 5988}#
                                #{w\ 5990}#
                                #{s\ 5991}#
                                #{mod\ 5992}#)))
                        (let ((#{tmp\ 6000}# #{e\ 5999}#))
                          (let ((#{tmp\ 6001}#
                                  ($sc-dispatch
                                    #{tmp\ 6000}#
                                    '(any any each-any . each-any))))
                            (if #{tmp\ 6001}#
                              (@apply
                                (lambda (#{_\ 6006}#
                                         #{val\ 6007}#
                                         #{key\ 6008}#
                                         #{m\ 6009}#)
                                  (if (and-map
                                        (lambda (#{x\ 6010}#)
                                          (if (#{id?\ 2496}# #{x\ 6010}#)
                                            (not (#{ellipsis?\ 2597}#
                                                   #{x\ 6010}#))
                                            #f))
                                        #{key\ 6008}#)
                                    (begin
                                      (let ((#{x\ 6016}#
                                              (#{gen-var\ 2609}# (quote tmp))))
                                        (#{build-application\ 2420}#
                                          #{s\ 5991}#
                                          (#{build-simple-lambda\ 2438}#
                                            #f
                                            (list (quote tmp))
                                            #f
                                            (list #{x\ 6016}#)
                                            '()
                                            (#{gen-syntax-case\ 5781}#
                                              (#{build-lexical-reference\ 2426}#
                                                'value
                                                #f
                                                'tmp
                                                #{x\ 6016}#)
                                              #{key\ 6008}#
                                              #{m\ 6009}#
                                              #{r\ 5989}#
                                              #{mod\ 5992}#))
                                          (list (#{chi\ 2581}#
                                                  #{val\ 6007}#
                                                  #{r\ 5989}#
                                                  '(())
                                                  #{mod\ 5992}#)))))
                                    (syntax-violation
                                      'syntax-case
                                      "invalid literals list"
                                      #{e\ 5999}#)))
                                #{tmp\ 6001}#)
                              (syntax-violation
                                #f
                                "source expression failed to match any pattern"
                                #{tmp\ 6000}#))))))))))
            (set! macroexpand
              (lambda*
                (#{x\ 6022}#
                  #:optional
                  (#{m\ 6024}# (quote e))
                  (#{esew\ 6026}# (quote (eval))))
                (#{chi-top\ 2579}#
                  #{x\ 6022}#
                  '()
                  '((top))
                  #{m\ 6024}#
                  #{esew\ 6026}#
                  (cons 'hygiene
                        (module-name (current-module))))))
            (set! identifier?
              (lambda (#{x\ 6030}#)
                (#{nonsymbol-id?\ 2494}# #{x\ 6030}#)))
            (set! datum->syntax
              (lambda (#{id\ 6032}# #{datum\ 6033}#)
                (#{make-syntax-object\ 2458}#
                  #{datum\ 6033}#
                  (#{syntax-object-wrap\ 2464}# #{id\ 6032}#)
                  (#{syntax-object-module\ 2466}# #{id\ 6032}#))))
            (set! syntax->datum
              (lambda (#{x\ 6036}#)
                (#{strip\ 2607}# #{x\ 6036}# (quote (())))))
            (set! syntax-source
              (lambda (#{x\ 6039}#)
                (#{source-annotation\ 2475}# #{x\ 6039}#)))
            (set! generate-temporaries
              (lambda (#{ls\ 6041}#)
                (begin
                  (begin
                    (let ((#{x\ 6045}# #{ls\ 6041}#))
                      (if (not (list? #{x\ 6045}#))
                        (syntax-violation
                          'generate-temporaries
                          "invalid argument"
                          #{x\ 6045}#))))
                  (map (lambda (#{x\ 6046}#)
                         (#{wrap\ 2565}# (gensym) (quote ((top))) #f))
                       #{ls\ 6041}#))))
            (set! free-identifier=?
              (lambda (#{x\ 6050}# #{y\ 6051}#)
                (begin
                  (begin
                    (let ((#{x\ 6056}# #{x\ 6050}#))
                      (if (not (#{nonsymbol-id?\ 2494}# #{x\ 6056}#))
                        (syntax-violation
                          'free-identifier=?
                          "invalid argument"
                          #{x\ 6056}#))))
                  (begin
                    (let ((#{x\ 6059}# #{y\ 6051}#))
                      (if (not (#{nonsymbol-id?\ 2494}# #{x\ 6059}#))
                        (syntax-violation
                          'free-identifier=?
                          "invalid argument"
                          #{x\ 6059}#))))
                  (#{free-id=?\ 2555}# #{x\ 6050}# #{y\ 6051}#))))
            (set! bound-identifier=?
              (lambda (#{x\ 6060}# #{y\ 6061}#)
                (begin
                  (begin
                    (let ((#{x\ 6066}# #{x\ 6060}#))
                      (if (not (#{nonsymbol-id?\ 2494}# #{x\ 6066}#))
                        (syntax-violation
                          'bound-identifier=?
                          "invalid argument"
                          #{x\ 6066}#))))
                  (begin
                    (let ((#{x\ 6069}# #{y\ 6061}#))
                      (if (not (#{nonsymbol-id?\ 2494}# #{x\ 6069}#))
                        (syntax-violation
                          'bound-identifier=?
                          "invalid argument"
                          #{x\ 6069}#))))
                  (#{bound-id=?\ 2557}# #{x\ 6060}# #{y\ 6061}#))))
            (set! syntax-violation
              (lambda (#{who\ 6070}#
                       #{message\ 6071}#
                       #{form\ 6072}#
                       .
                       #{subform\ 6073}#)
                (begin
                  (begin
                    (let ((#{x\ 6080}# #{who\ 6070}#))
                      (if (not (let ((#{x\ 6081}# #{x\ 6080}#))
                                 (begin
                                   (let ((#{t\ 6085}# (not #{x\ 6081}#)))
                                     (if #{t\ 6085}#
                                       #{t\ 6085}#
                                       (begin
                                         (let ((#{t\ 6088}#
                                                 (string? #{x\ 6081}#)))
                                           (if #{t\ 6088}#
                                             #{t\ 6088}#
                                             (symbol? #{x\ 6081}#)))))))))
                        (syntax-violation
                          'syntax-violation
                          "invalid argument"
                          #{x\ 6080}#))))
                  (begin
                    (let ((#{x\ 6092}# #{message\ 6071}#))
                      (if (not (string? #{x\ 6092}#))
                        (syntax-violation
                          'syntax-violation
                          "invalid argument"
                          #{x\ 6092}#))))
                  (scm-error
                    'syntax-error
                    'macroexpand
                    (string-append
                      (if #{who\ 6070}# "~a: " "")
                      "~a "
                      (if (null? #{subform\ 6073}#)
                        "in ~a"
                        "in subform `~s' of `~s'"))
                    (begin
                      (let ((#{tail\ 6094}#
                              (cons #{message\ 6071}#
                                    (map (lambda (#{x\ 6095}#)
                                           (#{strip\ 2607}#
                                             #{x\ 6095}#
                                             '(())))
                                         (append
                                           #{subform\ 6073}#
                                           (list #{form\ 6072}#))))))
                        (if #{who\ 6070}#
                          (cons #{who\ 6070}# #{tail\ 6094}#)
                          #{tail\ 6094}#)))
                    #f))))
            (letrec*
              ((#{match-each\ 6099}#
                 (lambda (#{e\ 6112}#
                          #{p\ 6113}#
                          #{w\ 6114}#
                          #{mod\ 6115}#)
                   (if (pair? #{e\ 6112}#)
                     (begin
                       (let ((#{first\ 6123}#
                               (#{match\ 6111}#
                                 (car #{e\ 6112}#)
                                 #{p\ 6113}#
                                 #{w\ 6114}#
                                 '()
                                 #{mod\ 6115}#)))
                         (if #{first\ 6123}#
                           (begin
                             (let ((#{rest\ 6127}#
                                     (#{match-each\ 6099}#
                                       (cdr #{e\ 6112}#)
                                       #{p\ 6113}#
                                       #{w\ 6114}#
                                       #{mod\ 6115}#)))
                               (if #{rest\ 6127}#
                                 (cons #{first\ 6123}# #{rest\ 6127}#)
                                 #f)))
                           #f)))
                     (if (null? #{e\ 6112}#)
                       '()
                       (if (#{syntax-object?\ 2460}# #{e\ 6112}#)
                         (#{match-each\ 6099}#
                           (#{syntax-object-expression\ 2462}# #{e\ 6112}#)
                           #{p\ 6113}#
                           (#{join-wraps\ 2547}#
                             #{w\ 6114}#
                             (#{syntax-object-wrap\ 2464}# #{e\ 6112}#))
                           (#{syntax-object-module\ 2466}# #{e\ 6112}#))
                         #f)))))
               (#{match-each+\ 6101}#
                 (lambda (#{e\ 6135}#
                          #{x-pat\ 6136}#
                          #{y-pat\ 6137}#
                          #{z-pat\ 6138}#
                          #{w\ 6139}#
                          #{r\ 6140}#
                          #{mod\ 6141}#)
                   (letrec*
                     ((#{f\ 6152}#
                        (lambda (#{e\ 6153}# #{w\ 6154}#)
                          (if (pair? #{e\ 6153}#)
                            (call-with-values
                              (lambda ()
                                (#{f\ 6152}# (cdr #{e\ 6153}#) #{w\ 6154}#))
                              (lambda (#{xr*\ 6157}#
                                       #{y-pat\ 6158}#
                                       #{r\ 6159}#)
                                (if #{r\ 6159}#
                                  (if (null? #{y-pat\ 6158}#)
                                    (begin
                                      (let ((#{xr\ 6164}#
                                              (#{match\ 6111}#
                                                (car #{e\ 6153}#)
                                                #{x-pat\ 6136}#
                                                #{w\ 6154}#
                                                '()
                                                #{mod\ 6141}#)))
                                        (if #{xr\ 6164}#
                                          (values
                                            (cons #{xr\ 6164}# #{xr*\ 6157}#)
                                            #{y-pat\ 6158}#
                                            #{r\ 6159}#)
                                          (values #f #f #f))))
                                    (values
                                      '()
                                      (cdr #{y-pat\ 6158}#)
                                      (#{match\ 6111}#
                                        (car #{e\ 6153}#)
                                        (car #{y-pat\ 6158}#)
                                        #{w\ 6154}#
                                        #{r\ 6159}#
                                        #{mod\ 6141}#)))
                                  (values #f #f #f))))
                            (if (#{syntax-object?\ 2460}# #{e\ 6153}#)
                              (#{f\ 6152}#
                                (#{syntax-object-expression\ 2462}#
                                  #{e\ 6153}#)
                                (#{join-wraps\ 2547}# #{w\ 6154}# #{e\ 6153}#))
                              (values
                                '()
                                #{y-pat\ 6137}#
                                (#{match\ 6111}#
                                  #{e\ 6153}#
                                  #{z-pat\ 6138}#
                                  #{w\ 6154}#
                                  #{r\ 6140}#
                                  #{mod\ 6141}#)))))))
                     (begin (#{f\ 6152}# #{e\ 6135}# #{w\ 6139}#)))))
               (#{match-each-any\ 6103}#
                 (lambda (#{e\ 6168}# #{w\ 6169}# #{mod\ 6170}#)
                   (if (pair? #{e\ 6168}#)
                     (begin
                       (let ((#{l\ 6177}#
                               (#{match-each-any\ 6103}#
                                 (cdr #{e\ 6168}#)
                                 #{w\ 6169}#
                                 #{mod\ 6170}#)))
                         (if #{l\ 6177}#
                           (cons (#{wrap\ 2565}#
                                   (car #{e\ 6168}#)
                                   #{w\ 6169}#
                                   #{mod\ 6170}#)
                                 #{l\ 6177}#)
                           #f)))
                     (if (null? #{e\ 6168}#)
                       '()
                       (if (#{syntax-object?\ 2460}# #{e\ 6168}#)
                         (#{match-each-any\ 6103}#
                           (#{syntax-object-expression\ 2462}# #{e\ 6168}#)
                           (#{join-wraps\ 2547}#
                             #{w\ 6169}#
                             (#{syntax-object-wrap\ 2464}# #{e\ 6168}#))
                           #{mod\ 6170}#)
                         #f)))))
               (#{match-empty\ 6105}#
                 (lambda (#{p\ 6185}# #{r\ 6186}#)
                   (if (null? #{p\ 6185}#)
                     #{r\ 6186}#
                     (if (eq? #{p\ 6185}# (quote any))
                       (cons (quote ()) #{r\ 6186}#)
                       (if (pair? #{p\ 6185}#)
                         (#{match-empty\ 6105}#
                           (car #{p\ 6185}#)
                           (#{match-empty\ 6105}#
                             (cdr #{p\ 6185}#)
                             #{r\ 6186}#))
                         (if (eq? #{p\ 6185}# (quote each-any))
                           (cons (quote ()) #{r\ 6186}#)
                           (begin
                             (let ((#{atom-key\ 6200}#
                                     (vector-ref #{p\ 6185}# 0)))
                               (if (eqv? #{atom-key\ 6200}# (quote each))
                                 (#{match-empty\ 6105}#
                                   (vector-ref #{p\ 6185}# 1)
                                   #{r\ 6186}#)
                                 (if (eqv? #{atom-key\ 6200}# (quote each+))
                                   (#{match-empty\ 6105}#
                                     (vector-ref #{p\ 6185}# 1)
                                     (#{match-empty\ 6105}#
                                       (reverse (vector-ref #{p\ 6185}# 2))
                                       (#{match-empty\ 6105}#
                                         (vector-ref #{p\ 6185}# 3)
                                         #{r\ 6186}#)))
                                   (if (if (eqv? #{atom-key\ 6200}#
                                                 'free-id)
                                         #t
                                         (eqv? #{atom-key\ 6200}#
                                               'atom))
                                     #{r\ 6186}#
                                     (if (eqv? #{atom-key\ 6200}#
                                               'vector)
                                       (#{match-empty\ 6105}#
                                         (vector-ref #{p\ 6185}# 1)
                                         #{r\ 6186}#)))))))))))))
               (#{combine\ 6107}#
                 (lambda (#{r*\ 6205}# #{r\ 6206}#)
                   (if (null? (car #{r*\ 6205}#))
                     #{r\ 6206}#
                     (cons (map car #{r*\ 6205}#)
                           (#{combine\ 6107}#
                             (map cdr #{r*\ 6205}#)
                             #{r\ 6206}#)))))
               (#{match*\ 6109}#
                 (lambda (#{e\ 6209}#
                          #{p\ 6210}#
                          #{w\ 6211}#
                          #{r\ 6212}#
                          #{mod\ 6213}#)
                   (if (null? #{p\ 6210}#)
                     (if (null? #{e\ 6209}#) #{r\ 6212}# #f)
                     (if (pair? #{p\ 6210}#)
                       (if (pair? #{e\ 6209}#)
                         (#{match\ 6111}#
                           (car #{e\ 6209}#)
                           (car #{p\ 6210}#)
                           #{w\ 6211}#
                           (#{match\ 6111}#
                             (cdr #{e\ 6209}#)
                             (cdr #{p\ 6210}#)
                             #{w\ 6211}#
                             #{r\ 6212}#
                             #{mod\ 6213}#)
                           #{mod\ 6213}#)
                         #f)
                       (if (eq? #{p\ 6210}# (quote each-any))
                         (begin
                           (let ((#{l\ 6230}#
                                   (#{match-each-any\ 6103}#
                                     #{e\ 6209}#
                                     #{w\ 6211}#
                                     #{mod\ 6213}#)))
                             (if #{l\ 6230}#
                               (cons #{l\ 6230}# #{r\ 6212}#)
                               #f)))
                         (begin
                           (let ((#{atom-key\ 6236}#
                                   (vector-ref #{p\ 6210}# 0)))
                             (if (eqv? #{atom-key\ 6236}# (quote each))
                               (if (null? #{e\ 6209}#)
                                 (#{match-empty\ 6105}#
                                   (vector-ref #{p\ 6210}# 1)
                                   #{r\ 6212}#)
                                 (begin
                                   (let ((#{l\ 6239}#
                                           (#{match-each\ 6099}#
                                             #{e\ 6209}#
                                             (vector-ref #{p\ 6210}# 1)
                                             #{w\ 6211}#
                                             #{mod\ 6213}#)))
                                     (if #{l\ 6239}#
                                       (letrec*
                                         ((#{collect\ 6244}#
                                            (lambda (#{l\ 6245}#)
                                              (if (null? (car #{l\ 6245}#))
                                                #{r\ 6212}#
                                                (cons (map car #{l\ 6245}#)
                                                      (#{collect\ 6244}#
                                                        (map cdr
                                                             #{l\ 6245}#)))))))
                                         (begin
                                           (#{collect\ 6244}# #{l\ 6239}#)))
                                       #f))))
                               (if (eqv? #{atom-key\ 6236}# (quote each+))
                                 (call-with-values
                                   (lambda ()
                                     (#{match-each+\ 6101}#
                                       #{e\ 6209}#
                                       (vector-ref #{p\ 6210}# 1)
                                       (vector-ref #{p\ 6210}# 2)
                                       (vector-ref #{p\ 6210}# 3)
                                       #{w\ 6211}#
                                       #{r\ 6212}#
                                       #{mod\ 6213}#))
                                   (lambda (#{xr*\ 6247}#
                                            #{y-pat\ 6248}#
                                            #{r\ 6249}#)
                                     (if #{r\ 6249}#
                                       (if (null? #{y-pat\ 6248}#)
                                         (if (null? #{xr*\ 6247}#)
                                           (#{match-empty\ 6105}#
                                             (vector-ref #{p\ 6210}# 1)
                                             #{r\ 6249}#)
                                           (#{combine\ 6107}#
                                             #{xr*\ 6247}#
                                             #{r\ 6249}#))
                                         #f)
                                       #f)))
                                 (if (eqv? #{atom-key\ 6236}# (quote free-id))
                                   (if (#{id?\ 2496}# #{e\ 6209}#)
                                     (if (#{free-id=?\ 2555}#
                                           (#{wrap\ 2565}#
                                             #{e\ 6209}#
                                             #{w\ 6211}#
                                             #{mod\ 6213}#)
                                           (vector-ref #{p\ 6210}# 1))
                                       #{r\ 6212}#
                                       #f)
                                     #f)
                                   (if (eqv? #{atom-key\ 6236}# (quote atom))
                                     (if (equal?
                                           (vector-ref #{p\ 6210}# 1)
                                           (#{strip\ 2607}#
                                             #{e\ 6209}#
                                             #{w\ 6211}#))
                                       #{r\ 6212}#
                                       #f)
                                     (if (eqv? #{atom-key\ 6236}#
                                               'vector)
                                       (if (vector? #{e\ 6209}#)
                                         (#{match\ 6111}#
                                           (vector->list #{e\ 6209}#)
                                           (vector-ref #{p\ 6210}# 1)
                                           #{w\ 6211}#
                                           #{r\ 6212}#
                                           #{mod\ 6213}#)
                                         #f)))))))))))))
               (#{match\ 6111}#
                 (lambda (#{e\ 6266}#
                          #{p\ 6267}#
                          #{w\ 6268}#
                          #{r\ 6269}#
                          #{mod\ 6270}#)
                   (if (not #{r\ 6269}#)
                     #f
                     (if (eq? #{p\ 6267}# (quote any))
                       (cons (#{wrap\ 2565}#
                               #{e\ 6266}#
                               #{w\ 6268}#
                               #{mod\ 6270}#)
                             #{r\ 6269}#)
                       (if (#{syntax-object?\ 2460}# #{e\ 6266}#)
                         (#{match*\ 6109}#
                           (#{syntax-object-expression\ 2462}# #{e\ 6266}#)
                           #{p\ 6267}#
                           (#{join-wraps\ 2547}#
                             #{w\ 6268}#
                             (#{syntax-object-wrap\ 2464}# #{e\ 6266}#))
                           #{r\ 6269}#
                           (#{syntax-object-module\ 2466}# #{e\ 6266}#))
                         (#{match*\ 6109}#
                           #{e\ 6266}#
                           #{p\ 6267}#
                           #{w\ 6268}#
                           #{r\ 6269}#
                           #{mod\ 6270}#)))))))
              (begin
                (set! $sc-dispatch
                  (lambda (#{e\ 6283}# #{p\ 6284}#)
                    (if (eq? #{p\ 6284}# (quote any))
                      (list #{e\ 6283}#)
                      (if (#{syntax-object?\ 2460}# #{e\ 6283}#)
                        (#{match*\ 6109}#
                          (#{syntax-object-expression\ 2462}# #{e\ 6283}#)
                          #{p\ 6284}#
                          (#{syntax-object-wrap\ 2464}# #{e\ 6283}#)
                          '()
                          (#{syntax-object-module\ 2466}# #{e\ 6283}#))
                        (#{match*\ 6109}#
                          #{e\ 6283}#
                          #{p\ 6284}#
                          '(())
                          '()
                          #f)))))))))))))

(define with-syntax
  (make-syntax-transformer
    'with-syntax
    'macro
    (lambda (#{x\ 6293}#)
      (let ((#{tmp\ 6295}# #{x\ 6293}#))
        (let ((#{tmp\ 6296}#
                ($sc-dispatch
                  #{tmp\ 6295}#
                  '(any () any . each-any))))
          (if #{tmp\ 6296}#
            (@apply
              (lambda (#{_\ 6300}# #{e1\ 6301}# #{e2\ 6302}#)
                (cons '#(syntax-object
                         begin
                         ((top)
                          #(ribcage
                            #(_ e1 e2)
                            #((top) (top) (top))
                            #("i6297" "i6298" "i6299"))
                          #(ribcage () () ())
                          #(ribcage #(x) #((top)) #("i6294")))
                         (hygiene guile))
                      (cons #{e1\ 6301}# #{e2\ 6302}#)))
              #{tmp\ 6296}#)
            (let ((#{tmp\ 6304}#
                    ($sc-dispatch
                      #{tmp\ 6295}#
                      '(any ((any any)) any . each-any))))
              (if #{tmp\ 6304}#
                (@apply
                  (lambda (#{_\ 6310}#
                           #{out\ 6311}#
                           #{in\ 6312}#
                           #{e1\ 6313}#
                           #{e2\ 6314}#)
                    (list '#(syntax-object
                             syntax-case
                             ((top)
                              #(ribcage
                                #(_ out in e1 e2)
                                #((top) (top) (top) (top) (top))
                                #("i6305" "i6306" "i6307" "i6308" "i6309"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i6294")))
                             (hygiene guile))
                          #{in\ 6312}#
                          '()
                          (list #{out\ 6311}#
                                (cons '#(syntax-object
                                         begin
                                         ((top)
                                          #(ribcage
                                            #(_ out in e1 e2)
                                            #((top) (top) (top) (top) (top))
                                            #("i6305"
                                              "i6306"
                                              "i6307"
                                              "i6308"
                                              "i6309"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i6294")))
                                         (hygiene guile))
                                      (cons #{e1\ 6313}# #{e2\ 6314}#)))))
                  #{tmp\ 6304}#)
                (let ((#{tmp\ 6316}#
                        ($sc-dispatch
                          #{tmp\ 6295}#
                          '(any #(each (any any)) any . each-any))))
                  (if #{tmp\ 6316}#
                    (@apply
                      (lambda (#{_\ 6322}#
                               #{out\ 6323}#
                               #{in\ 6324}#
                               #{e1\ 6325}#
                               #{e2\ 6326}#)
                        (list '#(syntax-object
                                 syntax-case
                                 ((top)
                                  #(ribcage
                                    #(_ out in e1 e2)
                                    #((top) (top) (top) (top) (top))
                                    #("i6317" "i6318" "i6319" "i6320" "i6321"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i6294")))
                                 (hygiene guile))
                              (cons '#(syntax-object
                                       list
                                       ((top)
                                        #(ribcage
                                          #(_ out in e1 e2)
                                          #((top) (top) (top) (top) (top))
                                          #("i6317"
                                            "i6318"
                                            "i6319"
                                            "i6320"
                                            "i6321"))
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i6294")))
                                       (hygiene guile))
                                    #{in\ 6324}#)
                              '()
                              (list #{out\ 6323}#
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
                                                #("i6317"
                                                  "i6318"
                                                  "i6319"
                                                  "i6320"
                                                  "i6321"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(x)
                                                #((top))
                                                #("i6294")))
                                             (hygiene guile))
                                          (cons #{e1\ 6325}# #{e2\ 6326}#)))))
                      #{tmp\ 6316}#)
                    (syntax-violation
                      #f
                      "source expression failed to match any pattern"
                      #{tmp\ 6295}#)))))))))))

(define syntax-rules
  (make-syntax-transformer
    'syntax-rules
    'macro
    (lambda (#{x\ 6330}#)
      (let ((#{tmp\ 6332}# #{x\ 6330}#))
        (let ((#{tmp\ 6333}#
                ($sc-dispatch
                  #{tmp\ 6332}#
                  '(any each-any . #(each ((any . any) any))))))
          (if #{tmp\ 6333}#
            (@apply
              (lambda (#{_\ 6339}#
                       #{k\ 6340}#
                       #{keyword\ 6341}#
                       #{pattern\ 6342}#
                       #{template\ 6343}#)
                (list '#(syntax-object
                         lambda
                         ((top)
                          #(ribcage
                            #(_ k keyword pattern template)
                            #((top) (top) (top) (top) (top))
                            #("i6334" "i6335" "i6336" "i6337" "i6338"))
                          #(ribcage () () ())
                          #(ribcage #(x) #((top)) #("i6331")))
                         (hygiene guile))
                      '(#(syntax-object
                          x
                          ((top)
                           #(ribcage
                             #(_ k keyword pattern template)
                             #((top) (top) (top) (top) (top))
                             #("i6334" "i6335" "i6336" "i6337" "i6338"))
                           #(ribcage () () ())
                           #(ribcage #(x) #((top)) #("i6331")))
                          (hygiene guile)))
                      (vector
                        '(#(syntax-object
                            macro-type
                            ((top)
                             #(ribcage
                               #(_ k keyword pattern template)
                               #((top) (top) (top) (top) (top))
                               #("i6334" "i6335" "i6336" "i6337" "i6338"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i6331")))
                            (hygiene guile))
                          .
                          #(syntax-object
                            syntax-rules
                            ((top)
                             #(ribcage
                               #(_ k keyword pattern template)
                               #((top) (top) (top) (top) (top))
                               #("i6334" "i6335" "i6336" "i6337" "i6338"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i6331")))
                            (hygiene guile)))
                        (cons '#(syntax-object
                                 patterns
                                 ((top)
                                  #(ribcage
                                    #(_ k keyword pattern template)
                                    #((top) (top) (top) (top) (top))
                                    #("i6334" "i6335" "i6336" "i6337" "i6338"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i6331")))
                                 (hygiene guile))
                              #{pattern\ 6342}#))
                      (cons '#(syntax-object
                               syntax-case
                               ((top)
                                #(ribcage
                                  #(_ k keyword pattern template)
                                  #((top) (top) (top) (top) (top))
                                  #("i6334" "i6335" "i6336" "i6337" "i6338"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i6331")))
                               (hygiene guile))
                            (cons '#(syntax-object
                                     x
                                     ((top)
                                      #(ribcage
                                        #(_ k keyword pattern template)
                                        #((top) (top) (top) (top) (top))
                                        #("i6334"
                                          "i6335"
                                          "i6336"
                                          "i6337"
                                          "i6338"))
                                      #(ribcage () () ())
                                      #(ribcage #(x) #((top)) #("i6331")))
                                     (hygiene guile))
                                  (cons #{k\ 6340}#
                                        (map (lambda (#{tmp\ 6347}#
                                                      #{tmp\ 6346}#)
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
                                                                 #("i6334"
                                                                   "i6335"
                                                                   "i6336"
                                                                   "i6337"
                                                                   "i6338"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i6331")))
                                                              (hygiene guile))
                                                           #{tmp\ 6346}#)
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
                                                                 #("i6334"
                                                                   "i6335"
                                                                   "i6336"
                                                                   "i6337"
                                                                   "i6338"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i6331")))
                                                              (hygiene guile))
                                                           #{tmp\ 6347}#)))
                                             #{template\ 6343}#
                                             #{pattern\ 6342}#))))))
              #{tmp\ 6333}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp\ 6332}#)))))))

(define let*
  (make-syntax-transformer
    'let*
    'macro
    (lambda (#{x\ 6348}#)
      (let ((#{tmp\ 6350}# #{x\ 6348}#))
        (let ((#{tmp\ 6351}#
                ($sc-dispatch
                  #{tmp\ 6350}#
                  '(any #(each (any any)) any . each-any))))
          (if (if #{tmp\ 6351}#
                (@apply
                  (lambda (#{let*\ 6357}#
                           #{x\ 6358}#
                           #{v\ 6359}#
                           #{e1\ 6360}#
                           #{e2\ 6361}#)
                    (and-map identifier? #{x\ 6358}#))
                  #{tmp\ 6351}#)
                #f)
            (@apply
              (lambda (#{let*\ 6368}#
                       #{x\ 6369}#
                       #{v\ 6370}#
                       #{e1\ 6371}#
                       #{e2\ 6372}#)
                (letrec*
                  ((#{f\ 6375}#
                     (lambda (#{bindings\ 6376}#)
                       (if (null? #{bindings\ 6376}#)
                         (cons '#(syntax-object
                                  let
                                  ((top)
                                   #(ribcage () () ())
                                   #(ribcage
                                     #(f bindings)
                                     #((top) (top))
                                     #("i6373" "i6374"))
                                   #(ribcage
                                     #(let* x v e1 e2)
                                     #((top) (top) (top) (top) (top))
                                     #("i6363"
                                       "i6364"
                                       "i6365"
                                       "i6366"
                                       "i6367"))
                                   #(ribcage () () ())
                                   #(ribcage #(x) #((top)) #("i6349")))
                                  (hygiene guile))
                               (cons '()
                                     (cons #{e1\ 6371}# #{e2\ 6372}#)))
                         (let ((#{tmp\ 6381}#
                                 (list (#{f\ 6375}# (cdr #{bindings\ 6376}#))
                                       (car #{bindings\ 6376}#))))
                           (let ((#{tmp\ 6382}#
                                   ($sc-dispatch
                                     #{tmp\ 6381}#
                                     '(any any))))
                             (if #{tmp\ 6382}#
                               (@apply
                                 (lambda (#{body\ 6385}# #{binding\ 6386}#)
                                   (list '#(syntax-object
                                            let
                                            ((top)
                                             #(ribcage
                                               #(body binding)
                                               #((top) (top))
                                               #("i6383" "i6384"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(f bindings)
                                               #((top) (top))
                                               #("i6373" "i6374"))
                                             #(ribcage
                                               #(let* x v e1 e2)
                                               #((top) (top) (top) (top) (top))
                                               #("i6363"
                                                 "i6364"
                                                 "i6365"
                                                 "i6366"
                                                 "i6367"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x)
                                               #((top))
                                               #("i6349")))
                                            (hygiene guile))
                                         (list #{binding\ 6386}#)
                                         #{body\ 6385}#))
                                 #{tmp\ 6382}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp\ 6381}#))))))))
                  (begin
                    (#{f\ 6375}# (map list #{x\ 6369}# #{v\ 6370}#)))))
              #{tmp\ 6351}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp\ 6350}#)))))))

(define do
  (make-syntax-transformer
    'do
    'macro
    (lambda (#{orig-x\ 6387}#)
      (let ((#{tmp\ 6389}# #{orig-x\ 6387}#))
        (let ((#{tmp\ 6390}#
                ($sc-dispatch
                  #{tmp\ 6389}#
                  '(any #(each (any any . any))
                        (any . each-any)
                        .
                        each-any))))
          (if #{tmp\ 6390}#
            (@apply
              (lambda (#{_\ 6398}#
                       #{var\ 6399}#
                       #{init\ 6400}#
                       #{step\ 6401}#
                       #{e0\ 6402}#
                       #{e1\ 6403}#
                       #{c\ 6404}#)
                (let ((#{tmp\ 6406}#
                        (map (lambda (#{v\ 6427}# #{s\ 6428}#)
                               (let ((#{tmp\ 6431}# #{s\ 6428}#))
                                 (let ((#{tmp\ 6432}#
                                         ($sc-dispatch
                                           #{tmp\ 6431}#
                                           '())))
                                   (if #{tmp\ 6432}#
                                     (@apply
                                       (lambda () #{v\ 6427}#)
                                       #{tmp\ 6432}#)
                                     (let ((#{tmp\ 6433}#
                                             ($sc-dispatch
                                               #{tmp\ 6431}#
                                               '(any))))
                                       (if #{tmp\ 6433}#
                                         (@apply
                                           (lambda (#{e\ 6435}#) #{e\ 6435}#)
                                           #{tmp\ 6433}#)
                                         (let ((#{_\ 6437}# #{tmp\ 6431}#))
                                           (syntax-violation
                                             'do
                                             "bad step expression"
                                             #{orig-x\ 6387}#
                                             #{s\ 6428}#))))))))
                             #{var\ 6399}#
                             #{step\ 6401}#)))
                  (let ((#{tmp\ 6407}#
                          ($sc-dispatch #{tmp\ 6406}# (quote each-any))))
                    (if #{tmp\ 6407}#
                      (@apply
                        (lambda (#{step\ 6409}#)
                          (let ((#{tmp\ 6410}# #{e1\ 6403}#))
                            (let ((#{tmp\ 6411}#
                                    ($sc-dispatch #{tmp\ 6410}# (quote ()))))
                              (if #{tmp\ 6411}#
                                (@apply
                                  (lambda ()
                                    (list '#(syntax-object
                                             let
                                             ((top)
                                              #(ribcage
                                                #(step)
                                                #((top))
                                                #("i6408"))
                                              #(ribcage
                                                #(_ var init step e0 e1 c)
                                                #((top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top))
                                                #("i6391"
                                                  "i6392"
                                                  "i6393"
                                                  "i6394"
                                                  "i6395"
                                                  "i6396"
                                                  "i6397"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(orig-x)
                                                #((top))
                                                #("i6388")))
                                             (hygiene guile))
                                          '#(syntax-object
                                             doloop
                                             ((top)
                                              #(ribcage
                                                #(step)
                                                #((top))
                                                #("i6408"))
                                              #(ribcage
                                                #(_ var init step e0 e1 c)
                                                #((top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top))
                                                #("i6391"
                                                  "i6392"
                                                  "i6393"
                                                  "i6394"
                                                  "i6395"
                                                  "i6396"
                                                  "i6397"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(orig-x)
                                                #((top))
                                                #("i6388")))
                                             (hygiene guile))
                                          (map list
                                               #{var\ 6399}#
                                               #{init\ 6400}#)
                                          (list '#(syntax-object
                                                   if
                                                   ((top)
                                                    #(ribcage
                                                      #(step)
                                                      #((top))
                                                      #("i6408"))
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
                                                      #("i6391"
                                                        "i6392"
                                                        "i6393"
                                                        "i6394"
                                                        "i6395"
                                                        "i6396"
                                                        "i6397"))
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(orig-x)
                                                      #((top))
                                                      #("i6388")))
                                                   (hygiene guile))
                                                (list '#(syntax-object
                                                         not
                                                         ((top)
                                                          #(ribcage
                                                            #(step)
                                                            #((top))
                                                            #("i6408"))
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
                                                            #("i6391"
                                                              "i6392"
                                                              "i6393"
                                                              "i6394"
                                                              "i6395"
                                                              "i6396"
                                                              "i6397"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(orig-x)
                                                            #((top))
                                                            #("i6388")))
                                                         (hygiene guile))
                                                      #{e0\ 6402}#)
                                                (cons '#(syntax-object
                                                         begin
                                                         ((top)
                                                          #(ribcage
                                                            #(step)
                                                            #((top))
                                                            #("i6408"))
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
                                                            #("i6391"
                                                              "i6392"
                                                              "i6393"
                                                              "i6394"
                                                              "i6395"
                                                              "i6396"
                                                              "i6397"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(orig-x)
                                                            #((top))
                                                            #("i6388")))
                                                         (hygiene guile))
                                                      (append
                                                        #{c\ 6404}#
                                                        (list (cons '#(syntax-object
                                                                       doloop
                                                                       ((top)
                                                                        #(ribcage
                                                                          #(step)
                                                                          #((top))
                                                                          #("i6408"))
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
                                                                          #("i6391"
                                                                            "i6392"
                                                                            "i6393"
                                                                            "i6394"
                                                                            "i6395"
                                                                            "i6396"
                                                                            "i6397"))
                                                                        #(ribcage
                                                                          ()
                                                                          ()
                                                                          ())
                                                                        #(ribcage
                                                                          #(orig-x)
                                                                          #((top))
                                                                          #("i6388")))
                                                                       (hygiene
                                                                         guile))
                                                                    #{step\ 6409}#)))))))
                                  #{tmp\ 6411}#)
                                (let ((#{tmp\ 6416}#
                                        ($sc-dispatch
                                          #{tmp\ 6410}#
                                          '(any . each-any))))
                                  (if #{tmp\ 6416}#
                                    (@apply
                                      (lambda (#{e1\ 6419}# #{e2\ 6420}#)
                                        (list '#(syntax-object
                                                 let
                                                 ((top)
                                                  #(ribcage
                                                    #(e1 e2)
                                                    #((top) (top))
                                                    #("i6417" "i6418"))
                                                  #(ribcage
                                                    #(step)
                                                    #((top))
                                                    #("i6408"))
                                                  #(ribcage
                                                    #(_ var init step e0 e1 c)
                                                    #((top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top))
                                                    #("i6391"
                                                      "i6392"
                                                      "i6393"
                                                      "i6394"
                                                      "i6395"
                                                      "i6396"
                                                      "i6397"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(orig-x)
                                                    #((top))
                                                    #("i6388")))
                                                 (hygiene guile))
                                              '#(syntax-object
                                                 doloop
                                                 ((top)
                                                  #(ribcage
                                                    #(e1 e2)
                                                    #((top) (top))
                                                    #("i6417" "i6418"))
                                                  #(ribcage
                                                    #(step)
                                                    #((top))
                                                    #("i6408"))
                                                  #(ribcage
                                                    #(_ var init step e0 e1 c)
                                                    #((top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top))
                                                    #("i6391"
                                                      "i6392"
                                                      "i6393"
                                                      "i6394"
                                                      "i6395"
                                                      "i6396"
                                                      "i6397"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(orig-x)
                                                    #((top))
                                                    #("i6388")))
                                                 (hygiene guile))
                                              (map list
                                                   #{var\ 6399}#
                                                   #{init\ 6400}#)
                                              (list '#(syntax-object
                                                       if
                                                       ((top)
                                                        #(ribcage
                                                          #(e1 e2)
                                                          #((top) (top))
                                                          #("i6417" "i6418"))
                                                        #(ribcage
                                                          #(step)
                                                          #((top))
                                                          #("i6408"))
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
                                                          #("i6391"
                                                            "i6392"
                                                            "i6393"
                                                            "i6394"
                                                            "i6395"
                                                            "i6396"
                                                            "i6397"))
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(orig-x)
                                                          #((top))
                                                          #("i6388")))
                                                       (hygiene guile))
                                                    #{e0\ 6402}#
                                                    (cons '#(syntax-object
                                                             begin
                                                             ((top)
                                                              #(ribcage
                                                                #(e1 e2)
                                                                #((top) (top))
                                                                #("i6417"
                                                                  "i6418"))
                                                              #(ribcage
                                                                #(step)
                                                                #((top))
                                                                #("i6408"))
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
                                                                #("i6391"
                                                                  "i6392"
                                                                  "i6393"
                                                                  "i6394"
                                                                  "i6395"
                                                                  "i6396"
                                                                  "i6397"))
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                #(orig-x)
                                                                #((top))
                                                                #("i6388")))
                                                             (hygiene guile))
                                                          (cons #{e1\ 6419}#
                                                                #{e2\ 6420}#))
                                                    (cons '#(syntax-object
                                                             begin
                                                             ((top)
                                                              #(ribcage
                                                                #(e1 e2)
                                                                #((top) (top))
                                                                #("i6417"
                                                                  "i6418"))
                                                              #(ribcage
                                                                #(step)
                                                                #((top))
                                                                #("i6408"))
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
                                                                #("i6391"
                                                                  "i6392"
                                                                  "i6393"
                                                                  "i6394"
                                                                  "i6395"
                                                                  "i6396"
                                                                  "i6397"))
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                #(orig-x)
                                                                #((top))
                                                                #("i6388")))
                                                             (hygiene guile))
                                                          (append
                                                            #{c\ 6404}#
                                                            (list (cons '#(syntax-object
                                                                           doloop
                                                                           ((top)
                                                                            #(ribcage
                                                                              #(e1
                                                                                e2)
                                                                              #((top)
                                                                                (top))
                                                                              #("i6417"
                                                                                "i6418"))
                                                                            #(ribcage
                                                                              #(step)
                                                                              #((top))
                                                                              #("i6408"))
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
                                                                              #("i6391"
                                                                                "i6392"
                                                                                "i6393"
                                                                                "i6394"
                                                                                "i6395"
                                                                                "i6396"
                                                                                "i6397"))
                                                                            #(ribcage
                                                                              ()
                                                                              ()
                                                                              ())
                                                                            #(ribcage
                                                                              #(orig-x)
                                                                              #((top))
                                                                              #("i6388")))
                                                                           (hygiene
                                                                             guile))
                                                                        #{step\ 6409}#)))))))
                                      #{tmp\ 6416}#)
                                    (syntax-violation
                                      #f
                                      "source expression failed to match any pattern"
                                      #{tmp\ 6410}#)))))))
                        #{tmp\ 6407}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp\ 6406}#)))))
              #{tmp\ 6390}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp\ 6389}#)))))))

(define quasiquote
  (make-syntax-transformer
    'quasiquote
    'macro
    (letrec*
      ((#{quasicons\ 6444}#
         (lambda (#{x\ 6448}# #{y\ 6449}#)
           (let ((#{tmp\ 6453}# (list #{x\ 6448}# #{y\ 6449}#)))
             (let ((#{tmp\ 6454}#
                     ($sc-dispatch #{tmp\ 6453}# (quote (any any)))))
               (if #{tmp\ 6454}#
                 (@apply
                   (lambda (#{x\ 6457}# #{y\ 6458}#)
                     (let ((#{tmp\ 6459}# #{y\ 6458}#))
                       (let ((#{tmp\ 6460}#
                               ($sc-dispatch
                                 #{tmp\ 6459}#
                                 '(#(free-id
                                     #(syntax-object
                                       quote
                                       ((top)
                                        #(ribcage
                                          #(x y)
                                          #((top) (top))
                                          #("i6455" "i6456"))
                                        #(ribcage () () ())
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(x y)
                                          #((top) (top))
                                          #("i6450" "i6451"))
                                        #(ribcage
                                          #(quasicons
                                            quasiappend
                                            quasivector
                                            quasi)
                                          #((top) (top) (top) (top))
                                          #("i6440" "i6441" "i6442" "i6443")))
                                       (hygiene guile)))
                                   any))))
                         (if #{tmp\ 6460}#
                           (@apply
                             (lambda (#{dy\ 6462}#)
                               (let ((#{tmp\ 6463}# #{x\ 6457}#))
                                 (let ((#{tmp\ 6464}#
                                         ($sc-dispatch
                                           #{tmp\ 6463}#
                                           '(#(free-id
                                               #(syntax-object
                                                 quote
                                                 ((top)
                                                  #(ribcage
                                                    #(dy)
                                                    #((top))
                                                    #("i6461"))
                                                  #(ribcage
                                                    #(x y)
                                                    #((top) (top))
                                                    #("i6455" "i6456"))
                                                  #(ribcage () () ())
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(x y)
                                                    #((top) (top))
                                                    #("i6450" "i6451"))
                                                  #(ribcage
                                                    #(quasicons
                                                      quasiappend
                                                      quasivector
                                                      quasi)
                                                    #((top) (top) (top) (top))
                                                    #("i6440"
                                                      "i6441"
                                                      "i6442"
                                                      "i6443")))
                                                 (hygiene guile)))
                                             any))))
                                   (if #{tmp\ 6464}#
                                     (@apply
                                       (lambda (#{dx\ 6466}#)
                                         (list '#(syntax-object
                                                  quote
                                                  ((top)
                                                   #(ribcage
                                                     #(dx)
                                                     #((top))
                                                     #("i6465"))
                                                   #(ribcage
                                                     #(dy)
                                                     #((top))
                                                     #("i6461"))
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i6455" "i6456"))
                                                   #(ribcage () () ())
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i6450" "i6451"))
                                                   #(ribcage
                                                     #(quasicons
                                                       quasiappend
                                                       quasivector
                                                       quasi)
                                                     #((top) (top) (top) (top))
                                                     #("i6440"
                                                       "i6441"
                                                       "i6442"
                                                       "i6443")))
                                                  (hygiene guile))
                                               (cons #{dx\ 6466}#
                                                     #{dy\ 6462}#)))
                                       #{tmp\ 6464}#)
                                     (let ((#{_\ 6468}# #{tmp\ 6463}#))
                                       (if (null? #{dy\ 6462}#)
                                         (list '#(syntax-object
                                                  list
                                                  ((top)
                                                   #(ribcage
                                                     #(_)
                                                     #((top))
                                                     #("i6467"))
                                                   #(ribcage
                                                     #(dy)
                                                     #((top))
                                                     #("i6461"))
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i6455" "i6456"))
                                                   #(ribcage () () ())
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i6450" "i6451"))
                                                   #(ribcage
                                                     #(quasicons
                                                       quasiappend
                                                       quasivector
                                                       quasi)
                                                     #((top) (top) (top) (top))
                                                     #("i6440"
                                                       "i6441"
                                                       "i6442"
                                                       "i6443")))
                                                  (hygiene guile))
                                               #{x\ 6457}#)
                                         (list '#(syntax-object
                                                  cons
                                                  ((top)
                                                   #(ribcage
                                                     #(_)
                                                     #((top))
                                                     #("i6467"))
                                                   #(ribcage
                                                     #(dy)
                                                     #((top))
                                                     #("i6461"))
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i6455" "i6456"))
                                                   #(ribcage () () ())
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i6450" "i6451"))
                                                   #(ribcage
                                                     #(quasicons
                                                       quasiappend
                                                       quasivector
                                                       quasi)
                                                     #((top) (top) (top) (top))
                                                     #("i6440"
                                                       "i6441"
                                                       "i6442"
                                                       "i6443")))
                                                  (hygiene guile))
                                               #{x\ 6457}#
                                               #{y\ 6458}#)))))))
                             #{tmp\ 6460}#)
                           (let ((#{tmp\ 6469}#
                                   ($sc-dispatch
                                     #{tmp\ 6459}#
                                     '(#(free-id
                                         #(syntax-object
                                           list
                                           ((top)
                                            #(ribcage
                                              #(x y)
                                              #((top) (top))
                                              #("i6455" "i6456"))
                                            #(ribcage () () ())
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(x y)
                                              #((top) (top))
                                              #("i6450" "i6451"))
                                            #(ribcage
                                              #(quasicons
                                                quasiappend
                                                quasivector
                                                quasi)
                                              #((top) (top) (top) (top))
                                              #("i6440"
                                                "i6441"
                                                "i6442"
                                                "i6443")))
                                           (hygiene guile)))
                                       .
                                       any))))
                             (if #{tmp\ 6469}#
                               (@apply
                                 (lambda (#{stuff\ 6471}#)
                                   (cons '#(syntax-object
                                            list
                                            ((top)
                                             #(ribcage
                                               #(stuff)
                                               #((top))
                                               #("i6470"))
                                             #(ribcage
                                               #(x y)
                                               #((top) (top))
                                               #("i6455" "i6456"))
                                             #(ribcage () () ())
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x y)
                                               #((top) (top))
                                               #("i6450" "i6451"))
                                             #(ribcage
                                               #(quasicons
                                                 quasiappend
                                                 quasivector
                                                 quasi)
                                               #((top) (top) (top) (top))
                                               #("i6440"
                                                 "i6441"
                                                 "i6442"
                                                 "i6443")))
                                            (hygiene guile))
                                         (cons #{x\ 6457}# #{stuff\ 6471}#)))
                                 #{tmp\ 6469}#)
                               (let ((#{else\ 6473}# #{tmp\ 6459}#))
                                 (list '#(syntax-object
                                          cons
                                          ((top)
                                           #(ribcage
                                             #(else)
                                             #((top))
                                             #("i6472"))
                                           #(ribcage
                                             #(x y)
                                             #((top) (top))
                                             #("i6455" "i6456"))
                                           #(ribcage () () ())
                                           #(ribcage () () ())
                                           #(ribcage
                                             #(x y)
                                             #((top) (top))
                                             #("i6450" "i6451"))
                                           #(ribcage
                                             #(quasicons
                                               quasiappend
                                               quasivector
                                               quasi)
                                             #((top) (top) (top) (top))
                                             #("i6440"
                                               "i6441"
                                               "i6442"
                                               "i6443")))
                                          (hygiene guile))
                                       #{x\ 6457}#
                                       #{y\ 6458}#))))))))
                   #{tmp\ 6454}#)
                 (syntax-violation
                   #f
                   "source expression failed to match any pattern"
                   #{tmp\ 6453}#))))))
       (#{quasiappend\ 6445}#
         (lambda (#{x\ 6474}# #{y\ 6475}#)
           (let ((#{tmp\ 6479}# (list #{x\ 6474}# #{y\ 6475}#)))
             (let ((#{tmp\ 6480}#
                     ($sc-dispatch #{tmp\ 6479}# (quote (any any)))))
               (if #{tmp\ 6480}#
                 (@apply
                   (lambda (#{x\ 6483}# #{y\ 6484}#)
                     (let ((#{tmp\ 6485}# #{y\ 6484}#))
                       (let ((#{tmp\ 6486}#
                               ($sc-dispatch
                                 #{tmp\ 6485}#
                                 '(#(free-id
                                     #(syntax-object
                                       quote
                                       ((top)
                                        #(ribcage
                                          #(x y)
                                          #((top) (top))
                                          #("i6481" "i6482"))
                                        #(ribcage () () ())
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(x y)
                                          #((top) (top))
                                          #("i6476" "i6477"))
                                        #(ribcage
                                          #(quasicons
                                            quasiappend
                                            quasivector
                                            quasi)
                                          #((top) (top) (top) (top))
                                          #("i6440" "i6441" "i6442" "i6443")))
                                       (hygiene guile)))
                                   ()))))
                         (if #{tmp\ 6486}#
                           (@apply (lambda () #{x\ 6483}#) #{tmp\ 6486}#)
                           (let ((#{_\ 6488}# #{tmp\ 6485}#))
                             (list '#(syntax-object
                                      append
                                      ((top)
                                       #(ribcage #(_) #((top)) #("i6487"))
                                       #(ribcage
                                         #(x y)
                                         #((top) (top))
                                         #("i6481" "i6482"))
                                       #(ribcage () () ())
                                       #(ribcage () () ())
                                       #(ribcage
                                         #(x y)
                                         #((top) (top))
                                         #("i6476" "i6477"))
                                       #(ribcage
                                         #(quasicons
                                           quasiappend
                                           quasivector
                                           quasi)
                                         #((top) (top) (top) (top))
                                         #("i6440" "i6441" "i6442" "i6443")))
                                      (hygiene guile))
                                   #{x\ 6483}#
                                   #{y\ 6484}#))))))
                   #{tmp\ 6480}#)
                 (syntax-violation
                   #f
                   "source expression failed to match any pattern"
                   #{tmp\ 6479}#))))))
       (#{quasivector\ 6446}#
         (lambda (#{x\ 6489}#)
           (let ((#{tmp\ 6492}# #{x\ 6489}#))
             (let ((#{x\ 6494}# #{tmp\ 6492}#))
               (let ((#{tmp\ 6495}# #{x\ 6494}#))
                 (let ((#{tmp\ 6496}#
                         ($sc-dispatch
                           #{tmp\ 6495}#
                           '(#(free-id
                               #(syntax-object
                                 quote
                                 ((top)
                                  #(ribcage #(x) #((top)) #("i6493"))
                                  #(ribcage () () ())
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i6490"))
                                  #(ribcage
                                    #(quasicons quasiappend quasivector quasi)
                                    #((top) (top) (top) (top))
                                    #("i6440" "i6441" "i6442" "i6443")))
                                 (hygiene guile)))
                             each-any))))
                   (if #{tmp\ 6496}#
                     (@apply
                       (lambda (#{x\ 6498}#)
                         (list '#(syntax-object
                                  quote
                                  ((top)
                                   #(ribcage #(x) #((top)) #("i6497"))
                                   #(ribcage #(x) #((top)) #("i6493"))
                                   #(ribcage () () ())
                                   #(ribcage () () ())
                                   #(ribcage #(x) #((top)) #("i6490"))
                                   #(ribcage
                                     #(quasicons quasiappend quasivector quasi)
                                     #((top) (top) (top) (top))
                                     #("i6440" "i6441" "i6442" "i6443")))
                                  (hygiene guile))
                               (list->vector #{x\ 6498}#)))
                       #{tmp\ 6496}#)
                     (let ((#{tmp\ 6500}#
                             ($sc-dispatch
                               #{tmp\ 6495}#
                               '(#(free-id
                                   #(syntax-object
                                     list
                                     ((top)
                                      #(ribcage #(x) #((top)) #("i6493"))
                                      #(ribcage () () ())
                                      #(ribcage () () ())
                                      #(ribcage #(x) #((top)) #("i6490"))
                                      #(ribcage
                                        #(quasicons
                                          quasiappend
                                          quasivector
                                          quasi)
                                        #((top) (top) (top) (top))
                                        #("i6440" "i6441" "i6442" "i6443")))
                                     (hygiene guile)))
                                 .
                                 each-any))))
                       (if #{tmp\ 6500}#
                         (@apply
                           (lambda (#{x\ 6502}#)
                             (cons '#(syntax-object
                                      vector
                                      ((top)
                                       #(ribcage #(x) #((top)) #("i6501"))
                                       #(ribcage #(x) #((top)) #("i6493"))
                                       #(ribcage () () ())
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i6490"))
                                       #(ribcage
                                         #(quasicons
                                           quasiappend
                                           quasivector
                                           quasi)
                                         #((top) (top) (top) (top))
                                         #("i6440" "i6441" "i6442" "i6443")))
                                      (hygiene guile))
                                   #{x\ 6502}#))
                           #{tmp\ 6500}#)
                         (let ((#{_\ 6505}# #{tmp\ 6495}#))
                           (list '#(syntax-object
                                    list->vector
                                    ((top)
                                     #(ribcage #(_) #((top)) #("i6504"))
                                     #(ribcage #(x) #((top)) #("i6493"))
                                     #(ribcage () () ())
                                     #(ribcage () () ())
                                     #(ribcage #(x) #((top)) #("i6490"))
                                     #(ribcage
                                       #(quasicons
                                         quasiappend
                                         quasivector
                                         quasi)
                                       #((top) (top) (top) (top))
                                       #("i6440" "i6441" "i6442" "i6443")))
                                    (hygiene guile))
                                 #{x\ 6494}#)))))))))))
       (#{quasi\ 6447}#
         (lambda (#{p\ 6506}# #{lev\ 6507}#)
           (let ((#{tmp\ 6510}# #{p\ 6506}#))
             (let ((#{tmp\ 6511}#
                     ($sc-dispatch
                       #{tmp\ 6510}#
                       '(#(free-id
                           #(syntax-object
                             unquote
                             ((top)
                              #(ribcage () () ())
                              #(ribcage
                                #(p lev)
                                #((top) (top))
                                #("i6508" "i6509"))
                              #(ribcage
                                #(quasicons quasiappend quasivector quasi)
                                #((top) (top) (top) (top))
                                #("i6440" "i6441" "i6442" "i6443")))
                             (hygiene guile)))
                         any))))
               (if #{tmp\ 6511}#
                 (@apply
                   (lambda (#{p\ 6513}#)
                     (if (= #{lev\ 6507}# 0)
                       #{p\ 6513}#
                       (#{quasicons\ 6444}#
                         '(#(syntax-object
                             quote
                             ((top)
                              #(ribcage #(p) #((top)) #("i6512"))
                              #(ribcage () () ())
                              #(ribcage
                                #(p lev)
                                #((top) (top))
                                #("i6508" "i6509"))
                              #(ribcage
                                #(quasicons quasiappend quasivector quasi)
                                #((top) (top) (top) (top))
                                #("i6440" "i6441" "i6442" "i6443")))
                             (hygiene guile))
                           #(syntax-object
                             unquote
                             ((top)
                              #(ribcage #(p) #((top)) #("i6512"))
                              #(ribcage () () ())
                              #(ribcage
                                #(p lev)
                                #((top) (top))
                                #("i6508" "i6509"))
                              #(ribcage
                                #(quasicons quasiappend quasivector quasi)
                                #((top) (top) (top) (top))
                                #("i6440" "i6441" "i6442" "i6443")))
                             (hygiene guile)))
                         (#{quasi\ 6447}#
                           (list #{p\ 6513}#)
                           (1- #{lev\ 6507}#)))))
                   #{tmp\ 6511}#)
                 (let ((#{tmp\ 6514}#
                         ($sc-dispatch
                           #{tmp\ 6510}#
                           '(#(free-id
                               #(syntax-object
                                 unquote
                                 ((top)
                                  #(ribcage () () ())
                                  #(ribcage
                                    #(p lev)
                                    #((top) (top))
                                    #("i6508" "i6509"))
                                  #(ribcage
                                    #(quasicons quasiappend quasivector quasi)
                                    #((top) (top) (top) (top))
                                    #("i6440" "i6441" "i6442" "i6443")))
                                 (hygiene guile)))
                             .
                             any))))
                   (if (if #{tmp\ 6514}#
                         (@apply
                           (lambda (#{args\ 6516}#) (= #{lev\ 6507}# 0))
                           #{tmp\ 6514}#)
                         #f)
                     (@apply
                       (lambda (#{args\ 6518}#)
                         (syntax-violation
                           'unquote
                           "unquote takes exactly one argument"
                           #{p\ 6506}#
                           (cons '#(syntax-object
                                    unquote
                                    ((top)
                                     #(ribcage #(args) #((top)) #("i6517"))
                                     #(ribcage () () ())
                                     #(ribcage
                                       #(p lev)
                                       #((top) (top))
                                       #("i6508" "i6509"))
                                     #(ribcage
                                       #(quasicons
                                         quasiappend
                                         quasivector
                                         quasi)
                                       #((top) (top) (top) (top))
                                       #("i6440" "i6441" "i6442" "i6443")))
                                    (hygiene guile))
                                 #{args\ 6518}#)))
                       #{tmp\ 6514}#)
                     (let ((#{tmp\ 6519}#
                             ($sc-dispatch
                               #{tmp\ 6510}#
                               '((#(free-id
                                    #(syntax-object
                                      unquote-splicing
                                      ((top)
                                       #(ribcage () () ())
                                       #(ribcage
                                         #(p lev)
                                         #((top) (top))
                                         #("i6508" "i6509"))
                                       #(ribcage
                                         #(quasicons
                                           quasiappend
                                           quasivector
                                           quasi)
                                         #((top) (top) (top) (top))
                                         #("i6440" "i6441" "i6442" "i6443")))
                                      (hygiene guile)))
                                  any)
                                 .
                                 any))))
                       (if #{tmp\ 6519}#
                         (@apply
                           (lambda (#{p\ 6522}# #{q\ 6523}#)
                             (if (= #{lev\ 6507}# 0)
                               (#{quasiappend\ 6445}#
                                 #{p\ 6522}#
                                 (#{quasi\ 6447}# #{q\ 6523}# #{lev\ 6507}#))
                               (#{quasicons\ 6444}#
                                 (#{quasicons\ 6444}#
                                   '(#(syntax-object
                                       quote
                                       ((top)
                                        #(ribcage
                                          #(p q)
                                          #((top) (top))
                                          #("i6520" "i6521"))
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(p lev)
                                          #((top) (top))
                                          #("i6508" "i6509"))
                                        #(ribcage
                                          #(quasicons
                                            quasiappend
                                            quasivector
                                            quasi)
                                          #((top) (top) (top) (top))
                                          #("i6440" "i6441" "i6442" "i6443")))
                                       (hygiene guile))
                                     #(syntax-object
                                       unquote-splicing
                                       ((top)
                                        #(ribcage
                                          #(p q)
                                          #((top) (top))
                                          #("i6520" "i6521"))
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(p lev)
                                          #((top) (top))
                                          #("i6508" "i6509"))
                                        #(ribcage
                                          #(quasicons
                                            quasiappend
                                            quasivector
                                            quasi)
                                          #((top) (top) (top) (top))
                                          #("i6440" "i6441" "i6442" "i6443")))
                                       (hygiene guile)))
                                   (#{quasi\ 6447}#
                                     (list #{p\ 6522}#)
                                     (1- #{lev\ 6507}#)))
                                 (#{quasi\ 6447}# #{q\ 6523}# #{lev\ 6507}#))))
                           #{tmp\ 6519}#)
                         (let ((#{tmp\ 6524}#
                                 ($sc-dispatch
                                   #{tmp\ 6510}#
                                   '((#(free-id
                                        #(syntax-object
                                          unquote-splicing
                                          ((top)
                                           #(ribcage () () ())
                                           #(ribcage
                                             #(p lev)
                                             #((top) (top))
                                             #("i6508" "i6509"))
                                           #(ribcage
                                             #(quasicons
                                               quasiappend
                                               quasivector
                                               quasi)
                                             #((top) (top) (top) (top))
                                             #("i6440"
                                               "i6441"
                                               "i6442"
                                               "i6443")))
                                          (hygiene guile)))
                                      .
                                      any)
                                     .
                                     any))))
                           (if (if #{tmp\ 6524}#
                                 (@apply
                                   (lambda (#{args\ 6527}# #{q\ 6528}#)
                                     (= #{lev\ 6507}# 0))
                                   #{tmp\ 6524}#)
                                 #f)
                             (@apply
                               (lambda (#{args\ 6531}# #{q\ 6532}#)
                                 (syntax-violation
                                   'unquote-splicing
                                   "unquote-splicing takes exactly one argument"
                                   #{p\ 6506}#
                                   (cons '#(syntax-object
                                            unquote-splicing
                                            ((top)
                                             #(ribcage
                                               #(args q)
                                               #((top) (top))
                                               #("i6529" "i6530"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(p lev)
                                               #((top) (top))
                                               #("i6508" "i6509"))
                                             #(ribcage
                                               #(quasicons
                                                 quasiappend
                                                 quasivector
                                                 quasi)
                                               #((top) (top) (top) (top))
                                               #("i6440"
                                                 "i6441"
                                                 "i6442"
                                                 "i6443")))
                                            (hygiene guile))
                                         #{args\ 6531}#)))
                               #{tmp\ 6524}#)
                             (let ((#{tmp\ 6533}#
                                     ($sc-dispatch
                                       #{tmp\ 6510}#
                                       '(#(free-id
                                           #(syntax-object
                                             quasiquote
                                             ((top)
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(p lev)
                                                #((top) (top))
                                                #("i6508" "i6509"))
                                              #(ribcage
                                                #(quasicons
                                                  quasiappend
                                                  quasivector
                                                  quasi)
                                                #((top) (top) (top) (top))
                                                #("i6440"
                                                  "i6441"
                                                  "i6442"
                                                  "i6443")))
                                             (hygiene guile)))
                                         any))))
                               (if #{tmp\ 6533}#
                                 (@apply
                                   (lambda (#{p\ 6535}#)
                                     (#{quasicons\ 6444}#
                                       '(#(syntax-object
                                           quote
                                           ((top)
                                            #(ribcage #(p) #((top)) #("i6534"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(p lev)
                                              #((top) (top))
                                              #("i6508" "i6509"))
                                            #(ribcage
                                              #(quasicons
                                                quasiappend
                                                quasivector
                                                quasi)
                                              #((top) (top) (top) (top))
                                              #("i6440"
                                                "i6441"
                                                "i6442"
                                                "i6443")))
                                           (hygiene guile))
                                         #(syntax-object
                                           quasiquote
                                           ((top)
                                            #(ribcage #(p) #((top)) #("i6534"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(p lev)
                                              #((top) (top))
                                              #("i6508" "i6509"))
                                            #(ribcage
                                              #(quasicons
                                                quasiappend
                                                quasivector
                                                quasi)
                                              #((top) (top) (top) (top))
                                              #("i6440"
                                                "i6441"
                                                "i6442"
                                                "i6443")))
                                           (hygiene guile)))
                                       (#{quasi\ 6447}#
                                         (list #{p\ 6535}#)
                                         (1+ #{lev\ 6507}#))))
                                   #{tmp\ 6533}#)
                                 (let ((#{tmp\ 6536}#
                                         ($sc-dispatch
                                           #{tmp\ 6510}#
                                           '(any . any))))
                                   (if #{tmp\ 6536}#
                                     (@apply
                                       (lambda (#{p\ 6539}# #{q\ 6540}#)
                                         (#{quasicons\ 6444}#
                                           (#{quasi\ 6447}#
                                             #{p\ 6539}#
                                             #{lev\ 6507}#)
                                           (#{quasi\ 6447}#
                                             #{q\ 6540}#
                                             #{lev\ 6507}#)))
                                       #{tmp\ 6536}#)
                                     (let ((#{tmp\ 6541}#
                                             ($sc-dispatch
                                               #{tmp\ 6510}#
                                               '#(vector each-any))))
                                       (if #{tmp\ 6541}#
                                         (@apply
                                           (lambda (#{x\ 6543}#)
                                             (#{quasivector\ 6446}#
                                               (#{quasi\ 6447}#
                                                 #{x\ 6543}#
                                                 #{lev\ 6507}#)))
                                           #{tmp\ 6541}#)
                                         (let ((#{p\ 6546}# #{tmp\ 6510}#))
                                           (list '#(syntax-object
                                                    quote
                                                    ((top)
                                                     #(ribcage
                                                       #(p)
                                                       #((top))
                                                       #("i6545"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(p lev)
                                                       #((top) (top))
                                                       #("i6508" "i6509"))
                                                     #(ribcage
                                                       #(quasicons
                                                         quasiappend
                                                         quasivector
                                                         quasi)
                                                       #((top)
                                                         (top)
                                                         (top)
                                                         (top))
                                                       #("i6440"
                                                         "i6441"
                                                         "i6442"
                                                         "i6443")))
                                                    (hygiene guile))
                                                 #{p\ 6546}#))))))))))))))))))))
      (begin
        (lambda (#{x\ 6547}#)
          (let ((#{tmp\ 6549}# #{x\ 6547}#))
            (let ((#{tmp\ 6550}#
                    ($sc-dispatch #{tmp\ 6549}# (quote (any any)))))
              (if #{tmp\ 6550}#
                (@apply
                  (lambda (#{_\ 6553}# #{e\ 6554}#)
                    (#{quasi\ 6447}# #{e\ 6554}# 0))
                  #{tmp\ 6550}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 6549}#)))))))))

(define include
  (make-syntax-transformer
    'include
    'macro
    (lambda (#{x\ 6555}#)
      (letrec*
        ((#{read-file\ 6558}#
           (lambda (#{fn\ 6559}# #{k\ 6560}#)
             (begin
               (let ((#{p\ 6564}# (open-input-file #{fn\ 6559}#)))
                 (letrec*
                   ((#{f\ 6568}#
                      (lambda (#{x\ 6569}# #{result\ 6570}#)
                        (if (eof-object? #{x\ 6569}#)
                          (begin
                            (close-input-port #{p\ 6564}#)
                            (reverse #{result\ 6570}#))
                          (#{f\ 6568}#
                            (read #{p\ 6564}#)
                            (cons (datum->syntax #{k\ 6560}# #{x\ 6569}#)
                                  #{result\ 6570}#))))))
                   (begin
                     (#{f\ 6568}# (read #{p\ 6564}#) (quote ())))))))))
        (begin
          (let ((#{tmp\ 6571}# #{x\ 6555}#))
            (let ((#{tmp\ 6572}#
                    ($sc-dispatch #{tmp\ 6571}# (quote (any any)))))
              (if #{tmp\ 6572}#
                (@apply
                  (lambda (#{k\ 6575}# #{filename\ 6576}#)
                    (begin
                      (let ((#{fn\ 6578}# (syntax->datum #{filename\ 6576}#)))
                        (let ((#{tmp\ 6580}#
                                (#{read-file\ 6558}#
                                  #{fn\ 6578}#
                                  #{filename\ 6576}#)))
                          (let ((#{tmp\ 6581}#
                                  ($sc-dispatch
                                    #{tmp\ 6580}#
                                    'each-any)))
                            (if #{tmp\ 6581}#
                              (@apply
                                (lambda (#{exp\ 6583}#)
                                  (cons '#(syntax-object
                                           begin
                                           ((top)
                                            #(ribcage
                                              #(exp)
                                              #((top))
                                              #("i6582"))
                                            #(ribcage () () ())
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(fn)
                                              #((top))
                                              #("i6577"))
                                            #(ribcage
                                              #(k filename)
                                              #((top) (top))
                                              #("i6573" "i6574"))
                                            #(ribcage
                                              (read-file)
                                              ((top))
                                              ("i6557"))
                                            #(ribcage
                                              #(x)
                                              #((top))
                                              #("i6556")))
                                           (hygiene guile))
                                        #{exp\ 6583}#))
                                #{tmp\ 6581}#)
                              (syntax-violation
                                #f
                                "source expression failed to match any pattern"
                                #{tmp\ 6580}#)))))))
                  #{tmp\ 6572}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 6571}#)))))))))

(define include-from-path
  (make-syntax-transformer
    'include-from-path
    'macro
    (lambda (#{x\ 6585}#)
      (let ((#{tmp\ 6587}# #{x\ 6585}#))
        (let ((#{tmp\ 6588}#
                ($sc-dispatch #{tmp\ 6587}# (quote (any any)))))
          (if #{tmp\ 6588}#
            (@apply
              (lambda (#{k\ 6591}# #{filename\ 6592}#)
                (begin
                  (let ((#{fn\ 6594}# (syntax->datum #{filename\ 6592}#)))
                    (let ((#{tmp\ 6596}#
                            (datum->syntax
                              #{filename\ 6592}#
                              (begin
                                (let ((#{t\ 6601}#
                                        (%search-load-path #{fn\ 6594}#)))
                                  (if #{t\ 6601}#
                                    #{t\ 6601}#
                                    (syntax-violation
                                      'include-from-path
                                      "file not found in path"
                                      #{x\ 6585}#
                                      #{filename\ 6592}#)))))))
                      (let ((#{fn\ 6598}# #{tmp\ 6596}#))
                        (list '#(syntax-object
                                 include
                                 ((top)
                                  #(ribcage #(fn) #((top)) #("i6597"))
                                  #(ribcage () () ())
                                  #(ribcage () () ())
                                  #(ribcage #(fn) #((top)) #("i6593"))
                                  #(ribcage
                                    #(k filename)
                                    #((top) (top))
                                    #("i6589" "i6590"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i6586")))
                                 (hygiene guile))
                              #{fn\ 6598}#))))))
              #{tmp\ 6588}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp\ 6587}#)))))))

(define unquote
  (make-syntax-transformer
    'unquote
    'macro
    (lambda (#{x\ 6603}#)
      (let ((#{tmp\ 6605}# #{x\ 6603}#))
        (let ((#{tmp\ 6606}#
                ($sc-dispatch #{tmp\ 6605}# (quote (any any)))))
          (if #{tmp\ 6606}#
            (@apply
              (lambda (#{_\ 6609}# #{e\ 6610}#)
                (syntax-violation
                  'unquote
                  "expression not valid outside of quasiquote"
                  #{x\ 6603}#))
              #{tmp\ 6606}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp\ 6605}#)))))))

(define unquote-splicing
  (make-syntax-transformer
    'unquote-splicing
    'macro
    (lambda (#{x\ 6611}#)
      (let ((#{tmp\ 6613}# #{x\ 6611}#))
        (let ((#{tmp\ 6614}#
                ($sc-dispatch #{tmp\ 6613}# (quote (any any)))))
          (if #{tmp\ 6614}#
            (@apply
              (lambda (#{_\ 6617}# #{e\ 6618}#)
                (syntax-violation
                  'unquote-splicing
                  "expression not valid outside of quasiquote"
                  #{x\ 6611}#))
              #{tmp\ 6614}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp\ 6613}#)))))))

(define case
  (make-syntax-transformer
    'case
    'macro
    (lambda (#{x\ 6619}#)
      (let ((#{tmp\ 6621}# #{x\ 6619}#))
        (let ((#{tmp\ 6622}#
                ($sc-dispatch
                  #{tmp\ 6621}#
                  '(any any any . each-any))))
          (if #{tmp\ 6622}#
            (@apply
              (lambda (#{_\ 6627}#
                       #{e\ 6628}#
                       #{m1\ 6629}#
                       #{m2\ 6630}#)
                (let ((#{tmp\ 6632}#
                        (letrec*
                          ((#{f\ 6638}#
                             (lambda (#{clause\ 6639}# #{clauses\ 6640}#)
                               (if (null? #{clauses\ 6640}#)
                                 (let ((#{tmp\ 6642}# #{clause\ 6639}#))
                                   (let ((#{tmp\ 6643}#
                                           ($sc-dispatch
                                             #{tmp\ 6642}#
                                             '(#(free-id
                                                 #(syntax-object
                                                   else
                                                   ((top)
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(f clause clauses)
                                                      #((top) (top) (top))
                                                      #("i6635"
                                                        "i6636"
                                                        "i6637"))
                                                    #(ribcage
                                                      #(_ e m1 m2)
                                                      #((top)
                                                        (top)
                                                        (top)
                                                        (top))
                                                      #("i6623"
                                                        "i6624"
                                                        "i6625"
                                                        "i6626"))
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(x)
                                                      #((top))
                                                      #("i6620")))
                                                   (hygiene guile)))
                                               any
                                               .
                                               each-any))))
                                     (if #{tmp\ 6643}#
                                       (@apply
                                         (lambda (#{e1\ 6646}# #{e2\ 6647}#)
                                           (cons '#(syntax-object
                                                    begin
                                                    ((top)
                                                     #(ribcage
                                                       #(e1 e2)
                                                       #((top) (top))
                                                       #("i6644" "i6645"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(f clause clauses)
                                                       #((top) (top) (top))
                                                       #("i6635"
                                                         "i6636"
                                                         "i6637"))
                                                     #(ribcage
                                                       #(_ e m1 m2)
                                                       #((top)
                                                         (top)
                                                         (top)
                                                         (top))
                                                       #("i6623"
                                                         "i6624"
                                                         "i6625"
                                                         "i6626"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(x)
                                                       #((top))
                                                       #("i6620")))
                                                    (hygiene guile))
                                                 (cons #{e1\ 6646}#
                                                       #{e2\ 6647}#)))
                                         #{tmp\ 6643}#)
                                       (let ((#{tmp\ 6649}#
                                               ($sc-dispatch
                                                 #{tmp\ 6642}#
                                                 '(each-any any . each-any))))
                                         (if #{tmp\ 6649}#
                                           (@apply
                                             (lambda (#{k\ 6653}#
                                                      #{e1\ 6654}#
                                                      #{e2\ 6655}#)
                                               (list '#(syntax-object
                                                        if
                                                        ((top)
                                                         #(ribcage
                                                           #(k e1 e2)
                                                           #((top) (top) (top))
                                                           #("i6650"
                                                             "i6651"
                                                             "i6652"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(f clause clauses)
                                                           #((top) (top) (top))
                                                           #("i6635"
                                                             "i6636"
                                                             "i6637"))
                                                         #(ribcage
                                                           #(_ e m1 m2)
                                                           #((top)
                                                             (top)
                                                             (top)
                                                             (top))
                                                           #("i6623"
                                                             "i6624"
                                                             "i6625"
                                                             "i6626"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(x)
                                                           #((top))
                                                           #("i6620")))
                                                        (hygiene guile))
                                                     (list '#(syntax-object
                                                              memv
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i6650"
                                                                   "i6651"
                                                                   "i6652"))
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
                                                                 #("i6635"
                                                                   "i6636"
                                                                   "i6637"))
                                                               #(ribcage
                                                                 #(_ e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top)
                                                                   (top))
                                                                 #("i6623"
                                                                   "i6624"
                                                                   "i6625"
                                                                   "i6626"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i6620")))
                                                              (hygiene guile))
                                                           '#(syntax-object
                                                              t
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i6650"
                                                                   "i6651"
                                                                   "i6652"))
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
                                                                 #("i6635"
                                                                   "i6636"
                                                                   "i6637"))
                                                               #(ribcage
                                                                 #(_ e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top)
                                                                   (top))
                                                                 #("i6623"
                                                                   "i6624"
                                                                   "i6625"
                                                                   "i6626"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i6620")))
                                                              (hygiene guile))
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
                                                                       #("i6650"
                                                                         "i6651"
                                                                         "i6652"))
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
                                                                       #("i6635"
                                                                         "i6636"
                                                                         "i6637"))
                                                                     #(ribcage
                                                                       #(_
                                                                         e
                                                                         m1
                                                                         m2)
                                                                       #((top)
                                                                         (top)
                                                                         (top)
                                                                         (top))
                                                                       #("i6623"
                                                                         "i6624"
                                                                         "i6625"
                                                                         "i6626"))
                                                                     #(ribcage
                                                                       ()
                                                                       ()
                                                                       ())
                                                                     #(ribcage
                                                                       #(x)
                                                                       #((top))
                                                                       #("i6620")))
                                                                    (hygiene
                                                                      guile))
                                                                 #{k\ 6653}#))
                                                     (cons '#(syntax-object
                                                              begin
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i6650"
                                                                   "i6651"
                                                                   "i6652"))
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
                                                                 #("i6635"
                                                                   "i6636"
                                                                   "i6637"))
                                                               #(ribcage
                                                                 #(_ e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top)
                                                                   (top))
                                                                 #("i6623"
                                                                   "i6624"
                                                                   "i6625"
                                                                   "i6626"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i6620")))
                                                              (hygiene guile))
                                                           (cons #{e1\ 6654}#
                                                                 #{e2\ 6655}#))))
                                             #{tmp\ 6649}#)
                                           (let ((#{_\ 6659}# #{tmp\ 6642}#))
                                             (syntax-violation
                                               'case
                                               "bad clause"
                                               #{x\ 6619}#
                                               #{clause\ 6639}#)))))))
                                 (let ((#{tmp\ 6661}#
                                         (#{f\ 6638}#
                                           (car #{clauses\ 6640}#)
                                           (cdr #{clauses\ 6640}#))))
                                   (let ((#{rest\ 6663}# #{tmp\ 6661}#))
                                     (let ((#{tmp\ 6664}# #{clause\ 6639}#))
                                       (let ((#{tmp\ 6665}#
                                               ($sc-dispatch
                                                 #{tmp\ 6664}#
                                                 '(each-any any . each-any))))
                                         (if #{tmp\ 6665}#
                                           (@apply
                                             (lambda (#{k\ 6669}#
                                                      #{e1\ 6670}#
                                                      #{e2\ 6671}#)
                                               (list '#(syntax-object
                                                        if
                                                        ((top)
                                                         #(ribcage
                                                           #(k e1 e2)
                                                           #((top) (top) (top))
                                                           #("i6666"
                                                             "i6667"
                                                             "i6668"))
                                                         #(ribcage
                                                           #(rest)
                                                           #((top))
                                                           #("i6662"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(f clause clauses)
                                                           #((top) (top) (top))
                                                           #("i6635"
                                                             "i6636"
                                                             "i6637"))
                                                         #(ribcage
                                                           #(_ e m1 m2)
                                                           #((top)
                                                             (top)
                                                             (top)
                                                             (top))
                                                           #("i6623"
                                                             "i6624"
                                                             "i6625"
                                                             "i6626"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(x)
                                                           #((top))
                                                           #("i6620")))
                                                        (hygiene guile))
                                                     (list '#(syntax-object
                                                              memv
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i6666"
                                                                   "i6667"
                                                                   "i6668"))
                                                               #(ribcage
                                                                 #(rest)
                                                                 #((top))
                                                                 #("i6662"))
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
                                                                 #("i6635"
                                                                   "i6636"
                                                                   "i6637"))
                                                               #(ribcage
                                                                 #(_ e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top)
                                                                   (top))
                                                                 #("i6623"
                                                                   "i6624"
                                                                   "i6625"
                                                                   "i6626"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i6620")))
                                                              (hygiene guile))
                                                           '#(syntax-object
                                                              t
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i6666"
                                                                   "i6667"
                                                                   "i6668"))
                                                               #(ribcage
                                                                 #(rest)
                                                                 #((top))
                                                                 #("i6662"))
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
                                                                 #("i6635"
                                                                   "i6636"
                                                                   "i6637"))
                                                               #(ribcage
                                                                 #(_ e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top)
                                                                   (top))
                                                                 #("i6623"
                                                                   "i6624"
                                                                   "i6625"
                                                                   "i6626"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i6620")))
                                                              (hygiene guile))
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
                                                                       #("i6666"
                                                                         "i6667"
                                                                         "i6668"))
                                                                     #(ribcage
                                                                       #(rest)
                                                                       #((top))
                                                                       #("i6662"))
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
                                                                       #("i6635"
                                                                         "i6636"
                                                                         "i6637"))
                                                                     #(ribcage
                                                                       #(_
                                                                         e
                                                                         m1
                                                                         m2)
                                                                       #((top)
                                                                         (top)
                                                                         (top)
                                                                         (top))
                                                                       #("i6623"
                                                                         "i6624"
                                                                         "i6625"
                                                                         "i6626"))
                                                                     #(ribcage
                                                                       ()
                                                                       ()
                                                                       ())
                                                                     #(ribcage
                                                                       #(x)
                                                                       #((top))
                                                                       #("i6620")))
                                                                    (hygiene
                                                                      guile))
                                                                 #{k\ 6669}#))
                                                     (cons '#(syntax-object
                                                              begin
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i6666"
                                                                   "i6667"
                                                                   "i6668"))
                                                               #(ribcage
                                                                 #(rest)
                                                                 #((top))
                                                                 #("i6662"))
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
                                                                 #("i6635"
                                                                   "i6636"
                                                                   "i6637"))
                                                               #(ribcage
                                                                 #(_ e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top)
                                                                   (top))
                                                                 #("i6623"
                                                                   "i6624"
                                                                   "i6625"
                                                                   "i6626"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i6620")))
                                                              (hygiene guile))
                                                           (cons #{e1\ 6670}#
                                                                 #{e2\ 6671}#))
                                                     #{rest\ 6663}#))
                                             #{tmp\ 6665}#)
                                           (let ((#{_\ 6675}# #{tmp\ 6664}#))
                                             (syntax-violation
                                               'case
                                               "bad clause"
                                               #{x\ 6619}#
                                               #{clause\ 6639}#)))))))))))
                          (begin (#{f\ 6638}# #{m1\ 6629}# #{m2\ 6630}#)))))
                  (let ((#{body\ 6634}# #{tmp\ 6632}#))
                    (list '#(syntax-object
                             let
                             ((top)
                              #(ribcage #(body) #((top)) #("i6633"))
                              #(ribcage
                                #(_ e m1 m2)
                                #((top) (top) (top) (top))
                                #("i6623" "i6624" "i6625" "i6626"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i6620")))
                             (hygiene guile))
                          (list (list '#(syntax-object
                                         t
                                         ((top)
                                          #(ribcage
                                            #(body)
                                            #((top))
                                            #("i6633"))
                                          #(ribcage
                                            #(_ e m1 m2)
                                            #((top) (top) (top) (top))
                                            #("i6623" "i6624" "i6625" "i6626"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i6620")))
                                         (hygiene guile))
                                      #{e\ 6628}#))
                          #{body\ 6634}#))))
              #{tmp\ 6622}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp\ 6621}#)))))))

(define make-variable-transformer
  (lambda (#{proc\ 6676}#)
    (if (procedure? #{proc\ 6676}#)
      (begin
        (letrec*
          ((#{trans\ 6679}#
             (lambda (#{x\ 6680}#)
               (#{proc\ 6676}# #{x\ 6680}#))))
          (begin
            (set-procedure-property!
              #{trans\ 6679}#
              'variable-transformer
              #t)
            #{trans\ 6679}#)))
      (error "variable transformer not a procedure"
             #{proc\ 6676}#))))

(define identifier-syntax
  (make-syntax-transformer
    'identifier-syntax
    'macro
    (lambda (#{x\ 6682}#)
      (let ((#{tmp\ 6684}# #{x\ 6682}#))
        (let ((#{tmp\ 6685}#
                ($sc-dispatch #{tmp\ 6684}# (quote (any any)))))
          (if #{tmp\ 6685}#
            (@apply
              (lambda (#{_\ 6688}# #{e\ 6689}#)
                (list '#(syntax-object
                         lambda
                         ((top)
                          #(ribcage
                            #(_ e)
                            #((top) (top))
                            #("i6686" "i6687"))
                          #(ribcage () () ())
                          #(ribcage #(x) #((top)) #("i6683")))
                         (hygiene guile))
                      '(#(syntax-object
                          x
                          ((top)
                           #(ribcage
                             #(_ e)
                             #((top) (top))
                             #("i6686" "i6687"))
                           #(ribcage () () ())
                           #(ribcage #(x) #((top)) #("i6683")))
                          (hygiene guile)))
                      '#((#(syntax-object
                            macro-type
                            ((top)
                             #(ribcage
                               #(_ e)
                               #((top) (top))
                               #("i6686" "i6687"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i6683")))
                            (hygiene guile))
                          .
                          #(syntax-object
                            identifier-syntax
                            ((top)
                             #(ribcage
                               #(_ e)
                               #((top) (top))
                               #("i6686" "i6687"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i6683")))
                            (hygiene guile))))
                      (list '#(syntax-object
                               syntax-case
                               ((top)
                                #(ribcage
                                  #(_ e)
                                  #((top) (top))
                                  #("i6686" "i6687"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i6683")))
                               (hygiene guile))
                            '#(syntax-object
                               x
                               ((top)
                                #(ribcage
                                  #(_ e)
                                  #((top) (top))
                                  #("i6686" "i6687"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i6683")))
                               (hygiene guile))
                            '()
                            (list '#(syntax-object
                                     id
                                     ((top)
                                      #(ribcage
                                        #(_ e)
                                        #((top) (top))
                                        #("i6686" "i6687"))
                                      #(ribcage () () ())
                                      #(ribcage #(x) #((top)) #("i6683")))
                                     (hygiene guile))
                                  '(#(syntax-object
                                      identifier?
                                      ((top)
                                       #(ribcage
                                         #(_ e)
                                         #((top) (top))
                                         #("i6686" "i6687"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i6683")))
                                      (hygiene guile))
                                    (#(syntax-object
                                       syntax
                                       ((top)
                                        #(ribcage
                                          #(_ e)
                                          #((top) (top))
                                          #("i6686" "i6687"))
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i6683")))
                                       (hygiene guile))
                                     #(syntax-object
                                       id
                                       ((top)
                                        #(ribcage
                                          #(_ e)
                                          #((top) (top))
                                          #("i6686" "i6687"))
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i6683")))
                                       (hygiene guile))))
                                  (list '#(syntax-object
                                           syntax
                                           ((top)
                                            #(ribcage
                                              #(_ e)
                                              #((top) (top))
                                              #("i6686" "i6687"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(x)
                                              #((top))
                                              #("i6683")))
                                           (hygiene guile))
                                        #{e\ 6689}#))
                            (list (cons #{_\ 6688}#
                                        '(#(syntax-object
                                            x
                                            ((top)
                                             #(ribcage
                                               #(_ e)
                                               #((top) (top))
                                               #("i6686" "i6687"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x)
                                               #((top))
                                               #("i6683")))
                                            (hygiene guile))
                                          #(syntax-object
                                            ...
                                            ((top)
                                             #(ribcage
                                               #(_ e)
                                               #((top) (top))
                                               #("i6686" "i6687"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x)
                                               #((top))
                                               #("i6683")))
                                            (hygiene guile))))
                                  (list '#(syntax-object
                                           syntax
                                           ((top)
                                            #(ribcage
                                              #(_ e)
                                              #((top) (top))
                                              #("i6686" "i6687"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(x)
                                              #((top))
                                              #("i6683")))
                                           (hygiene guile))
                                        (cons #{e\ 6689}#
                                              '(#(syntax-object
                                                  x
                                                  ((top)
                                                   #(ribcage
                                                     #(_ e)
                                                     #((top) (top))
                                                     #("i6686" "i6687"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x)
                                                     #((top))
                                                     #("i6683")))
                                                  (hygiene guile))
                                                #(syntax-object
                                                  ...
                                                  ((top)
                                                   #(ribcage
                                                     #(_ e)
                                                     #((top) (top))
                                                     #("i6686" "i6687"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x)
                                                     #((top))
                                                     #("i6683")))
                                                  (hygiene guile)))))))))
              #{tmp\ 6685}#)
            (let ((#{tmp\ 6690}#
                    ($sc-dispatch
                      #{tmp\ 6684}#
                      '(any (any any)
                            ((#(free-id
                                #(syntax-object
                                  set!
                                  ((top)
                                   #(ribcage () () ())
                                   #(ribcage #(x) #((top)) #("i6683")))
                                  (hygiene guile)))
                              any
                              any)
                             any)))))
              (if (if #{tmp\ 6690}#
                    (@apply
                      (lambda (#{_\ 6697}#
                               #{id\ 6698}#
                               #{exp1\ 6699}#
                               #{var\ 6700}#
                               #{val\ 6701}#
                               #{exp2\ 6702}#)
                        (if (identifier? #{id\ 6698}#)
                          (identifier? #{var\ 6700}#)
                          #f))
                      #{tmp\ 6690}#)
                    #f)
                (@apply
                  (lambda (#{_\ 6711}#
                           #{id\ 6712}#
                           #{exp1\ 6713}#
                           #{var\ 6714}#
                           #{val\ 6715}#
                           #{exp2\ 6716}#)
                    (list '#(syntax-object
                             make-variable-transformer
                             ((top)
                              #(ribcage
                                #(_ id exp1 var val exp2)
                                #((top) (top) (top) (top) (top) (top))
                                #("i6705"
                                  "i6706"
                                  "i6707"
                                  "i6708"
                                  "i6709"
                                  "i6710"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i6683")))
                             (hygiene guile))
                          (list '#(syntax-object
                                   lambda
                                   ((top)
                                    #(ribcage
                                      #(_ id exp1 var val exp2)
                                      #((top) (top) (top) (top) (top) (top))
                                      #("i6705"
                                        "i6706"
                                        "i6707"
                                        "i6708"
                                        "i6709"
                                        "i6710"))
                                    #(ribcage () () ())
                                    #(ribcage #(x) #((top)) #("i6683")))
                                   (hygiene guile))
                                '(#(syntax-object
                                    x
                                    ((top)
                                     #(ribcage
                                       #(_ id exp1 var val exp2)
                                       #((top) (top) (top) (top) (top) (top))
                                       #("i6705"
                                         "i6706"
                                         "i6707"
                                         "i6708"
                                         "i6709"
                                         "i6710"))
                                     #(ribcage () () ())
                                     #(ribcage #(x) #((top)) #("i6683")))
                                    (hygiene guile)))
                                '#((#(syntax-object
                                      macro-type
                                      ((top)
                                       #(ribcage
                                         #(_ id exp1 var val exp2)
                                         #((top) (top) (top) (top) (top) (top))
                                         #("i6705"
                                           "i6706"
                                           "i6707"
                                           "i6708"
                                           "i6709"
                                           "i6710"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i6683")))
                                      (hygiene guile))
                                    .
                                    #(syntax-object
                                      variable-transformer
                                      ((top)
                                       #(ribcage
                                         #(_ id exp1 var val exp2)
                                         #((top) (top) (top) (top) (top) (top))
                                         #("i6705"
                                           "i6706"
                                           "i6707"
                                           "i6708"
                                           "i6709"
                                           "i6710"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i6683")))
                                      (hygiene guile))))
                                (list '#(syntax-object
                                         syntax-case
                                         ((top)
                                          #(ribcage
                                            #(_ id exp1 var val exp2)
                                            #((top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top))
                                            #("i6705"
                                              "i6706"
                                              "i6707"
                                              "i6708"
                                              "i6709"
                                              "i6710"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i6683")))
                                         (hygiene guile))
                                      '#(syntax-object
                                         x
                                         ((top)
                                          #(ribcage
                                            #(_ id exp1 var val exp2)
                                            #((top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top))
                                            #("i6705"
                                              "i6706"
                                              "i6707"
                                              "i6708"
                                              "i6709"
                                              "i6710"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i6683")))
                                         (hygiene guile))
                                      '(#(syntax-object
                                          set!
                                          ((top)
                                           #(ribcage
                                             #(_ id exp1 var val exp2)
                                             #((top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top))
                                             #("i6705"
                                               "i6706"
                                               "i6707"
                                               "i6708"
                                               "i6709"
                                               "i6710"))
                                           #(ribcage () () ())
                                           #(ribcage #(x) #((top)) #("i6683")))
                                          (hygiene guile)))
                                      (list (list '#(syntax-object
                                                     set!
                                                     ((top)
                                                      #(ribcage
                                                        #(_
                                                          id
                                                          exp1
                                                          var
                                                          val
                                                          exp2)
                                                        #((top)
                                                          (top)
                                                          (top)
                                                          (top)
                                                          (top)
                                                          (top))
                                                        #("i6705"
                                                          "i6706"
                                                          "i6707"
                                                          "i6708"
                                                          "i6709"
                                                          "i6710"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i6683")))
                                                     (hygiene guile))
                                                  #{var\ 6714}#
                                                  #{val\ 6715}#)
                                            (list '#(syntax-object
                                                     syntax
                                                     ((top)
                                                      #(ribcage
                                                        #(_
                                                          id
                                                          exp1
                                                          var
                                                          val
                                                          exp2)
                                                        #((top)
                                                          (top)
                                                          (top)
                                                          (top)
                                                          (top)
                                                          (top))
                                                        #("i6705"
                                                          "i6706"
                                                          "i6707"
                                                          "i6708"
                                                          "i6709"
                                                          "i6710"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i6683")))
                                                     (hygiene guile))
                                                  #{exp2\ 6716}#))
                                      (list (cons #{id\ 6712}#
                                                  '(#(syntax-object
                                                      x
                                                      ((top)
                                                       #(ribcage
                                                         #(_
                                                           id
                                                           exp1
                                                           var
                                                           val
                                                           exp2)
                                                         #((top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top))
                                                         #("i6705"
                                                           "i6706"
                                                           "i6707"
                                                           "i6708"
                                                           "i6709"
                                                           "i6710"))
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(x)
                                                         #((top))
                                                         #("i6683")))
                                                      (hygiene guile))
                                                    #(syntax-object
                                                      ...
                                                      ((top)
                                                       #(ribcage
                                                         #(_
                                                           id
                                                           exp1
                                                           var
                                                           val
                                                           exp2)
                                                         #((top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top))
                                                         #("i6705"
                                                           "i6706"
                                                           "i6707"
                                                           "i6708"
                                                           "i6709"
                                                           "i6710"))
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(x)
                                                         #((top))
                                                         #("i6683")))
                                                      (hygiene guile))))
                                            (list '#(syntax-object
                                                     syntax
                                                     ((top)
                                                      #(ribcage
                                                        #(_
                                                          id
                                                          exp1
                                                          var
                                                          val
                                                          exp2)
                                                        #((top)
                                                          (top)
                                                          (top)
                                                          (top)
                                                          (top)
                                                          (top))
                                                        #("i6705"
                                                          "i6706"
                                                          "i6707"
                                                          "i6708"
                                                          "i6709"
                                                          "i6710"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i6683")))
                                                     (hygiene guile))
                                                  (cons #{exp1\ 6713}#
                                                        '(#(syntax-object
                                                            x
                                                            ((top)
                                                             #(ribcage
                                                               #(_
                                                                 id
                                                                 exp1
                                                                 var
                                                                 val
                                                                 exp2)
                                                               #((top)
                                                                 (top)
                                                                 (top)
                                                                 (top)
                                                                 (top)
                                                                 (top))
                                                               #("i6705"
                                                                 "i6706"
                                                                 "i6707"
                                                                 "i6708"
                                                                 "i6709"
                                                                 "i6710"))
                                                             #(ribcage
                                                               ()
                                                               ()
                                                               ())
                                                             #(ribcage
                                                               #(x)
                                                               #((top))
                                                               #("i6683")))
                                                            (hygiene guile))
                                                          #(syntax-object
                                                            ...
                                                            ((top)
                                                             #(ribcage
                                                               #(_
                                                                 id
                                                                 exp1
                                                                 var
                                                                 val
                                                                 exp2)
                                                               #((top)
                                                                 (top)
                                                                 (top)
                                                                 (top)
                                                                 (top)
                                                                 (top))
                                                               #("i6705"
                                                                 "i6706"
                                                                 "i6707"
                                                                 "i6708"
                                                                 "i6709"
                                                                 "i6710"))
                                                             #(ribcage
                                                               ()
                                                               ()
                                                               ())
                                                             #(ribcage
                                                               #(x)
                                                               #((top))
                                                               #("i6683")))
                                                            (hygiene
                                                              guile))))))
                                      (list #{id\ 6712}#
                                            (list '#(syntax-object
                                                     identifier?
                                                     ((top)
                                                      #(ribcage
                                                        #(_
                                                          id
                                                          exp1
                                                          var
                                                          val
                                                          exp2)
                                                        #((top)
                                                          (top)
                                                          (top)
                                                          (top)
                                                          (top)
                                                          (top))
                                                        #("i6705"
                                                          "i6706"
                                                          "i6707"
                                                          "i6708"
                                                          "i6709"
                                                          "i6710"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i6683")))
                                                     (hygiene guile))
                                                  (list '#(syntax-object
                                                           syntax
                                                           ((top)
                                                            #(ribcage
                                                              #(_
                                                                id
                                                                exp1
                                                                var
                                                                val
                                                                exp2)
                                                              #((top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top))
                                                              #("i6705"
                                                                "i6706"
                                                                "i6707"
                                                                "i6708"
                                                                "i6709"
                                                                "i6710"))
                                                            #(ribcage () () ())
                                                            #(ribcage
                                                              #(x)
                                                              #((top))
                                                              #("i6683")))
                                                           (hygiene guile))
                                                        #{id\ 6712}#))
                                            (list '#(syntax-object
                                                     syntax
                                                     ((top)
                                                      #(ribcage
                                                        #(_
                                                          id
                                                          exp1
                                                          var
                                                          val
                                                          exp2)
                                                        #((top)
                                                          (top)
                                                          (top)
                                                          (top)
                                                          (top)
                                                          (top))
                                                        #("i6705"
                                                          "i6706"
                                                          "i6707"
                                                          "i6708"
                                                          "i6709"
                                                          "i6710"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i6683")))
                                                     (hygiene guile))
                                                  #{exp1\ 6713}#))))))
                  #{tmp\ 6690}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 6684}#)))))))))

(define define*
  (make-syntax-transformer
    'define*
    'macro
    (lambda (#{x\ 6717}#)
      (let ((#{tmp\ 6719}# #{x\ 6717}#))
        (let ((#{tmp\ 6720}#
                ($sc-dispatch
                  #{tmp\ 6719}#
                  '(any (any . any) any . each-any))))
          (if #{tmp\ 6720}#
            (@apply
              (lambda (#{_\ 6726}#
                       #{id\ 6727}#
                       #{args\ 6728}#
                       #{b0\ 6729}#
                       #{b1\ 6730}#)
                (list '#(syntax-object
                         define
                         ((top)
                          #(ribcage
                            #(_ id args b0 b1)
                            #((top) (top) (top) (top) (top))
                            #("i6721" "i6722" "i6723" "i6724" "i6725"))
                          #(ribcage () () ())
                          #(ribcage #(x) #((top)) #("i6718")))
                         (hygiene guile))
                      #{id\ 6727}#
                      (cons '#(syntax-object
                               lambda*
                               ((top)
                                #(ribcage
                                  #(_ id args b0 b1)
                                  #((top) (top) (top) (top) (top))
                                  #("i6721" "i6722" "i6723" "i6724" "i6725"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i6718")))
                               (hygiene guile))
                            (cons #{args\ 6728}#
                                  (cons #{b0\ 6729}# #{b1\ 6730}#)))))
              #{tmp\ 6720}#)
            (let ((#{tmp\ 6732}#
                    ($sc-dispatch
                      #{tmp\ 6719}#
                      '(any any any))))
              (if (if #{tmp\ 6732}#
                    (@apply
                      (lambda (#{_\ 6736}# #{id\ 6737}# #{val\ 6738}#)
                        (identifier?
                          '#(syntax-object
                             x
                             ((top)
                              #(ribcage
                                #(_ id val)
                                #((top) (top) (top))
                                #("i6733" "i6734" "i6735"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i6718")))
                             (hygiene guile))))
                      #{tmp\ 6732}#)
                    #f)
                (@apply
                  (lambda (#{_\ 6742}# #{id\ 6743}# #{val\ 6744}#)
                    (list '#(syntax-object
                             define
                             ((top)
                              #(ribcage
                                #(_ id val)
                                #((top) (top) (top))
                                #("i6739" "i6740" "i6741"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i6718")))
                             (hygiene guile))
                          #{id\ 6743}#
                          #{val\ 6744}#))
                  #{tmp\ 6732}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 6719}#)))))))))

