(eval-when (compile) (set-current-module (resolve-module (quote (guile)))))
(if #f #f)

(letrec*
  ((#{and-map*\ 35}#
     (lambda (#{f\ 207}# #{first\ 208}# . #{rest\ 209}#)
       (begin
         (let ((#{t\ 215}# (null? #{first\ 208}#)))
           (if #{t\ 215}#
             #{t\ 215}#
             (if (null? #{rest\ 209}#)
               (letrec*
                 ((#{andmap\ 219}#
                    (lambda (#{first\ 220}#)
                      (begin
                        (let ((#{x\ 223}# (car #{first\ 220}#))
                              (#{first\ 224}# (cdr #{first\ 220}#)))
                          (if (null? #{first\ 224}#)
                            (#{f\ 207}# #{x\ 223}#)
                            (if (#{f\ 207}# #{x\ 223}#)
                              (#{andmap\ 219}# #{first\ 224}#)
                              #f)))))))
                 (begin (#{andmap\ 219}# #{first\ 208}#)))
               (letrec*
                 ((#{andmap\ 230}#
                    (lambda (#{first\ 231}# #{rest\ 232}#)
                      (begin
                        (let ((#{x\ 237}# (car #{first\ 231}#))
                              (#{xr\ 238}# (map car #{rest\ 232}#))
                              (#{first\ 239}# (cdr #{first\ 231}#))
                              (#{rest\ 240}# (map cdr #{rest\ 232}#)))
                          (if (null? #{first\ 239}#)
                            (@apply #{f\ 207}# (cons #{x\ 237}# #{xr\ 238}#))
                            (if (@apply
                                  #{f\ 207}#
                                  (cons #{x\ 237}# #{xr\ 238}#))
                              (#{andmap\ 230}# #{first\ 239}# #{rest\ 240}#)
                              #f)))))))
                 (begin
                   (#{andmap\ 230}# #{first\ 208}# #{rest\ 209}#))))))))))
  (begin
    (let ((#{make-primitive-ref\ 249}# (if #f #f))
          (#{fx+\ 288}# (if #f #f))
          (#{fx-\ 290}# (if #f #f))
          (#{fx=\ 292}# (if #f #f))
          (#{fx<\ 294}# (if #f #f))
          (#{set-syntax-object-expression!\ 359}#
            (if #f #f))
          (#{set-syntax-object-wrap!\ 361}# (if #f #f))
          (#{set-syntax-object-module!\ 363}# (if #f #f))
          (#{binding-type\ 370}# (if #f #f))
          (#{binding-value\ 372}# (if #f #f))
          (#{make-wrap\ 392}# (if #f #f))
          (#{wrap-marks\ 394}# (if #f #f))
          (#{wrap-subst\ 396}# (if #f #f))
          (#{ribcage?\ 410}# (if #f #f)))
      (letrec*
        ((#{make-void\ 245}#
           (lambda (#{src\ 717}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 0)
               #{src\ 717}#)))
         (#{make-const\ 247}#
           (lambda (#{src\ 719}# #{exp\ 720}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 1)
               #{src\ 719}#
               #{exp\ 720}#)))
         (#{make-lexical-ref\ 251}#
           (lambda (#{src\ 727}# #{name\ 728}# #{gensym\ 729}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 3)
               #{src\ 727}#
               #{name\ 728}#
               #{gensym\ 729}#)))
         (#{make-lexical-set\ 253}#
           (lambda (#{src\ 733}#
                    #{name\ 734}#
                    #{gensym\ 735}#
                    #{exp\ 736}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 4)
               #{src\ 733}#
               #{name\ 734}#
               #{gensym\ 735}#
               #{exp\ 736}#)))
         (#{make-module-ref\ 255}#
           (lambda (#{src\ 741}#
                    #{mod\ 742}#
                    #{name\ 743}#
                    #{public?\ 744}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 5)
               #{src\ 741}#
               #{mod\ 742}#
               #{name\ 743}#
               #{public?\ 744}#)))
         (#{make-module-set\ 257}#
           (lambda (#{src\ 749}#
                    #{mod\ 750}#
                    #{name\ 751}#
                    #{public?\ 752}#
                    #{exp\ 753}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 6)
               #{src\ 749}#
               #{mod\ 750}#
               #{name\ 751}#
               #{public?\ 752}#
               #{exp\ 753}#)))
         (#{make-toplevel-ref\ 259}#
           (lambda (#{src\ 759}# #{name\ 760}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 7)
               #{src\ 759}#
               #{name\ 760}#)))
         (#{make-toplevel-set\ 261}#
           (lambda (#{src\ 763}# #{name\ 764}# #{exp\ 765}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 8)
               #{src\ 763}#
               #{name\ 764}#
               #{exp\ 765}#)))
         (#{make-toplevel-define\ 263}#
           (lambda (#{src\ 769}# #{name\ 770}# #{exp\ 771}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 9)
               #{src\ 769}#
               #{name\ 770}#
               #{exp\ 771}#)))
         (#{make-conditional\ 265}#
           (lambda (#{src\ 775}#
                    #{test\ 776}#
                    #{consequent\ 777}#
                    #{alternate\ 778}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 10)
               #{src\ 775}#
               #{test\ 776}#
               #{consequent\ 777}#
               #{alternate\ 778}#)))
         (#{make-application\ 267}#
           (lambda (#{src\ 783}# #{proc\ 784}# #{args\ 785}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 11)
               #{src\ 783}#
               #{proc\ 784}#
               #{args\ 785}#)))
         (#{make-sequence\ 269}#
           (lambda (#{src\ 789}# #{exps\ 790}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 12)
               #{src\ 789}#
               #{exps\ 790}#)))
         (#{make-lambda\ 271}#
           (lambda (#{src\ 793}# #{meta\ 794}# #{body\ 795}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 13)
               #{src\ 793}#
               #{meta\ 794}#
               #{body\ 795}#)))
         (#{make-lambda-case\ 273}#
           (lambda (#{src\ 799}#
                    #{req\ 800}#
                    #{opt\ 801}#
                    #{rest\ 802}#
                    #{kw\ 803}#
                    #{inits\ 804}#
                    #{gensyms\ 805}#
                    #{body\ 806}#
                    #{alternate\ 807}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 14)
               #{src\ 799}#
               #{req\ 800}#
               #{opt\ 801}#
               #{rest\ 802}#
               #{kw\ 803}#
               #{inits\ 804}#
               #{gensyms\ 805}#
               #{body\ 806}#
               #{alternate\ 807}#)))
         (#{make-let\ 275}#
           (lambda (#{src\ 817}#
                    #{names\ 818}#
                    #{gensyms\ 819}#
                    #{vals\ 820}#
                    #{body\ 821}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 15)
               #{src\ 817}#
               #{names\ 818}#
               #{gensyms\ 819}#
               #{vals\ 820}#
               #{body\ 821}#)))
         (#{make-letrec\ 277}#
           (lambda (#{src\ 827}#
                    #{in-order?\ 828}#
                    #{names\ 829}#
                    #{gensyms\ 830}#
                    #{vals\ 831}#
                    #{body\ 832}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 16)
               #{src\ 827}#
               #{in-order?\ 828}#
               #{names\ 829}#
               #{gensyms\ 830}#
               #{vals\ 831}#
               #{body\ 832}#)))
         (#{make-dynlet\ 279}#
           (lambda (#{src\ 839}#
                    #{fluids\ 840}#
                    #{vals\ 841}#
                    #{body\ 842}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 17)
               #{src\ 839}#
               #{fluids\ 840}#
               #{vals\ 841}#
               #{body\ 842}#)))
         (#{lambda?\ 282}#
           (lambda (#{x\ 847}#)
             (if (struct? #{x\ 847}#)
               (eq? (struct-vtable #{x\ 847}#)
                    (vector-ref %expanded-vtables 13))
               #f)))
         (#{lambda-meta\ 284}#
           (lambda (#{x\ 851}#) (struct-ref #{x\ 851}# 1)))
         (#{set-lambda-meta!\ 286}#
           (lambda (#{x\ 853}# #{v\ 854}#)
             (struct-set! #{x\ 853}# 1 #{v\ 854}#)))
         (#{top-level-eval-hook\ 296}#
           (lambda (#{x\ 857}# #{mod\ 858}#)
             (primitive-eval #{x\ 857}#)))
         (#{local-eval-hook\ 298}#
           (lambda (#{x\ 861}# #{mod\ 862}#)
             (primitive-eval #{x\ 861}#)))
         (#{put-global-definition-hook\ 301}#
           (lambda (#{symbol\ 865}# #{type\ 866}# #{val\ 867}#)
             (module-define!
               (current-module)
               #{symbol\ 865}#
               (make-syntax-transformer
                 #{symbol\ 865}#
                 #{type\ 866}#
                 #{val\ 867}#))))
         (#{get-global-definition-hook\ 303}#
           (lambda (#{symbol\ 871}# #{module\ 872}#)
             (begin
               (if (if (not #{module\ 872}#) (current-module) #f)
                 (warn "module system is booted, we should have a module"
                       #{symbol\ 871}#))
               (begin
                 (let ((#{v\ 878}# (module-variable
                                     (if #{module\ 872}#
                                       (resolve-module (cdr #{module\ 872}#))
                                       (current-module))
                                     #{symbol\ 871}#)))
                   (if #{v\ 878}#
                     (if (variable-bound? #{v\ 878}#)
                       (begin
                         (let ((#{val\ 883}# (variable-ref #{v\ 878}#)))
                           (if (macro? #{val\ 883}#)
                             (if (macro-type #{val\ 883}#)
                               (cons (macro-type #{val\ 883}#)
                                     (macro-binding #{val\ 883}#))
                               #f)
                             #f)))
                       #f)
                     #f))))))
         (#{decorate-source\ 305}#
           (lambda (#{e\ 887}# #{s\ 888}#)
             (begin
               (if (if (pair? #{e\ 887}#) #{s\ 888}# #f)
                 (set-source-properties! #{e\ 887}# #{s\ 888}#))
               #{e\ 887}#)))
         (#{build-void\ 307}#
           (lambda (#{source\ 893}#)
             (#{make-void\ 245}# #{source\ 893}#)))
         (#{build-application\ 309}#
           (lambda (#{source\ 895}#
                    #{fun-exp\ 896}#
                    #{arg-exps\ 897}#)
             (#{make-application\ 267}#
               #{source\ 895}#
               #{fun-exp\ 896}#
               #{arg-exps\ 897}#)))
         (#{build-conditional\ 311}#
           (lambda (#{source\ 901}#
                    #{test-exp\ 902}#
                    #{then-exp\ 903}#
                    #{else-exp\ 904}#)
             (#{make-conditional\ 265}#
               #{source\ 901}#
               #{test-exp\ 902}#
               #{then-exp\ 903}#
               #{else-exp\ 904}#)))
         (#{build-dynlet\ 313}#
           (lambda (#{source\ 909}#
                    #{fluids\ 910}#
                    #{vals\ 911}#
                    #{body\ 912}#)
             (#{make-dynlet\ 279}#
               #{source\ 909}#
               #{fluids\ 910}#
               #{vals\ 911}#
               #{body\ 912}#)))
         (#{build-lexical-reference\ 315}#
           (lambda (#{type\ 917}#
                    #{source\ 918}#
                    #{name\ 919}#
                    #{var\ 920}#)
             (#{make-lexical-ref\ 251}#
               #{source\ 918}#
               #{name\ 919}#
               #{var\ 920}#)))
         (#{build-lexical-assignment\ 317}#
           (lambda (#{source\ 925}#
                    #{name\ 926}#
                    #{var\ 927}#
                    #{exp\ 928}#)
             (#{make-lexical-set\ 253}#
               #{source\ 925}#
               #{name\ 926}#
               #{var\ 927}#
               #{exp\ 928}#)))
         (#{analyze-variable\ 319}#
           (lambda (#{mod\ 933}#
                    #{var\ 934}#
                    #{modref-cont\ 935}#
                    #{bare-cont\ 936}#)
             (if (not #{mod\ 933}#)
               (#{bare-cont\ 936}# #{var\ 934}#)
               (begin
                 (let ((#{kind\ 943}# (car #{mod\ 933}#))
                       (#{mod\ 944}# (cdr #{mod\ 933}#)))
                   (if (eqv? #{kind\ 943}# (quote public))
                     (#{modref-cont\ 935}#
                       #{mod\ 944}#
                       #{var\ 934}#
                       #t)
                     (if (eqv? #{kind\ 943}# (quote private))
                       (if (not (equal?
                                  #{mod\ 944}#
                                  (module-name (current-module))))
                         (#{modref-cont\ 935}#
                           #{mod\ 944}#
                           #{var\ 934}#
                           #f)
                         (#{bare-cont\ 936}# #{var\ 934}#))
                       (if (eqv? #{kind\ 943}# (quote bare))
                         (#{bare-cont\ 936}# #{var\ 934}#)
                         (if (eqv? #{kind\ 943}# (quote hygiene))
                           (if (if (not (equal?
                                          #{mod\ 944}#
                                          (module-name (current-module))))
                                 (module-variable
                                   (resolve-module #{mod\ 944}#)
                                   #{var\ 934}#)
                                 #f)
                             (#{modref-cont\ 935}#
                               #{mod\ 944}#
                               #{var\ 934}#
                               #f)
                             (#{bare-cont\ 936}# #{var\ 934}#))
                           (syntax-violation
                             #f
                             "bad module kind"
                             #{var\ 934}#
                             #{mod\ 944}#))))))))))
         (#{build-global-reference\ 321}#
           (lambda (#{source\ 952}# #{var\ 953}# #{mod\ 954}#)
             (#{analyze-variable\ 319}#
               #{mod\ 954}#
               #{var\ 953}#
               (lambda (#{mod\ 958}# #{var\ 959}# #{public?\ 960}#)
                 (#{make-module-ref\ 255}#
                   #{source\ 952}#
                   #{mod\ 958}#
                   #{var\ 959}#
                   #{public?\ 960}#))
               (lambda (#{var\ 964}#)
                 (#{make-toplevel-ref\ 259}#
                   #{source\ 952}#
                   #{var\ 964}#)))))
         (#{build-global-assignment\ 323}#
           (lambda (#{source\ 966}#
                    #{var\ 967}#
                    #{exp\ 968}#
                    #{mod\ 969}#)
             (#{analyze-variable\ 319}#
               #{mod\ 969}#
               #{var\ 967}#
               (lambda (#{mod\ 974}# #{var\ 975}# #{public?\ 976}#)
                 (#{make-module-set\ 257}#
                   #{source\ 966}#
                   #{mod\ 974}#
                   #{var\ 975}#
                   #{public?\ 976}#
                   #{exp\ 968}#))
               (lambda (#{var\ 980}#)
                 (#{make-toplevel-set\ 261}#
                   #{source\ 966}#
                   #{var\ 980}#
                   #{exp\ 968}#)))))
         (#{maybe-name-value!\ 325}#
           (lambda (#{name\ 982}# #{val\ 983}#)
             (if (#{lambda?\ 282}# #{val\ 983}#)
               (begin
                 (let ((#{meta\ 987}#
                         (#{lambda-meta\ 284}# #{val\ 983}#)))
                   (if (not (assq (quote name) #{meta\ 987}#))
                     (#{set-lambda-meta!\ 286}#
                       #{val\ 983}#
                       (cons (cons (quote name) #{name\ 982}#)
                             #{meta\ 987}#))))))))
         (#{build-global-definition\ 327}#
           (lambda (#{source\ 988}# #{var\ 989}# #{exp\ 990}#)
             (begin
               (#{maybe-name-value!\ 325}#
                 #{var\ 989}#
                 #{exp\ 990}#)
               (#{make-toplevel-define\ 263}#
                 #{source\ 988}#
                 #{var\ 989}#
                 #{exp\ 990}#))))
         (#{build-simple-lambda\ 329}#
           (lambda (#{src\ 994}#
                    #{req\ 995}#
                    #{rest\ 996}#
                    #{vars\ 997}#
                    #{meta\ 998}#
                    #{exp\ 999}#)
             (#{make-lambda\ 271}#
               #{src\ 994}#
               #{meta\ 998}#
               (#{make-lambda-case\ 273}#
                 #{src\ 994}#
                 #{req\ 995}#
                 #f
                 #{rest\ 996}#
                 #f
                 '()
                 #{vars\ 997}#
                 #{exp\ 999}#
                 #f))))
         (#{build-case-lambda\ 331}#
           (lambda (#{src\ 1006}# #{meta\ 1007}# #{body\ 1008}#)
             (#{make-lambda\ 271}#
               #{src\ 1006}#
               #{meta\ 1007}#
               #{body\ 1008}#)))
         (#{build-lambda-case\ 333}#
           (lambda (#{src\ 1012}#
                    #{req\ 1013}#
                    #{opt\ 1014}#
                    #{rest\ 1015}#
                    #{kw\ 1016}#
                    #{inits\ 1017}#
                    #{vars\ 1018}#
                    #{body\ 1019}#
                    #{else-case\ 1020}#)
             (#{make-lambda-case\ 273}#
               #{src\ 1012}#
               #{req\ 1013}#
               #{opt\ 1014}#
               #{rest\ 1015}#
               #{kw\ 1016}#
               #{inits\ 1017}#
               #{vars\ 1018}#
               #{body\ 1019}#
               #{else-case\ 1020}#)))
         (#{build-primref\ 335}#
           (lambda (#{src\ 1030}# #{name\ 1031}#)
             (if (equal?
                   (module-name (current-module))
                   '(guile))
               (#{make-toplevel-ref\ 259}#
                 #{src\ 1030}#
                 #{name\ 1031}#)
               (#{make-module-ref\ 255}#
                 #{src\ 1030}#
                 '(guile)
                 #{name\ 1031}#
                 #f))))
         (#{build-data\ 337}#
           (lambda (#{src\ 1034}# #{exp\ 1035}#)
             (#{make-const\ 247}# #{src\ 1034}# #{exp\ 1035}#)))
         (#{build-sequence\ 339}#
           (lambda (#{src\ 1038}# #{exps\ 1039}#)
             (if (null? (cdr #{exps\ 1039}#))
               (car #{exps\ 1039}#)
               (#{make-sequence\ 269}#
                 #{src\ 1038}#
                 #{exps\ 1039}#))))
         (#{build-let\ 341}#
           (lambda (#{src\ 1042}#
                    #{ids\ 1043}#
                    #{vars\ 1044}#
                    #{val-exps\ 1045}#
                    #{body-exp\ 1046}#)
             (begin
               (for-each
                 #{maybe-name-value!\ 325}#
                 #{ids\ 1043}#
                 #{val-exps\ 1045}#)
               (if (null? #{vars\ 1044}#)
                 #{body-exp\ 1046}#
                 (#{make-let\ 275}#
                   #{src\ 1042}#
                   #{ids\ 1043}#
                   #{vars\ 1044}#
                   #{val-exps\ 1045}#
                   #{body-exp\ 1046}#)))))
         (#{build-named-let\ 343}#
           (lambda (#{src\ 1052}#
                    #{ids\ 1053}#
                    #{vars\ 1054}#
                    #{val-exps\ 1055}#
                    #{body-exp\ 1056}#)
             (begin
               (let ((#{f\ 1066}# (car #{vars\ 1054}#))
                     (#{f-name\ 1067}# (car #{ids\ 1053}#))
                     (#{vars\ 1068}# (cdr #{vars\ 1054}#))
                     (#{ids\ 1069}# (cdr #{ids\ 1053}#)))
                 (begin
                   (let ((#{proc\ 1071}#
                           (#{build-simple-lambda\ 329}#
                             #{src\ 1052}#
                             #{ids\ 1069}#
                             #f
                             #{vars\ 1068}#
                             '()
                             #{body-exp\ 1056}#)))
                     (begin
                       (#{maybe-name-value!\ 325}#
                         #{f-name\ 1067}#
                         #{proc\ 1071}#)
                       (for-each
                         #{maybe-name-value!\ 325}#
                         #{ids\ 1069}#
                         #{val-exps\ 1055}#)
                       (#{make-letrec\ 277}#
                         #{src\ 1052}#
                         #f
                         (list #{f-name\ 1067}#)
                         (list #{f\ 1066}#)
                         (list #{proc\ 1071}#)
                         (#{build-application\ 309}#
                           #{src\ 1052}#
                           (#{build-lexical-reference\ 315}#
                             'fun
                             #{src\ 1052}#
                             #{f-name\ 1067}#
                             #{f\ 1066}#)
                           #{val-exps\ 1055}#)))))))))
         (#{build-letrec\ 345}#
           (lambda (#{src\ 1072}#
                    #{in-order?\ 1073}#
                    #{ids\ 1074}#
                    #{vars\ 1075}#
                    #{val-exps\ 1076}#
                    #{body-exp\ 1077}#)
             (if (null? #{vars\ 1075}#)
               #{body-exp\ 1077}#
               (begin
                 (for-each
                   #{maybe-name-value!\ 325}#
                   #{ids\ 1074}#
                   #{val-exps\ 1076}#)
                 (#{make-letrec\ 277}#
                   #{src\ 1072}#
                   #{in-order?\ 1073}#
                   #{ids\ 1074}#
                   #{vars\ 1075}#
                   #{val-exps\ 1076}#
                   #{body-exp\ 1077}#)))))
         (#{make-syntax-object\ 349}#
           (lambda (#{expression\ 1084}#
                    #{wrap\ 1085}#
                    #{module\ 1086}#)
             (vector
               'syntax-object
               #{expression\ 1084}#
               #{wrap\ 1085}#
               #{module\ 1086}#)))
         (#{syntax-object?\ 351}#
           (lambda (#{x\ 1090}#)
             (if (vector? #{x\ 1090}#)
               (if (= (vector-length #{x\ 1090}#) 4)
                 (eq? (vector-ref #{x\ 1090}# 0)
                      'syntax-object)
                 #f)
               #f)))
         (#{syntax-object-expression\ 353}#
           (lambda (#{x\ 1095}#) (vector-ref #{x\ 1095}# 1)))
         (#{syntax-object-wrap\ 355}#
           (lambda (#{x\ 1097}#) (vector-ref #{x\ 1097}# 2)))
         (#{syntax-object-module\ 357}#
           (lambda (#{x\ 1099}#) (vector-ref #{x\ 1099}# 3)))
         (#{source-annotation\ 366}#
           (lambda (#{x\ 1113}#)
             (if (#{syntax-object?\ 351}# #{x\ 1113}#)
               (#{source-annotation\ 366}#
                 (#{syntax-object-expression\ 353}# #{x\ 1113}#))
               (if (pair? #{x\ 1113}#)
                 (begin
                   (let ((#{props\ 1120}# (source-properties #{x\ 1113}#)))
                     (if (pair? #{props\ 1120}#) #{props\ 1120}# #f)))
                 #f))))
         (#{extend-env\ 375}#
           (lambda (#{labels\ 1122}# #{bindings\ 1123}# #{r\ 1124}#)
             (if (null? #{labels\ 1122}#)
               #{r\ 1124}#
               (#{extend-env\ 375}#
                 (cdr #{labels\ 1122}#)
                 (cdr #{bindings\ 1123}#)
                 (cons (cons (car #{labels\ 1122}#)
                             (car #{bindings\ 1123}#))
                       #{r\ 1124}#)))))
         (#{extend-var-env\ 377}#
           (lambda (#{labels\ 1128}# #{vars\ 1129}# #{r\ 1130}#)
             (if (null? #{labels\ 1128}#)
               #{r\ 1130}#
               (#{extend-var-env\ 377}#
                 (cdr #{labels\ 1128}#)
                 (cdr #{vars\ 1129}#)
                 (cons (cons (car #{labels\ 1128}#)
                             (cons (quote lexical) (car #{vars\ 1129}#)))
                       #{r\ 1130}#)))))
         (#{macros-only-env\ 379}#
           (lambda (#{r\ 1135}#)
             (if (null? #{r\ 1135}#)
               '()
               (begin
                 (let ((#{a\ 1138}# (car #{r\ 1135}#)))
                   (if (eq? (car (cdr #{a\ 1138}#)) (quote macro))
                     (cons #{a\ 1138}#
                           (#{macros-only-env\ 379}# (cdr #{r\ 1135}#)))
                     (#{macros-only-env\ 379}# (cdr #{r\ 1135}#))))))))
         (#{lookup\ 381}#
           (lambda (#{x\ 1139}# #{r\ 1140}# #{mod\ 1141}#)
             (begin
               (let ((#{t\ 1147}# (assq #{x\ 1139}# #{r\ 1140}#)))
                 (if #{t\ 1147}#
                   (cdr #{t\ 1147}#)
                   (if (symbol? #{x\ 1139}#)
                     (begin
                       (let ((#{t\ 1153}#
                               (#{get-global-definition-hook\ 303}#
                                 #{x\ 1139}#
                                 #{mod\ 1141}#)))
                         (if #{t\ 1153}# #{t\ 1153}# (quote (global)))))
                     '(displaced-lexical)))))))
         (#{global-extend\ 383}#
           (lambda (#{type\ 1158}# #{sym\ 1159}# #{val\ 1160}#)
             (#{put-global-definition-hook\ 301}#
               #{sym\ 1159}#
               #{type\ 1158}#
               #{val\ 1160}#)))
         (#{nonsymbol-id?\ 385}#
           (lambda (#{x\ 1164}#)
             (if (#{syntax-object?\ 351}# #{x\ 1164}#)
               (symbol?
                 (#{syntax-object-expression\ 353}# #{x\ 1164}#))
               #f)))
         (#{id?\ 387}#
           (lambda (#{x\ 1168}#)
             (if (symbol? #{x\ 1168}#)
               #t
               (if (#{syntax-object?\ 351}# #{x\ 1168}#)
                 (symbol?
                   (#{syntax-object-expression\ 353}# #{x\ 1168}#))
                 #f))))
         (#{id-sym-name&marks\ 390}#
           (lambda (#{x\ 1175}# #{w\ 1176}#)
             (if (#{syntax-object?\ 351}# #{x\ 1175}#)
               (values
                 (#{syntax-object-expression\ 353}# #{x\ 1175}#)
                 (#{join-marks\ 440}#
                   (#{wrap-marks\ 394}# #{w\ 1176}#)
                   (#{wrap-marks\ 394}#
                     (#{syntax-object-wrap\ 355}# #{x\ 1175}#))))
               (values
                 #{x\ 1175}#
                 (#{wrap-marks\ 394}# #{w\ 1176}#)))))
         (#{gen-label\ 403}#
           (lambda () (symbol->string (gensym "i"))))
         (#{gen-labels\ 405}#
           (lambda (#{ls\ 1179}#)
             (if (null? #{ls\ 1179}#)
               '()
               (cons (#{gen-label\ 403}#)
                     (#{gen-labels\ 405}# (cdr #{ls\ 1179}#))))))
         (#{make-ribcage\ 408}#
           (lambda (#{symnames\ 1181}#
                    #{marks\ 1182}#
                    #{labels\ 1183}#)
             (vector
               'ribcage
               #{symnames\ 1181}#
               #{marks\ 1182}#
               #{labels\ 1183}#)))
         (#{ribcage-symnames\ 412}#
           (lambda (#{x\ 1192}#) (vector-ref #{x\ 1192}# 1)))
         (#{ribcage-marks\ 414}#
           (lambda (#{x\ 1194}#) (vector-ref #{x\ 1194}# 2)))
         (#{ribcage-labels\ 416}#
           (lambda (#{x\ 1196}#) (vector-ref #{x\ 1196}# 3)))
         (#{set-ribcage-symnames!\ 418}#
           (lambda (#{x\ 1198}# #{update\ 1199}#)
             (vector-set! #{x\ 1198}# 1 #{update\ 1199}#)))
         (#{set-ribcage-marks!\ 420}#
           (lambda (#{x\ 1202}# #{update\ 1203}#)
             (vector-set! #{x\ 1202}# 2 #{update\ 1203}#)))
         (#{set-ribcage-labels!\ 422}#
           (lambda (#{x\ 1206}# #{update\ 1207}#)
             (vector-set! #{x\ 1206}# 3 #{update\ 1207}#)))
         (#{anti-mark\ 428}#
           (lambda (#{w\ 1210}#)
             (#{make-wrap\ 392}#
               (cons #f (#{wrap-marks\ 394}# #{w\ 1210}#))
               (cons 'shift
                     (#{wrap-subst\ 396}# #{w\ 1210}#)))))
         (#{extend-ribcage!\ 432}#
           (lambda (#{ribcage\ 1213}# #{id\ 1214}# #{label\ 1215}#)
             (begin
               (#{set-ribcage-symnames!\ 418}#
                 #{ribcage\ 1213}#
                 (cons (#{syntax-object-expression\ 353}# #{id\ 1214}#)
                       (#{ribcage-symnames\ 412}# #{ribcage\ 1213}#)))
               (#{set-ribcage-marks!\ 420}#
                 #{ribcage\ 1213}#
                 (cons (#{wrap-marks\ 394}#
                         (#{syntax-object-wrap\ 355}# #{id\ 1214}#))
                       (#{ribcage-marks\ 414}# #{ribcage\ 1213}#)))
               (#{set-ribcage-labels!\ 422}#
                 #{ribcage\ 1213}#
                 (cons #{label\ 1215}#
                       (#{ribcage-labels\ 416}# #{ribcage\ 1213}#))))))
         (#{make-binding-wrap\ 434}#
           (lambda (#{ids\ 1219}# #{labels\ 1220}# #{w\ 1221}#)
             (if (null? #{ids\ 1219}#)
               #{w\ 1221}#
               (#{make-wrap\ 392}#
                 (#{wrap-marks\ 394}# #{w\ 1221}#)
                 (cons (begin
                         (let ((#{labelvec\ 1226}#
                                 (list->vector #{labels\ 1220}#)))
                           (begin
                             (let ((#{n\ 1228}#
                                     (vector-length #{labelvec\ 1226}#)))
                               (begin
                                 (let ((#{symnamevec\ 1231}#
                                         (make-vector #{n\ 1228}#))
                                       (#{marksvec\ 1232}#
                                         (make-vector #{n\ 1228}#)))
                                   (begin
                                     (letrec*
                                       ((#{f\ 1236}#
                                          (lambda (#{ids\ 1237}# #{i\ 1238}#)
                                            (if (not (null? #{ids\ 1237}#))
                                              (call-with-values
                                                (lambda ()
                                                  (#{id-sym-name&marks\ 390}#
                                                    (car #{ids\ 1237}#)
                                                    #{w\ 1221}#))
                                                (lambda (#{symname\ 1239}#
                                                         #{marks\ 1240}#)
                                                  (begin
                                                    (vector-set!
                                                      #{symnamevec\ 1231}#
                                                      #{i\ 1238}#
                                                      #{symname\ 1239}#)
                                                    (vector-set!
                                                      #{marksvec\ 1232}#
                                                      #{i\ 1238}#
                                                      #{marks\ 1240}#)
                                                    (#{f\ 1236}#
                                                      (cdr #{ids\ 1237}#)
                                                      (#{fx+\ 288}#
                                                        #{i\ 1238}#
                                                        1)))))))))
                                       (begin (#{f\ 1236}# #{ids\ 1219}# 0)))
                                     (#{make-ribcage\ 408}#
                                       #{symnamevec\ 1231}#
                                       #{marksvec\ 1232}#
                                       #{labelvec\ 1226}#))))))))
                       (#{wrap-subst\ 396}# #{w\ 1221}#))))))
         (#{smart-append\ 436}#
           (lambda (#{m1\ 1243}# #{m2\ 1244}#)
             (if (null? #{m2\ 1244}#)
               #{m1\ 1243}#
               (append #{m1\ 1243}# #{m2\ 1244}#))))
         (#{join-wraps\ 438}#
           (lambda (#{w1\ 1247}# #{w2\ 1248}#)
             (begin
               (let ((#{m1\ 1253}# (#{wrap-marks\ 394}# #{w1\ 1247}#))
                     (#{s1\ 1254}# (#{wrap-subst\ 396}# #{w1\ 1247}#)))
                 (if (null? #{m1\ 1253}#)
                   (if (null? #{s1\ 1254}#)
                     #{w2\ 1248}#
                     (#{make-wrap\ 392}#
                       (#{wrap-marks\ 394}# #{w2\ 1248}#)
                       (#{smart-append\ 436}#
                         #{s1\ 1254}#
                         (#{wrap-subst\ 396}# #{w2\ 1248}#))))
                   (#{make-wrap\ 392}#
                     (#{smart-append\ 436}#
                       #{m1\ 1253}#
                       (#{wrap-marks\ 394}# #{w2\ 1248}#))
                     (#{smart-append\ 436}#
                       #{s1\ 1254}#
                       (#{wrap-subst\ 396}# #{w2\ 1248}#))))))))
         (#{join-marks\ 440}#
           (lambda (#{m1\ 1255}# #{m2\ 1256}#)
             (#{smart-append\ 436}# #{m1\ 1255}# #{m2\ 1256}#)))
         (#{same-marks?\ 442}#
           (lambda (#{x\ 1259}# #{y\ 1260}#)
             (begin
               (let ((#{t\ 1265}# (eq? #{x\ 1259}# #{y\ 1260}#)))
                 (if #{t\ 1265}#
                   #{t\ 1265}#
                   (if (not (null? #{x\ 1259}#))
                     (if (not (null? #{y\ 1260}#))
                       (if (eq? (car #{x\ 1259}#) (car #{y\ 1260}#))
                         (#{same-marks?\ 442}#
                           (cdr #{x\ 1259}#)
                           (cdr #{y\ 1260}#))
                         #f)
                       #f)
                     #f))))))
         (#{id-var-name\ 444}#
           (lambda (#{id\ 1271}# #{w\ 1272}#)
             (letrec*
               ((#{search\ 1277}#
                  (lambda (#{sym\ 1293}# #{subst\ 1294}# #{marks\ 1295}#)
                    (if (null? #{subst\ 1294}#)
                      (values #f #{marks\ 1295}#)
                      (begin
                        (let ((#{fst\ 1300}# (car #{subst\ 1294}#)))
                          (if (eq? #{fst\ 1300}# (quote shift))
                            (#{search\ 1277}#
                              #{sym\ 1293}#
                              (cdr #{subst\ 1294}#)
                              (cdr #{marks\ 1295}#))
                            (begin
                              (let ((#{symnames\ 1302}#
                                      (#{ribcage-symnames\ 412}#
                                        #{fst\ 1300}#)))
                                (if (vector? #{symnames\ 1302}#)
                                  (#{search-vector-rib\ 1281}#
                                    #{sym\ 1293}#
                                    #{subst\ 1294}#
                                    #{marks\ 1295}#
                                    #{symnames\ 1302}#
                                    #{fst\ 1300}#)
                                  (#{search-list-rib\ 1279}#
                                    #{sym\ 1293}#
                                    #{subst\ 1294}#
                                    #{marks\ 1295}#
                                    #{symnames\ 1302}#
                                    #{fst\ 1300}#))))))))))
                (#{search-list-rib\ 1279}#
                  (lambda (#{sym\ 1303}#
                           #{subst\ 1304}#
                           #{marks\ 1305}#
                           #{symnames\ 1306}#
                           #{ribcage\ 1307}#)
                    (letrec*
                      ((#{f\ 1316}#
                         (lambda (#{symnames\ 1317}# #{i\ 1318}#)
                           (if (null? #{symnames\ 1317}#)
                             (#{search\ 1277}#
                               #{sym\ 1303}#
                               (cdr #{subst\ 1304}#)
                               #{marks\ 1305}#)
                             (if (if (eq? (car #{symnames\ 1317}#)
                                          #{sym\ 1303}#)
                                   (#{same-marks?\ 442}#
                                     #{marks\ 1305}#
                                     (list-ref
                                       (#{ribcage-marks\ 414}#
                                         #{ribcage\ 1307}#)
                                       #{i\ 1318}#))
                                   #f)
                               (values
                                 (list-ref
                                   (#{ribcage-labels\ 416}# #{ribcage\ 1307}#)
                                   #{i\ 1318}#)
                                 #{marks\ 1305}#)
                               (#{f\ 1316}#
                                 (cdr #{symnames\ 1317}#)
                                 (#{fx+\ 288}# #{i\ 1318}# 1)))))))
                      (begin (#{f\ 1316}# #{symnames\ 1306}# 0)))))
                (#{search-vector-rib\ 1281}#
                  (lambda (#{sym\ 1326}#
                           #{subst\ 1327}#
                           #{marks\ 1328}#
                           #{symnames\ 1329}#
                           #{ribcage\ 1330}#)
                    (begin
                      (let ((#{n\ 1337}# (vector-length #{symnames\ 1329}#)))
                        (letrec*
                          ((#{f\ 1340}#
                             (lambda (#{i\ 1341}#)
                               (if (#{fx=\ 292}# #{i\ 1341}# #{n\ 1337}#)
                                 (#{search\ 1277}#
                                   #{sym\ 1326}#
                                   (cdr #{subst\ 1327}#)
                                   #{marks\ 1328}#)
                                 (if (if (eq? (vector-ref
                                                #{symnames\ 1329}#
                                                #{i\ 1341}#)
                                              #{sym\ 1326}#)
                                       (#{same-marks?\ 442}#
                                         #{marks\ 1328}#
                                         (vector-ref
                                           (#{ribcage-marks\ 414}#
                                             #{ribcage\ 1330}#)
                                           #{i\ 1341}#))
                                       #f)
                                   (values
                                     (vector-ref
                                       (#{ribcage-labels\ 416}#
                                         #{ribcage\ 1330}#)
                                       #{i\ 1341}#)
                                     #{marks\ 1328}#)
                                   (#{f\ 1340}#
                                     (#{fx+\ 288}# #{i\ 1341}# 1)))))))
                          (begin (#{f\ 1340}# 0))))))))
               (begin
                 (if (symbol? #{id\ 1271}#)
                   (begin
                     (let ((#{t\ 1351}#
                             (call-with-values
                               (lambda ()
                                 (#{search\ 1277}#
                                   #{id\ 1271}#
                                   (#{wrap-subst\ 396}# #{w\ 1272}#)
                                   (#{wrap-marks\ 394}# #{w\ 1272}#)))
                               (lambda (#{x\ 1353}# . #{ignore\ 1354}#)
                                 #{x\ 1353}#))))
                       (if #{t\ 1351}# #{t\ 1351}# #{id\ 1271}#)))
                   (if (#{syntax-object?\ 351}# #{id\ 1271}#)
                     (begin
                       (let ((#{id\ 1362}#
                               (#{syntax-object-expression\ 353}#
                                 #{id\ 1271}#))
                             (#{w1\ 1363}#
                               (#{syntax-object-wrap\ 355}# #{id\ 1271}#)))
                         (begin
                           (let ((#{marks\ 1365}#
                                   (#{join-marks\ 440}#
                                     (#{wrap-marks\ 394}# #{w\ 1272}#)
                                     (#{wrap-marks\ 394}# #{w1\ 1363}#))))
                             (call-with-values
                               (lambda ()
                                 (#{search\ 1277}#
                                   #{id\ 1362}#
                                   (#{wrap-subst\ 396}# #{w\ 1272}#)
                                   #{marks\ 1365}#))
                               (lambda (#{new-id\ 1366}# #{marks\ 1367}#)
                                 (begin
                                   (let ((#{t\ 1372}# #{new-id\ 1366}#))
                                     (if #{t\ 1372}#
                                       #{t\ 1372}#
                                       (begin
                                         (let ((#{t\ 1375}#
                                                 (call-with-values
                                                   (lambda ()
                                                     (#{search\ 1277}#
                                                       #{id\ 1362}#
                                                       (#{wrap-subst\ 396}#
                                                         #{w1\ 1363}#)
                                                       #{marks\ 1367}#))
                                                   (lambda (#{x\ 1377}#
                                                            .
                                                            #{ignore\ 1378}#)
                                                     #{x\ 1377}#))))
                                           (if #{t\ 1375}#
                                             #{t\ 1375}#
                                             #{id\ 1362}#))))))))))))
                     (syntax-violation
                       'id-var-name
                       "invalid id"
                       #{id\ 1271}#)))))))
         (#{free-id=?\ 446}#
           (lambda (#{i\ 1383}# #{j\ 1384}#)
             (if (eq? (begin
                        (let ((#{x\ 1390}# #{i\ 1383}#))
                          (if (#{syntax-object?\ 351}# #{x\ 1390}#)
                            (#{syntax-object-expression\ 353}# #{x\ 1390}#)
                            #{x\ 1390}#)))
                      (begin
                        (let ((#{x\ 1393}# #{j\ 1384}#))
                          (if (#{syntax-object?\ 351}# #{x\ 1393}#)
                            (#{syntax-object-expression\ 353}# #{x\ 1393}#)
                            #{x\ 1393}#))))
               (eq? (#{id-var-name\ 444}# #{i\ 1383}# (quote (())))
                    (#{id-var-name\ 444}# #{j\ 1384}# (quote (()))))
               #f)))
         (#{bound-id=?\ 448}#
           (lambda (#{i\ 1397}# #{j\ 1398}#)
             (if (if (#{syntax-object?\ 351}# #{i\ 1397}#)
                   (#{syntax-object?\ 351}# #{j\ 1398}#)
                   #f)
               (if (eq? (#{syntax-object-expression\ 353}# #{i\ 1397}#)
                        (#{syntax-object-expression\ 353}# #{j\ 1398}#))
                 (#{same-marks?\ 442}#
                   (#{wrap-marks\ 394}#
                     (#{syntax-object-wrap\ 355}# #{i\ 1397}#))
                   (#{wrap-marks\ 394}#
                     (#{syntax-object-wrap\ 355}# #{j\ 1398}#)))
                 #f)
               (eq? #{i\ 1397}# #{j\ 1398}#))))
         (#{valid-bound-ids?\ 450}#
           (lambda (#{ids\ 1405}#)
             (if (letrec*
                   ((#{all-ids?\ 1410}#
                      (lambda (#{ids\ 1411}#)
                        (begin
                          (let ((#{t\ 1414}# (null? #{ids\ 1411}#)))
                            (if #{t\ 1414}#
                              #{t\ 1414}#
                              (if (#{id?\ 387}# (car #{ids\ 1411}#))
                                (#{all-ids?\ 1410}# (cdr #{ids\ 1411}#))
                                #f)))))))
                   (begin (#{all-ids?\ 1410}# #{ids\ 1405}#)))
               (#{distinct-bound-ids?\ 452}# #{ids\ 1405}#)
               #f)))
         (#{distinct-bound-ids?\ 452}#
           (lambda (#{ids\ 1419}#)
             (letrec*
               ((#{distinct?\ 1423}#
                  (lambda (#{ids\ 1424}#)
                    (begin
                      (let ((#{t\ 1427}# (null? #{ids\ 1424}#)))
                        (if #{t\ 1427}#
                          #{t\ 1427}#
                          (if (not (#{bound-id-member?\ 454}#
                                     (car #{ids\ 1424}#)
                                     (cdr #{ids\ 1424}#)))
                            (#{distinct?\ 1423}# (cdr #{ids\ 1424}#))
                            #f)))))))
               (begin (#{distinct?\ 1423}# #{ids\ 1419}#)))))
         (#{bound-id-member?\ 454}#
           (lambda (#{x\ 1431}# #{list\ 1432}#)
             (if (not (null? #{list\ 1432}#))
               (begin
                 (let ((#{t\ 1439}#
                         (#{bound-id=?\ 448}#
                           #{x\ 1431}#
                           (car #{list\ 1432}#))))
                   (if #{t\ 1439}#
                     #{t\ 1439}#
                     (#{bound-id-member?\ 454}#
                       #{x\ 1431}#
                       (cdr #{list\ 1432}#)))))
               #f)))
         (#{wrap\ 456}#
           (lambda (#{x\ 1441}# #{w\ 1442}# #{defmod\ 1443}#)
             (if (if (null? (#{wrap-marks\ 394}# #{w\ 1442}#))
                   (null? (#{wrap-subst\ 396}# #{w\ 1442}#))
                   #f)
               #{x\ 1441}#
               (if (#{syntax-object?\ 351}# #{x\ 1441}#)
                 (#{make-syntax-object\ 349}#
                   (#{syntax-object-expression\ 353}# #{x\ 1441}#)
                   (#{join-wraps\ 438}#
                     #{w\ 1442}#
                     (#{syntax-object-wrap\ 355}# #{x\ 1441}#))
                   (#{syntax-object-module\ 357}# #{x\ 1441}#))
                 (if (null? #{x\ 1441}#)
                   #{x\ 1441}#
                   (#{make-syntax-object\ 349}#
                     #{x\ 1441}#
                     #{w\ 1442}#
                     #{defmod\ 1443}#))))))
         (#{source-wrap\ 458}#
           (lambda (#{x\ 1456}#
                    #{w\ 1457}#
                    #{s\ 1458}#
                    #{defmod\ 1459}#)
             (#{wrap\ 456}#
               (#{decorate-source\ 305}#
                 #{x\ 1456}#
                 #{s\ 1458}#)
               #{w\ 1457}#
               #{defmod\ 1459}#)))
         (#{chi-sequence\ 460}#
           (lambda (#{body\ 1464}#
                    #{r\ 1465}#
                    #{w\ 1466}#
                    #{s\ 1467}#
                    #{mod\ 1468}#)
             (#{build-sequence\ 339}#
               #{s\ 1467}#
               (letrec*
                 ((#{dobody\ 1479}#
                    (lambda (#{body\ 1480}#
                             #{r\ 1481}#
                             #{w\ 1482}#
                             #{mod\ 1483}#)
                      (if (null? #{body\ 1480}#)
                        '()
                        (begin
                          (let ((#{first\ 1485}#
                                  (#{chi\ 472}#
                                    (car #{body\ 1480}#)
                                    #{r\ 1481}#
                                    #{w\ 1482}#
                                    #{mod\ 1483}#)))
                            (cons #{first\ 1485}#
                                  (#{dobody\ 1479}#
                                    (cdr #{body\ 1480}#)
                                    #{r\ 1481}#
                                    #{w\ 1482}#
                                    #{mod\ 1483}#))))))))
                 (begin
                   (#{dobody\ 1479}#
                     #{body\ 1464}#
                     #{r\ 1465}#
                     #{w\ 1466}#
                     #{mod\ 1468}#))))))
         (#{chi-top-sequence\ 462}#
           (lambda (#{body\ 1486}#
                    #{r\ 1487}#
                    #{w\ 1488}#
                    #{s\ 1489}#
                    #{m\ 1490}#
                    #{esew\ 1491}#
                    #{mod\ 1492}#)
             (#{build-sequence\ 339}#
               #{s\ 1489}#
               (letrec*
                 ((#{dobody\ 1508}#
                    (lambda (#{body\ 1509}#
                             #{r\ 1510}#
                             #{w\ 1511}#
                             #{m\ 1512}#
                             #{esew\ 1513}#
                             #{mod\ 1514}#
                             #{out\ 1515}#)
                      (if (null? #{body\ 1509}#)
                        (reverse #{out\ 1515}#)
                        (#{dobody\ 1508}#
                          (cdr #{body\ 1509}#)
                          #{r\ 1510}#
                          #{w\ 1511}#
                          #{m\ 1512}#
                          #{esew\ 1513}#
                          #{mod\ 1514}#
                          (cons (#{chi-top\ 470}#
                                  (car #{body\ 1509}#)
                                  #{r\ 1510}#
                                  #{w\ 1511}#
                                  #{m\ 1512}#
                                  #{esew\ 1513}#
                                  #{mod\ 1514}#)
                                #{out\ 1515}#))))))
                 (begin
                   (#{dobody\ 1508}#
                     #{body\ 1486}#
                     #{r\ 1487}#
                     #{w\ 1488}#
                     #{m\ 1490}#
                     #{esew\ 1491}#
                     #{mod\ 1492}#
                     '()))))))
         (#{chi-install-global\ 464}#
           (lambda (#{name\ 1516}# #{e\ 1517}#)
             (#{build-global-definition\ 327}#
               #f
               #{name\ 1516}#
               (#{build-application\ 309}#
                 #f
                 (#{build-primref\ 335}#
                   #f
                   'make-syntax-transformer)
                 (list (#{build-data\ 337}# #f #{name\ 1516}#)
                       (#{build-data\ 337}# #f (quote macro))
                       #{e\ 1517}#)))))
         (#{chi-when-list\ 466}#
           (lambda (#{e\ 1525}# #{when-list\ 1526}# #{w\ 1527}#)
             (letrec*
               ((#{f\ 1534}#
                  (lambda (#{when-list\ 1535}# #{situations\ 1536}#)
                    (if (null? #{when-list\ 1535}#)
                      #{situations\ 1536}#
                      (#{f\ 1534}#
                        (cdr #{when-list\ 1535}#)
                        (cons (begin
                                (let ((#{x\ 1538}# (car #{when-list\ 1535}#)))
                                  (if (#{free-id=?\ 446}#
                                        #{x\ 1538}#
                                        '#(syntax-object
                                           compile
                                           ((top)
                                            #(ribcage () () ())
                                            #(ribcage () () ())
                                            #(ribcage () () ())
                                            #(ribcage #(x) #((top)) #("i1537"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(f when-list situations)
                                              #((top) (top) (top))
                                              #("i1531" "i1532" "i1533"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(e when-list w)
                                              #((top) (top) (top))
                                              #("i1528" "i1529" "i1530"))
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
                                                build-dynlet
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
                                              ("i501"
                                               "i499"
                                               "i497"
                                               "i495"
                                               "i493"
                                               "i491"
                                               "i489"
                                               "i487"
                                               "i485"
                                               "i483"
                                               "i481"
                                               "i479"
                                               "i477"
                                               "i475"
                                               "i473"
                                               "i471"
                                               "i469"
                                               "i467"
                                               "i465"
                                               "i463"
                                               "i461"
                                               "i459"
                                               "i457"
                                               "i455"
                                               "i453"
                                               "i451"
                                               "i449"
                                               "i447"
                                               "i445"
                                               "i443"
                                               "i441"
                                               "i439"
                                               "i437"
                                               "i435"
                                               "i433"
                                               "i431"
                                               "i430"
                                               "i429"
                                               "i427"
                                               "i426"
                                               "i425"
                                               "i424"
                                               "i423"
                                               "i421"
                                               "i419"
                                               "i417"
                                               "i415"
                                               "i413"
                                               "i411"
                                               "i409"
                                               "i407"
                                               "i404"
                                               "i402"
                                               "i401"
                                               "i400"
                                               "i399"
                                               "i398"
                                               "i397"
                                               "i395"
                                               "i393"
                                               "i391"
                                               "i389"
                                               "i388"
                                               "i386"
                                               "i384"
                                               "i382"
                                               "i380"
                                               "i378"
                                               "i376"
                                               "i374"
                                               "i373"
                                               "i371"
                                               "i369"
                                               "i368"
                                               "i367"
                                               "i365"
                                               "i364"
                                               "i362"
                                               "i360"
                                               "i358"
                                               "i356"
                                               "i354"
                                               "i352"
                                               "i350"
                                               "i348"
                                               "i346"
                                               "i344"
                                               "i342"
                                               "i340"
                                               "i338"
                                               "i336"
                                               "i334"
                                               "i332"
                                               "i330"
                                               "i328"
                                               "i326"
                                               "i324"
                                               "i322"
                                               "i320"
                                               "i318"
                                               "i316"
                                               "i314"
                                               "i312"
                                               "i310"
                                               "i308"
                                               "i306"
                                               "i304"
                                               "i302"
                                               "i300"
                                               "i299"
                                               "i297"
                                               "i295"
                                               "i293"
                                               "i291"
                                               "i289"
                                               "i287"
                                               "i285"
                                               "i283"
                                               "i281"
                                               "i278"
                                               "i276"
                                               "i274"
                                               "i272"
                                               "i270"
                                               "i268"
                                               "i266"
                                               "i264"
                                               "i262"
                                               "i260"
                                               "i258"
                                               "i256"
                                               "i254"
                                               "i252"
                                               "i250"
                                               "i248"
                                               "i246"
                                               "i244"))
                                            #(ribcage
                                              (define-structure
                                                define-expansion-accessors
                                                define-expansion-constructors
                                                and-map*)
                                              ((top) (top) (top) (top))
                                              ("i38" "i37" "i36" "i34")))
                                           (hygiene guile)))
                                    'compile
                                    (if (#{free-id=?\ 446}#
                                          #{x\ 1538}#
                                          '#(syntax-object
                                             load
                                             ((top)
                                              #(ribcage () () ())
                                              #(ribcage () () ())
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(x)
                                                #((top))
                                                #("i1537"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(f when-list situations)
                                                #((top) (top) (top))
                                                #("i1531" "i1532" "i1533"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(e when-list w)
                                                #((top) (top) (top))
                                                #("i1528" "i1529" "i1530"))
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
                                                  build-dynlet
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
                                                ("i501"
                                                 "i499"
                                                 "i497"
                                                 "i495"
                                                 "i493"
                                                 "i491"
                                                 "i489"
                                                 "i487"
                                                 "i485"
                                                 "i483"
                                                 "i481"
                                                 "i479"
                                                 "i477"
                                                 "i475"
                                                 "i473"
                                                 "i471"
                                                 "i469"
                                                 "i467"
                                                 "i465"
                                                 "i463"
                                                 "i461"
                                                 "i459"
                                                 "i457"
                                                 "i455"
                                                 "i453"
                                                 "i451"
                                                 "i449"
                                                 "i447"
                                                 "i445"
                                                 "i443"
                                                 "i441"
                                                 "i439"
                                                 "i437"
                                                 "i435"
                                                 "i433"
                                                 "i431"
                                                 "i430"
                                                 "i429"
                                                 "i427"
                                                 "i426"
                                                 "i425"
                                                 "i424"
                                                 "i423"
                                                 "i421"
                                                 "i419"
                                                 "i417"
                                                 "i415"
                                                 "i413"
                                                 "i411"
                                                 "i409"
                                                 "i407"
                                                 "i404"
                                                 "i402"
                                                 "i401"
                                                 "i400"
                                                 "i399"
                                                 "i398"
                                                 "i397"
                                                 "i395"
                                                 "i393"
                                                 "i391"
                                                 "i389"
                                                 "i388"
                                                 "i386"
                                                 "i384"
                                                 "i382"
                                                 "i380"
                                                 "i378"
                                                 "i376"
                                                 "i374"
                                                 "i373"
                                                 "i371"
                                                 "i369"
                                                 "i368"
                                                 "i367"
                                                 "i365"
                                                 "i364"
                                                 "i362"
                                                 "i360"
                                                 "i358"
                                                 "i356"
                                                 "i354"
                                                 "i352"
                                                 "i350"
                                                 "i348"
                                                 "i346"
                                                 "i344"
                                                 "i342"
                                                 "i340"
                                                 "i338"
                                                 "i336"
                                                 "i334"
                                                 "i332"
                                                 "i330"
                                                 "i328"
                                                 "i326"
                                                 "i324"
                                                 "i322"
                                                 "i320"
                                                 "i318"
                                                 "i316"
                                                 "i314"
                                                 "i312"
                                                 "i310"
                                                 "i308"
                                                 "i306"
                                                 "i304"
                                                 "i302"
                                                 "i300"
                                                 "i299"
                                                 "i297"
                                                 "i295"
                                                 "i293"
                                                 "i291"
                                                 "i289"
                                                 "i287"
                                                 "i285"
                                                 "i283"
                                                 "i281"
                                                 "i278"
                                                 "i276"
                                                 "i274"
                                                 "i272"
                                                 "i270"
                                                 "i268"
                                                 "i266"
                                                 "i264"
                                                 "i262"
                                                 "i260"
                                                 "i258"
                                                 "i256"
                                                 "i254"
                                                 "i252"
                                                 "i250"
                                                 "i248"
                                                 "i246"
                                                 "i244"))
                                              #(ribcage
                                                (define-structure
                                                  define-expansion-accessors
                                                  define-expansion-constructors
                                                  and-map*)
                                                ((top) (top) (top) (top))
                                                ("i38" "i37" "i36" "i34")))
                                             (hygiene guile)))
                                      'load
                                      (if (#{free-id=?\ 446}#
                                            #{x\ 1538}#
                                            '#(syntax-object
                                               eval
                                               ((top)
                                                #(ribcage () () ())
                                                #(ribcage () () ())
                                                #(ribcage () () ())
                                                #(ribcage
                                                  #(x)
                                                  #((top))
                                                  #("i1537"))
                                                #(ribcage () () ())
                                                #(ribcage
                                                  #(f when-list situations)
                                                  #((top) (top) (top))
                                                  #("i1531" "i1532" "i1533"))
                                                #(ribcage () () ())
                                                #(ribcage
                                                  #(e when-list w)
                                                  #((top) (top) (top))
                                                  #("i1528" "i1529" "i1530"))
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
                                                    build-dynlet
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
                                                  ("i501"
                                                   "i499"
                                                   "i497"
                                                   "i495"
                                                   "i493"
                                                   "i491"
                                                   "i489"
                                                   "i487"
                                                   "i485"
                                                   "i483"
                                                   "i481"
                                                   "i479"
                                                   "i477"
                                                   "i475"
                                                   "i473"
                                                   "i471"
                                                   "i469"
                                                   "i467"
                                                   "i465"
                                                   "i463"
                                                   "i461"
                                                   "i459"
                                                   "i457"
                                                   "i455"
                                                   "i453"
                                                   "i451"
                                                   "i449"
                                                   "i447"
                                                   "i445"
                                                   "i443"
                                                   "i441"
                                                   "i439"
                                                   "i437"
                                                   "i435"
                                                   "i433"
                                                   "i431"
                                                   "i430"
                                                   "i429"
                                                   "i427"
                                                   "i426"
                                                   "i425"
                                                   "i424"
                                                   "i423"
                                                   "i421"
                                                   "i419"
                                                   "i417"
                                                   "i415"
                                                   "i413"
                                                   "i411"
                                                   "i409"
                                                   "i407"
                                                   "i404"
                                                   "i402"
                                                   "i401"
                                                   "i400"
                                                   "i399"
                                                   "i398"
                                                   "i397"
                                                   "i395"
                                                   "i393"
                                                   "i391"
                                                   "i389"
                                                   "i388"
                                                   "i386"
                                                   "i384"
                                                   "i382"
                                                   "i380"
                                                   "i378"
                                                   "i376"
                                                   "i374"
                                                   "i373"
                                                   "i371"
                                                   "i369"
                                                   "i368"
                                                   "i367"
                                                   "i365"
                                                   "i364"
                                                   "i362"
                                                   "i360"
                                                   "i358"
                                                   "i356"
                                                   "i354"
                                                   "i352"
                                                   "i350"
                                                   "i348"
                                                   "i346"
                                                   "i344"
                                                   "i342"
                                                   "i340"
                                                   "i338"
                                                   "i336"
                                                   "i334"
                                                   "i332"
                                                   "i330"
                                                   "i328"
                                                   "i326"
                                                   "i324"
                                                   "i322"
                                                   "i320"
                                                   "i318"
                                                   "i316"
                                                   "i314"
                                                   "i312"
                                                   "i310"
                                                   "i308"
                                                   "i306"
                                                   "i304"
                                                   "i302"
                                                   "i300"
                                                   "i299"
                                                   "i297"
                                                   "i295"
                                                   "i293"
                                                   "i291"
                                                   "i289"
                                                   "i287"
                                                   "i285"
                                                   "i283"
                                                   "i281"
                                                   "i278"
                                                   "i276"
                                                   "i274"
                                                   "i272"
                                                   "i270"
                                                   "i268"
                                                   "i266"
                                                   "i264"
                                                   "i262"
                                                   "i260"
                                                   "i258"
                                                   "i256"
                                                   "i254"
                                                   "i252"
                                                   "i250"
                                                   "i248"
                                                   "i246"
                                                   "i244"))
                                                #(ribcage
                                                  (define-structure
                                                    define-expansion-accessors
                                                    define-expansion-constructors
                                                    and-map*)
                                                  ((top) (top) (top) (top))
                                                  ("i38" "i37" "i36" "i34")))
                                               (hygiene guile)))
                                        'eval
                                        (if (#{free-id=?\ 446}#
                                              #{x\ 1538}#
                                              '#(syntax-object
                                                 expand
                                                 ((top)
                                                  #(ribcage () () ())
                                                  #(ribcage () () ())
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(x)
                                                    #((top))
                                                    #("i1537"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(f when-list situations)
                                                    #((top) (top) (top))
                                                    #("i1531" "i1532" "i1533"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(e when-list w)
                                                    #((top) (top) (top))
                                                    #("i1528" "i1529" "i1530"))
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
                                                      build-dynlet
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
                                                    ("i501"
                                                     "i499"
                                                     "i497"
                                                     "i495"
                                                     "i493"
                                                     "i491"
                                                     "i489"
                                                     "i487"
                                                     "i485"
                                                     "i483"
                                                     "i481"
                                                     "i479"
                                                     "i477"
                                                     "i475"
                                                     "i473"
                                                     "i471"
                                                     "i469"
                                                     "i467"
                                                     "i465"
                                                     "i463"
                                                     "i461"
                                                     "i459"
                                                     "i457"
                                                     "i455"
                                                     "i453"
                                                     "i451"
                                                     "i449"
                                                     "i447"
                                                     "i445"
                                                     "i443"
                                                     "i441"
                                                     "i439"
                                                     "i437"
                                                     "i435"
                                                     "i433"
                                                     "i431"
                                                     "i430"
                                                     "i429"
                                                     "i427"
                                                     "i426"
                                                     "i425"
                                                     "i424"
                                                     "i423"
                                                     "i421"
                                                     "i419"
                                                     "i417"
                                                     "i415"
                                                     "i413"
                                                     "i411"
                                                     "i409"
                                                     "i407"
                                                     "i404"
                                                     "i402"
                                                     "i401"
                                                     "i400"
                                                     "i399"
                                                     "i398"
                                                     "i397"
                                                     "i395"
                                                     "i393"
                                                     "i391"
                                                     "i389"
                                                     "i388"
                                                     "i386"
                                                     "i384"
                                                     "i382"
                                                     "i380"
                                                     "i378"
                                                     "i376"
                                                     "i374"
                                                     "i373"
                                                     "i371"
                                                     "i369"
                                                     "i368"
                                                     "i367"
                                                     "i365"
                                                     "i364"
                                                     "i362"
                                                     "i360"
                                                     "i358"
                                                     "i356"
                                                     "i354"
                                                     "i352"
                                                     "i350"
                                                     "i348"
                                                     "i346"
                                                     "i344"
                                                     "i342"
                                                     "i340"
                                                     "i338"
                                                     "i336"
                                                     "i334"
                                                     "i332"
                                                     "i330"
                                                     "i328"
                                                     "i326"
                                                     "i324"
                                                     "i322"
                                                     "i320"
                                                     "i318"
                                                     "i316"
                                                     "i314"
                                                     "i312"
                                                     "i310"
                                                     "i308"
                                                     "i306"
                                                     "i304"
                                                     "i302"
                                                     "i300"
                                                     "i299"
                                                     "i297"
                                                     "i295"
                                                     "i293"
                                                     "i291"
                                                     "i289"
                                                     "i287"
                                                     "i285"
                                                     "i283"
                                                     "i281"
                                                     "i278"
                                                     "i276"
                                                     "i274"
                                                     "i272"
                                                     "i270"
                                                     "i268"
                                                     "i266"
                                                     "i264"
                                                     "i262"
                                                     "i260"
                                                     "i258"
                                                     "i256"
                                                     "i254"
                                                     "i252"
                                                     "i250"
                                                     "i248"
                                                     "i246"
                                                     "i244"))
                                                  #(ribcage
                                                    (define-structure
                                                      define-expansion-accessors
                                                      define-expansion-constructors
                                                      and-map*)
                                                    ((top) (top) (top) (top))
                                                    ("i38" "i37" "i36" "i34")))
                                                 (hygiene guile)))
                                          'expand
                                          (syntax-violation
                                            'eval-when
                                            "invalid situation"
                                            #{e\ 1525}#
                                            (#{wrap\ 456}#
                                              #{x\ 1538}#
                                              #{w\ 1527}#
                                              #f))))))))
                              #{situations\ 1536}#))))))
               (begin
                 (#{f\ 1534}# #{when-list\ 1526}# (quote ()))))))
         (#{syntax-type\ 468}#
           (lambda (#{e\ 1548}#
                    #{r\ 1549}#
                    #{w\ 1550}#
                    #{s\ 1551}#
                    #{rib\ 1552}#
                    #{mod\ 1553}#
                    #{for-car?\ 1554}#)
             (if (symbol? #{e\ 1548}#)
               (begin
                 (let ((#{n\ 1566}#
                         (#{id-var-name\ 444}# #{e\ 1548}# #{w\ 1550}#)))
                   (begin
                     (let ((#{b\ 1568}#
                             (#{lookup\ 381}#
                               #{n\ 1566}#
                               #{r\ 1549}#
                               #{mod\ 1553}#)))
                       (begin
                         (let ((#{type\ 1570}#
                                 (#{binding-type\ 370}# #{b\ 1568}#)))
                           (if (eqv? #{type\ 1570}# (quote lexical))
                             (values
                               #{type\ 1570}#
                               (#{binding-value\ 372}# #{b\ 1568}#)
                               #{e\ 1548}#
                               #{w\ 1550}#
                               #{s\ 1551}#
                               #{mod\ 1553}#)
                             (if (eqv? #{type\ 1570}# (quote global))
                               (values
                                 #{type\ 1570}#
                                 #{n\ 1566}#
                                 #{e\ 1548}#
                                 #{w\ 1550}#
                                 #{s\ 1551}#
                                 #{mod\ 1553}#)
                               (if (eqv? #{type\ 1570}# (quote macro))
                                 (if #{for-car?\ 1554}#
                                   (values
                                     #{type\ 1570}#
                                     (#{binding-value\ 372}# #{b\ 1568}#)
                                     #{e\ 1548}#
                                     #{w\ 1550}#
                                     #{s\ 1551}#
                                     #{mod\ 1553}#)
                                   (#{syntax-type\ 468}#
                                     (#{chi-macro\ 478}#
                                       (#{binding-value\ 372}# #{b\ 1568}#)
                                       #{e\ 1548}#
                                       #{r\ 1549}#
                                       #{w\ 1550}#
                                       #{s\ 1551}#
                                       #{rib\ 1552}#
                                       #{mod\ 1553}#)
                                     #{r\ 1549}#
                                     '(())
                                     #{s\ 1551}#
                                     #{rib\ 1552}#
                                     #{mod\ 1553}#
                                     #f))
                                 (values
                                   #{type\ 1570}#
                                   (#{binding-value\ 372}# #{b\ 1568}#)
                                   #{e\ 1548}#
                                   #{w\ 1550}#
                                   #{s\ 1551}#
                                   #{mod\ 1553}#))))))))))
               (if (pair? #{e\ 1548}#)
                 (begin
                   (let ((#{first\ 1579}# (car #{e\ 1548}#)))
                     (call-with-values
                       (lambda ()
                         (#{syntax-type\ 468}#
                           #{first\ 1579}#
                           #{r\ 1549}#
                           #{w\ 1550}#
                           #{s\ 1551}#
                           #{rib\ 1552}#
                           #{mod\ 1553}#
                           #t))
                       (lambda (#{ftype\ 1580}#
                                #{fval\ 1581}#
                                #{fe\ 1582}#
                                #{fw\ 1583}#
                                #{fs\ 1584}#
                                #{fmod\ 1585}#)
                         (if (eqv? #{ftype\ 1580}# (quote lexical))
                           (values
                             'lexical-call
                             #{fval\ 1581}#
                             #{e\ 1548}#
                             #{w\ 1550}#
                             #{s\ 1551}#
                             #{mod\ 1553}#)
                           (if (eqv? #{ftype\ 1580}# (quote global))
                             (values
                               'global-call
                               (#{make-syntax-object\ 349}#
                                 #{fval\ 1581}#
                                 #{w\ 1550}#
                                 #{fmod\ 1585}#)
                               #{e\ 1548}#
                               #{w\ 1550}#
                               #{s\ 1551}#
                               #{mod\ 1553}#)
                             (if (eqv? #{ftype\ 1580}# (quote macro))
                               (#{syntax-type\ 468}#
                                 (#{chi-macro\ 478}#
                                   #{fval\ 1581}#
                                   #{e\ 1548}#
                                   #{r\ 1549}#
                                   #{w\ 1550}#
                                   #{s\ 1551}#
                                   #{rib\ 1552}#
                                   #{mod\ 1553}#)
                                 #{r\ 1549}#
                                 '(())
                                 #{s\ 1551}#
                                 #{rib\ 1552}#
                                 #{mod\ 1553}#
                                 #{for-car?\ 1554}#)
                               (if (eqv? #{ftype\ 1580}# (quote module-ref))
                                 (call-with-values
                                   (lambda ()
                                     (#{fval\ 1581}#
                                       #{e\ 1548}#
                                       #{r\ 1549}#
                                       #{w\ 1550}#))
                                   (lambda (#{e\ 1597}#
                                            #{r\ 1598}#
                                            #{w\ 1599}#
                                            #{s\ 1600}#
                                            #{mod\ 1601}#)
                                     (#{syntax-type\ 468}#
                                       #{e\ 1597}#
                                       #{r\ 1598}#
                                       #{w\ 1599}#
                                       #{s\ 1600}#
                                       #{rib\ 1552}#
                                       #{mod\ 1601}#
                                       #{for-car?\ 1554}#)))
                                 (if (eqv? #{ftype\ 1580}# (quote core))
                                   (values
                                     'core-form
                                     #{fval\ 1581}#
                                     #{e\ 1548}#
                                     #{w\ 1550}#
                                     #{s\ 1551}#
                                     #{mod\ 1553}#)
                                   (if (eqv? #{ftype\ 1580}#
                                             'local-syntax)
                                     (values
                                       'local-syntax-form
                                       #{fval\ 1581}#
                                       #{e\ 1548}#
                                       #{w\ 1550}#
                                       #{s\ 1551}#
                                       #{mod\ 1553}#)
                                     (if (eqv? #{ftype\ 1580}# (quote begin))
                                       (values
                                         'begin-form
                                         #f
                                         #{e\ 1548}#
                                         #{w\ 1550}#
                                         #{s\ 1551}#
                                         #{mod\ 1553}#)
                                       (if (eqv? #{ftype\ 1580}#
                                                 'eval-when)
                                         (values
                                           'eval-when-form
                                           #f
                                           #{e\ 1548}#
                                           #{w\ 1550}#
                                           #{s\ 1551}#
                                           #{mod\ 1553}#)
                                         (if (eqv? #{ftype\ 1580}#
                                                   'define)
                                           (let ((#{tmp\ 1612}# #{e\ 1548}#))
                                             (let ((#{tmp\ 1613}#
                                                     ($sc-dispatch
                                                       #{tmp\ 1612}#
                                                       '(any any any))))
                                               (if (if #{tmp\ 1613}#
                                                     (@apply
                                                       (lambda (#{_\ 1617}#
                                                                #{name\ 1618}#
                                                                #{val\ 1619}#)
                                                         (#{id?\ 387}#
                                                           #{name\ 1618}#))
                                                       #{tmp\ 1613}#)
                                                     #f)
                                                 (@apply
                                                   (lambda (#{_\ 1623}#
                                                            #{name\ 1624}#
                                                            #{val\ 1625}#)
                                                     (values
                                                       'define-form
                                                       #{name\ 1624}#
                                                       #{val\ 1625}#
                                                       #{w\ 1550}#
                                                       #{s\ 1551}#
                                                       #{mod\ 1553}#))
                                                   #{tmp\ 1613}#)
                                                 (let ((#{tmp\ 1626}#
                                                         ($sc-dispatch
                                                           #{tmp\ 1612}#
                                                           '(any (any . any)
                                                                 any
                                                                 .
                                                                 each-any))))
                                                   (if (if #{tmp\ 1626}#
                                                         (@apply
                                                           (lambda (#{_\ 1632}#
                                                                    #{name\ 1633}#
                                                                    #{args\ 1634}#
                                                                    #{e1\ 1635}#
                                                                    #{e2\ 1636}#)
                                                             (if (#{id?\ 387}#
                                                                   #{name\ 1633}#)
                                                               (#{valid-bound-ids?\ 450}#
                                                                 (#{lambda-var-list\ 502}#
                                                                   #{args\ 1634}#))
                                                               #f))
                                                           #{tmp\ 1626}#)
                                                         #f)
                                                     (@apply
                                                       (lambda (#{_\ 1644}#
                                                                #{name\ 1645}#
                                                                #{args\ 1646}#
                                                                #{e1\ 1647}#
                                                                #{e2\ 1648}#)
                                                         (values
                                                           'define-form
                                                           (#{wrap\ 456}#
                                                             #{name\ 1645}#
                                                             #{w\ 1550}#
                                                             #{mod\ 1553}#)
                                                           (#{decorate-source\ 305}#
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
                                                                         #("i1639"
                                                                           "i1640"
                                                                           "i1641"
                                                                           "i1642"
                                                                           "i1643"))
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
                                                                         #("i1586"
                                                                           "i1587"
                                                                           "i1588"
                                                                           "i1589"
                                                                           "i1590"
                                                                           "i1591"))
                                                                       #(ribcage
                                                                         ()
                                                                         ()
                                                                         ())
                                                                       #(ribcage
                                                                         #(first)
                                                                         #((top))
                                                                         #("i1578"))
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
                                                                         #("i1555"
                                                                           "i1556"
                                                                           "i1557"
                                                                           "i1558"
                                                                           "i1559"
                                                                           "i1560"
                                                                           "i1561"))
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
                                                                           build-dynlet
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
                                                                         ("i501"
                                                                          "i499"
                                                                          "i497"
                                                                          "i495"
                                                                          "i493"
                                                                          "i491"
                                                                          "i489"
                                                                          "i487"
                                                                          "i485"
                                                                          "i483"
                                                                          "i481"
                                                                          "i479"
                                                                          "i477"
                                                                          "i475"
                                                                          "i473"
                                                                          "i471"
                                                                          "i469"
                                                                          "i467"
                                                                          "i465"
                                                                          "i463"
                                                                          "i461"
                                                                          "i459"
                                                                          "i457"
                                                                          "i455"
                                                                          "i453"
                                                                          "i451"
                                                                          "i449"
                                                                          "i447"
                                                                          "i445"
                                                                          "i443"
                                                                          "i441"
                                                                          "i439"
                                                                          "i437"
                                                                          "i435"
                                                                          "i433"
                                                                          "i431"
                                                                          "i430"
                                                                          "i429"
                                                                          "i427"
                                                                          "i426"
                                                                          "i425"
                                                                          "i424"
                                                                          "i423"
                                                                          "i421"
                                                                          "i419"
                                                                          "i417"
                                                                          "i415"
                                                                          "i413"
                                                                          "i411"
                                                                          "i409"
                                                                          "i407"
                                                                          "i404"
                                                                          "i402"
                                                                          "i401"
                                                                          "i400"
                                                                          "i399"
                                                                          "i398"
                                                                          "i397"
                                                                          "i395"
                                                                          "i393"
                                                                          "i391"
                                                                          "i389"
                                                                          "i388"
                                                                          "i386"
                                                                          "i384"
                                                                          "i382"
                                                                          "i380"
                                                                          "i378"
                                                                          "i376"
                                                                          "i374"
                                                                          "i373"
                                                                          "i371"
                                                                          "i369"
                                                                          "i368"
                                                                          "i367"
                                                                          "i365"
                                                                          "i364"
                                                                          "i362"
                                                                          "i360"
                                                                          "i358"
                                                                          "i356"
                                                                          "i354"
                                                                          "i352"
                                                                          "i350"
                                                                          "i348"
                                                                          "i346"
                                                                          "i344"
                                                                          "i342"
                                                                          "i340"
                                                                          "i338"
                                                                          "i336"
                                                                          "i334"
                                                                          "i332"
                                                                          "i330"
                                                                          "i328"
                                                                          "i326"
                                                                          "i324"
                                                                          "i322"
                                                                          "i320"
                                                                          "i318"
                                                                          "i316"
                                                                          "i314"
                                                                          "i312"
                                                                          "i310"
                                                                          "i308"
                                                                          "i306"
                                                                          "i304"
                                                                          "i302"
                                                                          "i300"
                                                                          "i299"
                                                                          "i297"
                                                                          "i295"
                                                                          "i293"
                                                                          "i291"
                                                                          "i289"
                                                                          "i287"
                                                                          "i285"
                                                                          "i283"
                                                                          "i281"
                                                                          "i278"
                                                                          "i276"
                                                                          "i274"
                                                                          "i272"
                                                                          "i270"
                                                                          "i268"
                                                                          "i266"
                                                                          "i264"
                                                                          "i262"
                                                                          "i260"
                                                                          "i258"
                                                                          "i256"
                                                                          "i254"
                                                                          "i252"
                                                                          "i250"
                                                                          "i248"
                                                                          "i246"
                                                                          "i244"))
                                                                       #(ribcage
                                                                         (define-structure
                                                                           define-expansion-accessors
                                                                           define-expansion-constructors
                                                                           and-map*)
                                                                         ((top)
                                                                          (top)
                                                                          (top)
                                                                          (top))
                                                                         ("i38"
                                                                          "i37"
                                                                          "i36"
                                                                          "i34")))
                                                                      (hygiene
                                                                        guile))
                                                                   (#{wrap\ 456}#
                                                                     (cons #{args\ 1646}#
                                                                           (cons #{e1\ 1647}#
                                                                                 #{e2\ 1648}#))
                                                                     #{w\ 1550}#
                                                                     #{mod\ 1553}#))
                                                             #{s\ 1551}#)
                                                           '(())
                                                           #{s\ 1551}#
                                                           #{mod\ 1553}#))
                                                       #{tmp\ 1626}#)
                                                     (let ((#{tmp\ 1651}#
                                                             ($sc-dispatch
                                                               #{tmp\ 1612}#
                                                               '(any any))))
                                                       (if (if #{tmp\ 1651}#
                                                             (@apply
                                                               (lambda (#{_\ 1654}#
                                                                        #{name\ 1655}#)
                                                                 (#{id?\ 387}#
                                                                   #{name\ 1655}#))
                                                               #{tmp\ 1651}#)
                                                             #f)
                                                         (@apply
                                                           (lambda (#{_\ 1658}#
                                                                    #{name\ 1659}#)
                                                             (values
                                                               'define-form
                                                               (#{wrap\ 456}#
                                                                 #{name\ 1659}#
                                                                 #{w\ 1550}#
                                                                 #{mod\ 1553}#)
                                                               '(#(syntax-object
                                                                   if
                                                                   ((top)
                                                                    #(ribcage
                                                                      #(_ name)
                                                                      #((top)
                                                                        (top))
                                                                      #("i1656"
                                                                        "i1657"))
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
                                                                      #("i1586"
                                                                        "i1587"
                                                                        "i1588"
                                                                        "i1589"
                                                                        "i1590"
                                                                        "i1591"))
                                                                    #(ribcage
                                                                      ()
                                                                      ()
                                                                      ())
                                                                    #(ribcage
                                                                      #(first)
                                                                      #((top))
                                                                      #("i1578"))
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
                                                                      #("i1555"
                                                                        "i1556"
                                                                        "i1557"
                                                                        "i1558"
                                                                        "i1559"
                                                                        "i1560"
                                                                        "i1561"))
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
                                                                        build-dynlet
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
                                                                      ("i501"
                                                                       "i499"
                                                                       "i497"
                                                                       "i495"
                                                                       "i493"
                                                                       "i491"
                                                                       "i489"
                                                                       "i487"
                                                                       "i485"
                                                                       "i483"
                                                                       "i481"
                                                                       "i479"
                                                                       "i477"
                                                                       "i475"
                                                                       "i473"
                                                                       "i471"
                                                                       "i469"
                                                                       "i467"
                                                                       "i465"
                                                                       "i463"
                                                                       "i461"
                                                                       "i459"
                                                                       "i457"
                                                                       "i455"
                                                                       "i453"
                                                                       "i451"
                                                                       "i449"
                                                                       "i447"
                                                                       "i445"
                                                                       "i443"
                                                                       "i441"
                                                                       "i439"
                                                                       "i437"
                                                                       "i435"
                                                                       "i433"
                                                                       "i431"
                                                                       "i430"
                                                                       "i429"
                                                                       "i427"
                                                                       "i426"
                                                                       "i425"
                                                                       "i424"
                                                                       "i423"
                                                                       "i421"
                                                                       "i419"
                                                                       "i417"
                                                                       "i415"
                                                                       "i413"
                                                                       "i411"
                                                                       "i409"
                                                                       "i407"
                                                                       "i404"
                                                                       "i402"
                                                                       "i401"
                                                                       "i400"
                                                                       "i399"
                                                                       "i398"
                                                                       "i397"
                                                                       "i395"
                                                                       "i393"
                                                                       "i391"
                                                                       "i389"
                                                                       "i388"
                                                                       "i386"
                                                                       "i384"
                                                                       "i382"
                                                                       "i380"
                                                                       "i378"
                                                                       "i376"
                                                                       "i374"
                                                                       "i373"
                                                                       "i371"
                                                                       "i369"
                                                                       "i368"
                                                                       "i367"
                                                                       "i365"
                                                                       "i364"
                                                                       "i362"
                                                                       "i360"
                                                                       "i358"
                                                                       "i356"
                                                                       "i354"
                                                                       "i352"
                                                                       "i350"
                                                                       "i348"
                                                                       "i346"
                                                                       "i344"
                                                                       "i342"
                                                                       "i340"
                                                                       "i338"
                                                                       "i336"
                                                                       "i334"
                                                                       "i332"
                                                                       "i330"
                                                                       "i328"
                                                                       "i326"
                                                                       "i324"
                                                                       "i322"
                                                                       "i320"
                                                                       "i318"
                                                                       "i316"
                                                                       "i314"
                                                                       "i312"
                                                                       "i310"
                                                                       "i308"
                                                                       "i306"
                                                                       "i304"
                                                                       "i302"
                                                                       "i300"
                                                                       "i299"
                                                                       "i297"
                                                                       "i295"
                                                                       "i293"
                                                                       "i291"
                                                                       "i289"
                                                                       "i287"
                                                                       "i285"
                                                                       "i283"
                                                                       "i281"
                                                                       "i278"
                                                                       "i276"
                                                                       "i274"
                                                                       "i272"
                                                                       "i270"
                                                                       "i268"
                                                                       "i266"
                                                                       "i264"
                                                                       "i262"
                                                                       "i260"
                                                                       "i258"
                                                                       "i256"
                                                                       "i254"
                                                                       "i252"
                                                                       "i250"
                                                                       "i248"
                                                                       "i246"
                                                                       "i244"))
                                                                    #(ribcage
                                                                      (define-structure
                                                                        define-expansion-accessors
                                                                        define-expansion-constructors
                                                                        and-map*)
                                                                      ((top)
                                                                       (top)
                                                                       (top)
                                                                       (top))
                                                                      ("i38"
                                                                       "i37"
                                                                       "i36"
                                                                       "i34")))
                                                                   (hygiene
                                                                     guile))
                                                                 #(syntax-object
                                                                   #f
                                                                   ((top)
                                                                    #(ribcage
                                                                      #(_ name)
                                                                      #((top)
                                                                        (top))
                                                                      #("i1656"
                                                                        "i1657"))
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
                                                                      #("i1586"
                                                                        "i1587"
                                                                        "i1588"
                                                                        "i1589"
                                                                        "i1590"
                                                                        "i1591"))
                                                                    #(ribcage
                                                                      ()
                                                                      ()
                                                                      ())
                                                                    #(ribcage
                                                                      #(first)
                                                                      #((top))
                                                                      #("i1578"))
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
                                                                      #("i1555"
                                                                        "i1556"
                                                                        "i1557"
                                                                        "i1558"
                                                                        "i1559"
                                                                        "i1560"
                                                                        "i1561"))
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
                                                                        build-dynlet
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
                                                                      ("i501"
                                                                       "i499"
                                                                       "i497"
                                                                       "i495"
                                                                       "i493"
                                                                       "i491"
                                                                       "i489"
                                                                       "i487"
                                                                       "i485"
                                                                       "i483"
                                                                       "i481"
                                                                       "i479"
                                                                       "i477"
                                                                       "i475"
                                                                       "i473"
                                                                       "i471"
                                                                       "i469"
                                                                       "i467"
                                                                       "i465"
                                                                       "i463"
                                                                       "i461"
                                                                       "i459"
                                                                       "i457"
                                                                       "i455"
                                                                       "i453"
                                                                       "i451"
                                                                       "i449"
                                                                       "i447"
                                                                       "i445"
                                                                       "i443"
                                                                       "i441"
                                                                       "i439"
                                                                       "i437"
                                                                       "i435"
                                                                       "i433"
                                                                       "i431"
                                                                       "i430"
                                                                       "i429"
                                                                       "i427"
                                                                       "i426"
                                                                       "i425"
                                                                       "i424"
                                                                       "i423"
                                                                       "i421"
                                                                       "i419"
                                                                       "i417"
                                                                       "i415"
                                                                       "i413"
                                                                       "i411"
                                                                       "i409"
                                                                       "i407"
                                                                       "i404"
                                                                       "i402"
                                                                       "i401"
                                                                       "i400"
                                                                       "i399"
                                                                       "i398"
                                                                       "i397"
                                                                       "i395"
                                                                       "i393"
                                                                       "i391"
                                                                       "i389"
                                                                       "i388"
                                                                       "i386"
                                                                       "i384"
                                                                       "i382"
                                                                       "i380"
                                                                       "i378"
                                                                       "i376"
                                                                       "i374"
                                                                       "i373"
                                                                       "i371"
                                                                       "i369"
                                                                       "i368"
                                                                       "i367"
                                                                       "i365"
                                                                       "i364"
                                                                       "i362"
                                                                       "i360"
                                                                       "i358"
                                                                       "i356"
                                                                       "i354"
                                                                       "i352"
                                                                       "i350"
                                                                       "i348"
                                                                       "i346"
                                                                       "i344"
                                                                       "i342"
                                                                       "i340"
                                                                       "i338"
                                                                       "i336"
                                                                       "i334"
                                                                       "i332"
                                                                       "i330"
                                                                       "i328"
                                                                       "i326"
                                                                       "i324"
                                                                       "i322"
                                                                       "i320"
                                                                       "i318"
                                                                       "i316"
                                                                       "i314"
                                                                       "i312"
                                                                       "i310"
                                                                       "i308"
                                                                       "i306"
                                                                       "i304"
                                                                       "i302"
                                                                       "i300"
                                                                       "i299"
                                                                       "i297"
                                                                       "i295"
                                                                       "i293"
                                                                       "i291"
                                                                       "i289"
                                                                       "i287"
                                                                       "i285"
                                                                       "i283"
                                                                       "i281"
                                                                       "i278"
                                                                       "i276"
                                                                       "i274"
                                                                       "i272"
                                                                       "i270"
                                                                       "i268"
                                                                       "i266"
                                                                       "i264"
                                                                       "i262"
                                                                       "i260"
                                                                       "i258"
                                                                       "i256"
                                                                       "i254"
                                                                       "i252"
                                                                       "i250"
                                                                       "i248"
                                                                       "i246"
                                                                       "i244"))
                                                                    #(ribcage
                                                                      (define-structure
                                                                        define-expansion-accessors
                                                                        define-expansion-constructors
                                                                        and-map*)
                                                                      ((top)
                                                                       (top)
                                                                       (top)
                                                                       (top))
                                                                      ("i38"
                                                                       "i37"
                                                                       "i36"
                                                                       "i34")))
                                                                   (hygiene
                                                                     guile))
                                                                 #(syntax-object
                                                                   #f
                                                                   ((top)
                                                                    #(ribcage
                                                                      #(_ name)
                                                                      #((top)
                                                                        (top))
                                                                      #("i1656"
                                                                        "i1657"))
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
                                                                      #("i1586"
                                                                        "i1587"
                                                                        "i1588"
                                                                        "i1589"
                                                                        "i1590"
                                                                        "i1591"))
                                                                    #(ribcage
                                                                      ()
                                                                      ()
                                                                      ())
                                                                    #(ribcage
                                                                      #(first)
                                                                      #((top))
                                                                      #("i1578"))
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
                                                                      #("i1555"
                                                                        "i1556"
                                                                        "i1557"
                                                                        "i1558"
                                                                        "i1559"
                                                                        "i1560"
                                                                        "i1561"))
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
                                                                        build-dynlet
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
                                                                      ("i501"
                                                                       "i499"
                                                                       "i497"
                                                                       "i495"
                                                                       "i493"
                                                                       "i491"
                                                                       "i489"
                                                                       "i487"
                                                                       "i485"
                                                                       "i483"
                                                                       "i481"
                                                                       "i479"
                                                                       "i477"
                                                                       "i475"
                                                                       "i473"
                                                                       "i471"
                                                                       "i469"
                                                                       "i467"
                                                                       "i465"
                                                                       "i463"
                                                                       "i461"
                                                                       "i459"
                                                                       "i457"
                                                                       "i455"
                                                                       "i453"
                                                                       "i451"
                                                                       "i449"
                                                                       "i447"
                                                                       "i445"
                                                                       "i443"
                                                                       "i441"
                                                                       "i439"
                                                                       "i437"
                                                                       "i435"
                                                                       "i433"
                                                                       "i431"
                                                                       "i430"
                                                                       "i429"
                                                                       "i427"
                                                                       "i426"
                                                                       "i425"
                                                                       "i424"
                                                                       "i423"
                                                                       "i421"
                                                                       "i419"
                                                                       "i417"
                                                                       "i415"
                                                                       "i413"
                                                                       "i411"
                                                                       "i409"
                                                                       "i407"
                                                                       "i404"
                                                                       "i402"
                                                                       "i401"
                                                                       "i400"
                                                                       "i399"
                                                                       "i398"
                                                                       "i397"
                                                                       "i395"
                                                                       "i393"
                                                                       "i391"
                                                                       "i389"
                                                                       "i388"
                                                                       "i386"
                                                                       "i384"
                                                                       "i382"
                                                                       "i380"
                                                                       "i378"
                                                                       "i376"
                                                                       "i374"
                                                                       "i373"
                                                                       "i371"
                                                                       "i369"
                                                                       "i368"
                                                                       "i367"
                                                                       "i365"
                                                                       "i364"
                                                                       "i362"
                                                                       "i360"
                                                                       "i358"
                                                                       "i356"
                                                                       "i354"
                                                                       "i352"
                                                                       "i350"
                                                                       "i348"
                                                                       "i346"
                                                                       "i344"
                                                                       "i342"
                                                                       "i340"
                                                                       "i338"
                                                                       "i336"
                                                                       "i334"
                                                                       "i332"
                                                                       "i330"
                                                                       "i328"
                                                                       "i326"
                                                                       "i324"
                                                                       "i322"
                                                                       "i320"
                                                                       "i318"
                                                                       "i316"
                                                                       "i314"
                                                                       "i312"
                                                                       "i310"
                                                                       "i308"
                                                                       "i306"
                                                                       "i304"
                                                                       "i302"
                                                                       "i300"
                                                                       "i299"
                                                                       "i297"
                                                                       "i295"
                                                                       "i293"
                                                                       "i291"
                                                                       "i289"
                                                                       "i287"
                                                                       "i285"
                                                                       "i283"
                                                                       "i281"
                                                                       "i278"
                                                                       "i276"
                                                                       "i274"
                                                                       "i272"
                                                                       "i270"
                                                                       "i268"
                                                                       "i266"
                                                                       "i264"
                                                                       "i262"
                                                                       "i260"
                                                                       "i258"
                                                                       "i256"
                                                                       "i254"
                                                                       "i252"
                                                                       "i250"
                                                                       "i248"
                                                                       "i246"
                                                                       "i244"))
                                                                    #(ribcage
                                                                      (define-structure
                                                                        define-expansion-accessors
                                                                        define-expansion-constructors
                                                                        and-map*)
                                                                      ((top)
                                                                       (top)
                                                                       (top)
                                                                       (top))
                                                                      ("i38"
                                                                       "i37"
                                                                       "i36"
                                                                       "i34")))
                                                                   (hygiene
                                                                     guile)))
                                                               '(())
                                                               #{s\ 1551}#
                                                               #{mod\ 1553}#))
                                                           #{tmp\ 1651}#)
                                                         (syntax-violation
                                                           #f
                                                           "source expression failed to match any pattern"
                                                           #{tmp\ 1612}#))))))))
                                           (if (eqv? #{ftype\ 1580}#
                                                     'define-syntax)
                                             (let ((#{tmp\ 1662}# #{e\ 1548}#))
                                               (let ((#{tmp\ 1663}#
                                                       ($sc-dispatch
                                                         #{tmp\ 1662}#
                                                         '(any any any))))
                                                 (if (if #{tmp\ 1663}#
                                                       (@apply
                                                         (lambda (#{_\ 1667}#
                                                                  #{name\ 1668}#
                                                                  #{val\ 1669}#)
                                                           (#{id?\ 387}#
                                                             #{name\ 1668}#))
                                                         #{tmp\ 1663}#)
                                                       #f)
                                                   (@apply
                                                     (lambda (#{_\ 1673}#
                                                              #{name\ 1674}#
                                                              #{val\ 1675}#)
                                                       (values
                                                         'define-syntax-form
                                                         #{name\ 1674}#
                                                         #{val\ 1675}#
                                                         #{w\ 1550}#
                                                         #{s\ 1551}#
                                                         #{mod\ 1553}#))
                                                     #{tmp\ 1663}#)
                                                   (syntax-violation
                                                     #f
                                                     "source expression failed to match any pattern"
                                                     #{tmp\ 1662}#))))
                                             (values
                                               'call
                                               #f
                                               #{e\ 1548}#
                                               #{w\ 1550}#
                                               #{s\ 1551}#
                                               #{mod\ 1553}#)))))))))))))))
                 (if (#{syntax-object?\ 351}# #{e\ 1548}#)
                   (#{syntax-type\ 468}#
                     (#{syntax-object-expression\ 353}# #{e\ 1548}#)
                     #{r\ 1549}#
                     (#{join-wraps\ 438}#
                       #{w\ 1550}#
                       (#{syntax-object-wrap\ 355}# #{e\ 1548}#))
                     (begin
                       (let ((#{t\ 1681}#
                               (#{source-annotation\ 366}# #{e\ 1548}#)))
                         (if #{t\ 1681}# #{t\ 1681}# #{s\ 1551}#)))
                     #{rib\ 1552}#
                     (begin
                       (let ((#{t\ 1685}#
                               (#{syntax-object-module\ 357}# #{e\ 1548}#)))
                         (if #{t\ 1685}# #{t\ 1685}# #{mod\ 1553}#)))
                     #{for-car?\ 1554}#)
                   (if (self-evaluating? #{e\ 1548}#)
                     (values
                       'constant
                       #f
                       #{e\ 1548}#
                       #{w\ 1550}#
                       #{s\ 1551}#
                       #{mod\ 1553}#)
                     (values
                       'other
                       #f
                       #{e\ 1548}#
                       #{w\ 1550}#
                       #{s\ 1551}#
                       #{mod\ 1553}#)))))))
         (#{chi-top\ 470}#
           (lambda (#{e\ 1690}#
                    #{r\ 1691}#
                    #{w\ 1692}#
                    #{m\ 1693}#
                    #{esew\ 1694}#
                    #{mod\ 1695}#)
             (call-with-values
               (lambda ()
                 (#{syntax-type\ 468}#
                   #{e\ 1690}#
                   #{r\ 1691}#
                   #{w\ 1692}#
                   (#{source-annotation\ 366}# #{e\ 1690}#)
                   #f
                   #{mod\ 1695}#
                   #f))
               (lambda (#{type\ 1716}#
                        #{value\ 1717}#
                        #{e\ 1718}#
                        #{w\ 1719}#
                        #{s\ 1720}#
                        #{mod\ 1721}#)
                 (if (eqv? #{type\ 1716}# (quote begin-form))
                   (let ((#{tmp\ 1729}# #{e\ 1718}#))
                     (let ((#{tmp\ 1730}#
                             ($sc-dispatch #{tmp\ 1729}# (quote (any)))))
                       (if #{tmp\ 1730}#
                         (@apply
                           (lambda (#{_\ 1732}#) (#{chi-void\ 486}#))
                           #{tmp\ 1730}#)
                         (let ((#{tmp\ 1733}#
                                 ($sc-dispatch
                                   #{tmp\ 1729}#
                                   '(any any . each-any))))
                           (if #{tmp\ 1733}#
                             (@apply
                               (lambda (#{_\ 1737}# #{e1\ 1738}# #{e2\ 1739}#)
                                 (#{chi-top-sequence\ 462}#
                                   (cons #{e1\ 1738}# #{e2\ 1739}#)
                                   #{r\ 1691}#
                                   #{w\ 1719}#
                                   #{s\ 1720}#
                                   #{m\ 1693}#
                                   #{esew\ 1694}#
                                   #{mod\ 1721}#))
                               #{tmp\ 1733}#)
                             (syntax-violation
                               #f
                               "source expression failed to match any pattern"
                               #{tmp\ 1729}#))))))
                   (if (eqv? #{type\ 1716}# (quote local-syntax-form))
                     (#{chi-local-syntax\ 482}#
                       #{value\ 1717}#
                       #{e\ 1718}#
                       #{r\ 1691}#
                       #{w\ 1719}#
                       #{s\ 1720}#
                       #{mod\ 1721}#
                       (lambda (#{body\ 1742}#
                                #{r\ 1743}#
                                #{w\ 1744}#
                                #{s\ 1745}#
                                #{mod\ 1746}#)
                         (#{chi-top-sequence\ 462}#
                           #{body\ 1742}#
                           #{r\ 1743}#
                           #{w\ 1744}#
                           #{s\ 1745}#
                           #{m\ 1693}#
                           #{esew\ 1694}#
                           #{mod\ 1746}#)))
                     (if (eqv? #{type\ 1716}# (quote eval-when-form))
                       (let ((#{tmp\ 1753}# #{e\ 1718}#))
                         (let ((#{tmp\ 1754}#
                                 ($sc-dispatch
                                   #{tmp\ 1753}#
                                   '(any each-any any . each-any))))
                           (if #{tmp\ 1754}#
                             (@apply
                               (lambda (#{_\ 1759}#
                                        #{x\ 1760}#
                                        #{e1\ 1761}#
                                        #{e2\ 1762}#)
                                 (begin
                                   (let ((#{when-list\ 1765}#
                                           (#{chi-when-list\ 466}#
                                             #{e\ 1718}#
                                             #{x\ 1760}#
                                             #{w\ 1719}#))
                                         (#{body\ 1766}#
                                           (cons #{e1\ 1761}# #{e2\ 1762}#)))
                                     (if (eq? #{m\ 1693}# (quote e))
                                       (if (memq 'eval
                                                 #{when-list\ 1765}#)
                                         (#{chi-top-sequence\ 462}#
                                           #{body\ 1766}#
                                           #{r\ 1691}#
                                           #{w\ 1719}#
                                           #{s\ 1720}#
                                           (if (memq 'expand
                                                     #{when-list\ 1765}#)
                                             'c&e
                                             'e)
                                           '(eval)
                                           #{mod\ 1721}#)
                                         (begin
                                           (if (memq 'expand
                                                     #{when-list\ 1765}#)
                                             (#{top-level-eval-hook\ 296}#
                                               (#{chi-top-sequence\ 462}#
                                                 #{body\ 1766}#
                                                 #{r\ 1691}#
                                                 #{w\ 1719}#
                                                 #{s\ 1720}#
                                                 'e
                                                 '(eval)
                                                 #{mod\ 1721}#)
                                               #{mod\ 1721}#))
                                           (#{chi-void\ 486}#)))
                                       (if (memq 'load
                                                 #{when-list\ 1765}#)
                                         (if (begin
                                               (let ((#{t\ 1775}#
                                                       (memq 'compile
                                                             #{when-list\ 1765}#)))
                                                 (if #{t\ 1775}#
                                                   #{t\ 1775}#
                                                   (begin
                                                     (let ((#{t\ 1778}#
                                                             (memq 'expand
                                                                   #{when-list\ 1765}#)))
                                                       (if #{t\ 1778}#
                                                         #{t\ 1778}#
                                                         (if (eq? #{m\ 1693}#
                                                                  'c&e)
                                                           (memq 'eval
                                                                 #{when-list\ 1765}#)
                                                           #f)))))))
                                           (#{chi-top-sequence\ 462}#
                                             #{body\ 1766}#
                                             #{r\ 1691}#
                                             #{w\ 1719}#
                                             #{s\ 1720}#
                                             'c&e
                                             '(compile load)
                                             #{mod\ 1721}#)
                                           (if (if (eq? #{m\ 1693}# (quote c))
                                                 #t
                                                 (eq? #{m\ 1693}# (quote c&e)))
                                             (#{chi-top-sequence\ 462}#
                                               #{body\ 1766}#
                                               #{r\ 1691}#
                                               #{w\ 1719}#
                                               #{s\ 1720}#
                                               'c
                                               '(load)
                                               #{mod\ 1721}#)
                                             (#{chi-void\ 486}#)))
                                         (if (begin
                                               (let ((#{t\ 1786}#
                                                       (memq 'compile
                                                             #{when-list\ 1765}#)))
                                                 (if #{t\ 1786}#
                                                   #{t\ 1786}#
                                                   (begin
                                                     (let ((#{t\ 1789}#
                                                             (memq 'expand
                                                                   #{when-list\ 1765}#)))
                                                       (if #{t\ 1789}#
                                                         #{t\ 1789}#
                                                         (if (eq? #{m\ 1693}#
                                                                  'c&e)
                                                           (memq 'eval
                                                                 #{when-list\ 1765}#)
                                                           #f)))))))
                                           (begin
                                             (#{top-level-eval-hook\ 296}#
                                               (#{chi-top-sequence\ 462}#
                                                 #{body\ 1766}#
                                                 #{r\ 1691}#
                                                 #{w\ 1719}#
                                                 #{s\ 1720}#
                                                 'e
                                                 '(eval)
                                                 #{mod\ 1721}#)
                                               #{mod\ 1721}#)
                                             (#{chi-void\ 486}#))
                                           (#{chi-void\ 486}#)))))))
                               #{tmp\ 1754}#)
                             (syntax-violation
                               #f
                               "source expression failed to match any pattern"
                               #{tmp\ 1753}#))))
                       (if (eqv? #{type\ 1716}# (quote define-syntax-form))
                         (begin
                           (let ((#{n\ 1797}#
                                   (#{id-var-name\ 444}#
                                     #{value\ 1717}#
                                     #{w\ 1719}#))
                                 (#{r\ 1798}#
                                   (#{macros-only-env\ 379}# #{r\ 1691}#)))
                             (if (eqv? #{m\ 1693}# (quote c))
                               (if (memq (quote compile) #{esew\ 1694}#)
                                 (begin
                                   (let ((#{e\ 1801}#
                                           (#{chi-install-global\ 464}#
                                             #{n\ 1797}#
                                             (#{chi\ 472}#
                                               #{e\ 1718}#
                                               #{r\ 1798}#
                                               #{w\ 1719}#
                                               #{mod\ 1721}#))))
                                     (begin
                                       (#{top-level-eval-hook\ 296}#
                                         #{e\ 1801}#
                                         #{mod\ 1721}#)
                                       (if (memq (quote load) #{esew\ 1694}#)
                                         #{e\ 1801}#
                                         (#{chi-void\ 486}#)))))
                                 (if (memq (quote load) #{esew\ 1694}#)
                                   (#{chi-install-global\ 464}#
                                     #{n\ 1797}#
                                     (#{chi\ 472}#
                                       #{e\ 1718}#
                                       #{r\ 1798}#
                                       #{w\ 1719}#
                                       #{mod\ 1721}#))
                                   (#{chi-void\ 486}#)))
                               (if (eqv? #{m\ 1693}# (quote c&e))
                                 (begin
                                   (let ((#{e\ 1804}#
                                           (#{chi-install-global\ 464}#
                                             #{n\ 1797}#
                                             (#{chi\ 472}#
                                               #{e\ 1718}#
                                               #{r\ 1798}#
                                               #{w\ 1719}#
                                               #{mod\ 1721}#))))
                                     (begin
                                       (#{top-level-eval-hook\ 296}#
                                         #{e\ 1804}#
                                         #{mod\ 1721}#)
                                       #{e\ 1804}#)))
                                 (begin
                                   (if (memq (quote eval) #{esew\ 1694}#)
                                     (#{top-level-eval-hook\ 296}#
                                       (#{chi-install-global\ 464}#
                                         #{n\ 1797}#
                                         (#{chi\ 472}#
                                           #{e\ 1718}#
                                           #{r\ 1798}#
                                           #{w\ 1719}#
                                           #{mod\ 1721}#))
                                       #{mod\ 1721}#))
                                   (#{chi-void\ 486}#))))))
                         (if (eqv? #{type\ 1716}# (quote define-form))
                           (begin
                             (let ((#{n\ 1809}#
                                     (#{id-var-name\ 444}#
                                       #{value\ 1717}#
                                       #{w\ 1719}#)))
                               (begin
                                 (let ((#{type\ 1811}#
                                         (#{binding-type\ 370}#
                                           (#{lookup\ 381}#
                                             #{n\ 1809}#
                                             #{r\ 1691}#
                                             #{mod\ 1721}#))))
                                   (if (if (eqv? #{type\ 1811}# (quote global))
                                         #t
                                         (if (eqv? #{type\ 1811}# (quote core))
                                           #t
                                           (if (eqv? #{type\ 1811}#
                                                     'macro)
                                             #t
                                             (eqv? #{type\ 1811}#
                                                   'module-ref))))
                                     (begin
                                       (if (if (if (eq? #{m\ 1693}# (quote c))
                                                 #t
                                                 (eq? #{m\ 1693}# (quote c&e)))
                                             (if (not (module-local-variable
                                                        (current-module)
                                                        #{n\ 1809}#))
                                               (current-module)
                                               #f)
                                             #f)
                                         (begin
                                           (let ((#{old\ 1817}#
                                                   (module-variable
                                                     (current-module)
                                                     #{n\ 1809}#)))
                                             (module-define!
                                               (current-module)
                                               #{n\ 1809}#
                                               (if (variable? #{old\ 1817}#)
                                                 (variable-ref #{old\ 1817}#)
                                                 #f)))))
                                       (begin
                                         (let ((#{x\ 1820}#
                                                 (#{build-global-definition\ 327}#
                                                   #{s\ 1720}#
                                                   #{n\ 1809}#
                                                   (#{chi\ 472}#
                                                     #{e\ 1718}#
                                                     #{r\ 1691}#
                                                     #{w\ 1719}#
                                                     #{mod\ 1721}#))))
                                           (begin
                                             (if (eq? #{m\ 1693}# (quote c&e))
                                               (#{top-level-eval-hook\ 296}#
                                                 #{x\ 1820}#
                                                 #{mod\ 1721}#))
                                             #{x\ 1820}#))))
                                     (if (eqv? #{type\ 1811}#
                                               'displaced-lexical)
                                       (syntax-violation
                                         #f
                                         "identifier out of context"
                                         #{e\ 1718}#
                                         (#{wrap\ 456}#
                                           #{value\ 1717}#
                                           #{w\ 1719}#
                                           #{mod\ 1721}#))
                                       (syntax-violation
                                         #f
                                         "cannot define keyword at top level"
                                         #{e\ 1718}#
                                         (#{wrap\ 456}#
                                           #{value\ 1717}#
                                           #{w\ 1719}#
                                           #{mod\ 1721}#))))))))
                           (begin
                             (let ((#{x\ 1826}#
                                     (#{chi-expr\ 474}#
                                       #{type\ 1716}#
                                       #{value\ 1717}#
                                       #{e\ 1718}#
                                       #{r\ 1691}#
                                       #{w\ 1719}#
                                       #{s\ 1720}#
                                       #{mod\ 1721}#)))
                               (begin
                                 (if (eq? #{m\ 1693}# (quote c&e))
                                   (#{top-level-eval-hook\ 296}#
                                     #{x\ 1826}#
                                     #{mod\ 1721}#))
                                 #{x\ 1826}#))))))))))))
         (#{chi\ 472}#
           (lambda (#{e\ 1827}#
                    #{r\ 1828}#
                    #{w\ 1829}#
                    #{mod\ 1830}#)
             (call-with-values
               (lambda ()
                 (#{syntax-type\ 468}#
                   #{e\ 1827}#
                   #{r\ 1828}#
                   #{w\ 1829}#
                   (#{source-annotation\ 366}# #{e\ 1827}#)
                   #f
                   #{mod\ 1830}#
                   #f))
               (lambda (#{type\ 1835}#
                        #{value\ 1836}#
                        #{e\ 1837}#
                        #{w\ 1838}#
                        #{s\ 1839}#
                        #{mod\ 1840}#)
                 (#{chi-expr\ 474}#
                   #{type\ 1835}#
                   #{value\ 1836}#
                   #{e\ 1837}#
                   #{r\ 1828}#
                   #{w\ 1838}#
                   #{s\ 1839}#
                   #{mod\ 1840}#)))))
         (#{chi-expr\ 474}#
           (lambda (#{type\ 1847}#
                    #{value\ 1848}#
                    #{e\ 1849}#
                    #{r\ 1850}#
                    #{w\ 1851}#
                    #{s\ 1852}#
                    #{mod\ 1853}#)
             (if (eqv? #{type\ 1847}# (quote lexical))
               (#{build-lexical-reference\ 315}#
                 'value
                 #{s\ 1852}#
                 #{e\ 1849}#
                 #{value\ 1848}#)
               (if (if (eqv? #{type\ 1847}# (quote core))
                     #t
                     (eqv? #{type\ 1847}# (quote core-form)))
                 (#{value\ 1848}#
                   #{e\ 1849}#
                   #{r\ 1850}#
                   #{w\ 1851}#
                   #{s\ 1852}#
                   #{mod\ 1853}#)
                 (if (eqv? #{type\ 1847}# (quote module-ref))
                   (call-with-values
                     (lambda ()
                       (#{value\ 1848}#
                         #{e\ 1849}#
                         #{r\ 1850}#
                         #{w\ 1851}#))
                     (lambda (#{e\ 1864}#
                              #{r\ 1865}#
                              #{w\ 1866}#
                              #{s\ 1867}#
                              #{mod\ 1868}#)
                       (#{chi\ 472}#
                         #{e\ 1864}#
                         #{r\ 1865}#
                         #{w\ 1866}#
                         #{mod\ 1868}#)))
                   (if (eqv? #{type\ 1847}# (quote lexical-call))
                     (#{chi-application\ 476}#
                       (begin
                         (let ((#{id\ 1876}# (car #{e\ 1849}#)))
                           (#{build-lexical-reference\ 315}#
                             'fun
                             (#{source-annotation\ 366}# #{id\ 1876}#)
                             (if (#{syntax-object?\ 351}# #{id\ 1876}#)
                               (syntax->datum #{id\ 1876}#)
                               #{id\ 1876}#)
                             #{value\ 1848}#)))
                       #{e\ 1849}#
                       #{r\ 1850}#
                       #{w\ 1851}#
                       #{s\ 1852}#
                       #{mod\ 1853}#)
                     (if (eqv? #{type\ 1847}# (quote global-call))
                       (#{chi-application\ 476}#
                         (#{build-global-reference\ 321}#
                           (#{source-annotation\ 366}# (car #{e\ 1849}#))
                           (if (#{syntax-object?\ 351}# #{value\ 1848}#)
                             (#{syntax-object-expression\ 353}#
                               #{value\ 1848}#)
                             #{value\ 1848}#)
                           (if (#{syntax-object?\ 351}# #{value\ 1848}#)
                             (#{syntax-object-module\ 357}# #{value\ 1848}#)
                             #{mod\ 1853}#))
                         #{e\ 1849}#
                         #{r\ 1850}#
                         #{w\ 1851}#
                         #{s\ 1852}#
                         #{mod\ 1853}#)
                       (if (eqv? #{type\ 1847}# (quote constant))
                         (#{build-data\ 337}#
                           #{s\ 1852}#
                           (#{strip\ 498}#
                             (#{source-wrap\ 458}#
                               #{e\ 1849}#
                               #{w\ 1851}#
                               #{s\ 1852}#
                               #{mod\ 1853}#)
                             '(())))
                         (if (eqv? #{type\ 1847}# (quote global))
                           (#{build-global-reference\ 321}#
                             #{s\ 1852}#
                             #{value\ 1848}#
                             #{mod\ 1853}#)
                           (if (eqv? #{type\ 1847}# (quote call))
                             (#{chi-application\ 476}#
                               (#{chi\ 472}#
                                 (car #{e\ 1849}#)
                                 #{r\ 1850}#
                                 #{w\ 1851}#
                                 #{mod\ 1853}#)
                               #{e\ 1849}#
                               #{r\ 1850}#
                               #{w\ 1851}#
                               #{s\ 1852}#
                               #{mod\ 1853}#)
                             (if (eqv? #{type\ 1847}# (quote begin-form))
                               (let ((#{tmp\ 1883}# #{e\ 1849}#))
                                 (let ((#{tmp\ 1884}#
                                         ($sc-dispatch
                                           #{tmp\ 1883}#
                                           '(any any . each-any))))
                                   (if #{tmp\ 1884}#
                                     (@apply
                                       (lambda (#{_\ 1888}#
                                                #{e1\ 1889}#
                                                #{e2\ 1890}#)
                                         (#{chi-sequence\ 460}#
                                           (cons #{e1\ 1889}# #{e2\ 1890}#)
                                           #{r\ 1850}#
                                           #{w\ 1851}#
                                           #{s\ 1852}#
                                           #{mod\ 1853}#))
                                       #{tmp\ 1884}#)
                                     (syntax-violation
                                       #f
                                       "source expression failed to match any pattern"
                                       #{tmp\ 1883}#))))
                               (if (eqv? #{type\ 1847}#
                                         'local-syntax-form)
                                 (#{chi-local-syntax\ 482}#
                                   #{value\ 1848}#
                                   #{e\ 1849}#
                                   #{r\ 1850}#
                                   #{w\ 1851}#
                                   #{s\ 1852}#
                                   #{mod\ 1853}#
                                   #{chi-sequence\ 460}#)
                                 (if (eqv? #{type\ 1847}#
                                           'eval-when-form)
                                   (let ((#{tmp\ 1894}# #{e\ 1849}#))
                                     (let ((#{tmp\ 1895}#
                                             ($sc-dispatch
                                               #{tmp\ 1894}#
                                               '(any each-any
                                                     any
                                                     .
                                                     each-any))))
                                       (if #{tmp\ 1895}#
                                         (@apply
                                           (lambda (#{_\ 1900}#
                                                    #{x\ 1901}#
                                                    #{e1\ 1902}#
                                                    #{e2\ 1903}#)
                                             (begin
                                               (let ((#{when-list\ 1905}#
                                                       (#{chi-when-list\ 466}#
                                                         #{e\ 1849}#
                                                         #{x\ 1901}#
                                                         #{w\ 1851}#)))
                                                 (if (memq 'eval
                                                           #{when-list\ 1905}#)
                                                   (#{chi-sequence\ 460}#
                                                     (cons #{e1\ 1902}#
                                                           #{e2\ 1903}#)
                                                     #{r\ 1850}#
                                                     #{w\ 1851}#
                                                     #{s\ 1852}#
                                                     #{mod\ 1853}#)
                                                   (#{chi-void\ 486}#)))))
                                           #{tmp\ 1895}#)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           #{tmp\ 1894}#))))
                                   (if (if (eqv? #{type\ 1847}#
                                                 'define-form)
                                         #t
                                         (eqv? #{type\ 1847}#
                                               'define-syntax-form))
                                     (syntax-violation
                                       #f
                                       "definition in expression context"
                                       #{e\ 1849}#
                                       (#{wrap\ 456}#
                                         #{value\ 1848}#
                                         #{w\ 1851}#
                                         #{mod\ 1853}#))
                                     (if (eqv? #{type\ 1847}# (quote syntax))
                                       (syntax-violation
                                         #f
                                         "reference to pattern variable outside syntax form"
                                         (#{source-wrap\ 458}#
                                           #{e\ 1849}#
                                           #{w\ 1851}#
                                           #{s\ 1852}#
                                           #{mod\ 1853}#))
                                       (if (eqv? #{type\ 1847}#
                                                 'displaced-lexical)
                                         (syntax-violation
                                           #f
                                           "reference to identifier outside its scope"
                                           (#{source-wrap\ 458}#
                                             #{e\ 1849}#
                                             #{w\ 1851}#
                                             #{s\ 1852}#
                                             #{mod\ 1853}#))
                                         (syntax-violation
                                           #f
                                           "unexpected syntax"
                                           (#{source-wrap\ 458}#
                                             #{e\ 1849}#
                                             #{w\ 1851}#
                                             #{s\ 1852}#
                                             #{mod\ 1853}#))))))))))))))))))
         (#{chi-application\ 476}#
           (lambda (#{x\ 1912}#
                    #{e\ 1913}#
                    #{r\ 1914}#
                    #{w\ 1915}#
                    #{s\ 1916}#
                    #{mod\ 1917}#)
             (let ((#{tmp\ 1924}# #{e\ 1913}#))
               (let ((#{tmp\ 1925}#
                       ($sc-dispatch
                         #{tmp\ 1924}#
                         '(any . each-any))))
                 (if #{tmp\ 1925}#
                   (@apply
                     (lambda (#{e0\ 1928}# #{e1\ 1929}#)
                       (#{build-application\ 309}#
                         #{s\ 1916}#
                         #{x\ 1912}#
                         (map (lambda (#{e\ 1930}#)
                                (#{chi\ 472}#
                                  #{e\ 1930}#
                                  #{r\ 1914}#
                                  #{w\ 1915}#
                                  #{mod\ 1917}#))
                              #{e1\ 1929}#)))
                     #{tmp\ 1925}#)
                   (syntax-violation
                     #f
                     "source expression failed to match any pattern"
                     #{tmp\ 1924}#))))))
         (#{chi-macro\ 478}#
           (lambda (#{p\ 1933}#
                    #{e\ 1934}#
                    #{r\ 1935}#
                    #{w\ 1936}#
                    #{s\ 1937}#
                    #{rib\ 1938}#
                    #{mod\ 1939}#)
             (letrec*
               ((#{rebuild-macro-output\ 1948}#
                  (lambda (#{x\ 1949}# #{m\ 1950}#)
                    (if (pair? #{x\ 1949}#)
                      (#{decorate-source\ 305}#
                        (cons (#{rebuild-macro-output\ 1948}#
                                (car #{x\ 1949}#)
                                #{m\ 1950}#)
                              (#{rebuild-macro-output\ 1948}#
                                (cdr #{x\ 1949}#)
                                #{m\ 1950}#))
                        #{s\ 1937}#)
                      (if (#{syntax-object?\ 351}# #{x\ 1949}#)
                        (begin
                          (let ((#{w\ 1958}#
                                  (#{syntax-object-wrap\ 355}# #{x\ 1949}#)))
                            (begin
                              (let ((#{ms\ 1961}#
                                      (#{wrap-marks\ 394}# #{w\ 1958}#))
                                    (#{s\ 1962}#
                                      (#{wrap-subst\ 396}# #{w\ 1958}#)))
                                (if (if (pair? #{ms\ 1961}#)
                                      (eq? (car #{ms\ 1961}#) #f)
                                      #f)
                                  (#{make-syntax-object\ 349}#
                                    (#{syntax-object-expression\ 353}#
                                      #{x\ 1949}#)
                                    (#{make-wrap\ 392}#
                                      (cdr #{ms\ 1961}#)
                                      (if #{rib\ 1938}#
                                        (cons #{rib\ 1938}# (cdr #{s\ 1962}#))
                                        (cdr #{s\ 1962}#)))
                                    (#{syntax-object-module\ 357}#
                                      #{x\ 1949}#))
                                  (#{make-syntax-object\ 349}#
                                    (#{decorate-source\ 305}#
                                      (#{syntax-object-expression\ 353}#
                                        #{x\ 1949}#)
                                      #{s\ 1962}#)
                                    (#{make-wrap\ 392}#
                                      (cons #{m\ 1950}# #{ms\ 1961}#)
                                      (if #{rib\ 1938}#
                                        (cons #{rib\ 1938}#
                                              (cons (quote shift) #{s\ 1962}#))
                                        (cons (quote shift) #{s\ 1962}#)))
                                    (#{syntax-object-module\ 357}#
                                      #{x\ 1949}#)))))))
                        (if (vector? #{x\ 1949}#)
                          (begin
                            (let ((#{n\ 1970}# (vector-length #{x\ 1949}#)))
                              (begin
                                (let ((#{v\ 1972}#
                                        (#{decorate-source\ 305}#
                                          (make-vector #{n\ 1970}#)
                                          #{x\ 1949}#)))
                                  (letrec*
                                    ((#{loop\ 1975}#
                                       (lambda (#{i\ 1976}#)
                                         (if (#{fx=\ 292}#
                                               #{i\ 1976}#
                                               #{n\ 1970}#)
                                           (begin (if #f #f) #{v\ 1972}#)
                                           (begin
                                             (vector-set!
                                               #{v\ 1972}#
                                               #{i\ 1976}#
                                               (#{rebuild-macro-output\ 1948}#
                                                 (vector-ref
                                                   #{x\ 1949}#
                                                   #{i\ 1976}#)
                                                 #{m\ 1950}#))
                                             (#{loop\ 1975}#
                                               (#{fx+\ 288}#
                                                 #{i\ 1976}#
                                                 1)))))))
                                    (begin (#{loop\ 1975}# 0)))))))
                          (if (symbol? #{x\ 1949}#)
                            (syntax-violation
                              #f
                              "encountered raw symbol in macro output"
                              (#{source-wrap\ 458}#
                                #{e\ 1934}#
                                #{w\ 1936}#
                                (#{wrap-subst\ 396}# #{w\ 1936}#)
                                #{mod\ 1939}#)
                              #{x\ 1949}#)
                            (#{decorate-source\ 305}#
                              #{x\ 1949}#
                              #{s\ 1937}#))))))))
               (begin
                 (#{rebuild-macro-output\ 1948}#
                   (#{p\ 1933}#
                     (#{source-wrap\ 458}#
                       #{e\ 1934}#
                       (#{anti-mark\ 428}# #{w\ 1936}#)
                       #{s\ 1937}#
                       #{mod\ 1939}#))
                   (gensym "m"))))))
         (#{chi-body\ 480}#
           (lambda (#{body\ 1983}#
                    #{outer-form\ 1984}#
                    #{r\ 1985}#
                    #{w\ 1986}#
                    #{mod\ 1987}#)
             (begin
               (let ((#{r\ 1995}#
                       (cons '("placeholder" placeholder)
                             #{r\ 1985}#)))
                 (begin
                   (let ((#{ribcage\ 1997}#
                           (#{make-ribcage\ 408}#
                             '()
                             '()
                             '())))
                     (begin
                       (let ((#{w\ 2000}#
                               (#{make-wrap\ 392}#
                                 (#{wrap-marks\ 394}# #{w\ 1986}#)
                                 (cons #{ribcage\ 1997}#
                                       (#{wrap-subst\ 396}# #{w\ 1986}#)))))
                         (letrec*
                           ((#{parse\ 2009}#
                              (lambda (#{body\ 2010}#
                                       #{ids\ 2011}#
                                       #{labels\ 2012}#
                                       #{var-ids\ 2013}#
                                       #{vars\ 2014}#
                                       #{vals\ 2015}#
                                       #{bindings\ 2016}#)
                                (if (null? #{body\ 2010}#)
                                  (syntax-violation
                                    #f
                                    "no expressions in body"
                                    #{outer-form\ 1984}#)
                                  (begin
                                    (let ((#{e\ 2021}#
                                            (cdr (car #{body\ 2010}#)))
                                          (#{er\ 2022}#
                                            (car (car #{body\ 2010}#))))
                                      (call-with-values
                                        (lambda ()
                                          (#{syntax-type\ 468}#
                                            #{e\ 2021}#
                                            #{er\ 2022}#
                                            '(())
                                            (#{source-annotation\ 366}#
                                              #{er\ 2022}#)
                                            #{ribcage\ 1997}#
                                            #{mod\ 1987}#
                                            #f))
                                        (lambda (#{type\ 2024}#
                                                 #{value\ 2025}#
                                                 #{e\ 2026}#
                                                 #{w\ 2027}#
                                                 #{s\ 2028}#
                                                 #{mod\ 2029}#)
                                          (if (eqv? #{type\ 2024}#
                                                    'define-form)
                                            (begin
                                              (let ((#{id\ 2039}#
                                                      (#{wrap\ 456}#
                                                        #{value\ 2025}#
                                                        #{w\ 2027}#
                                                        #{mod\ 2029}#))
                                                    (#{label\ 2040}#
                                                      (#{gen-label\ 403}#)))
                                                (begin
                                                  (let ((#{var\ 2042}#
                                                          (#{gen-var\ 500}#
                                                            #{id\ 2039}#)))
                                                    (begin
                                                      (#{extend-ribcage!\ 432}#
                                                        #{ribcage\ 1997}#
                                                        #{id\ 2039}#
                                                        #{label\ 2040}#)
                                                      (#{parse\ 2009}#
                                                        (cdr #{body\ 2010}#)
                                                        (cons #{id\ 2039}#
                                                              #{ids\ 2011}#)
                                                        (cons #{label\ 2040}#
                                                              #{labels\ 2012}#)
                                                        (cons #{id\ 2039}#
                                                              #{var-ids\ 2013}#)
                                                        (cons #{var\ 2042}#
                                                              #{vars\ 2014}#)
                                                        (cons (cons #{er\ 2022}#
                                                                    (#{wrap\ 456}#
                                                                      #{e\ 2026}#
                                                                      #{w\ 2027}#
                                                                      #{mod\ 2029}#))
                                                              #{vals\ 2015}#)
                                                        (cons (cons 'lexical
                                                                    #{var\ 2042}#)
                                                              #{bindings\ 2016}#)))))))
                                            (if (eqv? #{type\ 2024}#
                                                      'define-syntax-form)
                                              (begin
                                                (let ((#{id\ 2047}#
                                                        (#{wrap\ 456}#
                                                          #{value\ 2025}#
                                                          #{w\ 2027}#
                                                          #{mod\ 2029}#))
                                                      (#{label\ 2048}#
                                                        (#{gen-label\ 403}#)))
                                                  (begin
                                                    (#{extend-ribcage!\ 432}#
                                                      #{ribcage\ 1997}#
                                                      #{id\ 2047}#
                                                      #{label\ 2048}#)
                                                    (#{parse\ 2009}#
                                                      (cdr #{body\ 2010}#)
                                                      (cons #{id\ 2047}#
                                                            #{ids\ 2011}#)
                                                      (cons #{label\ 2048}#
                                                            #{labels\ 2012}#)
                                                      #{var-ids\ 2013}#
                                                      #{vars\ 2014}#
                                                      #{vals\ 2015}#
                                                      (cons (cons 'macro
                                                                  (cons #{er\ 2022}#
                                                                        (#{wrap\ 456}#
                                                                          #{e\ 2026}#
                                                                          #{w\ 2027}#
                                                                          #{mod\ 2029}#)))
                                                            #{bindings\ 2016}#)))))
                                              (if (eqv? #{type\ 2024}#
                                                        'begin-form)
                                                (let ((#{tmp\ 2051}#
                                                        #{e\ 2026}#))
                                                  (let ((#{tmp\ 2052}#
                                                          ($sc-dispatch
                                                            #{tmp\ 2051}#
                                                            '(any .
                                                                  each-any))))
                                                    (if #{tmp\ 2052}#
                                                      (@apply
                                                        (lambda (#{_\ 2055}#
                                                                 #{e1\ 2056}#)
                                                          (#{parse\ 2009}#
                                                            (letrec*
                                                              ((#{f\ 2059}#
                                                                 (lambda (#{forms\ 2060}#)
                                                                   (if (null? #{forms\ 2060}#)
                                                                     (cdr #{body\ 2010}#)
                                                                     (cons (cons #{er\ 2022}#
                                                                                 (#{wrap\ 456}#
                                                                                   (car #{forms\ 2060}#)
                                                                                   #{w\ 2027}#
                                                                                   #{mod\ 2029}#))
                                                                           (#{f\ 2059}#
                                                                             (cdr #{forms\ 2060}#)))))))
                                                              (begin
                                                                (#{f\ 2059}#
                                                                  #{e1\ 2056}#)))
                                                            #{ids\ 2011}#
                                                            #{labels\ 2012}#
                                                            #{var-ids\ 2013}#
                                                            #{vars\ 2014}#
                                                            #{vals\ 2015}#
                                                            #{bindings\ 2016}#))
                                                        #{tmp\ 2052}#)
                                                      (syntax-violation
                                                        #f
                                                        "source expression failed to match any pattern"
                                                        #{tmp\ 2051}#))))
                                                (if (eqv? #{type\ 2024}#
                                                          'local-syntax-form)
                                                  (#{chi-local-syntax\ 482}#
                                                    #{value\ 2025}#
                                                    #{e\ 2026}#
                                                    #{er\ 2022}#
                                                    #{w\ 2027}#
                                                    #{s\ 2028}#
                                                    #{mod\ 2029}#
                                                    (lambda (#{forms\ 2063}#
                                                             #{er\ 2064}#
                                                             #{w\ 2065}#
                                                             #{s\ 2066}#
                                                             #{mod\ 2067}#)
                                                      (#{parse\ 2009}#
                                                        (letrec*
                                                          ((#{f\ 2075}#
                                                             (lambda (#{forms\ 2076}#)
                                                               (if (null? #{forms\ 2076}#)
                                                                 (cdr #{body\ 2010}#)
                                                                 (cons (cons #{er\ 2064}#
                                                                             (#{wrap\ 456}#
                                                                               (car #{forms\ 2076}#)
                                                                               #{w\ 2065}#
                                                                               #{mod\ 2067}#))
                                                                       (#{f\ 2075}#
                                                                         (cdr #{forms\ 2076}#)))))))
                                                          (begin
                                                            (#{f\ 2075}#
                                                              #{forms\ 2063}#)))
                                                        #{ids\ 2011}#
                                                        #{labels\ 2012}#
                                                        #{var-ids\ 2013}#
                                                        #{vars\ 2014}#
                                                        #{vals\ 2015}#
                                                        #{bindings\ 2016}#)))
                                                  (if (null? #{ids\ 2011}#)
                                                    (#{build-sequence\ 339}#
                                                      #f
                                                      (map (lambda (#{x\ 2079}#)
                                                             (#{chi\ 472}#
                                                               (cdr #{x\ 2079}#)
                                                               (car #{x\ 2079}#)
                                                               '(())
                                                               #{mod\ 2029}#))
                                                           (cons (cons #{er\ 2022}#
                                                                       (#{source-wrap\ 458}#
                                                                         #{e\ 2026}#
                                                                         #{w\ 2027}#
                                                                         #{s\ 2028}#
                                                                         #{mod\ 2029}#))
                                                                 (cdr #{body\ 2010}#))))
                                                    (begin
                                                      (if (not (#{valid-bound-ids?\ 450}#
                                                                 #{ids\ 2011}#))
                                                        (syntax-violation
                                                          #f
                                                          "invalid or duplicate identifier in definition"
                                                          #{outer-form\ 1984}#))
                                                      (letrec*
                                                        ((#{loop\ 2086}#
                                                           (lambda (#{bs\ 2087}#
                                                                    #{er-cache\ 2088}#
                                                                    #{r-cache\ 2089}#)
                                                             (if (not (null? #{bs\ 2087}#))
                                                               (begin
                                                                 (let ((#{b\ 2092}#
                                                                         (car #{bs\ 2087}#)))
                                                                   (if (eq? (car #{b\ 2092}#)
                                                                            'macro)
                                                                     (begin
                                                                       (let ((#{er\ 2095}#
                                                                               (car (cdr #{b\ 2092}#))))
                                                                         (begin
                                                                           (let ((#{r-cache\ 2097}#
                                                                                   (if (eq? #{er\ 2095}#
                                                                                            #{er-cache\ 2088}#)
                                                                                     #{r-cache\ 2089}#
                                                                                     (#{macros-only-env\ 379}#
                                                                                       #{er\ 2095}#))))
                                                                             (begin
                                                                               (set-cdr!
                                                                                 #{b\ 2092}#
                                                                                 (#{eval-local-transformer\ 484}#
                                                                                   (#{chi\ 472}#
                                                                                     (cdr (cdr #{b\ 2092}#))
                                                                                     #{r-cache\ 2097}#
                                                                                     '(())
                                                                                     #{mod\ 2029}#)
                                                                                   #{mod\ 2029}#))
                                                                               (#{loop\ 2086}#
                                                                                 (cdr #{bs\ 2087}#)
                                                                                 #{er\ 2095}#
                                                                                 #{r-cache\ 2097}#))))))
                                                                     (#{loop\ 2086}#
                                                                       (cdr #{bs\ 2087}#)
                                                                       #{er-cache\ 2088}#
                                                                       #{r-cache\ 2089}#))))))))
                                                        (begin
                                                          (#{loop\ 2086}#
                                                            #{bindings\ 2016}#
                                                            #f
                                                            #f)))
                                                      (set-cdr!
                                                        #{r\ 1995}#
                                                        (#{extend-env\ 375}#
                                                          #{labels\ 2012}#
                                                          #{bindings\ 2016}#
                                                          (cdr #{r\ 1995}#)))
                                                      (#{build-letrec\ 345}#
                                                        #f
                                                        #t
                                                        (reverse
                                                          (map syntax->datum
                                                               #{var-ids\ 2013}#))
                                                        (reverse
                                                          #{vars\ 2014}#)
                                                        (map (lambda (#{x\ 2100}#)
                                                               (#{chi\ 472}#
                                                                 (cdr #{x\ 2100}#)
                                                                 (car #{x\ 2100}#)
                                                                 '(())
                                                                 #{mod\ 2029}#))
                                                             (reverse
                                                               #{vals\ 2015}#))
                                                        (#{build-sequence\ 339}#
                                                          #f
                                                          (map (lambda (#{x\ 2104}#)
                                                                 (#{chi\ 472}#
                                                                   (cdr #{x\ 2104}#)
                                                                   (car #{x\ 2104}#)
                                                                   '(())
                                                                   #{mod\ 2029}#))
                                                               (cons (cons #{er\ 2022}#
                                                                           (#{source-wrap\ 458}#
                                                                             #{e\ 2026}#
                                                                             #{w\ 2027}#
                                                                             #{s\ 2028}#
                                                                             #{mod\ 2029}#))
                                                                     (cdr #{body\ 2010}#)))))))))))))))))))
                           (begin
                             (#{parse\ 2009}#
                               (map (lambda (#{x\ 2017}#)
                                      (cons #{r\ 1995}#
                                            (#{wrap\ 456}#
                                              #{x\ 2017}#
                                              #{w\ 2000}#
                                              #{mod\ 1987}#)))
                                    #{body\ 1983}#)
                               '()
                               '()
                               '()
                               '()
                               '()
                               '())))))))))))
         (#{chi-local-syntax\ 482}#
           (lambda (#{rec?\ 2107}#
                    #{e\ 2108}#
                    #{r\ 2109}#
                    #{w\ 2110}#
                    #{s\ 2111}#
                    #{mod\ 2112}#
                    #{k\ 2113}#)
             (let ((#{tmp\ 2121}# #{e\ 2108}#))
               (let ((#{tmp\ 2122}#
                       ($sc-dispatch
                         #{tmp\ 2121}#
                         '(any #(each (any any)) any . each-any))))
                 (if #{tmp\ 2122}#
                   (@apply
                     (lambda (#{_\ 2128}#
                              #{id\ 2129}#
                              #{val\ 2130}#
                              #{e1\ 2131}#
                              #{e2\ 2132}#)
                       (begin
                         (let ((#{ids\ 2134}# #{id\ 2129}#))
                           (if (not (#{valid-bound-ids?\ 450}# #{ids\ 2134}#))
                             (syntax-violation
                               #f
                               "duplicate bound keyword"
                               #{e\ 2108}#)
                             (begin
                               (let ((#{labels\ 2137}#
                                       (#{gen-labels\ 405}# #{ids\ 2134}#)))
                                 (begin
                                   (let ((#{new-w\ 2139}#
                                           (#{make-binding-wrap\ 434}#
                                             #{ids\ 2134}#
                                             #{labels\ 2137}#
                                             #{w\ 2110}#)))
                                     (#{k\ 2113}#
                                       (cons #{e1\ 2131}# #{e2\ 2132}#)
                                       (#{extend-env\ 375}#
                                         #{labels\ 2137}#
                                         (begin
                                           (let ((#{w\ 2143}#
                                                   (if #{rec?\ 2107}#
                                                     #{new-w\ 2139}#
                                                     #{w\ 2110}#))
                                                 (#{trans-r\ 2144}#
                                                   (#{macros-only-env\ 379}#
                                                     #{r\ 2109}#)))
                                             (map (lambda (#{x\ 2145}#)
                                                    (cons 'macro
                                                          (#{eval-local-transformer\ 484}#
                                                            (#{chi\ 472}#
                                                              #{x\ 2145}#
                                                              #{trans-r\ 2144}#
                                                              #{w\ 2143}#
                                                              #{mod\ 2112}#)
                                                            #{mod\ 2112}#)))
                                                  #{val\ 2130}#)))
                                         #{r\ 2109}#)
                                       #{new-w\ 2139}#
                                       #{s\ 2111}#
                                       #{mod\ 2112}#)))))))))
                     #{tmp\ 2122}#)
                   (let ((#{_\ 2150}# #{tmp\ 2121}#))
                     (syntax-violation
                       #f
                       "bad local syntax definition"
                       (#{source-wrap\ 458}#
                         #{e\ 2108}#
                         #{w\ 2110}#
                         #{s\ 2111}#
                         #{mod\ 2112}#))))))))
         (#{eval-local-transformer\ 484}#
           (lambda (#{expanded\ 2151}# #{mod\ 2152}#)
             (begin
               (let ((#{p\ 2156}#
                       (#{local-eval-hook\ 298}#
                         #{expanded\ 2151}#
                         #{mod\ 2152}#)))
                 (if (procedure? #{p\ 2156}#)
                   #{p\ 2156}#
                   (syntax-violation
                     #f
                     "nonprocedure transformer"
                     #{p\ 2156}#))))))
         (#{chi-void\ 486}#
           (lambda () (#{build-void\ 307}# #f)))
         (#{ellipsis?\ 488}#
           (lambda (#{x\ 2158}#)
             (if (#{nonsymbol-id?\ 385}# #{x\ 2158}#)
               (#{free-id=?\ 446}#
                 #{x\ 2158}#
                 '#(syntax-object
                    ...
                    ((top)
                     #(ribcage () () ())
                     #(ribcage () () ())
                     #(ribcage #(x) #((top)) #("i2159"))
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
                         build-dynlet
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
                       ("i501"
                        "i499"
                        "i497"
                        "i495"
                        "i493"
                        "i491"
                        "i489"
                        "i487"
                        "i485"
                        "i483"
                        "i481"
                        "i479"
                        "i477"
                        "i475"
                        "i473"
                        "i471"
                        "i469"
                        "i467"
                        "i465"
                        "i463"
                        "i461"
                        "i459"
                        "i457"
                        "i455"
                        "i453"
                        "i451"
                        "i449"
                        "i447"
                        "i445"
                        "i443"
                        "i441"
                        "i439"
                        "i437"
                        "i435"
                        "i433"
                        "i431"
                        "i430"
                        "i429"
                        "i427"
                        "i426"
                        "i425"
                        "i424"
                        "i423"
                        "i421"
                        "i419"
                        "i417"
                        "i415"
                        "i413"
                        "i411"
                        "i409"
                        "i407"
                        "i404"
                        "i402"
                        "i401"
                        "i400"
                        "i399"
                        "i398"
                        "i397"
                        "i395"
                        "i393"
                        "i391"
                        "i389"
                        "i388"
                        "i386"
                        "i384"
                        "i382"
                        "i380"
                        "i378"
                        "i376"
                        "i374"
                        "i373"
                        "i371"
                        "i369"
                        "i368"
                        "i367"
                        "i365"
                        "i364"
                        "i362"
                        "i360"
                        "i358"
                        "i356"
                        "i354"
                        "i352"
                        "i350"
                        "i348"
                        "i346"
                        "i344"
                        "i342"
                        "i340"
                        "i338"
                        "i336"
                        "i334"
                        "i332"
                        "i330"
                        "i328"
                        "i326"
                        "i324"
                        "i322"
                        "i320"
                        "i318"
                        "i316"
                        "i314"
                        "i312"
                        "i310"
                        "i308"
                        "i306"
                        "i304"
                        "i302"
                        "i300"
                        "i299"
                        "i297"
                        "i295"
                        "i293"
                        "i291"
                        "i289"
                        "i287"
                        "i285"
                        "i283"
                        "i281"
                        "i278"
                        "i276"
                        "i274"
                        "i272"
                        "i270"
                        "i268"
                        "i266"
                        "i264"
                        "i262"
                        "i260"
                        "i258"
                        "i256"
                        "i254"
                        "i252"
                        "i250"
                        "i248"
                        "i246"
                        "i244"))
                     #(ribcage
                       (define-structure
                         define-expansion-accessors
                         define-expansion-constructors
                         and-map*)
                       ((top) (top) (top) (top))
                       ("i38" "i37" "i36" "i34")))
                    (hygiene guile)))
               #f)))
         (#{lambda-formals\ 490}#
           (lambda (#{orig-args\ 2162}#)
             (letrec*
               ((#{req\ 2165}#
                  (lambda (#{args\ 2168}# #{rreq\ 2169}#)
                    (let ((#{tmp\ 2172}# #{args\ 2168}#))
                      (let ((#{tmp\ 2173}#
                              ($sc-dispatch #{tmp\ 2172}# (quote ()))))
                        (if #{tmp\ 2173}#
                          (@apply
                            (lambda ()
                              (#{check\ 2167}# (reverse #{rreq\ 2169}#) #f))
                            #{tmp\ 2173}#)
                          (let ((#{tmp\ 2174}#
                                  ($sc-dispatch
                                    #{tmp\ 2172}#
                                    '(any . any))))
                            (if (if #{tmp\ 2174}#
                                  (@apply
                                    (lambda (#{a\ 2177}# #{b\ 2178}#)
                                      (#{id?\ 387}# #{a\ 2177}#))
                                    #{tmp\ 2174}#)
                                  #f)
                              (@apply
                                (lambda (#{a\ 2181}# #{b\ 2182}#)
                                  (#{req\ 2165}#
                                    #{b\ 2182}#
                                    (cons #{a\ 2181}# #{rreq\ 2169}#)))
                                #{tmp\ 2174}#)
                              (let ((#{tmp\ 2183}# (list #{tmp\ 2172}#)))
                                (if (if #{tmp\ 2183}#
                                      (@apply
                                        (lambda (#{r\ 2185}#)
                                          (#{id?\ 387}# #{r\ 2185}#))
                                        #{tmp\ 2183}#)
                                      #f)
                                  (@apply
                                    (lambda (#{r\ 2187}#)
                                      (#{check\ 2167}#
                                        (reverse #{rreq\ 2169}#)
                                        #{r\ 2187}#))
                                    #{tmp\ 2183}#)
                                  (let ((#{else\ 2189}# #{tmp\ 2172}#))
                                    (syntax-violation
                                      'lambda
                                      "invalid argument list"
                                      #{orig-args\ 2162}#
                                      #{args\ 2168}#)))))))))))
                (#{check\ 2167}#
                  (lambda (#{req\ 2190}# #{rest\ 2191}#)
                    (if (#{distinct-bound-ids?\ 452}#
                          (if #{rest\ 2191}#
                            (cons #{rest\ 2191}# #{req\ 2190}#)
                            #{req\ 2190}#))
                      (values #{req\ 2190}# #f #{rest\ 2191}# #f)
                      (syntax-violation
                        'lambda
                        "duplicate identifier in argument list"
                        #{orig-args\ 2162}#)))))
               (begin
                 (#{req\ 2165}# #{orig-args\ 2162}# (quote ()))))))
         (#{chi-simple-lambda\ 492}#
           (lambda (#{e\ 2197}#
                    #{r\ 2198}#
                    #{w\ 2199}#
                    #{s\ 2200}#
                    #{mod\ 2201}#
                    #{req\ 2202}#
                    #{rest\ 2203}#
                    #{meta\ 2204}#
                    #{body\ 2205}#)
             (begin
               (let ((#{ids\ 2217}#
                       (if #{rest\ 2203}#
                         (append #{req\ 2202}# (list #{rest\ 2203}#))
                         #{req\ 2202}#)))
                 (begin
                   (let ((#{vars\ 2219}#
                           (map #{gen-var\ 500}# #{ids\ 2217}#)))
                     (begin
                       (let ((#{labels\ 2221}#
                               (#{gen-labels\ 405}# #{ids\ 2217}#)))
                         (#{build-simple-lambda\ 329}#
                           #{s\ 2200}#
                           (map syntax->datum #{req\ 2202}#)
                           (if #{rest\ 2203}#
                             (syntax->datum #{rest\ 2203}#)
                             #f)
                           #{vars\ 2219}#
                           #{meta\ 2204}#
                           (#{chi-body\ 480}#
                             #{body\ 2205}#
                             (#{source-wrap\ 458}#
                               #{e\ 2197}#
                               #{w\ 2199}#
                               #{s\ 2200}#
                               #{mod\ 2201}#)
                             (#{extend-var-env\ 377}#
                               #{labels\ 2221}#
                               #{vars\ 2219}#
                               #{r\ 2198}#)
                             (#{make-binding-wrap\ 434}#
                               #{ids\ 2217}#
                               #{labels\ 2221}#
                               #{w\ 2199}#)
                             #{mod\ 2201}#))))))))))
         (#{lambda*-formals\ 494}#
           (lambda (#{orig-args\ 2224}#)
             (letrec*
               ((#{req\ 2227}#
                  (lambda (#{args\ 2236}# #{rreq\ 2237}#)
                    (let ((#{tmp\ 2240}# #{args\ 2236}#))
                      (let ((#{tmp\ 2241}#
                              ($sc-dispatch #{tmp\ 2240}# (quote ()))))
                        (if #{tmp\ 2241}#
                          (@apply
                            (lambda ()
                              (#{check\ 2235}#
                                (reverse #{rreq\ 2237}#)
                                '()
                                #f
                                '()))
                            #{tmp\ 2241}#)
                          (let ((#{tmp\ 2242}#
                                  ($sc-dispatch
                                    #{tmp\ 2240}#
                                    '(any . any))))
                            (if (if #{tmp\ 2242}#
                                  (@apply
                                    (lambda (#{a\ 2245}# #{b\ 2246}#)
                                      (#{id?\ 387}# #{a\ 2245}#))
                                    #{tmp\ 2242}#)
                                  #f)
                              (@apply
                                (lambda (#{a\ 2249}# #{b\ 2250}#)
                                  (#{req\ 2227}#
                                    #{b\ 2250}#
                                    (cons #{a\ 2249}# #{rreq\ 2237}#)))
                                #{tmp\ 2242}#)
                              (let ((#{tmp\ 2251}#
                                      ($sc-dispatch
                                        #{tmp\ 2240}#
                                        '(any . any))))
                                (if (if #{tmp\ 2251}#
                                      (@apply
                                        (lambda (#{a\ 2254}# #{b\ 2255}#)
                                          (eq? (syntax->datum #{a\ 2254}#)
                                               #:optional))
                                        #{tmp\ 2251}#)
                                      #f)
                                  (@apply
                                    (lambda (#{a\ 2258}# #{b\ 2259}#)
                                      (#{opt\ 2229}#
                                        #{b\ 2259}#
                                        (reverse #{rreq\ 2237}#)
                                        '()))
                                    #{tmp\ 2251}#)
                                  (let ((#{tmp\ 2260}#
                                          ($sc-dispatch
                                            #{tmp\ 2240}#
                                            '(any . any))))
                                    (if (if #{tmp\ 2260}#
                                          (@apply
                                            (lambda (#{a\ 2263}# #{b\ 2264}#)
                                              (eq? (syntax->datum #{a\ 2263}#)
                                                   #:key))
                                            #{tmp\ 2260}#)
                                          #f)
                                      (@apply
                                        (lambda (#{a\ 2267}# #{b\ 2268}#)
                                          (#{key\ 2231}#
                                            #{b\ 2268}#
                                            (reverse #{rreq\ 2237}#)
                                            '()
                                            '()))
                                        #{tmp\ 2260}#)
                                      (let ((#{tmp\ 2269}#
                                              ($sc-dispatch
                                                #{tmp\ 2240}#
                                                '(any any))))
                                        (if (if #{tmp\ 2269}#
                                              (@apply
                                                (lambda (#{a\ 2272}#
                                                         #{b\ 2273}#)
                                                  (eq? (syntax->datum
                                                         #{a\ 2272}#)
                                                       #:rest))
                                                #{tmp\ 2269}#)
                                              #f)
                                          (@apply
                                            (lambda (#{a\ 2276}# #{b\ 2277}#)
                                              (#{rest\ 2233}#
                                                #{b\ 2277}#
                                                (reverse #{rreq\ 2237}#)
                                                '()
                                                '()))
                                            #{tmp\ 2269}#)
                                          (let ((#{tmp\ 2278}#
                                                  (list #{tmp\ 2240}#)))
                                            (if (if #{tmp\ 2278}#
                                                  (@apply
                                                    (lambda (#{r\ 2280}#)
                                                      (#{id?\ 387}#
                                                        #{r\ 2280}#))
                                                    #{tmp\ 2278}#)
                                                  #f)
                                              (@apply
                                                (lambda (#{r\ 2282}#)
                                                  (#{rest\ 2233}#
                                                    #{r\ 2282}#
                                                    (reverse #{rreq\ 2237}#)
                                                    '()
                                                    '()))
                                                #{tmp\ 2278}#)
                                              (let ((#{else\ 2284}#
                                                      #{tmp\ 2240}#))
                                                (syntax-violation
                                                  'lambda*
                                                  "invalid argument list"
                                                  #{orig-args\ 2224}#
                                                  #{args\ 2236}#)))))))))))))))))
                (#{opt\ 2229}#
                  (lambda (#{args\ 2285}# #{req\ 2286}# #{ropt\ 2287}#)
                    (let ((#{tmp\ 2291}# #{args\ 2285}#))
                      (let ((#{tmp\ 2292}#
                              ($sc-dispatch #{tmp\ 2291}# (quote ()))))
                        (if #{tmp\ 2292}#
                          (@apply
                            (lambda ()
                              (#{check\ 2235}#
                                #{req\ 2286}#
                                (reverse #{ropt\ 2287}#)
                                #f
                                '()))
                            #{tmp\ 2292}#)
                          (let ((#{tmp\ 2293}#
                                  ($sc-dispatch
                                    #{tmp\ 2291}#
                                    '(any . any))))
                            (if (if #{tmp\ 2293}#
                                  (@apply
                                    (lambda (#{a\ 2296}# #{b\ 2297}#)
                                      (#{id?\ 387}# #{a\ 2296}#))
                                    #{tmp\ 2293}#)
                                  #f)
                              (@apply
                                (lambda (#{a\ 2300}# #{b\ 2301}#)
                                  (#{opt\ 2229}#
                                    #{b\ 2301}#
                                    #{req\ 2286}#
                                    (cons (cons #{a\ 2300}#
                                                '(#(syntax-object
                                                    #f
                                                    ((top)
                                                     #(ribcage
                                                       #(a b)
                                                       #((top) (top))
                                                       #("i2298" "i2299"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(args req ropt)
                                                       #((top) (top) (top))
                                                       #("i2288"
                                                         "i2289"
                                                         "i2290"))
                                                     #(ribcage
                                                       (check rest key opt req)
                                                       ((top)
                                                        (top)
                                                        (top)
                                                        (top)
                                                        (top))
                                                       ("i2234"
                                                        "i2232"
                                                        "i2230"
                                                        "i2228"
                                                        "i2226"))
                                                     #(ribcage
                                                       #(orig-args)
                                                       #((top))
                                                       #("i2225"))
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
                                                         build-dynlet
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
                                                       ("i501"
                                                        "i499"
                                                        "i497"
                                                        "i495"
                                                        "i493"
                                                        "i491"
                                                        "i489"
                                                        "i487"
                                                        "i485"
                                                        "i483"
                                                        "i481"
                                                        "i479"
                                                        "i477"
                                                        "i475"
                                                        "i473"
                                                        "i471"
                                                        "i469"
                                                        "i467"
                                                        "i465"
                                                        "i463"
                                                        "i461"
                                                        "i459"
                                                        "i457"
                                                        "i455"
                                                        "i453"
                                                        "i451"
                                                        "i449"
                                                        "i447"
                                                        "i445"
                                                        "i443"
                                                        "i441"
                                                        "i439"
                                                        "i437"
                                                        "i435"
                                                        "i433"
                                                        "i431"
                                                        "i430"
                                                        "i429"
                                                        "i427"
                                                        "i426"
                                                        "i425"
                                                        "i424"
                                                        "i423"
                                                        "i421"
                                                        "i419"
                                                        "i417"
                                                        "i415"
                                                        "i413"
                                                        "i411"
                                                        "i409"
                                                        "i407"
                                                        "i404"
                                                        "i402"
                                                        "i401"
                                                        "i400"
                                                        "i399"
                                                        "i398"
                                                        "i397"
                                                        "i395"
                                                        "i393"
                                                        "i391"
                                                        "i389"
                                                        "i388"
                                                        "i386"
                                                        "i384"
                                                        "i382"
                                                        "i380"
                                                        "i378"
                                                        "i376"
                                                        "i374"
                                                        "i373"
                                                        "i371"
                                                        "i369"
                                                        "i368"
                                                        "i367"
                                                        "i365"
                                                        "i364"
                                                        "i362"
                                                        "i360"
                                                        "i358"
                                                        "i356"
                                                        "i354"
                                                        "i352"
                                                        "i350"
                                                        "i348"
                                                        "i346"
                                                        "i344"
                                                        "i342"
                                                        "i340"
                                                        "i338"
                                                        "i336"
                                                        "i334"
                                                        "i332"
                                                        "i330"
                                                        "i328"
                                                        "i326"
                                                        "i324"
                                                        "i322"
                                                        "i320"
                                                        "i318"
                                                        "i316"
                                                        "i314"
                                                        "i312"
                                                        "i310"
                                                        "i308"
                                                        "i306"
                                                        "i304"
                                                        "i302"
                                                        "i300"
                                                        "i299"
                                                        "i297"
                                                        "i295"
                                                        "i293"
                                                        "i291"
                                                        "i289"
                                                        "i287"
                                                        "i285"
                                                        "i283"
                                                        "i281"
                                                        "i278"
                                                        "i276"
                                                        "i274"
                                                        "i272"
                                                        "i270"
                                                        "i268"
                                                        "i266"
                                                        "i264"
                                                        "i262"
                                                        "i260"
                                                        "i258"
                                                        "i256"
                                                        "i254"
                                                        "i252"
                                                        "i250"
                                                        "i248"
                                                        "i246"
                                                        "i244"))
                                                     #(ribcage
                                                       (define-structure
                                                         define-expansion-accessors
                                                         define-expansion-constructors
                                                         and-map*)
                                                       ((top)
                                                        (top)
                                                        (top)
                                                        (top))
                                                       ("i38"
                                                        "i37"
                                                        "i36"
                                                        "i34")))
                                                    (hygiene guile))))
                                          #{ropt\ 2287}#)))
                                #{tmp\ 2293}#)
                              (let ((#{tmp\ 2302}#
                                      ($sc-dispatch
                                        #{tmp\ 2291}#
                                        '((any any) . any))))
                                (if (if #{tmp\ 2302}#
                                      (@apply
                                        (lambda (#{a\ 2306}#
                                                 #{init\ 2307}#
                                                 #{b\ 2308}#)
                                          (#{id?\ 387}# #{a\ 2306}#))
                                        #{tmp\ 2302}#)
                                      #f)
                                  (@apply
                                    (lambda (#{a\ 2312}#
                                             #{init\ 2313}#
                                             #{b\ 2314}#)
                                      (#{opt\ 2229}#
                                        #{b\ 2314}#
                                        #{req\ 2286}#
                                        (cons (list #{a\ 2312}# #{init\ 2313}#)
                                              #{ropt\ 2287}#)))
                                    #{tmp\ 2302}#)
                                  (let ((#{tmp\ 2315}#
                                          ($sc-dispatch
                                            #{tmp\ 2291}#
                                            '(any . any))))
                                    (if (if #{tmp\ 2315}#
                                          (@apply
                                            (lambda (#{a\ 2318}# #{b\ 2319}#)
                                              (eq? (syntax->datum #{a\ 2318}#)
                                                   #:key))
                                            #{tmp\ 2315}#)
                                          #f)
                                      (@apply
                                        (lambda (#{a\ 2322}# #{b\ 2323}#)
                                          (#{key\ 2231}#
                                            #{b\ 2323}#
                                            #{req\ 2286}#
                                            (reverse #{ropt\ 2287}#)
                                            '()))
                                        #{tmp\ 2315}#)
                                      (let ((#{tmp\ 2324}#
                                              ($sc-dispatch
                                                #{tmp\ 2291}#
                                                '(any any))))
                                        (if (if #{tmp\ 2324}#
                                              (@apply
                                                (lambda (#{a\ 2327}#
                                                         #{b\ 2328}#)
                                                  (eq? (syntax->datum
                                                         #{a\ 2327}#)
                                                       #:rest))
                                                #{tmp\ 2324}#)
                                              #f)
                                          (@apply
                                            (lambda (#{a\ 2331}# #{b\ 2332}#)
                                              (#{rest\ 2233}#
                                                #{b\ 2332}#
                                                #{req\ 2286}#
                                                (reverse #{ropt\ 2287}#)
                                                '()))
                                            #{tmp\ 2324}#)
                                          (let ((#{tmp\ 2333}#
                                                  (list #{tmp\ 2291}#)))
                                            (if (if #{tmp\ 2333}#
                                                  (@apply
                                                    (lambda (#{r\ 2335}#)
                                                      (#{id?\ 387}#
                                                        #{r\ 2335}#))
                                                    #{tmp\ 2333}#)
                                                  #f)
                                              (@apply
                                                (lambda (#{r\ 2337}#)
                                                  (#{rest\ 2233}#
                                                    #{r\ 2337}#
                                                    #{req\ 2286}#
                                                    (reverse #{ropt\ 2287}#)
                                                    '()))
                                                #{tmp\ 2333}#)
                                              (let ((#{else\ 2339}#
                                                      #{tmp\ 2291}#))
                                                (syntax-violation
                                                  'lambda*
                                                  "invalid optional argument list"
                                                  #{orig-args\ 2224}#
                                                  #{args\ 2285}#)))))))))))))))))
                (#{key\ 2231}#
                  (lambda (#{args\ 2340}#
                           #{req\ 2341}#
                           #{opt\ 2342}#
                           #{rkey\ 2343}#)
                    (let ((#{tmp\ 2348}# #{args\ 2340}#))
                      (let ((#{tmp\ 2349}#
                              ($sc-dispatch #{tmp\ 2348}# (quote ()))))
                        (if #{tmp\ 2349}#
                          (@apply
                            (lambda ()
                              (#{check\ 2235}#
                                #{req\ 2341}#
                                #{opt\ 2342}#
                                #f
                                (cons #f (reverse #{rkey\ 2343}#))))
                            #{tmp\ 2349}#)
                          (let ((#{tmp\ 2350}#
                                  ($sc-dispatch
                                    #{tmp\ 2348}#
                                    '(any . any))))
                            (if (if #{tmp\ 2350}#
                                  (@apply
                                    (lambda (#{a\ 2353}# #{b\ 2354}#)
                                      (#{id?\ 387}# #{a\ 2353}#))
                                    #{tmp\ 2350}#)
                                  #f)
                              (@apply
                                (lambda (#{a\ 2357}# #{b\ 2358}#)
                                  (let ((#{tmp\ 2360}#
                                          (symbol->keyword
                                            (syntax->datum #{a\ 2357}#))))
                                    (let ((#{k\ 2362}# #{tmp\ 2360}#))
                                      (#{key\ 2231}#
                                        #{b\ 2358}#
                                        #{req\ 2341}#
                                        #{opt\ 2342}#
                                        (cons (cons #{k\ 2362}#
                                                    (cons #{a\ 2357}#
                                                          '(#(syntax-object
                                                              #f
                                                              ((top)
                                                               #(ribcage
                                                                 #(k)
                                                                 #((top))
                                                                 #("i2361"))
                                                               #(ribcage
                                                                 #(a b)
                                                                 #((top) (top))
                                                                 #("i2355"
                                                                   "i2356"))
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
                                                                 #("i2344"
                                                                   "i2345"
                                                                   "i2346"
                                                                   "i2347"))
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
                                                                 ("i2234"
                                                                  "i2232"
                                                                  "i2230"
                                                                  "i2228"
                                                                  "i2226"))
                                                               #(ribcage
                                                                 #(orig-args)
                                                                 #((top))
                                                                 #("i2225"))
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
                                                                   build-dynlet
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
                                                                 ("i501"
                                                                  "i499"
                                                                  "i497"
                                                                  "i495"
                                                                  "i493"
                                                                  "i491"
                                                                  "i489"
                                                                  "i487"
                                                                  "i485"
                                                                  "i483"
                                                                  "i481"
                                                                  "i479"
                                                                  "i477"
                                                                  "i475"
                                                                  "i473"
                                                                  "i471"
                                                                  "i469"
                                                                  "i467"
                                                                  "i465"
                                                                  "i463"
                                                                  "i461"
                                                                  "i459"
                                                                  "i457"
                                                                  "i455"
                                                                  "i453"
                                                                  "i451"
                                                                  "i449"
                                                                  "i447"
                                                                  "i445"
                                                                  "i443"
                                                                  "i441"
                                                                  "i439"
                                                                  "i437"
                                                                  "i435"
                                                                  "i433"
                                                                  "i431"
                                                                  "i430"
                                                                  "i429"
                                                                  "i427"
                                                                  "i426"
                                                                  "i425"
                                                                  "i424"
                                                                  "i423"
                                                                  "i421"
                                                                  "i419"
                                                                  "i417"
                                                                  "i415"
                                                                  "i413"
                                                                  "i411"
                                                                  "i409"
                                                                  "i407"
                                                                  "i404"
                                                                  "i402"
                                                                  "i401"
                                                                  "i400"
                                                                  "i399"
                                                                  "i398"
                                                                  "i397"
                                                                  "i395"
                                                                  "i393"
                                                                  "i391"
                                                                  "i389"
                                                                  "i388"
                                                                  "i386"
                                                                  "i384"
                                                                  "i382"
                                                                  "i380"
                                                                  "i378"
                                                                  "i376"
                                                                  "i374"
                                                                  "i373"
                                                                  "i371"
                                                                  "i369"
                                                                  "i368"
                                                                  "i367"
                                                                  "i365"
                                                                  "i364"
                                                                  "i362"
                                                                  "i360"
                                                                  "i358"
                                                                  "i356"
                                                                  "i354"
                                                                  "i352"
                                                                  "i350"
                                                                  "i348"
                                                                  "i346"
                                                                  "i344"
                                                                  "i342"
                                                                  "i340"
                                                                  "i338"
                                                                  "i336"
                                                                  "i334"
                                                                  "i332"
                                                                  "i330"
                                                                  "i328"
                                                                  "i326"
                                                                  "i324"
                                                                  "i322"
                                                                  "i320"
                                                                  "i318"
                                                                  "i316"
                                                                  "i314"
                                                                  "i312"
                                                                  "i310"
                                                                  "i308"
                                                                  "i306"
                                                                  "i304"
                                                                  "i302"
                                                                  "i300"
                                                                  "i299"
                                                                  "i297"
                                                                  "i295"
                                                                  "i293"
                                                                  "i291"
                                                                  "i289"
                                                                  "i287"
                                                                  "i285"
                                                                  "i283"
                                                                  "i281"
                                                                  "i278"
                                                                  "i276"
                                                                  "i274"
                                                                  "i272"
                                                                  "i270"
                                                                  "i268"
                                                                  "i266"
                                                                  "i264"
                                                                  "i262"
                                                                  "i260"
                                                                  "i258"
                                                                  "i256"
                                                                  "i254"
                                                                  "i252"
                                                                  "i250"
                                                                  "i248"
                                                                  "i246"
                                                                  "i244"))
                                                               #(ribcage
                                                                 (define-structure
                                                                   define-expansion-accessors
                                                                   define-expansion-constructors
                                                                   and-map*)
                                                                 ((top)
                                                                  (top)
                                                                  (top)
                                                                  (top))
                                                                 ("i38"
                                                                  "i37"
                                                                  "i36"
                                                                  "i34")))
                                                              (hygiene
                                                                guile)))))
                                              #{rkey\ 2343}#)))))
                                #{tmp\ 2350}#)
                              (let ((#{tmp\ 2363}#
                                      ($sc-dispatch
                                        #{tmp\ 2348}#
                                        '((any any) . any))))
                                (if (if #{tmp\ 2363}#
                                      (@apply
                                        (lambda (#{a\ 2367}#
                                                 #{init\ 2368}#
                                                 #{b\ 2369}#)
                                          (#{id?\ 387}# #{a\ 2367}#))
                                        #{tmp\ 2363}#)
                                      #f)
                                  (@apply
                                    (lambda (#{a\ 2373}#
                                             #{init\ 2374}#
                                             #{b\ 2375}#)
                                      (let ((#{tmp\ 2377}#
                                              (symbol->keyword
                                                (syntax->datum #{a\ 2373}#))))
                                        (let ((#{k\ 2379}# #{tmp\ 2377}#))
                                          (#{key\ 2231}#
                                            #{b\ 2375}#
                                            #{req\ 2341}#
                                            #{opt\ 2342}#
                                            (cons (list #{k\ 2379}#
                                                        #{a\ 2373}#
                                                        #{init\ 2374}#)
                                                  #{rkey\ 2343}#)))))
                                    #{tmp\ 2363}#)
                                  (let ((#{tmp\ 2380}#
                                          ($sc-dispatch
                                            #{tmp\ 2348}#
                                            '((any any any) . any))))
                                    (if (if #{tmp\ 2380}#
                                          (@apply
                                            (lambda (#{a\ 2385}#
                                                     #{init\ 2386}#
                                                     #{k\ 2387}#
                                                     #{b\ 2388}#)
                                              (if (#{id?\ 387}# #{a\ 2385}#)
                                                (keyword?
                                                  (syntax->datum #{k\ 2387}#))
                                                #f))
                                            #{tmp\ 2380}#)
                                          #f)
                                      (@apply
                                        (lambda (#{a\ 2395}#
                                                 #{init\ 2396}#
                                                 #{k\ 2397}#
                                                 #{b\ 2398}#)
                                          (#{key\ 2231}#
                                            #{b\ 2398}#
                                            #{req\ 2341}#
                                            #{opt\ 2342}#
                                            (cons (list #{k\ 2397}#
                                                        #{a\ 2395}#
                                                        #{init\ 2396}#)
                                                  #{rkey\ 2343}#)))
                                        #{tmp\ 2380}#)
                                      (let ((#{tmp\ 2399}#
                                              ($sc-dispatch
                                                #{tmp\ 2348}#
                                                '(any))))
                                        (if (if #{tmp\ 2399}#
                                              (@apply
                                                (lambda (#{aok\ 2401}#)
                                                  (eq? (syntax->datum
                                                         #{aok\ 2401}#)
                                                       #:allow-other-keys))
                                                #{tmp\ 2399}#)
                                              #f)
                                          (@apply
                                            (lambda (#{aok\ 2403}#)
                                              (#{check\ 2235}#
                                                #{req\ 2341}#
                                                #{opt\ 2342}#
                                                #f
                                                (cons #t
                                                      (reverse
                                                        #{rkey\ 2343}#))))
                                            #{tmp\ 2399}#)
                                          (let ((#{tmp\ 2404}#
                                                  ($sc-dispatch
                                                    #{tmp\ 2348}#
                                                    '(any any any))))
                                            (if (if #{tmp\ 2404}#
                                                  (@apply
                                                    (lambda (#{aok\ 2408}#
                                                             #{a\ 2409}#
                                                             #{b\ 2410}#)
                                                      (if (eq? (syntax->datum
                                                                 #{aok\ 2408}#)
                                                               #:allow-other-keys)
                                                        (eq? (syntax->datum
                                                               #{a\ 2409}#)
                                                             #:rest)
                                                        #f))
                                                    #{tmp\ 2404}#)
                                                  #f)
                                              (@apply
                                                (lambda (#{aok\ 2416}#
                                                         #{a\ 2417}#
                                                         #{b\ 2418}#)
                                                  (#{rest\ 2233}#
                                                    #{b\ 2418}#
                                                    #{req\ 2341}#
                                                    #{opt\ 2342}#
                                                    (cons #t
                                                          (reverse
                                                            #{rkey\ 2343}#))))
                                                #{tmp\ 2404}#)
                                              (let ((#{tmp\ 2419}#
                                                      ($sc-dispatch
                                                        #{tmp\ 2348}#
                                                        '(any . any))))
                                                (if (if #{tmp\ 2419}#
                                                      (@apply
                                                        (lambda (#{aok\ 2422}#
                                                                 #{r\ 2423}#)
                                                          (if (eq? (syntax->datum
                                                                     #{aok\ 2422}#)
                                                                   #:allow-other-keys)
                                                            (#{id?\ 387}#
                                                              #{r\ 2423}#)
                                                            #f))
                                                        #{tmp\ 2419}#)
                                                      #f)
                                                  (@apply
                                                    (lambda (#{aok\ 2428}#
                                                             #{r\ 2429}#)
                                                      (#{rest\ 2233}#
                                                        #{r\ 2429}#
                                                        #{req\ 2341}#
                                                        #{opt\ 2342}#
                                                        (cons #t
                                                              (reverse
                                                                #{rkey\ 2343}#))))
                                                    #{tmp\ 2419}#)
                                                  (let ((#{tmp\ 2430}#
                                                          ($sc-dispatch
                                                            #{tmp\ 2348}#
                                                            '(any any))))
                                                    (if (if #{tmp\ 2430}#
                                                          (@apply
                                                            (lambda (#{a\ 2433}#
                                                                     #{b\ 2434}#)
                                                              (eq? (syntax->datum
                                                                     #{a\ 2433}#)
                                                                   #:rest))
                                                            #{tmp\ 2430}#)
                                                          #f)
                                                      (@apply
                                                        (lambda (#{a\ 2437}#
                                                                 #{b\ 2438}#)
                                                          (#{rest\ 2233}#
                                                            #{b\ 2438}#
                                                            #{req\ 2341}#
                                                            #{opt\ 2342}#
                                                            (cons #f
                                                                  (reverse
                                                                    #{rkey\ 2343}#))))
                                                        #{tmp\ 2430}#)
                                                      (let ((#{tmp\ 2439}#
                                                              (list #{tmp\ 2348}#)))
                                                        (if (if #{tmp\ 2439}#
                                                              (@apply
                                                                (lambda (#{r\ 2441}#)
                                                                  (#{id?\ 387}#
                                                                    #{r\ 2441}#))
                                                                #{tmp\ 2439}#)
                                                              #f)
                                                          (@apply
                                                            (lambda (#{r\ 2443}#)
                                                              (#{rest\ 2233}#
                                                                #{r\ 2443}#
                                                                #{req\ 2341}#
                                                                #{opt\ 2342}#
                                                                (cons #f
                                                                      (reverse
                                                                        #{rkey\ 2343}#))))
                                                            #{tmp\ 2439}#)
                                                          (let ((#{else\ 2445}#
                                                                  #{tmp\ 2348}#))
                                                            (syntax-violation
                                                              'lambda*
                                                              "invalid keyword argument list"
                                                              #{orig-args\ 2224}#
                                                              #{args\ 2340}#)))))))))))))))))))))))
                (#{rest\ 2233}#
                  (lambda (#{args\ 2446}#
                           #{req\ 2447}#
                           #{opt\ 2448}#
                           #{kw\ 2449}#)
                    (let ((#{tmp\ 2454}# #{args\ 2446}#))
                      (let ((#{tmp\ 2455}# (list #{tmp\ 2454}#)))
                        (if (if #{tmp\ 2455}#
                              (@apply
                                (lambda (#{r\ 2457}#)
                                  (#{id?\ 387}# #{r\ 2457}#))
                                #{tmp\ 2455}#)
                              #f)
                          (@apply
                            (lambda (#{r\ 2459}#)
                              (#{check\ 2235}#
                                #{req\ 2447}#
                                #{opt\ 2448}#
                                #{r\ 2459}#
                                #{kw\ 2449}#))
                            #{tmp\ 2455}#)
                          (let ((#{else\ 2461}# #{tmp\ 2454}#))
                            (syntax-violation
                              'lambda*
                              "invalid rest argument"
                              #{orig-args\ 2224}#
                              #{args\ 2446}#)))))))
                (#{check\ 2235}#
                  (lambda (#{req\ 2462}#
                           #{opt\ 2463}#
                           #{rest\ 2464}#
                           #{kw\ 2465}#)
                    (if (#{distinct-bound-ids?\ 452}#
                          (append
                            #{req\ 2462}#
                            (map car #{opt\ 2463}#)
                            (if #{rest\ 2464}#
                              (list #{rest\ 2464}#)
                              '())
                            (if (pair? #{kw\ 2465}#)
                              (map cadr (cdr #{kw\ 2465}#))
                              '())))
                      (values
                        #{req\ 2462}#
                        #{opt\ 2463}#
                        #{rest\ 2464}#
                        #{kw\ 2465}#)
                      (syntax-violation
                        'lambda*
                        "duplicate identifier in argument list"
                        #{orig-args\ 2224}#)))))
               (begin
                 (#{req\ 2227}# #{orig-args\ 2224}# (quote ()))))))
         (#{chi-lambda-case\ 496}#
           (lambda (#{e\ 2473}#
                    #{r\ 2474}#
                    #{w\ 2475}#
                    #{s\ 2476}#
                    #{mod\ 2477}#
                    #{get-formals\ 2478}#
                    #{clauses\ 2479}#)
             (letrec*
               ((#{expand-req\ 2488}#
                  (lambda (#{req\ 2495}#
                           #{opt\ 2496}#
                           #{rest\ 2497}#
                           #{kw\ 2498}#
                           #{body\ 2499}#)
                    (begin
                      (let ((#{vars\ 2507}#
                              (map #{gen-var\ 500}# #{req\ 2495}#))
                            (#{labels\ 2508}#
                              (#{gen-labels\ 405}# #{req\ 2495}#)))
                        (begin
                          (let ((#{r*\ 2511}#
                                  (#{extend-var-env\ 377}#
                                    #{labels\ 2508}#
                                    #{vars\ 2507}#
                                    #{r\ 2474}#))
                                (#{w*\ 2512}#
                                  (#{make-binding-wrap\ 434}#
                                    #{req\ 2495}#
                                    #{labels\ 2508}#
                                    #{w\ 2475}#)))
                            (#{expand-opt\ 2490}#
                              (map syntax->datum #{req\ 2495}#)
                              #{opt\ 2496}#
                              #{rest\ 2497}#
                              #{kw\ 2498}#
                              #{body\ 2499}#
                              (reverse #{vars\ 2507}#)
                              #{r*\ 2511}#
                              #{w*\ 2512}#
                              '()
                              '())))))))
                (#{expand-opt\ 2490}#
                  (lambda (#{req\ 2513}#
                           #{opt\ 2514}#
                           #{rest\ 2515}#
                           #{kw\ 2516}#
                           #{body\ 2517}#
                           #{vars\ 2518}#
                           #{r*\ 2519}#
                           #{w*\ 2520}#
                           #{out\ 2521}#
                           #{inits\ 2522}#)
                    (if (pair? #{opt\ 2514}#)
                      (let ((#{tmp\ 2535}# (car #{opt\ 2514}#)))
                        (let ((#{tmp\ 2536}#
                                ($sc-dispatch
                                  #{tmp\ 2535}#
                                  '(any any))))
                          (if #{tmp\ 2536}#
                            (@apply
                              (lambda (#{id\ 2539}# #{i\ 2540}#)
                                (begin
                                  (let ((#{v\ 2543}#
                                          (#{gen-var\ 500}# #{id\ 2539}#)))
                                    (begin
                                      (let ((#{l\ 2545}#
                                              (#{gen-labels\ 405}#
                                                (list #{v\ 2543}#))))
                                        (begin
                                          (let ((#{r**\ 2547}#
                                                  (#{extend-var-env\ 377}#
                                                    #{l\ 2545}#
                                                    (list #{v\ 2543}#)
                                                    #{r*\ 2519}#)))
                                            (begin
                                              (let ((#{w**\ 2549}#
                                                      (#{make-binding-wrap\ 434}#
                                                        (list #{id\ 2539}#)
                                                        #{l\ 2545}#
                                                        #{w*\ 2520}#)))
                                                (#{expand-opt\ 2490}#
                                                  #{req\ 2513}#
                                                  (cdr #{opt\ 2514}#)
                                                  #{rest\ 2515}#
                                                  #{kw\ 2516}#
                                                  #{body\ 2517}#
                                                  (cons #{v\ 2543}#
                                                        #{vars\ 2518}#)
                                                  #{r**\ 2547}#
                                                  #{w**\ 2549}#
                                                  (cons (syntax->datum
                                                          #{id\ 2539}#)
                                                        #{out\ 2521}#)
                                                  (cons (#{chi\ 472}#
                                                          #{i\ 2540}#
                                                          #{r*\ 2519}#
                                                          #{w*\ 2520}#
                                                          #{mod\ 2477}#)
                                                        #{inits\ 2522}#)))))))))))
                              #{tmp\ 2536}#)
                            (syntax-violation
                              #f
                              "source expression failed to match any pattern"
                              #{tmp\ 2535}#))))
                      (if #{rest\ 2515}#
                        (begin
                          (let ((#{v\ 2554}#
                                  (#{gen-var\ 500}# #{rest\ 2515}#)))
                            (begin
                              (let ((#{l\ 2556}#
                                      (#{gen-labels\ 405}#
                                        (list #{v\ 2554}#))))
                                (begin
                                  (let ((#{r*\ 2558}#
                                          (#{extend-var-env\ 377}#
                                            #{l\ 2556}#
                                            (list #{v\ 2554}#)
                                            #{r*\ 2519}#)))
                                    (begin
                                      (let ((#{w*\ 2560}#
                                              (#{make-binding-wrap\ 434}#
                                                (list #{rest\ 2515}#)
                                                #{l\ 2556}#
                                                #{w*\ 2520}#)))
                                        (#{expand-kw\ 2492}#
                                          #{req\ 2513}#
                                          (if (pair? #{out\ 2521}#)
                                            (reverse #{out\ 2521}#)
                                            #f)
                                          (syntax->datum #{rest\ 2515}#)
                                          (if (pair? #{kw\ 2516}#)
                                            (cdr #{kw\ 2516}#)
                                            #{kw\ 2516}#)
                                          #{body\ 2517}#
                                          (cons #{v\ 2554}# #{vars\ 2518}#)
                                          #{r*\ 2558}#
                                          #{w*\ 2560}#
                                          (if (pair? #{kw\ 2516}#)
                                            (car #{kw\ 2516}#)
                                            #f)
                                          '()
                                          #{inits\ 2522}#)))))))))
                        (#{expand-kw\ 2492}#
                          #{req\ 2513}#
                          (if (pair? #{out\ 2521}#)
                            (reverse #{out\ 2521}#)
                            #f)
                          #f
                          (if (pair? #{kw\ 2516}#)
                            (cdr #{kw\ 2516}#)
                            #{kw\ 2516}#)
                          #{body\ 2517}#
                          #{vars\ 2518}#
                          #{r*\ 2519}#
                          #{w*\ 2520}#
                          (if (pair? #{kw\ 2516}#) (car #{kw\ 2516}#) #f)
                          '()
                          #{inits\ 2522}#)))))
                (#{expand-kw\ 2492}#
                  (lambda (#{req\ 2562}#
                           #{opt\ 2563}#
                           #{rest\ 2564}#
                           #{kw\ 2565}#
                           #{body\ 2566}#
                           #{vars\ 2567}#
                           #{r*\ 2568}#
                           #{w*\ 2569}#
                           #{aok\ 2570}#
                           #{out\ 2571}#
                           #{inits\ 2572}#)
                    (if (pair? #{kw\ 2565}#)
                      (let ((#{tmp\ 2586}# (car #{kw\ 2565}#)))
                        (let ((#{tmp\ 2587}#
                                ($sc-dispatch
                                  #{tmp\ 2586}#
                                  '(any any any))))
                          (if #{tmp\ 2587}#
                            (@apply
                              (lambda (#{k\ 2591}# #{id\ 2592}# #{i\ 2593}#)
                                (begin
                                  (let ((#{v\ 2596}#
                                          (#{gen-var\ 500}# #{id\ 2592}#)))
                                    (begin
                                      (let ((#{l\ 2598}#
                                              (#{gen-labels\ 405}#
                                                (list #{v\ 2596}#))))
                                        (begin
                                          (let ((#{r**\ 2600}#
                                                  (#{extend-var-env\ 377}#
                                                    #{l\ 2598}#
                                                    (list #{v\ 2596}#)
                                                    #{r*\ 2568}#)))
                                            (begin
                                              (let ((#{w**\ 2602}#
                                                      (#{make-binding-wrap\ 434}#
                                                        (list #{id\ 2592}#)
                                                        #{l\ 2598}#
                                                        #{w*\ 2569}#)))
                                                (#{expand-kw\ 2492}#
                                                  #{req\ 2562}#
                                                  #{opt\ 2563}#
                                                  #{rest\ 2564}#
                                                  (cdr #{kw\ 2565}#)
                                                  #{body\ 2566}#
                                                  (cons #{v\ 2596}#
                                                        #{vars\ 2567}#)
                                                  #{r**\ 2600}#
                                                  #{w**\ 2602}#
                                                  #{aok\ 2570}#
                                                  (cons (list (syntax->datum
                                                                #{k\ 2591}#)
                                                              (syntax->datum
                                                                #{id\ 2592}#)
                                                              #{v\ 2596}#)
                                                        #{out\ 2571}#)
                                                  (cons (#{chi\ 472}#
                                                          #{i\ 2593}#
                                                          #{r*\ 2568}#
                                                          #{w*\ 2569}#
                                                          #{mod\ 2477}#)
                                                        #{inits\ 2572}#)))))))))))
                              #{tmp\ 2587}#)
                            (syntax-violation
                              #f
                              "source expression failed to match any pattern"
                              #{tmp\ 2586}#))))
                      (#{expand-body\ 2494}#
                        #{req\ 2562}#
                        #{opt\ 2563}#
                        #{rest\ 2564}#
                        (if (begin
                              (let ((#{t\ 2606}# #{aok\ 2570}#))
                                (if #{t\ 2606}#
                                  #{t\ 2606}#
                                  (pair? #{out\ 2571}#))))
                          (cons #{aok\ 2570}# (reverse #{out\ 2571}#))
                          #f)
                        #{body\ 2566}#
                        (reverse #{vars\ 2567}#)
                        #{r*\ 2568}#
                        #{w*\ 2569}#
                        (reverse #{inits\ 2572}#)
                        '()))))
                (#{expand-body\ 2494}#
                  (lambda (#{req\ 2608}#
                           #{opt\ 2609}#
                           #{rest\ 2610}#
                           #{kw\ 2611}#
                           #{body\ 2612}#
                           #{vars\ 2613}#
                           #{r*\ 2614}#
                           #{w*\ 2615}#
                           #{inits\ 2616}#
                           #{meta\ 2617}#)
                    (let ((#{tmp\ 2628}# #{body\ 2612}#))
                      (let ((#{tmp\ 2629}#
                              ($sc-dispatch
                                #{tmp\ 2628}#
                                '(any any . each-any))))
                        (if (if #{tmp\ 2629}#
                              (@apply
                                (lambda (#{docstring\ 2633}#
                                         #{e1\ 2634}#
                                         #{e2\ 2635}#)
                                  (string?
                                    (syntax->datum #{docstring\ 2633}#)))
                                #{tmp\ 2629}#)
                              #f)
                          (@apply
                            (lambda (#{docstring\ 2639}#
                                     #{e1\ 2640}#
                                     #{e2\ 2641}#)
                              (#{expand-body\ 2494}#
                                #{req\ 2608}#
                                #{opt\ 2609}#
                                #{rest\ 2610}#
                                #{kw\ 2611}#
                                (cons #{e1\ 2640}# #{e2\ 2641}#)
                                #{vars\ 2613}#
                                #{r*\ 2614}#
                                #{w*\ 2615}#
                                #{inits\ 2616}#
                                (append
                                  #{meta\ 2617}#
                                  (list (cons 'documentation
                                              (syntax->datum
                                                #{docstring\ 2639}#))))))
                            #{tmp\ 2629}#)
                          (let ((#{tmp\ 2644}#
                                  ($sc-dispatch
                                    #{tmp\ 2628}#
                                    '(#(vector #(each (any . any)))
                                      any
                                      .
                                      each-any))))
                            (if #{tmp\ 2644}#
                              (@apply
                                (lambda (#{k\ 2649}#
                                         #{v\ 2650}#
                                         #{e1\ 2651}#
                                         #{e2\ 2652}#)
                                  (#{expand-body\ 2494}#
                                    #{req\ 2608}#
                                    #{opt\ 2609}#
                                    #{rest\ 2610}#
                                    #{kw\ 2611}#
                                    (cons #{e1\ 2651}# #{e2\ 2652}#)
                                    #{vars\ 2613}#
                                    #{r*\ 2614}#
                                    #{w*\ 2615}#
                                    #{inits\ 2616}#
                                    (append
                                      #{meta\ 2617}#
                                      (syntax->datum
                                        (map cons #{k\ 2649}# #{v\ 2650}#)))))
                                #{tmp\ 2644}#)
                              (let ((#{tmp\ 2656}#
                                      ($sc-dispatch
                                        #{tmp\ 2628}#
                                        '(any . each-any))))
                                (if #{tmp\ 2656}#
                                  (@apply
                                    (lambda (#{e1\ 2659}# #{e2\ 2660}#)
                                      (values
                                        #{meta\ 2617}#
                                        #{req\ 2608}#
                                        #{opt\ 2609}#
                                        #{rest\ 2610}#
                                        #{kw\ 2611}#
                                        #{inits\ 2616}#
                                        #{vars\ 2613}#
                                        (#{chi-body\ 480}#
                                          (cons #{e1\ 2659}# #{e2\ 2660}#)
                                          (#{source-wrap\ 458}#
                                            #{e\ 2473}#
                                            #{w\ 2475}#
                                            #{s\ 2476}#
                                            #{mod\ 2477}#)
                                          #{r*\ 2614}#
                                          #{w*\ 2615}#
                                          #{mod\ 2477}#)))
                                    #{tmp\ 2656}#)
                                  (syntax-violation
                                    #f
                                    "source expression failed to match any pattern"
                                    #{tmp\ 2628}#)))))))))))
               (begin
                 (let ((#{tmp\ 2662}# #{clauses\ 2479}#))
                   (let ((#{tmp\ 2663}#
                           ($sc-dispatch #{tmp\ 2662}# (quote ()))))
                     (if #{tmp\ 2663}#
                       (@apply
                         (lambda () (values (quote ()) #f))
                         #{tmp\ 2663}#)
                       (let ((#{tmp\ 2664}#
                               ($sc-dispatch
                                 #{tmp\ 2662}#
                                 '((any any . each-any)
                                   .
                                   #(each (any any . each-any))))))
                         (if #{tmp\ 2664}#
                           (@apply
                             (lambda (#{args\ 2671}#
                                      #{e1\ 2672}#
                                      #{e2\ 2673}#
                                      #{args*\ 2674}#
                                      #{e1*\ 2675}#
                                      #{e2*\ 2676}#)
                               (call-with-values
                                 (lambda ()
                                   (#{get-formals\ 2478}# #{args\ 2671}#))
                                 (lambda (#{req\ 2677}#
                                          #{opt\ 2678}#
                                          #{rest\ 2679}#
                                          #{kw\ 2680}#)
                                   (call-with-values
                                     (lambda ()
                                       (#{expand-req\ 2488}#
                                         #{req\ 2677}#
                                         #{opt\ 2678}#
                                         #{rest\ 2679}#
                                         #{kw\ 2680}#
                                         (cons #{e1\ 2672}# #{e2\ 2673}#)))
                                     (lambda (#{meta\ 2686}#
                                              #{req\ 2687}#
                                              #{opt\ 2688}#
                                              #{rest\ 2689}#
                                              #{kw\ 2690}#
                                              #{inits\ 2691}#
                                              #{vars\ 2692}#
                                              #{body\ 2693}#)
                                       (call-with-values
                                         (lambda ()
                                           (#{chi-lambda-case\ 496}#
                                             #{e\ 2473}#
                                             #{r\ 2474}#
                                             #{w\ 2475}#
                                             #{s\ 2476}#
                                             #{mod\ 2477}#
                                             #{get-formals\ 2478}#
                                             (map (lambda (#{tmp\ 2704}#
                                                           #{tmp\ 2703}#
                                                           #{tmp\ 2702}#)
                                                    (cons #{tmp\ 2702}#
                                                          (cons #{tmp\ 2703}#
                                                                #{tmp\ 2704}#)))
                                                  #{e2*\ 2676}#
                                                  #{e1*\ 2675}#
                                                  #{args*\ 2674}#)))
                                         (lambda (#{meta*\ 2706}#
                                                  #{else*\ 2707}#)
                                           (values
                                             (append
                                               #{meta\ 2686}#
                                               #{meta*\ 2706}#)
                                             (#{build-lambda-case\ 333}#
                                               #{s\ 2476}#
                                               #{req\ 2687}#
                                               #{opt\ 2688}#
                                               #{rest\ 2689}#
                                               #{kw\ 2690}#
                                               #{inits\ 2691}#
                                               #{vars\ 2692}#
                                               #{body\ 2693}#
                                               #{else*\ 2707}#)))))))))
                             #{tmp\ 2664}#)
                           (syntax-violation
                             #f
                             "source expression failed to match any pattern"
                             #{tmp\ 2662}#))))))))))
         (#{strip\ 498}#
           (lambda (#{x\ 2710}# #{w\ 2711}#)
             (if (memq 'top
                       (#{wrap-marks\ 394}# #{w\ 2711}#))
               #{x\ 2710}#
               (letrec*
                 ((#{f\ 2717}#
                    (lambda (#{x\ 2718}#)
                      (if (#{syntax-object?\ 351}# #{x\ 2718}#)
                        (#{strip\ 498}#
                          (#{syntax-object-expression\ 353}# #{x\ 2718}#)
                          (#{syntax-object-wrap\ 355}# #{x\ 2718}#))
                        (if (pair? #{x\ 2718}#)
                          (begin
                            (let ((#{a\ 2725}# (#{f\ 2717}# (car #{x\ 2718}#)))
                                  (#{d\ 2726}#
                                    (#{f\ 2717}# (cdr #{x\ 2718}#))))
                              (if (if (eq? #{a\ 2725}# (car #{x\ 2718}#))
                                    (eq? #{d\ 2726}# (cdr #{x\ 2718}#))
                                    #f)
                                #{x\ 2718}#
                                (cons #{a\ 2725}# #{d\ 2726}#))))
                          (if (vector? #{x\ 2718}#)
                            (begin
                              (let ((#{old\ 2732}# (vector->list #{x\ 2718}#)))
                                (begin
                                  (let ((#{new\ 2734}#
                                          (map #{f\ 2717}# #{old\ 2732}#)))
                                    (if (#{and-map*\ 35}#
                                          eq?
                                          #{old\ 2732}#
                                          #{new\ 2734}#)
                                      #{x\ 2718}#
                                      (list->vector #{new\ 2734}#))))))
                            #{x\ 2718}#))))))
                 (begin (#{f\ 2717}# #{x\ 2710}#))))))
         (#{gen-var\ 500}#
           (lambda (#{id\ 2736}#)
             (begin
               (let ((#{id\ 2739}#
                       (if (#{syntax-object?\ 351}# #{id\ 2736}#)
                         (#{syntax-object-expression\ 353}# #{id\ 2736}#)
                         #{id\ 2736}#)))
                 (gensym
                   (string-append (symbol->string #{id\ 2739}#) " "))))))
         (#{lambda-var-list\ 502}#
           (lambda (#{vars\ 2741}#)
             (letrec*
               ((#{lvl\ 2747}#
                  (lambda (#{vars\ 2748}# #{ls\ 2749}# #{w\ 2750}#)
                    (if (pair? #{vars\ 2748}#)
                      (#{lvl\ 2747}#
                        (cdr #{vars\ 2748}#)
                        (cons (#{wrap\ 456}#
                                (car #{vars\ 2748}#)
                                #{w\ 2750}#
                                #f)
                              #{ls\ 2749}#)
                        #{w\ 2750}#)
                      (if (#{id?\ 387}# #{vars\ 2748}#)
                        (cons (#{wrap\ 456}# #{vars\ 2748}# #{w\ 2750}# #f)
                              #{ls\ 2749}#)
                        (if (null? #{vars\ 2748}#)
                          #{ls\ 2749}#
                          (if (#{syntax-object?\ 351}# #{vars\ 2748}#)
                            (#{lvl\ 2747}#
                              (#{syntax-object-expression\ 353}#
                                #{vars\ 2748}#)
                              #{ls\ 2749}#
                              (#{join-wraps\ 438}#
                                #{w\ 2750}#
                                (#{syntax-object-wrap\ 355}# #{vars\ 2748}#)))
                            (cons #{vars\ 2748}# #{ls\ 2749}#))))))))
               (begin
                 (#{lvl\ 2747}#
                   #{vars\ 2741}#
                   '()
                   '(())))))))
        (begin
          (set! #{make-primitive-ref\ 249}#
            (lambda (#{src\ 723}# #{name\ 724}#)
              (make-struct/no-tail
                (vector-ref %expanded-vtables 2)
                #{src\ 723}#
                #{name\ 724}#)))
          (set! #{fx+\ 288}# +)
          (set! #{fx-\ 290}# -)
          (set! #{fx=\ 292}# =)
          (set! #{fx<\ 294}# <)
          (set! #{set-syntax-object-expression!\ 359}#
            (lambda (#{x\ 1101}# #{update\ 1102}#)
              (vector-set! #{x\ 1101}# 1 #{update\ 1102}#)))
          (set! #{set-syntax-object-wrap!\ 361}#
            (lambda (#{x\ 1105}# #{update\ 1106}#)
              (vector-set! #{x\ 1105}# 2 #{update\ 1106}#)))
          (set! #{set-syntax-object-module!\ 363}#
            (lambda (#{x\ 1109}# #{update\ 1110}#)
              (vector-set! #{x\ 1109}# 3 #{update\ 1110}#)))
          (set! #{binding-type\ 370}# car)
          (set! #{binding-value\ 372}# cdr)
          (set! #{make-wrap\ 392}# cons)
          (set! #{wrap-marks\ 394}# car)
          (set! #{wrap-subst\ 396}# cdr)
          (set! #{ribcage?\ 410}#
            (lambda (#{x\ 1187}#)
              (if (vector? #{x\ 1187}#)
                (if (= (vector-length #{x\ 1187}#) 4)
                  (eq? (vector-ref #{x\ 1187}# 0) (quote ribcage))
                  #f)
                #f)))
          (begin
            (#{global-extend\ 383}#
              'local-syntax
              'letrec-syntax
              #t)
            (#{global-extend\ 383}#
              'local-syntax
              'let-syntax
              #f)
            (#{global-extend\ 383}#
              'core
              'fluid-let-syntax
              (lambda (#{e\ 2761}#
                       #{r\ 2762}#
                       #{w\ 2763}#
                       #{s\ 2764}#
                       #{mod\ 2765}#)
                (let ((#{tmp\ 2771}# #{e\ 2761}#))
                  (let ((#{tmp\ 2772}#
                          ($sc-dispatch
                            #{tmp\ 2771}#
                            '(any #(each (any any)) any . each-any))))
                    (if (if #{tmp\ 2772}#
                          (@apply
                            (lambda (#{_\ 2778}#
                                     #{var\ 2779}#
                                     #{val\ 2780}#
                                     #{e1\ 2781}#
                                     #{e2\ 2782}#)
                              (#{valid-bound-ids?\ 450}# #{var\ 2779}#))
                            #{tmp\ 2772}#)
                          #f)
                      (@apply
                        (lambda (#{_\ 2789}#
                                 #{var\ 2790}#
                                 #{val\ 2791}#
                                 #{e1\ 2792}#
                                 #{e2\ 2793}#)
                          (begin
                            (let ((#{names\ 2795}#
                                    (map (lambda (#{x\ 2796}#)
                                           (#{id-var-name\ 444}#
                                             #{x\ 2796}#
                                             #{w\ 2763}#))
                                         #{var\ 2790}#)))
                              (begin
                                (for-each
                                  (lambda (#{id\ 2799}# #{n\ 2800}#)
                                    (begin
                                      (let ((#{atom-key\ 2805}#
                                              (#{binding-type\ 370}#
                                                (#{lookup\ 381}#
                                                  #{n\ 2800}#
                                                  #{r\ 2762}#
                                                  #{mod\ 2765}#))))
                                        (if (eqv? #{atom-key\ 2805}#
                                                  'displaced-lexical)
                                          (syntax-violation
                                            'fluid-let-syntax
                                            "identifier out of context"
                                            #{e\ 2761}#
                                            (#{source-wrap\ 458}#
                                              #{id\ 2799}#
                                              #{w\ 2763}#
                                              #{s\ 2764}#
                                              #{mod\ 2765}#))))))
                                  #{var\ 2790}#
                                  #{names\ 2795}#)
                                (#{chi-body\ 480}#
                                  (cons #{e1\ 2792}# #{e2\ 2793}#)
                                  (#{source-wrap\ 458}#
                                    #{e\ 2761}#
                                    #{w\ 2763}#
                                    #{s\ 2764}#
                                    #{mod\ 2765}#)
                                  (#{extend-env\ 375}#
                                    #{names\ 2795}#
                                    (begin
                                      (let ((#{trans-r\ 2810}#
                                              (#{macros-only-env\ 379}#
                                                #{r\ 2762}#)))
                                        (map (lambda (#{x\ 2811}#)
                                               (cons 'macro
                                                     (#{eval-local-transformer\ 484}#
                                                       (#{chi\ 472}#
                                                         #{x\ 2811}#
                                                         #{trans-r\ 2810}#
                                                         #{w\ 2763}#
                                                         #{mod\ 2765}#)
                                                       #{mod\ 2765}#)))
                                             #{val\ 2791}#)))
                                    #{r\ 2762}#)
                                  #{w\ 2763}#
                                  #{mod\ 2765}#)))))
                        #{tmp\ 2772}#)
                      (let ((#{_\ 2816}# #{tmp\ 2771}#))
                        (syntax-violation
                          'fluid-let-syntax
                          "bad syntax"
                          (#{source-wrap\ 458}#
                            #{e\ 2761}#
                            #{w\ 2763}#
                            #{s\ 2764}#
                            #{mod\ 2765}#))))))))
            (#{global-extend\ 383}#
              'core
              'quote
              (lambda (#{e\ 2817}#
                       #{r\ 2818}#
                       #{w\ 2819}#
                       #{s\ 2820}#
                       #{mod\ 2821}#)
                (let ((#{tmp\ 2827}# #{e\ 2817}#))
                  (let ((#{tmp\ 2828}#
                          ($sc-dispatch #{tmp\ 2827}# (quote (any any)))))
                    (if #{tmp\ 2828}#
                      (@apply
                        (lambda (#{_\ 2831}# #{e\ 2832}#)
                          (#{build-data\ 337}#
                            #{s\ 2820}#
                            (#{strip\ 498}# #{e\ 2832}# #{w\ 2819}#)))
                        #{tmp\ 2828}#)
                      (let ((#{_\ 2834}# #{tmp\ 2827}#))
                        (syntax-violation
                          'quote
                          "bad syntax"
                          (#{source-wrap\ 458}#
                            #{e\ 2817}#
                            #{w\ 2819}#
                            #{s\ 2820}#
                            #{mod\ 2821}#))))))))
            (#{global-extend\ 383}#
              'core
              'syntax
              (letrec*
                ((#{gen-syntax\ 2836}#
                   (lambda (#{src\ 2851}#
                            #{e\ 2852}#
                            #{r\ 2853}#
                            #{maps\ 2854}#
                            #{ellipsis?\ 2855}#
                            #{mod\ 2856}#)
                     (if (#{id?\ 387}# #{e\ 2852}#)
                       (begin
                         (let ((#{label\ 2864}#
                                 (#{id-var-name\ 444}#
                                   #{e\ 2852}#
                                   '(()))))
                           (begin
                             (let ((#{b\ 2867}#
                                     (#{lookup\ 381}#
                                       #{label\ 2864}#
                                       #{r\ 2853}#
                                       #{mod\ 2856}#)))
                               (if (eq? (#{binding-type\ 370}# #{b\ 2867}#)
                                        'syntax)
                                 (call-with-values
                                   (lambda ()
                                     (begin
                                       (let ((#{var.lev\ 2869}#
                                               (#{binding-value\ 372}#
                                                 #{b\ 2867}#)))
                                         (#{gen-ref\ 2838}#
                                           #{src\ 2851}#
                                           (car #{var.lev\ 2869}#)
                                           (cdr #{var.lev\ 2869}#)
                                           #{maps\ 2854}#))))
                                   (lambda (#{var\ 2870}# #{maps\ 2871}#)
                                     (values
                                       (list (quote ref) #{var\ 2870}#)
                                       #{maps\ 2871}#)))
                                 (if (#{ellipsis?\ 2855}# #{e\ 2852}#)
                                   (syntax-violation
                                     'syntax
                                     "misplaced ellipsis"
                                     #{src\ 2851}#)
                                   (values
                                     (list (quote quote) #{e\ 2852}#)
                                     #{maps\ 2854}#)))))))
                       (let ((#{tmp\ 2876}# #{e\ 2852}#))
                         (let ((#{tmp\ 2877}#
                                 ($sc-dispatch
                                   #{tmp\ 2876}#
                                   '(any any))))
                           (if (if #{tmp\ 2877}#
                                 (@apply
                                   (lambda (#{dots\ 2880}# #{e\ 2881}#)
                                     (#{ellipsis?\ 2855}# #{dots\ 2880}#))
                                   #{tmp\ 2877}#)
                                 #f)
                             (@apply
                               (lambda (#{dots\ 2884}# #{e\ 2885}#)
                                 (#{gen-syntax\ 2836}#
                                   #{src\ 2851}#
                                   #{e\ 2885}#
                                   #{r\ 2853}#
                                   #{maps\ 2854}#
                                   (lambda (#{x\ 2886}#) #f)
                                   #{mod\ 2856}#))
                               #{tmp\ 2877}#)
                             (let ((#{tmp\ 2888}#
                                     ($sc-dispatch
                                       #{tmp\ 2876}#
                                       '(any any . any))))
                               (if (if #{tmp\ 2888}#
                                     (@apply
                                       (lambda (#{x\ 2892}#
                                                #{dots\ 2893}#
                                                #{y\ 2894}#)
                                         (#{ellipsis?\ 2855}# #{dots\ 2893}#))
                                       #{tmp\ 2888}#)
                                     #f)
                                 (@apply
                                   (lambda (#{x\ 2898}#
                                            #{dots\ 2899}#
                                            #{y\ 2900}#)
                                     (letrec*
                                       ((#{f\ 2904}#
                                          (lambda (#{y\ 2905}# #{k\ 2906}#)
                                            (let ((#{tmp\ 2913}# #{y\ 2905}#))
                                              (let ((#{tmp\ 2914}#
                                                      ($sc-dispatch
                                                        #{tmp\ 2913}#
                                                        '(any . any))))
                                                (if (if #{tmp\ 2914}#
                                                      (@apply
                                                        (lambda (#{dots\ 2917}#
                                                                 #{y\ 2918}#)
                                                          (#{ellipsis?\ 2855}#
                                                            #{dots\ 2917}#))
                                                        #{tmp\ 2914}#)
                                                      #f)
                                                  (@apply
                                                    (lambda (#{dots\ 2921}#
                                                             #{y\ 2922}#)
                                                      (#{f\ 2904}#
                                                        #{y\ 2922}#
                                                        (lambda (#{maps\ 2923}#)
                                                          (call-with-values
                                                            (lambda ()
                                                              (#{k\ 2906}#
                                                                (cons '()
                                                                      #{maps\ 2923}#)))
                                                            (lambda (#{x\ 2925}#
                                                                     #{maps\ 2926}#)
                                                              (if (null? (car #{maps\ 2926}#))
                                                                (syntax-violation
                                                                  'syntax
                                                                  "extra ellipsis"
                                                                  #{src\ 2851}#)
                                                                (values
                                                                  (#{gen-mappend\ 2840}#
                                                                    #{x\ 2925}#
                                                                    (car #{maps\ 2926}#))
                                                                  (cdr #{maps\ 2926}#))))))))
                                                    #{tmp\ 2914}#)
                                                  (let ((#{_\ 2930}#
                                                          #{tmp\ 2913}#))
                                                    (call-with-values
                                                      (lambda ()
                                                        (#{gen-syntax\ 2836}#
                                                          #{src\ 2851}#
                                                          #{y\ 2905}#
                                                          #{r\ 2853}#
                                                          #{maps\ 2854}#
                                                          #{ellipsis?\ 2855}#
                                                          #{mod\ 2856}#))
                                                      (lambda (#{y\ 2931}#
                                                               #{maps\ 2932}#)
                                                        (call-with-values
                                                          (lambda ()
                                                            (#{k\ 2906}#
                                                              #{maps\ 2932}#))
                                                          (lambda (#{x\ 2935}#
                                                                   #{maps\ 2936}#)
                                                            (values
                                                              (#{gen-append\ 2846}#
                                                                #{x\ 2935}#
                                                                #{y\ 2931}#)
                                                              #{maps\ 2936}#))))))))))))
                                       (begin
                                         (#{f\ 2904}#
                                           #{y\ 2900}#
                                           (lambda (#{maps\ 2907}#)
                                             (call-with-values
                                               (lambda ()
                                                 (#{gen-syntax\ 2836}#
                                                   #{src\ 2851}#
                                                   #{x\ 2898}#
                                                   #{r\ 2853}#
                                                   (cons '()
                                                         #{maps\ 2907}#)
                                                   #{ellipsis?\ 2855}#
                                                   #{mod\ 2856}#))
                                               (lambda (#{x\ 2909}#
                                                        #{maps\ 2910}#)
                                                 (if (null? (car #{maps\ 2910}#))
                                                   (syntax-violation
                                                     'syntax
                                                     "extra ellipsis"
                                                     #{src\ 2851}#)
                                                   (values
                                                     (#{gen-map\ 2842}#
                                                       #{x\ 2909}#
                                                       (car #{maps\ 2910}#))
                                                     (cdr #{maps\ 2910}#))))))))))
                                   #{tmp\ 2888}#)
                                 (let ((#{tmp\ 2939}#
                                         ($sc-dispatch
                                           #{tmp\ 2876}#
                                           '(any . any))))
                                   (if #{tmp\ 2939}#
                                     (@apply
                                       (lambda (#{x\ 2942}# #{y\ 2943}#)
                                         (call-with-values
                                           (lambda ()
                                             (#{gen-syntax\ 2836}#
                                               #{src\ 2851}#
                                               #{x\ 2942}#
                                               #{r\ 2853}#
                                               #{maps\ 2854}#
                                               #{ellipsis?\ 2855}#
                                               #{mod\ 2856}#))
                                           (lambda (#{x\ 2944}# #{maps\ 2945}#)
                                             (call-with-values
                                               (lambda ()
                                                 (#{gen-syntax\ 2836}#
                                                   #{src\ 2851}#
                                                   #{y\ 2943}#
                                                   #{r\ 2853}#
                                                   #{maps\ 2945}#
                                                   #{ellipsis?\ 2855}#
                                                   #{mod\ 2856}#))
                                               (lambda (#{y\ 2948}#
                                                        #{maps\ 2949}#)
                                                 (values
                                                   (#{gen-cons\ 2844}#
                                                     #{x\ 2944}#
                                                     #{y\ 2948}#)
                                                   #{maps\ 2949}#))))))
                                       #{tmp\ 2939}#)
                                     (let ((#{tmp\ 2952}#
                                             ($sc-dispatch
                                               #{tmp\ 2876}#
                                               '#(vector (any . each-any)))))
                                       (if #{tmp\ 2952}#
                                         (@apply
                                           (lambda (#{e1\ 2955}# #{e2\ 2956}#)
                                             (call-with-values
                                               (lambda ()
                                                 (#{gen-syntax\ 2836}#
                                                   #{src\ 2851}#
                                                   (cons #{e1\ 2955}#
                                                         #{e2\ 2956}#)
                                                   #{r\ 2853}#
                                                   #{maps\ 2854}#
                                                   #{ellipsis?\ 2855}#
                                                   #{mod\ 2856}#))
                                               (lambda (#{e\ 2958}#
                                                        #{maps\ 2959}#)
                                                 (values
                                                   (#{gen-vector\ 2848}#
                                                     #{e\ 2958}#)
                                                   #{maps\ 2959}#))))
                                           #{tmp\ 2952}#)
                                         (let ((#{_\ 2963}# #{tmp\ 2876}#))
                                           (values
                                             (list (quote quote) #{e\ 2852}#)
                                             #{maps\ 2854}#))))))))))))))
                 (#{gen-ref\ 2838}#
                   (lambda (#{src\ 2965}#
                            #{var\ 2966}#
                            #{level\ 2967}#
                            #{maps\ 2968}#)
                     (if (#{fx=\ 292}# #{level\ 2967}# 0)
                       (values #{var\ 2966}# #{maps\ 2968}#)
                       (if (null? #{maps\ 2968}#)
                         (syntax-violation
                           'syntax
                           "missing ellipsis"
                           #{src\ 2965}#)
                         (call-with-values
                           (lambda ()
                             (#{gen-ref\ 2838}#
                               #{src\ 2965}#
                               #{var\ 2966}#
                               (#{fx-\ 290}# #{level\ 2967}# 1)
                               (cdr #{maps\ 2968}#)))
                           (lambda (#{outer-var\ 2973}# #{outer-maps\ 2974}#)
                             (begin
                               (let ((#{b\ 2978}#
                                       (assq #{outer-var\ 2973}#
                                             (car #{maps\ 2968}#))))
                                 (if #{b\ 2978}#
                                   (values (cdr #{b\ 2978}#) #{maps\ 2968}#)
                                   (begin
                                     (let ((#{inner-var\ 2980}#
                                             (#{gen-var\ 500}# (quote tmp))))
                                       (values
                                         #{inner-var\ 2980}#
                                         (cons (cons (cons #{outer-var\ 2973}#
                                                           #{inner-var\ 2980}#)
                                                     (car #{maps\ 2968}#))
                                               #{outer-maps\ 2974}#)))))))))))))
                 (#{gen-mappend\ 2840}#
                   (lambda (#{e\ 2981}# #{map-env\ 2982}#)
                     (list 'apply
                           '(primitive append)
                           (#{gen-map\ 2842}# #{e\ 2981}# #{map-env\ 2982}#))))
                 (#{gen-map\ 2842}#
                   (lambda (#{e\ 2986}# #{map-env\ 2987}#)
                     (begin
                       (let ((#{formals\ 2992}# (map cdr #{map-env\ 2987}#))
                             (#{actuals\ 2993}#
                               (map (lambda (#{x\ 2994}#)
                                      (list (quote ref) (car #{x\ 2994}#)))
                                    #{map-env\ 2987}#)))
                         (if (eq? (car #{e\ 2986}#) (quote ref))
                           (car #{actuals\ 2993}#)
                           (if (and-map
                                 (lambda (#{x\ 3001}#)
                                   (if (eq? (car #{x\ 3001}#) (quote ref))
                                     (memq (car (cdr #{x\ 3001}#))
                                           #{formals\ 2992}#)
                                     #f))
                                 (cdr #{e\ 2986}#))
                             (cons 'map
                                   (cons (list 'primitive
                                               (car #{e\ 2986}#))
                                         (map (begin
                                                (let ((#{r\ 3007}#
                                                        (map cons
                                                             #{formals\ 2992}#
                                                             #{actuals\ 2993}#)))
                                                  (lambda (#{x\ 3008}#)
                                                    (cdr (assq (car (cdr #{x\ 3008}#))
                                                               #{r\ 3007}#)))))
                                              (cdr #{e\ 2986}#))))
                             (cons 'map
                                   (cons (list 'lambda
                                               #{formals\ 2992}#
                                               #{e\ 2986}#)
                                         #{actuals\ 2993}#))))))))
                 (#{gen-cons\ 2844}#
                   (lambda (#{x\ 3012}# #{y\ 3013}#)
                     (begin
                       (let ((#{atom-key\ 3018}# (car #{y\ 3013}#)))
                         (if (eqv? #{atom-key\ 3018}# (quote quote))
                           (if (eq? (car #{x\ 3012}#) (quote quote))
                             (list 'quote
                                   (cons (car (cdr #{x\ 3012}#))
                                         (car (cdr #{y\ 3013}#))))
                             (if (eq? (car (cdr #{y\ 3013}#)) (quote ()))
                               (list (quote list) #{x\ 3012}#)
                               (list (quote cons) #{x\ 3012}# #{y\ 3013}#)))
                           (if (eqv? #{atom-key\ 3018}# (quote list))
                             (cons 'list
                                   (cons #{x\ 3012}# (cdr #{y\ 3013}#)))
                             (list (quote cons) #{x\ 3012}# #{y\ 3013}#)))))))
                 (#{gen-append\ 2846}#
                   (lambda (#{x\ 3027}# #{y\ 3028}#)
                     (if (equal? #{y\ 3028}# (quote (quote ())))
                       #{x\ 3027}#
                       (list (quote append) #{x\ 3027}# #{y\ 3028}#))))
                 (#{gen-vector\ 2848}#
                   (lambda (#{x\ 3032}#)
                     (if (eq? (car #{x\ 3032}#) (quote list))
                       (cons (quote vector) (cdr #{x\ 3032}#))
                       (if (eq? (car #{x\ 3032}#) (quote quote))
                         (list 'quote
                               (list->vector (car (cdr #{x\ 3032}#))))
                         (list (quote list->vector) #{x\ 3032}#)))))
                 (#{regen\ 2850}#
                   (lambda (#{x\ 3042}#)
                     (begin
                       (let ((#{atom-key\ 3046}# (car #{x\ 3042}#)))
                         (if (eqv? #{atom-key\ 3046}# (quote ref))
                           (#{build-lexical-reference\ 315}#
                             'value
                             #f
                             (car (cdr #{x\ 3042}#))
                             (car (cdr #{x\ 3042}#)))
                           (if (eqv? #{atom-key\ 3046}# (quote primitive))
                             (#{build-primref\ 335}#
                               #f
                               (car (cdr #{x\ 3042}#)))
                             (if (eqv? #{atom-key\ 3046}# (quote quote))
                               (#{build-data\ 337}# #f (car (cdr #{x\ 3042}#)))
                               (if (eqv? #{atom-key\ 3046}# (quote lambda))
                                 (if (list? (car (cdr #{x\ 3042}#)))
                                   (#{build-simple-lambda\ 329}#
                                     #f
                                     (car (cdr #{x\ 3042}#))
                                     #f
                                     (car (cdr #{x\ 3042}#))
                                     '()
                                     (#{regen\ 2850}#
                                       (car (cdr (cdr #{x\ 3042}#)))))
                                   (error "how did we get here" #{x\ 3042}#))
                                 (#{build-application\ 309}#
                                   #f
                                   (#{build-primref\ 335}#
                                     #f
                                     (car #{x\ 3042}#))
                                   (map #{regen\ 2850}#
                                        (cdr #{x\ 3042}#))))))))))))
                (begin
                  (lambda (#{e\ 3058}#
                           #{r\ 3059}#
                           #{w\ 3060}#
                           #{s\ 3061}#
                           #{mod\ 3062}#)
                    (begin
                      (let ((#{e\ 3069}#
                              (#{source-wrap\ 458}#
                                #{e\ 3058}#
                                #{w\ 3060}#
                                #{s\ 3061}#
                                #{mod\ 3062}#)))
                        (let ((#{tmp\ 3070}# #{e\ 3069}#))
                          (let ((#{tmp\ 3071}#
                                  ($sc-dispatch
                                    #{tmp\ 3070}#
                                    '(any any))))
                            (if #{tmp\ 3071}#
                              (@apply
                                (lambda (#{_\ 3074}# #{x\ 3075}#)
                                  (call-with-values
                                    (lambda ()
                                      (#{gen-syntax\ 2836}#
                                        #{e\ 3069}#
                                        #{x\ 3075}#
                                        #{r\ 3059}#
                                        '()
                                        #{ellipsis?\ 488}#
                                        #{mod\ 3062}#))
                                    (lambda (#{e\ 3076}# #{maps\ 3077}#)
                                      (#{regen\ 2850}# #{e\ 3076}#))))
                                #{tmp\ 3071}#)
                              (let ((#{_\ 3081}# #{tmp\ 3070}#))
                                (syntax-violation
                                  'syntax
                                  "bad `syntax' form"
                                  #{e\ 3069}#)))))))))))
            (#{global-extend\ 383}#
              'core
              'lambda
              (lambda (#{e\ 3082}#
                       #{r\ 3083}#
                       #{w\ 3084}#
                       #{s\ 3085}#
                       #{mod\ 3086}#)
                (let ((#{tmp\ 3092}# #{e\ 3082}#))
                  (let ((#{tmp\ 3093}#
                          ($sc-dispatch
                            #{tmp\ 3092}#
                            '(any any any . each-any))))
                    (if #{tmp\ 3093}#
                      (@apply
                        (lambda (#{_\ 3098}#
                                 #{args\ 3099}#
                                 #{e1\ 3100}#
                                 #{e2\ 3101}#)
                          (call-with-values
                            (lambda ()
                              (#{lambda-formals\ 490}# #{args\ 3099}#))
                            (lambda (#{req\ 3102}#
                                     #{opt\ 3103}#
                                     #{rest\ 3104}#
                                     #{kw\ 3105}#)
                              (letrec*
                                ((#{lp\ 3113}#
                                   (lambda (#{body\ 3114}# #{meta\ 3115}#)
                                     (let ((#{tmp\ 3117}# #{body\ 3114}#))
                                       (let ((#{tmp\ 3118}#
                                               ($sc-dispatch
                                                 #{tmp\ 3117}#
                                                 '(any any . each-any))))
                                         (if (if #{tmp\ 3118}#
                                               (@apply
                                                 (lambda (#{docstring\ 3122}#
                                                          #{e1\ 3123}#
                                                          #{e2\ 3124}#)
                                                   (string?
                                                     (syntax->datum
                                                       #{docstring\ 3122}#)))
                                                 #{tmp\ 3118}#)
                                               #f)
                                           (@apply
                                             (lambda (#{docstring\ 3128}#
                                                      #{e1\ 3129}#
                                                      #{e2\ 3130}#)
                                               (#{lp\ 3113}#
                                                 (cons #{e1\ 3129}#
                                                       #{e2\ 3130}#)
                                                 (append
                                                   #{meta\ 3115}#
                                                   (list (cons 'documentation
                                                               (syntax->datum
                                                                 #{docstring\ 3128}#))))))
                                             #{tmp\ 3118}#)
                                           (let ((#{tmp\ 3133}#
                                                   ($sc-dispatch
                                                     #{tmp\ 3117}#
                                                     '(#(vector
                                                         #(each (any . any)))
                                                       any
                                                       .
                                                       each-any))))
                                             (if #{tmp\ 3133}#
                                               (@apply
                                                 (lambda (#{k\ 3138}#
                                                          #{v\ 3139}#
                                                          #{e1\ 3140}#
                                                          #{e2\ 3141}#)
                                                   (#{lp\ 3113}#
                                                     (cons #{e1\ 3140}#
                                                           #{e2\ 3141}#)
                                                     (append
                                                       #{meta\ 3115}#
                                                       (syntax->datum
                                                         (map cons
                                                              #{k\ 3138}#
                                                              #{v\ 3139}#)))))
                                                 #{tmp\ 3133}#)
                                               (let ((#{_\ 3146}#
                                                       #{tmp\ 3117}#))
                                                 (#{chi-simple-lambda\ 492}#
                                                   #{e\ 3082}#
                                                   #{r\ 3083}#
                                                   #{w\ 3084}#
                                                   #{s\ 3085}#
                                                   #{mod\ 3086}#
                                                   #{req\ 3102}#
                                                   #{rest\ 3104}#
                                                   #{meta\ 3115}#
                                                   #{body\ 3114}#))))))))))
                                (begin
                                  (#{lp\ 3113}#
                                    (cons #{e1\ 3100}# #{e2\ 3101}#)
                                    '()))))))
                        #{tmp\ 3093}#)
                      (let ((#{_\ 3148}# #{tmp\ 3092}#))
                        (syntax-violation
                          'lambda
                          "bad lambda"
                          #{e\ 3082}#)))))))
            (#{global-extend\ 383}#
              'core
              'lambda*
              (lambda (#{e\ 3149}#
                       #{r\ 3150}#
                       #{w\ 3151}#
                       #{s\ 3152}#
                       #{mod\ 3153}#)
                (let ((#{tmp\ 3159}# #{e\ 3149}#))
                  (let ((#{tmp\ 3160}#
                          ($sc-dispatch
                            #{tmp\ 3159}#
                            '(any any any . each-any))))
                    (if #{tmp\ 3160}#
                      (@apply
                        (lambda (#{_\ 3165}#
                                 #{args\ 3166}#
                                 #{e1\ 3167}#
                                 #{e2\ 3168}#)
                          (call-with-values
                            (lambda ()
                              (#{chi-lambda-case\ 496}#
                                #{e\ 3149}#
                                #{r\ 3150}#
                                #{w\ 3151}#
                                #{s\ 3152}#
                                #{mod\ 3153}#
                                #{lambda*-formals\ 494}#
                                (list (cons #{args\ 3166}#
                                            (cons #{e1\ 3167}#
                                                  #{e2\ 3168}#)))))
                            (lambda (#{meta\ 3170}# #{lcase\ 3171}#)
                              (#{build-case-lambda\ 331}#
                                #{s\ 3152}#
                                #{meta\ 3170}#
                                #{lcase\ 3171}#))))
                        #{tmp\ 3160}#)
                      (let ((#{_\ 3175}# #{tmp\ 3159}#))
                        (syntax-violation
                          'lambda
                          "bad lambda*"
                          #{e\ 3149}#)))))))
            (#{global-extend\ 383}#
              'core
              'case-lambda
              (lambda (#{e\ 3176}#
                       #{r\ 3177}#
                       #{w\ 3178}#
                       #{s\ 3179}#
                       #{mod\ 3180}#)
                (let ((#{tmp\ 3186}# #{e\ 3176}#))
                  (let ((#{tmp\ 3187}#
                          ($sc-dispatch
                            #{tmp\ 3186}#
                            '(any (any any . each-any)
                                  .
                                  #(each (any any . each-any))))))
                    (if #{tmp\ 3187}#
                      (@apply
                        (lambda (#{_\ 3195}#
                                 #{args\ 3196}#
                                 #{e1\ 3197}#
                                 #{e2\ 3198}#
                                 #{args*\ 3199}#
                                 #{e1*\ 3200}#
                                 #{e2*\ 3201}#)
                          (call-with-values
                            (lambda ()
                              (#{chi-lambda-case\ 496}#
                                #{e\ 3176}#
                                #{r\ 3177}#
                                #{w\ 3178}#
                                #{s\ 3179}#
                                #{mod\ 3180}#
                                #{lambda-formals\ 490}#
                                (cons (cons #{args\ 3196}#
                                            (cons #{e1\ 3197}# #{e2\ 3198}#))
                                      (map (lambda (#{tmp\ 3205}#
                                                    #{tmp\ 3204}#
                                                    #{tmp\ 3203}#)
                                             (cons #{tmp\ 3203}#
                                                   (cons #{tmp\ 3204}#
                                                         #{tmp\ 3205}#)))
                                           #{e2*\ 3201}#
                                           #{e1*\ 3200}#
                                           #{args*\ 3199}#))))
                            (lambda (#{meta\ 3207}# #{lcase\ 3208}#)
                              (#{build-case-lambda\ 331}#
                                #{s\ 3179}#
                                #{meta\ 3207}#
                                #{lcase\ 3208}#))))
                        #{tmp\ 3187}#)
                      (let ((#{_\ 3212}# #{tmp\ 3186}#))
                        (syntax-violation
                          'case-lambda
                          "bad case-lambda"
                          #{e\ 3176}#)))))))
            (#{global-extend\ 383}#
              'core
              'case-lambda*
              (lambda (#{e\ 3213}#
                       #{r\ 3214}#
                       #{w\ 3215}#
                       #{s\ 3216}#
                       #{mod\ 3217}#)
                (let ((#{tmp\ 3223}# #{e\ 3213}#))
                  (let ((#{tmp\ 3224}#
                          ($sc-dispatch
                            #{tmp\ 3223}#
                            '(any (any any . each-any)
                                  .
                                  #(each (any any . each-any))))))
                    (if #{tmp\ 3224}#
                      (@apply
                        (lambda (#{_\ 3232}#
                                 #{args\ 3233}#
                                 #{e1\ 3234}#
                                 #{e2\ 3235}#
                                 #{args*\ 3236}#
                                 #{e1*\ 3237}#
                                 #{e2*\ 3238}#)
                          (call-with-values
                            (lambda ()
                              (#{chi-lambda-case\ 496}#
                                #{e\ 3213}#
                                #{r\ 3214}#
                                #{w\ 3215}#
                                #{s\ 3216}#
                                #{mod\ 3217}#
                                #{lambda*-formals\ 494}#
                                (cons (cons #{args\ 3233}#
                                            (cons #{e1\ 3234}# #{e2\ 3235}#))
                                      (map (lambda (#{tmp\ 3242}#
                                                    #{tmp\ 3241}#
                                                    #{tmp\ 3240}#)
                                             (cons #{tmp\ 3240}#
                                                   (cons #{tmp\ 3241}#
                                                         #{tmp\ 3242}#)))
                                           #{e2*\ 3238}#
                                           #{e1*\ 3237}#
                                           #{args*\ 3236}#))))
                            (lambda (#{meta\ 3244}# #{lcase\ 3245}#)
                              (#{build-case-lambda\ 331}#
                                #{s\ 3216}#
                                #{meta\ 3244}#
                                #{lcase\ 3245}#))))
                        #{tmp\ 3224}#)
                      (let ((#{_\ 3249}# #{tmp\ 3223}#))
                        (syntax-violation
                          'case-lambda
                          "bad case-lambda*"
                          #{e\ 3213}#)))))))
            (#{global-extend\ 383}#
              'core
              'let
              (letrec*
                ((#{chi-let\ 3251}#
                   (lambda (#{e\ 3252}#
                            #{r\ 3253}#
                            #{w\ 3254}#
                            #{s\ 3255}#
                            #{mod\ 3256}#
                            #{constructor\ 3257}#
                            #{ids\ 3258}#
                            #{vals\ 3259}#
                            #{exps\ 3260}#)
                     (if (not (#{valid-bound-ids?\ 450}# #{ids\ 3258}#))
                       (syntax-violation
                         'let
                         "duplicate bound variable"
                         #{e\ 3252}#)
                       (begin
                         (let ((#{labels\ 3272}#
                                 (#{gen-labels\ 405}# #{ids\ 3258}#))
                               (#{new-vars\ 3273}#
                                 (map #{gen-var\ 500}# #{ids\ 3258}#)))
                           (begin
                             (let ((#{nw\ 3276}#
                                     (#{make-binding-wrap\ 434}#
                                       #{ids\ 3258}#
                                       #{labels\ 3272}#
                                       #{w\ 3254}#))
                                   (#{nr\ 3277}#
                                     (#{extend-var-env\ 377}#
                                       #{labels\ 3272}#
                                       #{new-vars\ 3273}#
                                       #{r\ 3253}#)))
                               (#{constructor\ 3257}#
                                 #{s\ 3255}#
                                 (map syntax->datum #{ids\ 3258}#)
                                 #{new-vars\ 3273}#
                                 (map (lambda (#{x\ 3278}#)
                                        (#{chi\ 472}#
                                          #{x\ 3278}#
                                          #{r\ 3253}#
                                          #{w\ 3254}#
                                          #{mod\ 3256}#))
                                      #{vals\ 3259}#)
                                 (#{chi-body\ 480}#
                                   #{exps\ 3260}#
                                   (#{source-wrap\ 458}#
                                     #{e\ 3252}#
                                     #{nw\ 3276}#
                                     #{s\ 3255}#
                                     #{mod\ 3256}#)
                                   #{nr\ 3277}#
                                   #{nw\ 3276}#
                                   #{mod\ 3256}#))))))))))
                (begin
                  (lambda (#{e\ 3280}#
                           #{r\ 3281}#
                           #{w\ 3282}#
                           #{s\ 3283}#
                           #{mod\ 3284}#)
                    (let ((#{tmp\ 3290}# #{e\ 3280}#))
                      (let ((#{tmp\ 3291}#
                              ($sc-dispatch
                                #{tmp\ 3290}#
                                '(any #(each (any any)) any . each-any))))
                        (if (if #{tmp\ 3291}#
                              (@apply
                                (lambda (#{_\ 3297}#
                                         #{id\ 3298}#
                                         #{val\ 3299}#
                                         #{e1\ 3300}#
                                         #{e2\ 3301}#)
                                  (and-map #{id?\ 387}# #{id\ 3298}#))
                                #{tmp\ 3291}#)
                              #f)
                          (@apply
                            (lambda (#{_\ 3308}#
                                     #{id\ 3309}#
                                     #{val\ 3310}#
                                     #{e1\ 3311}#
                                     #{e2\ 3312}#)
                              (#{chi-let\ 3251}#
                                #{e\ 3280}#
                                #{r\ 3281}#
                                #{w\ 3282}#
                                #{s\ 3283}#
                                #{mod\ 3284}#
                                #{build-let\ 341}#
                                #{id\ 3309}#
                                #{val\ 3310}#
                                (cons #{e1\ 3311}# #{e2\ 3312}#)))
                            #{tmp\ 3291}#)
                          (let ((#{tmp\ 3316}#
                                  ($sc-dispatch
                                    #{tmp\ 3290}#
                                    '(any any
                                          #(each (any any))
                                          any
                                          .
                                          each-any))))
                            (if (if #{tmp\ 3316}#
                                  (@apply
                                    (lambda (#{_\ 3323}#
                                             #{f\ 3324}#
                                             #{id\ 3325}#
                                             #{val\ 3326}#
                                             #{e1\ 3327}#
                                             #{e2\ 3328}#)
                                      (if (#{id?\ 387}# #{f\ 3324}#)
                                        (and-map #{id?\ 387}# #{id\ 3325}#)
                                        #f))
                                    #{tmp\ 3316}#)
                                  #f)
                              (@apply
                                (lambda (#{_\ 3338}#
                                         #{f\ 3339}#
                                         #{id\ 3340}#
                                         #{val\ 3341}#
                                         #{e1\ 3342}#
                                         #{e2\ 3343}#)
                                  (#{chi-let\ 3251}#
                                    #{e\ 3280}#
                                    #{r\ 3281}#
                                    #{w\ 3282}#
                                    #{s\ 3283}#
                                    #{mod\ 3284}#
                                    #{build-named-let\ 343}#
                                    (cons #{f\ 3339}# #{id\ 3340}#)
                                    #{val\ 3341}#
                                    (cons #{e1\ 3342}# #{e2\ 3343}#)))
                                #{tmp\ 3316}#)
                              (let ((#{_\ 3348}# #{tmp\ 3290}#))
                                (syntax-violation
                                  'let
                                  "bad let"
                                  (#{source-wrap\ 458}#
                                    #{e\ 3280}#
                                    #{w\ 3282}#
                                    #{s\ 3283}#
                                    #{mod\ 3284}#))))))))))))
            (#{global-extend\ 383}#
              'core
              'letrec
              (lambda (#{e\ 3349}#
                       #{r\ 3350}#
                       #{w\ 3351}#
                       #{s\ 3352}#
                       #{mod\ 3353}#)
                (let ((#{tmp\ 3359}# #{e\ 3349}#))
                  (let ((#{tmp\ 3360}#
                          ($sc-dispatch
                            #{tmp\ 3359}#
                            '(any #(each (any any)) any . each-any))))
                    (if (if #{tmp\ 3360}#
                          (@apply
                            (lambda (#{_\ 3366}#
                                     #{id\ 3367}#
                                     #{val\ 3368}#
                                     #{e1\ 3369}#
                                     #{e2\ 3370}#)
                              (and-map #{id?\ 387}# #{id\ 3367}#))
                            #{tmp\ 3360}#)
                          #f)
                      (@apply
                        (lambda (#{_\ 3377}#
                                 #{id\ 3378}#
                                 #{val\ 3379}#
                                 #{e1\ 3380}#
                                 #{e2\ 3381}#)
                          (begin
                            (let ((#{ids\ 3383}# #{id\ 3378}#))
                              (if (not (#{valid-bound-ids?\ 450}#
                                         #{ids\ 3383}#))
                                (syntax-violation
                                  'letrec
                                  "duplicate bound variable"
                                  #{e\ 3349}#)
                                (begin
                                  (let ((#{labels\ 3387}#
                                          (#{gen-labels\ 405}# #{ids\ 3383}#))
                                        (#{new-vars\ 3388}#
                                          (map #{gen-var\ 500}#
                                               #{ids\ 3383}#)))
                                    (begin
                                      (let ((#{w\ 3391}#
                                              (#{make-binding-wrap\ 434}#
                                                #{ids\ 3383}#
                                                #{labels\ 3387}#
                                                #{w\ 3351}#))
                                            (#{r\ 3392}#
                                              (#{extend-var-env\ 377}#
                                                #{labels\ 3387}#
                                                #{new-vars\ 3388}#
                                                #{r\ 3350}#)))
                                        (#{build-letrec\ 345}#
                                          #{s\ 3352}#
                                          #f
                                          (map syntax->datum #{ids\ 3383}#)
                                          #{new-vars\ 3388}#
                                          (map (lambda (#{x\ 3393}#)
                                                 (#{chi\ 472}#
                                                   #{x\ 3393}#
                                                   #{r\ 3392}#
                                                   #{w\ 3391}#
                                                   #{mod\ 3353}#))
                                               #{val\ 3379}#)
                                          (#{chi-body\ 480}#
                                            (cons #{e1\ 3380}# #{e2\ 3381}#)
                                            (#{source-wrap\ 458}#
                                              #{e\ 3349}#
                                              #{w\ 3391}#
                                              #{s\ 3352}#
                                              #{mod\ 3353}#)
                                            #{r\ 3392}#
                                            #{w\ 3391}#
                                            #{mod\ 3353}#))))))))))
                        #{tmp\ 3360}#)
                      (let ((#{_\ 3398}# #{tmp\ 3359}#))
                        (syntax-violation
                          'letrec
                          "bad letrec"
                          (#{source-wrap\ 458}#
                            #{e\ 3349}#
                            #{w\ 3351}#
                            #{s\ 3352}#
                            #{mod\ 3353}#))))))))
            (#{global-extend\ 383}#
              'core
              'letrec*
              (lambda (#{e\ 3399}#
                       #{r\ 3400}#
                       #{w\ 3401}#
                       #{s\ 3402}#
                       #{mod\ 3403}#)
                (let ((#{tmp\ 3409}# #{e\ 3399}#))
                  (let ((#{tmp\ 3410}#
                          ($sc-dispatch
                            #{tmp\ 3409}#
                            '(any #(each (any any)) any . each-any))))
                    (if (if #{tmp\ 3410}#
                          (@apply
                            (lambda (#{_\ 3416}#
                                     #{id\ 3417}#
                                     #{val\ 3418}#
                                     #{e1\ 3419}#
                                     #{e2\ 3420}#)
                              (and-map #{id?\ 387}# #{id\ 3417}#))
                            #{tmp\ 3410}#)
                          #f)
                      (@apply
                        (lambda (#{_\ 3427}#
                                 #{id\ 3428}#
                                 #{val\ 3429}#
                                 #{e1\ 3430}#
                                 #{e2\ 3431}#)
                          (begin
                            (let ((#{ids\ 3433}# #{id\ 3428}#))
                              (if (not (#{valid-bound-ids?\ 450}#
                                         #{ids\ 3433}#))
                                (syntax-violation
                                  'letrec*
                                  "duplicate bound variable"
                                  #{e\ 3399}#)
                                (begin
                                  (let ((#{labels\ 3437}#
                                          (#{gen-labels\ 405}# #{ids\ 3433}#))
                                        (#{new-vars\ 3438}#
                                          (map #{gen-var\ 500}#
                                               #{ids\ 3433}#)))
                                    (begin
                                      (let ((#{w\ 3441}#
                                              (#{make-binding-wrap\ 434}#
                                                #{ids\ 3433}#
                                                #{labels\ 3437}#
                                                #{w\ 3401}#))
                                            (#{r\ 3442}#
                                              (#{extend-var-env\ 377}#
                                                #{labels\ 3437}#
                                                #{new-vars\ 3438}#
                                                #{r\ 3400}#)))
                                        (#{build-letrec\ 345}#
                                          #{s\ 3402}#
                                          #t
                                          (map syntax->datum #{ids\ 3433}#)
                                          #{new-vars\ 3438}#
                                          (map (lambda (#{x\ 3443}#)
                                                 (#{chi\ 472}#
                                                   #{x\ 3443}#
                                                   #{r\ 3442}#
                                                   #{w\ 3441}#
                                                   #{mod\ 3403}#))
                                               #{val\ 3429}#)
                                          (#{chi-body\ 480}#
                                            (cons #{e1\ 3430}# #{e2\ 3431}#)
                                            (#{source-wrap\ 458}#
                                              #{e\ 3399}#
                                              #{w\ 3441}#
                                              #{s\ 3402}#
                                              #{mod\ 3403}#)
                                            #{r\ 3442}#
                                            #{w\ 3441}#
                                            #{mod\ 3403}#))))))))))
                        #{tmp\ 3410}#)
                      (let ((#{_\ 3448}# #{tmp\ 3409}#))
                        (syntax-violation
                          'letrec*
                          "bad letrec*"
                          (#{source-wrap\ 458}#
                            #{e\ 3399}#
                            #{w\ 3401}#
                            #{s\ 3402}#
                            #{mod\ 3403}#))))))))
            (#{global-extend\ 383}#
              'core
              'set!
              (lambda (#{e\ 3449}#
                       #{r\ 3450}#
                       #{w\ 3451}#
                       #{s\ 3452}#
                       #{mod\ 3453}#)
                (let ((#{tmp\ 3459}# #{e\ 3449}#))
                  (let ((#{tmp\ 3460}#
                          ($sc-dispatch
                            #{tmp\ 3459}#
                            '(any any any))))
                    (if (if #{tmp\ 3460}#
                          (@apply
                            (lambda (#{_\ 3464}# #{id\ 3465}# #{val\ 3466}#)
                              (#{id?\ 387}# #{id\ 3465}#))
                            #{tmp\ 3460}#)
                          #f)
                      (@apply
                        (lambda (#{_\ 3470}# #{id\ 3471}# #{val\ 3472}#)
                          (begin
                            (let ((#{val\ 3475}#
                                    (#{chi\ 472}#
                                      #{val\ 3472}#
                                      #{r\ 3450}#
                                      #{w\ 3451}#
                                      #{mod\ 3453}#))
                                  (#{n\ 3476}#
                                    (#{id-var-name\ 444}#
                                      #{id\ 3471}#
                                      #{w\ 3451}#)))
                              (begin
                                (let ((#{b\ 3478}#
                                        (#{lookup\ 381}#
                                          #{n\ 3476}#
                                          #{r\ 3450}#
                                          #{mod\ 3453}#)))
                                  (begin
                                    (let ((#{atom-key\ 3481}#
                                            (#{binding-type\ 370}#
                                              #{b\ 3478}#)))
                                      (if (eqv? #{atom-key\ 3481}#
                                                'lexical)
                                        (#{build-lexical-assignment\ 317}#
                                          #{s\ 3452}#
                                          (syntax->datum #{id\ 3471}#)
                                          (#{binding-value\ 372}# #{b\ 3478}#)
                                          #{val\ 3475}#)
                                        (if (eqv? #{atom-key\ 3481}#
                                                  'global)
                                          (#{build-global-assignment\ 323}#
                                            #{s\ 3452}#
                                            #{n\ 3476}#
                                            #{val\ 3475}#
                                            #{mod\ 3453}#)
                                          (if (eqv? #{atom-key\ 3481}#
                                                    'displaced-lexical)
                                            (syntax-violation
                                              'set!
                                              "identifier out of context"
                                              (#{wrap\ 456}#
                                                #{id\ 3471}#
                                                #{w\ 3451}#
                                                #{mod\ 3453}#))
                                            (syntax-violation
                                              'set!
                                              "bad set!"
                                              (#{source-wrap\ 458}#
                                                #{e\ 3449}#
                                                #{w\ 3451}#
                                                #{s\ 3452}#
                                                #{mod\ 3453}#))))))))))))
                        #{tmp\ 3460}#)
                      (let ((#{tmp\ 3486}#
                              ($sc-dispatch
                                #{tmp\ 3459}#
                                '(any (any . each-any) any))))
                        (if #{tmp\ 3486}#
                          (@apply
                            (lambda (#{_\ 3491}#
                                     #{head\ 3492}#
                                     #{tail\ 3493}#
                                     #{val\ 3494}#)
                              (call-with-values
                                (lambda ()
                                  (#{syntax-type\ 468}#
                                    #{head\ 3492}#
                                    #{r\ 3450}#
                                    '(())
                                    #f
                                    #f
                                    #{mod\ 3453}#
                                    #t))
                                (lambda (#{type\ 3497}#
                                         #{value\ 3498}#
                                         #{ee\ 3499}#
                                         #{ww\ 3500}#
                                         #{ss\ 3501}#
                                         #{modmod\ 3502}#)
                                  (if (eqv? #{type\ 3497}# (quote module-ref))
                                    (begin
                                      (let ((#{val\ 3511}#
                                              (#{chi\ 472}#
                                                #{val\ 3494}#
                                                #{r\ 3450}#
                                                #{w\ 3451}#
                                                #{mod\ 3453}#)))
                                        (call-with-values
                                          (lambda ()
                                            (#{value\ 3498}#
                                              (cons #{head\ 3492}#
                                                    #{tail\ 3493}#)
                                              #{r\ 3450}#
                                              #{w\ 3451}#))
                                          (lambda (#{e\ 3513}#
                                                   #{r\ 3514}#
                                                   #{w\ 3515}#
                                                   #{s*\ 3516}#
                                                   #{mod\ 3517}#)
                                            (let ((#{tmp\ 3523}# #{e\ 3513}#))
                                              (let ((#{tmp\ 3524}#
                                                      (list #{tmp\ 3523}#)))
                                                (if (if #{tmp\ 3524}#
                                                      (@apply
                                                        (lambda (#{e\ 3526}#)
                                                          (#{id?\ 387}#
                                                            #{e\ 3526}#))
                                                        #{tmp\ 3524}#)
                                                      #f)
                                                  (@apply
                                                    (lambda (#{e\ 3528}#)
                                                      (#{build-global-assignment\ 323}#
                                                        #{s\ 3452}#
                                                        (syntax->datum
                                                          #{e\ 3528}#)
                                                        #{val\ 3511}#
                                                        #{mod\ 3517}#))
                                                    #{tmp\ 3524}#)
                                                  (syntax-violation
                                                    #f
                                                    "source expression failed to match any pattern"
                                                    #{tmp\ 3523}#))))))))
                                    (#{build-application\ 309}#
                                      #{s\ 3452}#
                                      (#{chi\ 472}#
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
                                                    #("i3503"
                                                      "i3504"
                                                      "i3505"
                                                      "i3506"
                                                      "i3507"
                                                      "i3508"))
                                                  #(ribcage
                                                    #(_ head tail val)
                                                    #((top) (top) (top) (top))
                                                    #("i3487"
                                                      "i3488"
                                                      "i3489"
                                                      "i3490"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(e r w s mod)
                                                    #((top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top))
                                                    #("i3454"
                                                      "i3455"
                                                      "i3456"
                                                      "i3457"
                                                      "i3458"))
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
                                                      build-dynlet
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
                                                    ("i501"
                                                     "i499"
                                                     "i497"
                                                     "i495"
                                                     "i493"
                                                     "i491"
                                                     "i489"
                                                     "i487"
                                                     "i485"
                                                     "i483"
                                                     "i481"
                                                     "i479"
                                                     "i477"
                                                     "i475"
                                                     "i473"
                                                     "i471"
                                                     "i469"
                                                     "i467"
                                                     "i465"
                                                     "i463"
                                                     "i461"
                                                     "i459"
                                                     "i457"
                                                     "i455"
                                                     "i453"
                                                     "i451"
                                                     "i449"
                                                     "i447"
                                                     "i445"
                                                     "i443"
                                                     "i441"
                                                     "i439"
                                                     "i437"
                                                     "i435"
                                                     "i433"
                                                     "i431"
                                                     "i430"
                                                     "i429"
                                                     "i427"
                                                     "i426"
                                                     "i425"
                                                     "i424"
                                                     "i423"
                                                     "i421"
                                                     "i419"
                                                     "i417"
                                                     "i415"
                                                     "i413"
                                                     "i411"
                                                     "i409"
                                                     "i407"
                                                     "i404"
                                                     "i402"
                                                     "i401"
                                                     "i400"
                                                     "i399"
                                                     "i398"
                                                     "i397"
                                                     "i395"
                                                     "i393"
                                                     "i391"
                                                     "i389"
                                                     "i388"
                                                     "i386"
                                                     "i384"
                                                     "i382"
                                                     "i380"
                                                     "i378"
                                                     "i376"
                                                     "i374"
                                                     "i373"
                                                     "i371"
                                                     "i369"
                                                     "i368"
                                                     "i367"
                                                     "i365"
                                                     "i364"
                                                     "i362"
                                                     "i360"
                                                     "i358"
                                                     "i356"
                                                     "i354"
                                                     "i352"
                                                     "i350"
                                                     "i348"
                                                     "i346"
                                                     "i344"
                                                     "i342"
                                                     "i340"
                                                     "i338"
                                                     "i336"
                                                     "i334"
                                                     "i332"
                                                     "i330"
                                                     "i328"
                                                     "i326"
                                                     "i324"
                                                     "i322"
                                                     "i320"
                                                     "i318"
                                                     "i316"
                                                     "i314"
                                                     "i312"
                                                     "i310"
                                                     "i308"
                                                     "i306"
                                                     "i304"
                                                     "i302"
                                                     "i300"
                                                     "i299"
                                                     "i297"
                                                     "i295"
                                                     "i293"
                                                     "i291"
                                                     "i289"
                                                     "i287"
                                                     "i285"
                                                     "i283"
                                                     "i281"
                                                     "i278"
                                                     "i276"
                                                     "i274"
                                                     "i272"
                                                     "i270"
                                                     "i268"
                                                     "i266"
                                                     "i264"
                                                     "i262"
                                                     "i260"
                                                     "i258"
                                                     "i256"
                                                     "i254"
                                                     "i252"
                                                     "i250"
                                                     "i248"
                                                     "i246"
                                                     "i244"))
                                                  #(ribcage
                                                    (define-structure
                                                      define-expansion-accessors
                                                      define-expansion-constructors
                                                      and-map*)
                                                    ((top) (top) (top) (top))
                                                    ("i38" "i37" "i36" "i34")))
                                                 (hygiene guile))
                                              #{head\ 3492}#)
                                        #{r\ 3450}#
                                        #{w\ 3451}#
                                        #{mod\ 3453}#)
                                      (map (lambda (#{e\ 3530}#)
                                             (#{chi\ 472}#
                                               #{e\ 3530}#
                                               #{r\ 3450}#
                                               #{w\ 3451}#
                                               #{mod\ 3453}#))
                                           (append
                                             #{tail\ 3493}#
                                             (list #{val\ 3494}#))))))))
                            #{tmp\ 3486}#)
                          (let ((#{_\ 3534}# #{tmp\ 3459}#))
                            (syntax-violation
                              'set!
                              "bad set!"
                              (#{source-wrap\ 458}#
                                #{e\ 3449}#
                                #{w\ 3451}#
                                #{s\ 3452}#
                                #{mod\ 3453}#))))))))))
            (#{global-extend\ 383}#
              'module-ref
              '@
              (lambda (#{e\ 3535}# #{r\ 3536}# #{w\ 3537}#)
                (let ((#{tmp\ 3541}# #{e\ 3535}#))
                  (let ((#{tmp\ 3542}#
                          ($sc-dispatch
                            #{tmp\ 3541}#
                            '(any each-any any))))
                    (if (if #{tmp\ 3542}#
                          (@apply
                            (lambda (#{_\ 3546}# #{mod\ 3547}# #{id\ 3548}#)
                              (if (and-map #{id?\ 387}# #{mod\ 3547}#)
                                (#{id?\ 387}# #{id\ 3548}#)
                                #f))
                            #{tmp\ 3542}#)
                          #f)
                      (@apply
                        (lambda (#{_\ 3555}# #{mod\ 3556}# #{id\ 3557}#)
                          (values
                            (syntax->datum #{id\ 3557}#)
                            #{r\ 3536}#
                            #{w\ 3537}#
                            #f
                            (syntax->datum
                              (cons '#(syntax-object
                                       public
                                       ((top)
                                        #(ribcage
                                          #(_ mod id)
                                          #((top) (top) (top))
                                          #("i3552" "i3553" "i3554"))
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(e r w)
                                          #((top) (top) (top))
                                          #("i3538" "i3539" "i3540"))
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
                                            build-dynlet
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
                                          ("i501"
                                           "i499"
                                           "i497"
                                           "i495"
                                           "i493"
                                           "i491"
                                           "i489"
                                           "i487"
                                           "i485"
                                           "i483"
                                           "i481"
                                           "i479"
                                           "i477"
                                           "i475"
                                           "i473"
                                           "i471"
                                           "i469"
                                           "i467"
                                           "i465"
                                           "i463"
                                           "i461"
                                           "i459"
                                           "i457"
                                           "i455"
                                           "i453"
                                           "i451"
                                           "i449"
                                           "i447"
                                           "i445"
                                           "i443"
                                           "i441"
                                           "i439"
                                           "i437"
                                           "i435"
                                           "i433"
                                           "i431"
                                           "i430"
                                           "i429"
                                           "i427"
                                           "i426"
                                           "i425"
                                           "i424"
                                           "i423"
                                           "i421"
                                           "i419"
                                           "i417"
                                           "i415"
                                           "i413"
                                           "i411"
                                           "i409"
                                           "i407"
                                           "i404"
                                           "i402"
                                           "i401"
                                           "i400"
                                           "i399"
                                           "i398"
                                           "i397"
                                           "i395"
                                           "i393"
                                           "i391"
                                           "i389"
                                           "i388"
                                           "i386"
                                           "i384"
                                           "i382"
                                           "i380"
                                           "i378"
                                           "i376"
                                           "i374"
                                           "i373"
                                           "i371"
                                           "i369"
                                           "i368"
                                           "i367"
                                           "i365"
                                           "i364"
                                           "i362"
                                           "i360"
                                           "i358"
                                           "i356"
                                           "i354"
                                           "i352"
                                           "i350"
                                           "i348"
                                           "i346"
                                           "i344"
                                           "i342"
                                           "i340"
                                           "i338"
                                           "i336"
                                           "i334"
                                           "i332"
                                           "i330"
                                           "i328"
                                           "i326"
                                           "i324"
                                           "i322"
                                           "i320"
                                           "i318"
                                           "i316"
                                           "i314"
                                           "i312"
                                           "i310"
                                           "i308"
                                           "i306"
                                           "i304"
                                           "i302"
                                           "i300"
                                           "i299"
                                           "i297"
                                           "i295"
                                           "i293"
                                           "i291"
                                           "i289"
                                           "i287"
                                           "i285"
                                           "i283"
                                           "i281"
                                           "i278"
                                           "i276"
                                           "i274"
                                           "i272"
                                           "i270"
                                           "i268"
                                           "i266"
                                           "i264"
                                           "i262"
                                           "i260"
                                           "i258"
                                           "i256"
                                           "i254"
                                           "i252"
                                           "i250"
                                           "i248"
                                           "i246"
                                           "i244"))
                                        #(ribcage
                                          (define-structure
                                            define-expansion-accessors
                                            define-expansion-constructors
                                            and-map*)
                                          ((top) (top) (top) (top))
                                          ("i38" "i37" "i36" "i34")))
                                       (hygiene guile))
                                    #{mod\ 3556}#))))
                        #{tmp\ 3542}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp\ 3541}#))))))
            (#{global-extend\ 383}#
              'module-ref
              '@@
              (lambda (#{e\ 3559}# #{r\ 3560}# #{w\ 3561}#)
                (letrec*
                  ((#{remodulate\ 3566}#
                     (lambda (#{x\ 3567}# #{mod\ 3568}#)
                       (if (pair? #{x\ 3567}#)
                         (cons (#{remodulate\ 3566}#
                                 (car #{x\ 3567}#)
                                 #{mod\ 3568}#)
                               (#{remodulate\ 3566}#
                                 (cdr #{x\ 3567}#)
                                 #{mod\ 3568}#))
                         (if (#{syntax-object?\ 351}# #{x\ 3567}#)
                           (#{make-syntax-object\ 349}#
                             (#{remodulate\ 3566}#
                               (#{syntax-object-expression\ 353}# #{x\ 3567}#)
                               #{mod\ 3568}#)
                             (#{syntax-object-wrap\ 355}# #{x\ 3567}#)
                             #{mod\ 3568}#)
                           (if (vector? #{x\ 3567}#)
                             (begin
                               (let ((#{n\ 3579}# (vector-length #{x\ 3567}#)))
                                 (begin
                                   (let ((#{v\ 3581}#
                                           (make-vector #{n\ 3579}#)))
                                     (letrec*
                                       ((#{loop\ 3584}#
                                          (lambda (#{i\ 3585}#)
                                            (if (#{fx=\ 292}#
                                                  #{i\ 3585}#
                                                  #{n\ 3579}#)
                                              (begin (if #f #f) #{v\ 3581}#)
                                              (begin
                                                (vector-set!
                                                  #{v\ 3581}#
                                                  #{i\ 3585}#
                                                  (#{remodulate\ 3566}#
                                                    (vector-ref
                                                      #{x\ 3567}#
                                                      #{i\ 3585}#)
                                                    #{mod\ 3568}#))
                                                (#{loop\ 3584}#
                                                  (#{fx+\ 288}#
                                                    #{i\ 3585}#
                                                    1)))))))
                                       (begin (#{loop\ 3584}# 0)))))))
                             #{x\ 3567}#))))))
                  (begin
                    (let ((#{tmp\ 3589}# #{e\ 3559}#))
                      (let ((#{tmp\ 3590}#
                              ($sc-dispatch
                                #{tmp\ 3589}#
                                '(any each-any any))))
                        (if (if #{tmp\ 3590}#
                              (@apply
                                (lambda (#{_\ 3594}#
                                         #{mod\ 3595}#
                                         #{exp\ 3596}#)
                                  (and-map #{id?\ 387}# #{mod\ 3595}#))
                                #{tmp\ 3590}#)
                              #f)
                          (@apply
                            (lambda (#{_\ 3601}# #{mod\ 3602}# #{exp\ 3603}#)
                              (begin
                                (let ((#{mod\ 3605}#
                                        (syntax->datum
                                          (cons '#(syntax-object
                                                   private
                                                   ((top)
                                                    #(ribcage
                                                      #(_ mod exp)
                                                      #((top) (top) (top))
                                                      #("i3598"
                                                        "i3599"
                                                        "i3600"))
                                                    #(ribcage
                                                      (remodulate)
                                                      ((top))
                                                      ("i3565"))
                                                    #(ribcage
                                                      #(e r w)
                                                      #((top) (top) (top))
                                                      #("i3562"
                                                        "i3563"
                                                        "i3564"))
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
                                                        build-dynlet
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
                                                      ("i501"
                                                       "i499"
                                                       "i497"
                                                       "i495"
                                                       "i493"
                                                       "i491"
                                                       "i489"
                                                       "i487"
                                                       "i485"
                                                       "i483"
                                                       "i481"
                                                       "i479"
                                                       "i477"
                                                       "i475"
                                                       "i473"
                                                       "i471"
                                                       "i469"
                                                       "i467"
                                                       "i465"
                                                       "i463"
                                                       "i461"
                                                       "i459"
                                                       "i457"
                                                       "i455"
                                                       "i453"
                                                       "i451"
                                                       "i449"
                                                       "i447"
                                                       "i445"
                                                       "i443"
                                                       "i441"
                                                       "i439"
                                                       "i437"
                                                       "i435"
                                                       "i433"
                                                       "i431"
                                                       "i430"
                                                       "i429"
                                                       "i427"
                                                       "i426"
                                                       "i425"
                                                       "i424"
                                                       "i423"
                                                       "i421"
                                                       "i419"
                                                       "i417"
                                                       "i415"
                                                       "i413"
                                                       "i411"
                                                       "i409"
                                                       "i407"
                                                       "i404"
                                                       "i402"
                                                       "i401"
                                                       "i400"
                                                       "i399"
                                                       "i398"
                                                       "i397"
                                                       "i395"
                                                       "i393"
                                                       "i391"
                                                       "i389"
                                                       "i388"
                                                       "i386"
                                                       "i384"
                                                       "i382"
                                                       "i380"
                                                       "i378"
                                                       "i376"
                                                       "i374"
                                                       "i373"
                                                       "i371"
                                                       "i369"
                                                       "i368"
                                                       "i367"
                                                       "i365"
                                                       "i364"
                                                       "i362"
                                                       "i360"
                                                       "i358"
                                                       "i356"
                                                       "i354"
                                                       "i352"
                                                       "i350"
                                                       "i348"
                                                       "i346"
                                                       "i344"
                                                       "i342"
                                                       "i340"
                                                       "i338"
                                                       "i336"
                                                       "i334"
                                                       "i332"
                                                       "i330"
                                                       "i328"
                                                       "i326"
                                                       "i324"
                                                       "i322"
                                                       "i320"
                                                       "i318"
                                                       "i316"
                                                       "i314"
                                                       "i312"
                                                       "i310"
                                                       "i308"
                                                       "i306"
                                                       "i304"
                                                       "i302"
                                                       "i300"
                                                       "i299"
                                                       "i297"
                                                       "i295"
                                                       "i293"
                                                       "i291"
                                                       "i289"
                                                       "i287"
                                                       "i285"
                                                       "i283"
                                                       "i281"
                                                       "i278"
                                                       "i276"
                                                       "i274"
                                                       "i272"
                                                       "i270"
                                                       "i268"
                                                       "i266"
                                                       "i264"
                                                       "i262"
                                                       "i260"
                                                       "i258"
                                                       "i256"
                                                       "i254"
                                                       "i252"
                                                       "i250"
                                                       "i248"
                                                       "i246"
                                                       "i244"))
                                                    #(ribcage
                                                      (define-structure
                                                        define-expansion-accessors
                                                        define-expansion-constructors
                                                        and-map*)
                                                      ((top) (top) (top) (top))
                                                      ("i38"
                                                       "i37"
                                                       "i36"
                                                       "i34")))
                                                   (hygiene guile))
                                                #{mod\ 3602}#))))
                                  (values
                                    (#{remodulate\ 3566}#
                                      #{exp\ 3603}#
                                      #{mod\ 3605}#)
                                    #{r\ 3560}#
                                    #{w\ 3561}#
                                    (#{source-annotation\ 366}# #{exp\ 3603}#)
                                    #{mod\ 3605}#))))
                            #{tmp\ 3590}#)
                          (syntax-violation
                            #f
                            "source expression failed to match any pattern"
                            #{tmp\ 3589}#))))))))
            (#{global-extend\ 383}#
              'core
              'if
              (lambda (#{e\ 3607}#
                       #{r\ 3608}#
                       #{w\ 3609}#
                       #{s\ 3610}#
                       #{mod\ 3611}#)
                (let ((#{tmp\ 3617}# #{e\ 3607}#))
                  (let ((#{tmp\ 3618}#
                          ($sc-dispatch
                            #{tmp\ 3617}#
                            '(any any any))))
                    (if #{tmp\ 3618}#
                      (@apply
                        (lambda (#{_\ 3622}# #{test\ 3623}# #{then\ 3624}#)
                          (#{build-conditional\ 311}#
                            #{s\ 3610}#
                            (#{chi\ 472}#
                              #{test\ 3623}#
                              #{r\ 3608}#
                              #{w\ 3609}#
                              #{mod\ 3611}#)
                            (#{chi\ 472}#
                              #{then\ 3624}#
                              #{r\ 3608}#
                              #{w\ 3609}#
                              #{mod\ 3611}#)
                            (#{build-void\ 307}# #f)))
                        #{tmp\ 3618}#)
                      (let ((#{tmp\ 3626}#
                              ($sc-dispatch
                                #{tmp\ 3617}#
                                '(any any any any))))
                        (if #{tmp\ 3626}#
                          (@apply
                            (lambda (#{_\ 3631}#
                                     #{test\ 3632}#
                                     #{then\ 3633}#
                                     #{else\ 3634}#)
                              (#{build-conditional\ 311}#
                                #{s\ 3610}#
                                (#{chi\ 472}#
                                  #{test\ 3632}#
                                  #{r\ 3608}#
                                  #{w\ 3609}#
                                  #{mod\ 3611}#)
                                (#{chi\ 472}#
                                  #{then\ 3633}#
                                  #{r\ 3608}#
                                  #{w\ 3609}#
                                  #{mod\ 3611}#)
                                (#{chi\ 472}#
                                  #{else\ 3634}#
                                  #{r\ 3608}#
                                  #{w\ 3609}#
                                  #{mod\ 3611}#)))
                            #{tmp\ 3626}#)
                          (syntax-violation
                            #f
                            "source expression failed to match any pattern"
                            #{tmp\ 3617}#))))))))
            (#{global-extend\ 383}#
              'core
              'with-fluids
              (lambda (#{e\ 3635}#
                       #{r\ 3636}#
                       #{w\ 3637}#
                       #{s\ 3638}#
                       #{mod\ 3639}#)
                (let ((#{tmp\ 3645}# #{e\ 3635}#))
                  (let ((#{tmp\ 3646}#
                          ($sc-dispatch
                            #{tmp\ 3645}#
                            '(any #(each (any any)) any . each-any))))
                    (if #{tmp\ 3646}#
                      (@apply
                        (lambda (#{_\ 3652}#
                                 #{fluid\ 3653}#
                                 #{val\ 3654}#
                                 #{b\ 3655}#
                                 #{b*\ 3656}#)
                          (#{build-dynlet\ 313}#
                            #{s\ 3638}#
                            (map (lambda (#{x\ 3657}#)
                                   (#{chi\ 472}#
                                     #{x\ 3657}#
                                     #{r\ 3636}#
                                     #{w\ 3637}#
                                     #{mod\ 3639}#))
                                 #{fluid\ 3653}#)
                            (map (lambda (#{x\ 3660}#)
                                   (#{chi\ 472}#
                                     #{x\ 3660}#
                                     #{r\ 3636}#
                                     #{w\ 3637}#
                                     #{mod\ 3639}#))
                                 #{val\ 3654}#)
                            (#{chi-body\ 480}#
                              (cons #{b\ 3655}# #{b*\ 3656}#)
                              (#{source-wrap\ 458}#
                                #{e\ 3635}#
                                #{w\ 3637}#
                                #{s\ 3638}#
                                #{mod\ 3639}#)
                              #{r\ 3636}#
                              #{w\ 3637}#
                              #{mod\ 3639}#)))
                        #{tmp\ 3646}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp\ 3645}#))))))
            (#{global-extend\ 383}#
              'begin
              'begin
              '())
            (#{global-extend\ 383}#
              'define
              'define
              '())
            (#{global-extend\ 383}#
              'define-syntax
              'define-syntax
              '())
            (#{global-extend\ 383}#
              'eval-when
              'eval-when
              '())
            (#{global-extend\ 383}#
              'core
              'syntax-case
              (letrec*
                ((#{convert-pattern\ 3665}#
                   (lambda (#{pattern\ 3672}# #{keys\ 3673}#)
                     (letrec*
                       ((#{cvt*\ 3677}#
                          (lambda (#{p*\ 3680}# #{n\ 3681}# #{ids\ 3682}#)
                            (if (null? #{p*\ 3680}#)
                              (values (quote ()) #{ids\ 3682}#)
                              (call-with-values
                                (lambda ()
                                  (#{cvt*\ 3677}#
                                    (cdr #{p*\ 3680}#)
                                    #{n\ 3681}#
                                    #{ids\ 3682}#))
                                (lambda (#{y\ 3686}# #{ids\ 3687}#)
                                  (call-with-values
                                    (lambda ()
                                      (#{cvt\ 3679}#
                                        (car #{p*\ 3680}#)
                                        #{n\ 3681}#
                                        #{ids\ 3687}#))
                                    (lambda (#{x\ 3690}# #{ids\ 3691}#)
                                      (values
                                        (cons #{x\ 3690}# #{y\ 3686}#)
                                        #{ids\ 3691}#))))))))
                        (#{cvt\ 3679}#
                          (lambda (#{p\ 3694}# #{n\ 3695}# #{ids\ 3696}#)
                            (if (#{id?\ 387}# #{p\ 3694}#)
                              (if (#{bound-id-member?\ 454}#
                                    #{p\ 3694}#
                                    #{keys\ 3673}#)
                                (values
                                  (vector (quote free-id) #{p\ 3694}#)
                                  #{ids\ 3696}#)
                                (values
                                  'any
                                  (cons (cons #{p\ 3694}# #{n\ 3695}#)
                                        #{ids\ 3696}#)))
                              (let ((#{tmp\ 3700}# #{p\ 3694}#))
                                (let ((#{tmp\ 3701}#
                                        ($sc-dispatch
                                          #{tmp\ 3700}#
                                          '(any any))))
                                  (if (if #{tmp\ 3701}#
                                        (@apply
                                          (lambda (#{x\ 3704}# #{dots\ 3705}#)
                                            (#{ellipsis?\ 488}#
                                              #{dots\ 3705}#))
                                          #{tmp\ 3701}#)
                                        #f)
                                    (@apply
                                      (lambda (#{x\ 3708}# #{dots\ 3709}#)
                                        (call-with-values
                                          (lambda ()
                                            (#{cvt\ 3679}#
                                              #{x\ 3708}#
                                              (#{fx+\ 288}# #{n\ 3695}# 1)
                                              #{ids\ 3696}#))
                                          (lambda (#{p\ 3710}# #{ids\ 3711}#)
                                            (values
                                              (if (eq? #{p\ 3710}# (quote any))
                                                'each-any
                                                (vector
                                                  'each
                                                  #{p\ 3710}#))
                                              #{ids\ 3711}#))))
                                      #{tmp\ 3701}#)
                                    (let ((#{tmp\ 3714}#
                                            ($sc-dispatch
                                              #{tmp\ 3700}#
                                              '(any any . each-any))))
                                      (if (if #{tmp\ 3714}#
                                            (@apply
                                              (lambda (#{x\ 3718}#
                                                       #{dots\ 3719}#
                                                       #{ys\ 3720}#)
                                                (#{ellipsis?\ 488}#
                                                  #{dots\ 3719}#))
                                              #{tmp\ 3714}#)
                                            #f)
                                        (@apply
                                          (lambda (#{x\ 3724}#
                                                   #{dots\ 3725}#
                                                   #{ys\ 3726}#)
                                            (call-with-values
                                              (lambda ()
                                                (#{cvt*\ 3677}#
                                                  #{ys\ 3726}#
                                                  #{n\ 3695}#
                                                  #{ids\ 3696}#))
                                              (lambda (#{ys\ 3728}#
                                                       #{ids\ 3729}#)
                                                (call-with-values
                                                  (lambda ()
                                                    (#{cvt\ 3679}#
                                                      #{x\ 3724}#
                                                      (1+ #{n\ 3695}#)
                                                      #{ids\ 3729}#))
                                                  (lambda (#{x\ 3732}#
                                                           #{ids\ 3733}#)
                                                    (values
                                                      (list->vector
                                                        (cons 'each+
                                                              (cons #{x\ 3732}#
                                                                    (cons (reverse
                                                                            #{ys\ 3728}#)
                                                                          '(())))))
                                                      #{ids\ 3733}#))))))
                                          #{tmp\ 3714}#)
                                        (let ((#{tmp\ 3737}#
                                                ($sc-dispatch
                                                  #{tmp\ 3700}#
                                                  '(any . any))))
                                          (if #{tmp\ 3737}#
                                            (@apply
                                              (lambda (#{x\ 3740}# #{y\ 3741}#)
                                                (call-with-values
                                                  (lambda ()
                                                    (#{cvt\ 3679}#
                                                      #{y\ 3741}#
                                                      #{n\ 3695}#
                                                      #{ids\ 3696}#))
                                                  (lambda (#{y\ 3742}#
                                                           #{ids\ 3743}#)
                                                    (call-with-values
                                                      (lambda ()
                                                        (#{cvt\ 3679}#
                                                          #{x\ 3740}#
                                                          #{n\ 3695}#
                                                          #{ids\ 3743}#))
                                                      (lambda (#{x\ 3746}#
                                                               #{ids\ 3747}#)
                                                        (values
                                                          (cons #{x\ 3746}#
                                                                #{y\ 3742}#)
                                                          #{ids\ 3747}#))))))
                                              #{tmp\ 3737}#)
                                            (let ((#{tmp\ 3750}#
                                                    ($sc-dispatch
                                                      #{tmp\ 3700}#
                                                      '())))
                                              (if #{tmp\ 3750}#
                                                (@apply
                                                  (lambda ()
                                                    (values
                                                      '()
                                                      #{ids\ 3696}#))
                                                  #{tmp\ 3750}#)
                                                (let ((#{tmp\ 3751}#
                                                        ($sc-dispatch
                                                          #{tmp\ 3700}#
                                                          '#(vector
                                                             each-any))))
                                                  (if #{tmp\ 3751}#
                                                    (@apply
                                                      (lambda (#{x\ 3753}#)
                                                        (call-with-values
                                                          (lambda ()
                                                            (#{cvt\ 3679}#
                                                              #{x\ 3753}#
                                                              #{n\ 3695}#
                                                              #{ids\ 3696}#))
                                                          (lambda (#{p\ 3755}#
                                                                   #{ids\ 3756}#)
                                                            (values
                                                              (vector
                                                                'vector
                                                                #{p\ 3755}#)
                                                              #{ids\ 3756}#))))
                                                      #{tmp\ 3751}#)
                                                    (let ((#{x\ 3760}#
                                                            #{tmp\ 3700}#))
                                                      (values
                                                        (vector
                                                          'atom
                                                          (#{strip\ 498}#
                                                            #{p\ 3694}#
                                                            '(())))
                                                        #{ids\ 3696}#)))))))))))))))))
                       (begin
                         (#{cvt\ 3679}# #{pattern\ 3672}# 0 (quote ()))))))
                 (#{build-dispatch-call\ 3667}#
                   (lambda (#{pvars\ 3762}#
                            #{exp\ 3763}#
                            #{y\ 3764}#
                            #{r\ 3765}#
                            #{mod\ 3766}#)
                     (begin
                       (map cdr #{pvars\ 3762}#)
                       (let ((#{ids\ 3774}# (map car #{pvars\ 3762}#)))
                         (begin
                           (let ((#{labels\ 3778}#
                                   (#{gen-labels\ 405}# #{ids\ 3774}#))
                                 (#{new-vars\ 3779}#
                                   (map #{gen-var\ 500}# #{ids\ 3774}#)))
                             (#{build-application\ 309}#
                               #f
                               (#{build-primref\ 335}# #f (quote apply))
                               (list (#{build-simple-lambda\ 329}#
                                       #f
                                       (map syntax->datum #{ids\ 3774}#)
                                       #f
                                       #{new-vars\ 3779}#
                                       '()
                                       (#{chi\ 472}#
                                         #{exp\ 3763}#
                                         (#{extend-env\ 375}#
                                           #{labels\ 3778}#
                                           (map (lambda (#{var\ 3783}#
                                                         #{level\ 3784}#)
                                                  (cons 'syntax
                                                        (cons #{var\ 3783}#
                                                              #{level\ 3784}#)))
                                                #{new-vars\ 3779}#
                                                (map cdr #{pvars\ 3762}#))
                                           #{r\ 3765}#)
                                         (#{make-binding-wrap\ 434}#
                                           #{ids\ 3774}#
                                           #{labels\ 3778}#
                                           '(()))
                                         #{mod\ 3766}#))
                                     #{y\ 3764}#))))))))
                 (#{gen-clause\ 3669}#
                   (lambda (#{x\ 3790}#
                            #{keys\ 3791}#
                            #{clauses\ 3792}#
                            #{r\ 3793}#
                            #{pat\ 3794}#
                            #{fender\ 3795}#
                            #{exp\ 3796}#
                            #{mod\ 3797}#)
                     (call-with-values
                       (lambda ()
                         (#{convert-pattern\ 3665}#
                           #{pat\ 3794}#
                           #{keys\ 3791}#))
                       (lambda (#{p\ 3806}# #{pvars\ 3807}#)
                         (if (not (#{distinct-bound-ids?\ 452}#
                                    (map car #{pvars\ 3807}#)))
                           (syntax-violation
                             'syntax-case
                             "duplicate pattern variable"
                             #{pat\ 3794}#)
                           (if (not (and-map
                                      (lambda (#{x\ 3814}#)
                                        (not (#{ellipsis?\ 488}#
                                               (car #{x\ 3814}#))))
                                      #{pvars\ 3807}#))
                             (syntax-violation
                               'syntax-case
                               "misplaced ellipsis"
                               #{pat\ 3794}#)
                             (begin
                               (let ((#{y\ 3818}#
                                       (#{gen-var\ 500}# (quote tmp))))
                                 (#{build-application\ 309}#
                                   #f
                                   (#{build-simple-lambda\ 329}#
                                     #f
                                     (list (quote tmp))
                                     #f
                                     (list #{y\ 3818}#)
                                     '()
                                     (begin
                                       (let ((#{y\ 3822}#
                                               (#{build-lexical-reference\ 315}#
                                                 'value
                                                 #f
                                                 'tmp
                                                 #{y\ 3818}#)))
                                         (#{build-conditional\ 311}#
                                           #f
                                           (let ((#{tmp\ 3825}#
                                                   #{fender\ 3795}#))
                                             (let ((#{tmp\ 3826}#
                                                     ($sc-dispatch
                                                       #{tmp\ 3825}#
                                                       '#(atom #t))))
                                               (if #{tmp\ 3826}#
                                                 (@apply
                                                   (lambda () #{y\ 3822}#)
                                                   #{tmp\ 3826}#)
                                                 (let ((#{_\ 3828}#
                                                         #{tmp\ 3825}#))
                                                   (#{build-conditional\ 311}#
                                                     #f
                                                     #{y\ 3822}#
                                                     (#{build-dispatch-call\ 3667}#
                                                       #{pvars\ 3807}#
                                                       #{fender\ 3795}#
                                                       #{y\ 3822}#
                                                       #{r\ 3793}#
                                                       #{mod\ 3797}#)
                                                     (#{build-data\ 337}#
                                                       #f
                                                       #f))))))
                                           (#{build-dispatch-call\ 3667}#
                                             #{pvars\ 3807}#
                                             #{exp\ 3796}#
                                             #{y\ 3822}#
                                             #{r\ 3793}#
                                             #{mod\ 3797}#)
                                           (#{gen-syntax-case\ 3671}#
                                             #{x\ 3790}#
                                             #{keys\ 3791}#
                                             #{clauses\ 3792}#
                                             #{r\ 3793}#
                                             #{mod\ 3797}#)))))
                                   (list (if (eq? #{p\ 3806}# (quote any))
                                           (#{build-application\ 309}#
                                             #f
                                             (#{build-primref\ 335}#
                                               #f
                                               'list)
                                             (list #{x\ 3790}#))
                                           (#{build-application\ 309}#
                                             #f
                                             (#{build-primref\ 335}#
                                               #f
                                               '$sc-dispatch)
                                             (list #{x\ 3790}#
                                                   (#{build-data\ 337}#
                                                     #f
                                                     #{p\ 3806}#))))))))))))))
                 (#{gen-syntax-case\ 3671}#
                   (lambda (#{x\ 3836}#
                            #{keys\ 3837}#
                            #{clauses\ 3838}#
                            #{r\ 3839}#
                            #{mod\ 3840}#)
                     (if (null? #{clauses\ 3838}#)
                       (#{build-application\ 309}#
                         #f
                         (#{build-primref\ 335}#
                           #f
                           'syntax-violation)
                         (list (#{build-data\ 337}# #f #f)
                               (#{build-data\ 337}#
                                 #f
                                 "source expression failed to match any pattern")
                               #{x\ 3836}#))
                       (let ((#{tmp\ 3850}# (car #{clauses\ 3838}#)))
                         (let ((#{tmp\ 3851}#
                                 ($sc-dispatch
                                   #{tmp\ 3850}#
                                   '(any any))))
                           (if #{tmp\ 3851}#
                             (@apply
                               (lambda (#{pat\ 3854}# #{exp\ 3855}#)
                                 (if (if (#{id?\ 387}# #{pat\ 3854}#)
                                       (and-map
                                         (lambda (#{x\ 3858}#)
                                           (not (#{free-id=?\ 446}#
                                                  #{pat\ 3854}#
                                                  #{x\ 3858}#)))
                                         (cons '#(syntax-object
                                                  ...
                                                  ((top)
                                                   #(ribcage
                                                     #(pat exp)
                                                     #((top) (top))
                                                     #("i3852" "i3853"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x keys clauses r mod)
                                                     #((top)
                                                       (top)
                                                       (top)
                                                       (top)
                                                       (top))
                                                     #("i3841"
                                                       "i3842"
                                                       "i3843"
                                                       "i3844"
                                                       "i3845"))
                                                   #(ribcage
                                                     (gen-syntax-case
                                                       gen-clause
                                                       build-dispatch-call
                                                       convert-pattern)
                                                     ((top) (top) (top) (top))
                                                     ("i3670"
                                                      "i3668"
                                                      "i3666"
                                                      "i3664"))
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
                                                       build-dynlet
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
                                                     ("i501"
                                                      "i499"
                                                      "i497"
                                                      "i495"
                                                      "i493"
                                                      "i491"
                                                      "i489"
                                                      "i487"
                                                      "i485"
                                                      "i483"
                                                      "i481"
                                                      "i479"
                                                      "i477"
                                                      "i475"
                                                      "i473"
                                                      "i471"
                                                      "i469"
                                                      "i467"
                                                      "i465"
                                                      "i463"
                                                      "i461"
                                                      "i459"
                                                      "i457"
                                                      "i455"
                                                      "i453"
                                                      "i451"
                                                      "i449"
                                                      "i447"
                                                      "i445"
                                                      "i443"
                                                      "i441"
                                                      "i439"
                                                      "i437"
                                                      "i435"
                                                      "i433"
                                                      "i431"
                                                      "i430"
                                                      "i429"
                                                      "i427"
                                                      "i426"
                                                      "i425"
                                                      "i424"
                                                      "i423"
                                                      "i421"
                                                      "i419"
                                                      "i417"
                                                      "i415"
                                                      "i413"
                                                      "i411"
                                                      "i409"
                                                      "i407"
                                                      "i404"
                                                      "i402"
                                                      "i401"
                                                      "i400"
                                                      "i399"
                                                      "i398"
                                                      "i397"
                                                      "i395"
                                                      "i393"
                                                      "i391"
                                                      "i389"
                                                      "i388"
                                                      "i386"
                                                      "i384"
                                                      "i382"
                                                      "i380"
                                                      "i378"
                                                      "i376"
                                                      "i374"
                                                      "i373"
                                                      "i371"
                                                      "i369"
                                                      "i368"
                                                      "i367"
                                                      "i365"
                                                      "i364"
                                                      "i362"
                                                      "i360"
                                                      "i358"
                                                      "i356"
                                                      "i354"
                                                      "i352"
                                                      "i350"
                                                      "i348"
                                                      "i346"
                                                      "i344"
                                                      "i342"
                                                      "i340"
                                                      "i338"
                                                      "i336"
                                                      "i334"
                                                      "i332"
                                                      "i330"
                                                      "i328"
                                                      "i326"
                                                      "i324"
                                                      "i322"
                                                      "i320"
                                                      "i318"
                                                      "i316"
                                                      "i314"
                                                      "i312"
                                                      "i310"
                                                      "i308"
                                                      "i306"
                                                      "i304"
                                                      "i302"
                                                      "i300"
                                                      "i299"
                                                      "i297"
                                                      "i295"
                                                      "i293"
                                                      "i291"
                                                      "i289"
                                                      "i287"
                                                      "i285"
                                                      "i283"
                                                      "i281"
                                                      "i278"
                                                      "i276"
                                                      "i274"
                                                      "i272"
                                                      "i270"
                                                      "i268"
                                                      "i266"
                                                      "i264"
                                                      "i262"
                                                      "i260"
                                                      "i258"
                                                      "i256"
                                                      "i254"
                                                      "i252"
                                                      "i250"
                                                      "i248"
                                                      "i246"
                                                      "i244"))
                                                   #(ribcage
                                                     (define-structure
                                                       define-expansion-accessors
                                                       define-expansion-constructors
                                                       and-map*)
                                                     ((top) (top) (top) (top))
                                                     ("i38"
                                                      "i37"
                                                      "i36"
                                                      "i34")))
                                                  (hygiene guile))
                                               #{keys\ 3837}#))
                                       #f)
                                   (begin
                                     (let ((#{labels\ 3862}#
                                             (list (#{gen-label\ 403}#)))
                                           (#{var\ 3863}#
                                             (#{gen-var\ 500}# #{pat\ 3854}#)))
                                       (#{build-application\ 309}#
                                         #f
                                         (#{build-simple-lambda\ 329}#
                                           #f
                                           (list (syntax->datum #{pat\ 3854}#))
                                           #f
                                           (list #{var\ 3863}#)
                                           '()
                                           (#{chi\ 472}#
                                             #{exp\ 3855}#
                                             (#{extend-env\ 375}#
                                               #{labels\ 3862}#
                                               (list (cons 'syntax
                                                           (cons #{var\ 3863}#
                                                                 0)))
                                               #{r\ 3839}#)
                                             (#{make-binding-wrap\ 434}#
                                               (list #{pat\ 3854}#)
                                               #{labels\ 3862}#
                                               '(()))
                                             #{mod\ 3840}#))
                                         (list #{x\ 3836}#))))
                                   (#{gen-clause\ 3669}#
                                     #{x\ 3836}#
                                     #{keys\ 3837}#
                                     (cdr #{clauses\ 3838}#)
                                     #{r\ 3839}#
                                     #{pat\ 3854}#
                                     #t
                                     #{exp\ 3855}#
                                     #{mod\ 3840}#)))
                               #{tmp\ 3851}#)
                             (let ((#{tmp\ 3869}#
                                     ($sc-dispatch
                                       #{tmp\ 3850}#
                                       '(any any any))))
                               (if #{tmp\ 3869}#
                                 (@apply
                                   (lambda (#{pat\ 3873}#
                                            #{fender\ 3874}#
                                            #{exp\ 3875}#)
                                     (#{gen-clause\ 3669}#
                                       #{x\ 3836}#
                                       #{keys\ 3837}#
                                       (cdr #{clauses\ 3838}#)
                                       #{r\ 3839}#
                                       #{pat\ 3873}#
                                       #{fender\ 3874}#
                                       #{exp\ 3875}#
                                       #{mod\ 3840}#))
                                   #{tmp\ 3869}#)
                                 (let ((#{_\ 3877}# #{tmp\ 3850}#))
                                   (syntax-violation
                                     'syntax-case
                                     "invalid clause"
                                     (car #{clauses\ 3838}#))))))))))))
                (begin
                  (lambda (#{e\ 3878}#
                           #{r\ 3879}#
                           #{w\ 3880}#
                           #{s\ 3881}#
                           #{mod\ 3882}#)
                    (begin
                      (let ((#{e\ 3889}#
                              (#{source-wrap\ 458}#
                                #{e\ 3878}#
                                #{w\ 3880}#
                                #{s\ 3881}#
                                #{mod\ 3882}#)))
                        (let ((#{tmp\ 3890}# #{e\ 3889}#))
                          (let ((#{tmp\ 3891}#
                                  ($sc-dispatch
                                    #{tmp\ 3890}#
                                    '(any any each-any . each-any))))
                            (if #{tmp\ 3891}#
                              (@apply
                                (lambda (#{_\ 3896}#
                                         #{val\ 3897}#
                                         #{key\ 3898}#
                                         #{m\ 3899}#)
                                  (if (and-map
                                        (lambda (#{x\ 3900}#)
                                          (if (#{id?\ 387}# #{x\ 3900}#)
                                            (not (#{ellipsis?\ 488}#
                                                   #{x\ 3900}#))
                                            #f))
                                        #{key\ 3898}#)
                                    (begin
                                      (let ((#{x\ 3906}#
                                              (#{gen-var\ 500}# (quote tmp))))
                                        (#{build-application\ 309}#
                                          #{s\ 3881}#
                                          (#{build-simple-lambda\ 329}#
                                            #f
                                            (list (quote tmp))
                                            #f
                                            (list #{x\ 3906}#)
                                            '()
                                            (#{gen-syntax-case\ 3671}#
                                              (#{build-lexical-reference\ 315}#
                                                'value
                                                #f
                                                'tmp
                                                #{x\ 3906}#)
                                              #{key\ 3898}#
                                              #{m\ 3899}#
                                              #{r\ 3879}#
                                              #{mod\ 3882}#))
                                          (list (#{chi\ 472}#
                                                  #{val\ 3897}#
                                                  #{r\ 3879}#
                                                  '(())
                                                  #{mod\ 3882}#)))))
                                    (syntax-violation
                                      'syntax-case
                                      "invalid literals list"
                                      #{e\ 3889}#)))
                                #{tmp\ 3891}#)
                              (syntax-violation
                                #f
                                "source expression failed to match any pattern"
                                #{tmp\ 3890}#))))))))))
            (set! macroexpand
              (lambda*
                (#{x\ 3912}#
                  #:optional
                  (#{m\ 3914}# (quote e))
                  (#{esew\ 3916}# (quote (eval))))
                (#{chi-top\ 470}#
                  #{x\ 3912}#
                  '()
                  '((top))
                  #{m\ 3914}#
                  #{esew\ 3916}#
                  (cons 'hygiene
                        (module-name (current-module))))))
            (set! identifier?
              (lambda (#{x\ 3920}#)
                (#{nonsymbol-id?\ 385}# #{x\ 3920}#)))
            (set! datum->syntax
              (lambda (#{id\ 3922}# #{datum\ 3923}#)
                (#{make-syntax-object\ 349}#
                  #{datum\ 3923}#
                  (#{syntax-object-wrap\ 355}# #{id\ 3922}#)
                  (#{syntax-object-module\ 357}# #{id\ 3922}#))))
            (set! syntax->datum
              (lambda (#{x\ 3926}#)
                (#{strip\ 498}# #{x\ 3926}# (quote (())))))
            (set! syntax-source
              (lambda (#{x\ 3929}#)
                (#{source-annotation\ 366}# #{x\ 3929}#)))
            (set! generate-temporaries
              (lambda (#{ls\ 3931}#)
                (begin
                  (begin
                    (let ((#{x\ 3935}# #{ls\ 3931}#))
                      (if (not (list? #{x\ 3935}#))
                        (syntax-violation
                          'generate-temporaries
                          "invalid argument"
                          #{x\ 3935}#))))
                  (map (lambda (#{x\ 3936}#)
                         (#{wrap\ 456}# (gensym) (quote ((top))) #f))
                       #{ls\ 3931}#))))
            (set! free-identifier=?
              (lambda (#{x\ 3940}# #{y\ 3941}#)
                (begin
                  (begin
                    (let ((#{x\ 3946}# #{x\ 3940}#))
                      (if (not (#{nonsymbol-id?\ 385}# #{x\ 3946}#))
                        (syntax-violation
                          'free-identifier=?
                          "invalid argument"
                          #{x\ 3946}#))))
                  (begin
                    (let ((#{x\ 3949}# #{y\ 3941}#))
                      (if (not (#{nonsymbol-id?\ 385}# #{x\ 3949}#))
                        (syntax-violation
                          'free-identifier=?
                          "invalid argument"
                          #{x\ 3949}#))))
                  (#{free-id=?\ 446}# #{x\ 3940}# #{y\ 3941}#))))
            (set! bound-identifier=?
              (lambda (#{x\ 3950}# #{y\ 3951}#)
                (begin
                  (begin
                    (let ((#{x\ 3956}# #{x\ 3950}#))
                      (if (not (#{nonsymbol-id?\ 385}# #{x\ 3956}#))
                        (syntax-violation
                          'bound-identifier=?
                          "invalid argument"
                          #{x\ 3956}#))))
                  (begin
                    (let ((#{x\ 3959}# #{y\ 3951}#))
                      (if (not (#{nonsymbol-id?\ 385}# #{x\ 3959}#))
                        (syntax-violation
                          'bound-identifier=?
                          "invalid argument"
                          #{x\ 3959}#))))
                  (#{bound-id=?\ 448}# #{x\ 3950}# #{y\ 3951}#))))
            (set! syntax-violation
              (lambda (#{who\ 3960}#
                       #{message\ 3961}#
                       #{form\ 3962}#
                       .
                       #{subform\ 3963}#)
                (begin
                  (begin
                    (let ((#{x\ 3970}# #{who\ 3960}#))
                      (if (not (let ((#{x\ 3971}# #{x\ 3970}#))
                                 (begin
                                   (let ((#{t\ 3975}# (not #{x\ 3971}#)))
                                     (if #{t\ 3975}#
                                       #{t\ 3975}#
                                       (begin
                                         (let ((#{t\ 3978}#
                                                 (string? #{x\ 3971}#)))
                                           (if #{t\ 3978}#
                                             #{t\ 3978}#
                                             (symbol? #{x\ 3971}#)))))))))
                        (syntax-violation
                          'syntax-violation
                          "invalid argument"
                          #{x\ 3970}#))))
                  (begin
                    (let ((#{x\ 3982}# #{message\ 3961}#))
                      (if (not (string? #{x\ 3982}#))
                        (syntax-violation
                          'syntax-violation
                          "invalid argument"
                          #{x\ 3982}#))))
                  (scm-error
                    'syntax-error
                    'macroexpand
                    (string-append
                      (if #{who\ 3960}# "~a: " "")
                      "~a "
                      (if (null? #{subform\ 3963}#)
                        "in ~a"
                        "in subform `~s' of `~s'"))
                    (begin
                      (let ((#{tail\ 3984}#
                              (cons #{message\ 3961}#
                                    (map (lambda (#{x\ 3985}#)
                                           (#{strip\ 498}#
                                             #{x\ 3985}#
                                             '(())))
                                         (append
                                           #{subform\ 3963}#
                                           (list #{form\ 3962}#))))))
                        (if #{who\ 3960}#
                          (cons #{who\ 3960}# #{tail\ 3984}#)
                          #{tail\ 3984}#)))
                    #f))))
            (letrec*
              ((#{match-each\ 3989}#
                 (lambda (#{e\ 4002}#
                          #{p\ 4003}#
                          #{w\ 4004}#
                          #{mod\ 4005}#)
                   (if (pair? #{e\ 4002}#)
                     (begin
                       (let ((#{first\ 4013}#
                               (#{match\ 4001}#
                                 (car #{e\ 4002}#)
                                 #{p\ 4003}#
                                 #{w\ 4004}#
                                 '()
                                 #{mod\ 4005}#)))
                         (if #{first\ 4013}#
                           (begin
                             (let ((#{rest\ 4017}#
                                     (#{match-each\ 3989}#
                                       (cdr #{e\ 4002}#)
                                       #{p\ 4003}#
                                       #{w\ 4004}#
                                       #{mod\ 4005}#)))
                               (if #{rest\ 4017}#
                                 (cons #{first\ 4013}# #{rest\ 4017}#)
                                 #f)))
                           #f)))
                     (if (null? #{e\ 4002}#)
                       '()
                       (if (#{syntax-object?\ 351}# #{e\ 4002}#)
                         (#{match-each\ 3989}#
                           (#{syntax-object-expression\ 353}# #{e\ 4002}#)
                           #{p\ 4003}#
                           (#{join-wraps\ 438}#
                             #{w\ 4004}#
                             (#{syntax-object-wrap\ 355}# #{e\ 4002}#))
                           (#{syntax-object-module\ 357}# #{e\ 4002}#))
                         #f)))))
               (#{match-each+\ 3991}#
                 (lambda (#{e\ 4025}#
                          #{x-pat\ 4026}#
                          #{y-pat\ 4027}#
                          #{z-pat\ 4028}#
                          #{w\ 4029}#
                          #{r\ 4030}#
                          #{mod\ 4031}#)
                   (letrec*
                     ((#{f\ 4042}#
                        (lambda (#{e\ 4043}# #{w\ 4044}#)
                          (if (pair? #{e\ 4043}#)
                            (call-with-values
                              (lambda ()
                                (#{f\ 4042}# (cdr #{e\ 4043}#) #{w\ 4044}#))
                              (lambda (#{xr*\ 4047}#
                                       #{y-pat\ 4048}#
                                       #{r\ 4049}#)
                                (if #{r\ 4049}#
                                  (if (null? #{y-pat\ 4048}#)
                                    (begin
                                      (let ((#{xr\ 4054}#
                                              (#{match\ 4001}#
                                                (car #{e\ 4043}#)
                                                #{x-pat\ 4026}#
                                                #{w\ 4044}#
                                                '()
                                                #{mod\ 4031}#)))
                                        (if #{xr\ 4054}#
                                          (values
                                            (cons #{xr\ 4054}# #{xr*\ 4047}#)
                                            #{y-pat\ 4048}#
                                            #{r\ 4049}#)
                                          (values #f #f #f))))
                                    (values
                                      '()
                                      (cdr #{y-pat\ 4048}#)
                                      (#{match\ 4001}#
                                        (car #{e\ 4043}#)
                                        (car #{y-pat\ 4048}#)
                                        #{w\ 4044}#
                                        #{r\ 4049}#
                                        #{mod\ 4031}#)))
                                  (values #f #f #f))))
                            (if (#{syntax-object?\ 351}# #{e\ 4043}#)
                              (#{f\ 4042}#
                                (#{syntax-object-expression\ 353}# #{e\ 4043}#)
                                (#{join-wraps\ 438}# #{w\ 4044}# #{e\ 4043}#))
                              (values
                                '()
                                #{y-pat\ 4027}#
                                (#{match\ 4001}#
                                  #{e\ 4043}#
                                  #{z-pat\ 4028}#
                                  #{w\ 4044}#
                                  #{r\ 4030}#
                                  #{mod\ 4031}#)))))))
                     (begin (#{f\ 4042}# #{e\ 4025}# #{w\ 4029}#)))))
               (#{match-each-any\ 3993}#
                 (lambda (#{e\ 4058}# #{w\ 4059}# #{mod\ 4060}#)
                   (if (pair? #{e\ 4058}#)
                     (begin
                       (let ((#{l\ 4067}#
                               (#{match-each-any\ 3993}#
                                 (cdr #{e\ 4058}#)
                                 #{w\ 4059}#
                                 #{mod\ 4060}#)))
                         (if #{l\ 4067}#
                           (cons (#{wrap\ 456}#
                                   (car #{e\ 4058}#)
                                   #{w\ 4059}#
                                   #{mod\ 4060}#)
                                 #{l\ 4067}#)
                           #f)))
                     (if (null? #{e\ 4058}#)
                       '()
                       (if (#{syntax-object?\ 351}# #{e\ 4058}#)
                         (#{match-each-any\ 3993}#
                           (#{syntax-object-expression\ 353}# #{e\ 4058}#)
                           (#{join-wraps\ 438}#
                             #{w\ 4059}#
                             (#{syntax-object-wrap\ 355}# #{e\ 4058}#))
                           #{mod\ 4060}#)
                         #f)))))
               (#{match-empty\ 3995}#
                 (lambda (#{p\ 4075}# #{r\ 4076}#)
                   (if (null? #{p\ 4075}#)
                     #{r\ 4076}#
                     (if (eq? #{p\ 4075}# (quote any))
                       (cons (quote ()) #{r\ 4076}#)
                       (if (pair? #{p\ 4075}#)
                         (#{match-empty\ 3995}#
                           (car #{p\ 4075}#)
                           (#{match-empty\ 3995}#
                             (cdr #{p\ 4075}#)
                             #{r\ 4076}#))
                         (if (eq? #{p\ 4075}# (quote each-any))
                           (cons (quote ()) #{r\ 4076}#)
                           (begin
                             (let ((#{atom-key\ 4090}#
                                     (vector-ref #{p\ 4075}# 0)))
                               (if (eqv? #{atom-key\ 4090}# (quote each))
                                 (#{match-empty\ 3995}#
                                   (vector-ref #{p\ 4075}# 1)
                                   #{r\ 4076}#)
                                 (if (eqv? #{atom-key\ 4090}# (quote each+))
                                   (#{match-empty\ 3995}#
                                     (vector-ref #{p\ 4075}# 1)
                                     (#{match-empty\ 3995}#
                                       (reverse (vector-ref #{p\ 4075}# 2))
                                       (#{match-empty\ 3995}#
                                         (vector-ref #{p\ 4075}# 3)
                                         #{r\ 4076}#)))
                                   (if (if (eqv? #{atom-key\ 4090}#
                                                 'free-id)
                                         #t
                                         (eqv? #{atom-key\ 4090}#
                                               'atom))
                                     #{r\ 4076}#
                                     (if (eqv? #{atom-key\ 4090}#
                                               'vector)
                                       (#{match-empty\ 3995}#
                                         (vector-ref #{p\ 4075}# 1)
                                         #{r\ 4076}#)))))))))))))
               (#{combine\ 3997}#
                 (lambda (#{r*\ 4095}# #{r\ 4096}#)
                   (if (null? (car #{r*\ 4095}#))
                     #{r\ 4096}#
                     (cons (map car #{r*\ 4095}#)
                           (#{combine\ 3997}#
                             (map cdr #{r*\ 4095}#)
                             #{r\ 4096}#)))))
               (#{match*\ 3999}#
                 (lambda (#{e\ 4099}#
                          #{p\ 4100}#
                          #{w\ 4101}#
                          #{r\ 4102}#
                          #{mod\ 4103}#)
                   (if (null? #{p\ 4100}#)
                     (if (null? #{e\ 4099}#) #{r\ 4102}# #f)
                     (if (pair? #{p\ 4100}#)
                       (if (pair? #{e\ 4099}#)
                         (#{match\ 4001}#
                           (car #{e\ 4099}#)
                           (car #{p\ 4100}#)
                           #{w\ 4101}#
                           (#{match\ 4001}#
                             (cdr #{e\ 4099}#)
                             (cdr #{p\ 4100}#)
                             #{w\ 4101}#
                             #{r\ 4102}#
                             #{mod\ 4103}#)
                           #{mod\ 4103}#)
                         #f)
                       (if (eq? #{p\ 4100}# (quote each-any))
                         (begin
                           (let ((#{l\ 4120}#
                                   (#{match-each-any\ 3993}#
                                     #{e\ 4099}#
                                     #{w\ 4101}#
                                     #{mod\ 4103}#)))
                             (if #{l\ 4120}#
                               (cons #{l\ 4120}# #{r\ 4102}#)
                               #f)))
                         (begin
                           (let ((#{atom-key\ 4126}#
                                   (vector-ref #{p\ 4100}# 0)))
                             (if (eqv? #{atom-key\ 4126}# (quote each))
                               (if (null? #{e\ 4099}#)
                                 (#{match-empty\ 3995}#
                                   (vector-ref #{p\ 4100}# 1)
                                   #{r\ 4102}#)
                                 (begin
                                   (let ((#{l\ 4129}#
                                           (#{match-each\ 3989}#
                                             #{e\ 4099}#
                                             (vector-ref #{p\ 4100}# 1)
                                             #{w\ 4101}#
                                             #{mod\ 4103}#)))
                                     (if #{l\ 4129}#
                                       (letrec*
                                         ((#{collect\ 4134}#
                                            (lambda (#{l\ 4135}#)
                                              (if (null? (car #{l\ 4135}#))
                                                #{r\ 4102}#
                                                (cons (map car #{l\ 4135}#)
                                                      (#{collect\ 4134}#
                                                        (map cdr
                                                             #{l\ 4135}#)))))))
                                         (begin
                                           (#{collect\ 4134}# #{l\ 4129}#)))
                                       #f))))
                               (if (eqv? #{atom-key\ 4126}# (quote each+))
                                 (call-with-values
                                   (lambda ()
                                     (#{match-each+\ 3991}#
                                       #{e\ 4099}#
                                       (vector-ref #{p\ 4100}# 1)
                                       (vector-ref #{p\ 4100}# 2)
                                       (vector-ref #{p\ 4100}# 3)
                                       #{w\ 4101}#
                                       #{r\ 4102}#
                                       #{mod\ 4103}#))
                                   (lambda (#{xr*\ 4137}#
                                            #{y-pat\ 4138}#
                                            #{r\ 4139}#)
                                     (if #{r\ 4139}#
                                       (if (null? #{y-pat\ 4138}#)
                                         (if (null? #{xr*\ 4137}#)
                                           (#{match-empty\ 3995}#
                                             (vector-ref #{p\ 4100}# 1)
                                             #{r\ 4139}#)
                                           (#{combine\ 3997}#
                                             #{xr*\ 4137}#
                                             #{r\ 4139}#))
                                         #f)
                                       #f)))
                                 (if (eqv? #{atom-key\ 4126}# (quote free-id))
                                   (if (#{id?\ 387}# #{e\ 4099}#)
                                     (if (#{free-id=?\ 446}#
                                           (#{wrap\ 456}#
                                             #{e\ 4099}#
                                             #{w\ 4101}#
                                             #{mod\ 4103}#)
                                           (vector-ref #{p\ 4100}# 1))
                                       #{r\ 4102}#
                                       #f)
                                     #f)
                                   (if (eqv? #{atom-key\ 4126}# (quote atom))
                                     (if (equal?
                                           (vector-ref #{p\ 4100}# 1)
                                           (#{strip\ 498}#
                                             #{e\ 4099}#
                                             #{w\ 4101}#))
                                       #{r\ 4102}#
                                       #f)
                                     (if (eqv? #{atom-key\ 4126}#
                                               'vector)
                                       (if (vector? #{e\ 4099}#)
                                         (#{match\ 4001}#
                                           (vector->list #{e\ 4099}#)
                                           (vector-ref #{p\ 4100}# 1)
                                           #{w\ 4101}#
                                           #{r\ 4102}#
                                           #{mod\ 4103}#)
                                         #f)))))))))))))
               (#{match\ 4001}#
                 (lambda (#{e\ 4156}#
                          #{p\ 4157}#
                          #{w\ 4158}#
                          #{r\ 4159}#
                          #{mod\ 4160}#)
                   (if (not #{r\ 4159}#)
                     #f
                     (if (eq? #{p\ 4157}# (quote any))
                       (cons (#{wrap\ 456}#
                               #{e\ 4156}#
                               #{w\ 4158}#
                               #{mod\ 4160}#)
                             #{r\ 4159}#)
                       (if (#{syntax-object?\ 351}# #{e\ 4156}#)
                         (#{match*\ 3999}#
                           (#{syntax-object-expression\ 353}# #{e\ 4156}#)
                           #{p\ 4157}#
                           (#{join-wraps\ 438}#
                             #{w\ 4158}#
                             (#{syntax-object-wrap\ 355}# #{e\ 4156}#))
                           #{r\ 4159}#
                           (#{syntax-object-module\ 357}# #{e\ 4156}#))
                         (#{match*\ 3999}#
                           #{e\ 4156}#
                           #{p\ 4157}#
                           #{w\ 4158}#
                           #{r\ 4159}#
                           #{mod\ 4160}#)))))))
              (begin
                (set! $sc-dispatch
                  (lambda (#{e\ 4173}# #{p\ 4174}#)
                    (if (eq? #{p\ 4174}# (quote any))
                      (list #{e\ 4173}#)
                      (if (#{syntax-object?\ 351}# #{e\ 4173}#)
                        (#{match*\ 3999}#
                          (#{syntax-object-expression\ 353}# #{e\ 4173}#)
                          #{p\ 4174}#
                          (#{syntax-object-wrap\ 355}# #{e\ 4173}#)
                          '()
                          (#{syntax-object-module\ 357}# #{e\ 4173}#))
                        (#{match*\ 3999}#
                          #{e\ 4173}#
                          #{p\ 4174}#
                          '(())
                          '()
                          #f)))))))))))))

(define with-syntax
  (make-syntax-transformer
    'with-syntax
    'macro
    (lambda (#{x\ 4183}#)
      (let ((#{tmp\ 4185}# #{x\ 4183}#))
        (let ((#{tmp\ 4186}#
                ($sc-dispatch
                  #{tmp\ 4185}#
                  '(any () any . each-any))))
          (if #{tmp\ 4186}#
            (@apply
              (lambda (#{_\ 4190}# #{e1\ 4191}# #{e2\ 4192}#)
                (cons '#(syntax-object
                         begin
                         ((top)
                          #(ribcage
                            #(_ e1 e2)
                            #((top) (top) (top))
                            #("i4187" "i4188" "i4189"))
                          #(ribcage () () ())
                          #(ribcage #(x) #((top)) #("i4184")))
                         (hygiene guile))
                      (cons #{e1\ 4191}# #{e2\ 4192}#)))
              #{tmp\ 4186}#)
            (let ((#{tmp\ 4194}#
                    ($sc-dispatch
                      #{tmp\ 4185}#
                      '(any ((any any)) any . each-any))))
              (if #{tmp\ 4194}#
                (@apply
                  (lambda (#{_\ 4200}#
                           #{out\ 4201}#
                           #{in\ 4202}#
                           #{e1\ 4203}#
                           #{e2\ 4204}#)
                    (list '#(syntax-object
                             syntax-case
                             ((top)
                              #(ribcage
                                #(_ out in e1 e2)
                                #((top) (top) (top) (top) (top))
                                #("i4195" "i4196" "i4197" "i4198" "i4199"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4184")))
                             (hygiene guile))
                          #{in\ 4202}#
                          '()
                          (list #{out\ 4201}#
                                (cons '#(syntax-object
                                         begin
                                         ((top)
                                          #(ribcage
                                            #(_ out in e1 e2)
                                            #((top) (top) (top) (top) (top))
                                            #("i4195"
                                              "i4196"
                                              "i4197"
                                              "i4198"
                                              "i4199"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i4184")))
                                         (hygiene guile))
                                      (cons #{e1\ 4203}# #{e2\ 4204}#)))))
                  #{tmp\ 4194}#)
                (let ((#{tmp\ 4206}#
                        ($sc-dispatch
                          #{tmp\ 4185}#
                          '(any #(each (any any)) any . each-any))))
                  (if #{tmp\ 4206}#
                    (@apply
                      (lambda (#{_\ 4212}#
                               #{out\ 4213}#
                               #{in\ 4214}#
                               #{e1\ 4215}#
                               #{e2\ 4216}#)
                        (list '#(syntax-object
                                 syntax-case
                                 ((top)
                                  #(ribcage
                                    #(_ out in e1 e2)
                                    #((top) (top) (top) (top) (top))
                                    #("i4207" "i4208" "i4209" "i4210" "i4211"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i4184")))
                                 (hygiene guile))
                              (cons '#(syntax-object
                                       list
                                       ((top)
                                        #(ribcage
                                          #(_ out in e1 e2)
                                          #((top) (top) (top) (top) (top))
                                          #("i4207"
                                            "i4208"
                                            "i4209"
                                            "i4210"
                                            "i4211"))
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i4184")))
                                       (hygiene guile))
                                    #{in\ 4214}#)
                              '()
                              (list #{out\ 4213}#
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
                                                #("i4207"
                                                  "i4208"
                                                  "i4209"
                                                  "i4210"
                                                  "i4211"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(x)
                                                #((top))
                                                #("i4184")))
                                             (hygiene guile))
                                          (cons #{e1\ 4215}# #{e2\ 4216}#)))))
                      #{tmp\ 4206}#)
                    (syntax-violation
                      #f
                      "source expression failed to match any pattern"
                      #{tmp\ 4185}#)))))))))))

(define syntax-rules
  (make-syntax-transformer
    'syntax-rules
    'macro
    (lambda (#{x\ 4220}#)
      (let ((#{tmp\ 4222}# #{x\ 4220}#))
        (let ((#{tmp\ 4223}#
                ($sc-dispatch
                  #{tmp\ 4222}#
                  '(any each-any . #(each ((any . any) any))))))
          (if #{tmp\ 4223}#
            (@apply
              (lambda (#{_\ 4229}#
                       #{k\ 4230}#
                       #{keyword\ 4231}#
                       #{pattern\ 4232}#
                       #{template\ 4233}#)
                (list '#(syntax-object
                         lambda
                         ((top)
                          #(ribcage
                            #(_ k keyword pattern template)
                            #((top) (top) (top) (top) (top))
                            #("i4224" "i4225" "i4226" "i4227" "i4228"))
                          #(ribcage () () ())
                          #(ribcage #(x) #((top)) #("i4221")))
                         (hygiene guile))
                      '(#(syntax-object
                          x
                          ((top)
                           #(ribcage
                             #(_ k keyword pattern template)
                             #((top) (top) (top) (top) (top))
                             #("i4224" "i4225" "i4226" "i4227" "i4228"))
                           #(ribcage () () ())
                           #(ribcage #(x) #((top)) #("i4221")))
                          (hygiene guile)))
                      (vector
                        '(#(syntax-object
                            macro-type
                            ((top)
                             #(ribcage
                               #(_ k keyword pattern template)
                               #((top) (top) (top) (top) (top))
                               #("i4224" "i4225" "i4226" "i4227" "i4228"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i4221")))
                            (hygiene guile))
                          .
                          #(syntax-object
                            syntax-rules
                            ((top)
                             #(ribcage
                               #(_ k keyword pattern template)
                               #((top) (top) (top) (top) (top))
                               #("i4224" "i4225" "i4226" "i4227" "i4228"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i4221")))
                            (hygiene guile)))
                        (cons '#(syntax-object
                                 patterns
                                 ((top)
                                  #(ribcage
                                    #(_ k keyword pattern template)
                                    #((top) (top) (top) (top) (top))
                                    #("i4224" "i4225" "i4226" "i4227" "i4228"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i4221")))
                                 (hygiene guile))
                              #{pattern\ 4232}#))
                      (cons '#(syntax-object
                               syntax-case
                               ((top)
                                #(ribcage
                                  #(_ k keyword pattern template)
                                  #((top) (top) (top) (top) (top))
                                  #("i4224" "i4225" "i4226" "i4227" "i4228"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i4221")))
                               (hygiene guile))
                            (cons '#(syntax-object
                                     x
                                     ((top)
                                      #(ribcage
                                        #(_ k keyword pattern template)
                                        #((top) (top) (top) (top) (top))
                                        #("i4224"
                                          "i4225"
                                          "i4226"
                                          "i4227"
                                          "i4228"))
                                      #(ribcage () () ())
                                      #(ribcage #(x) #((top)) #("i4221")))
                                     (hygiene guile))
                                  (cons #{k\ 4230}#
                                        (map (lambda (#{tmp\ 4237}#
                                                      #{tmp\ 4236}#)
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
                                                                 #("i4224"
                                                                   "i4225"
                                                                   "i4226"
                                                                   "i4227"
                                                                   "i4228"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4221")))
                                                              (hygiene guile))
                                                           #{tmp\ 4236}#)
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
                                                                 #("i4224"
                                                                   "i4225"
                                                                   "i4226"
                                                                   "i4227"
                                                                   "i4228"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4221")))
                                                              (hygiene guile))
                                                           #{tmp\ 4237}#)))
                                             #{template\ 4233}#
                                             #{pattern\ 4232}#))))))
              #{tmp\ 4223}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp\ 4222}#)))))))

(define let*
  (make-syntax-transformer
    'let*
    'macro
    (lambda (#{x\ 4238}#)
      (let ((#{tmp\ 4240}# #{x\ 4238}#))
        (let ((#{tmp\ 4241}#
                ($sc-dispatch
                  #{tmp\ 4240}#
                  '(any #(each (any any)) any . each-any))))
          (if (if #{tmp\ 4241}#
                (@apply
                  (lambda (#{let*\ 4247}#
                           #{x\ 4248}#
                           #{v\ 4249}#
                           #{e1\ 4250}#
                           #{e2\ 4251}#)
                    (and-map identifier? #{x\ 4248}#))
                  #{tmp\ 4241}#)
                #f)
            (@apply
              (lambda (#{let*\ 4258}#
                       #{x\ 4259}#
                       #{v\ 4260}#
                       #{e1\ 4261}#
                       #{e2\ 4262}#)
                (letrec*
                  ((#{f\ 4265}#
                     (lambda (#{bindings\ 4266}#)
                       (if (null? #{bindings\ 4266}#)
                         (cons '#(syntax-object
                                  let
                                  ((top)
                                   #(ribcage () () ())
                                   #(ribcage
                                     #(f bindings)
                                     #((top) (top))
                                     #("i4263" "i4264"))
                                   #(ribcage
                                     #(let* x v e1 e2)
                                     #((top) (top) (top) (top) (top))
                                     #("i4253"
                                       "i4254"
                                       "i4255"
                                       "i4256"
                                       "i4257"))
                                   #(ribcage () () ())
                                   #(ribcage #(x) #((top)) #("i4239")))
                                  (hygiene guile))
                               (cons '()
                                     (cons #{e1\ 4261}# #{e2\ 4262}#)))
                         (let ((#{tmp\ 4271}#
                                 (list (#{f\ 4265}# (cdr #{bindings\ 4266}#))
                                       (car #{bindings\ 4266}#))))
                           (let ((#{tmp\ 4272}#
                                   ($sc-dispatch
                                     #{tmp\ 4271}#
                                     '(any any))))
                             (if #{tmp\ 4272}#
                               (@apply
                                 (lambda (#{body\ 4275}# #{binding\ 4276}#)
                                   (list '#(syntax-object
                                            let
                                            ((top)
                                             #(ribcage
                                               #(body binding)
                                               #((top) (top))
                                               #("i4273" "i4274"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(f bindings)
                                               #((top) (top))
                                               #("i4263" "i4264"))
                                             #(ribcage
                                               #(let* x v e1 e2)
                                               #((top) (top) (top) (top) (top))
                                               #("i4253"
                                                 "i4254"
                                                 "i4255"
                                                 "i4256"
                                                 "i4257"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x)
                                               #((top))
                                               #("i4239")))
                                            (hygiene guile))
                                         (list #{binding\ 4276}#)
                                         #{body\ 4275}#))
                                 #{tmp\ 4272}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp\ 4271}#))))))))
                  (begin
                    (#{f\ 4265}# (map list #{x\ 4259}# #{v\ 4260}#)))))
              #{tmp\ 4241}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp\ 4240}#)))))))

(define do
  (make-syntax-transformer
    'do
    'macro
    (lambda (#{orig-x\ 4277}#)
      (let ((#{tmp\ 4279}# #{orig-x\ 4277}#))
        (let ((#{tmp\ 4280}#
                ($sc-dispatch
                  #{tmp\ 4279}#
                  '(any #(each (any any . any))
                        (any . each-any)
                        .
                        each-any))))
          (if #{tmp\ 4280}#
            (@apply
              (lambda (#{_\ 4288}#
                       #{var\ 4289}#
                       #{init\ 4290}#
                       #{step\ 4291}#
                       #{e0\ 4292}#
                       #{e1\ 4293}#
                       #{c\ 4294}#)
                (let ((#{tmp\ 4296}#
                        (map (lambda (#{v\ 4317}# #{s\ 4318}#)
                               (let ((#{tmp\ 4321}# #{s\ 4318}#))
                                 (let ((#{tmp\ 4322}#
                                         ($sc-dispatch
                                           #{tmp\ 4321}#
                                           '())))
                                   (if #{tmp\ 4322}#
                                     (@apply
                                       (lambda () #{v\ 4317}#)
                                       #{tmp\ 4322}#)
                                     (let ((#{tmp\ 4323}#
                                             ($sc-dispatch
                                               #{tmp\ 4321}#
                                               '(any))))
                                       (if #{tmp\ 4323}#
                                         (@apply
                                           (lambda (#{e\ 4325}#) #{e\ 4325}#)
                                           #{tmp\ 4323}#)
                                         (let ((#{_\ 4327}# #{tmp\ 4321}#))
                                           (syntax-violation
                                             'do
                                             "bad step expression"
                                             #{orig-x\ 4277}#
                                             #{s\ 4318}#))))))))
                             #{var\ 4289}#
                             #{step\ 4291}#)))
                  (let ((#{tmp\ 4297}#
                          ($sc-dispatch #{tmp\ 4296}# (quote each-any))))
                    (if #{tmp\ 4297}#
                      (@apply
                        (lambda (#{step\ 4299}#)
                          (let ((#{tmp\ 4300}# #{e1\ 4293}#))
                            (let ((#{tmp\ 4301}#
                                    ($sc-dispatch #{tmp\ 4300}# (quote ()))))
                              (if #{tmp\ 4301}#
                                (@apply
                                  (lambda ()
                                    (list '#(syntax-object
                                             let
                                             ((top)
                                              #(ribcage
                                                #(step)
                                                #((top))
                                                #("i4298"))
                                              #(ribcage
                                                #(_ var init step e0 e1 c)
                                                #((top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top))
                                                #("i4281"
                                                  "i4282"
                                                  "i4283"
                                                  "i4284"
                                                  "i4285"
                                                  "i4286"
                                                  "i4287"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(orig-x)
                                                #((top))
                                                #("i4278")))
                                             (hygiene guile))
                                          '#(syntax-object
                                             doloop
                                             ((top)
                                              #(ribcage
                                                #(step)
                                                #((top))
                                                #("i4298"))
                                              #(ribcage
                                                #(_ var init step e0 e1 c)
                                                #((top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top))
                                                #("i4281"
                                                  "i4282"
                                                  "i4283"
                                                  "i4284"
                                                  "i4285"
                                                  "i4286"
                                                  "i4287"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(orig-x)
                                                #((top))
                                                #("i4278")))
                                             (hygiene guile))
                                          (map list
                                               #{var\ 4289}#
                                               #{init\ 4290}#)
                                          (list '#(syntax-object
                                                   if
                                                   ((top)
                                                    #(ribcage
                                                      #(step)
                                                      #((top))
                                                      #("i4298"))
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
                                                      #("i4281"
                                                        "i4282"
                                                        "i4283"
                                                        "i4284"
                                                        "i4285"
                                                        "i4286"
                                                        "i4287"))
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(orig-x)
                                                      #((top))
                                                      #("i4278")))
                                                   (hygiene guile))
                                                (list '#(syntax-object
                                                         not
                                                         ((top)
                                                          #(ribcage
                                                            #(step)
                                                            #((top))
                                                            #("i4298"))
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
                                                            #("i4281"
                                                              "i4282"
                                                              "i4283"
                                                              "i4284"
                                                              "i4285"
                                                              "i4286"
                                                              "i4287"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(orig-x)
                                                            #((top))
                                                            #("i4278")))
                                                         (hygiene guile))
                                                      #{e0\ 4292}#)
                                                (cons '#(syntax-object
                                                         begin
                                                         ((top)
                                                          #(ribcage
                                                            #(step)
                                                            #((top))
                                                            #("i4298"))
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
                                                            #("i4281"
                                                              "i4282"
                                                              "i4283"
                                                              "i4284"
                                                              "i4285"
                                                              "i4286"
                                                              "i4287"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(orig-x)
                                                            #((top))
                                                            #("i4278")))
                                                         (hygiene guile))
                                                      (append
                                                        #{c\ 4294}#
                                                        (list (cons '#(syntax-object
                                                                       doloop
                                                                       ((top)
                                                                        #(ribcage
                                                                          #(step)
                                                                          #((top))
                                                                          #("i4298"))
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
                                                                          #("i4281"
                                                                            "i4282"
                                                                            "i4283"
                                                                            "i4284"
                                                                            "i4285"
                                                                            "i4286"
                                                                            "i4287"))
                                                                        #(ribcage
                                                                          ()
                                                                          ()
                                                                          ())
                                                                        #(ribcage
                                                                          #(orig-x)
                                                                          #((top))
                                                                          #("i4278")))
                                                                       (hygiene
                                                                         guile))
                                                                    #{step\ 4299}#)))))))
                                  #{tmp\ 4301}#)
                                (let ((#{tmp\ 4306}#
                                        ($sc-dispatch
                                          #{tmp\ 4300}#
                                          '(any . each-any))))
                                  (if #{tmp\ 4306}#
                                    (@apply
                                      (lambda (#{e1\ 4309}# #{e2\ 4310}#)
                                        (list '#(syntax-object
                                                 let
                                                 ((top)
                                                  #(ribcage
                                                    #(e1 e2)
                                                    #((top) (top))
                                                    #("i4307" "i4308"))
                                                  #(ribcage
                                                    #(step)
                                                    #((top))
                                                    #("i4298"))
                                                  #(ribcage
                                                    #(_ var init step e0 e1 c)
                                                    #((top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top))
                                                    #("i4281"
                                                      "i4282"
                                                      "i4283"
                                                      "i4284"
                                                      "i4285"
                                                      "i4286"
                                                      "i4287"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(orig-x)
                                                    #((top))
                                                    #("i4278")))
                                                 (hygiene guile))
                                              '#(syntax-object
                                                 doloop
                                                 ((top)
                                                  #(ribcage
                                                    #(e1 e2)
                                                    #((top) (top))
                                                    #("i4307" "i4308"))
                                                  #(ribcage
                                                    #(step)
                                                    #((top))
                                                    #("i4298"))
                                                  #(ribcage
                                                    #(_ var init step e0 e1 c)
                                                    #((top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top))
                                                    #("i4281"
                                                      "i4282"
                                                      "i4283"
                                                      "i4284"
                                                      "i4285"
                                                      "i4286"
                                                      "i4287"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(orig-x)
                                                    #((top))
                                                    #("i4278")))
                                                 (hygiene guile))
                                              (map list
                                                   #{var\ 4289}#
                                                   #{init\ 4290}#)
                                              (list '#(syntax-object
                                                       if
                                                       ((top)
                                                        #(ribcage
                                                          #(e1 e2)
                                                          #((top) (top))
                                                          #("i4307" "i4308"))
                                                        #(ribcage
                                                          #(step)
                                                          #((top))
                                                          #("i4298"))
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
                                                          #("i4281"
                                                            "i4282"
                                                            "i4283"
                                                            "i4284"
                                                            "i4285"
                                                            "i4286"
                                                            "i4287"))
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(orig-x)
                                                          #((top))
                                                          #("i4278")))
                                                       (hygiene guile))
                                                    #{e0\ 4292}#
                                                    (cons '#(syntax-object
                                                             begin
                                                             ((top)
                                                              #(ribcage
                                                                #(e1 e2)
                                                                #((top) (top))
                                                                #("i4307"
                                                                  "i4308"))
                                                              #(ribcage
                                                                #(step)
                                                                #((top))
                                                                #("i4298"))
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
                                                                #("i4281"
                                                                  "i4282"
                                                                  "i4283"
                                                                  "i4284"
                                                                  "i4285"
                                                                  "i4286"
                                                                  "i4287"))
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                #(orig-x)
                                                                #((top))
                                                                #("i4278")))
                                                             (hygiene guile))
                                                          (cons #{e1\ 4309}#
                                                                #{e2\ 4310}#))
                                                    (cons '#(syntax-object
                                                             begin
                                                             ((top)
                                                              #(ribcage
                                                                #(e1 e2)
                                                                #((top) (top))
                                                                #("i4307"
                                                                  "i4308"))
                                                              #(ribcage
                                                                #(step)
                                                                #((top))
                                                                #("i4298"))
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
                                                                #("i4281"
                                                                  "i4282"
                                                                  "i4283"
                                                                  "i4284"
                                                                  "i4285"
                                                                  "i4286"
                                                                  "i4287"))
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                #(orig-x)
                                                                #((top))
                                                                #("i4278")))
                                                             (hygiene guile))
                                                          (append
                                                            #{c\ 4294}#
                                                            (list (cons '#(syntax-object
                                                                           doloop
                                                                           ((top)
                                                                            #(ribcage
                                                                              #(e1
                                                                                e2)
                                                                              #((top)
                                                                                (top))
                                                                              #("i4307"
                                                                                "i4308"))
                                                                            #(ribcage
                                                                              #(step)
                                                                              #((top))
                                                                              #("i4298"))
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
                                                                              #("i4281"
                                                                                "i4282"
                                                                                "i4283"
                                                                                "i4284"
                                                                                "i4285"
                                                                                "i4286"
                                                                                "i4287"))
                                                                            #(ribcage
                                                                              ()
                                                                              ()
                                                                              ())
                                                                            #(ribcage
                                                                              #(orig-x)
                                                                              #((top))
                                                                              #("i4278")))
                                                                           (hygiene
                                                                             guile))
                                                                        #{step\ 4299}#)))))))
                                      #{tmp\ 4306}#)
                                    (syntax-violation
                                      #f
                                      "source expression failed to match any pattern"
                                      #{tmp\ 4300}#)))))))
                        #{tmp\ 4297}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp\ 4296}#)))))
              #{tmp\ 4280}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp\ 4279}#)))))))

(define quasiquote
  (make-syntax-transformer
    'quasiquote
    'macro
    (letrec*
      ((#{quasicons\ 4334}#
         (lambda (#{x\ 4338}# #{y\ 4339}#)
           (let ((#{tmp\ 4343}# (list #{x\ 4338}# #{y\ 4339}#)))
             (let ((#{tmp\ 4344}#
                     ($sc-dispatch #{tmp\ 4343}# (quote (any any)))))
               (if #{tmp\ 4344}#
                 (@apply
                   (lambda (#{x\ 4347}# #{y\ 4348}#)
                     (let ((#{tmp\ 4349}# #{y\ 4348}#))
                       (let ((#{tmp\ 4350}#
                               ($sc-dispatch
                                 #{tmp\ 4349}#
                                 '(#(free-id
                                     #(syntax-object
                                       quote
                                       ((top)
                                        #(ribcage
                                          #(x y)
                                          #((top) (top))
                                          #("i4345" "i4346"))
                                        #(ribcage () () ())
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(x y)
                                          #((top) (top))
                                          #("i4340" "i4341"))
                                        #(ribcage
                                          #(quasicons
                                            quasiappend
                                            quasivector
                                            quasi)
                                          #((top) (top) (top) (top))
                                          #("i4330" "i4331" "i4332" "i4333")))
                                       (hygiene guile)))
                                   any))))
                         (if #{tmp\ 4350}#
                           (@apply
                             (lambda (#{dy\ 4352}#)
                               (let ((#{tmp\ 4353}# #{x\ 4347}#))
                                 (let ((#{tmp\ 4354}#
                                         ($sc-dispatch
                                           #{tmp\ 4353}#
                                           '(#(free-id
                                               #(syntax-object
                                                 quote
                                                 ((top)
                                                  #(ribcage
                                                    #(dy)
                                                    #((top))
                                                    #("i4351"))
                                                  #(ribcage
                                                    #(x y)
                                                    #((top) (top))
                                                    #("i4345" "i4346"))
                                                  #(ribcage () () ())
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(x y)
                                                    #((top) (top))
                                                    #("i4340" "i4341"))
                                                  #(ribcage
                                                    #(quasicons
                                                      quasiappend
                                                      quasivector
                                                      quasi)
                                                    #((top) (top) (top) (top))
                                                    #("i4330"
                                                      "i4331"
                                                      "i4332"
                                                      "i4333")))
                                                 (hygiene guile)))
                                             any))))
                                   (if #{tmp\ 4354}#
                                     (@apply
                                       (lambda (#{dx\ 4356}#)
                                         (list '#(syntax-object
                                                  quote
                                                  ((top)
                                                   #(ribcage
                                                     #(dx)
                                                     #((top))
                                                     #("i4355"))
                                                   #(ribcage
                                                     #(dy)
                                                     #((top))
                                                     #("i4351"))
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4345" "i4346"))
                                                   #(ribcage () () ())
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4340" "i4341"))
                                                   #(ribcage
                                                     #(quasicons
                                                       quasiappend
                                                       quasivector
                                                       quasi)
                                                     #((top) (top) (top) (top))
                                                     #("i4330"
                                                       "i4331"
                                                       "i4332"
                                                       "i4333")))
                                                  (hygiene guile))
                                               (cons #{dx\ 4356}#
                                                     #{dy\ 4352}#)))
                                       #{tmp\ 4354}#)
                                     (let ((#{_\ 4358}# #{tmp\ 4353}#))
                                       (if (null? #{dy\ 4352}#)
                                         (list '#(syntax-object
                                                  list
                                                  ((top)
                                                   #(ribcage
                                                     #(_)
                                                     #((top))
                                                     #("i4357"))
                                                   #(ribcage
                                                     #(dy)
                                                     #((top))
                                                     #("i4351"))
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4345" "i4346"))
                                                   #(ribcage () () ())
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4340" "i4341"))
                                                   #(ribcage
                                                     #(quasicons
                                                       quasiappend
                                                       quasivector
                                                       quasi)
                                                     #((top) (top) (top) (top))
                                                     #("i4330"
                                                       "i4331"
                                                       "i4332"
                                                       "i4333")))
                                                  (hygiene guile))
                                               #{x\ 4347}#)
                                         (list '#(syntax-object
                                                  cons
                                                  ((top)
                                                   #(ribcage
                                                     #(_)
                                                     #((top))
                                                     #("i4357"))
                                                   #(ribcage
                                                     #(dy)
                                                     #((top))
                                                     #("i4351"))
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4345" "i4346"))
                                                   #(ribcage () () ())
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4340" "i4341"))
                                                   #(ribcage
                                                     #(quasicons
                                                       quasiappend
                                                       quasivector
                                                       quasi)
                                                     #((top) (top) (top) (top))
                                                     #("i4330"
                                                       "i4331"
                                                       "i4332"
                                                       "i4333")))
                                                  (hygiene guile))
                                               #{x\ 4347}#
                                               #{y\ 4348}#)))))))
                             #{tmp\ 4350}#)
                           (let ((#{tmp\ 4359}#
                                   ($sc-dispatch
                                     #{tmp\ 4349}#
                                     '(#(free-id
                                         #(syntax-object
                                           list
                                           ((top)
                                            #(ribcage
                                              #(x y)
                                              #((top) (top))
                                              #("i4345" "i4346"))
                                            #(ribcage () () ())
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(x y)
                                              #((top) (top))
                                              #("i4340" "i4341"))
                                            #(ribcage
                                              #(quasicons
                                                quasiappend
                                                quasivector
                                                quasi)
                                              #((top) (top) (top) (top))
                                              #("i4330"
                                                "i4331"
                                                "i4332"
                                                "i4333")))
                                           (hygiene guile)))
                                       .
                                       any))))
                             (if #{tmp\ 4359}#
                               (@apply
                                 (lambda (#{stuff\ 4361}#)
                                   (cons '#(syntax-object
                                            list
                                            ((top)
                                             #(ribcage
                                               #(stuff)
                                               #((top))
                                               #("i4360"))
                                             #(ribcage
                                               #(x y)
                                               #((top) (top))
                                               #("i4345" "i4346"))
                                             #(ribcage () () ())
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x y)
                                               #((top) (top))
                                               #("i4340" "i4341"))
                                             #(ribcage
                                               #(quasicons
                                                 quasiappend
                                                 quasivector
                                                 quasi)
                                               #((top) (top) (top) (top))
                                               #("i4330"
                                                 "i4331"
                                                 "i4332"
                                                 "i4333")))
                                            (hygiene guile))
                                         (cons #{x\ 4347}# #{stuff\ 4361}#)))
                                 #{tmp\ 4359}#)
                               (let ((#{else\ 4363}# #{tmp\ 4349}#))
                                 (list '#(syntax-object
                                          cons
                                          ((top)
                                           #(ribcage
                                             #(else)
                                             #((top))
                                             #("i4362"))
                                           #(ribcage
                                             #(x y)
                                             #((top) (top))
                                             #("i4345" "i4346"))
                                           #(ribcage () () ())
                                           #(ribcage () () ())
                                           #(ribcage
                                             #(x y)
                                             #((top) (top))
                                             #("i4340" "i4341"))
                                           #(ribcage
                                             #(quasicons
                                               quasiappend
                                               quasivector
                                               quasi)
                                             #((top) (top) (top) (top))
                                             #("i4330"
                                               "i4331"
                                               "i4332"
                                               "i4333")))
                                          (hygiene guile))
                                       #{x\ 4347}#
                                       #{y\ 4348}#))))))))
                   #{tmp\ 4344}#)
                 (syntax-violation
                   #f
                   "source expression failed to match any pattern"
                   #{tmp\ 4343}#))))))
       (#{quasiappend\ 4335}#
         (lambda (#{x\ 4364}# #{y\ 4365}#)
           (let ((#{tmp\ 4369}# (list #{x\ 4364}# #{y\ 4365}#)))
             (let ((#{tmp\ 4370}#
                     ($sc-dispatch #{tmp\ 4369}# (quote (any any)))))
               (if #{tmp\ 4370}#
                 (@apply
                   (lambda (#{x\ 4373}# #{y\ 4374}#)
                     (let ((#{tmp\ 4375}# #{y\ 4374}#))
                       (let ((#{tmp\ 4376}#
                               ($sc-dispatch
                                 #{tmp\ 4375}#
                                 '(#(free-id
                                     #(syntax-object
                                       quote
                                       ((top)
                                        #(ribcage
                                          #(x y)
                                          #((top) (top))
                                          #("i4371" "i4372"))
                                        #(ribcage () () ())
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(x y)
                                          #((top) (top))
                                          #("i4366" "i4367"))
                                        #(ribcage
                                          #(quasicons
                                            quasiappend
                                            quasivector
                                            quasi)
                                          #((top) (top) (top) (top))
                                          #("i4330" "i4331" "i4332" "i4333")))
                                       (hygiene guile)))
                                   ()))))
                         (if #{tmp\ 4376}#
                           (@apply (lambda () #{x\ 4373}#) #{tmp\ 4376}#)
                           (let ((#{_\ 4378}# #{tmp\ 4375}#))
                             (list '#(syntax-object
                                      append
                                      ((top)
                                       #(ribcage #(_) #((top)) #("i4377"))
                                       #(ribcage
                                         #(x y)
                                         #((top) (top))
                                         #("i4371" "i4372"))
                                       #(ribcage () () ())
                                       #(ribcage () () ())
                                       #(ribcage
                                         #(x y)
                                         #((top) (top))
                                         #("i4366" "i4367"))
                                       #(ribcage
                                         #(quasicons
                                           quasiappend
                                           quasivector
                                           quasi)
                                         #((top) (top) (top) (top))
                                         #("i4330" "i4331" "i4332" "i4333")))
                                      (hygiene guile))
                                   #{x\ 4373}#
                                   #{y\ 4374}#))))))
                   #{tmp\ 4370}#)
                 (syntax-violation
                   #f
                   "source expression failed to match any pattern"
                   #{tmp\ 4369}#))))))
       (#{quasivector\ 4336}#
         (lambda (#{x\ 4379}#)
           (let ((#{tmp\ 4382}# #{x\ 4379}#))
             (let ((#{x\ 4384}# #{tmp\ 4382}#))
               (let ((#{tmp\ 4385}# #{x\ 4384}#))
                 (let ((#{tmp\ 4386}#
                         ($sc-dispatch
                           #{tmp\ 4385}#
                           '(#(free-id
                               #(syntax-object
                                 quote
                                 ((top)
                                  #(ribcage #(x) #((top)) #("i4383"))
                                  #(ribcage () () ())
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i4380"))
                                  #(ribcage
                                    #(quasicons quasiappend quasivector quasi)
                                    #((top) (top) (top) (top))
                                    #("i4330" "i4331" "i4332" "i4333")))
                                 (hygiene guile)))
                             each-any))))
                   (if #{tmp\ 4386}#
                     (@apply
                       (lambda (#{x\ 4388}#)
                         (list '#(syntax-object
                                  quote
                                  ((top)
                                   #(ribcage #(x) #((top)) #("i4387"))
                                   #(ribcage #(x) #((top)) #("i4383"))
                                   #(ribcage () () ())
                                   #(ribcage () () ())
                                   #(ribcage #(x) #((top)) #("i4380"))
                                   #(ribcage
                                     #(quasicons quasiappend quasivector quasi)
                                     #((top) (top) (top) (top))
                                     #("i4330" "i4331" "i4332" "i4333")))
                                  (hygiene guile))
                               (list->vector #{x\ 4388}#)))
                       #{tmp\ 4386}#)
                     (let ((#{tmp\ 4390}#
                             ($sc-dispatch
                               #{tmp\ 4385}#
                               '(#(free-id
                                   #(syntax-object
                                     list
                                     ((top)
                                      #(ribcage #(x) #((top)) #("i4383"))
                                      #(ribcage () () ())
                                      #(ribcage () () ())
                                      #(ribcage #(x) #((top)) #("i4380"))
                                      #(ribcage
                                        #(quasicons
                                          quasiappend
                                          quasivector
                                          quasi)
                                        #((top) (top) (top) (top))
                                        #("i4330" "i4331" "i4332" "i4333")))
                                     (hygiene guile)))
                                 .
                                 each-any))))
                       (if #{tmp\ 4390}#
                         (@apply
                           (lambda (#{x\ 4392}#)
                             (cons '#(syntax-object
                                      vector
                                      ((top)
                                       #(ribcage #(x) #((top)) #("i4391"))
                                       #(ribcage #(x) #((top)) #("i4383"))
                                       #(ribcage () () ())
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4380"))
                                       #(ribcage
                                         #(quasicons
                                           quasiappend
                                           quasivector
                                           quasi)
                                         #((top) (top) (top) (top))
                                         #("i4330" "i4331" "i4332" "i4333")))
                                      (hygiene guile))
                                   #{x\ 4392}#))
                           #{tmp\ 4390}#)
                         (let ((#{_\ 4395}# #{tmp\ 4385}#))
                           (list '#(syntax-object
                                    list->vector
                                    ((top)
                                     #(ribcage #(_) #((top)) #("i4394"))
                                     #(ribcage #(x) #((top)) #("i4383"))
                                     #(ribcage () () ())
                                     #(ribcage () () ())
                                     #(ribcage #(x) #((top)) #("i4380"))
                                     #(ribcage
                                       #(quasicons
                                         quasiappend
                                         quasivector
                                         quasi)
                                       #((top) (top) (top) (top))
                                       #("i4330" "i4331" "i4332" "i4333")))
                                    (hygiene guile))
                                 #{x\ 4384}#)))))))))))
       (#{quasi\ 4337}#
         (lambda (#{p\ 4396}# #{lev\ 4397}#)
           (let ((#{tmp\ 4400}# #{p\ 4396}#))
             (let ((#{tmp\ 4401}#
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
                                #("i4398" "i4399"))
                              #(ribcage
                                #(quasicons quasiappend quasivector quasi)
                                #((top) (top) (top) (top))
                                #("i4330" "i4331" "i4332" "i4333")))
                             (hygiene guile)))
                         any))))
               (if #{tmp\ 4401}#
                 (@apply
                   (lambda (#{p\ 4403}#)
                     (if (= #{lev\ 4397}# 0)
                       #{p\ 4403}#
                       (#{quasicons\ 4334}#
                         '(#(syntax-object
                             quote
                             ((top)
                              #(ribcage #(p) #((top)) #("i4402"))
                              #(ribcage () () ())
                              #(ribcage
                                #(p lev)
                                #((top) (top))
                                #("i4398" "i4399"))
                              #(ribcage
                                #(quasicons quasiappend quasivector quasi)
                                #((top) (top) (top) (top))
                                #("i4330" "i4331" "i4332" "i4333")))
                             (hygiene guile))
                           #(syntax-object
                             unquote
                             ((top)
                              #(ribcage #(p) #((top)) #("i4402"))
                              #(ribcage () () ())
                              #(ribcage
                                #(p lev)
                                #((top) (top))
                                #("i4398" "i4399"))
                              #(ribcage
                                #(quasicons quasiappend quasivector quasi)
                                #((top) (top) (top) (top))
                                #("i4330" "i4331" "i4332" "i4333")))
                             (hygiene guile)))
                         (#{quasi\ 4337}#
                           (list #{p\ 4403}#)
                           (1- #{lev\ 4397}#)))))
                   #{tmp\ 4401}#)
                 (let ((#{tmp\ 4404}#
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
                                    #("i4398" "i4399"))
                                  #(ribcage
                                    #(quasicons quasiappend quasivector quasi)
                                    #((top) (top) (top) (top))
                                    #("i4330" "i4331" "i4332" "i4333")))
                                 (hygiene guile)))
                             .
                             any))))
                   (if (if #{tmp\ 4404}#
                         (@apply
                           (lambda (#{args\ 4406}#) (= #{lev\ 4397}# 0))
                           #{tmp\ 4404}#)
                         #f)
                     (@apply
                       (lambda (#{args\ 4408}#)
                         (syntax-violation
                           'unquote
                           "unquote takes exactly one argument"
                           #{p\ 4396}#
                           (cons '#(syntax-object
                                    unquote
                                    ((top)
                                     #(ribcage #(args) #((top)) #("i4407"))
                                     #(ribcage () () ())
                                     #(ribcage
                                       #(p lev)
                                       #((top) (top))
                                       #("i4398" "i4399"))
                                     #(ribcage
                                       #(quasicons
                                         quasiappend
                                         quasivector
                                         quasi)
                                       #((top) (top) (top) (top))
                                       #("i4330" "i4331" "i4332" "i4333")))
                                    (hygiene guile))
                                 #{args\ 4408}#)))
                       #{tmp\ 4404}#)
                     (let ((#{tmp\ 4409}#
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
                                         #("i4398" "i4399"))
                                       #(ribcage
                                         #(quasicons
                                           quasiappend
                                           quasivector
                                           quasi)
                                         #((top) (top) (top) (top))
                                         #("i4330" "i4331" "i4332" "i4333")))
                                      (hygiene guile)))
                                  any)
                                 .
                                 any))))
                       (if #{tmp\ 4409}#
                         (@apply
                           (lambda (#{p\ 4412}# #{q\ 4413}#)
                             (if (= #{lev\ 4397}# 0)
                               (#{quasiappend\ 4335}#
                                 #{p\ 4412}#
                                 (#{quasi\ 4337}# #{q\ 4413}# #{lev\ 4397}#))
                               (#{quasicons\ 4334}#
                                 (#{quasicons\ 4334}#
                                   '(#(syntax-object
                                       quote
                                       ((top)
                                        #(ribcage
                                          #(p q)
                                          #((top) (top))
                                          #("i4410" "i4411"))
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(p lev)
                                          #((top) (top))
                                          #("i4398" "i4399"))
                                        #(ribcage
                                          #(quasicons
                                            quasiappend
                                            quasivector
                                            quasi)
                                          #((top) (top) (top) (top))
                                          #("i4330" "i4331" "i4332" "i4333")))
                                       (hygiene guile))
                                     #(syntax-object
                                       unquote-splicing
                                       ((top)
                                        #(ribcage
                                          #(p q)
                                          #((top) (top))
                                          #("i4410" "i4411"))
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(p lev)
                                          #((top) (top))
                                          #("i4398" "i4399"))
                                        #(ribcage
                                          #(quasicons
                                            quasiappend
                                            quasivector
                                            quasi)
                                          #((top) (top) (top) (top))
                                          #("i4330" "i4331" "i4332" "i4333")))
                                       (hygiene guile)))
                                   (#{quasi\ 4337}#
                                     (list #{p\ 4412}#)
                                     (1- #{lev\ 4397}#)))
                                 (#{quasi\ 4337}# #{q\ 4413}# #{lev\ 4397}#))))
                           #{tmp\ 4409}#)
                         (let ((#{tmp\ 4414}#
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
                                             #("i4398" "i4399"))
                                           #(ribcage
                                             #(quasicons
                                               quasiappend
                                               quasivector
                                               quasi)
                                             #((top) (top) (top) (top))
                                             #("i4330"
                                               "i4331"
                                               "i4332"
                                               "i4333")))
                                          (hygiene guile)))
                                      .
                                      any)
                                     .
                                     any))))
                           (if (if #{tmp\ 4414}#
                                 (@apply
                                   (lambda (#{args\ 4417}# #{q\ 4418}#)
                                     (= #{lev\ 4397}# 0))
                                   #{tmp\ 4414}#)
                                 #f)
                             (@apply
                               (lambda (#{args\ 4421}# #{q\ 4422}#)
                                 (syntax-violation
                                   'unquote-splicing
                                   "unquote-splicing takes exactly one argument"
                                   #{p\ 4396}#
                                   (cons '#(syntax-object
                                            unquote-splicing
                                            ((top)
                                             #(ribcage
                                               #(args q)
                                               #((top) (top))
                                               #("i4419" "i4420"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(p lev)
                                               #((top) (top))
                                               #("i4398" "i4399"))
                                             #(ribcage
                                               #(quasicons
                                                 quasiappend
                                                 quasivector
                                                 quasi)
                                               #((top) (top) (top) (top))
                                               #("i4330"
                                                 "i4331"
                                                 "i4332"
                                                 "i4333")))
                                            (hygiene guile))
                                         #{args\ 4421}#)))
                               #{tmp\ 4414}#)
                             (let ((#{tmp\ 4423}#
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
                                                #("i4398" "i4399"))
                                              #(ribcage
                                                #(quasicons
                                                  quasiappend
                                                  quasivector
                                                  quasi)
                                                #((top) (top) (top) (top))
                                                #("i4330"
                                                  "i4331"
                                                  "i4332"
                                                  "i4333")))
                                             (hygiene guile)))
                                         any))))
                               (if #{tmp\ 4423}#
                                 (@apply
                                   (lambda (#{p\ 4425}#)
                                     (#{quasicons\ 4334}#
                                       '(#(syntax-object
                                           quote
                                           ((top)
                                            #(ribcage #(p) #((top)) #("i4424"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(p lev)
                                              #((top) (top))
                                              #("i4398" "i4399"))
                                            #(ribcage
                                              #(quasicons
                                                quasiappend
                                                quasivector
                                                quasi)
                                              #((top) (top) (top) (top))
                                              #("i4330"
                                                "i4331"
                                                "i4332"
                                                "i4333")))
                                           (hygiene guile))
                                         #(syntax-object
                                           quasiquote
                                           ((top)
                                            #(ribcage #(p) #((top)) #("i4424"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(p lev)
                                              #((top) (top))
                                              #("i4398" "i4399"))
                                            #(ribcage
                                              #(quasicons
                                                quasiappend
                                                quasivector
                                                quasi)
                                              #((top) (top) (top) (top))
                                              #("i4330"
                                                "i4331"
                                                "i4332"
                                                "i4333")))
                                           (hygiene guile)))
                                       (#{quasi\ 4337}#
                                         (list #{p\ 4425}#)
                                         (1+ #{lev\ 4397}#))))
                                   #{tmp\ 4423}#)
                                 (let ((#{tmp\ 4426}#
                                         ($sc-dispatch
                                           #{tmp\ 4400}#
                                           '(any . any))))
                                   (if #{tmp\ 4426}#
                                     (@apply
                                       (lambda (#{p\ 4429}# #{q\ 4430}#)
                                         (#{quasicons\ 4334}#
                                           (#{quasi\ 4337}#
                                             #{p\ 4429}#
                                             #{lev\ 4397}#)
                                           (#{quasi\ 4337}#
                                             #{q\ 4430}#
                                             #{lev\ 4397}#)))
                                       #{tmp\ 4426}#)
                                     (let ((#{tmp\ 4431}#
                                             ($sc-dispatch
                                               #{tmp\ 4400}#
                                               '#(vector each-any))))
                                       (if #{tmp\ 4431}#
                                         (@apply
                                           (lambda (#{x\ 4433}#)
                                             (#{quasivector\ 4336}#
                                               (#{quasi\ 4337}#
                                                 #{x\ 4433}#
                                                 #{lev\ 4397}#)))
                                           #{tmp\ 4431}#)
                                         (let ((#{p\ 4436}# #{tmp\ 4400}#))
                                           (list '#(syntax-object
                                                    quote
                                                    ((top)
                                                     #(ribcage
                                                       #(p)
                                                       #((top))
                                                       #("i4435"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(p lev)
                                                       #((top) (top))
                                                       #("i4398" "i4399"))
                                                     #(ribcage
                                                       #(quasicons
                                                         quasiappend
                                                         quasivector
                                                         quasi)
                                                       #((top)
                                                         (top)
                                                         (top)
                                                         (top))
                                                       #("i4330"
                                                         "i4331"
                                                         "i4332"
                                                         "i4333")))
                                                    (hygiene guile))
                                                 #{p\ 4436}#))))))))))))))))))))
      (begin
        (lambda (#{x\ 4437}#)
          (let ((#{tmp\ 4439}# #{x\ 4437}#))
            (let ((#{tmp\ 4440}#
                    ($sc-dispatch #{tmp\ 4439}# (quote (any any)))))
              (if #{tmp\ 4440}#
                (@apply
                  (lambda (#{_\ 4443}# #{e\ 4444}#)
                    (#{quasi\ 4337}# #{e\ 4444}# 0))
                  #{tmp\ 4440}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 4439}#)))))))))

(define include
  (make-syntax-transformer
    'include
    'macro
    (lambda (#{x\ 4445}#)
      (letrec*
        ((#{read-file\ 4448}#
           (lambda (#{fn\ 4449}# #{k\ 4450}#)
             (begin
               (let ((#{p\ 4454}# (open-input-file #{fn\ 4449}#)))
                 (letrec*
                   ((#{f\ 4458}#
                      (lambda (#{x\ 4459}# #{result\ 4460}#)
                        (if (eof-object? #{x\ 4459}#)
                          (begin
                            (close-input-port #{p\ 4454}#)
                            (reverse #{result\ 4460}#))
                          (#{f\ 4458}#
                            (read #{p\ 4454}#)
                            (cons (datum->syntax #{k\ 4450}# #{x\ 4459}#)
                                  #{result\ 4460}#))))))
                   (begin
                     (#{f\ 4458}# (read #{p\ 4454}#) (quote ())))))))))
        (begin
          (let ((#{tmp\ 4461}# #{x\ 4445}#))
            (let ((#{tmp\ 4462}#
                    ($sc-dispatch #{tmp\ 4461}# (quote (any any)))))
              (if #{tmp\ 4462}#
                (@apply
                  (lambda (#{k\ 4465}# #{filename\ 4466}#)
                    (begin
                      (let ((#{fn\ 4468}# (syntax->datum #{filename\ 4466}#)))
                        (let ((#{tmp\ 4470}#
                                (#{read-file\ 4448}#
                                  #{fn\ 4468}#
                                  #{filename\ 4466}#)))
                          (let ((#{tmp\ 4471}#
                                  ($sc-dispatch
                                    #{tmp\ 4470}#
                                    'each-any)))
                            (if #{tmp\ 4471}#
                              (@apply
                                (lambda (#{exp\ 4473}#)
                                  (cons '#(syntax-object
                                           begin
                                           ((top)
                                            #(ribcage
                                              #(exp)
                                              #((top))
                                              #("i4472"))
                                            #(ribcage () () ())
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(fn)
                                              #((top))
                                              #("i4467"))
                                            #(ribcage
                                              #(k filename)
                                              #((top) (top))
                                              #("i4463" "i4464"))
                                            #(ribcage
                                              (read-file)
                                              ((top))
                                              ("i4447"))
                                            #(ribcage
                                              #(x)
                                              #((top))
                                              #("i4446")))
                                           (hygiene guile))
                                        #{exp\ 4473}#))
                                #{tmp\ 4471}#)
                              (syntax-violation
                                #f
                                "source expression failed to match any pattern"
                                #{tmp\ 4470}#)))))))
                  #{tmp\ 4462}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 4461}#)))))))))

(define include-from-path
  (make-syntax-transformer
    'include-from-path
    'macro
    (lambda (#{x\ 4475}#)
      (let ((#{tmp\ 4477}# #{x\ 4475}#))
        (let ((#{tmp\ 4478}#
                ($sc-dispatch #{tmp\ 4477}# (quote (any any)))))
          (if #{tmp\ 4478}#
            (@apply
              (lambda (#{k\ 4481}# #{filename\ 4482}#)
                (begin
                  (let ((#{fn\ 4484}# (syntax->datum #{filename\ 4482}#)))
                    (let ((#{tmp\ 4486}#
                            (datum->syntax
                              #{filename\ 4482}#
                              (begin
                                (let ((#{t\ 4491}#
                                        (%search-load-path #{fn\ 4484}#)))
                                  (if #{t\ 4491}#
                                    #{t\ 4491}#
                                    (syntax-violation
                                      'include-from-path
                                      "file not found in path"
                                      #{x\ 4475}#
                                      #{filename\ 4482}#)))))))
                      (let ((#{fn\ 4488}# #{tmp\ 4486}#))
                        (list '#(syntax-object
                                 include
                                 ((top)
                                  #(ribcage #(fn) #((top)) #("i4487"))
                                  #(ribcage () () ())
                                  #(ribcage () () ())
                                  #(ribcage #(fn) #((top)) #("i4483"))
                                  #(ribcage
                                    #(k filename)
                                    #((top) (top))
                                    #("i4479" "i4480"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i4476")))
                                 (hygiene guile))
                              #{fn\ 4488}#))))))
              #{tmp\ 4478}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp\ 4477}#)))))))

(define unquote
  (make-syntax-transformer
    'unquote
    'macro
    (lambda (#{x\ 4493}#)
      (let ((#{tmp\ 4495}# #{x\ 4493}#))
        (let ((#{tmp\ 4496}#
                ($sc-dispatch #{tmp\ 4495}# (quote (any any)))))
          (if #{tmp\ 4496}#
            (@apply
              (lambda (#{_\ 4499}# #{e\ 4500}#)
                (syntax-violation
                  'unquote
                  "expression not valid outside of quasiquote"
                  #{x\ 4493}#))
              #{tmp\ 4496}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp\ 4495}#)))))))

(define unquote-splicing
  (make-syntax-transformer
    'unquote-splicing
    'macro
    (lambda (#{x\ 4501}#)
      (let ((#{tmp\ 4503}# #{x\ 4501}#))
        (let ((#{tmp\ 4504}#
                ($sc-dispatch #{tmp\ 4503}# (quote (any any)))))
          (if #{tmp\ 4504}#
            (@apply
              (lambda (#{_\ 4507}# #{e\ 4508}#)
                (syntax-violation
                  'unquote-splicing
                  "expression not valid outside of quasiquote"
                  #{x\ 4501}#))
              #{tmp\ 4504}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp\ 4503}#)))))))

(define case
  (make-syntax-transformer
    'case
    'macro
    (lambda (#{x\ 4509}#)
      (let ((#{tmp\ 4511}# #{x\ 4509}#))
        (let ((#{tmp\ 4512}#
                ($sc-dispatch
                  #{tmp\ 4511}#
                  '(any any any . each-any))))
          (if #{tmp\ 4512}#
            (@apply
              (lambda (#{_\ 4517}#
                       #{e\ 4518}#
                       #{m1\ 4519}#
                       #{m2\ 4520}#)
                (let ((#{tmp\ 4522}#
                        (letrec*
                          ((#{f\ 4528}#
                             (lambda (#{clause\ 4529}# #{clauses\ 4530}#)
                               (if (null? #{clauses\ 4530}#)
                                 (let ((#{tmp\ 4532}# #{clause\ 4529}#))
                                   (let ((#{tmp\ 4533}#
                                           ($sc-dispatch
                                             #{tmp\ 4532}#
                                             '(#(free-id
                                                 #(syntax-object
                                                   else
                                                   ((top)
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(f clause clauses)
                                                      #((top) (top) (top))
                                                      #("i4525"
                                                        "i4526"
                                                        "i4527"))
                                                    #(ribcage
                                                      #(_ e m1 m2)
                                                      #((top)
                                                        (top)
                                                        (top)
                                                        (top))
                                                      #("i4513"
                                                        "i4514"
                                                        "i4515"
                                                        "i4516"))
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(x)
                                                      #((top))
                                                      #("i4510")))
                                                   (hygiene guile)))
                                               any
                                               .
                                               each-any))))
                                     (if #{tmp\ 4533}#
                                       (@apply
                                         (lambda (#{e1\ 4536}# #{e2\ 4537}#)
                                           (cons '#(syntax-object
                                                    begin
                                                    ((top)
                                                     #(ribcage
                                                       #(e1 e2)
                                                       #((top) (top))
                                                       #("i4534" "i4535"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(f clause clauses)
                                                       #((top) (top) (top))
                                                       #("i4525"
                                                         "i4526"
                                                         "i4527"))
                                                     #(ribcage
                                                       #(_ e m1 m2)
                                                       #((top)
                                                         (top)
                                                         (top)
                                                         (top))
                                                       #("i4513"
                                                         "i4514"
                                                         "i4515"
                                                         "i4516"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(x)
                                                       #((top))
                                                       #("i4510")))
                                                    (hygiene guile))
                                                 (cons #{e1\ 4536}#
                                                       #{e2\ 4537}#)))
                                         #{tmp\ 4533}#)
                                       (let ((#{tmp\ 4539}#
                                               ($sc-dispatch
                                                 #{tmp\ 4532}#
                                                 '(each-any any . each-any))))
                                         (if #{tmp\ 4539}#
                                           (@apply
                                             (lambda (#{k\ 4543}#
                                                      #{e1\ 4544}#
                                                      #{e2\ 4545}#)
                                               (list '#(syntax-object
                                                        if
                                                        ((top)
                                                         #(ribcage
                                                           #(k e1 e2)
                                                           #((top) (top) (top))
                                                           #("i4540"
                                                             "i4541"
                                                             "i4542"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(f clause clauses)
                                                           #((top) (top) (top))
                                                           #("i4525"
                                                             "i4526"
                                                             "i4527"))
                                                         #(ribcage
                                                           #(_ e m1 m2)
                                                           #((top)
                                                             (top)
                                                             (top)
                                                             (top))
                                                           #("i4513"
                                                             "i4514"
                                                             "i4515"
                                                             "i4516"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(x)
                                                           #((top))
                                                           #("i4510")))
                                                        (hygiene guile))
                                                     (list '#(syntax-object
                                                              memv
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4540"
                                                                   "i4541"
                                                                   "i4542"))
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
                                                                 #("i4525"
                                                                   "i4526"
                                                                   "i4527"))
                                                               #(ribcage
                                                                 #(_ e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4513"
                                                                   "i4514"
                                                                   "i4515"
                                                                   "i4516"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4510")))
                                                              (hygiene guile))
                                                           '#(syntax-object
                                                              t
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4540"
                                                                   "i4541"
                                                                   "i4542"))
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
                                                                 #("i4525"
                                                                   "i4526"
                                                                   "i4527"))
                                                               #(ribcage
                                                                 #(_ e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4513"
                                                                   "i4514"
                                                                   "i4515"
                                                                   "i4516"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4510")))
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
                                                                       #("i4540"
                                                                         "i4541"
                                                                         "i4542"))
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
                                                                       #("i4525"
                                                                         "i4526"
                                                                         "i4527"))
                                                                     #(ribcage
                                                                       #(_
                                                                         e
                                                                         m1
                                                                         m2)
                                                                       #((top)
                                                                         (top)
                                                                         (top)
                                                                         (top))
                                                                       #("i4513"
                                                                         "i4514"
                                                                         "i4515"
                                                                         "i4516"))
                                                                     #(ribcage
                                                                       ()
                                                                       ()
                                                                       ())
                                                                     #(ribcage
                                                                       #(x)
                                                                       #((top))
                                                                       #("i4510")))
                                                                    (hygiene
                                                                      guile))
                                                                 #{k\ 4543}#))
                                                     (cons '#(syntax-object
                                                              begin
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4540"
                                                                   "i4541"
                                                                   "i4542"))
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
                                                                 #("i4525"
                                                                   "i4526"
                                                                   "i4527"))
                                                               #(ribcage
                                                                 #(_ e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4513"
                                                                   "i4514"
                                                                   "i4515"
                                                                   "i4516"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4510")))
                                                              (hygiene guile))
                                                           (cons #{e1\ 4544}#
                                                                 #{e2\ 4545}#))))
                                             #{tmp\ 4539}#)
                                           (let ((#{_\ 4549}# #{tmp\ 4532}#))
                                             (syntax-violation
                                               'case
                                               "bad clause"
                                               #{x\ 4509}#
                                               #{clause\ 4529}#)))))))
                                 (let ((#{tmp\ 4551}#
                                         (#{f\ 4528}#
                                           (car #{clauses\ 4530}#)
                                           (cdr #{clauses\ 4530}#))))
                                   (let ((#{rest\ 4553}# #{tmp\ 4551}#))
                                     (let ((#{tmp\ 4554}# #{clause\ 4529}#))
                                       (let ((#{tmp\ 4555}#
                                               ($sc-dispatch
                                                 #{tmp\ 4554}#
                                                 '(each-any any . each-any))))
                                         (if #{tmp\ 4555}#
                                           (@apply
                                             (lambda (#{k\ 4559}#
                                                      #{e1\ 4560}#
                                                      #{e2\ 4561}#)
                                               (list '#(syntax-object
                                                        if
                                                        ((top)
                                                         #(ribcage
                                                           #(k e1 e2)
                                                           #((top) (top) (top))
                                                           #("i4556"
                                                             "i4557"
                                                             "i4558"))
                                                         #(ribcage
                                                           #(rest)
                                                           #((top))
                                                           #("i4552"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(f clause clauses)
                                                           #((top) (top) (top))
                                                           #("i4525"
                                                             "i4526"
                                                             "i4527"))
                                                         #(ribcage
                                                           #(_ e m1 m2)
                                                           #((top)
                                                             (top)
                                                             (top)
                                                             (top))
                                                           #("i4513"
                                                             "i4514"
                                                             "i4515"
                                                             "i4516"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(x)
                                                           #((top))
                                                           #("i4510")))
                                                        (hygiene guile))
                                                     (list '#(syntax-object
                                                              memv
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4556"
                                                                   "i4557"
                                                                   "i4558"))
                                                               #(ribcage
                                                                 #(rest)
                                                                 #((top))
                                                                 #("i4552"))
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
                                                                 #("i4525"
                                                                   "i4526"
                                                                   "i4527"))
                                                               #(ribcage
                                                                 #(_ e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4513"
                                                                   "i4514"
                                                                   "i4515"
                                                                   "i4516"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4510")))
                                                              (hygiene guile))
                                                           '#(syntax-object
                                                              t
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4556"
                                                                   "i4557"
                                                                   "i4558"))
                                                               #(ribcage
                                                                 #(rest)
                                                                 #((top))
                                                                 #("i4552"))
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
                                                                 #("i4525"
                                                                   "i4526"
                                                                   "i4527"))
                                                               #(ribcage
                                                                 #(_ e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4513"
                                                                   "i4514"
                                                                   "i4515"
                                                                   "i4516"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4510")))
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
                                                                       #("i4556"
                                                                         "i4557"
                                                                         "i4558"))
                                                                     #(ribcage
                                                                       #(rest)
                                                                       #((top))
                                                                       #("i4552"))
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
                                                                       #("i4525"
                                                                         "i4526"
                                                                         "i4527"))
                                                                     #(ribcage
                                                                       #(_
                                                                         e
                                                                         m1
                                                                         m2)
                                                                       #((top)
                                                                         (top)
                                                                         (top)
                                                                         (top))
                                                                       #("i4513"
                                                                         "i4514"
                                                                         "i4515"
                                                                         "i4516"))
                                                                     #(ribcage
                                                                       ()
                                                                       ()
                                                                       ())
                                                                     #(ribcage
                                                                       #(x)
                                                                       #((top))
                                                                       #("i4510")))
                                                                    (hygiene
                                                                      guile))
                                                                 #{k\ 4559}#))
                                                     (cons '#(syntax-object
                                                              begin
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4556"
                                                                   "i4557"
                                                                   "i4558"))
                                                               #(ribcage
                                                                 #(rest)
                                                                 #((top))
                                                                 #("i4552"))
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
                                                                 #("i4525"
                                                                   "i4526"
                                                                   "i4527"))
                                                               #(ribcage
                                                                 #(_ e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4513"
                                                                   "i4514"
                                                                   "i4515"
                                                                   "i4516"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4510")))
                                                              (hygiene guile))
                                                           (cons #{e1\ 4560}#
                                                                 #{e2\ 4561}#))
                                                     #{rest\ 4553}#))
                                             #{tmp\ 4555}#)
                                           (let ((#{_\ 4565}# #{tmp\ 4554}#))
                                             (syntax-violation
                                               'case
                                               "bad clause"
                                               #{x\ 4509}#
                                               #{clause\ 4529}#)))))))))))
                          (begin (#{f\ 4528}# #{m1\ 4519}# #{m2\ 4520}#)))))
                  (let ((#{body\ 4524}# #{tmp\ 4522}#))
                    (list '#(syntax-object
                             let
                             ((top)
                              #(ribcage #(body) #((top)) #("i4523"))
                              #(ribcage
                                #(_ e m1 m2)
                                #((top) (top) (top) (top))
                                #("i4513" "i4514" "i4515" "i4516"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4510")))
                             (hygiene guile))
                          (list (list '#(syntax-object
                                         t
                                         ((top)
                                          #(ribcage
                                            #(body)
                                            #((top))
                                            #("i4523"))
                                          #(ribcage
                                            #(_ e m1 m2)
                                            #((top) (top) (top) (top))
                                            #("i4513" "i4514" "i4515" "i4516"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i4510")))
                                         (hygiene guile))
                                      #{e\ 4518}#))
                          #{body\ 4524}#))))
              #{tmp\ 4512}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp\ 4511}#)))))))

(define identifier-syntax
  (make-syntax-transformer
    'identifier-syntax
    'macro
    (lambda (#{x\ 4566}#)
      (let ((#{tmp\ 4568}# #{x\ 4566}#))
        (let ((#{tmp\ 4569}#
                ($sc-dispatch #{tmp\ 4568}# (quote (any any)))))
          (if #{tmp\ 4569}#
            (@apply
              (lambda (#{_\ 4572}# #{e\ 4573}#)
                (list '#(syntax-object
                         lambda
                         ((top)
                          #(ribcage
                            #(_ e)
                            #((top) (top))
                            #("i4570" "i4571"))
                          #(ribcage () () ())
                          #(ribcage #(x) #((top)) #("i4567")))
                         (hygiene guile))
                      '(#(syntax-object
                          x
                          ((top)
                           #(ribcage
                             #(_ e)
                             #((top) (top))
                             #("i4570" "i4571"))
                           #(ribcage () () ())
                           #(ribcage #(x) #((top)) #("i4567")))
                          (hygiene guile)))
                      '#((#(syntax-object
                            macro-type
                            ((top)
                             #(ribcage
                               #(_ e)
                               #((top) (top))
                               #("i4570" "i4571"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i4567")))
                            (hygiene guile))
                          .
                          #(syntax-object
                            identifier-syntax
                            ((top)
                             #(ribcage
                               #(_ e)
                               #((top) (top))
                               #("i4570" "i4571"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i4567")))
                            (hygiene guile))))
                      (list '#(syntax-object
                               syntax-case
                               ((top)
                                #(ribcage
                                  #(_ e)
                                  #((top) (top))
                                  #("i4570" "i4571"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i4567")))
                               (hygiene guile))
                            '#(syntax-object
                               x
                               ((top)
                                #(ribcage
                                  #(_ e)
                                  #((top) (top))
                                  #("i4570" "i4571"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i4567")))
                               (hygiene guile))
                            '()
                            (list '#(syntax-object
                                     id
                                     ((top)
                                      #(ribcage
                                        #(_ e)
                                        #((top) (top))
                                        #("i4570" "i4571"))
                                      #(ribcage () () ())
                                      #(ribcage #(x) #((top)) #("i4567")))
                                     (hygiene guile))
                                  '(#(syntax-object
                                      identifier?
                                      ((top)
                                       #(ribcage
                                         #(_ e)
                                         #((top) (top))
                                         #("i4570" "i4571"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4567")))
                                      (hygiene guile))
                                    (#(syntax-object
                                       syntax
                                       ((top)
                                        #(ribcage
                                          #(_ e)
                                          #((top) (top))
                                          #("i4570" "i4571"))
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i4567")))
                                       (hygiene guile))
                                     #(syntax-object
                                       id
                                       ((top)
                                        #(ribcage
                                          #(_ e)
                                          #((top) (top))
                                          #("i4570" "i4571"))
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i4567")))
                                       (hygiene guile))))
                                  (list '#(syntax-object
                                           syntax
                                           ((top)
                                            #(ribcage
                                              #(_ e)
                                              #((top) (top))
                                              #("i4570" "i4571"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(x)
                                              #((top))
                                              #("i4567")))
                                           (hygiene guile))
                                        #{e\ 4573}#))
                            (list (cons #{_\ 4572}#
                                        '(#(syntax-object
                                            x
                                            ((top)
                                             #(ribcage
                                               #(_ e)
                                               #((top) (top))
                                               #("i4570" "i4571"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x)
                                               #((top))
                                               #("i4567")))
                                            (hygiene guile))
                                          #(syntax-object
                                            ...
                                            ((top)
                                             #(ribcage
                                               #(_ e)
                                               #((top) (top))
                                               #("i4570" "i4571"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x)
                                               #((top))
                                               #("i4567")))
                                            (hygiene guile))))
                                  (list '#(syntax-object
                                           syntax
                                           ((top)
                                            #(ribcage
                                              #(_ e)
                                              #((top) (top))
                                              #("i4570" "i4571"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(x)
                                              #((top))
                                              #("i4567")))
                                           (hygiene guile))
                                        (cons #{e\ 4573}#
                                              '(#(syntax-object
                                                  x
                                                  ((top)
                                                   #(ribcage
                                                     #(_ e)
                                                     #((top) (top))
                                                     #("i4570" "i4571"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x)
                                                     #((top))
                                                     #("i4567")))
                                                  (hygiene guile))
                                                #(syntax-object
                                                  ...
                                                  ((top)
                                                   #(ribcage
                                                     #(_ e)
                                                     #((top) (top))
                                                     #("i4570" "i4571"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x)
                                                     #((top))
                                                     #("i4567")))
                                                  (hygiene guile)))))))))
              #{tmp\ 4569}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp\ 4568}#)))))))

(define define*
  (make-syntax-transformer
    'define*
    'macro
    (lambda (#{x\ 4574}#)
      (let ((#{tmp\ 4576}# #{x\ 4574}#))
        (let ((#{tmp\ 4577}#
                ($sc-dispatch
                  #{tmp\ 4576}#
                  '(any (any . any) any . each-any))))
          (if #{tmp\ 4577}#
            (@apply
              (lambda (#{_\ 4583}#
                       #{id\ 4584}#
                       #{args\ 4585}#
                       #{b0\ 4586}#
                       #{b1\ 4587}#)
                (list '#(syntax-object
                         define
                         ((top)
                          #(ribcage
                            #(_ id args b0 b1)
                            #((top) (top) (top) (top) (top))
                            #("i4578" "i4579" "i4580" "i4581" "i4582"))
                          #(ribcage () () ())
                          #(ribcage #(x) #((top)) #("i4575")))
                         (hygiene guile))
                      #{id\ 4584}#
                      (cons '#(syntax-object
                               lambda*
                               ((top)
                                #(ribcage
                                  #(_ id args b0 b1)
                                  #((top) (top) (top) (top) (top))
                                  #("i4578" "i4579" "i4580" "i4581" "i4582"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i4575")))
                               (hygiene guile))
                            (cons #{args\ 4585}#
                                  (cons #{b0\ 4586}# #{b1\ 4587}#)))))
              #{tmp\ 4577}#)
            (let ((#{tmp\ 4589}#
                    ($sc-dispatch
                      #{tmp\ 4576}#
                      '(any any any))))
              (if (if #{tmp\ 4589}#
                    (@apply
                      (lambda (#{_\ 4593}# #{id\ 4594}# #{val\ 4595}#)
                        (identifier?
                          '#(syntax-object
                             x
                             ((top)
                              #(ribcage
                                #(_ id val)
                                #((top) (top) (top))
                                #("i4590" "i4591" "i4592"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4575")))
                             (hygiene guile))))
                      #{tmp\ 4589}#)
                    #f)
                (@apply
                  (lambda (#{_\ 4599}# #{id\ 4600}# #{val\ 4601}#)
                    (list '#(syntax-object
                             define
                             ((top)
                              #(ribcage
                                #(_ id val)
                                #((top) (top) (top))
                                #("i4596" "i4597" "i4598"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4575")))
                             (hygiene guile))
                          #{id\ 4600}#
                          #{val\ 4601}#))
                  #{tmp\ 4589}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 4576}#)))))))))

