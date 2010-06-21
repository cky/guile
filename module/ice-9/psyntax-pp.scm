(eval-when (compile) (set-current-module (resolve-module (quote (guile)))))
(if #f #f)

(letrec*
  ((#{and-map*\ 36}#
     (lambda (#{f\ 208}# #{first\ 209}# . #{rest\ 210}#)
       (begin
         (let ((#{t\ 216}# (null? #{first\ 209}#)))
           (if #{t\ 216}#
             #{t\ 216}#
             (if (null? #{rest\ 210}#)
               (letrec*
                 ((#{andmap\ 220}#
                    (lambda (#{first\ 221}#)
                      (begin
                        (let ((#{x\ 224}# (car #{first\ 221}#))
                              (#{first\ 225}# (cdr #{first\ 221}#)))
                          (if (null? #{first\ 225}#)
                            (#{f\ 208}# #{x\ 224}#)
                            (if (#{f\ 208}# #{x\ 224}#)
                              (#{andmap\ 220}# #{first\ 225}#)
                              #f)))))))
                 (begin (#{andmap\ 220}# #{first\ 209}#)))
               (letrec*
                 ((#{andmap\ 231}#
                    (lambda (#{first\ 232}# #{rest\ 233}#)
                      (begin
                        (let ((#{x\ 238}# (car #{first\ 232}#))
                              (#{xr\ 239}# (map car #{rest\ 233}#))
                              (#{first\ 240}# (cdr #{first\ 232}#))
                              (#{rest\ 241}# (map cdr #{rest\ 233}#)))
                          (if (null? #{first\ 240}#)
                            (@apply #{f\ 208}# (cons #{x\ 238}# #{xr\ 239}#))
                            (if (@apply
                                  #{f\ 208}#
                                  (cons #{x\ 238}# #{xr\ 239}#))
                              (#{andmap\ 231}# #{first\ 240}# #{rest\ 241}#)
                              #f)))))))
                 (begin
                   (#{andmap\ 231}# #{first\ 209}# #{rest\ 210}#))))))))))
  (begin
    (let ((#{make-primitive-ref\ 250}# (if #f #f))
          (#{fx+\ 289}# (if #f #f))
          (#{fx-\ 291}# (if #f #f))
          (#{fx=\ 293}# (if #f #f))
          (#{fx<\ 295}# (if #f #f))
          (#{set-syntax-object-expression!\ 360}#
            (if #f #f))
          (#{set-syntax-object-wrap!\ 362}# (if #f #f))
          (#{set-syntax-object-module!\ 364}# (if #f #f))
          (#{binding-type\ 371}# (if #f #f))
          (#{binding-value\ 373}# (if #f #f))
          (#{make-wrap\ 393}# (if #f #f))
          (#{wrap-marks\ 395}# (if #f #f))
          (#{wrap-subst\ 397}# (if #f #f))
          (#{ribcage?\ 411}# (if #f #f)))
      (letrec*
        ((#{make-void\ 246}#
           (lambda (#{src\ 718}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 0)
               #{src\ 718}#)))
         (#{make-const\ 248}#
           (lambda (#{src\ 720}# #{exp\ 721}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 1)
               #{src\ 720}#
               #{exp\ 721}#)))
         (#{make-lexical-ref\ 252}#
           (lambda (#{src\ 728}# #{name\ 729}# #{gensym\ 730}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 3)
               #{src\ 728}#
               #{name\ 729}#
               #{gensym\ 730}#)))
         (#{make-lexical-set\ 254}#
           (lambda (#{src\ 734}#
                    #{name\ 735}#
                    #{gensym\ 736}#
                    #{exp\ 737}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 4)
               #{src\ 734}#
               #{name\ 735}#
               #{gensym\ 736}#
               #{exp\ 737}#)))
         (#{make-module-ref\ 256}#
           (lambda (#{src\ 742}#
                    #{mod\ 743}#
                    #{name\ 744}#
                    #{public?\ 745}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 5)
               #{src\ 742}#
               #{mod\ 743}#
               #{name\ 744}#
               #{public?\ 745}#)))
         (#{make-module-set\ 258}#
           (lambda (#{src\ 750}#
                    #{mod\ 751}#
                    #{name\ 752}#
                    #{public?\ 753}#
                    #{exp\ 754}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 6)
               #{src\ 750}#
               #{mod\ 751}#
               #{name\ 752}#
               #{public?\ 753}#
               #{exp\ 754}#)))
         (#{make-toplevel-ref\ 260}#
           (lambda (#{src\ 760}# #{name\ 761}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 7)
               #{src\ 760}#
               #{name\ 761}#)))
         (#{make-toplevel-set\ 262}#
           (lambda (#{src\ 764}# #{name\ 765}# #{exp\ 766}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 8)
               #{src\ 764}#
               #{name\ 765}#
               #{exp\ 766}#)))
         (#{make-toplevel-define\ 264}#
           (lambda (#{src\ 770}# #{name\ 771}# #{exp\ 772}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 9)
               #{src\ 770}#
               #{name\ 771}#
               #{exp\ 772}#)))
         (#{make-conditional\ 266}#
           (lambda (#{src\ 776}#
                    #{test\ 777}#
                    #{consequent\ 778}#
                    #{alternate\ 779}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 10)
               #{src\ 776}#
               #{test\ 777}#
               #{consequent\ 778}#
               #{alternate\ 779}#)))
         (#{make-application\ 268}#
           (lambda (#{src\ 784}# #{proc\ 785}# #{args\ 786}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 11)
               #{src\ 784}#
               #{proc\ 785}#
               #{args\ 786}#)))
         (#{make-sequence\ 270}#
           (lambda (#{src\ 790}# #{exps\ 791}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 12)
               #{src\ 790}#
               #{exps\ 791}#)))
         (#{make-lambda\ 272}#
           (lambda (#{src\ 794}# #{meta\ 795}# #{body\ 796}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 13)
               #{src\ 794}#
               #{meta\ 795}#
               #{body\ 796}#)))
         (#{make-lambda-case\ 274}#
           (lambda (#{src\ 800}#
                    #{req\ 801}#
                    #{opt\ 802}#
                    #{rest\ 803}#
                    #{kw\ 804}#
                    #{inits\ 805}#
                    #{gensyms\ 806}#
                    #{body\ 807}#
                    #{alternate\ 808}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 14)
               #{src\ 800}#
               #{req\ 801}#
               #{opt\ 802}#
               #{rest\ 803}#
               #{kw\ 804}#
               #{inits\ 805}#
               #{gensyms\ 806}#
               #{body\ 807}#
               #{alternate\ 808}#)))
         (#{make-let\ 276}#
           (lambda (#{src\ 818}#
                    #{names\ 819}#
                    #{gensyms\ 820}#
                    #{vals\ 821}#
                    #{body\ 822}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 15)
               #{src\ 818}#
               #{names\ 819}#
               #{gensyms\ 820}#
               #{vals\ 821}#
               #{body\ 822}#)))
         (#{make-letrec\ 278}#
           (lambda (#{src\ 828}#
                    #{in-order?\ 829}#
                    #{names\ 830}#
                    #{gensyms\ 831}#
                    #{vals\ 832}#
                    #{body\ 833}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 16)
               #{src\ 828}#
               #{in-order?\ 829}#
               #{names\ 830}#
               #{gensyms\ 831}#
               #{vals\ 832}#
               #{body\ 833}#)))
         (#{make-dynlet\ 280}#
           (lambda (#{src\ 840}#
                    #{fluids\ 841}#
                    #{vals\ 842}#
                    #{body\ 843}#)
             (make-struct/no-tail
               (vector-ref %expanded-vtables 17)
               #{src\ 840}#
               #{fluids\ 841}#
               #{vals\ 842}#
               #{body\ 843}#)))
         (#{lambda?\ 283}#
           (lambda (#{x\ 848}#)
             (if (struct? #{x\ 848}#)
               (eq? (struct-vtable #{x\ 848}#)
                    (vector-ref %expanded-vtables 13))
               #f)))
         (#{lambda-meta\ 285}#
           (lambda (#{x\ 852}#) (struct-ref #{x\ 852}# 1)))
         (#{set-lambda-meta!\ 287}#
           (lambda (#{x\ 854}# #{v\ 855}#)
             (struct-set! #{x\ 854}# 1 #{v\ 855}#)))
         (#{top-level-eval-hook\ 297}#
           (lambda (#{x\ 858}# #{mod\ 859}#)
             (primitive-eval #{x\ 858}#)))
         (#{local-eval-hook\ 299}#
           (lambda (#{x\ 862}# #{mod\ 863}#)
             (primitive-eval #{x\ 862}#)))
         (#{put-global-definition-hook\ 302}#
           (lambda (#{symbol\ 866}# #{type\ 867}# #{val\ 868}#)
             (module-define!
               (current-module)
               #{symbol\ 866}#
               (make-syntax-transformer
                 #{symbol\ 866}#
                 #{type\ 867}#
                 #{val\ 868}#))))
         (#{get-global-definition-hook\ 304}#
           (lambda (#{symbol\ 872}# #{module\ 873}#)
             (begin
               (if (if (not #{module\ 873}#) (current-module) #f)
                 (warn "module system is booted, we should have a module"
                       #{symbol\ 872}#))
               (begin
                 (let ((#{v\ 879}# (module-variable
                                     (if #{module\ 873}#
                                       (resolve-module (cdr #{module\ 873}#))
                                       (current-module))
                                     #{symbol\ 872}#)))
                   (if #{v\ 879}#
                     (if (variable-bound? #{v\ 879}#)
                       (begin
                         (let ((#{val\ 884}# (variable-ref #{v\ 879}#)))
                           (if (macro? #{val\ 884}#)
                             (if (macro-type #{val\ 884}#)
                               (cons (macro-type #{val\ 884}#)
                                     (macro-binding #{val\ 884}#))
                               #f)
                             #f)))
                       #f)
                     #f))))))
         (#{decorate-source\ 306}#
           (lambda (#{e\ 888}# #{s\ 889}#)
             (begin
               (if (if (pair? #{e\ 888}#) #{s\ 889}# #f)
                 (set-source-properties! #{e\ 888}# #{s\ 889}#))
               #{e\ 888}#)))
         (#{maybe-name-value!\ 308}#
           (lambda (#{name\ 894}# #{val\ 895}#)
             (if (#{lambda?\ 283}# #{val\ 895}#)
               (begin
                 (let ((#{meta\ 899}#
                         (#{lambda-meta\ 285}# #{val\ 895}#)))
                   (if (not (assq (quote name) #{meta\ 899}#))
                     (#{set-lambda-meta!\ 287}#
                       #{val\ 895}#
                       (cons (cons (quote name) #{name\ 894}#)
                             #{meta\ 899}#))))))))
         (#{build-void\ 310}#
           (lambda (#{source\ 900}#)
             (#{make-void\ 246}# #{source\ 900}#)))
         (#{build-application\ 312}#
           (lambda (#{source\ 902}#
                    #{fun-exp\ 903}#
                    #{arg-exps\ 904}#)
             (#{make-application\ 268}#
               #{source\ 902}#
               #{fun-exp\ 903}#
               #{arg-exps\ 904}#)))
         (#{build-conditional\ 314}#
           (lambda (#{source\ 908}#
                    #{test-exp\ 909}#
                    #{then-exp\ 910}#
                    #{else-exp\ 911}#)
             (#{make-conditional\ 266}#
               #{source\ 908}#
               #{test-exp\ 909}#
               #{then-exp\ 910}#
               #{else-exp\ 911}#)))
         (#{build-dynlet\ 316}#
           (lambda (#{source\ 916}#
                    #{fluids\ 917}#
                    #{vals\ 918}#
                    #{body\ 919}#)
             (#{make-dynlet\ 280}#
               #{source\ 916}#
               #{fluids\ 917}#
               #{vals\ 918}#
               #{body\ 919}#)))
         (#{build-lexical-reference\ 318}#
           (lambda (#{type\ 924}#
                    #{source\ 925}#
                    #{name\ 926}#
                    #{var\ 927}#)
             (#{make-lexical-ref\ 252}#
               #{source\ 925}#
               #{name\ 926}#
               #{var\ 927}#)))
         (#{build-lexical-assignment\ 320}#
           (lambda (#{source\ 932}#
                    #{name\ 933}#
                    #{var\ 934}#
                    #{exp\ 935}#)
             (begin
               (#{maybe-name-value!\ 308}#
                 #{name\ 933}#
                 #{exp\ 935}#)
               (#{make-lexical-set\ 254}#
                 #{source\ 932}#
                 #{name\ 933}#
                 #{var\ 934}#
                 #{exp\ 935}#))))
         (#{analyze-variable\ 322}#
           (lambda (#{mod\ 940}#
                    #{var\ 941}#
                    #{modref-cont\ 942}#
                    #{bare-cont\ 943}#)
             (if (not #{mod\ 940}#)
               (#{bare-cont\ 943}# #{var\ 941}#)
               (begin
                 (let ((#{kind\ 950}# (car #{mod\ 940}#))
                       (#{mod\ 951}# (cdr #{mod\ 940}#)))
                   (if (eqv? #{kind\ 950}# (quote public))
                     (#{modref-cont\ 942}#
                       #{mod\ 951}#
                       #{var\ 941}#
                       #t)
                     (if (eqv? #{kind\ 950}# (quote private))
                       (if (not (equal?
                                  #{mod\ 951}#
                                  (module-name (current-module))))
                         (#{modref-cont\ 942}#
                           #{mod\ 951}#
                           #{var\ 941}#
                           #f)
                         (#{bare-cont\ 943}# #{var\ 941}#))
                       (if (eqv? #{kind\ 950}# (quote bare))
                         (#{bare-cont\ 943}# #{var\ 941}#)
                         (if (eqv? #{kind\ 950}# (quote hygiene))
                           (if (if (not (equal?
                                          #{mod\ 951}#
                                          (module-name (current-module))))
                                 (module-variable
                                   (resolve-module #{mod\ 951}#)
                                   #{var\ 941}#)
                                 #f)
                             (#{modref-cont\ 942}#
                               #{mod\ 951}#
                               #{var\ 941}#
                               #f)
                             (#{bare-cont\ 943}# #{var\ 941}#))
                           (syntax-violation
                             #f
                             "bad module kind"
                             #{var\ 941}#
                             #{mod\ 951}#))))))))))
         (#{build-global-reference\ 324}#
           (lambda (#{source\ 959}# #{var\ 960}# #{mod\ 961}#)
             (#{analyze-variable\ 322}#
               #{mod\ 961}#
               #{var\ 960}#
               (lambda (#{mod\ 965}# #{var\ 966}# #{public?\ 967}#)
                 (#{make-module-ref\ 256}#
                   #{source\ 959}#
                   #{mod\ 965}#
                   #{var\ 966}#
                   #{public?\ 967}#))
               (lambda (#{var\ 971}#)
                 (#{make-toplevel-ref\ 260}#
                   #{source\ 959}#
                   #{var\ 971}#)))))
         (#{build-global-assignment\ 326}#
           (lambda (#{source\ 973}#
                    #{var\ 974}#
                    #{exp\ 975}#
                    #{mod\ 976}#)
             (begin
               (#{maybe-name-value!\ 308}#
                 #{var\ 974}#
                 #{exp\ 975}#)
               (#{analyze-variable\ 322}#
                 #{mod\ 976}#
                 #{var\ 974}#
                 (lambda (#{mod\ 981}# #{var\ 982}# #{public?\ 983}#)
                   (#{make-module-set\ 258}#
                     #{source\ 973}#
                     #{mod\ 981}#
                     #{var\ 982}#
                     #{public?\ 983}#
                     #{exp\ 975}#))
                 (lambda (#{var\ 987}#)
                   (#{make-toplevel-set\ 262}#
                     #{source\ 973}#
                     #{var\ 987}#
                     #{exp\ 975}#))))))
         (#{build-global-definition\ 328}#
           (lambda (#{source\ 989}# #{var\ 990}# #{exp\ 991}#)
             (begin
               (#{maybe-name-value!\ 308}#
                 #{var\ 990}#
                 #{exp\ 991}#)
               (#{make-toplevel-define\ 264}#
                 #{source\ 989}#
                 #{var\ 990}#
                 #{exp\ 991}#))))
         (#{build-simple-lambda\ 330}#
           (lambda (#{src\ 995}#
                    #{req\ 996}#
                    #{rest\ 997}#
                    #{vars\ 998}#
                    #{meta\ 999}#
                    #{exp\ 1000}#)
             (#{make-lambda\ 272}#
               #{src\ 995}#
               #{meta\ 999}#
               (#{make-lambda-case\ 274}#
                 #{src\ 995}#
                 #{req\ 996}#
                 #f
                 #{rest\ 997}#
                 #f
                 '()
                 #{vars\ 998}#
                 #{exp\ 1000}#
                 #f))))
         (#{build-case-lambda\ 332}#
           (lambda (#{src\ 1007}# #{meta\ 1008}# #{body\ 1009}#)
             (#{make-lambda\ 272}#
               #{src\ 1007}#
               #{meta\ 1008}#
               #{body\ 1009}#)))
         (#{build-lambda-case\ 334}#
           (lambda (#{src\ 1013}#
                    #{req\ 1014}#
                    #{opt\ 1015}#
                    #{rest\ 1016}#
                    #{kw\ 1017}#
                    #{inits\ 1018}#
                    #{vars\ 1019}#
                    #{body\ 1020}#
                    #{else-case\ 1021}#)
             (#{make-lambda-case\ 274}#
               #{src\ 1013}#
               #{req\ 1014}#
               #{opt\ 1015}#
               #{rest\ 1016}#
               #{kw\ 1017}#
               #{inits\ 1018}#
               #{vars\ 1019}#
               #{body\ 1020}#
               #{else-case\ 1021}#)))
         (#{build-primref\ 336}#
           (lambda (#{src\ 1031}# #{name\ 1032}#)
             (if (equal?
                   (module-name (current-module))
                   '(guile))
               (#{make-toplevel-ref\ 260}#
                 #{src\ 1031}#
                 #{name\ 1032}#)
               (#{make-module-ref\ 256}#
                 #{src\ 1031}#
                 '(guile)
                 #{name\ 1032}#
                 #f))))
         (#{build-data\ 338}#
           (lambda (#{src\ 1035}# #{exp\ 1036}#)
             (#{make-const\ 248}# #{src\ 1035}# #{exp\ 1036}#)))
         (#{build-sequence\ 340}#
           (lambda (#{src\ 1039}# #{exps\ 1040}#)
             (if (null? (cdr #{exps\ 1040}#))
               (car #{exps\ 1040}#)
               (#{make-sequence\ 270}#
                 #{src\ 1039}#
                 #{exps\ 1040}#))))
         (#{build-let\ 342}#
           (lambda (#{src\ 1043}#
                    #{ids\ 1044}#
                    #{vars\ 1045}#
                    #{val-exps\ 1046}#
                    #{body-exp\ 1047}#)
             (begin
               (for-each
                 #{maybe-name-value!\ 308}#
                 #{ids\ 1044}#
                 #{val-exps\ 1046}#)
               (if (null? #{vars\ 1045}#)
                 #{body-exp\ 1047}#
                 (#{make-let\ 276}#
                   #{src\ 1043}#
                   #{ids\ 1044}#
                   #{vars\ 1045}#
                   #{val-exps\ 1046}#
                   #{body-exp\ 1047}#)))))
         (#{build-named-let\ 344}#
           (lambda (#{src\ 1053}#
                    #{ids\ 1054}#
                    #{vars\ 1055}#
                    #{val-exps\ 1056}#
                    #{body-exp\ 1057}#)
             (begin
               (let ((#{f\ 1067}# (car #{vars\ 1055}#))
                     (#{f-name\ 1068}# (car #{ids\ 1054}#))
                     (#{vars\ 1069}# (cdr #{vars\ 1055}#))
                     (#{ids\ 1070}# (cdr #{ids\ 1054}#)))
                 (begin
                   (let ((#{proc\ 1072}#
                           (#{build-simple-lambda\ 330}#
                             #{src\ 1053}#
                             #{ids\ 1070}#
                             #f
                             #{vars\ 1069}#
                             '()
                             #{body-exp\ 1057}#)))
                     (begin
                       (#{maybe-name-value!\ 308}#
                         #{f-name\ 1068}#
                         #{proc\ 1072}#)
                       (for-each
                         #{maybe-name-value!\ 308}#
                         #{ids\ 1070}#
                         #{val-exps\ 1056}#)
                       (#{make-letrec\ 278}#
                         #{src\ 1053}#
                         #f
                         (list #{f-name\ 1068}#)
                         (list #{f\ 1067}#)
                         (list #{proc\ 1072}#)
                         (#{build-application\ 312}#
                           #{src\ 1053}#
                           (#{build-lexical-reference\ 318}#
                             'fun
                             #{src\ 1053}#
                             #{f-name\ 1068}#
                             #{f\ 1067}#)
                           #{val-exps\ 1056}#)))))))))
         (#{build-letrec\ 346}#
           (lambda (#{src\ 1073}#
                    #{in-order?\ 1074}#
                    #{ids\ 1075}#
                    #{vars\ 1076}#
                    #{val-exps\ 1077}#
                    #{body-exp\ 1078}#)
             (if (null? #{vars\ 1076}#)
               #{body-exp\ 1078}#
               (begin
                 (for-each
                   #{maybe-name-value!\ 308}#
                   #{ids\ 1075}#
                   #{val-exps\ 1077}#)
                 (#{make-letrec\ 278}#
                   #{src\ 1073}#
                   #{in-order?\ 1074}#
                   #{ids\ 1075}#
                   #{vars\ 1076}#
                   #{val-exps\ 1077}#
                   #{body-exp\ 1078}#)))))
         (#{make-syntax-object\ 350}#
           (lambda (#{expression\ 1085}#
                    #{wrap\ 1086}#
                    #{module\ 1087}#)
             (vector
               'syntax-object
               #{expression\ 1085}#
               #{wrap\ 1086}#
               #{module\ 1087}#)))
         (#{syntax-object?\ 352}#
           (lambda (#{x\ 1091}#)
             (if (vector? #{x\ 1091}#)
               (if (= (vector-length #{x\ 1091}#) 4)
                 (eq? (vector-ref #{x\ 1091}# 0)
                      'syntax-object)
                 #f)
               #f)))
         (#{syntax-object-expression\ 354}#
           (lambda (#{x\ 1096}#) (vector-ref #{x\ 1096}# 1)))
         (#{syntax-object-wrap\ 356}#
           (lambda (#{x\ 1098}#) (vector-ref #{x\ 1098}# 2)))
         (#{syntax-object-module\ 358}#
           (lambda (#{x\ 1100}#) (vector-ref #{x\ 1100}# 3)))
         (#{source-annotation\ 367}#
           (lambda (#{x\ 1114}#)
             (if (#{syntax-object?\ 352}# #{x\ 1114}#)
               (#{source-annotation\ 367}#
                 (#{syntax-object-expression\ 354}# #{x\ 1114}#))
               (if (pair? #{x\ 1114}#)
                 (begin
                   (let ((#{props\ 1121}# (source-properties #{x\ 1114}#)))
                     (if (pair? #{props\ 1121}#) #{props\ 1121}# #f)))
                 #f))))
         (#{extend-env\ 376}#
           (lambda (#{labels\ 1123}# #{bindings\ 1124}# #{r\ 1125}#)
             (if (null? #{labels\ 1123}#)
               #{r\ 1125}#
               (#{extend-env\ 376}#
                 (cdr #{labels\ 1123}#)
                 (cdr #{bindings\ 1124}#)
                 (cons (cons (car #{labels\ 1123}#)
                             (car #{bindings\ 1124}#))
                       #{r\ 1125}#)))))
         (#{extend-var-env\ 378}#
           (lambda (#{labels\ 1129}# #{vars\ 1130}# #{r\ 1131}#)
             (if (null? #{labels\ 1129}#)
               #{r\ 1131}#
               (#{extend-var-env\ 378}#
                 (cdr #{labels\ 1129}#)
                 (cdr #{vars\ 1130}#)
                 (cons (cons (car #{labels\ 1129}#)
                             (cons (quote lexical) (car #{vars\ 1130}#)))
                       #{r\ 1131}#)))))
         (#{macros-only-env\ 380}#
           (lambda (#{r\ 1136}#)
             (if (null? #{r\ 1136}#)
               '()
               (begin
                 (let ((#{a\ 1139}# (car #{r\ 1136}#)))
                   (if (eq? (car (cdr #{a\ 1139}#)) (quote macro))
                     (cons #{a\ 1139}#
                           (#{macros-only-env\ 380}# (cdr #{r\ 1136}#)))
                     (#{macros-only-env\ 380}# (cdr #{r\ 1136}#))))))))
         (#{lookup\ 382}#
           (lambda (#{x\ 1140}# #{r\ 1141}# #{mod\ 1142}#)
             (begin
               (let ((#{t\ 1148}# (assq #{x\ 1140}# #{r\ 1141}#)))
                 (if #{t\ 1148}#
                   (cdr #{t\ 1148}#)
                   (if (symbol? #{x\ 1140}#)
                     (begin
                       (let ((#{t\ 1154}#
                               (#{get-global-definition-hook\ 304}#
                                 #{x\ 1140}#
                                 #{mod\ 1142}#)))
                         (if #{t\ 1154}# #{t\ 1154}# (quote (global)))))
                     '(displaced-lexical)))))))
         (#{global-extend\ 384}#
           (lambda (#{type\ 1159}# #{sym\ 1160}# #{val\ 1161}#)
             (#{put-global-definition-hook\ 302}#
               #{sym\ 1160}#
               #{type\ 1159}#
               #{val\ 1161}#)))
         (#{nonsymbol-id?\ 386}#
           (lambda (#{x\ 1165}#)
             (if (#{syntax-object?\ 352}# #{x\ 1165}#)
               (symbol?
                 (#{syntax-object-expression\ 354}# #{x\ 1165}#))
               #f)))
         (#{id?\ 388}#
           (lambda (#{x\ 1169}#)
             (if (symbol? #{x\ 1169}#)
               #t
               (if (#{syntax-object?\ 352}# #{x\ 1169}#)
                 (symbol?
                   (#{syntax-object-expression\ 354}# #{x\ 1169}#))
                 #f))))
         (#{id-sym-name&marks\ 391}#
           (lambda (#{x\ 1176}# #{w\ 1177}#)
             (if (#{syntax-object?\ 352}# #{x\ 1176}#)
               (values
                 (#{syntax-object-expression\ 354}# #{x\ 1176}#)
                 (#{join-marks\ 441}#
                   (#{wrap-marks\ 395}# #{w\ 1177}#)
                   (#{wrap-marks\ 395}#
                     (#{syntax-object-wrap\ 356}# #{x\ 1176}#))))
               (values
                 #{x\ 1176}#
                 (#{wrap-marks\ 395}# #{w\ 1177}#)))))
         (#{gen-label\ 404}#
           (lambda () (symbol->string (gensym "i"))))
         (#{gen-labels\ 406}#
           (lambda (#{ls\ 1180}#)
             (if (null? #{ls\ 1180}#)
               '()
               (cons (#{gen-label\ 404}#)
                     (#{gen-labels\ 406}# (cdr #{ls\ 1180}#))))))
         (#{make-ribcage\ 409}#
           (lambda (#{symnames\ 1182}#
                    #{marks\ 1183}#
                    #{labels\ 1184}#)
             (vector
               'ribcage
               #{symnames\ 1182}#
               #{marks\ 1183}#
               #{labels\ 1184}#)))
         (#{ribcage-symnames\ 413}#
           (lambda (#{x\ 1193}#) (vector-ref #{x\ 1193}# 1)))
         (#{ribcage-marks\ 415}#
           (lambda (#{x\ 1195}#) (vector-ref #{x\ 1195}# 2)))
         (#{ribcage-labels\ 417}#
           (lambda (#{x\ 1197}#) (vector-ref #{x\ 1197}# 3)))
         (#{set-ribcage-symnames!\ 419}#
           (lambda (#{x\ 1199}# #{update\ 1200}#)
             (vector-set! #{x\ 1199}# 1 #{update\ 1200}#)))
         (#{set-ribcage-marks!\ 421}#
           (lambda (#{x\ 1203}# #{update\ 1204}#)
             (vector-set! #{x\ 1203}# 2 #{update\ 1204}#)))
         (#{set-ribcage-labels!\ 423}#
           (lambda (#{x\ 1207}# #{update\ 1208}#)
             (vector-set! #{x\ 1207}# 3 #{update\ 1208}#)))
         (#{anti-mark\ 429}#
           (lambda (#{w\ 1211}#)
             (#{make-wrap\ 393}#
               (cons #f (#{wrap-marks\ 395}# #{w\ 1211}#))
               (cons 'shift
                     (#{wrap-subst\ 397}# #{w\ 1211}#)))))
         (#{extend-ribcage!\ 433}#
           (lambda (#{ribcage\ 1214}# #{id\ 1215}# #{label\ 1216}#)
             (begin
               (#{set-ribcage-symnames!\ 419}#
                 #{ribcage\ 1214}#
                 (cons (#{syntax-object-expression\ 354}# #{id\ 1215}#)
                       (#{ribcage-symnames\ 413}# #{ribcage\ 1214}#)))
               (#{set-ribcage-marks!\ 421}#
                 #{ribcage\ 1214}#
                 (cons (#{wrap-marks\ 395}#
                         (#{syntax-object-wrap\ 356}# #{id\ 1215}#))
                       (#{ribcage-marks\ 415}# #{ribcage\ 1214}#)))
               (#{set-ribcage-labels!\ 423}#
                 #{ribcage\ 1214}#
                 (cons #{label\ 1216}#
                       (#{ribcage-labels\ 417}# #{ribcage\ 1214}#))))))
         (#{make-binding-wrap\ 435}#
           (lambda (#{ids\ 1220}# #{labels\ 1221}# #{w\ 1222}#)
             (if (null? #{ids\ 1220}#)
               #{w\ 1222}#
               (#{make-wrap\ 393}#
                 (#{wrap-marks\ 395}# #{w\ 1222}#)
                 (cons (begin
                         (let ((#{labelvec\ 1227}#
                                 (list->vector #{labels\ 1221}#)))
                           (begin
                             (let ((#{n\ 1229}#
                                     (vector-length #{labelvec\ 1227}#)))
                               (begin
                                 (let ((#{symnamevec\ 1232}#
                                         (make-vector #{n\ 1229}#))
                                       (#{marksvec\ 1233}#
                                         (make-vector #{n\ 1229}#)))
                                   (begin
                                     (letrec*
                                       ((#{f\ 1237}#
                                          (lambda (#{ids\ 1238}# #{i\ 1239}#)
                                            (if (not (null? #{ids\ 1238}#))
                                              (call-with-values
                                                (lambda ()
                                                  (#{id-sym-name&marks\ 391}#
                                                    (car #{ids\ 1238}#)
                                                    #{w\ 1222}#))
                                                (lambda (#{symname\ 1240}#
                                                         #{marks\ 1241}#)
                                                  (begin
                                                    (vector-set!
                                                      #{symnamevec\ 1232}#
                                                      #{i\ 1239}#
                                                      #{symname\ 1240}#)
                                                    (vector-set!
                                                      #{marksvec\ 1233}#
                                                      #{i\ 1239}#
                                                      #{marks\ 1241}#)
                                                    (#{f\ 1237}#
                                                      (cdr #{ids\ 1238}#)
                                                      (#{fx+\ 289}#
                                                        #{i\ 1239}#
                                                        1)))))))))
                                       (begin (#{f\ 1237}# #{ids\ 1220}# 0)))
                                     (#{make-ribcage\ 409}#
                                       #{symnamevec\ 1232}#
                                       #{marksvec\ 1233}#
                                       #{labelvec\ 1227}#))))))))
                       (#{wrap-subst\ 397}# #{w\ 1222}#))))))
         (#{smart-append\ 437}#
           (lambda (#{m1\ 1244}# #{m2\ 1245}#)
             (if (null? #{m2\ 1245}#)
               #{m1\ 1244}#
               (append #{m1\ 1244}# #{m2\ 1245}#))))
         (#{join-wraps\ 439}#
           (lambda (#{w1\ 1248}# #{w2\ 1249}#)
             (begin
               (let ((#{m1\ 1254}# (#{wrap-marks\ 395}# #{w1\ 1248}#))
                     (#{s1\ 1255}# (#{wrap-subst\ 397}# #{w1\ 1248}#)))
                 (if (null? #{m1\ 1254}#)
                   (if (null? #{s1\ 1255}#)
                     #{w2\ 1249}#
                     (#{make-wrap\ 393}#
                       (#{wrap-marks\ 395}# #{w2\ 1249}#)
                       (#{smart-append\ 437}#
                         #{s1\ 1255}#
                         (#{wrap-subst\ 397}# #{w2\ 1249}#))))
                   (#{make-wrap\ 393}#
                     (#{smart-append\ 437}#
                       #{m1\ 1254}#
                       (#{wrap-marks\ 395}# #{w2\ 1249}#))
                     (#{smart-append\ 437}#
                       #{s1\ 1255}#
                       (#{wrap-subst\ 397}# #{w2\ 1249}#))))))))
         (#{join-marks\ 441}#
           (lambda (#{m1\ 1256}# #{m2\ 1257}#)
             (#{smart-append\ 437}# #{m1\ 1256}# #{m2\ 1257}#)))
         (#{same-marks?\ 443}#
           (lambda (#{x\ 1260}# #{y\ 1261}#)
             (begin
               (let ((#{t\ 1266}# (eq? #{x\ 1260}# #{y\ 1261}#)))
                 (if #{t\ 1266}#
                   #{t\ 1266}#
                   (if (not (null? #{x\ 1260}#))
                     (if (not (null? #{y\ 1261}#))
                       (if (eq? (car #{x\ 1260}#) (car #{y\ 1261}#))
                         (#{same-marks?\ 443}#
                           (cdr #{x\ 1260}#)
                           (cdr #{y\ 1261}#))
                         #f)
                       #f)
                     #f))))))
         (#{id-var-name\ 445}#
           (lambda (#{id\ 1272}# #{w\ 1273}#)
             (letrec*
               ((#{search\ 1278}#
                  (lambda (#{sym\ 1294}# #{subst\ 1295}# #{marks\ 1296}#)
                    (if (null? #{subst\ 1295}#)
                      (values #f #{marks\ 1296}#)
                      (begin
                        (let ((#{fst\ 1301}# (car #{subst\ 1295}#)))
                          (if (eq? #{fst\ 1301}# (quote shift))
                            (#{search\ 1278}#
                              #{sym\ 1294}#
                              (cdr #{subst\ 1295}#)
                              (cdr #{marks\ 1296}#))
                            (begin
                              (let ((#{symnames\ 1303}#
                                      (#{ribcage-symnames\ 413}#
                                        #{fst\ 1301}#)))
                                (if (vector? #{symnames\ 1303}#)
                                  (#{search-vector-rib\ 1282}#
                                    #{sym\ 1294}#
                                    #{subst\ 1295}#
                                    #{marks\ 1296}#
                                    #{symnames\ 1303}#
                                    #{fst\ 1301}#)
                                  (#{search-list-rib\ 1280}#
                                    #{sym\ 1294}#
                                    #{subst\ 1295}#
                                    #{marks\ 1296}#
                                    #{symnames\ 1303}#
                                    #{fst\ 1301}#))))))))))
                (#{search-list-rib\ 1280}#
                  (lambda (#{sym\ 1304}#
                           #{subst\ 1305}#
                           #{marks\ 1306}#
                           #{symnames\ 1307}#
                           #{ribcage\ 1308}#)
                    (letrec*
                      ((#{f\ 1317}#
                         (lambda (#{symnames\ 1318}# #{i\ 1319}#)
                           (if (null? #{symnames\ 1318}#)
                             (#{search\ 1278}#
                               #{sym\ 1304}#
                               (cdr #{subst\ 1305}#)
                               #{marks\ 1306}#)
                             (if (if (eq? (car #{symnames\ 1318}#)
                                          #{sym\ 1304}#)
                                   (#{same-marks?\ 443}#
                                     #{marks\ 1306}#
                                     (list-ref
                                       (#{ribcage-marks\ 415}#
                                         #{ribcage\ 1308}#)
                                       #{i\ 1319}#))
                                   #f)
                               (values
                                 (list-ref
                                   (#{ribcage-labels\ 417}# #{ribcage\ 1308}#)
                                   #{i\ 1319}#)
                                 #{marks\ 1306}#)
                               (#{f\ 1317}#
                                 (cdr #{symnames\ 1318}#)
                                 (#{fx+\ 289}# #{i\ 1319}# 1)))))))
                      (begin (#{f\ 1317}# #{symnames\ 1307}# 0)))))
                (#{search-vector-rib\ 1282}#
                  (lambda (#{sym\ 1327}#
                           #{subst\ 1328}#
                           #{marks\ 1329}#
                           #{symnames\ 1330}#
                           #{ribcage\ 1331}#)
                    (begin
                      (let ((#{n\ 1338}# (vector-length #{symnames\ 1330}#)))
                        (letrec*
                          ((#{f\ 1341}#
                             (lambda (#{i\ 1342}#)
                               (if (#{fx=\ 293}# #{i\ 1342}# #{n\ 1338}#)
                                 (#{search\ 1278}#
                                   #{sym\ 1327}#
                                   (cdr #{subst\ 1328}#)
                                   #{marks\ 1329}#)
                                 (if (if (eq? (vector-ref
                                                #{symnames\ 1330}#
                                                #{i\ 1342}#)
                                              #{sym\ 1327}#)
                                       (#{same-marks?\ 443}#
                                         #{marks\ 1329}#
                                         (vector-ref
                                           (#{ribcage-marks\ 415}#
                                             #{ribcage\ 1331}#)
                                           #{i\ 1342}#))
                                       #f)
                                   (values
                                     (vector-ref
                                       (#{ribcage-labels\ 417}#
                                         #{ribcage\ 1331}#)
                                       #{i\ 1342}#)
                                     #{marks\ 1329}#)
                                   (#{f\ 1341}#
                                     (#{fx+\ 289}# #{i\ 1342}# 1)))))))
                          (begin (#{f\ 1341}# 0))))))))
               (begin
                 (if (symbol? #{id\ 1272}#)
                   (begin
                     (let ((#{t\ 1352}#
                             (call-with-values
                               (lambda ()
                                 (#{search\ 1278}#
                                   #{id\ 1272}#
                                   (#{wrap-subst\ 397}# #{w\ 1273}#)
                                   (#{wrap-marks\ 395}# #{w\ 1273}#)))
                               (lambda (#{x\ 1354}# . #{ignore\ 1355}#)
                                 #{x\ 1354}#))))
                       (if #{t\ 1352}# #{t\ 1352}# #{id\ 1272}#)))
                   (if (#{syntax-object?\ 352}# #{id\ 1272}#)
                     (begin
                       (let ((#{id\ 1363}#
                               (#{syntax-object-expression\ 354}#
                                 #{id\ 1272}#))
                             (#{w1\ 1364}#
                               (#{syntax-object-wrap\ 356}# #{id\ 1272}#)))
                         (begin
                           (let ((#{marks\ 1366}#
                                   (#{join-marks\ 441}#
                                     (#{wrap-marks\ 395}# #{w\ 1273}#)
                                     (#{wrap-marks\ 395}# #{w1\ 1364}#))))
                             (call-with-values
                               (lambda ()
                                 (#{search\ 1278}#
                                   #{id\ 1363}#
                                   (#{wrap-subst\ 397}# #{w\ 1273}#)
                                   #{marks\ 1366}#))
                               (lambda (#{new-id\ 1367}# #{marks\ 1368}#)
                                 (begin
                                   (let ((#{t\ 1373}# #{new-id\ 1367}#))
                                     (if #{t\ 1373}#
                                       #{t\ 1373}#
                                       (begin
                                         (let ((#{t\ 1376}#
                                                 (call-with-values
                                                   (lambda ()
                                                     (#{search\ 1278}#
                                                       #{id\ 1363}#
                                                       (#{wrap-subst\ 397}#
                                                         #{w1\ 1364}#)
                                                       #{marks\ 1368}#))
                                                   (lambda (#{x\ 1378}#
                                                            .
                                                            #{ignore\ 1379}#)
                                                     #{x\ 1378}#))))
                                           (if #{t\ 1376}#
                                             #{t\ 1376}#
                                             #{id\ 1363}#))))))))))))
                     (syntax-violation
                       'id-var-name
                       "invalid id"
                       #{id\ 1272}#)))))))
         (#{free-id=?\ 447}#
           (lambda (#{i\ 1384}# #{j\ 1385}#)
             (if (eq? (begin
                        (let ((#{x\ 1391}# #{i\ 1384}#))
                          (if (#{syntax-object?\ 352}# #{x\ 1391}#)
                            (#{syntax-object-expression\ 354}# #{x\ 1391}#)
                            #{x\ 1391}#)))
                      (begin
                        (let ((#{x\ 1394}# #{j\ 1385}#))
                          (if (#{syntax-object?\ 352}# #{x\ 1394}#)
                            (#{syntax-object-expression\ 354}# #{x\ 1394}#)
                            #{x\ 1394}#))))
               (eq? (#{id-var-name\ 445}# #{i\ 1384}# (quote (())))
                    (#{id-var-name\ 445}# #{j\ 1385}# (quote (()))))
               #f)))
         (#{bound-id=?\ 449}#
           (lambda (#{i\ 1398}# #{j\ 1399}#)
             (if (if (#{syntax-object?\ 352}# #{i\ 1398}#)
                   (#{syntax-object?\ 352}# #{j\ 1399}#)
                   #f)
               (if (eq? (#{syntax-object-expression\ 354}# #{i\ 1398}#)
                        (#{syntax-object-expression\ 354}# #{j\ 1399}#))
                 (#{same-marks?\ 443}#
                   (#{wrap-marks\ 395}#
                     (#{syntax-object-wrap\ 356}# #{i\ 1398}#))
                   (#{wrap-marks\ 395}#
                     (#{syntax-object-wrap\ 356}# #{j\ 1399}#)))
                 #f)
               (eq? #{i\ 1398}# #{j\ 1399}#))))
         (#{valid-bound-ids?\ 451}#
           (lambda (#{ids\ 1406}#)
             (if (letrec*
                   ((#{all-ids?\ 1411}#
                      (lambda (#{ids\ 1412}#)
                        (begin
                          (let ((#{t\ 1415}# (null? #{ids\ 1412}#)))
                            (if #{t\ 1415}#
                              #{t\ 1415}#
                              (if (#{id?\ 388}# (car #{ids\ 1412}#))
                                (#{all-ids?\ 1411}# (cdr #{ids\ 1412}#))
                                #f)))))))
                   (begin (#{all-ids?\ 1411}# #{ids\ 1406}#)))
               (#{distinct-bound-ids?\ 453}# #{ids\ 1406}#)
               #f)))
         (#{distinct-bound-ids?\ 453}#
           (lambda (#{ids\ 1420}#)
             (letrec*
               ((#{distinct?\ 1424}#
                  (lambda (#{ids\ 1425}#)
                    (begin
                      (let ((#{t\ 1428}# (null? #{ids\ 1425}#)))
                        (if #{t\ 1428}#
                          #{t\ 1428}#
                          (if (not (#{bound-id-member?\ 455}#
                                     (car #{ids\ 1425}#)
                                     (cdr #{ids\ 1425}#)))
                            (#{distinct?\ 1424}# (cdr #{ids\ 1425}#))
                            #f)))))))
               (begin (#{distinct?\ 1424}# #{ids\ 1420}#)))))
         (#{bound-id-member?\ 455}#
           (lambda (#{x\ 1432}# #{list\ 1433}#)
             (if (not (null? #{list\ 1433}#))
               (begin
                 (let ((#{t\ 1440}#
                         (#{bound-id=?\ 449}#
                           #{x\ 1432}#
                           (car #{list\ 1433}#))))
                   (if #{t\ 1440}#
                     #{t\ 1440}#
                     (#{bound-id-member?\ 455}#
                       #{x\ 1432}#
                       (cdr #{list\ 1433}#)))))
               #f)))
         (#{wrap\ 457}#
           (lambda (#{x\ 1442}# #{w\ 1443}# #{defmod\ 1444}#)
             (if (if (null? (#{wrap-marks\ 395}# #{w\ 1443}#))
                   (null? (#{wrap-subst\ 397}# #{w\ 1443}#))
                   #f)
               #{x\ 1442}#
               (if (#{syntax-object?\ 352}# #{x\ 1442}#)
                 (#{make-syntax-object\ 350}#
                   (#{syntax-object-expression\ 354}# #{x\ 1442}#)
                   (#{join-wraps\ 439}#
                     #{w\ 1443}#
                     (#{syntax-object-wrap\ 356}# #{x\ 1442}#))
                   (#{syntax-object-module\ 358}# #{x\ 1442}#))
                 (if (null? #{x\ 1442}#)
                   #{x\ 1442}#
                   (#{make-syntax-object\ 350}#
                     #{x\ 1442}#
                     #{w\ 1443}#
                     #{defmod\ 1444}#))))))
         (#{source-wrap\ 459}#
           (lambda (#{x\ 1457}#
                    #{w\ 1458}#
                    #{s\ 1459}#
                    #{defmod\ 1460}#)
             (#{wrap\ 457}#
               (#{decorate-source\ 306}#
                 #{x\ 1457}#
                 #{s\ 1459}#)
               #{w\ 1458}#
               #{defmod\ 1460}#)))
         (#{chi-sequence\ 461}#
           (lambda (#{body\ 1465}#
                    #{r\ 1466}#
                    #{w\ 1467}#
                    #{s\ 1468}#
                    #{mod\ 1469}#)
             (#{build-sequence\ 340}#
               #{s\ 1468}#
               (letrec*
                 ((#{dobody\ 1480}#
                    (lambda (#{body\ 1481}#
                             #{r\ 1482}#
                             #{w\ 1483}#
                             #{mod\ 1484}#)
                      (if (null? #{body\ 1481}#)
                        '()
                        (begin
                          (let ((#{first\ 1486}#
                                  (#{chi\ 473}#
                                    (car #{body\ 1481}#)
                                    #{r\ 1482}#
                                    #{w\ 1483}#
                                    #{mod\ 1484}#)))
                            (cons #{first\ 1486}#
                                  (#{dobody\ 1480}#
                                    (cdr #{body\ 1481}#)
                                    #{r\ 1482}#
                                    #{w\ 1483}#
                                    #{mod\ 1484}#))))))))
                 (begin
                   (#{dobody\ 1480}#
                     #{body\ 1465}#
                     #{r\ 1466}#
                     #{w\ 1467}#
                     #{mod\ 1469}#))))))
         (#{chi-top-sequence\ 463}#
           (lambda (#{body\ 1487}#
                    #{r\ 1488}#
                    #{w\ 1489}#
                    #{s\ 1490}#
                    #{m\ 1491}#
                    #{esew\ 1492}#
                    #{mod\ 1493}#)
             (#{build-sequence\ 340}#
               #{s\ 1490}#
               (letrec*
                 ((#{dobody\ 1509}#
                    (lambda (#{body\ 1510}#
                             #{r\ 1511}#
                             #{w\ 1512}#
                             #{m\ 1513}#
                             #{esew\ 1514}#
                             #{mod\ 1515}#
                             #{out\ 1516}#)
                      (if (null? #{body\ 1510}#)
                        (reverse #{out\ 1516}#)
                        (#{dobody\ 1509}#
                          (cdr #{body\ 1510}#)
                          #{r\ 1511}#
                          #{w\ 1512}#
                          #{m\ 1513}#
                          #{esew\ 1514}#
                          #{mod\ 1515}#
                          (cons (#{chi-top\ 471}#
                                  (car #{body\ 1510}#)
                                  #{r\ 1511}#
                                  #{w\ 1512}#
                                  #{m\ 1513}#
                                  #{esew\ 1514}#
                                  #{mod\ 1515}#)
                                #{out\ 1516}#))))))
                 (begin
                   (#{dobody\ 1509}#
                     #{body\ 1487}#
                     #{r\ 1488}#
                     #{w\ 1489}#
                     #{m\ 1491}#
                     #{esew\ 1492}#
                     #{mod\ 1493}#
                     '()))))))
         (#{chi-install-global\ 465}#
           (lambda (#{name\ 1517}# #{e\ 1518}#)
             (#{build-global-definition\ 328}#
               #f
               #{name\ 1517}#
               (#{build-application\ 312}#
                 #f
                 (#{build-primref\ 336}#
                   #f
                   'make-syntax-transformer)
                 (list (#{build-data\ 338}# #f #{name\ 1517}#)
                       (#{build-data\ 338}# #f (quote macro))
                       #{e\ 1518}#)))))
         (#{chi-when-list\ 467}#
           (lambda (#{e\ 1526}# #{when-list\ 1527}# #{w\ 1528}#)
             (letrec*
               ((#{f\ 1535}#
                  (lambda (#{when-list\ 1536}# #{situations\ 1537}#)
                    (if (null? #{when-list\ 1536}#)
                      #{situations\ 1537}#
                      (#{f\ 1535}#
                        (cdr #{when-list\ 1536}#)
                        (cons (begin
                                (let ((#{x\ 1539}# (car #{when-list\ 1536}#)))
                                  (if (#{free-id=?\ 447}#
                                        #{x\ 1539}#
                                        '#(syntax-object
                                           compile
                                           ((top)
                                            #(ribcage () () ())
                                            #(ribcage () () ())
                                            #(ribcage () () ())
                                            #(ribcage #(x) #((top)) #("i1538"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(f when-list situations)
                                              #((top) (top) (top))
                                              #("i1532" "i1533" "i1534"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(e when-list w)
                                              #((top) (top) (top))
                                              #("i1529" "i1530" "i1531"))
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
                                              ("i502"
                                               "i500"
                                               "i498"
                                               "i496"
                                               "i494"
                                               "i492"
                                               "i490"
                                               "i488"
                                               "i486"
                                               "i484"
                                               "i482"
                                               "i480"
                                               "i478"
                                               "i476"
                                               "i474"
                                               "i472"
                                               "i470"
                                               "i468"
                                               "i466"
                                               "i464"
                                               "i462"
                                               "i460"
                                               "i458"
                                               "i456"
                                               "i454"
                                               "i452"
                                               "i450"
                                               "i448"
                                               "i446"
                                               "i444"
                                               "i442"
                                               "i440"
                                               "i438"
                                               "i436"
                                               "i434"
                                               "i432"
                                               "i431"
                                               "i430"
                                               "i428"
                                               "i427"
                                               "i426"
                                               "i425"
                                               "i424"
                                               "i422"
                                               "i420"
                                               "i418"
                                               "i416"
                                               "i414"
                                               "i412"
                                               "i410"
                                               "i408"
                                               "i405"
                                               "i403"
                                               "i402"
                                               "i401"
                                               "i400"
                                               "i399"
                                               "i398"
                                               "i396"
                                               "i394"
                                               "i392"
                                               "i390"
                                               "i389"
                                               "i387"
                                               "i385"
                                               "i383"
                                               "i381"
                                               "i379"
                                               "i377"
                                               "i375"
                                               "i374"
                                               "i372"
                                               "i370"
                                               "i369"
                                               "i368"
                                               "i366"
                                               "i365"
                                               "i363"
                                               "i361"
                                               "i359"
                                               "i357"
                                               "i355"
                                               "i353"
                                               "i351"
                                               "i349"
                                               "i347"
                                               "i345"
                                               "i343"
                                               "i341"
                                               "i339"
                                               "i337"
                                               "i335"
                                               "i333"
                                               "i331"
                                               "i329"
                                               "i327"
                                               "i325"
                                               "i323"
                                               "i321"
                                               "i319"
                                               "i317"
                                               "i315"
                                               "i313"
                                               "i311"
                                               "i309"
                                               "i307"
                                               "i305"
                                               "i303"
                                               "i301"
                                               "i300"
                                               "i298"
                                               "i296"
                                               "i294"
                                               "i292"
                                               "i290"
                                               "i288"
                                               "i286"
                                               "i284"
                                               "i282"
                                               "i279"
                                               "i277"
                                               "i275"
                                               "i273"
                                               "i271"
                                               "i269"
                                               "i267"
                                               "i265"
                                               "i263"
                                               "i261"
                                               "i259"
                                               "i257"
                                               "i255"
                                               "i253"
                                               "i251"
                                               "i249"
                                               "i247"
                                               "i245"))
                                            #(ribcage
                                              (define-structure
                                                define-expansion-accessors
                                                define-expansion-constructors
                                                and-map*)
                                              ((top) (top) (top) (top))
                                              ("i39" "i38" "i37" "i35")))
                                           (hygiene guile)))
                                    'compile
                                    (if (#{free-id=?\ 447}#
                                          #{x\ 1539}#
                                          '#(syntax-object
                                             load
                                             ((top)
                                              #(ribcage () () ())
                                              #(ribcage () () ())
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(x)
                                                #((top))
                                                #("i1538"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(f when-list situations)
                                                #((top) (top) (top))
                                                #("i1532" "i1533" "i1534"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(e when-list w)
                                                #((top) (top) (top))
                                                #("i1529" "i1530" "i1531"))
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
                                                ("i502"
                                                 "i500"
                                                 "i498"
                                                 "i496"
                                                 "i494"
                                                 "i492"
                                                 "i490"
                                                 "i488"
                                                 "i486"
                                                 "i484"
                                                 "i482"
                                                 "i480"
                                                 "i478"
                                                 "i476"
                                                 "i474"
                                                 "i472"
                                                 "i470"
                                                 "i468"
                                                 "i466"
                                                 "i464"
                                                 "i462"
                                                 "i460"
                                                 "i458"
                                                 "i456"
                                                 "i454"
                                                 "i452"
                                                 "i450"
                                                 "i448"
                                                 "i446"
                                                 "i444"
                                                 "i442"
                                                 "i440"
                                                 "i438"
                                                 "i436"
                                                 "i434"
                                                 "i432"
                                                 "i431"
                                                 "i430"
                                                 "i428"
                                                 "i427"
                                                 "i426"
                                                 "i425"
                                                 "i424"
                                                 "i422"
                                                 "i420"
                                                 "i418"
                                                 "i416"
                                                 "i414"
                                                 "i412"
                                                 "i410"
                                                 "i408"
                                                 "i405"
                                                 "i403"
                                                 "i402"
                                                 "i401"
                                                 "i400"
                                                 "i399"
                                                 "i398"
                                                 "i396"
                                                 "i394"
                                                 "i392"
                                                 "i390"
                                                 "i389"
                                                 "i387"
                                                 "i385"
                                                 "i383"
                                                 "i381"
                                                 "i379"
                                                 "i377"
                                                 "i375"
                                                 "i374"
                                                 "i372"
                                                 "i370"
                                                 "i369"
                                                 "i368"
                                                 "i366"
                                                 "i365"
                                                 "i363"
                                                 "i361"
                                                 "i359"
                                                 "i357"
                                                 "i355"
                                                 "i353"
                                                 "i351"
                                                 "i349"
                                                 "i347"
                                                 "i345"
                                                 "i343"
                                                 "i341"
                                                 "i339"
                                                 "i337"
                                                 "i335"
                                                 "i333"
                                                 "i331"
                                                 "i329"
                                                 "i327"
                                                 "i325"
                                                 "i323"
                                                 "i321"
                                                 "i319"
                                                 "i317"
                                                 "i315"
                                                 "i313"
                                                 "i311"
                                                 "i309"
                                                 "i307"
                                                 "i305"
                                                 "i303"
                                                 "i301"
                                                 "i300"
                                                 "i298"
                                                 "i296"
                                                 "i294"
                                                 "i292"
                                                 "i290"
                                                 "i288"
                                                 "i286"
                                                 "i284"
                                                 "i282"
                                                 "i279"
                                                 "i277"
                                                 "i275"
                                                 "i273"
                                                 "i271"
                                                 "i269"
                                                 "i267"
                                                 "i265"
                                                 "i263"
                                                 "i261"
                                                 "i259"
                                                 "i257"
                                                 "i255"
                                                 "i253"
                                                 "i251"
                                                 "i249"
                                                 "i247"
                                                 "i245"))
                                              #(ribcage
                                                (define-structure
                                                  define-expansion-accessors
                                                  define-expansion-constructors
                                                  and-map*)
                                                ((top) (top) (top) (top))
                                                ("i39" "i38" "i37" "i35")))
                                             (hygiene guile)))
                                      'load
                                      (if (#{free-id=?\ 447}#
                                            #{x\ 1539}#
                                            '#(syntax-object
                                               eval
                                               ((top)
                                                #(ribcage () () ())
                                                #(ribcage () () ())
                                                #(ribcage () () ())
                                                #(ribcage
                                                  #(x)
                                                  #((top))
                                                  #("i1538"))
                                                #(ribcage () () ())
                                                #(ribcage
                                                  #(f when-list situations)
                                                  #((top) (top) (top))
                                                  #("i1532" "i1533" "i1534"))
                                                #(ribcage () () ())
                                                #(ribcage
                                                  #(e when-list w)
                                                  #((top) (top) (top))
                                                  #("i1529" "i1530" "i1531"))
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
                                                  ("i502"
                                                   "i500"
                                                   "i498"
                                                   "i496"
                                                   "i494"
                                                   "i492"
                                                   "i490"
                                                   "i488"
                                                   "i486"
                                                   "i484"
                                                   "i482"
                                                   "i480"
                                                   "i478"
                                                   "i476"
                                                   "i474"
                                                   "i472"
                                                   "i470"
                                                   "i468"
                                                   "i466"
                                                   "i464"
                                                   "i462"
                                                   "i460"
                                                   "i458"
                                                   "i456"
                                                   "i454"
                                                   "i452"
                                                   "i450"
                                                   "i448"
                                                   "i446"
                                                   "i444"
                                                   "i442"
                                                   "i440"
                                                   "i438"
                                                   "i436"
                                                   "i434"
                                                   "i432"
                                                   "i431"
                                                   "i430"
                                                   "i428"
                                                   "i427"
                                                   "i426"
                                                   "i425"
                                                   "i424"
                                                   "i422"
                                                   "i420"
                                                   "i418"
                                                   "i416"
                                                   "i414"
                                                   "i412"
                                                   "i410"
                                                   "i408"
                                                   "i405"
                                                   "i403"
                                                   "i402"
                                                   "i401"
                                                   "i400"
                                                   "i399"
                                                   "i398"
                                                   "i396"
                                                   "i394"
                                                   "i392"
                                                   "i390"
                                                   "i389"
                                                   "i387"
                                                   "i385"
                                                   "i383"
                                                   "i381"
                                                   "i379"
                                                   "i377"
                                                   "i375"
                                                   "i374"
                                                   "i372"
                                                   "i370"
                                                   "i369"
                                                   "i368"
                                                   "i366"
                                                   "i365"
                                                   "i363"
                                                   "i361"
                                                   "i359"
                                                   "i357"
                                                   "i355"
                                                   "i353"
                                                   "i351"
                                                   "i349"
                                                   "i347"
                                                   "i345"
                                                   "i343"
                                                   "i341"
                                                   "i339"
                                                   "i337"
                                                   "i335"
                                                   "i333"
                                                   "i331"
                                                   "i329"
                                                   "i327"
                                                   "i325"
                                                   "i323"
                                                   "i321"
                                                   "i319"
                                                   "i317"
                                                   "i315"
                                                   "i313"
                                                   "i311"
                                                   "i309"
                                                   "i307"
                                                   "i305"
                                                   "i303"
                                                   "i301"
                                                   "i300"
                                                   "i298"
                                                   "i296"
                                                   "i294"
                                                   "i292"
                                                   "i290"
                                                   "i288"
                                                   "i286"
                                                   "i284"
                                                   "i282"
                                                   "i279"
                                                   "i277"
                                                   "i275"
                                                   "i273"
                                                   "i271"
                                                   "i269"
                                                   "i267"
                                                   "i265"
                                                   "i263"
                                                   "i261"
                                                   "i259"
                                                   "i257"
                                                   "i255"
                                                   "i253"
                                                   "i251"
                                                   "i249"
                                                   "i247"
                                                   "i245"))
                                                #(ribcage
                                                  (define-structure
                                                    define-expansion-accessors
                                                    define-expansion-constructors
                                                    and-map*)
                                                  ((top) (top) (top) (top))
                                                  ("i39" "i38" "i37" "i35")))
                                               (hygiene guile)))
                                        'eval
                                        (if (#{free-id=?\ 447}#
                                              #{x\ 1539}#
                                              '#(syntax-object
                                                 expand
                                                 ((top)
                                                  #(ribcage () () ())
                                                  #(ribcage () () ())
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(x)
                                                    #((top))
                                                    #("i1538"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(f when-list situations)
                                                    #((top) (top) (top))
                                                    #("i1532" "i1533" "i1534"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(e when-list w)
                                                    #((top) (top) (top))
                                                    #("i1529" "i1530" "i1531"))
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
                                                    ("i502"
                                                     "i500"
                                                     "i498"
                                                     "i496"
                                                     "i494"
                                                     "i492"
                                                     "i490"
                                                     "i488"
                                                     "i486"
                                                     "i484"
                                                     "i482"
                                                     "i480"
                                                     "i478"
                                                     "i476"
                                                     "i474"
                                                     "i472"
                                                     "i470"
                                                     "i468"
                                                     "i466"
                                                     "i464"
                                                     "i462"
                                                     "i460"
                                                     "i458"
                                                     "i456"
                                                     "i454"
                                                     "i452"
                                                     "i450"
                                                     "i448"
                                                     "i446"
                                                     "i444"
                                                     "i442"
                                                     "i440"
                                                     "i438"
                                                     "i436"
                                                     "i434"
                                                     "i432"
                                                     "i431"
                                                     "i430"
                                                     "i428"
                                                     "i427"
                                                     "i426"
                                                     "i425"
                                                     "i424"
                                                     "i422"
                                                     "i420"
                                                     "i418"
                                                     "i416"
                                                     "i414"
                                                     "i412"
                                                     "i410"
                                                     "i408"
                                                     "i405"
                                                     "i403"
                                                     "i402"
                                                     "i401"
                                                     "i400"
                                                     "i399"
                                                     "i398"
                                                     "i396"
                                                     "i394"
                                                     "i392"
                                                     "i390"
                                                     "i389"
                                                     "i387"
                                                     "i385"
                                                     "i383"
                                                     "i381"
                                                     "i379"
                                                     "i377"
                                                     "i375"
                                                     "i374"
                                                     "i372"
                                                     "i370"
                                                     "i369"
                                                     "i368"
                                                     "i366"
                                                     "i365"
                                                     "i363"
                                                     "i361"
                                                     "i359"
                                                     "i357"
                                                     "i355"
                                                     "i353"
                                                     "i351"
                                                     "i349"
                                                     "i347"
                                                     "i345"
                                                     "i343"
                                                     "i341"
                                                     "i339"
                                                     "i337"
                                                     "i335"
                                                     "i333"
                                                     "i331"
                                                     "i329"
                                                     "i327"
                                                     "i325"
                                                     "i323"
                                                     "i321"
                                                     "i319"
                                                     "i317"
                                                     "i315"
                                                     "i313"
                                                     "i311"
                                                     "i309"
                                                     "i307"
                                                     "i305"
                                                     "i303"
                                                     "i301"
                                                     "i300"
                                                     "i298"
                                                     "i296"
                                                     "i294"
                                                     "i292"
                                                     "i290"
                                                     "i288"
                                                     "i286"
                                                     "i284"
                                                     "i282"
                                                     "i279"
                                                     "i277"
                                                     "i275"
                                                     "i273"
                                                     "i271"
                                                     "i269"
                                                     "i267"
                                                     "i265"
                                                     "i263"
                                                     "i261"
                                                     "i259"
                                                     "i257"
                                                     "i255"
                                                     "i253"
                                                     "i251"
                                                     "i249"
                                                     "i247"
                                                     "i245"))
                                                  #(ribcage
                                                    (define-structure
                                                      define-expansion-accessors
                                                      define-expansion-constructors
                                                      and-map*)
                                                    ((top) (top) (top) (top))
                                                    ("i39" "i38" "i37" "i35")))
                                                 (hygiene guile)))
                                          'expand
                                          (syntax-violation
                                            'eval-when
                                            "invalid situation"
                                            #{e\ 1526}#
                                            (#{wrap\ 457}#
                                              #{x\ 1539}#
                                              #{w\ 1528}#
                                              #f))))))))
                              #{situations\ 1537}#))))))
               (begin
                 (#{f\ 1535}# #{when-list\ 1527}# (quote ()))))))
         (#{syntax-type\ 469}#
           (lambda (#{e\ 1549}#
                    #{r\ 1550}#
                    #{w\ 1551}#
                    #{s\ 1552}#
                    #{rib\ 1553}#
                    #{mod\ 1554}#
                    #{for-car?\ 1555}#)
             (if (symbol? #{e\ 1549}#)
               (begin
                 (let ((#{n\ 1567}#
                         (#{id-var-name\ 445}# #{e\ 1549}# #{w\ 1551}#)))
                   (begin
                     (let ((#{b\ 1569}#
                             (#{lookup\ 382}#
                               #{n\ 1567}#
                               #{r\ 1550}#
                               #{mod\ 1554}#)))
                       (begin
                         (let ((#{type\ 1571}#
                                 (#{binding-type\ 371}# #{b\ 1569}#)))
                           (if (eqv? #{type\ 1571}# (quote lexical))
                             (values
                               #{type\ 1571}#
                               (#{binding-value\ 373}# #{b\ 1569}#)
                               #{e\ 1549}#
                               #{w\ 1551}#
                               #{s\ 1552}#
                               #{mod\ 1554}#)
                             (if (eqv? #{type\ 1571}# (quote global))
                               (values
                                 #{type\ 1571}#
                                 #{n\ 1567}#
                                 #{e\ 1549}#
                                 #{w\ 1551}#
                                 #{s\ 1552}#
                                 #{mod\ 1554}#)
                               (if (eqv? #{type\ 1571}# (quote macro))
                                 (if #{for-car?\ 1555}#
                                   (values
                                     #{type\ 1571}#
                                     (#{binding-value\ 373}# #{b\ 1569}#)
                                     #{e\ 1549}#
                                     #{w\ 1551}#
                                     #{s\ 1552}#
                                     #{mod\ 1554}#)
                                   (#{syntax-type\ 469}#
                                     (#{chi-macro\ 479}#
                                       (#{binding-value\ 373}# #{b\ 1569}#)
                                       #{e\ 1549}#
                                       #{r\ 1550}#
                                       #{w\ 1551}#
                                       #{s\ 1552}#
                                       #{rib\ 1553}#
                                       #{mod\ 1554}#)
                                     #{r\ 1550}#
                                     '(())
                                     #{s\ 1552}#
                                     #{rib\ 1553}#
                                     #{mod\ 1554}#
                                     #f))
                                 (values
                                   #{type\ 1571}#
                                   (#{binding-value\ 373}# #{b\ 1569}#)
                                   #{e\ 1549}#
                                   #{w\ 1551}#
                                   #{s\ 1552}#
                                   #{mod\ 1554}#))))))))))
               (if (pair? #{e\ 1549}#)
                 (begin
                   (let ((#{first\ 1580}# (car #{e\ 1549}#)))
                     (call-with-values
                       (lambda ()
                         (#{syntax-type\ 469}#
                           #{first\ 1580}#
                           #{r\ 1550}#
                           #{w\ 1551}#
                           #{s\ 1552}#
                           #{rib\ 1553}#
                           #{mod\ 1554}#
                           #t))
                       (lambda (#{ftype\ 1581}#
                                #{fval\ 1582}#
                                #{fe\ 1583}#
                                #{fw\ 1584}#
                                #{fs\ 1585}#
                                #{fmod\ 1586}#)
                         (if (eqv? #{ftype\ 1581}# (quote lexical))
                           (values
                             'lexical-call
                             #{fval\ 1582}#
                             #{e\ 1549}#
                             #{w\ 1551}#
                             #{s\ 1552}#
                             #{mod\ 1554}#)
                           (if (eqv? #{ftype\ 1581}# (quote global))
                             (values
                               'global-call
                               (#{make-syntax-object\ 350}#
                                 #{fval\ 1582}#
                                 #{w\ 1551}#
                                 #{fmod\ 1586}#)
                               #{e\ 1549}#
                               #{w\ 1551}#
                               #{s\ 1552}#
                               #{mod\ 1554}#)
                             (if (eqv? #{ftype\ 1581}# (quote macro))
                               (#{syntax-type\ 469}#
                                 (#{chi-macro\ 479}#
                                   #{fval\ 1582}#
                                   #{e\ 1549}#
                                   #{r\ 1550}#
                                   #{w\ 1551}#
                                   #{s\ 1552}#
                                   #{rib\ 1553}#
                                   #{mod\ 1554}#)
                                 #{r\ 1550}#
                                 '(())
                                 #{s\ 1552}#
                                 #{rib\ 1553}#
                                 #{mod\ 1554}#
                                 #{for-car?\ 1555}#)
                               (if (eqv? #{ftype\ 1581}# (quote module-ref))
                                 (call-with-values
                                   (lambda ()
                                     (#{fval\ 1582}#
                                       #{e\ 1549}#
                                       #{r\ 1550}#
                                       #{w\ 1551}#))
                                   (lambda (#{e\ 1598}#
                                            #{r\ 1599}#
                                            #{w\ 1600}#
                                            #{s\ 1601}#
                                            #{mod\ 1602}#)
                                     (#{syntax-type\ 469}#
                                       #{e\ 1598}#
                                       #{r\ 1599}#
                                       #{w\ 1600}#
                                       #{s\ 1601}#
                                       #{rib\ 1553}#
                                       #{mod\ 1602}#
                                       #{for-car?\ 1555}#)))
                                 (if (eqv? #{ftype\ 1581}# (quote core))
                                   (values
                                     'core-form
                                     #{fval\ 1582}#
                                     #{e\ 1549}#
                                     #{w\ 1551}#
                                     #{s\ 1552}#
                                     #{mod\ 1554}#)
                                   (if (eqv? #{ftype\ 1581}#
                                             'local-syntax)
                                     (values
                                       'local-syntax-form
                                       #{fval\ 1582}#
                                       #{e\ 1549}#
                                       #{w\ 1551}#
                                       #{s\ 1552}#
                                       #{mod\ 1554}#)
                                     (if (eqv? #{ftype\ 1581}# (quote begin))
                                       (values
                                         'begin-form
                                         #f
                                         #{e\ 1549}#
                                         #{w\ 1551}#
                                         #{s\ 1552}#
                                         #{mod\ 1554}#)
                                       (if (eqv? #{ftype\ 1581}#
                                                 'eval-when)
                                         (values
                                           'eval-when-form
                                           #f
                                           #{e\ 1549}#
                                           #{w\ 1551}#
                                           #{s\ 1552}#
                                           #{mod\ 1554}#)
                                         (if (eqv? #{ftype\ 1581}#
                                                   'define)
                                           (let ((#{tmp\ 1613}# #{e\ 1549}#))
                                             (let ((#{tmp\ 1614}#
                                                     ($sc-dispatch
                                                       #{tmp\ 1613}#
                                                       '(any any any))))
                                               (if (if #{tmp\ 1614}#
                                                     (@apply
                                                       (lambda (#{_\ 1618}#
                                                                #{name\ 1619}#
                                                                #{val\ 1620}#)
                                                         (#{id?\ 388}#
                                                           #{name\ 1619}#))
                                                       #{tmp\ 1614}#)
                                                     #f)
                                                 (@apply
                                                   (lambda (#{_\ 1624}#
                                                            #{name\ 1625}#
                                                            #{val\ 1626}#)
                                                     (values
                                                       'define-form
                                                       #{name\ 1625}#
                                                       #{val\ 1626}#
                                                       #{w\ 1551}#
                                                       #{s\ 1552}#
                                                       #{mod\ 1554}#))
                                                   #{tmp\ 1614}#)
                                                 (let ((#{tmp\ 1627}#
                                                         ($sc-dispatch
                                                           #{tmp\ 1613}#
                                                           '(any (any . any)
                                                                 any
                                                                 .
                                                                 each-any))))
                                                   (if (if #{tmp\ 1627}#
                                                         (@apply
                                                           (lambda (#{_\ 1633}#
                                                                    #{name\ 1634}#
                                                                    #{args\ 1635}#
                                                                    #{e1\ 1636}#
                                                                    #{e2\ 1637}#)
                                                             (if (#{id?\ 388}#
                                                                   #{name\ 1634}#)
                                                               (#{valid-bound-ids?\ 451}#
                                                                 (#{lambda-var-list\ 503}#
                                                                   #{args\ 1635}#))
                                                               #f))
                                                           #{tmp\ 1627}#)
                                                         #f)
                                                     (@apply
                                                       (lambda (#{_\ 1645}#
                                                                #{name\ 1646}#
                                                                #{args\ 1647}#
                                                                #{e1\ 1648}#
                                                                #{e2\ 1649}#)
                                                         (values
                                                           'define-form
                                                           (#{wrap\ 457}#
                                                             #{name\ 1646}#
                                                             #{w\ 1551}#
                                                             #{mod\ 1554}#)
                                                           (#{decorate-source\ 306}#
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
                                                                         #("i1640"
                                                                           "i1641"
                                                                           "i1642"
                                                                           "i1643"
                                                                           "i1644"))
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
                                                                         #("i1587"
                                                                           "i1588"
                                                                           "i1589"
                                                                           "i1590"
                                                                           "i1591"
                                                                           "i1592"))
                                                                       #(ribcage
                                                                         ()
                                                                         ()
                                                                         ())
                                                                       #(ribcage
                                                                         #(first)
                                                                         #((top))
                                                                         #("i1579"))
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
                                                                         #("i1556"
                                                                           "i1557"
                                                                           "i1558"
                                                                           "i1559"
                                                                           "i1560"
                                                                           "i1561"
                                                                           "i1562"))
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
                                                                         ("i502"
                                                                          "i500"
                                                                          "i498"
                                                                          "i496"
                                                                          "i494"
                                                                          "i492"
                                                                          "i490"
                                                                          "i488"
                                                                          "i486"
                                                                          "i484"
                                                                          "i482"
                                                                          "i480"
                                                                          "i478"
                                                                          "i476"
                                                                          "i474"
                                                                          "i472"
                                                                          "i470"
                                                                          "i468"
                                                                          "i466"
                                                                          "i464"
                                                                          "i462"
                                                                          "i460"
                                                                          "i458"
                                                                          "i456"
                                                                          "i454"
                                                                          "i452"
                                                                          "i450"
                                                                          "i448"
                                                                          "i446"
                                                                          "i444"
                                                                          "i442"
                                                                          "i440"
                                                                          "i438"
                                                                          "i436"
                                                                          "i434"
                                                                          "i432"
                                                                          "i431"
                                                                          "i430"
                                                                          "i428"
                                                                          "i427"
                                                                          "i426"
                                                                          "i425"
                                                                          "i424"
                                                                          "i422"
                                                                          "i420"
                                                                          "i418"
                                                                          "i416"
                                                                          "i414"
                                                                          "i412"
                                                                          "i410"
                                                                          "i408"
                                                                          "i405"
                                                                          "i403"
                                                                          "i402"
                                                                          "i401"
                                                                          "i400"
                                                                          "i399"
                                                                          "i398"
                                                                          "i396"
                                                                          "i394"
                                                                          "i392"
                                                                          "i390"
                                                                          "i389"
                                                                          "i387"
                                                                          "i385"
                                                                          "i383"
                                                                          "i381"
                                                                          "i379"
                                                                          "i377"
                                                                          "i375"
                                                                          "i374"
                                                                          "i372"
                                                                          "i370"
                                                                          "i369"
                                                                          "i368"
                                                                          "i366"
                                                                          "i365"
                                                                          "i363"
                                                                          "i361"
                                                                          "i359"
                                                                          "i357"
                                                                          "i355"
                                                                          "i353"
                                                                          "i351"
                                                                          "i349"
                                                                          "i347"
                                                                          "i345"
                                                                          "i343"
                                                                          "i341"
                                                                          "i339"
                                                                          "i337"
                                                                          "i335"
                                                                          "i333"
                                                                          "i331"
                                                                          "i329"
                                                                          "i327"
                                                                          "i325"
                                                                          "i323"
                                                                          "i321"
                                                                          "i319"
                                                                          "i317"
                                                                          "i315"
                                                                          "i313"
                                                                          "i311"
                                                                          "i309"
                                                                          "i307"
                                                                          "i305"
                                                                          "i303"
                                                                          "i301"
                                                                          "i300"
                                                                          "i298"
                                                                          "i296"
                                                                          "i294"
                                                                          "i292"
                                                                          "i290"
                                                                          "i288"
                                                                          "i286"
                                                                          "i284"
                                                                          "i282"
                                                                          "i279"
                                                                          "i277"
                                                                          "i275"
                                                                          "i273"
                                                                          "i271"
                                                                          "i269"
                                                                          "i267"
                                                                          "i265"
                                                                          "i263"
                                                                          "i261"
                                                                          "i259"
                                                                          "i257"
                                                                          "i255"
                                                                          "i253"
                                                                          "i251"
                                                                          "i249"
                                                                          "i247"
                                                                          "i245"))
                                                                       #(ribcage
                                                                         (define-structure
                                                                           define-expansion-accessors
                                                                           define-expansion-constructors
                                                                           and-map*)
                                                                         ((top)
                                                                          (top)
                                                                          (top)
                                                                          (top))
                                                                         ("i39"
                                                                          "i38"
                                                                          "i37"
                                                                          "i35")))
                                                                      (hygiene
                                                                        guile))
                                                                   (#{wrap\ 457}#
                                                                     (cons #{args\ 1647}#
                                                                           (cons #{e1\ 1648}#
                                                                                 #{e2\ 1649}#))
                                                                     #{w\ 1551}#
                                                                     #{mod\ 1554}#))
                                                             #{s\ 1552}#)
                                                           '(())
                                                           #{s\ 1552}#
                                                           #{mod\ 1554}#))
                                                       #{tmp\ 1627}#)
                                                     (let ((#{tmp\ 1652}#
                                                             ($sc-dispatch
                                                               #{tmp\ 1613}#
                                                               '(any any))))
                                                       (if (if #{tmp\ 1652}#
                                                             (@apply
                                                               (lambda (#{_\ 1655}#
                                                                        #{name\ 1656}#)
                                                                 (#{id?\ 388}#
                                                                   #{name\ 1656}#))
                                                               #{tmp\ 1652}#)
                                                             #f)
                                                         (@apply
                                                           (lambda (#{_\ 1659}#
                                                                    #{name\ 1660}#)
                                                             (values
                                                               'define-form
                                                               (#{wrap\ 457}#
                                                                 #{name\ 1660}#
                                                                 #{w\ 1551}#
                                                                 #{mod\ 1554}#)
                                                               '(#(syntax-object
                                                                   if
                                                                   ((top)
                                                                    #(ribcage
                                                                      #(_ name)
                                                                      #((top)
                                                                        (top))
                                                                      #("i1657"
                                                                        "i1658"))
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
                                                                      #("i1587"
                                                                        "i1588"
                                                                        "i1589"
                                                                        "i1590"
                                                                        "i1591"
                                                                        "i1592"))
                                                                    #(ribcage
                                                                      ()
                                                                      ()
                                                                      ())
                                                                    #(ribcage
                                                                      #(first)
                                                                      #((top))
                                                                      #("i1579"))
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
                                                                      #("i1556"
                                                                        "i1557"
                                                                        "i1558"
                                                                        "i1559"
                                                                        "i1560"
                                                                        "i1561"
                                                                        "i1562"))
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
                                                                      ("i502"
                                                                       "i500"
                                                                       "i498"
                                                                       "i496"
                                                                       "i494"
                                                                       "i492"
                                                                       "i490"
                                                                       "i488"
                                                                       "i486"
                                                                       "i484"
                                                                       "i482"
                                                                       "i480"
                                                                       "i478"
                                                                       "i476"
                                                                       "i474"
                                                                       "i472"
                                                                       "i470"
                                                                       "i468"
                                                                       "i466"
                                                                       "i464"
                                                                       "i462"
                                                                       "i460"
                                                                       "i458"
                                                                       "i456"
                                                                       "i454"
                                                                       "i452"
                                                                       "i450"
                                                                       "i448"
                                                                       "i446"
                                                                       "i444"
                                                                       "i442"
                                                                       "i440"
                                                                       "i438"
                                                                       "i436"
                                                                       "i434"
                                                                       "i432"
                                                                       "i431"
                                                                       "i430"
                                                                       "i428"
                                                                       "i427"
                                                                       "i426"
                                                                       "i425"
                                                                       "i424"
                                                                       "i422"
                                                                       "i420"
                                                                       "i418"
                                                                       "i416"
                                                                       "i414"
                                                                       "i412"
                                                                       "i410"
                                                                       "i408"
                                                                       "i405"
                                                                       "i403"
                                                                       "i402"
                                                                       "i401"
                                                                       "i400"
                                                                       "i399"
                                                                       "i398"
                                                                       "i396"
                                                                       "i394"
                                                                       "i392"
                                                                       "i390"
                                                                       "i389"
                                                                       "i387"
                                                                       "i385"
                                                                       "i383"
                                                                       "i381"
                                                                       "i379"
                                                                       "i377"
                                                                       "i375"
                                                                       "i374"
                                                                       "i372"
                                                                       "i370"
                                                                       "i369"
                                                                       "i368"
                                                                       "i366"
                                                                       "i365"
                                                                       "i363"
                                                                       "i361"
                                                                       "i359"
                                                                       "i357"
                                                                       "i355"
                                                                       "i353"
                                                                       "i351"
                                                                       "i349"
                                                                       "i347"
                                                                       "i345"
                                                                       "i343"
                                                                       "i341"
                                                                       "i339"
                                                                       "i337"
                                                                       "i335"
                                                                       "i333"
                                                                       "i331"
                                                                       "i329"
                                                                       "i327"
                                                                       "i325"
                                                                       "i323"
                                                                       "i321"
                                                                       "i319"
                                                                       "i317"
                                                                       "i315"
                                                                       "i313"
                                                                       "i311"
                                                                       "i309"
                                                                       "i307"
                                                                       "i305"
                                                                       "i303"
                                                                       "i301"
                                                                       "i300"
                                                                       "i298"
                                                                       "i296"
                                                                       "i294"
                                                                       "i292"
                                                                       "i290"
                                                                       "i288"
                                                                       "i286"
                                                                       "i284"
                                                                       "i282"
                                                                       "i279"
                                                                       "i277"
                                                                       "i275"
                                                                       "i273"
                                                                       "i271"
                                                                       "i269"
                                                                       "i267"
                                                                       "i265"
                                                                       "i263"
                                                                       "i261"
                                                                       "i259"
                                                                       "i257"
                                                                       "i255"
                                                                       "i253"
                                                                       "i251"
                                                                       "i249"
                                                                       "i247"
                                                                       "i245"))
                                                                    #(ribcage
                                                                      (define-structure
                                                                        define-expansion-accessors
                                                                        define-expansion-constructors
                                                                        and-map*)
                                                                      ((top)
                                                                       (top)
                                                                       (top)
                                                                       (top))
                                                                      ("i39"
                                                                       "i38"
                                                                       "i37"
                                                                       "i35")))
                                                                   (hygiene
                                                                     guile))
                                                                 #(syntax-object
                                                                   #f
                                                                   ((top)
                                                                    #(ribcage
                                                                      #(_ name)
                                                                      #((top)
                                                                        (top))
                                                                      #("i1657"
                                                                        "i1658"))
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
                                                                      #("i1587"
                                                                        "i1588"
                                                                        "i1589"
                                                                        "i1590"
                                                                        "i1591"
                                                                        "i1592"))
                                                                    #(ribcage
                                                                      ()
                                                                      ()
                                                                      ())
                                                                    #(ribcage
                                                                      #(first)
                                                                      #((top))
                                                                      #("i1579"))
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
                                                                      #("i1556"
                                                                        "i1557"
                                                                        "i1558"
                                                                        "i1559"
                                                                        "i1560"
                                                                        "i1561"
                                                                        "i1562"))
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
                                                                      ("i502"
                                                                       "i500"
                                                                       "i498"
                                                                       "i496"
                                                                       "i494"
                                                                       "i492"
                                                                       "i490"
                                                                       "i488"
                                                                       "i486"
                                                                       "i484"
                                                                       "i482"
                                                                       "i480"
                                                                       "i478"
                                                                       "i476"
                                                                       "i474"
                                                                       "i472"
                                                                       "i470"
                                                                       "i468"
                                                                       "i466"
                                                                       "i464"
                                                                       "i462"
                                                                       "i460"
                                                                       "i458"
                                                                       "i456"
                                                                       "i454"
                                                                       "i452"
                                                                       "i450"
                                                                       "i448"
                                                                       "i446"
                                                                       "i444"
                                                                       "i442"
                                                                       "i440"
                                                                       "i438"
                                                                       "i436"
                                                                       "i434"
                                                                       "i432"
                                                                       "i431"
                                                                       "i430"
                                                                       "i428"
                                                                       "i427"
                                                                       "i426"
                                                                       "i425"
                                                                       "i424"
                                                                       "i422"
                                                                       "i420"
                                                                       "i418"
                                                                       "i416"
                                                                       "i414"
                                                                       "i412"
                                                                       "i410"
                                                                       "i408"
                                                                       "i405"
                                                                       "i403"
                                                                       "i402"
                                                                       "i401"
                                                                       "i400"
                                                                       "i399"
                                                                       "i398"
                                                                       "i396"
                                                                       "i394"
                                                                       "i392"
                                                                       "i390"
                                                                       "i389"
                                                                       "i387"
                                                                       "i385"
                                                                       "i383"
                                                                       "i381"
                                                                       "i379"
                                                                       "i377"
                                                                       "i375"
                                                                       "i374"
                                                                       "i372"
                                                                       "i370"
                                                                       "i369"
                                                                       "i368"
                                                                       "i366"
                                                                       "i365"
                                                                       "i363"
                                                                       "i361"
                                                                       "i359"
                                                                       "i357"
                                                                       "i355"
                                                                       "i353"
                                                                       "i351"
                                                                       "i349"
                                                                       "i347"
                                                                       "i345"
                                                                       "i343"
                                                                       "i341"
                                                                       "i339"
                                                                       "i337"
                                                                       "i335"
                                                                       "i333"
                                                                       "i331"
                                                                       "i329"
                                                                       "i327"
                                                                       "i325"
                                                                       "i323"
                                                                       "i321"
                                                                       "i319"
                                                                       "i317"
                                                                       "i315"
                                                                       "i313"
                                                                       "i311"
                                                                       "i309"
                                                                       "i307"
                                                                       "i305"
                                                                       "i303"
                                                                       "i301"
                                                                       "i300"
                                                                       "i298"
                                                                       "i296"
                                                                       "i294"
                                                                       "i292"
                                                                       "i290"
                                                                       "i288"
                                                                       "i286"
                                                                       "i284"
                                                                       "i282"
                                                                       "i279"
                                                                       "i277"
                                                                       "i275"
                                                                       "i273"
                                                                       "i271"
                                                                       "i269"
                                                                       "i267"
                                                                       "i265"
                                                                       "i263"
                                                                       "i261"
                                                                       "i259"
                                                                       "i257"
                                                                       "i255"
                                                                       "i253"
                                                                       "i251"
                                                                       "i249"
                                                                       "i247"
                                                                       "i245"))
                                                                    #(ribcage
                                                                      (define-structure
                                                                        define-expansion-accessors
                                                                        define-expansion-constructors
                                                                        and-map*)
                                                                      ((top)
                                                                       (top)
                                                                       (top)
                                                                       (top))
                                                                      ("i39"
                                                                       "i38"
                                                                       "i37"
                                                                       "i35")))
                                                                   (hygiene
                                                                     guile))
                                                                 #(syntax-object
                                                                   #f
                                                                   ((top)
                                                                    #(ribcage
                                                                      #(_ name)
                                                                      #((top)
                                                                        (top))
                                                                      #("i1657"
                                                                        "i1658"))
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
                                                                      #("i1587"
                                                                        "i1588"
                                                                        "i1589"
                                                                        "i1590"
                                                                        "i1591"
                                                                        "i1592"))
                                                                    #(ribcage
                                                                      ()
                                                                      ()
                                                                      ())
                                                                    #(ribcage
                                                                      #(first)
                                                                      #((top))
                                                                      #("i1579"))
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
                                                                      #("i1556"
                                                                        "i1557"
                                                                        "i1558"
                                                                        "i1559"
                                                                        "i1560"
                                                                        "i1561"
                                                                        "i1562"))
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
                                                                      ("i502"
                                                                       "i500"
                                                                       "i498"
                                                                       "i496"
                                                                       "i494"
                                                                       "i492"
                                                                       "i490"
                                                                       "i488"
                                                                       "i486"
                                                                       "i484"
                                                                       "i482"
                                                                       "i480"
                                                                       "i478"
                                                                       "i476"
                                                                       "i474"
                                                                       "i472"
                                                                       "i470"
                                                                       "i468"
                                                                       "i466"
                                                                       "i464"
                                                                       "i462"
                                                                       "i460"
                                                                       "i458"
                                                                       "i456"
                                                                       "i454"
                                                                       "i452"
                                                                       "i450"
                                                                       "i448"
                                                                       "i446"
                                                                       "i444"
                                                                       "i442"
                                                                       "i440"
                                                                       "i438"
                                                                       "i436"
                                                                       "i434"
                                                                       "i432"
                                                                       "i431"
                                                                       "i430"
                                                                       "i428"
                                                                       "i427"
                                                                       "i426"
                                                                       "i425"
                                                                       "i424"
                                                                       "i422"
                                                                       "i420"
                                                                       "i418"
                                                                       "i416"
                                                                       "i414"
                                                                       "i412"
                                                                       "i410"
                                                                       "i408"
                                                                       "i405"
                                                                       "i403"
                                                                       "i402"
                                                                       "i401"
                                                                       "i400"
                                                                       "i399"
                                                                       "i398"
                                                                       "i396"
                                                                       "i394"
                                                                       "i392"
                                                                       "i390"
                                                                       "i389"
                                                                       "i387"
                                                                       "i385"
                                                                       "i383"
                                                                       "i381"
                                                                       "i379"
                                                                       "i377"
                                                                       "i375"
                                                                       "i374"
                                                                       "i372"
                                                                       "i370"
                                                                       "i369"
                                                                       "i368"
                                                                       "i366"
                                                                       "i365"
                                                                       "i363"
                                                                       "i361"
                                                                       "i359"
                                                                       "i357"
                                                                       "i355"
                                                                       "i353"
                                                                       "i351"
                                                                       "i349"
                                                                       "i347"
                                                                       "i345"
                                                                       "i343"
                                                                       "i341"
                                                                       "i339"
                                                                       "i337"
                                                                       "i335"
                                                                       "i333"
                                                                       "i331"
                                                                       "i329"
                                                                       "i327"
                                                                       "i325"
                                                                       "i323"
                                                                       "i321"
                                                                       "i319"
                                                                       "i317"
                                                                       "i315"
                                                                       "i313"
                                                                       "i311"
                                                                       "i309"
                                                                       "i307"
                                                                       "i305"
                                                                       "i303"
                                                                       "i301"
                                                                       "i300"
                                                                       "i298"
                                                                       "i296"
                                                                       "i294"
                                                                       "i292"
                                                                       "i290"
                                                                       "i288"
                                                                       "i286"
                                                                       "i284"
                                                                       "i282"
                                                                       "i279"
                                                                       "i277"
                                                                       "i275"
                                                                       "i273"
                                                                       "i271"
                                                                       "i269"
                                                                       "i267"
                                                                       "i265"
                                                                       "i263"
                                                                       "i261"
                                                                       "i259"
                                                                       "i257"
                                                                       "i255"
                                                                       "i253"
                                                                       "i251"
                                                                       "i249"
                                                                       "i247"
                                                                       "i245"))
                                                                    #(ribcage
                                                                      (define-structure
                                                                        define-expansion-accessors
                                                                        define-expansion-constructors
                                                                        and-map*)
                                                                      ((top)
                                                                       (top)
                                                                       (top)
                                                                       (top))
                                                                      ("i39"
                                                                       "i38"
                                                                       "i37"
                                                                       "i35")))
                                                                   (hygiene
                                                                     guile)))
                                                               '(())
                                                               #{s\ 1552}#
                                                               #{mod\ 1554}#))
                                                           #{tmp\ 1652}#)
                                                         (syntax-violation
                                                           #f
                                                           "source expression failed to match any pattern"
                                                           #{tmp\ 1613}#))))))))
                                           (if (eqv? #{ftype\ 1581}#
                                                     'define-syntax)
                                             (let ((#{tmp\ 1663}# #{e\ 1549}#))
                                               (let ((#{tmp\ 1664}#
                                                       ($sc-dispatch
                                                         #{tmp\ 1663}#
                                                         '(any any any))))
                                                 (if (if #{tmp\ 1664}#
                                                       (@apply
                                                         (lambda (#{_\ 1668}#
                                                                  #{name\ 1669}#
                                                                  #{val\ 1670}#)
                                                           (#{id?\ 388}#
                                                             #{name\ 1669}#))
                                                         #{tmp\ 1664}#)
                                                       #f)
                                                   (@apply
                                                     (lambda (#{_\ 1674}#
                                                              #{name\ 1675}#
                                                              #{val\ 1676}#)
                                                       (values
                                                         'define-syntax-form
                                                         #{name\ 1675}#
                                                         #{val\ 1676}#
                                                         #{w\ 1551}#
                                                         #{s\ 1552}#
                                                         #{mod\ 1554}#))
                                                     #{tmp\ 1664}#)
                                                   (syntax-violation
                                                     #f
                                                     "source expression failed to match any pattern"
                                                     #{tmp\ 1663}#))))
                                             (values
                                               'call
                                               #f
                                               #{e\ 1549}#
                                               #{w\ 1551}#
                                               #{s\ 1552}#
                                               #{mod\ 1554}#)))))))))))))))
                 (if (#{syntax-object?\ 352}# #{e\ 1549}#)
                   (#{syntax-type\ 469}#
                     (#{syntax-object-expression\ 354}# #{e\ 1549}#)
                     #{r\ 1550}#
                     (#{join-wraps\ 439}#
                       #{w\ 1551}#
                       (#{syntax-object-wrap\ 356}# #{e\ 1549}#))
                     (begin
                       (let ((#{t\ 1682}#
                               (#{source-annotation\ 367}# #{e\ 1549}#)))
                         (if #{t\ 1682}# #{t\ 1682}# #{s\ 1552}#)))
                     #{rib\ 1553}#
                     (begin
                       (let ((#{t\ 1686}#
                               (#{syntax-object-module\ 358}# #{e\ 1549}#)))
                         (if #{t\ 1686}# #{t\ 1686}# #{mod\ 1554}#)))
                     #{for-car?\ 1555}#)
                   (if (self-evaluating? #{e\ 1549}#)
                     (values
                       'constant
                       #f
                       #{e\ 1549}#
                       #{w\ 1551}#
                       #{s\ 1552}#
                       #{mod\ 1554}#)
                     (values
                       'other
                       #f
                       #{e\ 1549}#
                       #{w\ 1551}#
                       #{s\ 1552}#
                       #{mod\ 1554}#)))))))
         (#{chi-top\ 471}#
           (lambda (#{e\ 1691}#
                    #{r\ 1692}#
                    #{w\ 1693}#
                    #{m\ 1694}#
                    #{esew\ 1695}#
                    #{mod\ 1696}#)
             (call-with-values
               (lambda ()
                 (#{syntax-type\ 469}#
                   #{e\ 1691}#
                   #{r\ 1692}#
                   #{w\ 1693}#
                   (#{source-annotation\ 367}# #{e\ 1691}#)
                   #f
                   #{mod\ 1696}#
                   #f))
               (lambda (#{type\ 1717}#
                        #{value\ 1718}#
                        #{e\ 1719}#
                        #{w\ 1720}#
                        #{s\ 1721}#
                        #{mod\ 1722}#)
                 (if (eqv? #{type\ 1717}# (quote begin-form))
                   (let ((#{tmp\ 1730}# #{e\ 1719}#))
                     (let ((#{tmp\ 1731}#
                             ($sc-dispatch #{tmp\ 1730}# (quote (any)))))
                       (if #{tmp\ 1731}#
                         (@apply
                           (lambda (#{_\ 1733}#) (#{chi-void\ 487}#))
                           #{tmp\ 1731}#)
                         (let ((#{tmp\ 1734}#
                                 ($sc-dispatch
                                   #{tmp\ 1730}#
                                   '(any any . each-any))))
                           (if #{tmp\ 1734}#
                             (@apply
                               (lambda (#{_\ 1738}# #{e1\ 1739}# #{e2\ 1740}#)
                                 (#{chi-top-sequence\ 463}#
                                   (cons #{e1\ 1739}# #{e2\ 1740}#)
                                   #{r\ 1692}#
                                   #{w\ 1720}#
                                   #{s\ 1721}#
                                   #{m\ 1694}#
                                   #{esew\ 1695}#
                                   #{mod\ 1722}#))
                               #{tmp\ 1734}#)
                             (syntax-violation
                               #f
                               "source expression failed to match any pattern"
                               #{tmp\ 1730}#))))))
                   (if (eqv? #{type\ 1717}# (quote local-syntax-form))
                     (#{chi-local-syntax\ 483}#
                       #{value\ 1718}#
                       #{e\ 1719}#
                       #{r\ 1692}#
                       #{w\ 1720}#
                       #{s\ 1721}#
                       #{mod\ 1722}#
                       (lambda (#{body\ 1743}#
                                #{r\ 1744}#
                                #{w\ 1745}#
                                #{s\ 1746}#
                                #{mod\ 1747}#)
                         (#{chi-top-sequence\ 463}#
                           #{body\ 1743}#
                           #{r\ 1744}#
                           #{w\ 1745}#
                           #{s\ 1746}#
                           #{m\ 1694}#
                           #{esew\ 1695}#
                           #{mod\ 1747}#)))
                     (if (eqv? #{type\ 1717}# (quote eval-when-form))
                       (let ((#{tmp\ 1754}# #{e\ 1719}#))
                         (let ((#{tmp\ 1755}#
                                 ($sc-dispatch
                                   #{tmp\ 1754}#
                                   '(any each-any any . each-any))))
                           (if #{tmp\ 1755}#
                             (@apply
                               (lambda (#{_\ 1760}#
                                        #{x\ 1761}#
                                        #{e1\ 1762}#
                                        #{e2\ 1763}#)
                                 (begin
                                   (let ((#{when-list\ 1766}#
                                           (#{chi-when-list\ 467}#
                                             #{e\ 1719}#
                                             #{x\ 1761}#
                                             #{w\ 1720}#))
                                         (#{body\ 1767}#
                                           (cons #{e1\ 1762}# #{e2\ 1763}#)))
                                     (if (eq? #{m\ 1694}# (quote e))
                                       (if (memq 'eval
                                                 #{when-list\ 1766}#)
                                         (#{chi-top-sequence\ 463}#
                                           #{body\ 1767}#
                                           #{r\ 1692}#
                                           #{w\ 1720}#
                                           #{s\ 1721}#
                                           (if (memq 'expand
                                                     #{when-list\ 1766}#)
                                             'c&e
                                             'e)
                                           '(eval)
                                           #{mod\ 1722}#)
                                         (begin
                                           (if (memq 'expand
                                                     #{when-list\ 1766}#)
                                             (#{top-level-eval-hook\ 297}#
                                               (#{chi-top-sequence\ 463}#
                                                 #{body\ 1767}#
                                                 #{r\ 1692}#
                                                 #{w\ 1720}#
                                                 #{s\ 1721}#
                                                 'e
                                                 '(eval)
                                                 #{mod\ 1722}#)
                                               #{mod\ 1722}#))
                                           (#{chi-void\ 487}#)))
                                       (if (memq 'load
                                                 #{when-list\ 1766}#)
                                         (if (begin
                                               (let ((#{t\ 1776}#
                                                       (memq 'compile
                                                             #{when-list\ 1766}#)))
                                                 (if #{t\ 1776}#
                                                   #{t\ 1776}#
                                                   (begin
                                                     (let ((#{t\ 1779}#
                                                             (memq 'expand
                                                                   #{when-list\ 1766}#)))
                                                       (if #{t\ 1779}#
                                                         #{t\ 1779}#
                                                         (if (eq? #{m\ 1694}#
                                                                  'c&e)
                                                           (memq 'eval
                                                                 #{when-list\ 1766}#)
                                                           #f)))))))
                                           (#{chi-top-sequence\ 463}#
                                             #{body\ 1767}#
                                             #{r\ 1692}#
                                             #{w\ 1720}#
                                             #{s\ 1721}#
                                             'c&e
                                             '(compile load)
                                             #{mod\ 1722}#)
                                           (if (if (eq? #{m\ 1694}# (quote c))
                                                 #t
                                                 (eq? #{m\ 1694}# (quote c&e)))
                                             (#{chi-top-sequence\ 463}#
                                               #{body\ 1767}#
                                               #{r\ 1692}#
                                               #{w\ 1720}#
                                               #{s\ 1721}#
                                               'c
                                               '(load)
                                               #{mod\ 1722}#)
                                             (#{chi-void\ 487}#)))
                                         (if (begin
                                               (let ((#{t\ 1787}#
                                                       (memq 'compile
                                                             #{when-list\ 1766}#)))
                                                 (if #{t\ 1787}#
                                                   #{t\ 1787}#
                                                   (begin
                                                     (let ((#{t\ 1790}#
                                                             (memq 'expand
                                                                   #{when-list\ 1766}#)))
                                                       (if #{t\ 1790}#
                                                         #{t\ 1790}#
                                                         (if (eq? #{m\ 1694}#
                                                                  'c&e)
                                                           (memq 'eval
                                                                 #{when-list\ 1766}#)
                                                           #f)))))))
                                           (begin
                                             (#{top-level-eval-hook\ 297}#
                                               (#{chi-top-sequence\ 463}#
                                                 #{body\ 1767}#
                                                 #{r\ 1692}#
                                                 #{w\ 1720}#
                                                 #{s\ 1721}#
                                                 'e
                                                 '(eval)
                                                 #{mod\ 1722}#)
                                               #{mod\ 1722}#)
                                             (#{chi-void\ 487}#))
                                           (#{chi-void\ 487}#)))))))
                               #{tmp\ 1755}#)
                             (syntax-violation
                               #f
                               "source expression failed to match any pattern"
                               #{tmp\ 1754}#))))
                       (if (eqv? #{type\ 1717}# (quote define-syntax-form))
                         (begin
                           (let ((#{n\ 1798}#
                                   (#{id-var-name\ 445}#
                                     #{value\ 1718}#
                                     #{w\ 1720}#))
                                 (#{r\ 1799}#
                                   (#{macros-only-env\ 380}# #{r\ 1692}#)))
                             (if (eqv? #{m\ 1694}# (quote c))
                               (if (memq (quote compile) #{esew\ 1695}#)
                                 (begin
                                   (let ((#{e\ 1802}#
                                           (#{chi-install-global\ 465}#
                                             #{n\ 1798}#
                                             (#{chi\ 473}#
                                               #{e\ 1719}#
                                               #{r\ 1799}#
                                               #{w\ 1720}#
                                               #{mod\ 1722}#))))
                                     (begin
                                       (#{top-level-eval-hook\ 297}#
                                         #{e\ 1802}#
                                         #{mod\ 1722}#)
                                       (if (memq (quote load) #{esew\ 1695}#)
                                         #{e\ 1802}#
                                         (#{chi-void\ 487}#)))))
                                 (if (memq (quote load) #{esew\ 1695}#)
                                   (#{chi-install-global\ 465}#
                                     #{n\ 1798}#
                                     (#{chi\ 473}#
                                       #{e\ 1719}#
                                       #{r\ 1799}#
                                       #{w\ 1720}#
                                       #{mod\ 1722}#))
                                   (#{chi-void\ 487}#)))
                               (if (eqv? #{m\ 1694}# (quote c&e))
                                 (begin
                                   (let ((#{e\ 1805}#
                                           (#{chi-install-global\ 465}#
                                             #{n\ 1798}#
                                             (#{chi\ 473}#
                                               #{e\ 1719}#
                                               #{r\ 1799}#
                                               #{w\ 1720}#
                                               #{mod\ 1722}#))))
                                     (begin
                                       (#{top-level-eval-hook\ 297}#
                                         #{e\ 1805}#
                                         #{mod\ 1722}#)
                                       #{e\ 1805}#)))
                                 (begin
                                   (if (memq (quote eval) #{esew\ 1695}#)
                                     (#{top-level-eval-hook\ 297}#
                                       (#{chi-install-global\ 465}#
                                         #{n\ 1798}#
                                         (#{chi\ 473}#
                                           #{e\ 1719}#
                                           #{r\ 1799}#
                                           #{w\ 1720}#
                                           #{mod\ 1722}#))
                                       #{mod\ 1722}#))
                                   (#{chi-void\ 487}#))))))
                         (if (eqv? #{type\ 1717}# (quote define-form))
                           (begin
                             (let ((#{n\ 1810}#
                                     (#{id-var-name\ 445}#
                                       #{value\ 1718}#
                                       #{w\ 1720}#)))
                               (begin
                                 (let ((#{type\ 1812}#
                                         (#{binding-type\ 371}#
                                           (#{lookup\ 382}#
                                             #{n\ 1810}#
                                             #{r\ 1692}#
                                             #{mod\ 1722}#))))
                                   (if (if (eqv? #{type\ 1812}# (quote global))
                                         #t
                                         (if (eqv? #{type\ 1812}# (quote core))
                                           #t
                                           (if (eqv? #{type\ 1812}#
                                                     'macro)
                                             #t
                                             (eqv? #{type\ 1812}#
                                                   'module-ref))))
                                     (begin
                                       (if (if (if (eq? #{m\ 1694}# (quote c))
                                                 #t
                                                 (eq? #{m\ 1694}# (quote c&e)))
                                             (if (not (module-local-variable
                                                        (current-module)
                                                        #{n\ 1810}#))
                                               (current-module)
                                               #f)
                                             #f)
                                         (begin
                                           (let ((#{old\ 1818}#
                                                   (module-variable
                                                     (current-module)
                                                     #{n\ 1810}#)))
                                             (module-define!
                                               (current-module)
                                               #{n\ 1810}#
                                               (if (variable? #{old\ 1818}#)
                                                 (variable-ref #{old\ 1818}#)
                                                 #f)))))
                                       (begin
                                         (let ((#{x\ 1821}#
                                                 (#{build-global-definition\ 328}#
                                                   #{s\ 1721}#
                                                   #{n\ 1810}#
                                                   (#{chi\ 473}#
                                                     #{e\ 1719}#
                                                     #{r\ 1692}#
                                                     #{w\ 1720}#
                                                     #{mod\ 1722}#))))
                                           (begin
                                             (if (eq? #{m\ 1694}# (quote c&e))
                                               (#{top-level-eval-hook\ 297}#
                                                 #{x\ 1821}#
                                                 #{mod\ 1722}#))
                                             #{x\ 1821}#))))
                                     (if (eqv? #{type\ 1812}#
                                               'displaced-lexical)
                                       (syntax-violation
                                         #f
                                         "identifier out of context"
                                         #{e\ 1719}#
                                         (#{wrap\ 457}#
                                           #{value\ 1718}#
                                           #{w\ 1720}#
                                           #{mod\ 1722}#))
                                       (syntax-violation
                                         #f
                                         "cannot define keyword at top level"
                                         #{e\ 1719}#
                                         (#{wrap\ 457}#
                                           #{value\ 1718}#
                                           #{w\ 1720}#
                                           #{mod\ 1722}#))))))))
                           (begin
                             (let ((#{x\ 1827}#
                                     (#{chi-expr\ 475}#
                                       #{type\ 1717}#
                                       #{value\ 1718}#
                                       #{e\ 1719}#
                                       #{r\ 1692}#
                                       #{w\ 1720}#
                                       #{s\ 1721}#
                                       #{mod\ 1722}#)))
                               (begin
                                 (if (eq? #{m\ 1694}# (quote c&e))
                                   (#{top-level-eval-hook\ 297}#
                                     #{x\ 1827}#
                                     #{mod\ 1722}#))
                                 #{x\ 1827}#))))))))))))
         (#{chi\ 473}#
           (lambda (#{e\ 1828}#
                    #{r\ 1829}#
                    #{w\ 1830}#
                    #{mod\ 1831}#)
             (call-with-values
               (lambda ()
                 (#{syntax-type\ 469}#
                   #{e\ 1828}#
                   #{r\ 1829}#
                   #{w\ 1830}#
                   (#{source-annotation\ 367}# #{e\ 1828}#)
                   #f
                   #{mod\ 1831}#
                   #f))
               (lambda (#{type\ 1836}#
                        #{value\ 1837}#
                        #{e\ 1838}#
                        #{w\ 1839}#
                        #{s\ 1840}#
                        #{mod\ 1841}#)
                 (#{chi-expr\ 475}#
                   #{type\ 1836}#
                   #{value\ 1837}#
                   #{e\ 1838}#
                   #{r\ 1829}#
                   #{w\ 1839}#
                   #{s\ 1840}#
                   #{mod\ 1841}#)))))
         (#{chi-expr\ 475}#
           (lambda (#{type\ 1848}#
                    #{value\ 1849}#
                    #{e\ 1850}#
                    #{r\ 1851}#
                    #{w\ 1852}#
                    #{s\ 1853}#
                    #{mod\ 1854}#)
             (if (eqv? #{type\ 1848}# (quote lexical))
               (#{build-lexical-reference\ 318}#
                 'value
                 #{s\ 1853}#
                 #{e\ 1850}#
                 #{value\ 1849}#)
               (if (if (eqv? #{type\ 1848}# (quote core))
                     #t
                     (eqv? #{type\ 1848}# (quote core-form)))
                 (#{value\ 1849}#
                   #{e\ 1850}#
                   #{r\ 1851}#
                   #{w\ 1852}#
                   #{s\ 1853}#
                   #{mod\ 1854}#)
                 (if (eqv? #{type\ 1848}# (quote module-ref))
                   (call-with-values
                     (lambda ()
                       (#{value\ 1849}#
                         #{e\ 1850}#
                         #{r\ 1851}#
                         #{w\ 1852}#))
                     (lambda (#{e\ 1865}#
                              #{r\ 1866}#
                              #{w\ 1867}#
                              #{s\ 1868}#
                              #{mod\ 1869}#)
                       (#{chi\ 473}#
                         #{e\ 1865}#
                         #{r\ 1866}#
                         #{w\ 1867}#
                         #{mod\ 1869}#)))
                   (if (eqv? #{type\ 1848}# (quote lexical-call))
                     (#{chi-application\ 477}#
                       (begin
                         (let ((#{id\ 1877}# (car #{e\ 1850}#)))
                           (#{build-lexical-reference\ 318}#
                             'fun
                             (#{source-annotation\ 367}# #{id\ 1877}#)
                             (if (#{syntax-object?\ 352}# #{id\ 1877}#)
                               (syntax->datum #{id\ 1877}#)
                               #{id\ 1877}#)
                             #{value\ 1849}#)))
                       #{e\ 1850}#
                       #{r\ 1851}#
                       #{w\ 1852}#
                       #{s\ 1853}#
                       #{mod\ 1854}#)
                     (if (eqv? #{type\ 1848}# (quote global-call))
                       (#{chi-application\ 477}#
                         (#{build-global-reference\ 324}#
                           (#{source-annotation\ 367}# (car #{e\ 1850}#))
                           (if (#{syntax-object?\ 352}# #{value\ 1849}#)
                             (#{syntax-object-expression\ 354}#
                               #{value\ 1849}#)
                             #{value\ 1849}#)
                           (if (#{syntax-object?\ 352}# #{value\ 1849}#)
                             (#{syntax-object-module\ 358}# #{value\ 1849}#)
                             #{mod\ 1854}#))
                         #{e\ 1850}#
                         #{r\ 1851}#
                         #{w\ 1852}#
                         #{s\ 1853}#
                         #{mod\ 1854}#)
                       (if (eqv? #{type\ 1848}# (quote constant))
                         (#{build-data\ 338}#
                           #{s\ 1853}#
                           (#{strip\ 499}#
                             (#{source-wrap\ 459}#
                               #{e\ 1850}#
                               #{w\ 1852}#
                               #{s\ 1853}#
                               #{mod\ 1854}#)
                             '(())))
                         (if (eqv? #{type\ 1848}# (quote global))
                           (#{build-global-reference\ 324}#
                             #{s\ 1853}#
                             #{value\ 1849}#
                             #{mod\ 1854}#)
                           (if (eqv? #{type\ 1848}# (quote call))
                             (#{chi-application\ 477}#
                               (#{chi\ 473}#
                                 (car #{e\ 1850}#)
                                 #{r\ 1851}#
                                 #{w\ 1852}#
                                 #{mod\ 1854}#)
                               #{e\ 1850}#
                               #{r\ 1851}#
                               #{w\ 1852}#
                               #{s\ 1853}#
                               #{mod\ 1854}#)
                             (if (eqv? #{type\ 1848}# (quote begin-form))
                               (let ((#{tmp\ 1884}# #{e\ 1850}#))
                                 (let ((#{tmp\ 1885}#
                                         ($sc-dispatch
                                           #{tmp\ 1884}#
                                           '(any any . each-any))))
                                   (if #{tmp\ 1885}#
                                     (@apply
                                       (lambda (#{_\ 1889}#
                                                #{e1\ 1890}#
                                                #{e2\ 1891}#)
                                         (#{chi-sequence\ 461}#
                                           (cons #{e1\ 1890}# #{e2\ 1891}#)
                                           #{r\ 1851}#
                                           #{w\ 1852}#
                                           #{s\ 1853}#
                                           #{mod\ 1854}#))
                                       #{tmp\ 1885}#)
                                     (syntax-violation
                                       #f
                                       "source expression failed to match any pattern"
                                       #{tmp\ 1884}#))))
                               (if (eqv? #{type\ 1848}#
                                         'local-syntax-form)
                                 (#{chi-local-syntax\ 483}#
                                   #{value\ 1849}#
                                   #{e\ 1850}#
                                   #{r\ 1851}#
                                   #{w\ 1852}#
                                   #{s\ 1853}#
                                   #{mod\ 1854}#
                                   #{chi-sequence\ 461}#)
                                 (if (eqv? #{type\ 1848}#
                                           'eval-when-form)
                                   (let ((#{tmp\ 1895}# #{e\ 1850}#))
                                     (let ((#{tmp\ 1896}#
                                             ($sc-dispatch
                                               #{tmp\ 1895}#
                                               '(any each-any
                                                     any
                                                     .
                                                     each-any))))
                                       (if #{tmp\ 1896}#
                                         (@apply
                                           (lambda (#{_\ 1901}#
                                                    #{x\ 1902}#
                                                    #{e1\ 1903}#
                                                    #{e2\ 1904}#)
                                             (begin
                                               (let ((#{when-list\ 1906}#
                                                       (#{chi-when-list\ 467}#
                                                         #{e\ 1850}#
                                                         #{x\ 1902}#
                                                         #{w\ 1852}#)))
                                                 (if (memq 'eval
                                                           #{when-list\ 1906}#)
                                                   (#{chi-sequence\ 461}#
                                                     (cons #{e1\ 1903}#
                                                           #{e2\ 1904}#)
                                                     #{r\ 1851}#
                                                     #{w\ 1852}#
                                                     #{s\ 1853}#
                                                     #{mod\ 1854}#)
                                                   (#{chi-void\ 487}#)))))
                                           #{tmp\ 1896}#)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           #{tmp\ 1895}#))))
                                   (if (if (eqv? #{type\ 1848}#
                                                 'define-form)
                                         #t
                                         (eqv? #{type\ 1848}#
                                               'define-syntax-form))
                                     (syntax-violation
                                       #f
                                       "definition in expression context"
                                       #{e\ 1850}#
                                       (#{wrap\ 457}#
                                         #{value\ 1849}#
                                         #{w\ 1852}#
                                         #{mod\ 1854}#))
                                     (if (eqv? #{type\ 1848}# (quote syntax))
                                       (syntax-violation
                                         #f
                                         "reference to pattern variable outside syntax form"
                                         (#{source-wrap\ 459}#
                                           #{e\ 1850}#
                                           #{w\ 1852}#
                                           #{s\ 1853}#
                                           #{mod\ 1854}#))
                                       (if (eqv? #{type\ 1848}#
                                                 'displaced-lexical)
                                         (syntax-violation
                                           #f
                                           "reference to identifier outside its scope"
                                           (#{source-wrap\ 459}#
                                             #{e\ 1850}#
                                             #{w\ 1852}#
                                             #{s\ 1853}#
                                             #{mod\ 1854}#))
                                         (syntax-violation
                                           #f
                                           "unexpected syntax"
                                           (#{source-wrap\ 459}#
                                             #{e\ 1850}#
                                             #{w\ 1852}#
                                             #{s\ 1853}#
                                             #{mod\ 1854}#))))))))))))))))))
         (#{chi-application\ 477}#
           (lambda (#{x\ 1913}#
                    #{e\ 1914}#
                    #{r\ 1915}#
                    #{w\ 1916}#
                    #{s\ 1917}#
                    #{mod\ 1918}#)
             (let ((#{tmp\ 1925}# #{e\ 1914}#))
               (let ((#{tmp\ 1926}#
                       ($sc-dispatch
                         #{tmp\ 1925}#
                         '(any . each-any))))
                 (if #{tmp\ 1926}#
                   (@apply
                     (lambda (#{e0\ 1929}# #{e1\ 1930}#)
                       (#{build-application\ 312}#
                         #{s\ 1917}#
                         #{x\ 1913}#
                         (map (lambda (#{e\ 1931}#)
                                (#{chi\ 473}#
                                  #{e\ 1931}#
                                  #{r\ 1915}#
                                  #{w\ 1916}#
                                  #{mod\ 1918}#))
                              #{e1\ 1930}#)))
                     #{tmp\ 1926}#)
                   (syntax-violation
                     #f
                     "source expression failed to match any pattern"
                     #{tmp\ 1925}#))))))
         (#{chi-macro\ 479}#
           (lambda (#{p\ 1934}#
                    #{e\ 1935}#
                    #{r\ 1936}#
                    #{w\ 1937}#
                    #{s\ 1938}#
                    #{rib\ 1939}#
                    #{mod\ 1940}#)
             (letrec*
               ((#{rebuild-macro-output\ 1949}#
                  (lambda (#{x\ 1950}# #{m\ 1951}#)
                    (if (pair? #{x\ 1950}#)
                      (#{decorate-source\ 306}#
                        (cons (#{rebuild-macro-output\ 1949}#
                                (car #{x\ 1950}#)
                                #{m\ 1951}#)
                              (#{rebuild-macro-output\ 1949}#
                                (cdr #{x\ 1950}#)
                                #{m\ 1951}#))
                        #{s\ 1938}#)
                      (if (#{syntax-object?\ 352}# #{x\ 1950}#)
                        (begin
                          (let ((#{w\ 1959}#
                                  (#{syntax-object-wrap\ 356}# #{x\ 1950}#)))
                            (begin
                              (let ((#{ms\ 1962}#
                                      (#{wrap-marks\ 395}# #{w\ 1959}#))
                                    (#{s\ 1963}#
                                      (#{wrap-subst\ 397}# #{w\ 1959}#)))
                                (if (if (pair? #{ms\ 1962}#)
                                      (eq? (car #{ms\ 1962}#) #f)
                                      #f)
                                  (#{make-syntax-object\ 350}#
                                    (#{syntax-object-expression\ 354}#
                                      #{x\ 1950}#)
                                    (#{make-wrap\ 393}#
                                      (cdr #{ms\ 1962}#)
                                      (if #{rib\ 1939}#
                                        (cons #{rib\ 1939}# (cdr #{s\ 1963}#))
                                        (cdr #{s\ 1963}#)))
                                    (#{syntax-object-module\ 358}#
                                      #{x\ 1950}#))
                                  (#{make-syntax-object\ 350}#
                                    (#{decorate-source\ 306}#
                                      (#{syntax-object-expression\ 354}#
                                        #{x\ 1950}#)
                                      #{s\ 1963}#)
                                    (#{make-wrap\ 393}#
                                      (cons #{m\ 1951}# #{ms\ 1962}#)
                                      (if #{rib\ 1939}#
                                        (cons #{rib\ 1939}#
                                              (cons (quote shift) #{s\ 1963}#))
                                        (cons (quote shift) #{s\ 1963}#)))
                                    (#{syntax-object-module\ 358}#
                                      #{x\ 1950}#)))))))
                        (if (vector? #{x\ 1950}#)
                          (begin
                            (let ((#{n\ 1971}# (vector-length #{x\ 1950}#)))
                              (begin
                                (let ((#{v\ 1973}#
                                        (#{decorate-source\ 306}#
                                          (make-vector #{n\ 1971}#)
                                          #{x\ 1950}#)))
                                  (letrec*
                                    ((#{loop\ 1976}#
                                       (lambda (#{i\ 1977}#)
                                         (if (#{fx=\ 293}#
                                               #{i\ 1977}#
                                               #{n\ 1971}#)
                                           (begin (if #f #f) #{v\ 1973}#)
                                           (begin
                                             (vector-set!
                                               #{v\ 1973}#
                                               #{i\ 1977}#
                                               (#{rebuild-macro-output\ 1949}#
                                                 (vector-ref
                                                   #{x\ 1950}#
                                                   #{i\ 1977}#)
                                                 #{m\ 1951}#))
                                             (#{loop\ 1976}#
                                               (#{fx+\ 289}#
                                                 #{i\ 1977}#
                                                 1)))))))
                                    (begin (#{loop\ 1976}# 0)))))))
                          (if (symbol? #{x\ 1950}#)
                            (syntax-violation
                              #f
                              "encountered raw symbol in macro output"
                              (#{source-wrap\ 459}#
                                #{e\ 1935}#
                                #{w\ 1937}#
                                (#{wrap-subst\ 397}# #{w\ 1937}#)
                                #{mod\ 1940}#)
                              #{x\ 1950}#)
                            (#{decorate-source\ 306}#
                              #{x\ 1950}#
                              #{s\ 1938}#))))))))
               (begin
                 (#{rebuild-macro-output\ 1949}#
                   (#{p\ 1934}#
                     (#{source-wrap\ 459}#
                       #{e\ 1935}#
                       (#{anti-mark\ 429}# #{w\ 1937}#)
                       #{s\ 1938}#
                       #{mod\ 1940}#))
                   (gensym "m"))))))
         (#{chi-body\ 481}#
           (lambda (#{body\ 1984}#
                    #{outer-form\ 1985}#
                    #{r\ 1986}#
                    #{w\ 1987}#
                    #{mod\ 1988}#)
             (begin
               (let ((#{r\ 1996}#
                       (cons '("placeholder" placeholder)
                             #{r\ 1986}#)))
                 (begin
                   (let ((#{ribcage\ 1998}#
                           (#{make-ribcage\ 409}#
                             '()
                             '()
                             '())))
                     (begin
                       (let ((#{w\ 2001}#
                               (#{make-wrap\ 393}#
                                 (#{wrap-marks\ 395}# #{w\ 1987}#)
                                 (cons #{ribcage\ 1998}#
                                       (#{wrap-subst\ 397}# #{w\ 1987}#)))))
                         (letrec*
                           ((#{parse\ 2010}#
                              (lambda (#{body\ 2011}#
                                       #{ids\ 2012}#
                                       #{labels\ 2013}#
                                       #{var-ids\ 2014}#
                                       #{vars\ 2015}#
                                       #{vals\ 2016}#
                                       #{bindings\ 2017}#)
                                (if (null? #{body\ 2011}#)
                                  (syntax-violation
                                    #f
                                    "no expressions in body"
                                    #{outer-form\ 1985}#)
                                  (begin
                                    (let ((#{e\ 2022}#
                                            (cdr (car #{body\ 2011}#)))
                                          (#{er\ 2023}#
                                            (car (car #{body\ 2011}#))))
                                      (call-with-values
                                        (lambda ()
                                          (#{syntax-type\ 469}#
                                            #{e\ 2022}#
                                            #{er\ 2023}#
                                            '(())
                                            (#{source-annotation\ 367}#
                                              #{er\ 2023}#)
                                            #{ribcage\ 1998}#
                                            #{mod\ 1988}#
                                            #f))
                                        (lambda (#{type\ 2025}#
                                                 #{value\ 2026}#
                                                 #{e\ 2027}#
                                                 #{w\ 2028}#
                                                 #{s\ 2029}#
                                                 #{mod\ 2030}#)
                                          (if (eqv? #{type\ 2025}#
                                                    'define-form)
                                            (begin
                                              (let ((#{id\ 2040}#
                                                      (#{wrap\ 457}#
                                                        #{value\ 2026}#
                                                        #{w\ 2028}#
                                                        #{mod\ 2030}#))
                                                    (#{label\ 2041}#
                                                      (#{gen-label\ 404}#)))
                                                (begin
                                                  (let ((#{var\ 2043}#
                                                          (#{gen-var\ 501}#
                                                            #{id\ 2040}#)))
                                                    (begin
                                                      (#{extend-ribcage!\ 433}#
                                                        #{ribcage\ 1998}#
                                                        #{id\ 2040}#
                                                        #{label\ 2041}#)
                                                      (#{parse\ 2010}#
                                                        (cdr #{body\ 2011}#)
                                                        (cons #{id\ 2040}#
                                                              #{ids\ 2012}#)
                                                        (cons #{label\ 2041}#
                                                              #{labels\ 2013}#)
                                                        (cons #{id\ 2040}#
                                                              #{var-ids\ 2014}#)
                                                        (cons #{var\ 2043}#
                                                              #{vars\ 2015}#)
                                                        (cons (cons #{er\ 2023}#
                                                                    (#{wrap\ 457}#
                                                                      #{e\ 2027}#
                                                                      #{w\ 2028}#
                                                                      #{mod\ 2030}#))
                                                              #{vals\ 2016}#)
                                                        (cons (cons 'lexical
                                                                    #{var\ 2043}#)
                                                              #{bindings\ 2017}#)))))))
                                            (if (eqv? #{type\ 2025}#
                                                      'define-syntax-form)
                                              (begin
                                                (let ((#{id\ 2048}#
                                                        (#{wrap\ 457}#
                                                          #{value\ 2026}#
                                                          #{w\ 2028}#
                                                          #{mod\ 2030}#))
                                                      (#{label\ 2049}#
                                                        (#{gen-label\ 404}#)))
                                                  (begin
                                                    (#{extend-ribcage!\ 433}#
                                                      #{ribcage\ 1998}#
                                                      #{id\ 2048}#
                                                      #{label\ 2049}#)
                                                    (#{parse\ 2010}#
                                                      (cdr #{body\ 2011}#)
                                                      (cons #{id\ 2048}#
                                                            #{ids\ 2012}#)
                                                      (cons #{label\ 2049}#
                                                            #{labels\ 2013}#)
                                                      #{var-ids\ 2014}#
                                                      #{vars\ 2015}#
                                                      #{vals\ 2016}#
                                                      (cons (cons 'macro
                                                                  (cons #{er\ 2023}#
                                                                        (#{wrap\ 457}#
                                                                          #{e\ 2027}#
                                                                          #{w\ 2028}#
                                                                          #{mod\ 2030}#)))
                                                            #{bindings\ 2017}#)))))
                                              (if (eqv? #{type\ 2025}#
                                                        'begin-form)
                                                (let ((#{tmp\ 2052}#
                                                        #{e\ 2027}#))
                                                  (let ((#{tmp\ 2053}#
                                                          ($sc-dispatch
                                                            #{tmp\ 2052}#
                                                            '(any .
                                                                  each-any))))
                                                    (if #{tmp\ 2053}#
                                                      (@apply
                                                        (lambda (#{_\ 2056}#
                                                                 #{e1\ 2057}#)
                                                          (#{parse\ 2010}#
                                                            (letrec*
                                                              ((#{f\ 2060}#
                                                                 (lambda (#{forms\ 2061}#)
                                                                   (if (null? #{forms\ 2061}#)
                                                                     (cdr #{body\ 2011}#)
                                                                     (cons (cons #{er\ 2023}#
                                                                                 (#{wrap\ 457}#
                                                                                   (car #{forms\ 2061}#)
                                                                                   #{w\ 2028}#
                                                                                   #{mod\ 2030}#))
                                                                           (#{f\ 2060}#
                                                                             (cdr #{forms\ 2061}#)))))))
                                                              (begin
                                                                (#{f\ 2060}#
                                                                  #{e1\ 2057}#)))
                                                            #{ids\ 2012}#
                                                            #{labels\ 2013}#
                                                            #{var-ids\ 2014}#
                                                            #{vars\ 2015}#
                                                            #{vals\ 2016}#
                                                            #{bindings\ 2017}#))
                                                        #{tmp\ 2053}#)
                                                      (syntax-violation
                                                        #f
                                                        "source expression failed to match any pattern"
                                                        #{tmp\ 2052}#))))
                                                (if (eqv? #{type\ 2025}#
                                                          'local-syntax-form)
                                                  (#{chi-local-syntax\ 483}#
                                                    #{value\ 2026}#
                                                    #{e\ 2027}#
                                                    #{er\ 2023}#
                                                    #{w\ 2028}#
                                                    #{s\ 2029}#
                                                    #{mod\ 2030}#
                                                    (lambda (#{forms\ 2064}#
                                                             #{er\ 2065}#
                                                             #{w\ 2066}#
                                                             #{s\ 2067}#
                                                             #{mod\ 2068}#)
                                                      (#{parse\ 2010}#
                                                        (letrec*
                                                          ((#{f\ 2076}#
                                                             (lambda (#{forms\ 2077}#)
                                                               (if (null? #{forms\ 2077}#)
                                                                 (cdr #{body\ 2011}#)
                                                                 (cons (cons #{er\ 2065}#
                                                                             (#{wrap\ 457}#
                                                                               (car #{forms\ 2077}#)
                                                                               #{w\ 2066}#
                                                                               #{mod\ 2068}#))
                                                                       (#{f\ 2076}#
                                                                         (cdr #{forms\ 2077}#)))))))
                                                          (begin
                                                            (#{f\ 2076}#
                                                              #{forms\ 2064}#)))
                                                        #{ids\ 2012}#
                                                        #{labels\ 2013}#
                                                        #{var-ids\ 2014}#
                                                        #{vars\ 2015}#
                                                        #{vals\ 2016}#
                                                        #{bindings\ 2017}#)))
                                                  (if (null? #{ids\ 2012}#)
                                                    (#{build-sequence\ 340}#
                                                      #f
                                                      (map (lambda (#{x\ 2080}#)
                                                             (#{chi\ 473}#
                                                               (cdr #{x\ 2080}#)
                                                               (car #{x\ 2080}#)
                                                               '(())
                                                               #{mod\ 2030}#))
                                                           (cons (cons #{er\ 2023}#
                                                                       (#{source-wrap\ 459}#
                                                                         #{e\ 2027}#
                                                                         #{w\ 2028}#
                                                                         #{s\ 2029}#
                                                                         #{mod\ 2030}#))
                                                                 (cdr #{body\ 2011}#))))
                                                    (begin
                                                      (if (not (#{valid-bound-ids?\ 451}#
                                                                 #{ids\ 2012}#))
                                                        (syntax-violation
                                                          #f
                                                          "invalid or duplicate identifier in definition"
                                                          #{outer-form\ 1985}#))
                                                      (letrec*
                                                        ((#{loop\ 2087}#
                                                           (lambda (#{bs\ 2088}#
                                                                    #{er-cache\ 2089}#
                                                                    #{r-cache\ 2090}#)
                                                             (if (not (null? #{bs\ 2088}#))
                                                               (begin
                                                                 (let ((#{b\ 2093}#
                                                                         (car #{bs\ 2088}#)))
                                                                   (if (eq? (car #{b\ 2093}#)
                                                                            'macro)
                                                                     (begin
                                                                       (let ((#{er\ 2096}#
                                                                               (car (cdr #{b\ 2093}#))))
                                                                         (begin
                                                                           (let ((#{r-cache\ 2098}#
                                                                                   (if (eq? #{er\ 2096}#
                                                                                            #{er-cache\ 2089}#)
                                                                                     #{r-cache\ 2090}#
                                                                                     (#{macros-only-env\ 380}#
                                                                                       #{er\ 2096}#))))
                                                                             (begin
                                                                               (set-cdr!
                                                                                 #{b\ 2093}#
                                                                                 (#{eval-local-transformer\ 485}#
                                                                                   (#{chi\ 473}#
                                                                                     (cdr (cdr #{b\ 2093}#))
                                                                                     #{r-cache\ 2098}#
                                                                                     '(())
                                                                                     #{mod\ 2030}#)
                                                                                   #{mod\ 2030}#))
                                                                               (#{loop\ 2087}#
                                                                                 (cdr #{bs\ 2088}#)
                                                                                 #{er\ 2096}#
                                                                                 #{r-cache\ 2098}#))))))
                                                                     (#{loop\ 2087}#
                                                                       (cdr #{bs\ 2088}#)
                                                                       #{er-cache\ 2089}#
                                                                       #{r-cache\ 2090}#))))))))
                                                        (begin
                                                          (#{loop\ 2087}#
                                                            #{bindings\ 2017}#
                                                            #f
                                                            #f)))
                                                      (set-cdr!
                                                        #{r\ 1996}#
                                                        (#{extend-env\ 376}#
                                                          #{labels\ 2013}#
                                                          #{bindings\ 2017}#
                                                          (cdr #{r\ 1996}#)))
                                                      (#{build-letrec\ 346}#
                                                        #f
                                                        #t
                                                        (reverse
                                                          (map syntax->datum
                                                               #{var-ids\ 2014}#))
                                                        (reverse
                                                          #{vars\ 2015}#)
                                                        (map (lambda (#{x\ 2101}#)
                                                               (#{chi\ 473}#
                                                                 (cdr #{x\ 2101}#)
                                                                 (car #{x\ 2101}#)
                                                                 '(())
                                                                 #{mod\ 2030}#))
                                                             (reverse
                                                               #{vals\ 2016}#))
                                                        (#{build-sequence\ 340}#
                                                          #f
                                                          (map (lambda (#{x\ 2105}#)
                                                                 (#{chi\ 473}#
                                                                   (cdr #{x\ 2105}#)
                                                                   (car #{x\ 2105}#)
                                                                   '(())
                                                                   #{mod\ 2030}#))
                                                               (cons (cons #{er\ 2023}#
                                                                           (#{source-wrap\ 459}#
                                                                             #{e\ 2027}#
                                                                             #{w\ 2028}#
                                                                             #{s\ 2029}#
                                                                             #{mod\ 2030}#))
                                                                     (cdr #{body\ 2011}#)))))))))))))))))))
                           (begin
                             (#{parse\ 2010}#
                               (map (lambda (#{x\ 2018}#)
                                      (cons #{r\ 1996}#
                                            (#{wrap\ 457}#
                                              #{x\ 2018}#
                                              #{w\ 2001}#
                                              #{mod\ 1988}#)))
                                    #{body\ 1984}#)
                               '()
                               '()
                               '()
                               '()
                               '()
                               '())))))))))))
         (#{chi-local-syntax\ 483}#
           (lambda (#{rec?\ 2108}#
                    #{e\ 2109}#
                    #{r\ 2110}#
                    #{w\ 2111}#
                    #{s\ 2112}#
                    #{mod\ 2113}#
                    #{k\ 2114}#)
             (let ((#{tmp\ 2122}# #{e\ 2109}#))
               (let ((#{tmp\ 2123}#
                       ($sc-dispatch
                         #{tmp\ 2122}#
                         '(any #(each (any any)) any . each-any))))
                 (if #{tmp\ 2123}#
                   (@apply
                     (lambda (#{_\ 2129}#
                              #{id\ 2130}#
                              #{val\ 2131}#
                              #{e1\ 2132}#
                              #{e2\ 2133}#)
                       (begin
                         (let ((#{ids\ 2135}# #{id\ 2130}#))
                           (if (not (#{valid-bound-ids?\ 451}# #{ids\ 2135}#))
                             (syntax-violation
                               #f
                               "duplicate bound keyword"
                               #{e\ 2109}#)
                             (begin
                               (let ((#{labels\ 2138}#
                                       (#{gen-labels\ 406}# #{ids\ 2135}#)))
                                 (begin
                                   (let ((#{new-w\ 2140}#
                                           (#{make-binding-wrap\ 435}#
                                             #{ids\ 2135}#
                                             #{labels\ 2138}#
                                             #{w\ 2111}#)))
                                     (#{k\ 2114}#
                                       (cons #{e1\ 2132}# #{e2\ 2133}#)
                                       (#{extend-env\ 376}#
                                         #{labels\ 2138}#
                                         (begin
                                           (let ((#{w\ 2144}#
                                                   (if #{rec?\ 2108}#
                                                     #{new-w\ 2140}#
                                                     #{w\ 2111}#))
                                                 (#{trans-r\ 2145}#
                                                   (#{macros-only-env\ 380}#
                                                     #{r\ 2110}#)))
                                             (map (lambda (#{x\ 2146}#)
                                                    (cons 'macro
                                                          (#{eval-local-transformer\ 485}#
                                                            (#{chi\ 473}#
                                                              #{x\ 2146}#
                                                              #{trans-r\ 2145}#
                                                              #{w\ 2144}#
                                                              #{mod\ 2113}#)
                                                            #{mod\ 2113}#)))
                                                  #{val\ 2131}#)))
                                         #{r\ 2110}#)
                                       #{new-w\ 2140}#
                                       #{s\ 2112}#
                                       #{mod\ 2113}#)))))))))
                     #{tmp\ 2123}#)
                   (let ((#{_\ 2151}# #{tmp\ 2122}#))
                     (syntax-violation
                       #f
                       "bad local syntax definition"
                       (#{source-wrap\ 459}#
                         #{e\ 2109}#
                         #{w\ 2111}#
                         #{s\ 2112}#
                         #{mod\ 2113}#))))))))
         (#{eval-local-transformer\ 485}#
           (lambda (#{expanded\ 2152}# #{mod\ 2153}#)
             (begin
               (let ((#{p\ 2157}#
                       (#{local-eval-hook\ 299}#
                         #{expanded\ 2152}#
                         #{mod\ 2153}#)))
                 (if (procedure? #{p\ 2157}#)
                   #{p\ 2157}#
                   (syntax-violation
                     #f
                     "nonprocedure transformer"
                     #{p\ 2157}#))))))
         (#{chi-void\ 487}#
           (lambda () (#{build-void\ 310}# #f)))
         (#{ellipsis?\ 489}#
           (lambda (#{x\ 2159}#)
             (if (#{nonsymbol-id?\ 386}# #{x\ 2159}#)
               (#{free-id=?\ 447}#
                 #{x\ 2159}#
                 '#(syntax-object
                    ...
                    ((top)
                     #(ribcage () () ())
                     #(ribcage () () ())
                     #(ribcage #(x) #((top)) #("i2160"))
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
                       ("i502"
                        "i500"
                        "i498"
                        "i496"
                        "i494"
                        "i492"
                        "i490"
                        "i488"
                        "i486"
                        "i484"
                        "i482"
                        "i480"
                        "i478"
                        "i476"
                        "i474"
                        "i472"
                        "i470"
                        "i468"
                        "i466"
                        "i464"
                        "i462"
                        "i460"
                        "i458"
                        "i456"
                        "i454"
                        "i452"
                        "i450"
                        "i448"
                        "i446"
                        "i444"
                        "i442"
                        "i440"
                        "i438"
                        "i436"
                        "i434"
                        "i432"
                        "i431"
                        "i430"
                        "i428"
                        "i427"
                        "i426"
                        "i425"
                        "i424"
                        "i422"
                        "i420"
                        "i418"
                        "i416"
                        "i414"
                        "i412"
                        "i410"
                        "i408"
                        "i405"
                        "i403"
                        "i402"
                        "i401"
                        "i400"
                        "i399"
                        "i398"
                        "i396"
                        "i394"
                        "i392"
                        "i390"
                        "i389"
                        "i387"
                        "i385"
                        "i383"
                        "i381"
                        "i379"
                        "i377"
                        "i375"
                        "i374"
                        "i372"
                        "i370"
                        "i369"
                        "i368"
                        "i366"
                        "i365"
                        "i363"
                        "i361"
                        "i359"
                        "i357"
                        "i355"
                        "i353"
                        "i351"
                        "i349"
                        "i347"
                        "i345"
                        "i343"
                        "i341"
                        "i339"
                        "i337"
                        "i335"
                        "i333"
                        "i331"
                        "i329"
                        "i327"
                        "i325"
                        "i323"
                        "i321"
                        "i319"
                        "i317"
                        "i315"
                        "i313"
                        "i311"
                        "i309"
                        "i307"
                        "i305"
                        "i303"
                        "i301"
                        "i300"
                        "i298"
                        "i296"
                        "i294"
                        "i292"
                        "i290"
                        "i288"
                        "i286"
                        "i284"
                        "i282"
                        "i279"
                        "i277"
                        "i275"
                        "i273"
                        "i271"
                        "i269"
                        "i267"
                        "i265"
                        "i263"
                        "i261"
                        "i259"
                        "i257"
                        "i255"
                        "i253"
                        "i251"
                        "i249"
                        "i247"
                        "i245"))
                     #(ribcage
                       (define-structure
                         define-expansion-accessors
                         define-expansion-constructors
                         and-map*)
                       ((top) (top) (top) (top))
                       ("i39" "i38" "i37" "i35")))
                    (hygiene guile)))
               #f)))
         (#{lambda-formals\ 491}#
           (lambda (#{orig-args\ 2163}#)
             (letrec*
               ((#{req\ 2166}#
                  (lambda (#{args\ 2169}# #{rreq\ 2170}#)
                    (let ((#{tmp\ 2173}# #{args\ 2169}#))
                      (let ((#{tmp\ 2174}#
                              ($sc-dispatch #{tmp\ 2173}# (quote ()))))
                        (if #{tmp\ 2174}#
                          (@apply
                            (lambda ()
                              (#{check\ 2168}# (reverse #{rreq\ 2170}#) #f))
                            #{tmp\ 2174}#)
                          (let ((#{tmp\ 2175}#
                                  ($sc-dispatch
                                    #{tmp\ 2173}#
                                    '(any . any))))
                            (if (if #{tmp\ 2175}#
                                  (@apply
                                    (lambda (#{a\ 2178}# #{b\ 2179}#)
                                      (#{id?\ 388}# #{a\ 2178}#))
                                    #{tmp\ 2175}#)
                                  #f)
                              (@apply
                                (lambda (#{a\ 2182}# #{b\ 2183}#)
                                  (#{req\ 2166}#
                                    #{b\ 2183}#
                                    (cons #{a\ 2182}# #{rreq\ 2170}#)))
                                #{tmp\ 2175}#)
                              (let ((#{tmp\ 2184}# (list #{tmp\ 2173}#)))
                                (if (if #{tmp\ 2184}#
                                      (@apply
                                        (lambda (#{r\ 2186}#)
                                          (#{id?\ 388}# #{r\ 2186}#))
                                        #{tmp\ 2184}#)
                                      #f)
                                  (@apply
                                    (lambda (#{r\ 2188}#)
                                      (#{check\ 2168}#
                                        (reverse #{rreq\ 2170}#)
                                        #{r\ 2188}#))
                                    #{tmp\ 2184}#)
                                  (let ((#{else\ 2190}# #{tmp\ 2173}#))
                                    (syntax-violation
                                      'lambda
                                      "invalid argument list"
                                      #{orig-args\ 2163}#
                                      #{args\ 2169}#)))))))))))
                (#{check\ 2168}#
                  (lambda (#{req\ 2191}# #{rest\ 2192}#)
                    (if (#{distinct-bound-ids?\ 453}#
                          (if #{rest\ 2192}#
                            (cons #{rest\ 2192}# #{req\ 2191}#)
                            #{req\ 2191}#))
                      (values #{req\ 2191}# #f #{rest\ 2192}# #f)
                      (syntax-violation
                        'lambda
                        "duplicate identifier in argument list"
                        #{orig-args\ 2163}#)))))
               (begin
                 (#{req\ 2166}# #{orig-args\ 2163}# (quote ()))))))
         (#{chi-simple-lambda\ 493}#
           (lambda (#{e\ 2198}#
                    #{r\ 2199}#
                    #{w\ 2200}#
                    #{s\ 2201}#
                    #{mod\ 2202}#
                    #{req\ 2203}#
                    #{rest\ 2204}#
                    #{meta\ 2205}#
                    #{body\ 2206}#)
             (begin
               (let ((#{ids\ 2218}#
                       (if #{rest\ 2204}#
                         (append #{req\ 2203}# (list #{rest\ 2204}#))
                         #{req\ 2203}#)))
                 (begin
                   (let ((#{vars\ 2220}#
                           (map #{gen-var\ 501}# #{ids\ 2218}#)))
                     (begin
                       (let ((#{labels\ 2222}#
                               (#{gen-labels\ 406}# #{ids\ 2218}#)))
                         (#{build-simple-lambda\ 330}#
                           #{s\ 2201}#
                           (map syntax->datum #{req\ 2203}#)
                           (if #{rest\ 2204}#
                             (syntax->datum #{rest\ 2204}#)
                             #f)
                           #{vars\ 2220}#
                           #{meta\ 2205}#
                           (#{chi-body\ 481}#
                             #{body\ 2206}#
                             (#{source-wrap\ 459}#
                               #{e\ 2198}#
                               #{w\ 2200}#
                               #{s\ 2201}#
                               #{mod\ 2202}#)
                             (#{extend-var-env\ 378}#
                               #{labels\ 2222}#
                               #{vars\ 2220}#
                               #{r\ 2199}#)
                             (#{make-binding-wrap\ 435}#
                               #{ids\ 2218}#
                               #{labels\ 2222}#
                               #{w\ 2200}#)
                             #{mod\ 2202}#))))))))))
         (#{lambda*-formals\ 495}#
           (lambda (#{orig-args\ 2225}#)
             (letrec*
               ((#{req\ 2228}#
                  (lambda (#{args\ 2237}# #{rreq\ 2238}#)
                    (let ((#{tmp\ 2241}# #{args\ 2237}#))
                      (let ((#{tmp\ 2242}#
                              ($sc-dispatch #{tmp\ 2241}# (quote ()))))
                        (if #{tmp\ 2242}#
                          (@apply
                            (lambda ()
                              (#{check\ 2236}#
                                (reverse #{rreq\ 2238}#)
                                '()
                                #f
                                '()))
                            #{tmp\ 2242}#)
                          (let ((#{tmp\ 2243}#
                                  ($sc-dispatch
                                    #{tmp\ 2241}#
                                    '(any . any))))
                            (if (if #{tmp\ 2243}#
                                  (@apply
                                    (lambda (#{a\ 2246}# #{b\ 2247}#)
                                      (#{id?\ 388}# #{a\ 2246}#))
                                    #{tmp\ 2243}#)
                                  #f)
                              (@apply
                                (lambda (#{a\ 2250}# #{b\ 2251}#)
                                  (#{req\ 2228}#
                                    #{b\ 2251}#
                                    (cons #{a\ 2250}# #{rreq\ 2238}#)))
                                #{tmp\ 2243}#)
                              (let ((#{tmp\ 2252}#
                                      ($sc-dispatch
                                        #{tmp\ 2241}#
                                        '(any . any))))
                                (if (if #{tmp\ 2252}#
                                      (@apply
                                        (lambda (#{a\ 2255}# #{b\ 2256}#)
                                          (eq? (syntax->datum #{a\ 2255}#)
                                               #:optional))
                                        #{tmp\ 2252}#)
                                      #f)
                                  (@apply
                                    (lambda (#{a\ 2259}# #{b\ 2260}#)
                                      (#{opt\ 2230}#
                                        #{b\ 2260}#
                                        (reverse #{rreq\ 2238}#)
                                        '()))
                                    #{tmp\ 2252}#)
                                  (let ((#{tmp\ 2261}#
                                          ($sc-dispatch
                                            #{tmp\ 2241}#
                                            '(any . any))))
                                    (if (if #{tmp\ 2261}#
                                          (@apply
                                            (lambda (#{a\ 2264}# #{b\ 2265}#)
                                              (eq? (syntax->datum #{a\ 2264}#)
                                                   #:key))
                                            #{tmp\ 2261}#)
                                          #f)
                                      (@apply
                                        (lambda (#{a\ 2268}# #{b\ 2269}#)
                                          (#{key\ 2232}#
                                            #{b\ 2269}#
                                            (reverse #{rreq\ 2238}#)
                                            '()
                                            '()))
                                        #{tmp\ 2261}#)
                                      (let ((#{tmp\ 2270}#
                                              ($sc-dispatch
                                                #{tmp\ 2241}#
                                                '(any any))))
                                        (if (if #{tmp\ 2270}#
                                              (@apply
                                                (lambda (#{a\ 2273}#
                                                         #{b\ 2274}#)
                                                  (eq? (syntax->datum
                                                         #{a\ 2273}#)
                                                       #:rest))
                                                #{tmp\ 2270}#)
                                              #f)
                                          (@apply
                                            (lambda (#{a\ 2277}# #{b\ 2278}#)
                                              (#{rest\ 2234}#
                                                #{b\ 2278}#
                                                (reverse #{rreq\ 2238}#)
                                                '()
                                                '()))
                                            #{tmp\ 2270}#)
                                          (let ((#{tmp\ 2279}#
                                                  (list #{tmp\ 2241}#)))
                                            (if (if #{tmp\ 2279}#
                                                  (@apply
                                                    (lambda (#{r\ 2281}#)
                                                      (#{id?\ 388}#
                                                        #{r\ 2281}#))
                                                    #{tmp\ 2279}#)
                                                  #f)
                                              (@apply
                                                (lambda (#{r\ 2283}#)
                                                  (#{rest\ 2234}#
                                                    #{r\ 2283}#
                                                    (reverse #{rreq\ 2238}#)
                                                    '()
                                                    '()))
                                                #{tmp\ 2279}#)
                                              (let ((#{else\ 2285}#
                                                      #{tmp\ 2241}#))
                                                (syntax-violation
                                                  'lambda*
                                                  "invalid argument list"
                                                  #{orig-args\ 2225}#
                                                  #{args\ 2237}#)))))))))))))))))
                (#{opt\ 2230}#
                  (lambda (#{args\ 2286}# #{req\ 2287}# #{ropt\ 2288}#)
                    (let ((#{tmp\ 2292}# #{args\ 2286}#))
                      (let ((#{tmp\ 2293}#
                              ($sc-dispatch #{tmp\ 2292}# (quote ()))))
                        (if #{tmp\ 2293}#
                          (@apply
                            (lambda ()
                              (#{check\ 2236}#
                                #{req\ 2287}#
                                (reverse #{ropt\ 2288}#)
                                #f
                                '()))
                            #{tmp\ 2293}#)
                          (let ((#{tmp\ 2294}#
                                  ($sc-dispatch
                                    #{tmp\ 2292}#
                                    '(any . any))))
                            (if (if #{tmp\ 2294}#
                                  (@apply
                                    (lambda (#{a\ 2297}# #{b\ 2298}#)
                                      (#{id?\ 388}# #{a\ 2297}#))
                                    #{tmp\ 2294}#)
                                  #f)
                              (@apply
                                (lambda (#{a\ 2301}# #{b\ 2302}#)
                                  (#{opt\ 2230}#
                                    #{b\ 2302}#
                                    #{req\ 2287}#
                                    (cons (cons #{a\ 2301}#
                                                '(#(syntax-object
                                                    #f
                                                    ((top)
                                                     #(ribcage
                                                       #(a b)
                                                       #((top) (top))
                                                       #("i2299" "i2300"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(args req ropt)
                                                       #((top) (top) (top))
                                                       #("i2289"
                                                         "i2290"
                                                         "i2291"))
                                                     #(ribcage
                                                       (check rest key opt req)
                                                       ((top)
                                                        (top)
                                                        (top)
                                                        (top)
                                                        (top))
                                                       ("i2235"
                                                        "i2233"
                                                        "i2231"
                                                        "i2229"
                                                        "i2227"))
                                                     #(ribcage
                                                       #(orig-args)
                                                       #((top))
                                                       #("i2226"))
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
                                                       ("i502"
                                                        "i500"
                                                        "i498"
                                                        "i496"
                                                        "i494"
                                                        "i492"
                                                        "i490"
                                                        "i488"
                                                        "i486"
                                                        "i484"
                                                        "i482"
                                                        "i480"
                                                        "i478"
                                                        "i476"
                                                        "i474"
                                                        "i472"
                                                        "i470"
                                                        "i468"
                                                        "i466"
                                                        "i464"
                                                        "i462"
                                                        "i460"
                                                        "i458"
                                                        "i456"
                                                        "i454"
                                                        "i452"
                                                        "i450"
                                                        "i448"
                                                        "i446"
                                                        "i444"
                                                        "i442"
                                                        "i440"
                                                        "i438"
                                                        "i436"
                                                        "i434"
                                                        "i432"
                                                        "i431"
                                                        "i430"
                                                        "i428"
                                                        "i427"
                                                        "i426"
                                                        "i425"
                                                        "i424"
                                                        "i422"
                                                        "i420"
                                                        "i418"
                                                        "i416"
                                                        "i414"
                                                        "i412"
                                                        "i410"
                                                        "i408"
                                                        "i405"
                                                        "i403"
                                                        "i402"
                                                        "i401"
                                                        "i400"
                                                        "i399"
                                                        "i398"
                                                        "i396"
                                                        "i394"
                                                        "i392"
                                                        "i390"
                                                        "i389"
                                                        "i387"
                                                        "i385"
                                                        "i383"
                                                        "i381"
                                                        "i379"
                                                        "i377"
                                                        "i375"
                                                        "i374"
                                                        "i372"
                                                        "i370"
                                                        "i369"
                                                        "i368"
                                                        "i366"
                                                        "i365"
                                                        "i363"
                                                        "i361"
                                                        "i359"
                                                        "i357"
                                                        "i355"
                                                        "i353"
                                                        "i351"
                                                        "i349"
                                                        "i347"
                                                        "i345"
                                                        "i343"
                                                        "i341"
                                                        "i339"
                                                        "i337"
                                                        "i335"
                                                        "i333"
                                                        "i331"
                                                        "i329"
                                                        "i327"
                                                        "i325"
                                                        "i323"
                                                        "i321"
                                                        "i319"
                                                        "i317"
                                                        "i315"
                                                        "i313"
                                                        "i311"
                                                        "i309"
                                                        "i307"
                                                        "i305"
                                                        "i303"
                                                        "i301"
                                                        "i300"
                                                        "i298"
                                                        "i296"
                                                        "i294"
                                                        "i292"
                                                        "i290"
                                                        "i288"
                                                        "i286"
                                                        "i284"
                                                        "i282"
                                                        "i279"
                                                        "i277"
                                                        "i275"
                                                        "i273"
                                                        "i271"
                                                        "i269"
                                                        "i267"
                                                        "i265"
                                                        "i263"
                                                        "i261"
                                                        "i259"
                                                        "i257"
                                                        "i255"
                                                        "i253"
                                                        "i251"
                                                        "i249"
                                                        "i247"
                                                        "i245"))
                                                     #(ribcage
                                                       (define-structure
                                                         define-expansion-accessors
                                                         define-expansion-constructors
                                                         and-map*)
                                                       ((top)
                                                        (top)
                                                        (top)
                                                        (top))
                                                       ("i39"
                                                        "i38"
                                                        "i37"
                                                        "i35")))
                                                    (hygiene guile))))
                                          #{ropt\ 2288}#)))
                                #{tmp\ 2294}#)
                              (let ((#{tmp\ 2303}#
                                      ($sc-dispatch
                                        #{tmp\ 2292}#
                                        '((any any) . any))))
                                (if (if #{tmp\ 2303}#
                                      (@apply
                                        (lambda (#{a\ 2307}#
                                                 #{init\ 2308}#
                                                 #{b\ 2309}#)
                                          (#{id?\ 388}# #{a\ 2307}#))
                                        #{tmp\ 2303}#)
                                      #f)
                                  (@apply
                                    (lambda (#{a\ 2313}#
                                             #{init\ 2314}#
                                             #{b\ 2315}#)
                                      (#{opt\ 2230}#
                                        #{b\ 2315}#
                                        #{req\ 2287}#
                                        (cons (list #{a\ 2313}# #{init\ 2314}#)
                                              #{ropt\ 2288}#)))
                                    #{tmp\ 2303}#)
                                  (let ((#{tmp\ 2316}#
                                          ($sc-dispatch
                                            #{tmp\ 2292}#
                                            '(any . any))))
                                    (if (if #{tmp\ 2316}#
                                          (@apply
                                            (lambda (#{a\ 2319}# #{b\ 2320}#)
                                              (eq? (syntax->datum #{a\ 2319}#)
                                                   #:key))
                                            #{tmp\ 2316}#)
                                          #f)
                                      (@apply
                                        (lambda (#{a\ 2323}# #{b\ 2324}#)
                                          (#{key\ 2232}#
                                            #{b\ 2324}#
                                            #{req\ 2287}#
                                            (reverse #{ropt\ 2288}#)
                                            '()))
                                        #{tmp\ 2316}#)
                                      (let ((#{tmp\ 2325}#
                                              ($sc-dispatch
                                                #{tmp\ 2292}#
                                                '(any any))))
                                        (if (if #{tmp\ 2325}#
                                              (@apply
                                                (lambda (#{a\ 2328}#
                                                         #{b\ 2329}#)
                                                  (eq? (syntax->datum
                                                         #{a\ 2328}#)
                                                       #:rest))
                                                #{tmp\ 2325}#)
                                              #f)
                                          (@apply
                                            (lambda (#{a\ 2332}# #{b\ 2333}#)
                                              (#{rest\ 2234}#
                                                #{b\ 2333}#
                                                #{req\ 2287}#
                                                (reverse #{ropt\ 2288}#)
                                                '()))
                                            #{tmp\ 2325}#)
                                          (let ((#{tmp\ 2334}#
                                                  (list #{tmp\ 2292}#)))
                                            (if (if #{tmp\ 2334}#
                                                  (@apply
                                                    (lambda (#{r\ 2336}#)
                                                      (#{id?\ 388}#
                                                        #{r\ 2336}#))
                                                    #{tmp\ 2334}#)
                                                  #f)
                                              (@apply
                                                (lambda (#{r\ 2338}#)
                                                  (#{rest\ 2234}#
                                                    #{r\ 2338}#
                                                    #{req\ 2287}#
                                                    (reverse #{ropt\ 2288}#)
                                                    '()))
                                                #{tmp\ 2334}#)
                                              (let ((#{else\ 2340}#
                                                      #{tmp\ 2292}#))
                                                (syntax-violation
                                                  'lambda*
                                                  "invalid optional argument list"
                                                  #{orig-args\ 2225}#
                                                  #{args\ 2286}#)))))))))))))))))
                (#{key\ 2232}#
                  (lambda (#{args\ 2341}#
                           #{req\ 2342}#
                           #{opt\ 2343}#
                           #{rkey\ 2344}#)
                    (let ((#{tmp\ 2349}# #{args\ 2341}#))
                      (let ((#{tmp\ 2350}#
                              ($sc-dispatch #{tmp\ 2349}# (quote ()))))
                        (if #{tmp\ 2350}#
                          (@apply
                            (lambda ()
                              (#{check\ 2236}#
                                #{req\ 2342}#
                                #{opt\ 2343}#
                                #f
                                (cons #f (reverse #{rkey\ 2344}#))))
                            #{tmp\ 2350}#)
                          (let ((#{tmp\ 2351}#
                                  ($sc-dispatch
                                    #{tmp\ 2349}#
                                    '(any . any))))
                            (if (if #{tmp\ 2351}#
                                  (@apply
                                    (lambda (#{a\ 2354}# #{b\ 2355}#)
                                      (#{id?\ 388}# #{a\ 2354}#))
                                    #{tmp\ 2351}#)
                                  #f)
                              (@apply
                                (lambda (#{a\ 2358}# #{b\ 2359}#)
                                  (let ((#{tmp\ 2361}#
                                          (symbol->keyword
                                            (syntax->datum #{a\ 2358}#))))
                                    (let ((#{k\ 2363}# #{tmp\ 2361}#))
                                      (#{key\ 2232}#
                                        #{b\ 2359}#
                                        #{req\ 2342}#
                                        #{opt\ 2343}#
                                        (cons (cons #{k\ 2363}#
                                                    (cons #{a\ 2358}#
                                                          '(#(syntax-object
                                                              #f
                                                              ((top)
                                                               #(ribcage
                                                                 #(k)
                                                                 #((top))
                                                                 #("i2362"))
                                                               #(ribcage
                                                                 #(a b)
                                                                 #((top) (top))
                                                                 #("i2356"
                                                                   "i2357"))
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
                                                                 #("i2345"
                                                                   "i2346"
                                                                   "i2347"
                                                                   "i2348"))
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
                                                                 ("i2235"
                                                                  "i2233"
                                                                  "i2231"
                                                                  "i2229"
                                                                  "i2227"))
                                                               #(ribcage
                                                                 #(orig-args)
                                                                 #((top))
                                                                 #("i2226"))
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
                                                                 ("i502"
                                                                  "i500"
                                                                  "i498"
                                                                  "i496"
                                                                  "i494"
                                                                  "i492"
                                                                  "i490"
                                                                  "i488"
                                                                  "i486"
                                                                  "i484"
                                                                  "i482"
                                                                  "i480"
                                                                  "i478"
                                                                  "i476"
                                                                  "i474"
                                                                  "i472"
                                                                  "i470"
                                                                  "i468"
                                                                  "i466"
                                                                  "i464"
                                                                  "i462"
                                                                  "i460"
                                                                  "i458"
                                                                  "i456"
                                                                  "i454"
                                                                  "i452"
                                                                  "i450"
                                                                  "i448"
                                                                  "i446"
                                                                  "i444"
                                                                  "i442"
                                                                  "i440"
                                                                  "i438"
                                                                  "i436"
                                                                  "i434"
                                                                  "i432"
                                                                  "i431"
                                                                  "i430"
                                                                  "i428"
                                                                  "i427"
                                                                  "i426"
                                                                  "i425"
                                                                  "i424"
                                                                  "i422"
                                                                  "i420"
                                                                  "i418"
                                                                  "i416"
                                                                  "i414"
                                                                  "i412"
                                                                  "i410"
                                                                  "i408"
                                                                  "i405"
                                                                  "i403"
                                                                  "i402"
                                                                  "i401"
                                                                  "i400"
                                                                  "i399"
                                                                  "i398"
                                                                  "i396"
                                                                  "i394"
                                                                  "i392"
                                                                  "i390"
                                                                  "i389"
                                                                  "i387"
                                                                  "i385"
                                                                  "i383"
                                                                  "i381"
                                                                  "i379"
                                                                  "i377"
                                                                  "i375"
                                                                  "i374"
                                                                  "i372"
                                                                  "i370"
                                                                  "i369"
                                                                  "i368"
                                                                  "i366"
                                                                  "i365"
                                                                  "i363"
                                                                  "i361"
                                                                  "i359"
                                                                  "i357"
                                                                  "i355"
                                                                  "i353"
                                                                  "i351"
                                                                  "i349"
                                                                  "i347"
                                                                  "i345"
                                                                  "i343"
                                                                  "i341"
                                                                  "i339"
                                                                  "i337"
                                                                  "i335"
                                                                  "i333"
                                                                  "i331"
                                                                  "i329"
                                                                  "i327"
                                                                  "i325"
                                                                  "i323"
                                                                  "i321"
                                                                  "i319"
                                                                  "i317"
                                                                  "i315"
                                                                  "i313"
                                                                  "i311"
                                                                  "i309"
                                                                  "i307"
                                                                  "i305"
                                                                  "i303"
                                                                  "i301"
                                                                  "i300"
                                                                  "i298"
                                                                  "i296"
                                                                  "i294"
                                                                  "i292"
                                                                  "i290"
                                                                  "i288"
                                                                  "i286"
                                                                  "i284"
                                                                  "i282"
                                                                  "i279"
                                                                  "i277"
                                                                  "i275"
                                                                  "i273"
                                                                  "i271"
                                                                  "i269"
                                                                  "i267"
                                                                  "i265"
                                                                  "i263"
                                                                  "i261"
                                                                  "i259"
                                                                  "i257"
                                                                  "i255"
                                                                  "i253"
                                                                  "i251"
                                                                  "i249"
                                                                  "i247"
                                                                  "i245"))
                                                               #(ribcage
                                                                 (define-structure
                                                                   define-expansion-accessors
                                                                   define-expansion-constructors
                                                                   and-map*)
                                                                 ((top)
                                                                  (top)
                                                                  (top)
                                                                  (top))
                                                                 ("i39"
                                                                  "i38"
                                                                  "i37"
                                                                  "i35")))
                                                              (hygiene
                                                                guile)))))
                                              #{rkey\ 2344}#)))))
                                #{tmp\ 2351}#)
                              (let ((#{tmp\ 2364}#
                                      ($sc-dispatch
                                        #{tmp\ 2349}#
                                        '((any any) . any))))
                                (if (if #{tmp\ 2364}#
                                      (@apply
                                        (lambda (#{a\ 2368}#
                                                 #{init\ 2369}#
                                                 #{b\ 2370}#)
                                          (#{id?\ 388}# #{a\ 2368}#))
                                        #{tmp\ 2364}#)
                                      #f)
                                  (@apply
                                    (lambda (#{a\ 2374}#
                                             #{init\ 2375}#
                                             #{b\ 2376}#)
                                      (let ((#{tmp\ 2378}#
                                              (symbol->keyword
                                                (syntax->datum #{a\ 2374}#))))
                                        (let ((#{k\ 2380}# #{tmp\ 2378}#))
                                          (#{key\ 2232}#
                                            #{b\ 2376}#
                                            #{req\ 2342}#
                                            #{opt\ 2343}#
                                            (cons (list #{k\ 2380}#
                                                        #{a\ 2374}#
                                                        #{init\ 2375}#)
                                                  #{rkey\ 2344}#)))))
                                    #{tmp\ 2364}#)
                                  (let ((#{tmp\ 2381}#
                                          ($sc-dispatch
                                            #{tmp\ 2349}#
                                            '((any any any) . any))))
                                    (if (if #{tmp\ 2381}#
                                          (@apply
                                            (lambda (#{a\ 2386}#
                                                     #{init\ 2387}#
                                                     #{k\ 2388}#
                                                     #{b\ 2389}#)
                                              (if (#{id?\ 388}# #{a\ 2386}#)
                                                (keyword?
                                                  (syntax->datum #{k\ 2388}#))
                                                #f))
                                            #{tmp\ 2381}#)
                                          #f)
                                      (@apply
                                        (lambda (#{a\ 2396}#
                                                 #{init\ 2397}#
                                                 #{k\ 2398}#
                                                 #{b\ 2399}#)
                                          (#{key\ 2232}#
                                            #{b\ 2399}#
                                            #{req\ 2342}#
                                            #{opt\ 2343}#
                                            (cons (list #{k\ 2398}#
                                                        #{a\ 2396}#
                                                        #{init\ 2397}#)
                                                  #{rkey\ 2344}#)))
                                        #{tmp\ 2381}#)
                                      (let ((#{tmp\ 2400}#
                                              ($sc-dispatch
                                                #{tmp\ 2349}#
                                                '(any))))
                                        (if (if #{tmp\ 2400}#
                                              (@apply
                                                (lambda (#{aok\ 2402}#)
                                                  (eq? (syntax->datum
                                                         #{aok\ 2402}#)
                                                       #:allow-other-keys))
                                                #{tmp\ 2400}#)
                                              #f)
                                          (@apply
                                            (lambda (#{aok\ 2404}#)
                                              (#{check\ 2236}#
                                                #{req\ 2342}#
                                                #{opt\ 2343}#
                                                #f
                                                (cons #t
                                                      (reverse
                                                        #{rkey\ 2344}#))))
                                            #{tmp\ 2400}#)
                                          (let ((#{tmp\ 2405}#
                                                  ($sc-dispatch
                                                    #{tmp\ 2349}#
                                                    '(any any any))))
                                            (if (if #{tmp\ 2405}#
                                                  (@apply
                                                    (lambda (#{aok\ 2409}#
                                                             #{a\ 2410}#
                                                             #{b\ 2411}#)
                                                      (if (eq? (syntax->datum
                                                                 #{aok\ 2409}#)
                                                               #:allow-other-keys)
                                                        (eq? (syntax->datum
                                                               #{a\ 2410}#)
                                                             #:rest)
                                                        #f))
                                                    #{tmp\ 2405}#)
                                                  #f)
                                              (@apply
                                                (lambda (#{aok\ 2417}#
                                                         #{a\ 2418}#
                                                         #{b\ 2419}#)
                                                  (#{rest\ 2234}#
                                                    #{b\ 2419}#
                                                    #{req\ 2342}#
                                                    #{opt\ 2343}#
                                                    (cons #t
                                                          (reverse
                                                            #{rkey\ 2344}#))))
                                                #{tmp\ 2405}#)
                                              (let ((#{tmp\ 2420}#
                                                      ($sc-dispatch
                                                        #{tmp\ 2349}#
                                                        '(any . any))))
                                                (if (if #{tmp\ 2420}#
                                                      (@apply
                                                        (lambda (#{aok\ 2423}#
                                                                 #{r\ 2424}#)
                                                          (if (eq? (syntax->datum
                                                                     #{aok\ 2423}#)
                                                                   #:allow-other-keys)
                                                            (#{id?\ 388}#
                                                              #{r\ 2424}#)
                                                            #f))
                                                        #{tmp\ 2420}#)
                                                      #f)
                                                  (@apply
                                                    (lambda (#{aok\ 2429}#
                                                             #{r\ 2430}#)
                                                      (#{rest\ 2234}#
                                                        #{r\ 2430}#
                                                        #{req\ 2342}#
                                                        #{opt\ 2343}#
                                                        (cons #t
                                                              (reverse
                                                                #{rkey\ 2344}#))))
                                                    #{tmp\ 2420}#)
                                                  (let ((#{tmp\ 2431}#
                                                          ($sc-dispatch
                                                            #{tmp\ 2349}#
                                                            '(any any))))
                                                    (if (if #{tmp\ 2431}#
                                                          (@apply
                                                            (lambda (#{a\ 2434}#
                                                                     #{b\ 2435}#)
                                                              (eq? (syntax->datum
                                                                     #{a\ 2434}#)
                                                                   #:rest))
                                                            #{tmp\ 2431}#)
                                                          #f)
                                                      (@apply
                                                        (lambda (#{a\ 2438}#
                                                                 #{b\ 2439}#)
                                                          (#{rest\ 2234}#
                                                            #{b\ 2439}#
                                                            #{req\ 2342}#
                                                            #{opt\ 2343}#
                                                            (cons #f
                                                                  (reverse
                                                                    #{rkey\ 2344}#))))
                                                        #{tmp\ 2431}#)
                                                      (let ((#{tmp\ 2440}#
                                                              (list #{tmp\ 2349}#)))
                                                        (if (if #{tmp\ 2440}#
                                                              (@apply
                                                                (lambda (#{r\ 2442}#)
                                                                  (#{id?\ 388}#
                                                                    #{r\ 2442}#))
                                                                #{tmp\ 2440}#)
                                                              #f)
                                                          (@apply
                                                            (lambda (#{r\ 2444}#)
                                                              (#{rest\ 2234}#
                                                                #{r\ 2444}#
                                                                #{req\ 2342}#
                                                                #{opt\ 2343}#
                                                                (cons #f
                                                                      (reverse
                                                                        #{rkey\ 2344}#))))
                                                            #{tmp\ 2440}#)
                                                          (let ((#{else\ 2446}#
                                                                  #{tmp\ 2349}#))
                                                            (syntax-violation
                                                              'lambda*
                                                              "invalid keyword argument list"
                                                              #{orig-args\ 2225}#
                                                              #{args\ 2341}#)))))))))))))))))))))))
                (#{rest\ 2234}#
                  (lambda (#{args\ 2447}#
                           #{req\ 2448}#
                           #{opt\ 2449}#
                           #{kw\ 2450}#)
                    (let ((#{tmp\ 2455}# #{args\ 2447}#))
                      (let ((#{tmp\ 2456}# (list #{tmp\ 2455}#)))
                        (if (if #{tmp\ 2456}#
                              (@apply
                                (lambda (#{r\ 2458}#)
                                  (#{id?\ 388}# #{r\ 2458}#))
                                #{tmp\ 2456}#)
                              #f)
                          (@apply
                            (lambda (#{r\ 2460}#)
                              (#{check\ 2236}#
                                #{req\ 2448}#
                                #{opt\ 2449}#
                                #{r\ 2460}#
                                #{kw\ 2450}#))
                            #{tmp\ 2456}#)
                          (let ((#{else\ 2462}# #{tmp\ 2455}#))
                            (syntax-violation
                              'lambda*
                              "invalid rest argument"
                              #{orig-args\ 2225}#
                              #{args\ 2447}#)))))))
                (#{check\ 2236}#
                  (lambda (#{req\ 2463}#
                           #{opt\ 2464}#
                           #{rest\ 2465}#
                           #{kw\ 2466}#)
                    (if (#{distinct-bound-ids?\ 453}#
                          (append
                            #{req\ 2463}#
                            (map car #{opt\ 2464}#)
                            (if #{rest\ 2465}#
                              (list #{rest\ 2465}#)
                              '())
                            (if (pair? #{kw\ 2466}#)
                              (map cadr (cdr #{kw\ 2466}#))
                              '())))
                      (values
                        #{req\ 2463}#
                        #{opt\ 2464}#
                        #{rest\ 2465}#
                        #{kw\ 2466}#)
                      (syntax-violation
                        'lambda*
                        "duplicate identifier in argument list"
                        #{orig-args\ 2225}#)))))
               (begin
                 (#{req\ 2228}# #{orig-args\ 2225}# (quote ()))))))
         (#{chi-lambda-case\ 497}#
           (lambda (#{e\ 2474}#
                    #{r\ 2475}#
                    #{w\ 2476}#
                    #{s\ 2477}#
                    #{mod\ 2478}#
                    #{get-formals\ 2479}#
                    #{clauses\ 2480}#)
             (letrec*
               ((#{expand-req\ 2489}#
                  (lambda (#{req\ 2496}#
                           #{opt\ 2497}#
                           #{rest\ 2498}#
                           #{kw\ 2499}#
                           #{body\ 2500}#)
                    (begin
                      (let ((#{vars\ 2508}#
                              (map #{gen-var\ 501}# #{req\ 2496}#))
                            (#{labels\ 2509}#
                              (#{gen-labels\ 406}# #{req\ 2496}#)))
                        (begin
                          (let ((#{r*\ 2512}#
                                  (#{extend-var-env\ 378}#
                                    #{labels\ 2509}#
                                    #{vars\ 2508}#
                                    #{r\ 2475}#))
                                (#{w*\ 2513}#
                                  (#{make-binding-wrap\ 435}#
                                    #{req\ 2496}#
                                    #{labels\ 2509}#
                                    #{w\ 2476}#)))
                            (#{expand-opt\ 2491}#
                              (map syntax->datum #{req\ 2496}#)
                              #{opt\ 2497}#
                              #{rest\ 2498}#
                              #{kw\ 2499}#
                              #{body\ 2500}#
                              (reverse #{vars\ 2508}#)
                              #{r*\ 2512}#
                              #{w*\ 2513}#
                              '()
                              '())))))))
                (#{expand-opt\ 2491}#
                  (lambda (#{req\ 2514}#
                           #{opt\ 2515}#
                           #{rest\ 2516}#
                           #{kw\ 2517}#
                           #{body\ 2518}#
                           #{vars\ 2519}#
                           #{r*\ 2520}#
                           #{w*\ 2521}#
                           #{out\ 2522}#
                           #{inits\ 2523}#)
                    (if (pair? #{opt\ 2515}#)
                      (let ((#{tmp\ 2536}# (car #{opt\ 2515}#)))
                        (let ((#{tmp\ 2537}#
                                ($sc-dispatch
                                  #{tmp\ 2536}#
                                  '(any any))))
                          (if #{tmp\ 2537}#
                            (@apply
                              (lambda (#{id\ 2540}# #{i\ 2541}#)
                                (begin
                                  (let ((#{v\ 2544}#
                                          (#{gen-var\ 501}# #{id\ 2540}#)))
                                    (begin
                                      (let ((#{l\ 2546}#
                                              (#{gen-labels\ 406}#
                                                (list #{v\ 2544}#))))
                                        (begin
                                          (let ((#{r**\ 2548}#
                                                  (#{extend-var-env\ 378}#
                                                    #{l\ 2546}#
                                                    (list #{v\ 2544}#)
                                                    #{r*\ 2520}#)))
                                            (begin
                                              (let ((#{w**\ 2550}#
                                                      (#{make-binding-wrap\ 435}#
                                                        (list #{id\ 2540}#)
                                                        #{l\ 2546}#
                                                        #{w*\ 2521}#)))
                                                (#{expand-opt\ 2491}#
                                                  #{req\ 2514}#
                                                  (cdr #{opt\ 2515}#)
                                                  #{rest\ 2516}#
                                                  #{kw\ 2517}#
                                                  #{body\ 2518}#
                                                  (cons #{v\ 2544}#
                                                        #{vars\ 2519}#)
                                                  #{r**\ 2548}#
                                                  #{w**\ 2550}#
                                                  (cons (syntax->datum
                                                          #{id\ 2540}#)
                                                        #{out\ 2522}#)
                                                  (cons (#{chi\ 473}#
                                                          #{i\ 2541}#
                                                          #{r*\ 2520}#
                                                          #{w*\ 2521}#
                                                          #{mod\ 2478}#)
                                                        #{inits\ 2523}#)))))))))))
                              #{tmp\ 2537}#)
                            (syntax-violation
                              #f
                              "source expression failed to match any pattern"
                              #{tmp\ 2536}#))))
                      (if #{rest\ 2516}#
                        (begin
                          (let ((#{v\ 2555}#
                                  (#{gen-var\ 501}# #{rest\ 2516}#)))
                            (begin
                              (let ((#{l\ 2557}#
                                      (#{gen-labels\ 406}#
                                        (list #{v\ 2555}#))))
                                (begin
                                  (let ((#{r*\ 2559}#
                                          (#{extend-var-env\ 378}#
                                            #{l\ 2557}#
                                            (list #{v\ 2555}#)
                                            #{r*\ 2520}#)))
                                    (begin
                                      (let ((#{w*\ 2561}#
                                              (#{make-binding-wrap\ 435}#
                                                (list #{rest\ 2516}#)
                                                #{l\ 2557}#
                                                #{w*\ 2521}#)))
                                        (#{expand-kw\ 2493}#
                                          #{req\ 2514}#
                                          (if (pair? #{out\ 2522}#)
                                            (reverse #{out\ 2522}#)
                                            #f)
                                          (syntax->datum #{rest\ 2516}#)
                                          (if (pair? #{kw\ 2517}#)
                                            (cdr #{kw\ 2517}#)
                                            #{kw\ 2517}#)
                                          #{body\ 2518}#
                                          (cons #{v\ 2555}# #{vars\ 2519}#)
                                          #{r*\ 2559}#
                                          #{w*\ 2561}#
                                          (if (pair? #{kw\ 2517}#)
                                            (car #{kw\ 2517}#)
                                            #f)
                                          '()
                                          #{inits\ 2523}#)))))))))
                        (#{expand-kw\ 2493}#
                          #{req\ 2514}#
                          (if (pair? #{out\ 2522}#)
                            (reverse #{out\ 2522}#)
                            #f)
                          #f
                          (if (pair? #{kw\ 2517}#)
                            (cdr #{kw\ 2517}#)
                            #{kw\ 2517}#)
                          #{body\ 2518}#
                          #{vars\ 2519}#
                          #{r*\ 2520}#
                          #{w*\ 2521}#
                          (if (pair? #{kw\ 2517}#) (car #{kw\ 2517}#) #f)
                          '()
                          #{inits\ 2523}#)))))
                (#{expand-kw\ 2493}#
                  (lambda (#{req\ 2563}#
                           #{opt\ 2564}#
                           #{rest\ 2565}#
                           #{kw\ 2566}#
                           #{body\ 2567}#
                           #{vars\ 2568}#
                           #{r*\ 2569}#
                           #{w*\ 2570}#
                           #{aok\ 2571}#
                           #{out\ 2572}#
                           #{inits\ 2573}#)
                    (if (pair? #{kw\ 2566}#)
                      (let ((#{tmp\ 2587}# (car #{kw\ 2566}#)))
                        (let ((#{tmp\ 2588}#
                                ($sc-dispatch
                                  #{tmp\ 2587}#
                                  '(any any any))))
                          (if #{tmp\ 2588}#
                            (@apply
                              (lambda (#{k\ 2592}# #{id\ 2593}# #{i\ 2594}#)
                                (begin
                                  (let ((#{v\ 2597}#
                                          (#{gen-var\ 501}# #{id\ 2593}#)))
                                    (begin
                                      (let ((#{l\ 2599}#
                                              (#{gen-labels\ 406}#
                                                (list #{v\ 2597}#))))
                                        (begin
                                          (let ((#{r**\ 2601}#
                                                  (#{extend-var-env\ 378}#
                                                    #{l\ 2599}#
                                                    (list #{v\ 2597}#)
                                                    #{r*\ 2569}#)))
                                            (begin
                                              (let ((#{w**\ 2603}#
                                                      (#{make-binding-wrap\ 435}#
                                                        (list #{id\ 2593}#)
                                                        #{l\ 2599}#
                                                        #{w*\ 2570}#)))
                                                (#{expand-kw\ 2493}#
                                                  #{req\ 2563}#
                                                  #{opt\ 2564}#
                                                  #{rest\ 2565}#
                                                  (cdr #{kw\ 2566}#)
                                                  #{body\ 2567}#
                                                  (cons #{v\ 2597}#
                                                        #{vars\ 2568}#)
                                                  #{r**\ 2601}#
                                                  #{w**\ 2603}#
                                                  #{aok\ 2571}#
                                                  (cons (list (syntax->datum
                                                                #{k\ 2592}#)
                                                              (syntax->datum
                                                                #{id\ 2593}#)
                                                              #{v\ 2597}#)
                                                        #{out\ 2572}#)
                                                  (cons (#{chi\ 473}#
                                                          #{i\ 2594}#
                                                          #{r*\ 2569}#
                                                          #{w*\ 2570}#
                                                          #{mod\ 2478}#)
                                                        #{inits\ 2573}#)))))))))))
                              #{tmp\ 2588}#)
                            (syntax-violation
                              #f
                              "source expression failed to match any pattern"
                              #{tmp\ 2587}#))))
                      (#{expand-body\ 2495}#
                        #{req\ 2563}#
                        #{opt\ 2564}#
                        #{rest\ 2565}#
                        (if (begin
                              (let ((#{t\ 2607}# #{aok\ 2571}#))
                                (if #{t\ 2607}#
                                  #{t\ 2607}#
                                  (pair? #{out\ 2572}#))))
                          (cons #{aok\ 2571}# (reverse #{out\ 2572}#))
                          #f)
                        #{body\ 2567}#
                        (reverse #{vars\ 2568}#)
                        #{r*\ 2569}#
                        #{w*\ 2570}#
                        (reverse #{inits\ 2573}#)
                        '()))))
                (#{expand-body\ 2495}#
                  (lambda (#{req\ 2609}#
                           #{opt\ 2610}#
                           #{rest\ 2611}#
                           #{kw\ 2612}#
                           #{body\ 2613}#
                           #{vars\ 2614}#
                           #{r*\ 2615}#
                           #{w*\ 2616}#
                           #{inits\ 2617}#
                           #{meta\ 2618}#)
                    (let ((#{tmp\ 2629}# #{body\ 2613}#))
                      (let ((#{tmp\ 2630}#
                              ($sc-dispatch
                                #{tmp\ 2629}#
                                '(any any . each-any))))
                        (if (if #{tmp\ 2630}#
                              (@apply
                                (lambda (#{docstring\ 2634}#
                                         #{e1\ 2635}#
                                         #{e2\ 2636}#)
                                  (string?
                                    (syntax->datum #{docstring\ 2634}#)))
                                #{tmp\ 2630}#)
                              #f)
                          (@apply
                            (lambda (#{docstring\ 2640}#
                                     #{e1\ 2641}#
                                     #{e2\ 2642}#)
                              (#{expand-body\ 2495}#
                                #{req\ 2609}#
                                #{opt\ 2610}#
                                #{rest\ 2611}#
                                #{kw\ 2612}#
                                (cons #{e1\ 2641}# #{e2\ 2642}#)
                                #{vars\ 2614}#
                                #{r*\ 2615}#
                                #{w*\ 2616}#
                                #{inits\ 2617}#
                                (append
                                  #{meta\ 2618}#
                                  (list (cons 'documentation
                                              (syntax->datum
                                                #{docstring\ 2640}#))))))
                            #{tmp\ 2630}#)
                          (let ((#{tmp\ 2645}#
                                  ($sc-dispatch
                                    #{tmp\ 2629}#
                                    '(#(vector #(each (any . any)))
                                      any
                                      .
                                      each-any))))
                            (if #{tmp\ 2645}#
                              (@apply
                                (lambda (#{k\ 2650}#
                                         #{v\ 2651}#
                                         #{e1\ 2652}#
                                         #{e2\ 2653}#)
                                  (#{expand-body\ 2495}#
                                    #{req\ 2609}#
                                    #{opt\ 2610}#
                                    #{rest\ 2611}#
                                    #{kw\ 2612}#
                                    (cons #{e1\ 2652}# #{e2\ 2653}#)
                                    #{vars\ 2614}#
                                    #{r*\ 2615}#
                                    #{w*\ 2616}#
                                    #{inits\ 2617}#
                                    (append
                                      #{meta\ 2618}#
                                      (syntax->datum
                                        (map cons #{k\ 2650}# #{v\ 2651}#)))))
                                #{tmp\ 2645}#)
                              (let ((#{tmp\ 2657}#
                                      ($sc-dispatch
                                        #{tmp\ 2629}#
                                        '(any . each-any))))
                                (if #{tmp\ 2657}#
                                  (@apply
                                    (lambda (#{e1\ 2660}# #{e2\ 2661}#)
                                      (values
                                        #{meta\ 2618}#
                                        #{req\ 2609}#
                                        #{opt\ 2610}#
                                        #{rest\ 2611}#
                                        #{kw\ 2612}#
                                        #{inits\ 2617}#
                                        #{vars\ 2614}#
                                        (#{chi-body\ 481}#
                                          (cons #{e1\ 2660}# #{e2\ 2661}#)
                                          (#{source-wrap\ 459}#
                                            #{e\ 2474}#
                                            #{w\ 2476}#
                                            #{s\ 2477}#
                                            #{mod\ 2478}#)
                                          #{r*\ 2615}#
                                          #{w*\ 2616}#
                                          #{mod\ 2478}#)))
                                    #{tmp\ 2657}#)
                                  (syntax-violation
                                    #f
                                    "source expression failed to match any pattern"
                                    #{tmp\ 2629}#)))))))))))
               (begin
                 (let ((#{tmp\ 2663}# #{clauses\ 2480}#))
                   (let ((#{tmp\ 2664}#
                           ($sc-dispatch #{tmp\ 2663}# (quote ()))))
                     (if #{tmp\ 2664}#
                       (@apply
                         (lambda () (values (quote ()) #f))
                         #{tmp\ 2664}#)
                       (let ((#{tmp\ 2665}#
                               ($sc-dispatch
                                 #{tmp\ 2663}#
                                 '((any any . each-any)
                                   .
                                   #(each (any any . each-any))))))
                         (if #{tmp\ 2665}#
                           (@apply
                             (lambda (#{args\ 2672}#
                                      #{e1\ 2673}#
                                      #{e2\ 2674}#
                                      #{args*\ 2675}#
                                      #{e1*\ 2676}#
                                      #{e2*\ 2677}#)
                               (call-with-values
                                 (lambda ()
                                   (#{get-formals\ 2479}# #{args\ 2672}#))
                                 (lambda (#{req\ 2678}#
                                          #{opt\ 2679}#
                                          #{rest\ 2680}#
                                          #{kw\ 2681}#)
                                   (call-with-values
                                     (lambda ()
                                       (#{expand-req\ 2489}#
                                         #{req\ 2678}#
                                         #{opt\ 2679}#
                                         #{rest\ 2680}#
                                         #{kw\ 2681}#
                                         (cons #{e1\ 2673}# #{e2\ 2674}#)))
                                     (lambda (#{meta\ 2687}#
                                              #{req\ 2688}#
                                              #{opt\ 2689}#
                                              #{rest\ 2690}#
                                              #{kw\ 2691}#
                                              #{inits\ 2692}#
                                              #{vars\ 2693}#
                                              #{body\ 2694}#)
                                       (call-with-values
                                         (lambda ()
                                           (#{chi-lambda-case\ 497}#
                                             #{e\ 2474}#
                                             #{r\ 2475}#
                                             #{w\ 2476}#
                                             #{s\ 2477}#
                                             #{mod\ 2478}#
                                             #{get-formals\ 2479}#
                                             (map (lambda (#{tmp\ 2705}#
                                                           #{tmp\ 2704}#
                                                           #{tmp\ 2703}#)
                                                    (cons #{tmp\ 2703}#
                                                          (cons #{tmp\ 2704}#
                                                                #{tmp\ 2705}#)))
                                                  #{e2*\ 2677}#
                                                  #{e1*\ 2676}#
                                                  #{args*\ 2675}#)))
                                         (lambda (#{meta*\ 2707}#
                                                  #{else*\ 2708}#)
                                           (values
                                             (append
                                               #{meta\ 2687}#
                                               #{meta*\ 2707}#)
                                             (#{build-lambda-case\ 334}#
                                               #{s\ 2477}#
                                               #{req\ 2688}#
                                               #{opt\ 2689}#
                                               #{rest\ 2690}#
                                               #{kw\ 2691}#
                                               #{inits\ 2692}#
                                               #{vars\ 2693}#
                                               #{body\ 2694}#
                                               #{else*\ 2708}#)))))))))
                             #{tmp\ 2665}#)
                           (syntax-violation
                             #f
                             "source expression failed to match any pattern"
                             #{tmp\ 2663}#))))))))))
         (#{strip\ 499}#
           (lambda (#{x\ 2711}# #{w\ 2712}#)
             (if (memq 'top
                       (#{wrap-marks\ 395}# #{w\ 2712}#))
               #{x\ 2711}#
               (letrec*
                 ((#{f\ 2718}#
                    (lambda (#{x\ 2719}#)
                      (if (#{syntax-object?\ 352}# #{x\ 2719}#)
                        (#{strip\ 499}#
                          (#{syntax-object-expression\ 354}# #{x\ 2719}#)
                          (#{syntax-object-wrap\ 356}# #{x\ 2719}#))
                        (if (pair? #{x\ 2719}#)
                          (begin
                            (let ((#{a\ 2726}# (#{f\ 2718}# (car #{x\ 2719}#)))
                                  (#{d\ 2727}#
                                    (#{f\ 2718}# (cdr #{x\ 2719}#))))
                              (if (if (eq? #{a\ 2726}# (car #{x\ 2719}#))
                                    (eq? #{d\ 2727}# (cdr #{x\ 2719}#))
                                    #f)
                                #{x\ 2719}#
                                (cons #{a\ 2726}# #{d\ 2727}#))))
                          (if (vector? #{x\ 2719}#)
                            (begin
                              (let ((#{old\ 2733}# (vector->list #{x\ 2719}#)))
                                (begin
                                  (let ((#{new\ 2735}#
                                          (map #{f\ 2718}# #{old\ 2733}#)))
                                    (if (#{and-map*\ 36}#
                                          eq?
                                          #{old\ 2733}#
                                          #{new\ 2735}#)
                                      #{x\ 2719}#
                                      (list->vector #{new\ 2735}#))))))
                            #{x\ 2719}#))))))
                 (begin (#{f\ 2718}# #{x\ 2711}#))))))
         (#{gen-var\ 501}#
           (lambda (#{id\ 2737}#)
             (begin
               (let ((#{id\ 2740}#
                       (if (#{syntax-object?\ 352}# #{id\ 2737}#)
                         (#{syntax-object-expression\ 354}# #{id\ 2737}#)
                         #{id\ 2737}#)))
                 (gensym
                   (string-append (symbol->string #{id\ 2740}#) " "))))))
         (#{lambda-var-list\ 503}#
           (lambda (#{vars\ 2742}#)
             (letrec*
               ((#{lvl\ 2748}#
                  (lambda (#{vars\ 2749}# #{ls\ 2750}# #{w\ 2751}#)
                    (if (pair? #{vars\ 2749}#)
                      (#{lvl\ 2748}#
                        (cdr #{vars\ 2749}#)
                        (cons (#{wrap\ 457}#
                                (car #{vars\ 2749}#)
                                #{w\ 2751}#
                                #f)
                              #{ls\ 2750}#)
                        #{w\ 2751}#)
                      (if (#{id?\ 388}# #{vars\ 2749}#)
                        (cons (#{wrap\ 457}# #{vars\ 2749}# #{w\ 2751}# #f)
                              #{ls\ 2750}#)
                        (if (null? #{vars\ 2749}#)
                          #{ls\ 2750}#
                          (if (#{syntax-object?\ 352}# #{vars\ 2749}#)
                            (#{lvl\ 2748}#
                              (#{syntax-object-expression\ 354}#
                                #{vars\ 2749}#)
                              #{ls\ 2750}#
                              (#{join-wraps\ 439}#
                                #{w\ 2751}#
                                (#{syntax-object-wrap\ 356}# #{vars\ 2749}#)))
                            (cons #{vars\ 2749}# #{ls\ 2750}#))))))))
               (begin
                 (#{lvl\ 2748}#
                   #{vars\ 2742}#
                   '()
                   '(())))))))
        (begin
          (set! #{make-primitive-ref\ 250}#
            (lambda (#{src\ 724}# #{name\ 725}#)
              (make-struct/no-tail
                (vector-ref %expanded-vtables 2)
                #{src\ 724}#
                #{name\ 725}#)))
          (set! #{fx+\ 289}# +)
          (set! #{fx-\ 291}# -)
          (set! #{fx=\ 293}# =)
          (set! #{fx<\ 295}# <)
          (set! #{set-syntax-object-expression!\ 360}#
            (lambda (#{x\ 1102}# #{update\ 1103}#)
              (vector-set! #{x\ 1102}# 1 #{update\ 1103}#)))
          (set! #{set-syntax-object-wrap!\ 362}#
            (lambda (#{x\ 1106}# #{update\ 1107}#)
              (vector-set! #{x\ 1106}# 2 #{update\ 1107}#)))
          (set! #{set-syntax-object-module!\ 364}#
            (lambda (#{x\ 1110}# #{update\ 1111}#)
              (vector-set! #{x\ 1110}# 3 #{update\ 1111}#)))
          (set! #{binding-type\ 371}# car)
          (set! #{binding-value\ 373}# cdr)
          (set! #{make-wrap\ 393}# cons)
          (set! #{wrap-marks\ 395}# car)
          (set! #{wrap-subst\ 397}# cdr)
          (set! #{ribcage?\ 411}#
            (lambda (#{x\ 1188}#)
              (if (vector? #{x\ 1188}#)
                (if (= (vector-length #{x\ 1188}#) 4)
                  (eq? (vector-ref #{x\ 1188}# 0) (quote ribcage))
                  #f)
                #f)))
          (begin
            (#{global-extend\ 384}#
              'local-syntax
              'letrec-syntax
              #t)
            (#{global-extend\ 384}#
              'local-syntax
              'let-syntax
              #f)
            (#{global-extend\ 384}#
              'core
              'fluid-let-syntax
              (lambda (#{e\ 2762}#
                       #{r\ 2763}#
                       #{w\ 2764}#
                       #{s\ 2765}#
                       #{mod\ 2766}#)
                (let ((#{tmp\ 2772}# #{e\ 2762}#))
                  (let ((#{tmp\ 2773}#
                          ($sc-dispatch
                            #{tmp\ 2772}#
                            '(any #(each (any any)) any . each-any))))
                    (if (if #{tmp\ 2773}#
                          (@apply
                            (lambda (#{_\ 2779}#
                                     #{var\ 2780}#
                                     #{val\ 2781}#
                                     #{e1\ 2782}#
                                     #{e2\ 2783}#)
                              (#{valid-bound-ids?\ 451}# #{var\ 2780}#))
                            #{tmp\ 2773}#)
                          #f)
                      (@apply
                        (lambda (#{_\ 2790}#
                                 #{var\ 2791}#
                                 #{val\ 2792}#
                                 #{e1\ 2793}#
                                 #{e2\ 2794}#)
                          (begin
                            (let ((#{names\ 2796}#
                                    (map (lambda (#{x\ 2797}#)
                                           (#{id-var-name\ 445}#
                                             #{x\ 2797}#
                                             #{w\ 2764}#))
                                         #{var\ 2791}#)))
                              (begin
                                (for-each
                                  (lambda (#{id\ 2800}# #{n\ 2801}#)
                                    (begin
                                      (let ((#{atom-key\ 2806}#
                                              (#{binding-type\ 371}#
                                                (#{lookup\ 382}#
                                                  #{n\ 2801}#
                                                  #{r\ 2763}#
                                                  #{mod\ 2766}#))))
                                        (if (eqv? #{atom-key\ 2806}#
                                                  'displaced-lexical)
                                          (syntax-violation
                                            'fluid-let-syntax
                                            "identifier out of context"
                                            #{e\ 2762}#
                                            (#{source-wrap\ 459}#
                                              #{id\ 2800}#
                                              #{w\ 2764}#
                                              #{s\ 2765}#
                                              #{mod\ 2766}#))))))
                                  #{var\ 2791}#
                                  #{names\ 2796}#)
                                (#{chi-body\ 481}#
                                  (cons #{e1\ 2793}# #{e2\ 2794}#)
                                  (#{source-wrap\ 459}#
                                    #{e\ 2762}#
                                    #{w\ 2764}#
                                    #{s\ 2765}#
                                    #{mod\ 2766}#)
                                  (#{extend-env\ 376}#
                                    #{names\ 2796}#
                                    (begin
                                      (let ((#{trans-r\ 2811}#
                                              (#{macros-only-env\ 380}#
                                                #{r\ 2763}#)))
                                        (map (lambda (#{x\ 2812}#)
                                               (cons 'macro
                                                     (#{eval-local-transformer\ 485}#
                                                       (#{chi\ 473}#
                                                         #{x\ 2812}#
                                                         #{trans-r\ 2811}#
                                                         #{w\ 2764}#
                                                         #{mod\ 2766}#)
                                                       #{mod\ 2766}#)))
                                             #{val\ 2792}#)))
                                    #{r\ 2763}#)
                                  #{w\ 2764}#
                                  #{mod\ 2766}#)))))
                        #{tmp\ 2773}#)
                      (let ((#{_\ 2817}# #{tmp\ 2772}#))
                        (syntax-violation
                          'fluid-let-syntax
                          "bad syntax"
                          (#{source-wrap\ 459}#
                            #{e\ 2762}#
                            #{w\ 2764}#
                            #{s\ 2765}#
                            #{mod\ 2766}#))))))))
            (#{global-extend\ 384}#
              'core
              'quote
              (lambda (#{e\ 2818}#
                       #{r\ 2819}#
                       #{w\ 2820}#
                       #{s\ 2821}#
                       #{mod\ 2822}#)
                (let ((#{tmp\ 2828}# #{e\ 2818}#))
                  (let ((#{tmp\ 2829}#
                          ($sc-dispatch #{tmp\ 2828}# (quote (any any)))))
                    (if #{tmp\ 2829}#
                      (@apply
                        (lambda (#{_\ 2832}# #{e\ 2833}#)
                          (#{build-data\ 338}#
                            #{s\ 2821}#
                            (#{strip\ 499}# #{e\ 2833}# #{w\ 2820}#)))
                        #{tmp\ 2829}#)
                      (let ((#{_\ 2835}# #{tmp\ 2828}#))
                        (syntax-violation
                          'quote
                          "bad syntax"
                          (#{source-wrap\ 459}#
                            #{e\ 2818}#
                            #{w\ 2820}#
                            #{s\ 2821}#
                            #{mod\ 2822}#))))))))
            (#{global-extend\ 384}#
              'core
              'syntax
              (letrec*
                ((#{gen-syntax\ 2837}#
                   (lambda (#{src\ 2852}#
                            #{e\ 2853}#
                            #{r\ 2854}#
                            #{maps\ 2855}#
                            #{ellipsis?\ 2856}#
                            #{mod\ 2857}#)
                     (if (#{id?\ 388}# #{e\ 2853}#)
                       (begin
                         (let ((#{label\ 2865}#
                                 (#{id-var-name\ 445}#
                                   #{e\ 2853}#
                                   '(()))))
                           (begin
                             (let ((#{b\ 2868}#
                                     (#{lookup\ 382}#
                                       #{label\ 2865}#
                                       #{r\ 2854}#
                                       #{mod\ 2857}#)))
                               (if (eq? (#{binding-type\ 371}# #{b\ 2868}#)
                                        'syntax)
                                 (call-with-values
                                   (lambda ()
                                     (begin
                                       (let ((#{var.lev\ 2870}#
                                               (#{binding-value\ 373}#
                                                 #{b\ 2868}#)))
                                         (#{gen-ref\ 2839}#
                                           #{src\ 2852}#
                                           (car #{var.lev\ 2870}#)
                                           (cdr #{var.lev\ 2870}#)
                                           #{maps\ 2855}#))))
                                   (lambda (#{var\ 2871}# #{maps\ 2872}#)
                                     (values
                                       (list (quote ref) #{var\ 2871}#)
                                       #{maps\ 2872}#)))
                                 (if (#{ellipsis?\ 2856}# #{e\ 2853}#)
                                   (syntax-violation
                                     'syntax
                                     "misplaced ellipsis"
                                     #{src\ 2852}#)
                                   (values
                                     (list (quote quote) #{e\ 2853}#)
                                     #{maps\ 2855}#)))))))
                       (let ((#{tmp\ 2877}# #{e\ 2853}#))
                         (let ((#{tmp\ 2878}#
                                 ($sc-dispatch
                                   #{tmp\ 2877}#
                                   '(any any))))
                           (if (if #{tmp\ 2878}#
                                 (@apply
                                   (lambda (#{dots\ 2881}# #{e\ 2882}#)
                                     (#{ellipsis?\ 2856}# #{dots\ 2881}#))
                                   #{tmp\ 2878}#)
                                 #f)
                             (@apply
                               (lambda (#{dots\ 2885}# #{e\ 2886}#)
                                 (#{gen-syntax\ 2837}#
                                   #{src\ 2852}#
                                   #{e\ 2886}#
                                   #{r\ 2854}#
                                   #{maps\ 2855}#
                                   (lambda (#{x\ 2887}#) #f)
                                   #{mod\ 2857}#))
                               #{tmp\ 2878}#)
                             (let ((#{tmp\ 2889}#
                                     ($sc-dispatch
                                       #{tmp\ 2877}#
                                       '(any any . any))))
                               (if (if #{tmp\ 2889}#
                                     (@apply
                                       (lambda (#{x\ 2893}#
                                                #{dots\ 2894}#
                                                #{y\ 2895}#)
                                         (#{ellipsis?\ 2856}# #{dots\ 2894}#))
                                       #{tmp\ 2889}#)
                                     #f)
                                 (@apply
                                   (lambda (#{x\ 2899}#
                                            #{dots\ 2900}#
                                            #{y\ 2901}#)
                                     (letrec*
                                       ((#{f\ 2905}#
                                          (lambda (#{y\ 2906}# #{k\ 2907}#)
                                            (let ((#{tmp\ 2914}# #{y\ 2906}#))
                                              (let ((#{tmp\ 2915}#
                                                      ($sc-dispatch
                                                        #{tmp\ 2914}#
                                                        '(any . any))))
                                                (if (if #{tmp\ 2915}#
                                                      (@apply
                                                        (lambda (#{dots\ 2918}#
                                                                 #{y\ 2919}#)
                                                          (#{ellipsis?\ 2856}#
                                                            #{dots\ 2918}#))
                                                        #{tmp\ 2915}#)
                                                      #f)
                                                  (@apply
                                                    (lambda (#{dots\ 2922}#
                                                             #{y\ 2923}#)
                                                      (#{f\ 2905}#
                                                        #{y\ 2923}#
                                                        (lambda (#{maps\ 2924}#)
                                                          (call-with-values
                                                            (lambda ()
                                                              (#{k\ 2907}#
                                                                (cons '()
                                                                      #{maps\ 2924}#)))
                                                            (lambda (#{x\ 2926}#
                                                                     #{maps\ 2927}#)
                                                              (if (null? (car #{maps\ 2927}#))
                                                                (syntax-violation
                                                                  'syntax
                                                                  "extra ellipsis"
                                                                  #{src\ 2852}#)
                                                                (values
                                                                  (#{gen-mappend\ 2841}#
                                                                    #{x\ 2926}#
                                                                    (car #{maps\ 2927}#))
                                                                  (cdr #{maps\ 2927}#))))))))
                                                    #{tmp\ 2915}#)
                                                  (let ((#{_\ 2931}#
                                                          #{tmp\ 2914}#))
                                                    (call-with-values
                                                      (lambda ()
                                                        (#{gen-syntax\ 2837}#
                                                          #{src\ 2852}#
                                                          #{y\ 2906}#
                                                          #{r\ 2854}#
                                                          #{maps\ 2855}#
                                                          #{ellipsis?\ 2856}#
                                                          #{mod\ 2857}#))
                                                      (lambda (#{y\ 2932}#
                                                               #{maps\ 2933}#)
                                                        (call-with-values
                                                          (lambda ()
                                                            (#{k\ 2907}#
                                                              #{maps\ 2933}#))
                                                          (lambda (#{x\ 2936}#
                                                                   #{maps\ 2937}#)
                                                            (values
                                                              (#{gen-append\ 2847}#
                                                                #{x\ 2936}#
                                                                #{y\ 2932}#)
                                                              #{maps\ 2937}#))))))))))))
                                       (begin
                                         (#{f\ 2905}#
                                           #{y\ 2901}#
                                           (lambda (#{maps\ 2908}#)
                                             (call-with-values
                                               (lambda ()
                                                 (#{gen-syntax\ 2837}#
                                                   #{src\ 2852}#
                                                   #{x\ 2899}#
                                                   #{r\ 2854}#
                                                   (cons '()
                                                         #{maps\ 2908}#)
                                                   #{ellipsis?\ 2856}#
                                                   #{mod\ 2857}#))
                                               (lambda (#{x\ 2910}#
                                                        #{maps\ 2911}#)
                                                 (if (null? (car #{maps\ 2911}#))
                                                   (syntax-violation
                                                     'syntax
                                                     "extra ellipsis"
                                                     #{src\ 2852}#)
                                                   (values
                                                     (#{gen-map\ 2843}#
                                                       #{x\ 2910}#
                                                       (car #{maps\ 2911}#))
                                                     (cdr #{maps\ 2911}#))))))))))
                                   #{tmp\ 2889}#)
                                 (let ((#{tmp\ 2940}#
                                         ($sc-dispatch
                                           #{tmp\ 2877}#
                                           '(any . any))))
                                   (if #{tmp\ 2940}#
                                     (@apply
                                       (lambda (#{x\ 2943}# #{y\ 2944}#)
                                         (call-with-values
                                           (lambda ()
                                             (#{gen-syntax\ 2837}#
                                               #{src\ 2852}#
                                               #{x\ 2943}#
                                               #{r\ 2854}#
                                               #{maps\ 2855}#
                                               #{ellipsis?\ 2856}#
                                               #{mod\ 2857}#))
                                           (lambda (#{x\ 2945}# #{maps\ 2946}#)
                                             (call-with-values
                                               (lambda ()
                                                 (#{gen-syntax\ 2837}#
                                                   #{src\ 2852}#
                                                   #{y\ 2944}#
                                                   #{r\ 2854}#
                                                   #{maps\ 2946}#
                                                   #{ellipsis?\ 2856}#
                                                   #{mod\ 2857}#))
                                               (lambda (#{y\ 2949}#
                                                        #{maps\ 2950}#)
                                                 (values
                                                   (#{gen-cons\ 2845}#
                                                     #{x\ 2945}#
                                                     #{y\ 2949}#)
                                                   #{maps\ 2950}#))))))
                                       #{tmp\ 2940}#)
                                     (let ((#{tmp\ 2953}#
                                             ($sc-dispatch
                                               #{tmp\ 2877}#
                                               '#(vector (any . each-any)))))
                                       (if #{tmp\ 2953}#
                                         (@apply
                                           (lambda (#{e1\ 2956}# #{e2\ 2957}#)
                                             (call-with-values
                                               (lambda ()
                                                 (#{gen-syntax\ 2837}#
                                                   #{src\ 2852}#
                                                   (cons #{e1\ 2956}#
                                                         #{e2\ 2957}#)
                                                   #{r\ 2854}#
                                                   #{maps\ 2855}#
                                                   #{ellipsis?\ 2856}#
                                                   #{mod\ 2857}#))
                                               (lambda (#{e\ 2959}#
                                                        #{maps\ 2960}#)
                                                 (values
                                                   (#{gen-vector\ 2849}#
                                                     #{e\ 2959}#)
                                                   #{maps\ 2960}#))))
                                           #{tmp\ 2953}#)
                                         (let ((#{_\ 2964}# #{tmp\ 2877}#))
                                           (values
                                             (list (quote quote) #{e\ 2853}#)
                                             #{maps\ 2855}#))))))))))))))
                 (#{gen-ref\ 2839}#
                   (lambda (#{src\ 2966}#
                            #{var\ 2967}#
                            #{level\ 2968}#
                            #{maps\ 2969}#)
                     (if (#{fx=\ 293}# #{level\ 2968}# 0)
                       (values #{var\ 2967}# #{maps\ 2969}#)
                       (if (null? #{maps\ 2969}#)
                         (syntax-violation
                           'syntax
                           "missing ellipsis"
                           #{src\ 2966}#)
                         (call-with-values
                           (lambda ()
                             (#{gen-ref\ 2839}#
                               #{src\ 2966}#
                               #{var\ 2967}#
                               (#{fx-\ 291}# #{level\ 2968}# 1)
                               (cdr #{maps\ 2969}#)))
                           (lambda (#{outer-var\ 2974}# #{outer-maps\ 2975}#)
                             (begin
                               (let ((#{b\ 2979}#
                                       (assq #{outer-var\ 2974}#
                                             (car #{maps\ 2969}#))))
                                 (if #{b\ 2979}#
                                   (values (cdr #{b\ 2979}#) #{maps\ 2969}#)
                                   (begin
                                     (let ((#{inner-var\ 2981}#
                                             (#{gen-var\ 501}# (quote tmp))))
                                       (values
                                         #{inner-var\ 2981}#
                                         (cons (cons (cons #{outer-var\ 2974}#
                                                           #{inner-var\ 2981}#)
                                                     (car #{maps\ 2969}#))
                                               #{outer-maps\ 2975}#)))))))))))))
                 (#{gen-mappend\ 2841}#
                   (lambda (#{e\ 2982}# #{map-env\ 2983}#)
                     (list 'apply
                           '(primitive append)
                           (#{gen-map\ 2843}# #{e\ 2982}# #{map-env\ 2983}#))))
                 (#{gen-map\ 2843}#
                   (lambda (#{e\ 2987}# #{map-env\ 2988}#)
                     (begin
                       (let ((#{formals\ 2993}# (map cdr #{map-env\ 2988}#))
                             (#{actuals\ 2994}#
                               (map (lambda (#{x\ 2995}#)
                                      (list (quote ref) (car #{x\ 2995}#)))
                                    #{map-env\ 2988}#)))
                         (if (eq? (car #{e\ 2987}#) (quote ref))
                           (car #{actuals\ 2994}#)
                           (if (and-map
                                 (lambda (#{x\ 3002}#)
                                   (if (eq? (car #{x\ 3002}#) (quote ref))
                                     (memq (car (cdr #{x\ 3002}#))
                                           #{formals\ 2993}#)
                                     #f))
                                 (cdr #{e\ 2987}#))
                             (cons 'map
                                   (cons (list 'primitive
                                               (car #{e\ 2987}#))
                                         (map (begin
                                                (let ((#{r\ 3008}#
                                                        (map cons
                                                             #{formals\ 2993}#
                                                             #{actuals\ 2994}#)))
                                                  (lambda (#{x\ 3009}#)
                                                    (cdr (assq (car (cdr #{x\ 3009}#))
                                                               #{r\ 3008}#)))))
                                              (cdr #{e\ 2987}#))))
                             (cons 'map
                                   (cons (list 'lambda
                                               #{formals\ 2993}#
                                               #{e\ 2987}#)
                                         #{actuals\ 2994}#))))))))
                 (#{gen-cons\ 2845}#
                   (lambda (#{x\ 3013}# #{y\ 3014}#)
                     (begin
                       (let ((#{atom-key\ 3019}# (car #{y\ 3014}#)))
                         (if (eqv? #{atom-key\ 3019}# (quote quote))
                           (if (eq? (car #{x\ 3013}#) (quote quote))
                             (list 'quote
                                   (cons (car (cdr #{x\ 3013}#))
                                         (car (cdr #{y\ 3014}#))))
                             (if (eq? (car (cdr #{y\ 3014}#)) (quote ()))
                               (list (quote list) #{x\ 3013}#)
                               (list (quote cons) #{x\ 3013}# #{y\ 3014}#)))
                           (if (eqv? #{atom-key\ 3019}# (quote list))
                             (cons 'list
                                   (cons #{x\ 3013}# (cdr #{y\ 3014}#)))
                             (list (quote cons) #{x\ 3013}# #{y\ 3014}#)))))))
                 (#{gen-append\ 2847}#
                   (lambda (#{x\ 3028}# #{y\ 3029}#)
                     (if (equal? #{y\ 3029}# (quote (quote ())))
                       #{x\ 3028}#
                       (list (quote append) #{x\ 3028}# #{y\ 3029}#))))
                 (#{gen-vector\ 2849}#
                   (lambda (#{x\ 3033}#)
                     (if (eq? (car #{x\ 3033}#) (quote list))
                       (cons (quote vector) (cdr #{x\ 3033}#))
                       (if (eq? (car #{x\ 3033}#) (quote quote))
                         (list 'quote
                               (list->vector (car (cdr #{x\ 3033}#))))
                         (list (quote list->vector) #{x\ 3033}#)))))
                 (#{regen\ 2851}#
                   (lambda (#{x\ 3043}#)
                     (begin
                       (let ((#{atom-key\ 3047}# (car #{x\ 3043}#)))
                         (if (eqv? #{atom-key\ 3047}# (quote ref))
                           (#{build-lexical-reference\ 318}#
                             'value
                             #f
                             (car (cdr #{x\ 3043}#))
                             (car (cdr #{x\ 3043}#)))
                           (if (eqv? #{atom-key\ 3047}# (quote primitive))
                             (#{build-primref\ 336}#
                               #f
                               (car (cdr #{x\ 3043}#)))
                             (if (eqv? #{atom-key\ 3047}# (quote quote))
                               (#{build-data\ 338}# #f (car (cdr #{x\ 3043}#)))
                               (if (eqv? #{atom-key\ 3047}# (quote lambda))
                                 (if (list? (car (cdr #{x\ 3043}#)))
                                   (#{build-simple-lambda\ 330}#
                                     #f
                                     (car (cdr #{x\ 3043}#))
                                     #f
                                     (car (cdr #{x\ 3043}#))
                                     '()
                                     (#{regen\ 2851}#
                                       (car (cdr (cdr #{x\ 3043}#)))))
                                   (error "how did we get here" #{x\ 3043}#))
                                 (#{build-application\ 312}#
                                   #f
                                   (#{build-primref\ 336}#
                                     #f
                                     (car #{x\ 3043}#))
                                   (map #{regen\ 2851}#
                                        (cdr #{x\ 3043}#))))))))))))
                (begin
                  (lambda (#{e\ 3059}#
                           #{r\ 3060}#
                           #{w\ 3061}#
                           #{s\ 3062}#
                           #{mod\ 3063}#)
                    (begin
                      (let ((#{e\ 3070}#
                              (#{source-wrap\ 459}#
                                #{e\ 3059}#
                                #{w\ 3061}#
                                #{s\ 3062}#
                                #{mod\ 3063}#)))
                        (let ((#{tmp\ 3071}# #{e\ 3070}#))
                          (let ((#{tmp\ 3072}#
                                  ($sc-dispatch
                                    #{tmp\ 3071}#
                                    '(any any))))
                            (if #{tmp\ 3072}#
                              (@apply
                                (lambda (#{_\ 3075}# #{x\ 3076}#)
                                  (call-with-values
                                    (lambda ()
                                      (#{gen-syntax\ 2837}#
                                        #{e\ 3070}#
                                        #{x\ 3076}#
                                        #{r\ 3060}#
                                        '()
                                        #{ellipsis?\ 489}#
                                        #{mod\ 3063}#))
                                    (lambda (#{e\ 3077}# #{maps\ 3078}#)
                                      (#{regen\ 2851}# #{e\ 3077}#))))
                                #{tmp\ 3072}#)
                              (let ((#{_\ 3082}# #{tmp\ 3071}#))
                                (syntax-violation
                                  'syntax
                                  "bad `syntax' form"
                                  #{e\ 3070}#)))))))))))
            (#{global-extend\ 384}#
              'core
              'lambda
              (lambda (#{e\ 3083}#
                       #{r\ 3084}#
                       #{w\ 3085}#
                       #{s\ 3086}#
                       #{mod\ 3087}#)
                (let ((#{tmp\ 3093}# #{e\ 3083}#))
                  (let ((#{tmp\ 3094}#
                          ($sc-dispatch
                            #{tmp\ 3093}#
                            '(any any any . each-any))))
                    (if #{tmp\ 3094}#
                      (@apply
                        (lambda (#{_\ 3099}#
                                 #{args\ 3100}#
                                 #{e1\ 3101}#
                                 #{e2\ 3102}#)
                          (call-with-values
                            (lambda ()
                              (#{lambda-formals\ 491}# #{args\ 3100}#))
                            (lambda (#{req\ 3103}#
                                     #{opt\ 3104}#
                                     #{rest\ 3105}#
                                     #{kw\ 3106}#)
                              (letrec*
                                ((#{lp\ 3114}#
                                   (lambda (#{body\ 3115}# #{meta\ 3116}#)
                                     (let ((#{tmp\ 3118}# #{body\ 3115}#))
                                       (let ((#{tmp\ 3119}#
                                               ($sc-dispatch
                                                 #{tmp\ 3118}#
                                                 '(any any . each-any))))
                                         (if (if #{tmp\ 3119}#
                                               (@apply
                                                 (lambda (#{docstring\ 3123}#
                                                          #{e1\ 3124}#
                                                          #{e2\ 3125}#)
                                                   (string?
                                                     (syntax->datum
                                                       #{docstring\ 3123}#)))
                                                 #{tmp\ 3119}#)
                                               #f)
                                           (@apply
                                             (lambda (#{docstring\ 3129}#
                                                      #{e1\ 3130}#
                                                      #{e2\ 3131}#)
                                               (#{lp\ 3114}#
                                                 (cons #{e1\ 3130}#
                                                       #{e2\ 3131}#)
                                                 (append
                                                   #{meta\ 3116}#
                                                   (list (cons 'documentation
                                                               (syntax->datum
                                                                 #{docstring\ 3129}#))))))
                                             #{tmp\ 3119}#)
                                           (let ((#{tmp\ 3134}#
                                                   ($sc-dispatch
                                                     #{tmp\ 3118}#
                                                     '(#(vector
                                                         #(each (any . any)))
                                                       any
                                                       .
                                                       each-any))))
                                             (if #{tmp\ 3134}#
                                               (@apply
                                                 (lambda (#{k\ 3139}#
                                                          #{v\ 3140}#
                                                          #{e1\ 3141}#
                                                          #{e2\ 3142}#)
                                                   (#{lp\ 3114}#
                                                     (cons #{e1\ 3141}#
                                                           #{e2\ 3142}#)
                                                     (append
                                                       #{meta\ 3116}#
                                                       (syntax->datum
                                                         (map cons
                                                              #{k\ 3139}#
                                                              #{v\ 3140}#)))))
                                                 #{tmp\ 3134}#)
                                               (let ((#{_\ 3147}#
                                                       #{tmp\ 3118}#))
                                                 (#{chi-simple-lambda\ 493}#
                                                   #{e\ 3083}#
                                                   #{r\ 3084}#
                                                   #{w\ 3085}#
                                                   #{s\ 3086}#
                                                   #{mod\ 3087}#
                                                   #{req\ 3103}#
                                                   #{rest\ 3105}#
                                                   #{meta\ 3116}#
                                                   #{body\ 3115}#))))))))))
                                (begin
                                  (#{lp\ 3114}#
                                    (cons #{e1\ 3101}# #{e2\ 3102}#)
                                    '()))))))
                        #{tmp\ 3094}#)
                      (let ((#{_\ 3149}# #{tmp\ 3093}#))
                        (syntax-violation
                          'lambda
                          "bad lambda"
                          #{e\ 3083}#)))))))
            (#{global-extend\ 384}#
              'core
              'lambda*
              (lambda (#{e\ 3150}#
                       #{r\ 3151}#
                       #{w\ 3152}#
                       #{s\ 3153}#
                       #{mod\ 3154}#)
                (let ((#{tmp\ 3160}# #{e\ 3150}#))
                  (let ((#{tmp\ 3161}#
                          ($sc-dispatch
                            #{tmp\ 3160}#
                            '(any any any . each-any))))
                    (if #{tmp\ 3161}#
                      (@apply
                        (lambda (#{_\ 3166}#
                                 #{args\ 3167}#
                                 #{e1\ 3168}#
                                 #{e2\ 3169}#)
                          (call-with-values
                            (lambda ()
                              (#{chi-lambda-case\ 497}#
                                #{e\ 3150}#
                                #{r\ 3151}#
                                #{w\ 3152}#
                                #{s\ 3153}#
                                #{mod\ 3154}#
                                #{lambda*-formals\ 495}#
                                (list (cons #{args\ 3167}#
                                            (cons #{e1\ 3168}#
                                                  #{e2\ 3169}#)))))
                            (lambda (#{meta\ 3171}# #{lcase\ 3172}#)
                              (#{build-case-lambda\ 332}#
                                #{s\ 3153}#
                                #{meta\ 3171}#
                                #{lcase\ 3172}#))))
                        #{tmp\ 3161}#)
                      (let ((#{_\ 3176}# #{tmp\ 3160}#))
                        (syntax-violation
                          'lambda
                          "bad lambda*"
                          #{e\ 3150}#)))))))
            (#{global-extend\ 384}#
              'core
              'case-lambda
              (lambda (#{e\ 3177}#
                       #{r\ 3178}#
                       #{w\ 3179}#
                       #{s\ 3180}#
                       #{mod\ 3181}#)
                (let ((#{tmp\ 3187}# #{e\ 3177}#))
                  (let ((#{tmp\ 3188}#
                          ($sc-dispatch
                            #{tmp\ 3187}#
                            '(any (any any . each-any)
                                  .
                                  #(each (any any . each-any))))))
                    (if #{tmp\ 3188}#
                      (@apply
                        (lambda (#{_\ 3196}#
                                 #{args\ 3197}#
                                 #{e1\ 3198}#
                                 #{e2\ 3199}#
                                 #{args*\ 3200}#
                                 #{e1*\ 3201}#
                                 #{e2*\ 3202}#)
                          (call-with-values
                            (lambda ()
                              (#{chi-lambda-case\ 497}#
                                #{e\ 3177}#
                                #{r\ 3178}#
                                #{w\ 3179}#
                                #{s\ 3180}#
                                #{mod\ 3181}#
                                #{lambda-formals\ 491}#
                                (cons (cons #{args\ 3197}#
                                            (cons #{e1\ 3198}# #{e2\ 3199}#))
                                      (map (lambda (#{tmp\ 3206}#
                                                    #{tmp\ 3205}#
                                                    #{tmp\ 3204}#)
                                             (cons #{tmp\ 3204}#
                                                   (cons #{tmp\ 3205}#
                                                         #{tmp\ 3206}#)))
                                           #{e2*\ 3202}#
                                           #{e1*\ 3201}#
                                           #{args*\ 3200}#))))
                            (lambda (#{meta\ 3208}# #{lcase\ 3209}#)
                              (#{build-case-lambda\ 332}#
                                #{s\ 3180}#
                                #{meta\ 3208}#
                                #{lcase\ 3209}#))))
                        #{tmp\ 3188}#)
                      (let ((#{_\ 3213}# #{tmp\ 3187}#))
                        (syntax-violation
                          'case-lambda
                          "bad case-lambda"
                          #{e\ 3177}#)))))))
            (#{global-extend\ 384}#
              'core
              'case-lambda*
              (lambda (#{e\ 3214}#
                       #{r\ 3215}#
                       #{w\ 3216}#
                       #{s\ 3217}#
                       #{mod\ 3218}#)
                (let ((#{tmp\ 3224}# #{e\ 3214}#))
                  (let ((#{tmp\ 3225}#
                          ($sc-dispatch
                            #{tmp\ 3224}#
                            '(any (any any . each-any)
                                  .
                                  #(each (any any . each-any))))))
                    (if #{tmp\ 3225}#
                      (@apply
                        (lambda (#{_\ 3233}#
                                 #{args\ 3234}#
                                 #{e1\ 3235}#
                                 #{e2\ 3236}#
                                 #{args*\ 3237}#
                                 #{e1*\ 3238}#
                                 #{e2*\ 3239}#)
                          (call-with-values
                            (lambda ()
                              (#{chi-lambda-case\ 497}#
                                #{e\ 3214}#
                                #{r\ 3215}#
                                #{w\ 3216}#
                                #{s\ 3217}#
                                #{mod\ 3218}#
                                #{lambda*-formals\ 495}#
                                (cons (cons #{args\ 3234}#
                                            (cons #{e1\ 3235}# #{e2\ 3236}#))
                                      (map (lambda (#{tmp\ 3243}#
                                                    #{tmp\ 3242}#
                                                    #{tmp\ 3241}#)
                                             (cons #{tmp\ 3241}#
                                                   (cons #{tmp\ 3242}#
                                                         #{tmp\ 3243}#)))
                                           #{e2*\ 3239}#
                                           #{e1*\ 3238}#
                                           #{args*\ 3237}#))))
                            (lambda (#{meta\ 3245}# #{lcase\ 3246}#)
                              (#{build-case-lambda\ 332}#
                                #{s\ 3217}#
                                #{meta\ 3245}#
                                #{lcase\ 3246}#))))
                        #{tmp\ 3225}#)
                      (let ((#{_\ 3250}# #{tmp\ 3224}#))
                        (syntax-violation
                          'case-lambda
                          "bad case-lambda*"
                          #{e\ 3214}#)))))))
            (#{global-extend\ 384}#
              'core
              'let
              (letrec*
                ((#{chi-let\ 3252}#
                   (lambda (#{e\ 3253}#
                            #{r\ 3254}#
                            #{w\ 3255}#
                            #{s\ 3256}#
                            #{mod\ 3257}#
                            #{constructor\ 3258}#
                            #{ids\ 3259}#
                            #{vals\ 3260}#
                            #{exps\ 3261}#)
                     (if (not (#{valid-bound-ids?\ 451}# #{ids\ 3259}#))
                       (syntax-violation
                         'let
                         "duplicate bound variable"
                         #{e\ 3253}#)
                       (begin
                         (let ((#{labels\ 3273}#
                                 (#{gen-labels\ 406}# #{ids\ 3259}#))
                               (#{new-vars\ 3274}#
                                 (map #{gen-var\ 501}# #{ids\ 3259}#)))
                           (begin
                             (let ((#{nw\ 3277}#
                                     (#{make-binding-wrap\ 435}#
                                       #{ids\ 3259}#
                                       #{labels\ 3273}#
                                       #{w\ 3255}#))
                                   (#{nr\ 3278}#
                                     (#{extend-var-env\ 378}#
                                       #{labels\ 3273}#
                                       #{new-vars\ 3274}#
                                       #{r\ 3254}#)))
                               (#{constructor\ 3258}#
                                 #{s\ 3256}#
                                 (map syntax->datum #{ids\ 3259}#)
                                 #{new-vars\ 3274}#
                                 (map (lambda (#{x\ 3279}#)
                                        (#{chi\ 473}#
                                          #{x\ 3279}#
                                          #{r\ 3254}#
                                          #{w\ 3255}#
                                          #{mod\ 3257}#))
                                      #{vals\ 3260}#)
                                 (#{chi-body\ 481}#
                                   #{exps\ 3261}#
                                   (#{source-wrap\ 459}#
                                     #{e\ 3253}#
                                     #{nw\ 3277}#
                                     #{s\ 3256}#
                                     #{mod\ 3257}#)
                                   #{nr\ 3278}#
                                   #{nw\ 3277}#
                                   #{mod\ 3257}#))))))))))
                (begin
                  (lambda (#{e\ 3281}#
                           #{r\ 3282}#
                           #{w\ 3283}#
                           #{s\ 3284}#
                           #{mod\ 3285}#)
                    (let ((#{tmp\ 3291}# #{e\ 3281}#))
                      (let ((#{tmp\ 3292}#
                              ($sc-dispatch
                                #{tmp\ 3291}#
                                '(any #(each (any any)) any . each-any))))
                        (if (if #{tmp\ 3292}#
                              (@apply
                                (lambda (#{_\ 3298}#
                                         #{id\ 3299}#
                                         #{val\ 3300}#
                                         #{e1\ 3301}#
                                         #{e2\ 3302}#)
                                  (and-map #{id?\ 388}# #{id\ 3299}#))
                                #{tmp\ 3292}#)
                              #f)
                          (@apply
                            (lambda (#{_\ 3309}#
                                     #{id\ 3310}#
                                     #{val\ 3311}#
                                     #{e1\ 3312}#
                                     #{e2\ 3313}#)
                              (#{chi-let\ 3252}#
                                #{e\ 3281}#
                                #{r\ 3282}#
                                #{w\ 3283}#
                                #{s\ 3284}#
                                #{mod\ 3285}#
                                #{build-let\ 342}#
                                #{id\ 3310}#
                                #{val\ 3311}#
                                (cons #{e1\ 3312}# #{e2\ 3313}#)))
                            #{tmp\ 3292}#)
                          (let ((#{tmp\ 3317}#
                                  ($sc-dispatch
                                    #{tmp\ 3291}#
                                    '(any any
                                          #(each (any any))
                                          any
                                          .
                                          each-any))))
                            (if (if #{tmp\ 3317}#
                                  (@apply
                                    (lambda (#{_\ 3324}#
                                             #{f\ 3325}#
                                             #{id\ 3326}#
                                             #{val\ 3327}#
                                             #{e1\ 3328}#
                                             #{e2\ 3329}#)
                                      (if (#{id?\ 388}# #{f\ 3325}#)
                                        (and-map #{id?\ 388}# #{id\ 3326}#)
                                        #f))
                                    #{tmp\ 3317}#)
                                  #f)
                              (@apply
                                (lambda (#{_\ 3339}#
                                         #{f\ 3340}#
                                         #{id\ 3341}#
                                         #{val\ 3342}#
                                         #{e1\ 3343}#
                                         #{e2\ 3344}#)
                                  (#{chi-let\ 3252}#
                                    #{e\ 3281}#
                                    #{r\ 3282}#
                                    #{w\ 3283}#
                                    #{s\ 3284}#
                                    #{mod\ 3285}#
                                    #{build-named-let\ 344}#
                                    (cons #{f\ 3340}# #{id\ 3341}#)
                                    #{val\ 3342}#
                                    (cons #{e1\ 3343}# #{e2\ 3344}#)))
                                #{tmp\ 3317}#)
                              (let ((#{_\ 3349}# #{tmp\ 3291}#))
                                (syntax-violation
                                  'let
                                  "bad let"
                                  (#{source-wrap\ 459}#
                                    #{e\ 3281}#
                                    #{w\ 3283}#
                                    #{s\ 3284}#
                                    #{mod\ 3285}#))))))))))))
            (#{global-extend\ 384}#
              'core
              'letrec
              (lambda (#{e\ 3350}#
                       #{r\ 3351}#
                       #{w\ 3352}#
                       #{s\ 3353}#
                       #{mod\ 3354}#)
                (let ((#{tmp\ 3360}# #{e\ 3350}#))
                  (let ((#{tmp\ 3361}#
                          ($sc-dispatch
                            #{tmp\ 3360}#
                            '(any #(each (any any)) any . each-any))))
                    (if (if #{tmp\ 3361}#
                          (@apply
                            (lambda (#{_\ 3367}#
                                     #{id\ 3368}#
                                     #{val\ 3369}#
                                     #{e1\ 3370}#
                                     #{e2\ 3371}#)
                              (and-map #{id?\ 388}# #{id\ 3368}#))
                            #{tmp\ 3361}#)
                          #f)
                      (@apply
                        (lambda (#{_\ 3378}#
                                 #{id\ 3379}#
                                 #{val\ 3380}#
                                 #{e1\ 3381}#
                                 #{e2\ 3382}#)
                          (begin
                            (let ((#{ids\ 3384}# #{id\ 3379}#))
                              (if (not (#{valid-bound-ids?\ 451}#
                                         #{ids\ 3384}#))
                                (syntax-violation
                                  'letrec
                                  "duplicate bound variable"
                                  #{e\ 3350}#)
                                (begin
                                  (let ((#{labels\ 3388}#
                                          (#{gen-labels\ 406}# #{ids\ 3384}#))
                                        (#{new-vars\ 3389}#
                                          (map #{gen-var\ 501}#
                                               #{ids\ 3384}#)))
                                    (begin
                                      (let ((#{w\ 3392}#
                                              (#{make-binding-wrap\ 435}#
                                                #{ids\ 3384}#
                                                #{labels\ 3388}#
                                                #{w\ 3352}#))
                                            (#{r\ 3393}#
                                              (#{extend-var-env\ 378}#
                                                #{labels\ 3388}#
                                                #{new-vars\ 3389}#
                                                #{r\ 3351}#)))
                                        (#{build-letrec\ 346}#
                                          #{s\ 3353}#
                                          #f
                                          (map syntax->datum #{ids\ 3384}#)
                                          #{new-vars\ 3389}#
                                          (map (lambda (#{x\ 3394}#)
                                                 (#{chi\ 473}#
                                                   #{x\ 3394}#
                                                   #{r\ 3393}#
                                                   #{w\ 3392}#
                                                   #{mod\ 3354}#))
                                               #{val\ 3380}#)
                                          (#{chi-body\ 481}#
                                            (cons #{e1\ 3381}# #{e2\ 3382}#)
                                            (#{source-wrap\ 459}#
                                              #{e\ 3350}#
                                              #{w\ 3392}#
                                              #{s\ 3353}#
                                              #{mod\ 3354}#)
                                            #{r\ 3393}#
                                            #{w\ 3392}#
                                            #{mod\ 3354}#))))))))))
                        #{tmp\ 3361}#)
                      (let ((#{_\ 3399}# #{tmp\ 3360}#))
                        (syntax-violation
                          'letrec
                          "bad letrec"
                          (#{source-wrap\ 459}#
                            #{e\ 3350}#
                            #{w\ 3352}#
                            #{s\ 3353}#
                            #{mod\ 3354}#))))))))
            (#{global-extend\ 384}#
              'core
              'letrec*
              (lambda (#{e\ 3400}#
                       #{r\ 3401}#
                       #{w\ 3402}#
                       #{s\ 3403}#
                       #{mod\ 3404}#)
                (let ((#{tmp\ 3410}# #{e\ 3400}#))
                  (let ((#{tmp\ 3411}#
                          ($sc-dispatch
                            #{tmp\ 3410}#
                            '(any #(each (any any)) any . each-any))))
                    (if (if #{tmp\ 3411}#
                          (@apply
                            (lambda (#{_\ 3417}#
                                     #{id\ 3418}#
                                     #{val\ 3419}#
                                     #{e1\ 3420}#
                                     #{e2\ 3421}#)
                              (and-map #{id?\ 388}# #{id\ 3418}#))
                            #{tmp\ 3411}#)
                          #f)
                      (@apply
                        (lambda (#{_\ 3428}#
                                 #{id\ 3429}#
                                 #{val\ 3430}#
                                 #{e1\ 3431}#
                                 #{e2\ 3432}#)
                          (begin
                            (let ((#{ids\ 3434}# #{id\ 3429}#))
                              (if (not (#{valid-bound-ids?\ 451}#
                                         #{ids\ 3434}#))
                                (syntax-violation
                                  'letrec*
                                  "duplicate bound variable"
                                  #{e\ 3400}#)
                                (begin
                                  (let ((#{labels\ 3438}#
                                          (#{gen-labels\ 406}# #{ids\ 3434}#))
                                        (#{new-vars\ 3439}#
                                          (map #{gen-var\ 501}#
                                               #{ids\ 3434}#)))
                                    (begin
                                      (let ((#{w\ 3442}#
                                              (#{make-binding-wrap\ 435}#
                                                #{ids\ 3434}#
                                                #{labels\ 3438}#
                                                #{w\ 3402}#))
                                            (#{r\ 3443}#
                                              (#{extend-var-env\ 378}#
                                                #{labels\ 3438}#
                                                #{new-vars\ 3439}#
                                                #{r\ 3401}#)))
                                        (#{build-letrec\ 346}#
                                          #{s\ 3403}#
                                          #t
                                          (map syntax->datum #{ids\ 3434}#)
                                          #{new-vars\ 3439}#
                                          (map (lambda (#{x\ 3444}#)
                                                 (#{chi\ 473}#
                                                   #{x\ 3444}#
                                                   #{r\ 3443}#
                                                   #{w\ 3442}#
                                                   #{mod\ 3404}#))
                                               #{val\ 3430}#)
                                          (#{chi-body\ 481}#
                                            (cons #{e1\ 3431}# #{e2\ 3432}#)
                                            (#{source-wrap\ 459}#
                                              #{e\ 3400}#
                                              #{w\ 3442}#
                                              #{s\ 3403}#
                                              #{mod\ 3404}#)
                                            #{r\ 3443}#
                                            #{w\ 3442}#
                                            #{mod\ 3404}#))))))))))
                        #{tmp\ 3411}#)
                      (let ((#{_\ 3449}# #{tmp\ 3410}#))
                        (syntax-violation
                          'letrec*
                          "bad letrec*"
                          (#{source-wrap\ 459}#
                            #{e\ 3400}#
                            #{w\ 3402}#
                            #{s\ 3403}#
                            #{mod\ 3404}#))))))))
            (#{global-extend\ 384}#
              'core
              'set!
              (lambda (#{e\ 3450}#
                       #{r\ 3451}#
                       #{w\ 3452}#
                       #{s\ 3453}#
                       #{mod\ 3454}#)
                (let ((#{tmp\ 3460}# #{e\ 3450}#))
                  (let ((#{tmp\ 3461}#
                          ($sc-dispatch
                            #{tmp\ 3460}#
                            '(any any any))))
                    (if (if #{tmp\ 3461}#
                          (@apply
                            (lambda (#{_\ 3465}# #{id\ 3466}# #{val\ 3467}#)
                              (#{id?\ 388}# #{id\ 3466}#))
                            #{tmp\ 3461}#)
                          #f)
                      (@apply
                        (lambda (#{_\ 3471}# #{id\ 3472}# #{val\ 3473}#)
                          (begin
                            (let ((#{n\ 3475}#
                                    (#{id-var-name\ 445}#
                                      #{id\ 3472}#
                                      #{w\ 3452}#)))
                              (begin
                                (let ((#{b\ 3477}#
                                        (#{lookup\ 382}#
                                          #{n\ 3475}#
                                          #{r\ 3451}#
                                          #{mod\ 3454}#)))
                                  (begin
                                    (let ((#{atom-key\ 3480}#
                                            (#{binding-type\ 371}#
                                              #{b\ 3477}#)))
                                      (if (eqv? #{atom-key\ 3480}#
                                                'lexical)
                                        (#{build-lexical-assignment\ 320}#
                                          #{s\ 3453}#
                                          (syntax->datum #{id\ 3472}#)
                                          (#{binding-value\ 373}# #{b\ 3477}#)
                                          (#{chi\ 473}#
                                            #{val\ 3473}#
                                            #{r\ 3451}#
                                            #{w\ 3452}#
                                            #{mod\ 3454}#))
                                        (if (eqv? #{atom-key\ 3480}#
                                                  'global)
                                          (#{build-global-assignment\ 326}#
                                            #{s\ 3453}#
                                            #{n\ 3475}#
                                            (#{chi\ 473}#
                                              #{val\ 3473}#
                                              #{r\ 3451}#
                                              #{w\ 3452}#
                                              #{mod\ 3454}#)
                                            #{mod\ 3454}#)
                                          (if (eqv? #{atom-key\ 3480}#
                                                    'macro)
                                            (begin
                                              (let ((#{p\ 3485}#
                                                      (#{binding-value\ 373}#
                                                        #{b\ 3477}#)))
                                                (if (procedure-property
                                                      #{p\ 3485}#
                                                      'variable-transformer)
                                                  (#{chi\ 473}#
                                                    (#{chi-macro\ 479}#
                                                      #{p\ 3485}#
                                                      #{e\ 3450}#
                                                      #{r\ 3451}#
                                                      #{w\ 3452}#
                                                      #{s\ 3453}#
                                                      #f
                                                      #{mod\ 3454}#)
                                                    #{r\ 3451}#
                                                    #{w\ 3452}#
                                                    #{mod\ 3454}#)
                                                  (syntax-violation
                                                    'set!
                                                    "not a variable transformer"
                                                    (#{wrap\ 457}#
                                                      #{e\ 3450}#
                                                      #{w\ 3452}#
                                                      #{mod\ 3454}#)
                                                    (#{wrap\ 457}#
                                                      #{id\ 3472}#
                                                      #{w\ 3452}#
                                                      #{mod\ 3454}#)))))
                                            (if (eqv? #{atom-key\ 3480}#
                                                      'displaced-lexical)
                                              (syntax-violation
                                                'set!
                                                "identifier out of context"
                                                (#{wrap\ 457}#
                                                  #{id\ 3472}#
                                                  #{w\ 3452}#
                                                  #{mod\ 3454}#))
                                              (syntax-violation
                                                'set!
                                                "bad set!"
                                                (#{source-wrap\ 459}#
                                                  #{e\ 3450}#
                                                  #{w\ 3452}#
                                                  #{s\ 3453}#
                                                  #{mod\ 3454}#)))))))))))))
                        #{tmp\ 3461}#)
                      (let ((#{tmp\ 3488}#
                              ($sc-dispatch
                                #{tmp\ 3460}#
                                '(any (any . each-any) any))))
                        (if #{tmp\ 3488}#
                          (@apply
                            (lambda (#{_\ 3493}#
                                     #{head\ 3494}#
                                     #{tail\ 3495}#
                                     #{val\ 3496}#)
                              (call-with-values
                                (lambda ()
                                  (#{syntax-type\ 469}#
                                    #{head\ 3494}#
                                    #{r\ 3451}#
                                    '(())
                                    #f
                                    #f
                                    #{mod\ 3454}#
                                    #t))
                                (lambda (#{type\ 3499}#
                                         #{value\ 3500}#
                                         #{ee\ 3501}#
                                         #{ww\ 3502}#
                                         #{ss\ 3503}#
                                         #{modmod\ 3504}#)
                                  (if (eqv? #{type\ 3499}# (quote module-ref))
                                    (begin
                                      (let ((#{val\ 3513}#
                                              (#{chi\ 473}#
                                                #{val\ 3496}#
                                                #{r\ 3451}#
                                                #{w\ 3452}#
                                                #{mod\ 3454}#)))
                                        (call-with-values
                                          (lambda ()
                                            (#{value\ 3500}#
                                              (cons #{head\ 3494}#
                                                    #{tail\ 3495}#)
                                              #{r\ 3451}#
                                              #{w\ 3452}#))
                                          (lambda (#{e\ 3515}#
                                                   #{r\ 3516}#
                                                   #{w\ 3517}#
                                                   #{s*\ 3518}#
                                                   #{mod\ 3519}#)
                                            (let ((#{tmp\ 3525}# #{e\ 3515}#))
                                              (let ((#{tmp\ 3526}#
                                                      (list #{tmp\ 3525}#)))
                                                (if (if #{tmp\ 3526}#
                                                      (@apply
                                                        (lambda (#{e\ 3528}#)
                                                          (#{id?\ 388}#
                                                            #{e\ 3528}#))
                                                        #{tmp\ 3526}#)
                                                      #f)
                                                  (@apply
                                                    (lambda (#{e\ 3530}#)
                                                      (#{build-global-assignment\ 326}#
                                                        #{s\ 3453}#
                                                        (syntax->datum
                                                          #{e\ 3530}#)
                                                        #{val\ 3513}#
                                                        #{mod\ 3519}#))
                                                    #{tmp\ 3526}#)
                                                  (syntax-violation
                                                    #f
                                                    "source expression failed to match any pattern"
                                                    #{tmp\ 3525}#))))))))
                                    (#{build-application\ 312}#
                                      #{s\ 3453}#
                                      (#{chi\ 473}#
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
                                                    #("i3505"
                                                      "i3506"
                                                      "i3507"
                                                      "i3508"
                                                      "i3509"
                                                      "i3510"))
                                                  #(ribcage
                                                    #(_ head tail val)
                                                    #((top) (top) (top) (top))
                                                    #("i3489"
                                                      "i3490"
                                                      "i3491"
                                                      "i3492"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(e r w s mod)
                                                    #((top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top))
                                                    #("i3455"
                                                      "i3456"
                                                      "i3457"
                                                      "i3458"
                                                      "i3459"))
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
                                                    ("i502"
                                                     "i500"
                                                     "i498"
                                                     "i496"
                                                     "i494"
                                                     "i492"
                                                     "i490"
                                                     "i488"
                                                     "i486"
                                                     "i484"
                                                     "i482"
                                                     "i480"
                                                     "i478"
                                                     "i476"
                                                     "i474"
                                                     "i472"
                                                     "i470"
                                                     "i468"
                                                     "i466"
                                                     "i464"
                                                     "i462"
                                                     "i460"
                                                     "i458"
                                                     "i456"
                                                     "i454"
                                                     "i452"
                                                     "i450"
                                                     "i448"
                                                     "i446"
                                                     "i444"
                                                     "i442"
                                                     "i440"
                                                     "i438"
                                                     "i436"
                                                     "i434"
                                                     "i432"
                                                     "i431"
                                                     "i430"
                                                     "i428"
                                                     "i427"
                                                     "i426"
                                                     "i425"
                                                     "i424"
                                                     "i422"
                                                     "i420"
                                                     "i418"
                                                     "i416"
                                                     "i414"
                                                     "i412"
                                                     "i410"
                                                     "i408"
                                                     "i405"
                                                     "i403"
                                                     "i402"
                                                     "i401"
                                                     "i400"
                                                     "i399"
                                                     "i398"
                                                     "i396"
                                                     "i394"
                                                     "i392"
                                                     "i390"
                                                     "i389"
                                                     "i387"
                                                     "i385"
                                                     "i383"
                                                     "i381"
                                                     "i379"
                                                     "i377"
                                                     "i375"
                                                     "i374"
                                                     "i372"
                                                     "i370"
                                                     "i369"
                                                     "i368"
                                                     "i366"
                                                     "i365"
                                                     "i363"
                                                     "i361"
                                                     "i359"
                                                     "i357"
                                                     "i355"
                                                     "i353"
                                                     "i351"
                                                     "i349"
                                                     "i347"
                                                     "i345"
                                                     "i343"
                                                     "i341"
                                                     "i339"
                                                     "i337"
                                                     "i335"
                                                     "i333"
                                                     "i331"
                                                     "i329"
                                                     "i327"
                                                     "i325"
                                                     "i323"
                                                     "i321"
                                                     "i319"
                                                     "i317"
                                                     "i315"
                                                     "i313"
                                                     "i311"
                                                     "i309"
                                                     "i307"
                                                     "i305"
                                                     "i303"
                                                     "i301"
                                                     "i300"
                                                     "i298"
                                                     "i296"
                                                     "i294"
                                                     "i292"
                                                     "i290"
                                                     "i288"
                                                     "i286"
                                                     "i284"
                                                     "i282"
                                                     "i279"
                                                     "i277"
                                                     "i275"
                                                     "i273"
                                                     "i271"
                                                     "i269"
                                                     "i267"
                                                     "i265"
                                                     "i263"
                                                     "i261"
                                                     "i259"
                                                     "i257"
                                                     "i255"
                                                     "i253"
                                                     "i251"
                                                     "i249"
                                                     "i247"
                                                     "i245"))
                                                  #(ribcage
                                                    (define-structure
                                                      define-expansion-accessors
                                                      define-expansion-constructors
                                                      and-map*)
                                                    ((top) (top) (top) (top))
                                                    ("i39" "i38" "i37" "i35")))
                                                 (hygiene guile))
                                              #{head\ 3494}#)
                                        #{r\ 3451}#
                                        #{w\ 3452}#
                                        #{mod\ 3454}#)
                                      (map (lambda (#{e\ 3532}#)
                                             (#{chi\ 473}#
                                               #{e\ 3532}#
                                               #{r\ 3451}#
                                               #{w\ 3452}#
                                               #{mod\ 3454}#))
                                           (append
                                             #{tail\ 3495}#
                                             (list #{val\ 3496}#))))))))
                            #{tmp\ 3488}#)
                          (let ((#{_\ 3536}# #{tmp\ 3460}#))
                            (syntax-violation
                              'set!
                              "bad set!"
                              (#{source-wrap\ 459}#
                                #{e\ 3450}#
                                #{w\ 3452}#
                                #{s\ 3453}#
                                #{mod\ 3454}#))))))))))
            (#{global-extend\ 384}#
              'module-ref
              '@
              (lambda (#{e\ 3537}# #{r\ 3538}# #{w\ 3539}#)
                (let ((#{tmp\ 3543}# #{e\ 3537}#))
                  (let ((#{tmp\ 3544}#
                          ($sc-dispatch
                            #{tmp\ 3543}#
                            '(any each-any any))))
                    (if (if #{tmp\ 3544}#
                          (@apply
                            (lambda (#{_\ 3548}# #{mod\ 3549}# #{id\ 3550}#)
                              (if (and-map #{id?\ 388}# #{mod\ 3549}#)
                                (#{id?\ 388}# #{id\ 3550}#)
                                #f))
                            #{tmp\ 3544}#)
                          #f)
                      (@apply
                        (lambda (#{_\ 3557}# #{mod\ 3558}# #{id\ 3559}#)
                          (values
                            (syntax->datum #{id\ 3559}#)
                            #{r\ 3538}#
                            #{w\ 3539}#
                            #f
                            (syntax->datum
                              (cons '#(syntax-object
                                       public
                                       ((top)
                                        #(ribcage
                                          #(_ mod id)
                                          #((top) (top) (top))
                                          #("i3554" "i3555" "i3556"))
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(e r w)
                                          #((top) (top) (top))
                                          #("i3540" "i3541" "i3542"))
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
                                          ("i502"
                                           "i500"
                                           "i498"
                                           "i496"
                                           "i494"
                                           "i492"
                                           "i490"
                                           "i488"
                                           "i486"
                                           "i484"
                                           "i482"
                                           "i480"
                                           "i478"
                                           "i476"
                                           "i474"
                                           "i472"
                                           "i470"
                                           "i468"
                                           "i466"
                                           "i464"
                                           "i462"
                                           "i460"
                                           "i458"
                                           "i456"
                                           "i454"
                                           "i452"
                                           "i450"
                                           "i448"
                                           "i446"
                                           "i444"
                                           "i442"
                                           "i440"
                                           "i438"
                                           "i436"
                                           "i434"
                                           "i432"
                                           "i431"
                                           "i430"
                                           "i428"
                                           "i427"
                                           "i426"
                                           "i425"
                                           "i424"
                                           "i422"
                                           "i420"
                                           "i418"
                                           "i416"
                                           "i414"
                                           "i412"
                                           "i410"
                                           "i408"
                                           "i405"
                                           "i403"
                                           "i402"
                                           "i401"
                                           "i400"
                                           "i399"
                                           "i398"
                                           "i396"
                                           "i394"
                                           "i392"
                                           "i390"
                                           "i389"
                                           "i387"
                                           "i385"
                                           "i383"
                                           "i381"
                                           "i379"
                                           "i377"
                                           "i375"
                                           "i374"
                                           "i372"
                                           "i370"
                                           "i369"
                                           "i368"
                                           "i366"
                                           "i365"
                                           "i363"
                                           "i361"
                                           "i359"
                                           "i357"
                                           "i355"
                                           "i353"
                                           "i351"
                                           "i349"
                                           "i347"
                                           "i345"
                                           "i343"
                                           "i341"
                                           "i339"
                                           "i337"
                                           "i335"
                                           "i333"
                                           "i331"
                                           "i329"
                                           "i327"
                                           "i325"
                                           "i323"
                                           "i321"
                                           "i319"
                                           "i317"
                                           "i315"
                                           "i313"
                                           "i311"
                                           "i309"
                                           "i307"
                                           "i305"
                                           "i303"
                                           "i301"
                                           "i300"
                                           "i298"
                                           "i296"
                                           "i294"
                                           "i292"
                                           "i290"
                                           "i288"
                                           "i286"
                                           "i284"
                                           "i282"
                                           "i279"
                                           "i277"
                                           "i275"
                                           "i273"
                                           "i271"
                                           "i269"
                                           "i267"
                                           "i265"
                                           "i263"
                                           "i261"
                                           "i259"
                                           "i257"
                                           "i255"
                                           "i253"
                                           "i251"
                                           "i249"
                                           "i247"
                                           "i245"))
                                        #(ribcage
                                          (define-structure
                                            define-expansion-accessors
                                            define-expansion-constructors
                                            and-map*)
                                          ((top) (top) (top) (top))
                                          ("i39" "i38" "i37" "i35")))
                                       (hygiene guile))
                                    #{mod\ 3558}#))))
                        #{tmp\ 3544}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp\ 3543}#))))))
            (#{global-extend\ 384}#
              'module-ref
              '@@
              (lambda (#{e\ 3561}# #{r\ 3562}# #{w\ 3563}#)
                (letrec*
                  ((#{remodulate\ 3568}#
                     (lambda (#{x\ 3569}# #{mod\ 3570}#)
                       (if (pair? #{x\ 3569}#)
                         (cons (#{remodulate\ 3568}#
                                 (car #{x\ 3569}#)
                                 #{mod\ 3570}#)
                               (#{remodulate\ 3568}#
                                 (cdr #{x\ 3569}#)
                                 #{mod\ 3570}#))
                         (if (#{syntax-object?\ 352}# #{x\ 3569}#)
                           (#{make-syntax-object\ 350}#
                             (#{remodulate\ 3568}#
                               (#{syntax-object-expression\ 354}# #{x\ 3569}#)
                               #{mod\ 3570}#)
                             (#{syntax-object-wrap\ 356}# #{x\ 3569}#)
                             #{mod\ 3570}#)
                           (if (vector? #{x\ 3569}#)
                             (begin
                               (let ((#{n\ 3581}# (vector-length #{x\ 3569}#)))
                                 (begin
                                   (let ((#{v\ 3583}#
                                           (make-vector #{n\ 3581}#)))
                                     (letrec*
                                       ((#{loop\ 3586}#
                                          (lambda (#{i\ 3587}#)
                                            (if (#{fx=\ 293}#
                                                  #{i\ 3587}#
                                                  #{n\ 3581}#)
                                              (begin (if #f #f) #{v\ 3583}#)
                                              (begin
                                                (vector-set!
                                                  #{v\ 3583}#
                                                  #{i\ 3587}#
                                                  (#{remodulate\ 3568}#
                                                    (vector-ref
                                                      #{x\ 3569}#
                                                      #{i\ 3587}#)
                                                    #{mod\ 3570}#))
                                                (#{loop\ 3586}#
                                                  (#{fx+\ 289}#
                                                    #{i\ 3587}#
                                                    1)))))))
                                       (begin (#{loop\ 3586}# 0)))))))
                             #{x\ 3569}#))))))
                  (begin
                    (let ((#{tmp\ 3591}# #{e\ 3561}#))
                      (let ((#{tmp\ 3592}#
                              ($sc-dispatch
                                #{tmp\ 3591}#
                                '(any each-any any))))
                        (if (if #{tmp\ 3592}#
                              (@apply
                                (lambda (#{_\ 3596}#
                                         #{mod\ 3597}#
                                         #{exp\ 3598}#)
                                  (and-map #{id?\ 388}# #{mod\ 3597}#))
                                #{tmp\ 3592}#)
                              #f)
                          (@apply
                            (lambda (#{_\ 3603}# #{mod\ 3604}# #{exp\ 3605}#)
                              (begin
                                (let ((#{mod\ 3607}#
                                        (syntax->datum
                                          (cons '#(syntax-object
                                                   private
                                                   ((top)
                                                    #(ribcage
                                                      #(_ mod exp)
                                                      #((top) (top) (top))
                                                      #("i3600"
                                                        "i3601"
                                                        "i3602"))
                                                    #(ribcage
                                                      (remodulate)
                                                      ((top))
                                                      ("i3567"))
                                                    #(ribcage
                                                      #(e r w)
                                                      #((top) (top) (top))
                                                      #("i3564"
                                                        "i3565"
                                                        "i3566"))
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
                                                      ("i502"
                                                       "i500"
                                                       "i498"
                                                       "i496"
                                                       "i494"
                                                       "i492"
                                                       "i490"
                                                       "i488"
                                                       "i486"
                                                       "i484"
                                                       "i482"
                                                       "i480"
                                                       "i478"
                                                       "i476"
                                                       "i474"
                                                       "i472"
                                                       "i470"
                                                       "i468"
                                                       "i466"
                                                       "i464"
                                                       "i462"
                                                       "i460"
                                                       "i458"
                                                       "i456"
                                                       "i454"
                                                       "i452"
                                                       "i450"
                                                       "i448"
                                                       "i446"
                                                       "i444"
                                                       "i442"
                                                       "i440"
                                                       "i438"
                                                       "i436"
                                                       "i434"
                                                       "i432"
                                                       "i431"
                                                       "i430"
                                                       "i428"
                                                       "i427"
                                                       "i426"
                                                       "i425"
                                                       "i424"
                                                       "i422"
                                                       "i420"
                                                       "i418"
                                                       "i416"
                                                       "i414"
                                                       "i412"
                                                       "i410"
                                                       "i408"
                                                       "i405"
                                                       "i403"
                                                       "i402"
                                                       "i401"
                                                       "i400"
                                                       "i399"
                                                       "i398"
                                                       "i396"
                                                       "i394"
                                                       "i392"
                                                       "i390"
                                                       "i389"
                                                       "i387"
                                                       "i385"
                                                       "i383"
                                                       "i381"
                                                       "i379"
                                                       "i377"
                                                       "i375"
                                                       "i374"
                                                       "i372"
                                                       "i370"
                                                       "i369"
                                                       "i368"
                                                       "i366"
                                                       "i365"
                                                       "i363"
                                                       "i361"
                                                       "i359"
                                                       "i357"
                                                       "i355"
                                                       "i353"
                                                       "i351"
                                                       "i349"
                                                       "i347"
                                                       "i345"
                                                       "i343"
                                                       "i341"
                                                       "i339"
                                                       "i337"
                                                       "i335"
                                                       "i333"
                                                       "i331"
                                                       "i329"
                                                       "i327"
                                                       "i325"
                                                       "i323"
                                                       "i321"
                                                       "i319"
                                                       "i317"
                                                       "i315"
                                                       "i313"
                                                       "i311"
                                                       "i309"
                                                       "i307"
                                                       "i305"
                                                       "i303"
                                                       "i301"
                                                       "i300"
                                                       "i298"
                                                       "i296"
                                                       "i294"
                                                       "i292"
                                                       "i290"
                                                       "i288"
                                                       "i286"
                                                       "i284"
                                                       "i282"
                                                       "i279"
                                                       "i277"
                                                       "i275"
                                                       "i273"
                                                       "i271"
                                                       "i269"
                                                       "i267"
                                                       "i265"
                                                       "i263"
                                                       "i261"
                                                       "i259"
                                                       "i257"
                                                       "i255"
                                                       "i253"
                                                       "i251"
                                                       "i249"
                                                       "i247"
                                                       "i245"))
                                                    #(ribcage
                                                      (define-structure
                                                        define-expansion-accessors
                                                        define-expansion-constructors
                                                        and-map*)
                                                      ((top) (top) (top) (top))
                                                      ("i39"
                                                       "i38"
                                                       "i37"
                                                       "i35")))
                                                   (hygiene guile))
                                                #{mod\ 3604}#))))
                                  (values
                                    (#{remodulate\ 3568}#
                                      #{exp\ 3605}#
                                      #{mod\ 3607}#)
                                    #{r\ 3562}#
                                    #{w\ 3563}#
                                    (#{source-annotation\ 367}# #{exp\ 3605}#)
                                    #{mod\ 3607}#))))
                            #{tmp\ 3592}#)
                          (syntax-violation
                            #f
                            "source expression failed to match any pattern"
                            #{tmp\ 3591}#))))))))
            (#{global-extend\ 384}#
              'core
              'if
              (lambda (#{e\ 3609}#
                       #{r\ 3610}#
                       #{w\ 3611}#
                       #{s\ 3612}#
                       #{mod\ 3613}#)
                (let ((#{tmp\ 3619}# #{e\ 3609}#))
                  (let ((#{tmp\ 3620}#
                          ($sc-dispatch
                            #{tmp\ 3619}#
                            '(any any any))))
                    (if #{tmp\ 3620}#
                      (@apply
                        (lambda (#{_\ 3624}# #{test\ 3625}# #{then\ 3626}#)
                          (#{build-conditional\ 314}#
                            #{s\ 3612}#
                            (#{chi\ 473}#
                              #{test\ 3625}#
                              #{r\ 3610}#
                              #{w\ 3611}#
                              #{mod\ 3613}#)
                            (#{chi\ 473}#
                              #{then\ 3626}#
                              #{r\ 3610}#
                              #{w\ 3611}#
                              #{mod\ 3613}#)
                            (#{build-void\ 310}# #f)))
                        #{tmp\ 3620}#)
                      (let ((#{tmp\ 3628}#
                              ($sc-dispatch
                                #{tmp\ 3619}#
                                '(any any any any))))
                        (if #{tmp\ 3628}#
                          (@apply
                            (lambda (#{_\ 3633}#
                                     #{test\ 3634}#
                                     #{then\ 3635}#
                                     #{else\ 3636}#)
                              (#{build-conditional\ 314}#
                                #{s\ 3612}#
                                (#{chi\ 473}#
                                  #{test\ 3634}#
                                  #{r\ 3610}#
                                  #{w\ 3611}#
                                  #{mod\ 3613}#)
                                (#{chi\ 473}#
                                  #{then\ 3635}#
                                  #{r\ 3610}#
                                  #{w\ 3611}#
                                  #{mod\ 3613}#)
                                (#{chi\ 473}#
                                  #{else\ 3636}#
                                  #{r\ 3610}#
                                  #{w\ 3611}#
                                  #{mod\ 3613}#)))
                            #{tmp\ 3628}#)
                          (syntax-violation
                            #f
                            "source expression failed to match any pattern"
                            #{tmp\ 3619}#))))))))
            (#{global-extend\ 384}#
              'core
              'with-fluids
              (lambda (#{e\ 3637}#
                       #{r\ 3638}#
                       #{w\ 3639}#
                       #{s\ 3640}#
                       #{mod\ 3641}#)
                (let ((#{tmp\ 3647}# #{e\ 3637}#))
                  (let ((#{tmp\ 3648}#
                          ($sc-dispatch
                            #{tmp\ 3647}#
                            '(any #(each (any any)) any . each-any))))
                    (if #{tmp\ 3648}#
                      (@apply
                        (lambda (#{_\ 3654}#
                                 #{fluid\ 3655}#
                                 #{val\ 3656}#
                                 #{b\ 3657}#
                                 #{b*\ 3658}#)
                          (#{build-dynlet\ 316}#
                            #{s\ 3640}#
                            (map (lambda (#{x\ 3659}#)
                                   (#{chi\ 473}#
                                     #{x\ 3659}#
                                     #{r\ 3638}#
                                     #{w\ 3639}#
                                     #{mod\ 3641}#))
                                 #{fluid\ 3655}#)
                            (map (lambda (#{x\ 3662}#)
                                   (#{chi\ 473}#
                                     #{x\ 3662}#
                                     #{r\ 3638}#
                                     #{w\ 3639}#
                                     #{mod\ 3641}#))
                                 #{val\ 3656}#)
                            (#{chi-body\ 481}#
                              (cons #{b\ 3657}# #{b*\ 3658}#)
                              (#{source-wrap\ 459}#
                                #{e\ 3637}#
                                #{w\ 3639}#
                                #{s\ 3640}#
                                #{mod\ 3641}#)
                              #{r\ 3638}#
                              #{w\ 3639}#
                              #{mod\ 3641}#)))
                        #{tmp\ 3648}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp\ 3647}#))))))
            (#{global-extend\ 384}#
              'begin
              'begin
              '())
            (#{global-extend\ 384}#
              'define
              'define
              '())
            (#{global-extend\ 384}#
              'define-syntax
              'define-syntax
              '())
            (#{global-extend\ 384}#
              'eval-when
              'eval-when
              '())
            (#{global-extend\ 384}#
              'core
              'syntax-case
              (letrec*
                ((#{convert-pattern\ 3667}#
                   (lambda (#{pattern\ 3674}# #{keys\ 3675}#)
                     (letrec*
                       ((#{cvt*\ 3679}#
                          (lambda (#{p*\ 3682}# #{n\ 3683}# #{ids\ 3684}#)
                            (if (null? #{p*\ 3682}#)
                              (values (quote ()) #{ids\ 3684}#)
                              (call-with-values
                                (lambda ()
                                  (#{cvt*\ 3679}#
                                    (cdr #{p*\ 3682}#)
                                    #{n\ 3683}#
                                    #{ids\ 3684}#))
                                (lambda (#{y\ 3688}# #{ids\ 3689}#)
                                  (call-with-values
                                    (lambda ()
                                      (#{cvt\ 3681}#
                                        (car #{p*\ 3682}#)
                                        #{n\ 3683}#
                                        #{ids\ 3689}#))
                                    (lambda (#{x\ 3692}# #{ids\ 3693}#)
                                      (values
                                        (cons #{x\ 3692}# #{y\ 3688}#)
                                        #{ids\ 3693}#))))))))
                        (#{cvt\ 3681}#
                          (lambda (#{p\ 3696}# #{n\ 3697}# #{ids\ 3698}#)
                            (if (#{id?\ 388}# #{p\ 3696}#)
                              (if (#{bound-id-member?\ 455}#
                                    #{p\ 3696}#
                                    #{keys\ 3675}#)
                                (values
                                  (vector (quote free-id) #{p\ 3696}#)
                                  #{ids\ 3698}#)
                                (values
                                  'any
                                  (cons (cons #{p\ 3696}# #{n\ 3697}#)
                                        #{ids\ 3698}#)))
                              (let ((#{tmp\ 3702}# #{p\ 3696}#))
                                (let ((#{tmp\ 3703}#
                                        ($sc-dispatch
                                          #{tmp\ 3702}#
                                          '(any any))))
                                  (if (if #{tmp\ 3703}#
                                        (@apply
                                          (lambda (#{x\ 3706}# #{dots\ 3707}#)
                                            (#{ellipsis?\ 489}#
                                              #{dots\ 3707}#))
                                          #{tmp\ 3703}#)
                                        #f)
                                    (@apply
                                      (lambda (#{x\ 3710}# #{dots\ 3711}#)
                                        (call-with-values
                                          (lambda ()
                                            (#{cvt\ 3681}#
                                              #{x\ 3710}#
                                              (#{fx+\ 289}# #{n\ 3697}# 1)
                                              #{ids\ 3698}#))
                                          (lambda (#{p\ 3712}# #{ids\ 3713}#)
                                            (values
                                              (if (eq? #{p\ 3712}# (quote any))
                                                'each-any
                                                (vector
                                                  'each
                                                  #{p\ 3712}#))
                                              #{ids\ 3713}#))))
                                      #{tmp\ 3703}#)
                                    (let ((#{tmp\ 3716}#
                                            ($sc-dispatch
                                              #{tmp\ 3702}#
                                              '(any any . each-any))))
                                      (if (if #{tmp\ 3716}#
                                            (@apply
                                              (lambda (#{x\ 3720}#
                                                       #{dots\ 3721}#
                                                       #{ys\ 3722}#)
                                                (#{ellipsis?\ 489}#
                                                  #{dots\ 3721}#))
                                              #{tmp\ 3716}#)
                                            #f)
                                        (@apply
                                          (lambda (#{x\ 3726}#
                                                   #{dots\ 3727}#
                                                   #{ys\ 3728}#)
                                            (call-with-values
                                              (lambda ()
                                                (#{cvt*\ 3679}#
                                                  #{ys\ 3728}#
                                                  #{n\ 3697}#
                                                  #{ids\ 3698}#))
                                              (lambda (#{ys\ 3730}#
                                                       #{ids\ 3731}#)
                                                (call-with-values
                                                  (lambda ()
                                                    (#{cvt\ 3681}#
                                                      #{x\ 3726}#
                                                      (1+ #{n\ 3697}#)
                                                      #{ids\ 3731}#))
                                                  (lambda (#{x\ 3734}#
                                                           #{ids\ 3735}#)
                                                    (values
                                                      (list->vector
                                                        (cons 'each+
                                                              (cons #{x\ 3734}#
                                                                    (cons (reverse
                                                                            #{ys\ 3730}#)
                                                                          '(())))))
                                                      #{ids\ 3735}#))))))
                                          #{tmp\ 3716}#)
                                        (let ((#{tmp\ 3739}#
                                                ($sc-dispatch
                                                  #{tmp\ 3702}#
                                                  '(any . any))))
                                          (if #{tmp\ 3739}#
                                            (@apply
                                              (lambda (#{x\ 3742}# #{y\ 3743}#)
                                                (call-with-values
                                                  (lambda ()
                                                    (#{cvt\ 3681}#
                                                      #{y\ 3743}#
                                                      #{n\ 3697}#
                                                      #{ids\ 3698}#))
                                                  (lambda (#{y\ 3744}#
                                                           #{ids\ 3745}#)
                                                    (call-with-values
                                                      (lambda ()
                                                        (#{cvt\ 3681}#
                                                          #{x\ 3742}#
                                                          #{n\ 3697}#
                                                          #{ids\ 3745}#))
                                                      (lambda (#{x\ 3748}#
                                                               #{ids\ 3749}#)
                                                        (values
                                                          (cons #{x\ 3748}#
                                                                #{y\ 3744}#)
                                                          #{ids\ 3749}#))))))
                                              #{tmp\ 3739}#)
                                            (let ((#{tmp\ 3752}#
                                                    ($sc-dispatch
                                                      #{tmp\ 3702}#
                                                      '())))
                                              (if #{tmp\ 3752}#
                                                (@apply
                                                  (lambda ()
                                                    (values
                                                      '()
                                                      #{ids\ 3698}#))
                                                  #{tmp\ 3752}#)
                                                (let ((#{tmp\ 3753}#
                                                        ($sc-dispatch
                                                          #{tmp\ 3702}#
                                                          '#(vector
                                                             each-any))))
                                                  (if #{tmp\ 3753}#
                                                    (@apply
                                                      (lambda (#{x\ 3755}#)
                                                        (call-with-values
                                                          (lambda ()
                                                            (#{cvt\ 3681}#
                                                              #{x\ 3755}#
                                                              #{n\ 3697}#
                                                              #{ids\ 3698}#))
                                                          (lambda (#{p\ 3757}#
                                                                   #{ids\ 3758}#)
                                                            (values
                                                              (vector
                                                                'vector
                                                                #{p\ 3757}#)
                                                              #{ids\ 3758}#))))
                                                      #{tmp\ 3753}#)
                                                    (let ((#{x\ 3762}#
                                                            #{tmp\ 3702}#))
                                                      (values
                                                        (vector
                                                          'atom
                                                          (#{strip\ 499}#
                                                            #{p\ 3696}#
                                                            '(())))
                                                        #{ids\ 3698}#)))))))))))))))))
                       (begin
                         (#{cvt\ 3681}# #{pattern\ 3674}# 0 (quote ()))))))
                 (#{build-dispatch-call\ 3669}#
                   (lambda (#{pvars\ 3764}#
                            #{exp\ 3765}#
                            #{y\ 3766}#
                            #{r\ 3767}#
                            #{mod\ 3768}#)
                     (begin
                       (map cdr #{pvars\ 3764}#)
                       (let ((#{ids\ 3776}# (map car #{pvars\ 3764}#)))
                         (begin
                           (let ((#{labels\ 3780}#
                                   (#{gen-labels\ 406}# #{ids\ 3776}#))
                                 (#{new-vars\ 3781}#
                                   (map #{gen-var\ 501}# #{ids\ 3776}#)))
                             (#{build-application\ 312}#
                               #f
                               (#{build-primref\ 336}# #f (quote apply))
                               (list (#{build-simple-lambda\ 330}#
                                       #f
                                       (map syntax->datum #{ids\ 3776}#)
                                       #f
                                       #{new-vars\ 3781}#
                                       '()
                                       (#{chi\ 473}#
                                         #{exp\ 3765}#
                                         (#{extend-env\ 376}#
                                           #{labels\ 3780}#
                                           (map (lambda (#{var\ 3785}#
                                                         #{level\ 3786}#)
                                                  (cons 'syntax
                                                        (cons #{var\ 3785}#
                                                              #{level\ 3786}#)))
                                                #{new-vars\ 3781}#
                                                (map cdr #{pvars\ 3764}#))
                                           #{r\ 3767}#)
                                         (#{make-binding-wrap\ 435}#
                                           #{ids\ 3776}#
                                           #{labels\ 3780}#
                                           '(()))
                                         #{mod\ 3768}#))
                                     #{y\ 3766}#))))))))
                 (#{gen-clause\ 3671}#
                   (lambda (#{x\ 3792}#
                            #{keys\ 3793}#
                            #{clauses\ 3794}#
                            #{r\ 3795}#
                            #{pat\ 3796}#
                            #{fender\ 3797}#
                            #{exp\ 3798}#
                            #{mod\ 3799}#)
                     (call-with-values
                       (lambda ()
                         (#{convert-pattern\ 3667}#
                           #{pat\ 3796}#
                           #{keys\ 3793}#))
                       (lambda (#{p\ 3808}# #{pvars\ 3809}#)
                         (if (not (#{distinct-bound-ids?\ 453}#
                                    (map car #{pvars\ 3809}#)))
                           (syntax-violation
                             'syntax-case
                             "duplicate pattern variable"
                             #{pat\ 3796}#)
                           (if (not (and-map
                                      (lambda (#{x\ 3816}#)
                                        (not (#{ellipsis?\ 489}#
                                               (car #{x\ 3816}#))))
                                      #{pvars\ 3809}#))
                             (syntax-violation
                               'syntax-case
                               "misplaced ellipsis"
                               #{pat\ 3796}#)
                             (begin
                               (let ((#{y\ 3820}#
                                       (#{gen-var\ 501}# (quote tmp))))
                                 (#{build-application\ 312}#
                                   #f
                                   (#{build-simple-lambda\ 330}#
                                     #f
                                     (list (quote tmp))
                                     #f
                                     (list #{y\ 3820}#)
                                     '()
                                     (begin
                                       (let ((#{y\ 3824}#
                                               (#{build-lexical-reference\ 318}#
                                                 'value
                                                 #f
                                                 'tmp
                                                 #{y\ 3820}#)))
                                         (#{build-conditional\ 314}#
                                           #f
                                           (let ((#{tmp\ 3827}#
                                                   #{fender\ 3797}#))
                                             (let ((#{tmp\ 3828}#
                                                     ($sc-dispatch
                                                       #{tmp\ 3827}#
                                                       '#(atom #t))))
                                               (if #{tmp\ 3828}#
                                                 (@apply
                                                   (lambda () #{y\ 3824}#)
                                                   #{tmp\ 3828}#)
                                                 (let ((#{_\ 3830}#
                                                         #{tmp\ 3827}#))
                                                   (#{build-conditional\ 314}#
                                                     #f
                                                     #{y\ 3824}#
                                                     (#{build-dispatch-call\ 3669}#
                                                       #{pvars\ 3809}#
                                                       #{fender\ 3797}#
                                                       #{y\ 3824}#
                                                       #{r\ 3795}#
                                                       #{mod\ 3799}#)
                                                     (#{build-data\ 338}#
                                                       #f
                                                       #f))))))
                                           (#{build-dispatch-call\ 3669}#
                                             #{pvars\ 3809}#
                                             #{exp\ 3798}#
                                             #{y\ 3824}#
                                             #{r\ 3795}#
                                             #{mod\ 3799}#)
                                           (#{gen-syntax-case\ 3673}#
                                             #{x\ 3792}#
                                             #{keys\ 3793}#
                                             #{clauses\ 3794}#
                                             #{r\ 3795}#
                                             #{mod\ 3799}#)))))
                                   (list (if (eq? #{p\ 3808}# (quote any))
                                           (#{build-application\ 312}#
                                             #f
                                             (#{build-primref\ 336}#
                                               #f
                                               'list)
                                             (list #{x\ 3792}#))
                                           (#{build-application\ 312}#
                                             #f
                                             (#{build-primref\ 336}#
                                               #f
                                               '$sc-dispatch)
                                             (list #{x\ 3792}#
                                                   (#{build-data\ 338}#
                                                     #f
                                                     #{p\ 3808}#))))))))))))))
                 (#{gen-syntax-case\ 3673}#
                   (lambda (#{x\ 3838}#
                            #{keys\ 3839}#
                            #{clauses\ 3840}#
                            #{r\ 3841}#
                            #{mod\ 3842}#)
                     (if (null? #{clauses\ 3840}#)
                       (#{build-application\ 312}#
                         #f
                         (#{build-primref\ 336}#
                           #f
                           'syntax-violation)
                         (list (#{build-data\ 338}# #f #f)
                               (#{build-data\ 338}#
                                 #f
                                 "source expression failed to match any pattern")
                               #{x\ 3838}#))
                       (let ((#{tmp\ 3852}# (car #{clauses\ 3840}#)))
                         (let ((#{tmp\ 3853}#
                                 ($sc-dispatch
                                   #{tmp\ 3852}#
                                   '(any any))))
                           (if #{tmp\ 3853}#
                             (@apply
                               (lambda (#{pat\ 3856}# #{exp\ 3857}#)
                                 (if (if (#{id?\ 388}# #{pat\ 3856}#)
                                       (and-map
                                         (lambda (#{x\ 3860}#)
                                           (not (#{free-id=?\ 447}#
                                                  #{pat\ 3856}#
                                                  #{x\ 3860}#)))
                                         (cons '#(syntax-object
                                                  ...
                                                  ((top)
                                                   #(ribcage
                                                     #(pat exp)
                                                     #((top) (top))
                                                     #("i3854" "i3855"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x keys clauses r mod)
                                                     #((top)
                                                       (top)
                                                       (top)
                                                       (top)
                                                       (top))
                                                     #("i3843"
                                                       "i3844"
                                                       "i3845"
                                                       "i3846"
                                                       "i3847"))
                                                   #(ribcage
                                                     (gen-syntax-case
                                                       gen-clause
                                                       build-dispatch-call
                                                       convert-pattern)
                                                     ((top) (top) (top) (top))
                                                     ("i3672"
                                                      "i3670"
                                                      "i3668"
                                                      "i3666"))
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
                                                     ("i502"
                                                      "i500"
                                                      "i498"
                                                      "i496"
                                                      "i494"
                                                      "i492"
                                                      "i490"
                                                      "i488"
                                                      "i486"
                                                      "i484"
                                                      "i482"
                                                      "i480"
                                                      "i478"
                                                      "i476"
                                                      "i474"
                                                      "i472"
                                                      "i470"
                                                      "i468"
                                                      "i466"
                                                      "i464"
                                                      "i462"
                                                      "i460"
                                                      "i458"
                                                      "i456"
                                                      "i454"
                                                      "i452"
                                                      "i450"
                                                      "i448"
                                                      "i446"
                                                      "i444"
                                                      "i442"
                                                      "i440"
                                                      "i438"
                                                      "i436"
                                                      "i434"
                                                      "i432"
                                                      "i431"
                                                      "i430"
                                                      "i428"
                                                      "i427"
                                                      "i426"
                                                      "i425"
                                                      "i424"
                                                      "i422"
                                                      "i420"
                                                      "i418"
                                                      "i416"
                                                      "i414"
                                                      "i412"
                                                      "i410"
                                                      "i408"
                                                      "i405"
                                                      "i403"
                                                      "i402"
                                                      "i401"
                                                      "i400"
                                                      "i399"
                                                      "i398"
                                                      "i396"
                                                      "i394"
                                                      "i392"
                                                      "i390"
                                                      "i389"
                                                      "i387"
                                                      "i385"
                                                      "i383"
                                                      "i381"
                                                      "i379"
                                                      "i377"
                                                      "i375"
                                                      "i374"
                                                      "i372"
                                                      "i370"
                                                      "i369"
                                                      "i368"
                                                      "i366"
                                                      "i365"
                                                      "i363"
                                                      "i361"
                                                      "i359"
                                                      "i357"
                                                      "i355"
                                                      "i353"
                                                      "i351"
                                                      "i349"
                                                      "i347"
                                                      "i345"
                                                      "i343"
                                                      "i341"
                                                      "i339"
                                                      "i337"
                                                      "i335"
                                                      "i333"
                                                      "i331"
                                                      "i329"
                                                      "i327"
                                                      "i325"
                                                      "i323"
                                                      "i321"
                                                      "i319"
                                                      "i317"
                                                      "i315"
                                                      "i313"
                                                      "i311"
                                                      "i309"
                                                      "i307"
                                                      "i305"
                                                      "i303"
                                                      "i301"
                                                      "i300"
                                                      "i298"
                                                      "i296"
                                                      "i294"
                                                      "i292"
                                                      "i290"
                                                      "i288"
                                                      "i286"
                                                      "i284"
                                                      "i282"
                                                      "i279"
                                                      "i277"
                                                      "i275"
                                                      "i273"
                                                      "i271"
                                                      "i269"
                                                      "i267"
                                                      "i265"
                                                      "i263"
                                                      "i261"
                                                      "i259"
                                                      "i257"
                                                      "i255"
                                                      "i253"
                                                      "i251"
                                                      "i249"
                                                      "i247"
                                                      "i245"))
                                                   #(ribcage
                                                     (define-structure
                                                       define-expansion-accessors
                                                       define-expansion-constructors
                                                       and-map*)
                                                     ((top) (top) (top) (top))
                                                     ("i39"
                                                      "i38"
                                                      "i37"
                                                      "i35")))
                                                  (hygiene guile))
                                               #{keys\ 3839}#))
                                       #f)
                                   (begin
                                     (let ((#{labels\ 3864}#
                                             (list (#{gen-label\ 404}#)))
                                           (#{var\ 3865}#
                                             (#{gen-var\ 501}# #{pat\ 3856}#)))
                                       (#{build-application\ 312}#
                                         #f
                                         (#{build-simple-lambda\ 330}#
                                           #f
                                           (list (syntax->datum #{pat\ 3856}#))
                                           #f
                                           (list #{var\ 3865}#)
                                           '()
                                           (#{chi\ 473}#
                                             #{exp\ 3857}#
                                             (#{extend-env\ 376}#
                                               #{labels\ 3864}#
                                               (list (cons 'syntax
                                                           (cons #{var\ 3865}#
                                                                 0)))
                                               #{r\ 3841}#)
                                             (#{make-binding-wrap\ 435}#
                                               (list #{pat\ 3856}#)
                                               #{labels\ 3864}#
                                               '(()))
                                             #{mod\ 3842}#))
                                         (list #{x\ 3838}#))))
                                   (#{gen-clause\ 3671}#
                                     #{x\ 3838}#
                                     #{keys\ 3839}#
                                     (cdr #{clauses\ 3840}#)
                                     #{r\ 3841}#
                                     #{pat\ 3856}#
                                     #t
                                     #{exp\ 3857}#
                                     #{mod\ 3842}#)))
                               #{tmp\ 3853}#)
                             (let ((#{tmp\ 3871}#
                                     ($sc-dispatch
                                       #{tmp\ 3852}#
                                       '(any any any))))
                               (if #{tmp\ 3871}#
                                 (@apply
                                   (lambda (#{pat\ 3875}#
                                            #{fender\ 3876}#
                                            #{exp\ 3877}#)
                                     (#{gen-clause\ 3671}#
                                       #{x\ 3838}#
                                       #{keys\ 3839}#
                                       (cdr #{clauses\ 3840}#)
                                       #{r\ 3841}#
                                       #{pat\ 3875}#
                                       #{fender\ 3876}#
                                       #{exp\ 3877}#
                                       #{mod\ 3842}#))
                                   #{tmp\ 3871}#)
                                 (let ((#{_\ 3879}# #{tmp\ 3852}#))
                                   (syntax-violation
                                     'syntax-case
                                     "invalid clause"
                                     (car #{clauses\ 3840}#))))))))))))
                (begin
                  (lambda (#{e\ 3880}#
                           #{r\ 3881}#
                           #{w\ 3882}#
                           #{s\ 3883}#
                           #{mod\ 3884}#)
                    (begin
                      (let ((#{e\ 3891}#
                              (#{source-wrap\ 459}#
                                #{e\ 3880}#
                                #{w\ 3882}#
                                #{s\ 3883}#
                                #{mod\ 3884}#)))
                        (let ((#{tmp\ 3892}# #{e\ 3891}#))
                          (let ((#{tmp\ 3893}#
                                  ($sc-dispatch
                                    #{tmp\ 3892}#
                                    '(any any each-any . each-any))))
                            (if #{tmp\ 3893}#
                              (@apply
                                (lambda (#{_\ 3898}#
                                         #{val\ 3899}#
                                         #{key\ 3900}#
                                         #{m\ 3901}#)
                                  (if (and-map
                                        (lambda (#{x\ 3902}#)
                                          (if (#{id?\ 388}# #{x\ 3902}#)
                                            (not (#{ellipsis?\ 489}#
                                                   #{x\ 3902}#))
                                            #f))
                                        #{key\ 3900}#)
                                    (begin
                                      (let ((#{x\ 3908}#
                                              (#{gen-var\ 501}# (quote tmp))))
                                        (#{build-application\ 312}#
                                          #{s\ 3883}#
                                          (#{build-simple-lambda\ 330}#
                                            #f
                                            (list (quote tmp))
                                            #f
                                            (list #{x\ 3908}#)
                                            '()
                                            (#{gen-syntax-case\ 3673}#
                                              (#{build-lexical-reference\ 318}#
                                                'value
                                                #f
                                                'tmp
                                                #{x\ 3908}#)
                                              #{key\ 3900}#
                                              #{m\ 3901}#
                                              #{r\ 3881}#
                                              #{mod\ 3884}#))
                                          (list (#{chi\ 473}#
                                                  #{val\ 3899}#
                                                  #{r\ 3881}#
                                                  '(())
                                                  #{mod\ 3884}#)))))
                                    (syntax-violation
                                      'syntax-case
                                      "invalid literals list"
                                      #{e\ 3891}#)))
                                #{tmp\ 3893}#)
                              (syntax-violation
                                #f
                                "source expression failed to match any pattern"
                                #{tmp\ 3892}#))))))))))
            (set! macroexpand
              (lambda*
                (#{x\ 3914}#
                  #:optional
                  (#{m\ 3916}# (quote e))
                  (#{esew\ 3918}# (quote (eval))))
                (#{chi-top\ 471}#
                  #{x\ 3914}#
                  '()
                  '((top))
                  #{m\ 3916}#
                  #{esew\ 3918}#
                  (cons 'hygiene
                        (module-name (current-module))))))
            (set! identifier?
              (lambda (#{x\ 3922}#)
                (#{nonsymbol-id?\ 386}# #{x\ 3922}#)))
            (set! datum->syntax
              (lambda (#{id\ 3924}# #{datum\ 3925}#)
                (#{make-syntax-object\ 350}#
                  #{datum\ 3925}#
                  (#{syntax-object-wrap\ 356}# #{id\ 3924}#)
                  (#{syntax-object-module\ 358}# #{id\ 3924}#))))
            (set! syntax->datum
              (lambda (#{x\ 3928}#)
                (#{strip\ 499}# #{x\ 3928}# (quote (())))))
            (set! syntax-source
              (lambda (#{x\ 3931}#)
                (#{source-annotation\ 367}# #{x\ 3931}#)))
            (set! generate-temporaries
              (lambda (#{ls\ 3933}#)
                (begin
                  (begin
                    (let ((#{x\ 3937}# #{ls\ 3933}#))
                      (if (not (list? #{x\ 3937}#))
                        (syntax-violation
                          'generate-temporaries
                          "invalid argument"
                          #{x\ 3937}#))))
                  (map (lambda (#{x\ 3938}#)
                         (#{wrap\ 457}# (gensym) (quote ((top))) #f))
                       #{ls\ 3933}#))))
            (set! free-identifier=?
              (lambda (#{x\ 3942}# #{y\ 3943}#)
                (begin
                  (begin
                    (let ((#{x\ 3948}# #{x\ 3942}#))
                      (if (not (#{nonsymbol-id?\ 386}# #{x\ 3948}#))
                        (syntax-violation
                          'free-identifier=?
                          "invalid argument"
                          #{x\ 3948}#))))
                  (begin
                    (let ((#{x\ 3951}# #{y\ 3943}#))
                      (if (not (#{nonsymbol-id?\ 386}# #{x\ 3951}#))
                        (syntax-violation
                          'free-identifier=?
                          "invalid argument"
                          #{x\ 3951}#))))
                  (#{free-id=?\ 447}# #{x\ 3942}# #{y\ 3943}#))))
            (set! bound-identifier=?
              (lambda (#{x\ 3952}# #{y\ 3953}#)
                (begin
                  (begin
                    (let ((#{x\ 3958}# #{x\ 3952}#))
                      (if (not (#{nonsymbol-id?\ 386}# #{x\ 3958}#))
                        (syntax-violation
                          'bound-identifier=?
                          "invalid argument"
                          #{x\ 3958}#))))
                  (begin
                    (let ((#{x\ 3961}# #{y\ 3953}#))
                      (if (not (#{nonsymbol-id?\ 386}# #{x\ 3961}#))
                        (syntax-violation
                          'bound-identifier=?
                          "invalid argument"
                          #{x\ 3961}#))))
                  (#{bound-id=?\ 449}# #{x\ 3952}# #{y\ 3953}#))))
            (set! syntax-violation
              (lambda (#{who\ 3962}#
                       #{message\ 3963}#
                       #{form\ 3964}#
                       .
                       #{subform\ 3965}#)
                (begin
                  (begin
                    (let ((#{x\ 3972}# #{who\ 3962}#))
                      (if (not (let ((#{x\ 3973}# #{x\ 3972}#))
                                 (begin
                                   (let ((#{t\ 3977}# (not #{x\ 3973}#)))
                                     (if #{t\ 3977}#
                                       #{t\ 3977}#
                                       (begin
                                         (let ((#{t\ 3980}#
                                                 (string? #{x\ 3973}#)))
                                           (if #{t\ 3980}#
                                             #{t\ 3980}#
                                             (symbol? #{x\ 3973}#)))))))))
                        (syntax-violation
                          'syntax-violation
                          "invalid argument"
                          #{x\ 3972}#))))
                  (begin
                    (let ((#{x\ 3984}# #{message\ 3963}#))
                      (if (not (string? #{x\ 3984}#))
                        (syntax-violation
                          'syntax-violation
                          "invalid argument"
                          #{x\ 3984}#))))
                  (scm-error
                    'syntax-error
                    'macroexpand
                    (string-append
                      (if #{who\ 3962}# "~a: " "")
                      "~a "
                      (if (null? #{subform\ 3965}#)
                        "in ~a"
                        "in subform `~s' of `~s'"))
                    (begin
                      (let ((#{tail\ 3986}#
                              (cons #{message\ 3963}#
                                    (map (lambda (#{x\ 3987}#)
                                           (#{strip\ 499}#
                                             #{x\ 3987}#
                                             '(())))
                                         (append
                                           #{subform\ 3965}#
                                           (list #{form\ 3964}#))))))
                        (if #{who\ 3962}#
                          (cons #{who\ 3962}# #{tail\ 3986}#)
                          #{tail\ 3986}#)))
                    #f))))
            (letrec*
              ((#{match-each\ 3991}#
                 (lambda (#{e\ 4004}#
                          #{p\ 4005}#
                          #{w\ 4006}#
                          #{mod\ 4007}#)
                   (if (pair? #{e\ 4004}#)
                     (begin
                       (let ((#{first\ 4015}#
                               (#{match\ 4003}#
                                 (car #{e\ 4004}#)
                                 #{p\ 4005}#
                                 #{w\ 4006}#
                                 '()
                                 #{mod\ 4007}#)))
                         (if #{first\ 4015}#
                           (begin
                             (let ((#{rest\ 4019}#
                                     (#{match-each\ 3991}#
                                       (cdr #{e\ 4004}#)
                                       #{p\ 4005}#
                                       #{w\ 4006}#
                                       #{mod\ 4007}#)))
                               (if #{rest\ 4019}#
                                 (cons #{first\ 4015}# #{rest\ 4019}#)
                                 #f)))
                           #f)))
                     (if (null? #{e\ 4004}#)
                       '()
                       (if (#{syntax-object?\ 352}# #{e\ 4004}#)
                         (#{match-each\ 3991}#
                           (#{syntax-object-expression\ 354}# #{e\ 4004}#)
                           #{p\ 4005}#
                           (#{join-wraps\ 439}#
                             #{w\ 4006}#
                             (#{syntax-object-wrap\ 356}# #{e\ 4004}#))
                           (#{syntax-object-module\ 358}# #{e\ 4004}#))
                         #f)))))
               (#{match-each+\ 3993}#
                 (lambda (#{e\ 4027}#
                          #{x-pat\ 4028}#
                          #{y-pat\ 4029}#
                          #{z-pat\ 4030}#
                          #{w\ 4031}#
                          #{r\ 4032}#
                          #{mod\ 4033}#)
                   (letrec*
                     ((#{f\ 4044}#
                        (lambda (#{e\ 4045}# #{w\ 4046}#)
                          (if (pair? #{e\ 4045}#)
                            (call-with-values
                              (lambda ()
                                (#{f\ 4044}# (cdr #{e\ 4045}#) #{w\ 4046}#))
                              (lambda (#{xr*\ 4049}#
                                       #{y-pat\ 4050}#
                                       #{r\ 4051}#)
                                (if #{r\ 4051}#
                                  (if (null? #{y-pat\ 4050}#)
                                    (begin
                                      (let ((#{xr\ 4056}#
                                              (#{match\ 4003}#
                                                (car #{e\ 4045}#)
                                                #{x-pat\ 4028}#
                                                #{w\ 4046}#
                                                '()
                                                #{mod\ 4033}#)))
                                        (if #{xr\ 4056}#
                                          (values
                                            (cons #{xr\ 4056}# #{xr*\ 4049}#)
                                            #{y-pat\ 4050}#
                                            #{r\ 4051}#)
                                          (values #f #f #f))))
                                    (values
                                      '()
                                      (cdr #{y-pat\ 4050}#)
                                      (#{match\ 4003}#
                                        (car #{e\ 4045}#)
                                        (car #{y-pat\ 4050}#)
                                        #{w\ 4046}#
                                        #{r\ 4051}#
                                        #{mod\ 4033}#)))
                                  (values #f #f #f))))
                            (if (#{syntax-object?\ 352}# #{e\ 4045}#)
                              (#{f\ 4044}#
                                (#{syntax-object-expression\ 354}# #{e\ 4045}#)
                                (#{join-wraps\ 439}# #{w\ 4046}# #{e\ 4045}#))
                              (values
                                '()
                                #{y-pat\ 4029}#
                                (#{match\ 4003}#
                                  #{e\ 4045}#
                                  #{z-pat\ 4030}#
                                  #{w\ 4046}#
                                  #{r\ 4032}#
                                  #{mod\ 4033}#)))))))
                     (begin (#{f\ 4044}# #{e\ 4027}# #{w\ 4031}#)))))
               (#{match-each-any\ 3995}#
                 (lambda (#{e\ 4060}# #{w\ 4061}# #{mod\ 4062}#)
                   (if (pair? #{e\ 4060}#)
                     (begin
                       (let ((#{l\ 4069}#
                               (#{match-each-any\ 3995}#
                                 (cdr #{e\ 4060}#)
                                 #{w\ 4061}#
                                 #{mod\ 4062}#)))
                         (if #{l\ 4069}#
                           (cons (#{wrap\ 457}#
                                   (car #{e\ 4060}#)
                                   #{w\ 4061}#
                                   #{mod\ 4062}#)
                                 #{l\ 4069}#)
                           #f)))
                     (if (null? #{e\ 4060}#)
                       '()
                       (if (#{syntax-object?\ 352}# #{e\ 4060}#)
                         (#{match-each-any\ 3995}#
                           (#{syntax-object-expression\ 354}# #{e\ 4060}#)
                           (#{join-wraps\ 439}#
                             #{w\ 4061}#
                             (#{syntax-object-wrap\ 356}# #{e\ 4060}#))
                           #{mod\ 4062}#)
                         #f)))))
               (#{match-empty\ 3997}#
                 (lambda (#{p\ 4077}# #{r\ 4078}#)
                   (if (null? #{p\ 4077}#)
                     #{r\ 4078}#
                     (if (eq? #{p\ 4077}# (quote any))
                       (cons (quote ()) #{r\ 4078}#)
                       (if (pair? #{p\ 4077}#)
                         (#{match-empty\ 3997}#
                           (car #{p\ 4077}#)
                           (#{match-empty\ 3997}#
                             (cdr #{p\ 4077}#)
                             #{r\ 4078}#))
                         (if (eq? #{p\ 4077}# (quote each-any))
                           (cons (quote ()) #{r\ 4078}#)
                           (begin
                             (let ((#{atom-key\ 4092}#
                                     (vector-ref #{p\ 4077}# 0)))
                               (if (eqv? #{atom-key\ 4092}# (quote each))
                                 (#{match-empty\ 3997}#
                                   (vector-ref #{p\ 4077}# 1)
                                   #{r\ 4078}#)
                                 (if (eqv? #{atom-key\ 4092}# (quote each+))
                                   (#{match-empty\ 3997}#
                                     (vector-ref #{p\ 4077}# 1)
                                     (#{match-empty\ 3997}#
                                       (reverse (vector-ref #{p\ 4077}# 2))
                                       (#{match-empty\ 3997}#
                                         (vector-ref #{p\ 4077}# 3)
                                         #{r\ 4078}#)))
                                   (if (if (eqv? #{atom-key\ 4092}#
                                                 'free-id)
                                         #t
                                         (eqv? #{atom-key\ 4092}#
                                               'atom))
                                     #{r\ 4078}#
                                     (if (eqv? #{atom-key\ 4092}#
                                               'vector)
                                       (#{match-empty\ 3997}#
                                         (vector-ref #{p\ 4077}# 1)
                                         #{r\ 4078}#)))))))))))))
               (#{combine\ 3999}#
                 (lambda (#{r*\ 4097}# #{r\ 4098}#)
                   (if (null? (car #{r*\ 4097}#))
                     #{r\ 4098}#
                     (cons (map car #{r*\ 4097}#)
                           (#{combine\ 3999}#
                             (map cdr #{r*\ 4097}#)
                             #{r\ 4098}#)))))
               (#{match*\ 4001}#
                 (lambda (#{e\ 4101}#
                          #{p\ 4102}#
                          #{w\ 4103}#
                          #{r\ 4104}#
                          #{mod\ 4105}#)
                   (if (null? #{p\ 4102}#)
                     (if (null? #{e\ 4101}#) #{r\ 4104}# #f)
                     (if (pair? #{p\ 4102}#)
                       (if (pair? #{e\ 4101}#)
                         (#{match\ 4003}#
                           (car #{e\ 4101}#)
                           (car #{p\ 4102}#)
                           #{w\ 4103}#
                           (#{match\ 4003}#
                             (cdr #{e\ 4101}#)
                             (cdr #{p\ 4102}#)
                             #{w\ 4103}#
                             #{r\ 4104}#
                             #{mod\ 4105}#)
                           #{mod\ 4105}#)
                         #f)
                       (if (eq? #{p\ 4102}# (quote each-any))
                         (begin
                           (let ((#{l\ 4122}#
                                   (#{match-each-any\ 3995}#
                                     #{e\ 4101}#
                                     #{w\ 4103}#
                                     #{mod\ 4105}#)))
                             (if #{l\ 4122}#
                               (cons #{l\ 4122}# #{r\ 4104}#)
                               #f)))
                         (begin
                           (let ((#{atom-key\ 4128}#
                                   (vector-ref #{p\ 4102}# 0)))
                             (if (eqv? #{atom-key\ 4128}# (quote each))
                               (if (null? #{e\ 4101}#)
                                 (#{match-empty\ 3997}#
                                   (vector-ref #{p\ 4102}# 1)
                                   #{r\ 4104}#)
                                 (begin
                                   (let ((#{l\ 4131}#
                                           (#{match-each\ 3991}#
                                             #{e\ 4101}#
                                             (vector-ref #{p\ 4102}# 1)
                                             #{w\ 4103}#
                                             #{mod\ 4105}#)))
                                     (if #{l\ 4131}#
                                       (letrec*
                                         ((#{collect\ 4136}#
                                            (lambda (#{l\ 4137}#)
                                              (if (null? (car #{l\ 4137}#))
                                                #{r\ 4104}#
                                                (cons (map car #{l\ 4137}#)
                                                      (#{collect\ 4136}#
                                                        (map cdr
                                                             #{l\ 4137}#)))))))
                                         (begin
                                           (#{collect\ 4136}# #{l\ 4131}#)))
                                       #f))))
                               (if (eqv? #{atom-key\ 4128}# (quote each+))
                                 (call-with-values
                                   (lambda ()
                                     (#{match-each+\ 3993}#
                                       #{e\ 4101}#
                                       (vector-ref #{p\ 4102}# 1)
                                       (vector-ref #{p\ 4102}# 2)
                                       (vector-ref #{p\ 4102}# 3)
                                       #{w\ 4103}#
                                       #{r\ 4104}#
                                       #{mod\ 4105}#))
                                   (lambda (#{xr*\ 4139}#
                                            #{y-pat\ 4140}#
                                            #{r\ 4141}#)
                                     (if #{r\ 4141}#
                                       (if (null? #{y-pat\ 4140}#)
                                         (if (null? #{xr*\ 4139}#)
                                           (#{match-empty\ 3997}#
                                             (vector-ref #{p\ 4102}# 1)
                                             #{r\ 4141}#)
                                           (#{combine\ 3999}#
                                             #{xr*\ 4139}#
                                             #{r\ 4141}#))
                                         #f)
                                       #f)))
                                 (if (eqv? #{atom-key\ 4128}# (quote free-id))
                                   (if (#{id?\ 388}# #{e\ 4101}#)
                                     (if (#{free-id=?\ 447}#
                                           (#{wrap\ 457}#
                                             #{e\ 4101}#
                                             #{w\ 4103}#
                                             #{mod\ 4105}#)
                                           (vector-ref #{p\ 4102}# 1))
                                       #{r\ 4104}#
                                       #f)
                                     #f)
                                   (if (eqv? #{atom-key\ 4128}# (quote atom))
                                     (if (equal?
                                           (vector-ref #{p\ 4102}# 1)
                                           (#{strip\ 499}#
                                             #{e\ 4101}#
                                             #{w\ 4103}#))
                                       #{r\ 4104}#
                                       #f)
                                     (if (eqv? #{atom-key\ 4128}#
                                               'vector)
                                       (if (vector? #{e\ 4101}#)
                                         (#{match\ 4003}#
                                           (vector->list #{e\ 4101}#)
                                           (vector-ref #{p\ 4102}# 1)
                                           #{w\ 4103}#
                                           #{r\ 4104}#
                                           #{mod\ 4105}#)
                                         #f)))))))))))))
               (#{match\ 4003}#
                 (lambda (#{e\ 4158}#
                          #{p\ 4159}#
                          #{w\ 4160}#
                          #{r\ 4161}#
                          #{mod\ 4162}#)
                   (if (not #{r\ 4161}#)
                     #f
                     (if (eq? #{p\ 4159}# (quote any))
                       (cons (#{wrap\ 457}#
                               #{e\ 4158}#
                               #{w\ 4160}#
                               #{mod\ 4162}#)
                             #{r\ 4161}#)
                       (if (#{syntax-object?\ 352}# #{e\ 4158}#)
                         (#{match*\ 4001}#
                           (#{syntax-object-expression\ 354}# #{e\ 4158}#)
                           #{p\ 4159}#
                           (#{join-wraps\ 439}#
                             #{w\ 4160}#
                             (#{syntax-object-wrap\ 356}# #{e\ 4158}#))
                           #{r\ 4161}#
                           (#{syntax-object-module\ 358}# #{e\ 4158}#))
                         (#{match*\ 4001}#
                           #{e\ 4158}#
                           #{p\ 4159}#
                           #{w\ 4160}#
                           #{r\ 4161}#
                           #{mod\ 4162}#)))))))
              (begin
                (set! $sc-dispatch
                  (lambda (#{e\ 4175}# #{p\ 4176}#)
                    (if (eq? #{p\ 4176}# (quote any))
                      (list #{e\ 4175}#)
                      (if (#{syntax-object?\ 352}# #{e\ 4175}#)
                        (#{match*\ 4001}#
                          (#{syntax-object-expression\ 354}# #{e\ 4175}#)
                          #{p\ 4176}#
                          (#{syntax-object-wrap\ 356}# #{e\ 4175}#)
                          '()
                          (#{syntax-object-module\ 358}# #{e\ 4175}#))
                        (#{match*\ 4001}#
                          #{e\ 4175}#
                          #{p\ 4176}#
                          '(())
                          '()
                          #f)))))))))))))

(define with-syntax
  (make-syntax-transformer
    'with-syntax
    'macro
    (lambda (#{x\ 4185}#)
      (let ((#{tmp\ 4187}# #{x\ 4185}#))
        (let ((#{tmp\ 4188}#
                ($sc-dispatch
                  #{tmp\ 4187}#
                  '(any () any . each-any))))
          (if #{tmp\ 4188}#
            (@apply
              (lambda (#{_\ 4192}# #{e1\ 4193}# #{e2\ 4194}#)
                (cons '#(syntax-object
                         begin
                         ((top)
                          #(ribcage
                            #(_ e1 e2)
                            #((top) (top) (top))
                            #("i4189" "i4190" "i4191"))
                          #(ribcage () () ())
                          #(ribcage #(x) #((top)) #("i4186")))
                         (hygiene guile))
                      (cons #{e1\ 4193}# #{e2\ 4194}#)))
              #{tmp\ 4188}#)
            (let ((#{tmp\ 4196}#
                    ($sc-dispatch
                      #{tmp\ 4187}#
                      '(any ((any any)) any . each-any))))
              (if #{tmp\ 4196}#
                (@apply
                  (lambda (#{_\ 4202}#
                           #{out\ 4203}#
                           #{in\ 4204}#
                           #{e1\ 4205}#
                           #{e2\ 4206}#)
                    (list '#(syntax-object
                             syntax-case
                             ((top)
                              #(ribcage
                                #(_ out in e1 e2)
                                #((top) (top) (top) (top) (top))
                                #("i4197" "i4198" "i4199" "i4200" "i4201"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4186")))
                             (hygiene guile))
                          #{in\ 4204}#
                          '()
                          (list #{out\ 4203}#
                                (cons '#(syntax-object
                                         begin
                                         ((top)
                                          #(ribcage
                                            #(_ out in e1 e2)
                                            #((top) (top) (top) (top) (top))
                                            #("i4197"
                                              "i4198"
                                              "i4199"
                                              "i4200"
                                              "i4201"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i4186")))
                                         (hygiene guile))
                                      (cons #{e1\ 4205}# #{e2\ 4206}#)))))
                  #{tmp\ 4196}#)
                (let ((#{tmp\ 4208}#
                        ($sc-dispatch
                          #{tmp\ 4187}#
                          '(any #(each (any any)) any . each-any))))
                  (if #{tmp\ 4208}#
                    (@apply
                      (lambda (#{_\ 4214}#
                               #{out\ 4215}#
                               #{in\ 4216}#
                               #{e1\ 4217}#
                               #{e2\ 4218}#)
                        (list '#(syntax-object
                                 syntax-case
                                 ((top)
                                  #(ribcage
                                    #(_ out in e1 e2)
                                    #((top) (top) (top) (top) (top))
                                    #("i4209" "i4210" "i4211" "i4212" "i4213"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i4186")))
                                 (hygiene guile))
                              (cons '#(syntax-object
                                       list
                                       ((top)
                                        #(ribcage
                                          #(_ out in e1 e2)
                                          #((top) (top) (top) (top) (top))
                                          #("i4209"
                                            "i4210"
                                            "i4211"
                                            "i4212"
                                            "i4213"))
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i4186")))
                                       (hygiene guile))
                                    #{in\ 4216}#)
                              '()
                              (list #{out\ 4215}#
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
                                                #("i4209"
                                                  "i4210"
                                                  "i4211"
                                                  "i4212"
                                                  "i4213"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(x)
                                                #((top))
                                                #("i4186")))
                                             (hygiene guile))
                                          (cons #{e1\ 4217}# #{e2\ 4218}#)))))
                      #{tmp\ 4208}#)
                    (syntax-violation
                      #f
                      "source expression failed to match any pattern"
                      #{tmp\ 4187}#)))))))))))

(define syntax-rules
  (make-syntax-transformer
    'syntax-rules
    'macro
    (lambda (#{x\ 4222}#)
      (let ((#{tmp\ 4224}# #{x\ 4222}#))
        (let ((#{tmp\ 4225}#
                ($sc-dispatch
                  #{tmp\ 4224}#
                  '(any each-any . #(each ((any . any) any))))))
          (if #{tmp\ 4225}#
            (@apply
              (lambda (#{_\ 4231}#
                       #{k\ 4232}#
                       #{keyword\ 4233}#
                       #{pattern\ 4234}#
                       #{template\ 4235}#)
                (list '#(syntax-object
                         lambda
                         ((top)
                          #(ribcage
                            #(_ k keyword pattern template)
                            #((top) (top) (top) (top) (top))
                            #("i4226" "i4227" "i4228" "i4229" "i4230"))
                          #(ribcage () () ())
                          #(ribcage #(x) #((top)) #("i4223")))
                         (hygiene guile))
                      '(#(syntax-object
                          x
                          ((top)
                           #(ribcage
                             #(_ k keyword pattern template)
                             #((top) (top) (top) (top) (top))
                             #("i4226" "i4227" "i4228" "i4229" "i4230"))
                           #(ribcage () () ())
                           #(ribcage #(x) #((top)) #("i4223")))
                          (hygiene guile)))
                      (vector
                        '(#(syntax-object
                            macro-type
                            ((top)
                             #(ribcage
                               #(_ k keyword pattern template)
                               #((top) (top) (top) (top) (top))
                               #("i4226" "i4227" "i4228" "i4229" "i4230"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i4223")))
                            (hygiene guile))
                          .
                          #(syntax-object
                            syntax-rules
                            ((top)
                             #(ribcage
                               #(_ k keyword pattern template)
                               #((top) (top) (top) (top) (top))
                               #("i4226" "i4227" "i4228" "i4229" "i4230"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i4223")))
                            (hygiene guile)))
                        (cons '#(syntax-object
                                 patterns
                                 ((top)
                                  #(ribcage
                                    #(_ k keyword pattern template)
                                    #((top) (top) (top) (top) (top))
                                    #("i4226" "i4227" "i4228" "i4229" "i4230"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i4223")))
                                 (hygiene guile))
                              #{pattern\ 4234}#))
                      (cons '#(syntax-object
                               syntax-case
                               ((top)
                                #(ribcage
                                  #(_ k keyword pattern template)
                                  #((top) (top) (top) (top) (top))
                                  #("i4226" "i4227" "i4228" "i4229" "i4230"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i4223")))
                               (hygiene guile))
                            (cons '#(syntax-object
                                     x
                                     ((top)
                                      #(ribcage
                                        #(_ k keyword pattern template)
                                        #((top) (top) (top) (top) (top))
                                        #("i4226"
                                          "i4227"
                                          "i4228"
                                          "i4229"
                                          "i4230"))
                                      #(ribcage () () ())
                                      #(ribcage #(x) #((top)) #("i4223")))
                                     (hygiene guile))
                                  (cons #{k\ 4232}#
                                        (map (lambda (#{tmp\ 4239}#
                                                      #{tmp\ 4238}#)
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
                                                                 #("i4226"
                                                                   "i4227"
                                                                   "i4228"
                                                                   "i4229"
                                                                   "i4230"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4223")))
                                                              (hygiene guile))
                                                           #{tmp\ 4238}#)
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
                                                                 #("i4226"
                                                                   "i4227"
                                                                   "i4228"
                                                                   "i4229"
                                                                   "i4230"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4223")))
                                                              (hygiene guile))
                                                           #{tmp\ 4239}#)))
                                             #{template\ 4235}#
                                             #{pattern\ 4234}#))))))
              #{tmp\ 4225}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp\ 4224}#)))))))

(define let*
  (make-syntax-transformer
    'let*
    'macro
    (lambda (#{x\ 4240}#)
      (let ((#{tmp\ 4242}# #{x\ 4240}#))
        (let ((#{tmp\ 4243}#
                ($sc-dispatch
                  #{tmp\ 4242}#
                  '(any #(each (any any)) any . each-any))))
          (if (if #{tmp\ 4243}#
                (@apply
                  (lambda (#{let*\ 4249}#
                           #{x\ 4250}#
                           #{v\ 4251}#
                           #{e1\ 4252}#
                           #{e2\ 4253}#)
                    (and-map identifier? #{x\ 4250}#))
                  #{tmp\ 4243}#)
                #f)
            (@apply
              (lambda (#{let*\ 4260}#
                       #{x\ 4261}#
                       #{v\ 4262}#
                       #{e1\ 4263}#
                       #{e2\ 4264}#)
                (letrec*
                  ((#{f\ 4267}#
                     (lambda (#{bindings\ 4268}#)
                       (if (null? #{bindings\ 4268}#)
                         (cons '#(syntax-object
                                  let
                                  ((top)
                                   #(ribcage () () ())
                                   #(ribcage
                                     #(f bindings)
                                     #((top) (top))
                                     #("i4265" "i4266"))
                                   #(ribcage
                                     #(let* x v e1 e2)
                                     #((top) (top) (top) (top) (top))
                                     #("i4255"
                                       "i4256"
                                       "i4257"
                                       "i4258"
                                       "i4259"))
                                   #(ribcage () () ())
                                   #(ribcage #(x) #((top)) #("i4241")))
                                  (hygiene guile))
                               (cons '()
                                     (cons #{e1\ 4263}# #{e2\ 4264}#)))
                         (let ((#{tmp\ 4273}#
                                 (list (#{f\ 4267}# (cdr #{bindings\ 4268}#))
                                       (car #{bindings\ 4268}#))))
                           (let ((#{tmp\ 4274}#
                                   ($sc-dispatch
                                     #{tmp\ 4273}#
                                     '(any any))))
                             (if #{tmp\ 4274}#
                               (@apply
                                 (lambda (#{body\ 4277}# #{binding\ 4278}#)
                                   (list '#(syntax-object
                                            let
                                            ((top)
                                             #(ribcage
                                               #(body binding)
                                               #((top) (top))
                                               #("i4275" "i4276"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(f bindings)
                                               #((top) (top))
                                               #("i4265" "i4266"))
                                             #(ribcage
                                               #(let* x v e1 e2)
                                               #((top) (top) (top) (top) (top))
                                               #("i4255"
                                                 "i4256"
                                                 "i4257"
                                                 "i4258"
                                                 "i4259"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x)
                                               #((top))
                                               #("i4241")))
                                            (hygiene guile))
                                         (list #{binding\ 4278}#)
                                         #{body\ 4277}#))
                                 #{tmp\ 4274}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp\ 4273}#))))))))
                  (begin
                    (#{f\ 4267}# (map list #{x\ 4261}# #{v\ 4262}#)))))
              #{tmp\ 4243}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp\ 4242}#)))))))

(define do
  (make-syntax-transformer
    'do
    'macro
    (lambda (#{orig-x\ 4279}#)
      (let ((#{tmp\ 4281}# #{orig-x\ 4279}#))
        (let ((#{tmp\ 4282}#
                ($sc-dispatch
                  #{tmp\ 4281}#
                  '(any #(each (any any . any))
                        (any . each-any)
                        .
                        each-any))))
          (if #{tmp\ 4282}#
            (@apply
              (lambda (#{_\ 4290}#
                       #{var\ 4291}#
                       #{init\ 4292}#
                       #{step\ 4293}#
                       #{e0\ 4294}#
                       #{e1\ 4295}#
                       #{c\ 4296}#)
                (let ((#{tmp\ 4298}#
                        (map (lambda (#{v\ 4319}# #{s\ 4320}#)
                               (let ((#{tmp\ 4323}# #{s\ 4320}#))
                                 (let ((#{tmp\ 4324}#
                                         ($sc-dispatch
                                           #{tmp\ 4323}#
                                           '())))
                                   (if #{tmp\ 4324}#
                                     (@apply
                                       (lambda () #{v\ 4319}#)
                                       #{tmp\ 4324}#)
                                     (let ((#{tmp\ 4325}#
                                             ($sc-dispatch
                                               #{tmp\ 4323}#
                                               '(any))))
                                       (if #{tmp\ 4325}#
                                         (@apply
                                           (lambda (#{e\ 4327}#) #{e\ 4327}#)
                                           #{tmp\ 4325}#)
                                         (let ((#{_\ 4329}# #{tmp\ 4323}#))
                                           (syntax-violation
                                             'do
                                             "bad step expression"
                                             #{orig-x\ 4279}#
                                             #{s\ 4320}#))))))))
                             #{var\ 4291}#
                             #{step\ 4293}#)))
                  (let ((#{tmp\ 4299}#
                          ($sc-dispatch #{tmp\ 4298}# (quote each-any))))
                    (if #{tmp\ 4299}#
                      (@apply
                        (lambda (#{step\ 4301}#)
                          (let ((#{tmp\ 4302}# #{e1\ 4295}#))
                            (let ((#{tmp\ 4303}#
                                    ($sc-dispatch #{tmp\ 4302}# (quote ()))))
                              (if #{tmp\ 4303}#
                                (@apply
                                  (lambda ()
                                    (list '#(syntax-object
                                             let
                                             ((top)
                                              #(ribcage
                                                #(step)
                                                #((top))
                                                #("i4300"))
                                              #(ribcage
                                                #(_ var init step e0 e1 c)
                                                #((top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top))
                                                #("i4283"
                                                  "i4284"
                                                  "i4285"
                                                  "i4286"
                                                  "i4287"
                                                  "i4288"
                                                  "i4289"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(orig-x)
                                                #((top))
                                                #("i4280")))
                                             (hygiene guile))
                                          '#(syntax-object
                                             doloop
                                             ((top)
                                              #(ribcage
                                                #(step)
                                                #((top))
                                                #("i4300"))
                                              #(ribcage
                                                #(_ var init step e0 e1 c)
                                                #((top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top))
                                                #("i4283"
                                                  "i4284"
                                                  "i4285"
                                                  "i4286"
                                                  "i4287"
                                                  "i4288"
                                                  "i4289"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(orig-x)
                                                #((top))
                                                #("i4280")))
                                             (hygiene guile))
                                          (map list
                                               #{var\ 4291}#
                                               #{init\ 4292}#)
                                          (list '#(syntax-object
                                                   if
                                                   ((top)
                                                    #(ribcage
                                                      #(step)
                                                      #((top))
                                                      #("i4300"))
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
                                                      #("i4283"
                                                        "i4284"
                                                        "i4285"
                                                        "i4286"
                                                        "i4287"
                                                        "i4288"
                                                        "i4289"))
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(orig-x)
                                                      #((top))
                                                      #("i4280")))
                                                   (hygiene guile))
                                                (list '#(syntax-object
                                                         not
                                                         ((top)
                                                          #(ribcage
                                                            #(step)
                                                            #((top))
                                                            #("i4300"))
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
                                                            #("i4283"
                                                              "i4284"
                                                              "i4285"
                                                              "i4286"
                                                              "i4287"
                                                              "i4288"
                                                              "i4289"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(orig-x)
                                                            #((top))
                                                            #("i4280")))
                                                         (hygiene guile))
                                                      #{e0\ 4294}#)
                                                (cons '#(syntax-object
                                                         begin
                                                         ((top)
                                                          #(ribcage
                                                            #(step)
                                                            #((top))
                                                            #("i4300"))
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
                                                            #("i4283"
                                                              "i4284"
                                                              "i4285"
                                                              "i4286"
                                                              "i4287"
                                                              "i4288"
                                                              "i4289"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(orig-x)
                                                            #((top))
                                                            #("i4280")))
                                                         (hygiene guile))
                                                      (append
                                                        #{c\ 4296}#
                                                        (list (cons '#(syntax-object
                                                                       doloop
                                                                       ((top)
                                                                        #(ribcage
                                                                          #(step)
                                                                          #((top))
                                                                          #("i4300"))
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
                                                                          #("i4283"
                                                                            "i4284"
                                                                            "i4285"
                                                                            "i4286"
                                                                            "i4287"
                                                                            "i4288"
                                                                            "i4289"))
                                                                        #(ribcage
                                                                          ()
                                                                          ()
                                                                          ())
                                                                        #(ribcage
                                                                          #(orig-x)
                                                                          #((top))
                                                                          #("i4280")))
                                                                       (hygiene
                                                                         guile))
                                                                    #{step\ 4301}#)))))))
                                  #{tmp\ 4303}#)
                                (let ((#{tmp\ 4308}#
                                        ($sc-dispatch
                                          #{tmp\ 4302}#
                                          '(any . each-any))))
                                  (if #{tmp\ 4308}#
                                    (@apply
                                      (lambda (#{e1\ 4311}# #{e2\ 4312}#)
                                        (list '#(syntax-object
                                                 let
                                                 ((top)
                                                  #(ribcage
                                                    #(e1 e2)
                                                    #((top) (top))
                                                    #("i4309" "i4310"))
                                                  #(ribcage
                                                    #(step)
                                                    #((top))
                                                    #("i4300"))
                                                  #(ribcage
                                                    #(_ var init step e0 e1 c)
                                                    #((top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top))
                                                    #("i4283"
                                                      "i4284"
                                                      "i4285"
                                                      "i4286"
                                                      "i4287"
                                                      "i4288"
                                                      "i4289"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(orig-x)
                                                    #((top))
                                                    #("i4280")))
                                                 (hygiene guile))
                                              '#(syntax-object
                                                 doloop
                                                 ((top)
                                                  #(ribcage
                                                    #(e1 e2)
                                                    #((top) (top))
                                                    #("i4309" "i4310"))
                                                  #(ribcage
                                                    #(step)
                                                    #((top))
                                                    #("i4300"))
                                                  #(ribcage
                                                    #(_ var init step e0 e1 c)
                                                    #((top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top))
                                                    #("i4283"
                                                      "i4284"
                                                      "i4285"
                                                      "i4286"
                                                      "i4287"
                                                      "i4288"
                                                      "i4289"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(orig-x)
                                                    #((top))
                                                    #("i4280")))
                                                 (hygiene guile))
                                              (map list
                                                   #{var\ 4291}#
                                                   #{init\ 4292}#)
                                              (list '#(syntax-object
                                                       if
                                                       ((top)
                                                        #(ribcage
                                                          #(e1 e2)
                                                          #((top) (top))
                                                          #("i4309" "i4310"))
                                                        #(ribcage
                                                          #(step)
                                                          #((top))
                                                          #("i4300"))
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
                                                          #("i4283"
                                                            "i4284"
                                                            "i4285"
                                                            "i4286"
                                                            "i4287"
                                                            "i4288"
                                                            "i4289"))
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(orig-x)
                                                          #((top))
                                                          #("i4280")))
                                                       (hygiene guile))
                                                    #{e0\ 4294}#
                                                    (cons '#(syntax-object
                                                             begin
                                                             ((top)
                                                              #(ribcage
                                                                #(e1 e2)
                                                                #((top) (top))
                                                                #("i4309"
                                                                  "i4310"))
                                                              #(ribcage
                                                                #(step)
                                                                #((top))
                                                                #("i4300"))
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
                                                                #("i4283"
                                                                  "i4284"
                                                                  "i4285"
                                                                  "i4286"
                                                                  "i4287"
                                                                  "i4288"
                                                                  "i4289"))
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                #(orig-x)
                                                                #((top))
                                                                #("i4280")))
                                                             (hygiene guile))
                                                          (cons #{e1\ 4311}#
                                                                #{e2\ 4312}#))
                                                    (cons '#(syntax-object
                                                             begin
                                                             ((top)
                                                              #(ribcage
                                                                #(e1 e2)
                                                                #((top) (top))
                                                                #("i4309"
                                                                  "i4310"))
                                                              #(ribcage
                                                                #(step)
                                                                #((top))
                                                                #("i4300"))
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
                                                                #("i4283"
                                                                  "i4284"
                                                                  "i4285"
                                                                  "i4286"
                                                                  "i4287"
                                                                  "i4288"
                                                                  "i4289"))
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                #(orig-x)
                                                                #((top))
                                                                #("i4280")))
                                                             (hygiene guile))
                                                          (append
                                                            #{c\ 4296}#
                                                            (list (cons '#(syntax-object
                                                                           doloop
                                                                           ((top)
                                                                            #(ribcage
                                                                              #(e1
                                                                                e2)
                                                                              #((top)
                                                                                (top))
                                                                              #("i4309"
                                                                                "i4310"))
                                                                            #(ribcage
                                                                              #(step)
                                                                              #((top))
                                                                              #("i4300"))
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
                                                                              #("i4283"
                                                                                "i4284"
                                                                                "i4285"
                                                                                "i4286"
                                                                                "i4287"
                                                                                "i4288"
                                                                                "i4289"))
                                                                            #(ribcage
                                                                              ()
                                                                              ()
                                                                              ())
                                                                            #(ribcage
                                                                              #(orig-x)
                                                                              #((top))
                                                                              #("i4280")))
                                                                           (hygiene
                                                                             guile))
                                                                        #{step\ 4301}#)))))))
                                      #{tmp\ 4308}#)
                                    (syntax-violation
                                      #f
                                      "source expression failed to match any pattern"
                                      #{tmp\ 4302}#)))))))
                        #{tmp\ 4299}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp\ 4298}#)))))
              #{tmp\ 4282}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp\ 4281}#)))))))

(define quasiquote
  (make-syntax-transformer
    'quasiquote
    'macro
    (letrec*
      ((#{quasicons\ 4336}#
         (lambda (#{x\ 4340}# #{y\ 4341}#)
           (let ((#{tmp\ 4345}# (list #{x\ 4340}# #{y\ 4341}#)))
             (let ((#{tmp\ 4346}#
                     ($sc-dispatch #{tmp\ 4345}# (quote (any any)))))
               (if #{tmp\ 4346}#
                 (@apply
                   (lambda (#{x\ 4349}# #{y\ 4350}#)
                     (let ((#{tmp\ 4351}# #{y\ 4350}#))
                       (let ((#{tmp\ 4352}#
                               ($sc-dispatch
                                 #{tmp\ 4351}#
                                 '(#(free-id
                                     #(syntax-object
                                       quote
                                       ((top)
                                        #(ribcage
                                          #(x y)
                                          #((top) (top))
                                          #("i4347" "i4348"))
                                        #(ribcage () () ())
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(x y)
                                          #((top) (top))
                                          #("i4342" "i4343"))
                                        #(ribcage
                                          #(quasicons
                                            quasiappend
                                            quasivector
                                            quasi)
                                          #((top) (top) (top) (top))
                                          #("i4332" "i4333" "i4334" "i4335")))
                                       (hygiene guile)))
                                   any))))
                         (if #{tmp\ 4352}#
                           (@apply
                             (lambda (#{dy\ 4354}#)
                               (let ((#{tmp\ 4355}# #{x\ 4349}#))
                                 (let ((#{tmp\ 4356}#
                                         ($sc-dispatch
                                           #{tmp\ 4355}#
                                           '(#(free-id
                                               #(syntax-object
                                                 quote
                                                 ((top)
                                                  #(ribcage
                                                    #(dy)
                                                    #((top))
                                                    #("i4353"))
                                                  #(ribcage
                                                    #(x y)
                                                    #((top) (top))
                                                    #("i4347" "i4348"))
                                                  #(ribcage () () ())
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(x y)
                                                    #((top) (top))
                                                    #("i4342" "i4343"))
                                                  #(ribcage
                                                    #(quasicons
                                                      quasiappend
                                                      quasivector
                                                      quasi)
                                                    #((top) (top) (top) (top))
                                                    #("i4332"
                                                      "i4333"
                                                      "i4334"
                                                      "i4335")))
                                                 (hygiene guile)))
                                             any))))
                                   (if #{tmp\ 4356}#
                                     (@apply
                                       (lambda (#{dx\ 4358}#)
                                         (list '#(syntax-object
                                                  quote
                                                  ((top)
                                                   #(ribcage
                                                     #(dx)
                                                     #((top))
                                                     #("i4357"))
                                                   #(ribcage
                                                     #(dy)
                                                     #((top))
                                                     #("i4353"))
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4347" "i4348"))
                                                   #(ribcage () () ())
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4342" "i4343"))
                                                   #(ribcage
                                                     #(quasicons
                                                       quasiappend
                                                       quasivector
                                                       quasi)
                                                     #((top) (top) (top) (top))
                                                     #("i4332"
                                                       "i4333"
                                                       "i4334"
                                                       "i4335")))
                                                  (hygiene guile))
                                               (cons #{dx\ 4358}#
                                                     #{dy\ 4354}#)))
                                       #{tmp\ 4356}#)
                                     (let ((#{_\ 4360}# #{tmp\ 4355}#))
                                       (if (null? #{dy\ 4354}#)
                                         (list '#(syntax-object
                                                  list
                                                  ((top)
                                                   #(ribcage
                                                     #(_)
                                                     #((top))
                                                     #("i4359"))
                                                   #(ribcage
                                                     #(dy)
                                                     #((top))
                                                     #("i4353"))
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4347" "i4348"))
                                                   #(ribcage () () ())
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4342" "i4343"))
                                                   #(ribcage
                                                     #(quasicons
                                                       quasiappend
                                                       quasivector
                                                       quasi)
                                                     #((top) (top) (top) (top))
                                                     #("i4332"
                                                       "i4333"
                                                       "i4334"
                                                       "i4335")))
                                                  (hygiene guile))
                                               #{x\ 4349}#)
                                         (list '#(syntax-object
                                                  cons
                                                  ((top)
                                                   #(ribcage
                                                     #(_)
                                                     #((top))
                                                     #("i4359"))
                                                   #(ribcage
                                                     #(dy)
                                                     #((top))
                                                     #("i4353"))
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4347" "i4348"))
                                                   #(ribcage () () ())
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4342" "i4343"))
                                                   #(ribcage
                                                     #(quasicons
                                                       quasiappend
                                                       quasivector
                                                       quasi)
                                                     #((top) (top) (top) (top))
                                                     #("i4332"
                                                       "i4333"
                                                       "i4334"
                                                       "i4335")))
                                                  (hygiene guile))
                                               #{x\ 4349}#
                                               #{y\ 4350}#)))))))
                             #{tmp\ 4352}#)
                           (let ((#{tmp\ 4361}#
                                   ($sc-dispatch
                                     #{tmp\ 4351}#
                                     '(#(free-id
                                         #(syntax-object
                                           list
                                           ((top)
                                            #(ribcage
                                              #(x y)
                                              #((top) (top))
                                              #("i4347" "i4348"))
                                            #(ribcage () () ())
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(x y)
                                              #((top) (top))
                                              #("i4342" "i4343"))
                                            #(ribcage
                                              #(quasicons
                                                quasiappend
                                                quasivector
                                                quasi)
                                              #((top) (top) (top) (top))
                                              #("i4332"
                                                "i4333"
                                                "i4334"
                                                "i4335")))
                                           (hygiene guile)))
                                       .
                                       any))))
                             (if #{tmp\ 4361}#
                               (@apply
                                 (lambda (#{stuff\ 4363}#)
                                   (cons '#(syntax-object
                                            list
                                            ((top)
                                             #(ribcage
                                               #(stuff)
                                               #((top))
                                               #("i4362"))
                                             #(ribcage
                                               #(x y)
                                               #((top) (top))
                                               #("i4347" "i4348"))
                                             #(ribcage () () ())
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x y)
                                               #((top) (top))
                                               #("i4342" "i4343"))
                                             #(ribcage
                                               #(quasicons
                                                 quasiappend
                                                 quasivector
                                                 quasi)
                                               #((top) (top) (top) (top))
                                               #("i4332"
                                                 "i4333"
                                                 "i4334"
                                                 "i4335")))
                                            (hygiene guile))
                                         (cons #{x\ 4349}# #{stuff\ 4363}#)))
                                 #{tmp\ 4361}#)
                               (let ((#{else\ 4365}# #{tmp\ 4351}#))
                                 (list '#(syntax-object
                                          cons
                                          ((top)
                                           #(ribcage
                                             #(else)
                                             #((top))
                                             #("i4364"))
                                           #(ribcage
                                             #(x y)
                                             #((top) (top))
                                             #("i4347" "i4348"))
                                           #(ribcage () () ())
                                           #(ribcage () () ())
                                           #(ribcage
                                             #(x y)
                                             #((top) (top))
                                             #("i4342" "i4343"))
                                           #(ribcage
                                             #(quasicons
                                               quasiappend
                                               quasivector
                                               quasi)
                                             #((top) (top) (top) (top))
                                             #("i4332"
                                               "i4333"
                                               "i4334"
                                               "i4335")))
                                          (hygiene guile))
                                       #{x\ 4349}#
                                       #{y\ 4350}#))))))))
                   #{tmp\ 4346}#)
                 (syntax-violation
                   #f
                   "source expression failed to match any pattern"
                   #{tmp\ 4345}#))))))
       (#{quasiappend\ 4337}#
         (lambda (#{x\ 4366}# #{y\ 4367}#)
           (let ((#{tmp\ 4371}# (list #{x\ 4366}# #{y\ 4367}#)))
             (let ((#{tmp\ 4372}#
                     ($sc-dispatch #{tmp\ 4371}# (quote (any any)))))
               (if #{tmp\ 4372}#
                 (@apply
                   (lambda (#{x\ 4375}# #{y\ 4376}#)
                     (let ((#{tmp\ 4377}# #{y\ 4376}#))
                       (let ((#{tmp\ 4378}#
                               ($sc-dispatch
                                 #{tmp\ 4377}#
                                 '(#(free-id
                                     #(syntax-object
                                       quote
                                       ((top)
                                        #(ribcage
                                          #(x y)
                                          #((top) (top))
                                          #("i4373" "i4374"))
                                        #(ribcage () () ())
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(x y)
                                          #((top) (top))
                                          #("i4368" "i4369"))
                                        #(ribcage
                                          #(quasicons
                                            quasiappend
                                            quasivector
                                            quasi)
                                          #((top) (top) (top) (top))
                                          #("i4332" "i4333" "i4334" "i4335")))
                                       (hygiene guile)))
                                   ()))))
                         (if #{tmp\ 4378}#
                           (@apply (lambda () #{x\ 4375}#) #{tmp\ 4378}#)
                           (let ((#{_\ 4380}# #{tmp\ 4377}#))
                             (list '#(syntax-object
                                      append
                                      ((top)
                                       #(ribcage #(_) #((top)) #("i4379"))
                                       #(ribcage
                                         #(x y)
                                         #((top) (top))
                                         #("i4373" "i4374"))
                                       #(ribcage () () ())
                                       #(ribcage () () ())
                                       #(ribcage
                                         #(x y)
                                         #((top) (top))
                                         #("i4368" "i4369"))
                                       #(ribcage
                                         #(quasicons
                                           quasiappend
                                           quasivector
                                           quasi)
                                         #((top) (top) (top) (top))
                                         #("i4332" "i4333" "i4334" "i4335")))
                                      (hygiene guile))
                                   #{x\ 4375}#
                                   #{y\ 4376}#))))))
                   #{tmp\ 4372}#)
                 (syntax-violation
                   #f
                   "source expression failed to match any pattern"
                   #{tmp\ 4371}#))))))
       (#{quasivector\ 4338}#
         (lambda (#{x\ 4381}#)
           (let ((#{tmp\ 4384}# #{x\ 4381}#))
             (let ((#{x\ 4386}# #{tmp\ 4384}#))
               (let ((#{tmp\ 4387}# #{x\ 4386}#))
                 (let ((#{tmp\ 4388}#
                         ($sc-dispatch
                           #{tmp\ 4387}#
                           '(#(free-id
                               #(syntax-object
                                 quote
                                 ((top)
                                  #(ribcage #(x) #((top)) #("i4385"))
                                  #(ribcage () () ())
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i4382"))
                                  #(ribcage
                                    #(quasicons quasiappend quasivector quasi)
                                    #((top) (top) (top) (top))
                                    #("i4332" "i4333" "i4334" "i4335")))
                                 (hygiene guile)))
                             each-any))))
                   (if #{tmp\ 4388}#
                     (@apply
                       (lambda (#{x\ 4390}#)
                         (list '#(syntax-object
                                  quote
                                  ((top)
                                   #(ribcage #(x) #((top)) #("i4389"))
                                   #(ribcage #(x) #((top)) #("i4385"))
                                   #(ribcage () () ())
                                   #(ribcage () () ())
                                   #(ribcage #(x) #((top)) #("i4382"))
                                   #(ribcage
                                     #(quasicons quasiappend quasivector quasi)
                                     #((top) (top) (top) (top))
                                     #("i4332" "i4333" "i4334" "i4335")))
                                  (hygiene guile))
                               (list->vector #{x\ 4390}#)))
                       #{tmp\ 4388}#)
                     (let ((#{tmp\ 4392}#
                             ($sc-dispatch
                               #{tmp\ 4387}#
                               '(#(free-id
                                   #(syntax-object
                                     list
                                     ((top)
                                      #(ribcage #(x) #((top)) #("i4385"))
                                      #(ribcage () () ())
                                      #(ribcage () () ())
                                      #(ribcage #(x) #((top)) #("i4382"))
                                      #(ribcage
                                        #(quasicons
                                          quasiappend
                                          quasivector
                                          quasi)
                                        #((top) (top) (top) (top))
                                        #("i4332" "i4333" "i4334" "i4335")))
                                     (hygiene guile)))
                                 .
                                 each-any))))
                       (if #{tmp\ 4392}#
                         (@apply
                           (lambda (#{x\ 4394}#)
                             (cons '#(syntax-object
                                      vector
                                      ((top)
                                       #(ribcage #(x) #((top)) #("i4393"))
                                       #(ribcage #(x) #((top)) #("i4385"))
                                       #(ribcage () () ())
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4382"))
                                       #(ribcage
                                         #(quasicons
                                           quasiappend
                                           quasivector
                                           quasi)
                                         #((top) (top) (top) (top))
                                         #("i4332" "i4333" "i4334" "i4335")))
                                      (hygiene guile))
                                   #{x\ 4394}#))
                           #{tmp\ 4392}#)
                         (let ((#{_\ 4397}# #{tmp\ 4387}#))
                           (list '#(syntax-object
                                    list->vector
                                    ((top)
                                     #(ribcage #(_) #((top)) #("i4396"))
                                     #(ribcage #(x) #((top)) #("i4385"))
                                     #(ribcage () () ())
                                     #(ribcage () () ())
                                     #(ribcage #(x) #((top)) #("i4382"))
                                     #(ribcage
                                       #(quasicons
                                         quasiappend
                                         quasivector
                                         quasi)
                                       #((top) (top) (top) (top))
                                       #("i4332" "i4333" "i4334" "i4335")))
                                    (hygiene guile))
                                 #{x\ 4386}#)))))))))))
       (#{quasi\ 4339}#
         (lambda (#{p\ 4398}# #{lev\ 4399}#)
           (let ((#{tmp\ 4402}# #{p\ 4398}#))
             (let ((#{tmp\ 4403}#
                     ($sc-dispatch
                       #{tmp\ 4402}#
                       '(#(free-id
                           #(syntax-object
                             unquote
                             ((top)
                              #(ribcage () () ())
                              #(ribcage
                                #(p lev)
                                #((top) (top))
                                #("i4400" "i4401"))
                              #(ribcage
                                #(quasicons quasiappend quasivector quasi)
                                #((top) (top) (top) (top))
                                #("i4332" "i4333" "i4334" "i4335")))
                             (hygiene guile)))
                         any))))
               (if #{tmp\ 4403}#
                 (@apply
                   (lambda (#{p\ 4405}#)
                     (if (= #{lev\ 4399}# 0)
                       #{p\ 4405}#
                       (#{quasicons\ 4336}#
                         '(#(syntax-object
                             quote
                             ((top)
                              #(ribcage #(p) #((top)) #("i4404"))
                              #(ribcage () () ())
                              #(ribcage
                                #(p lev)
                                #((top) (top))
                                #("i4400" "i4401"))
                              #(ribcage
                                #(quasicons quasiappend quasivector quasi)
                                #((top) (top) (top) (top))
                                #("i4332" "i4333" "i4334" "i4335")))
                             (hygiene guile))
                           #(syntax-object
                             unquote
                             ((top)
                              #(ribcage #(p) #((top)) #("i4404"))
                              #(ribcage () () ())
                              #(ribcage
                                #(p lev)
                                #((top) (top))
                                #("i4400" "i4401"))
                              #(ribcage
                                #(quasicons quasiappend quasivector quasi)
                                #((top) (top) (top) (top))
                                #("i4332" "i4333" "i4334" "i4335")))
                             (hygiene guile)))
                         (#{quasi\ 4339}#
                           (list #{p\ 4405}#)
                           (1- #{lev\ 4399}#)))))
                   #{tmp\ 4403}#)
                 (let ((#{tmp\ 4406}#
                         ($sc-dispatch
                           #{tmp\ 4402}#
                           '(#(free-id
                               #(syntax-object
                                 unquote
                                 ((top)
                                  #(ribcage () () ())
                                  #(ribcage
                                    #(p lev)
                                    #((top) (top))
                                    #("i4400" "i4401"))
                                  #(ribcage
                                    #(quasicons quasiappend quasivector quasi)
                                    #((top) (top) (top) (top))
                                    #("i4332" "i4333" "i4334" "i4335")))
                                 (hygiene guile)))
                             .
                             any))))
                   (if (if #{tmp\ 4406}#
                         (@apply
                           (lambda (#{args\ 4408}#) (= #{lev\ 4399}# 0))
                           #{tmp\ 4406}#)
                         #f)
                     (@apply
                       (lambda (#{args\ 4410}#)
                         (syntax-violation
                           'unquote
                           "unquote takes exactly one argument"
                           #{p\ 4398}#
                           (cons '#(syntax-object
                                    unquote
                                    ((top)
                                     #(ribcage #(args) #((top)) #("i4409"))
                                     #(ribcage () () ())
                                     #(ribcage
                                       #(p lev)
                                       #((top) (top))
                                       #("i4400" "i4401"))
                                     #(ribcage
                                       #(quasicons
                                         quasiappend
                                         quasivector
                                         quasi)
                                       #((top) (top) (top) (top))
                                       #("i4332" "i4333" "i4334" "i4335")))
                                    (hygiene guile))
                                 #{args\ 4410}#)))
                       #{tmp\ 4406}#)
                     (let ((#{tmp\ 4411}#
                             ($sc-dispatch
                               #{tmp\ 4402}#
                               '((#(free-id
                                    #(syntax-object
                                      unquote-splicing
                                      ((top)
                                       #(ribcage () () ())
                                       #(ribcage
                                         #(p lev)
                                         #((top) (top))
                                         #("i4400" "i4401"))
                                       #(ribcage
                                         #(quasicons
                                           quasiappend
                                           quasivector
                                           quasi)
                                         #((top) (top) (top) (top))
                                         #("i4332" "i4333" "i4334" "i4335")))
                                      (hygiene guile)))
                                  any)
                                 .
                                 any))))
                       (if #{tmp\ 4411}#
                         (@apply
                           (lambda (#{p\ 4414}# #{q\ 4415}#)
                             (if (= #{lev\ 4399}# 0)
                               (#{quasiappend\ 4337}#
                                 #{p\ 4414}#
                                 (#{quasi\ 4339}# #{q\ 4415}# #{lev\ 4399}#))
                               (#{quasicons\ 4336}#
                                 (#{quasicons\ 4336}#
                                   '(#(syntax-object
                                       quote
                                       ((top)
                                        #(ribcage
                                          #(p q)
                                          #((top) (top))
                                          #("i4412" "i4413"))
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(p lev)
                                          #((top) (top))
                                          #("i4400" "i4401"))
                                        #(ribcage
                                          #(quasicons
                                            quasiappend
                                            quasivector
                                            quasi)
                                          #((top) (top) (top) (top))
                                          #("i4332" "i4333" "i4334" "i4335")))
                                       (hygiene guile))
                                     #(syntax-object
                                       unquote-splicing
                                       ((top)
                                        #(ribcage
                                          #(p q)
                                          #((top) (top))
                                          #("i4412" "i4413"))
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(p lev)
                                          #((top) (top))
                                          #("i4400" "i4401"))
                                        #(ribcage
                                          #(quasicons
                                            quasiappend
                                            quasivector
                                            quasi)
                                          #((top) (top) (top) (top))
                                          #("i4332" "i4333" "i4334" "i4335")))
                                       (hygiene guile)))
                                   (#{quasi\ 4339}#
                                     (list #{p\ 4414}#)
                                     (1- #{lev\ 4399}#)))
                                 (#{quasi\ 4339}# #{q\ 4415}# #{lev\ 4399}#))))
                           #{tmp\ 4411}#)
                         (let ((#{tmp\ 4416}#
                                 ($sc-dispatch
                                   #{tmp\ 4402}#
                                   '((#(free-id
                                        #(syntax-object
                                          unquote-splicing
                                          ((top)
                                           #(ribcage () () ())
                                           #(ribcage
                                             #(p lev)
                                             #((top) (top))
                                             #("i4400" "i4401"))
                                           #(ribcage
                                             #(quasicons
                                               quasiappend
                                               quasivector
                                               quasi)
                                             #((top) (top) (top) (top))
                                             #("i4332"
                                               "i4333"
                                               "i4334"
                                               "i4335")))
                                          (hygiene guile)))
                                      .
                                      any)
                                     .
                                     any))))
                           (if (if #{tmp\ 4416}#
                                 (@apply
                                   (lambda (#{args\ 4419}# #{q\ 4420}#)
                                     (= #{lev\ 4399}# 0))
                                   #{tmp\ 4416}#)
                                 #f)
                             (@apply
                               (lambda (#{args\ 4423}# #{q\ 4424}#)
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
                                               #("i4421" "i4422"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(p lev)
                                               #((top) (top))
                                               #("i4400" "i4401"))
                                             #(ribcage
                                               #(quasicons
                                                 quasiappend
                                                 quasivector
                                                 quasi)
                                               #((top) (top) (top) (top))
                                               #("i4332"
                                                 "i4333"
                                                 "i4334"
                                                 "i4335")))
                                            (hygiene guile))
                                         #{args\ 4423}#)))
                               #{tmp\ 4416}#)
                             (let ((#{tmp\ 4425}#
                                     ($sc-dispatch
                                       #{tmp\ 4402}#
                                       '(#(free-id
                                           #(syntax-object
                                             quasiquote
                                             ((top)
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(p lev)
                                                #((top) (top))
                                                #("i4400" "i4401"))
                                              #(ribcage
                                                #(quasicons
                                                  quasiappend
                                                  quasivector
                                                  quasi)
                                                #((top) (top) (top) (top))
                                                #("i4332"
                                                  "i4333"
                                                  "i4334"
                                                  "i4335")))
                                             (hygiene guile)))
                                         any))))
                               (if #{tmp\ 4425}#
                                 (@apply
                                   (lambda (#{p\ 4427}#)
                                     (#{quasicons\ 4336}#
                                       '(#(syntax-object
                                           quote
                                           ((top)
                                            #(ribcage #(p) #((top)) #("i4426"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(p lev)
                                              #((top) (top))
                                              #("i4400" "i4401"))
                                            #(ribcage
                                              #(quasicons
                                                quasiappend
                                                quasivector
                                                quasi)
                                              #((top) (top) (top) (top))
                                              #("i4332"
                                                "i4333"
                                                "i4334"
                                                "i4335")))
                                           (hygiene guile))
                                         #(syntax-object
                                           quasiquote
                                           ((top)
                                            #(ribcage #(p) #((top)) #("i4426"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(p lev)
                                              #((top) (top))
                                              #("i4400" "i4401"))
                                            #(ribcage
                                              #(quasicons
                                                quasiappend
                                                quasivector
                                                quasi)
                                              #((top) (top) (top) (top))
                                              #("i4332"
                                                "i4333"
                                                "i4334"
                                                "i4335")))
                                           (hygiene guile)))
                                       (#{quasi\ 4339}#
                                         (list #{p\ 4427}#)
                                         (1+ #{lev\ 4399}#))))
                                   #{tmp\ 4425}#)
                                 (let ((#{tmp\ 4428}#
                                         ($sc-dispatch
                                           #{tmp\ 4402}#
                                           '(any . any))))
                                   (if #{tmp\ 4428}#
                                     (@apply
                                       (lambda (#{p\ 4431}# #{q\ 4432}#)
                                         (#{quasicons\ 4336}#
                                           (#{quasi\ 4339}#
                                             #{p\ 4431}#
                                             #{lev\ 4399}#)
                                           (#{quasi\ 4339}#
                                             #{q\ 4432}#
                                             #{lev\ 4399}#)))
                                       #{tmp\ 4428}#)
                                     (let ((#{tmp\ 4433}#
                                             ($sc-dispatch
                                               #{tmp\ 4402}#
                                               '#(vector each-any))))
                                       (if #{tmp\ 4433}#
                                         (@apply
                                           (lambda (#{x\ 4435}#)
                                             (#{quasivector\ 4338}#
                                               (#{quasi\ 4339}#
                                                 #{x\ 4435}#
                                                 #{lev\ 4399}#)))
                                           #{tmp\ 4433}#)
                                         (let ((#{p\ 4438}# #{tmp\ 4402}#))
                                           (list '#(syntax-object
                                                    quote
                                                    ((top)
                                                     #(ribcage
                                                       #(p)
                                                       #((top))
                                                       #("i4437"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(p lev)
                                                       #((top) (top))
                                                       #("i4400" "i4401"))
                                                     #(ribcage
                                                       #(quasicons
                                                         quasiappend
                                                         quasivector
                                                         quasi)
                                                       #((top)
                                                         (top)
                                                         (top)
                                                         (top))
                                                       #("i4332"
                                                         "i4333"
                                                         "i4334"
                                                         "i4335")))
                                                    (hygiene guile))
                                                 #{p\ 4438}#))))))))))))))))))))
      (begin
        (lambda (#{x\ 4439}#)
          (let ((#{tmp\ 4441}# #{x\ 4439}#))
            (let ((#{tmp\ 4442}#
                    ($sc-dispatch #{tmp\ 4441}# (quote (any any)))))
              (if #{tmp\ 4442}#
                (@apply
                  (lambda (#{_\ 4445}# #{e\ 4446}#)
                    (#{quasi\ 4339}# #{e\ 4446}# 0))
                  #{tmp\ 4442}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 4441}#)))))))))

(define include
  (make-syntax-transformer
    'include
    'macro
    (lambda (#{x\ 4447}#)
      (letrec*
        ((#{read-file\ 4450}#
           (lambda (#{fn\ 4451}# #{k\ 4452}#)
             (begin
               (let ((#{p\ 4456}# (open-input-file #{fn\ 4451}#)))
                 (letrec*
                   ((#{f\ 4460}#
                      (lambda (#{x\ 4461}# #{result\ 4462}#)
                        (if (eof-object? #{x\ 4461}#)
                          (begin
                            (close-input-port #{p\ 4456}#)
                            (reverse #{result\ 4462}#))
                          (#{f\ 4460}#
                            (read #{p\ 4456}#)
                            (cons (datum->syntax #{k\ 4452}# #{x\ 4461}#)
                                  #{result\ 4462}#))))))
                   (begin
                     (#{f\ 4460}# (read #{p\ 4456}#) (quote ())))))))))
        (begin
          (let ((#{tmp\ 4463}# #{x\ 4447}#))
            (let ((#{tmp\ 4464}#
                    ($sc-dispatch #{tmp\ 4463}# (quote (any any)))))
              (if #{tmp\ 4464}#
                (@apply
                  (lambda (#{k\ 4467}# #{filename\ 4468}#)
                    (begin
                      (let ((#{fn\ 4470}# (syntax->datum #{filename\ 4468}#)))
                        (let ((#{tmp\ 4472}#
                                (#{read-file\ 4450}#
                                  #{fn\ 4470}#
                                  #{filename\ 4468}#)))
                          (let ((#{tmp\ 4473}#
                                  ($sc-dispatch
                                    #{tmp\ 4472}#
                                    'each-any)))
                            (if #{tmp\ 4473}#
                              (@apply
                                (lambda (#{exp\ 4475}#)
                                  (cons '#(syntax-object
                                           begin
                                           ((top)
                                            #(ribcage
                                              #(exp)
                                              #((top))
                                              #("i4474"))
                                            #(ribcage () () ())
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(fn)
                                              #((top))
                                              #("i4469"))
                                            #(ribcage
                                              #(k filename)
                                              #((top) (top))
                                              #("i4465" "i4466"))
                                            #(ribcage
                                              (read-file)
                                              ((top))
                                              ("i4449"))
                                            #(ribcage
                                              #(x)
                                              #((top))
                                              #("i4448")))
                                           (hygiene guile))
                                        #{exp\ 4475}#))
                                #{tmp\ 4473}#)
                              (syntax-violation
                                #f
                                "source expression failed to match any pattern"
                                #{tmp\ 4472}#)))))))
                  #{tmp\ 4464}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 4463}#)))))))))

(define include-from-path
  (make-syntax-transformer
    'include-from-path
    'macro
    (lambda (#{x\ 4477}#)
      (let ((#{tmp\ 4479}# #{x\ 4477}#))
        (let ((#{tmp\ 4480}#
                ($sc-dispatch #{tmp\ 4479}# (quote (any any)))))
          (if #{tmp\ 4480}#
            (@apply
              (lambda (#{k\ 4483}# #{filename\ 4484}#)
                (begin
                  (let ((#{fn\ 4486}# (syntax->datum #{filename\ 4484}#)))
                    (let ((#{tmp\ 4488}#
                            (datum->syntax
                              #{filename\ 4484}#
                              (begin
                                (let ((#{t\ 4493}#
                                        (%search-load-path #{fn\ 4486}#)))
                                  (if #{t\ 4493}#
                                    #{t\ 4493}#
                                    (syntax-violation
                                      'include-from-path
                                      "file not found in path"
                                      #{x\ 4477}#
                                      #{filename\ 4484}#)))))))
                      (let ((#{fn\ 4490}# #{tmp\ 4488}#))
                        (list '#(syntax-object
                                 include
                                 ((top)
                                  #(ribcage #(fn) #((top)) #("i4489"))
                                  #(ribcage () () ())
                                  #(ribcage () () ())
                                  #(ribcage #(fn) #((top)) #("i4485"))
                                  #(ribcage
                                    #(k filename)
                                    #((top) (top))
                                    #("i4481" "i4482"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i4478")))
                                 (hygiene guile))
                              #{fn\ 4490}#))))))
              #{tmp\ 4480}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp\ 4479}#)))))))

(define unquote
  (make-syntax-transformer
    'unquote
    'macro
    (lambda (#{x\ 4495}#)
      (let ((#{tmp\ 4497}# #{x\ 4495}#))
        (let ((#{tmp\ 4498}#
                ($sc-dispatch #{tmp\ 4497}# (quote (any any)))))
          (if #{tmp\ 4498}#
            (@apply
              (lambda (#{_\ 4501}# #{e\ 4502}#)
                (syntax-violation
                  'unquote
                  "expression not valid outside of quasiquote"
                  #{x\ 4495}#))
              #{tmp\ 4498}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp\ 4497}#)))))))

(define unquote-splicing
  (make-syntax-transformer
    'unquote-splicing
    'macro
    (lambda (#{x\ 4503}#)
      (let ((#{tmp\ 4505}# #{x\ 4503}#))
        (let ((#{tmp\ 4506}#
                ($sc-dispatch #{tmp\ 4505}# (quote (any any)))))
          (if #{tmp\ 4506}#
            (@apply
              (lambda (#{_\ 4509}# #{e\ 4510}#)
                (syntax-violation
                  'unquote-splicing
                  "expression not valid outside of quasiquote"
                  #{x\ 4503}#))
              #{tmp\ 4506}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp\ 4505}#)))))))

(define case
  (make-syntax-transformer
    'case
    'macro
    (lambda (#{x\ 4511}#)
      (let ((#{tmp\ 4513}# #{x\ 4511}#))
        (let ((#{tmp\ 4514}#
                ($sc-dispatch
                  #{tmp\ 4513}#
                  '(any any any . each-any))))
          (if #{tmp\ 4514}#
            (@apply
              (lambda (#{_\ 4519}#
                       #{e\ 4520}#
                       #{m1\ 4521}#
                       #{m2\ 4522}#)
                (let ((#{tmp\ 4524}#
                        (letrec*
                          ((#{f\ 4530}#
                             (lambda (#{clause\ 4531}# #{clauses\ 4532}#)
                               (if (null? #{clauses\ 4532}#)
                                 (let ((#{tmp\ 4534}# #{clause\ 4531}#))
                                   (let ((#{tmp\ 4535}#
                                           ($sc-dispatch
                                             #{tmp\ 4534}#
                                             '(#(free-id
                                                 #(syntax-object
                                                   else
                                                   ((top)
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(f clause clauses)
                                                      #((top) (top) (top))
                                                      #("i4527"
                                                        "i4528"
                                                        "i4529"))
                                                    #(ribcage
                                                      #(_ e m1 m2)
                                                      #((top)
                                                        (top)
                                                        (top)
                                                        (top))
                                                      #("i4515"
                                                        "i4516"
                                                        "i4517"
                                                        "i4518"))
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(x)
                                                      #((top))
                                                      #("i4512")))
                                                   (hygiene guile)))
                                               any
                                               .
                                               each-any))))
                                     (if #{tmp\ 4535}#
                                       (@apply
                                         (lambda (#{e1\ 4538}# #{e2\ 4539}#)
                                           (cons '#(syntax-object
                                                    begin
                                                    ((top)
                                                     #(ribcage
                                                       #(e1 e2)
                                                       #((top) (top))
                                                       #("i4536" "i4537"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(f clause clauses)
                                                       #((top) (top) (top))
                                                       #("i4527"
                                                         "i4528"
                                                         "i4529"))
                                                     #(ribcage
                                                       #(_ e m1 m2)
                                                       #((top)
                                                         (top)
                                                         (top)
                                                         (top))
                                                       #("i4515"
                                                         "i4516"
                                                         "i4517"
                                                         "i4518"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(x)
                                                       #((top))
                                                       #("i4512")))
                                                    (hygiene guile))
                                                 (cons #{e1\ 4538}#
                                                       #{e2\ 4539}#)))
                                         #{tmp\ 4535}#)
                                       (let ((#{tmp\ 4541}#
                                               ($sc-dispatch
                                                 #{tmp\ 4534}#
                                                 '(each-any any . each-any))))
                                         (if #{tmp\ 4541}#
                                           (@apply
                                             (lambda (#{k\ 4545}#
                                                      #{e1\ 4546}#
                                                      #{e2\ 4547}#)
                                               (list '#(syntax-object
                                                        if
                                                        ((top)
                                                         #(ribcage
                                                           #(k e1 e2)
                                                           #((top) (top) (top))
                                                           #("i4542"
                                                             "i4543"
                                                             "i4544"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(f clause clauses)
                                                           #((top) (top) (top))
                                                           #("i4527"
                                                             "i4528"
                                                             "i4529"))
                                                         #(ribcage
                                                           #(_ e m1 m2)
                                                           #((top)
                                                             (top)
                                                             (top)
                                                             (top))
                                                           #("i4515"
                                                             "i4516"
                                                             "i4517"
                                                             "i4518"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(x)
                                                           #((top))
                                                           #("i4512")))
                                                        (hygiene guile))
                                                     (list '#(syntax-object
                                                              memv
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4542"
                                                                   "i4543"
                                                                   "i4544"))
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
                                                                 #("i4527"
                                                                   "i4528"
                                                                   "i4529"))
                                                               #(ribcage
                                                                 #(_ e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4515"
                                                                   "i4516"
                                                                   "i4517"
                                                                   "i4518"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4512")))
                                                              (hygiene guile))
                                                           '#(syntax-object
                                                              t
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4542"
                                                                   "i4543"
                                                                   "i4544"))
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
                                                                 #("i4527"
                                                                   "i4528"
                                                                   "i4529"))
                                                               #(ribcage
                                                                 #(_ e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4515"
                                                                   "i4516"
                                                                   "i4517"
                                                                   "i4518"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4512")))
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
                                                                       #("i4542"
                                                                         "i4543"
                                                                         "i4544"))
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
                                                                       #("i4527"
                                                                         "i4528"
                                                                         "i4529"))
                                                                     #(ribcage
                                                                       #(_
                                                                         e
                                                                         m1
                                                                         m2)
                                                                       #((top)
                                                                         (top)
                                                                         (top)
                                                                         (top))
                                                                       #("i4515"
                                                                         "i4516"
                                                                         "i4517"
                                                                         "i4518"))
                                                                     #(ribcage
                                                                       ()
                                                                       ()
                                                                       ())
                                                                     #(ribcage
                                                                       #(x)
                                                                       #((top))
                                                                       #("i4512")))
                                                                    (hygiene
                                                                      guile))
                                                                 #{k\ 4545}#))
                                                     (cons '#(syntax-object
                                                              begin
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4542"
                                                                   "i4543"
                                                                   "i4544"))
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
                                                                 #("i4527"
                                                                   "i4528"
                                                                   "i4529"))
                                                               #(ribcage
                                                                 #(_ e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4515"
                                                                   "i4516"
                                                                   "i4517"
                                                                   "i4518"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4512")))
                                                              (hygiene guile))
                                                           (cons #{e1\ 4546}#
                                                                 #{e2\ 4547}#))))
                                             #{tmp\ 4541}#)
                                           (let ((#{_\ 4551}# #{tmp\ 4534}#))
                                             (syntax-violation
                                               'case
                                               "bad clause"
                                               #{x\ 4511}#
                                               #{clause\ 4531}#)))))))
                                 (let ((#{tmp\ 4553}#
                                         (#{f\ 4530}#
                                           (car #{clauses\ 4532}#)
                                           (cdr #{clauses\ 4532}#))))
                                   (let ((#{rest\ 4555}# #{tmp\ 4553}#))
                                     (let ((#{tmp\ 4556}# #{clause\ 4531}#))
                                       (let ((#{tmp\ 4557}#
                                               ($sc-dispatch
                                                 #{tmp\ 4556}#
                                                 '(each-any any . each-any))))
                                         (if #{tmp\ 4557}#
                                           (@apply
                                             (lambda (#{k\ 4561}#
                                                      #{e1\ 4562}#
                                                      #{e2\ 4563}#)
                                               (list '#(syntax-object
                                                        if
                                                        ((top)
                                                         #(ribcage
                                                           #(k e1 e2)
                                                           #((top) (top) (top))
                                                           #("i4558"
                                                             "i4559"
                                                             "i4560"))
                                                         #(ribcage
                                                           #(rest)
                                                           #((top))
                                                           #("i4554"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(f clause clauses)
                                                           #((top) (top) (top))
                                                           #("i4527"
                                                             "i4528"
                                                             "i4529"))
                                                         #(ribcage
                                                           #(_ e m1 m2)
                                                           #((top)
                                                             (top)
                                                             (top)
                                                             (top))
                                                           #("i4515"
                                                             "i4516"
                                                             "i4517"
                                                             "i4518"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(x)
                                                           #((top))
                                                           #("i4512")))
                                                        (hygiene guile))
                                                     (list '#(syntax-object
                                                              memv
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4558"
                                                                   "i4559"
                                                                   "i4560"))
                                                               #(ribcage
                                                                 #(rest)
                                                                 #((top))
                                                                 #("i4554"))
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
                                                                 #("i4527"
                                                                   "i4528"
                                                                   "i4529"))
                                                               #(ribcage
                                                                 #(_ e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4515"
                                                                   "i4516"
                                                                   "i4517"
                                                                   "i4518"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4512")))
                                                              (hygiene guile))
                                                           '#(syntax-object
                                                              t
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4558"
                                                                   "i4559"
                                                                   "i4560"))
                                                               #(ribcage
                                                                 #(rest)
                                                                 #((top))
                                                                 #("i4554"))
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
                                                                 #("i4527"
                                                                   "i4528"
                                                                   "i4529"))
                                                               #(ribcage
                                                                 #(_ e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4515"
                                                                   "i4516"
                                                                   "i4517"
                                                                   "i4518"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4512")))
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
                                                                       #("i4558"
                                                                         "i4559"
                                                                         "i4560"))
                                                                     #(ribcage
                                                                       #(rest)
                                                                       #((top))
                                                                       #("i4554"))
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
                                                                       #("i4527"
                                                                         "i4528"
                                                                         "i4529"))
                                                                     #(ribcage
                                                                       #(_
                                                                         e
                                                                         m1
                                                                         m2)
                                                                       #((top)
                                                                         (top)
                                                                         (top)
                                                                         (top))
                                                                       #("i4515"
                                                                         "i4516"
                                                                         "i4517"
                                                                         "i4518"))
                                                                     #(ribcage
                                                                       ()
                                                                       ()
                                                                       ())
                                                                     #(ribcage
                                                                       #(x)
                                                                       #((top))
                                                                       #("i4512")))
                                                                    (hygiene
                                                                      guile))
                                                                 #{k\ 4561}#))
                                                     (cons '#(syntax-object
                                                              begin
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4558"
                                                                   "i4559"
                                                                   "i4560"))
                                                               #(ribcage
                                                                 #(rest)
                                                                 #((top))
                                                                 #("i4554"))
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
                                                                 #("i4527"
                                                                   "i4528"
                                                                   "i4529"))
                                                               #(ribcage
                                                                 #(_ e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4515"
                                                                   "i4516"
                                                                   "i4517"
                                                                   "i4518"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4512")))
                                                              (hygiene guile))
                                                           (cons #{e1\ 4562}#
                                                                 #{e2\ 4563}#))
                                                     #{rest\ 4555}#))
                                             #{tmp\ 4557}#)
                                           (let ((#{_\ 4567}# #{tmp\ 4556}#))
                                             (syntax-violation
                                               'case
                                               "bad clause"
                                               #{x\ 4511}#
                                               #{clause\ 4531}#)))))))))))
                          (begin (#{f\ 4530}# #{m1\ 4521}# #{m2\ 4522}#)))))
                  (let ((#{body\ 4526}# #{tmp\ 4524}#))
                    (list '#(syntax-object
                             let
                             ((top)
                              #(ribcage #(body) #((top)) #("i4525"))
                              #(ribcage
                                #(_ e m1 m2)
                                #((top) (top) (top) (top))
                                #("i4515" "i4516" "i4517" "i4518"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4512")))
                             (hygiene guile))
                          (list (list '#(syntax-object
                                         t
                                         ((top)
                                          #(ribcage
                                            #(body)
                                            #((top))
                                            #("i4525"))
                                          #(ribcage
                                            #(_ e m1 m2)
                                            #((top) (top) (top) (top))
                                            #("i4515" "i4516" "i4517" "i4518"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i4512")))
                                         (hygiene guile))
                                      #{e\ 4520}#))
                          #{body\ 4526}#))))
              #{tmp\ 4514}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp\ 4513}#)))))))

(define make-variable-transformer
  (lambda (#{proc\ 4568}#)
    (if (procedure? #{proc\ 4568}#)
      (begin
        (letrec*
          ((#{trans\ 4571}#
             (lambda (#{x\ 4572}#)
               (#{proc\ 4568}# #{x\ 4572}#))))
          (begin
            (set-procedure-property!
              #{trans\ 4571}#
              'variable-transformer
              #t)
            #{trans\ 4571}#)))
      (error "variable transformer not a procedure"
             #{proc\ 4568}#))))

(define identifier-syntax
  (make-syntax-transformer
    'identifier-syntax
    'macro
    (lambda (#{x\ 4574}#)
      (let ((#{tmp\ 4576}# #{x\ 4574}#))
        (let ((#{tmp\ 4577}#
                ($sc-dispatch #{tmp\ 4576}# (quote (any any)))))
          (if #{tmp\ 4577}#
            (@apply
              (lambda (#{_\ 4580}# #{e\ 4581}#)
                (list '#(syntax-object
                         lambda
                         ((top)
                          #(ribcage
                            #(_ e)
                            #((top) (top))
                            #("i4578" "i4579"))
                          #(ribcage () () ())
                          #(ribcage #(x) #((top)) #("i4575")))
                         (hygiene guile))
                      '(#(syntax-object
                          x
                          ((top)
                           #(ribcage
                             #(_ e)
                             #((top) (top))
                             #("i4578" "i4579"))
                           #(ribcage () () ())
                           #(ribcage #(x) #((top)) #("i4575")))
                          (hygiene guile)))
                      '#((#(syntax-object
                            macro-type
                            ((top)
                             #(ribcage
                               #(_ e)
                               #((top) (top))
                               #("i4578" "i4579"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i4575")))
                            (hygiene guile))
                          .
                          #(syntax-object
                            identifier-syntax
                            ((top)
                             #(ribcage
                               #(_ e)
                               #((top) (top))
                               #("i4578" "i4579"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i4575")))
                            (hygiene guile))))
                      (list '#(syntax-object
                               syntax-case
                               ((top)
                                #(ribcage
                                  #(_ e)
                                  #((top) (top))
                                  #("i4578" "i4579"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i4575")))
                               (hygiene guile))
                            '#(syntax-object
                               x
                               ((top)
                                #(ribcage
                                  #(_ e)
                                  #((top) (top))
                                  #("i4578" "i4579"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i4575")))
                               (hygiene guile))
                            '()
                            (list '#(syntax-object
                                     id
                                     ((top)
                                      #(ribcage
                                        #(_ e)
                                        #((top) (top))
                                        #("i4578" "i4579"))
                                      #(ribcage () () ())
                                      #(ribcage #(x) #((top)) #("i4575")))
                                     (hygiene guile))
                                  '(#(syntax-object
                                      identifier?
                                      ((top)
                                       #(ribcage
                                         #(_ e)
                                         #((top) (top))
                                         #("i4578" "i4579"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4575")))
                                      (hygiene guile))
                                    (#(syntax-object
                                       syntax
                                       ((top)
                                        #(ribcage
                                          #(_ e)
                                          #((top) (top))
                                          #("i4578" "i4579"))
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i4575")))
                                       (hygiene guile))
                                     #(syntax-object
                                       id
                                       ((top)
                                        #(ribcage
                                          #(_ e)
                                          #((top) (top))
                                          #("i4578" "i4579"))
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i4575")))
                                       (hygiene guile))))
                                  (list '#(syntax-object
                                           syntax
                                           ((top)
                                            #(ribcage
                                              #(_ e)
                                              #((top) (top))
                                              #("i4578" "i4579"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(x)
                                              #((top))
                                              #("i4575")))
                                           (hygiene guile))
                                        #{e\ 4581}#))
                            (list (cons #{_\ 4580}#
                                        '(#(syntax-object
                                            x
                                            ((top)
                                             #(ribcage
                                               #(_ e)
                                               #((top) (top))
                                               #("i4578" "i4579"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x)
                                               #((top))
                                               #("i4575")))
                                            (hygiene guile))
                                          #(syntax-object
                                            ...
                                            ((top)
                                             #(ribcage
                                               #(_ e)
                                               #((top) (top))
                                               #("i4578" "i4579"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x)
                                               #((top))
                                               #("i4575")))
                                            (hygiene guile))))
                                  (list '#(syntax-object
                                           syntax
                                           ((top)
                                            #(ribcage
                                              #(_ e)
                                              #((top) (top))
                                              #("i4578" "i4579"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(x)
                                              #((top))
                                              #("i4575")))
                                           (hygiene guile))
                                        (cons #{e\ 4581}#
                                              '(#(syntax-object
                                                  x
                                                  ((top)
                                                   #(ribcage
                                                     #(_ e)
                                                     #((top) (top))
                                                     #("i4578" "i4579"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x)
                                                     #((top))
                                                     #("i4575")))
                                                  (hygiene guile))
                                                #(syntax-object
                                                  ...
                                                  ((top)
                                                   #(ribcage
                                                     #(_ e)
                                                     #((top) (top))
                                                     #("i4578" "i4579"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x)
                                                     #((top))
                                                     #("i4575")))
                                                  (hygiene guile)))))))))
              #{tmp\ 4577}#)
            (let ((#{tmp\ 4582}#
                    ($sc-dispatch
                      #{tmp\ 4576}#
                      '(any (any any)
                            ((#(free-id
                                #(syntax-object
                                  set!
                                  ((top)
                                   #(ribcage () () ())
                                   #(ribcage #(x) #((top)) #("i4575")))
                                  (hygiene guile)))
                              any
                              any)
                             any)))))
              (if (if #{tmp\ 4582}#
                    (@apply
                      (lambda (#{_\ 4589}#
                               #{id\ 4590}#
                               #{exp1\ 4591}#
                               #{var\ 4592}#
                               #{val\ 4593}#
                               #{exp2\ 4594}#)
                        (if (identifier? #{id\ 4590}#)
                          (identifier? #{var\ 4592}#)
                          #f))
                      #{tmp\ 4582}#)
                    #f)
                (@apply
                  (lambda (#{_\ 4603}#
                           #{id\ 4604}#
                           #{exp1\ 4605}#
                           #{var\ 4606}#
                           #{val\ 4607}#
                           #{exp2\ 4608}#)
                    (list '#(syntax-object
                             make-variable-transformer
                             ((top)
                              #(ribcage
                                #(_ id exp1 var val exp2)
                                #((top) (top) (top) (top) (top) (top))
                                #("i4597"
                                  "i4598"
                                  "i4599"
                                  "i4600"
                                  "i4601"
                                  "i4602"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4575")))
                             (hygiene guile))
                          (list '#(syntax-object
                                   lambda
                                   ((top)
                                    #(ribcage
                                      #(_ id exp1 var val exp2)
                                      #((top) (top) (top) (top) (top) (top))
                                      #("i4597"
                                        "i4598"
                                        "i4599"
                                        "i4600"
                                        "i4601"
                                        "i4602"))
                                    #(ribcage () () ())
                                    #(ribcage #(x) #((top)) #("i4575")))
                                   (hygiene guile))
                                '(#(syntax-object
                                    x
                                    ((top)
                                     #(ribcage
                                       #(_ id exp1 var val exp2)
                                       #((top) (top) (top) (top) (top) (top))
                                       #("i4597"
                                         "i4598"
                                         "i4599"
                                         "i4600"
                                         "i4601"
                                         "i4602"))
                                     #(ribcage () () ())
                                     #(ribcage #(x) #((top)) #("i4575")))
                                    (hygiene guile)))
                                '#((#(syntax-object
                                      macro-type
                                      ((top)
                                       #(ribcage
                                         #(_ id exp1 var val exp2)
                                         #((top) (top) (top) (top) (top) (top))
                                         #("i4597"
                                           "i4598"
                                           "i4599"
                                           "i4600"
                                           "i4601"
                                           "i4602"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4575")))
                                      (hygiene guile))
                                    .
                                    #(syntax-object
                                      variable-transformer
                                      ((top)
                                       #(ribcage
                                         #(_ id exp1 var val exp2)
                                         #((top) (top) (top) (top) (top) (top))
                                         #("i4597"
                                           "i4598"
                                           "i4599"
                                           "i4600"
                                           "i4601"
                                           "i4602"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4575")))
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
                                            #("i4597"
                                              "i4598"
                                              "i4599"
                                              "i4600"
                                              "i4601"
                                              "i4602"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i4575")))
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
                                            #("i4597"
                                              "i4598"
                                              "i4599"
                                              "i4600"
                                              "i4601"
                                              "i4602"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i4575")))
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
                                             #("i4597"
                                               "i4598"
                                               "i4599"
                                               "i4600"
                                               "i4601"
                                               "i4602"))
                                           #(ribcage () () ())
                                           #(ribcage #(x) #((top)) #("i4575")))
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
                                                        #("i4597"
                                                          "i4598"
                                                          "i4599"
                                                          "i4600"
                                                          "i4601"
                                                          "i4602"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i4575")))
                                                     (hygiene guile))
                                                  #{var\ 4606}#
                                                  #{val\ 4607}#)
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
                                                        #("i4597"
                                                          "i4598"
                                                          "i4599"
                                                          "i4600"
                                                          "i4601"
                                                          "i4602"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i4575")))
                                                     (hygiene guile))
                                                  #{exp2\ 4608}#))
                                      (list (cons #{id\ 4604}#
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
                                                         #("i4597"
                                                           "i4598"
                                                           "i4599"
                                                           "i4600"
                                                           "i4601"
                                                           "i4602"))
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(x)
                                                         #((top))
                                                         #("i4575")))
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
                                                         #("i4597"
                                                           "i4598"
                                                           "i4599"
                                                           "i4600"
                                                           "i4601"
                                                           "i4602"))
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(x)
                                                         #((top))
                                                         #("i4575")))
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
                                                        #("i4597"
                                                          "i4598"
                                                          "i4599"
                                                          "i4600"
                                                          "i4601"
                                                          "i4602"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i4575")))
                                                     (hygiene guile))
                                                  (cons #{exp1\ 4605}#
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
                                                               #("i4597"
                                                                 "i4598"
                                                                 "i4599"
                                                                 "i4600"
                                                                 "i4601"
                                                                 "i4602"))
                                                             #(ribcage
                                                               ()
                                                               ()
                                                               ())
                                                             #(ribcage
                                                               #(x)
                                                               #((top))
                                                               #("i4575")))
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
                                                               #("i4597"
                                                                 "i4598"
                                                                 "i4599"
                                                                 "i4600"
                                                                 "i4601"
                                                                 "i4602"))
                                                             #(ribcage
                                                               ()
                                                               ()
                                                               ())
                                                             #(ribcage
                                                               #(x)
                                                               #((top))
                                                               #("i4575")))
                                                            (hygiene
                                                              guile))))))
                                      (list #{id\ 4604}#
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
                                                        #("i4597"
                                                          "i4598"
                                                          "i4599"
                                                          "i4600"
                                                          "i4601"
                                                          "i4602"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i4575")))
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
                                                              #("i4597"
                                                                "i4598"
                                                                "i4599"
                                                                "i4600"
                                                                "i4601"
                                                                "i4602"))
                                                            #(ribcage () () ())
                                                            #(ribcage
                                                              #(x)
                                                              #((top))
                                                              #("i4575")))
                                                           (hygiene guile))
                                                        #{id\ 4604}#))
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
                                                        #("i4597"
                                                          "i4598"
                                                          "i4599"
                                                          "i4600"
                                                          "i4601"
                                                          "i4602"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i4575")))
                                                     (hygiene guile))
                                                  #{exp1\ 4605}#))))))
                  #{tmp\ 4582}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 4576}#)))))))))

(define define*
  (make-syntax-transformer
    'define*
    'macro
    (lambda (#{x\ 4609}#)
      (let ((#{tmp\ 4611}# #{x\ 4609}#))
        (let ((#{tmp\ 4612}#
                ($sc-dispatch
                  #{tmp\ 4611}#
                  '(any (any . any) any . each-any))))
          (if #{tmp\ 4612}#
            (@apply
              (lambda (#{_\ 4618}#
                       #{id\ 4619}#
                       #{args\ 4620}#
                       #{b0\ 4621}#
                       #{b1\ 4622}#)
                (list '#(syntax-object
                         define
                         ((top)
                          #(ribcage
                            #(_ id args b0 b1)
                            #((top) (top) (top) (top) (top))
                            #("i4613" "i4614" "i4615" "i4616" "i4617"))
                          #(ribcage () () ())
                          #(ribcage #(x) #((top)) #("i4610")))
                         (hygiene guile))
                      #{id\ 4619}#
                      (cons '#(syntax-object
                               lambda*
                               ((top)
                                #(ribcage
                                  #(_ id args b0 b1)
                                  #((top) (top) (top) (top) (top))
                                  #("i4613" "i4614" "i4615" "i4616" "i4617"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i4610")))
                               (hygiene guile))
                            (cons #{args\ 4620}#
                                  (cons #{b0\ 4621}# #{b1\ 4622}#)))))
              #{tmp\ 4612}#)
            (let ((#{tmp\ 4624}#
                    ($sc-dispatch
                      #{tmp\ 4611}#
                      '(any any any))))
              (if (if #{tmp\ 4624}#
                    (@apply
                      (lambda (#{_\ 4628}# #{id\ 4629}# #{val\ 4630}#)
                        (identifier?
                          '#(syntax-object
                             x
                             ((top)
                              #(ribcage
                                #(_ id val)
                                #((top) (top) (top))
                                #("i4625" "i4626" "i4627"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4610")))
                             (hygiene guile))))
                      #{tmp\ 4624}#)
                    #f)
                (@apply
                  (lambda (#{_\ 4634}# #{id\ 4635}# #{val\ 4636}#)
                    (list '#(syntax-object
                             define
                             ((top)
                              #(ribcage
                                #(_ id val)
                                #((top) (top) (top))
                                #("i4631" "i4632" "i4633"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4610")))
                             (hygiene guile))
                          #{id\ 4635}#
                          #{val\ 4636}#))
                  #{tmp\ 4624}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp\ 4611}#)))))))))

